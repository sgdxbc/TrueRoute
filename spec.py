"""
spec.py: Frontend of Counting Context Free Grammar, the grammar of protocol and
extraction specifications.

The main interface is `Grammar(s)`, which accept a string of specification text.
`Grammar` instance is iterable of `ProductionRule`. Wrap it into `tuple()` in
order to pass into `crg.optimize`.
"""
from string import ascii_letters, digits, whitespace
from crg import ProductionRule, RuleItem, Regular, compose_action


# you should put recusive descent as my epitaph
class Grammar:
    def __init__(self, s):
        self.s = s.lstrip() + "\n"
        # maintain line number or something for a better error reporting

        self.prev_item = None
        self.extract = None
        self.regex_mode = False  # coarse-grained disable meaningless whitespace
        # in regex. consider loose restriction if necessary
        self.case_insensitive = False

    def __iter__(self):
        while self.s:
            if self.s[0] == "#":
                self.s = self.s.split("\n", maxsplit=1)[1].lstrip()
            else:
                yield self.rule()

    def name(self):
        assert (
            self.s[0] in ascii_letters + "_"
        ), f"expect name: {self.s.splitlines()[0]}"
        for i in range(1, len(self.s)):
            if self.s[i] not in ascii_letters + digits + "_":
                break
        assert not self.regex_mode
        name, self.s = self.s[:i], self.s[i:].lstrip()
        return name

    def skip(self, pattern):
        assert self.s.startswith(pattern)
        self.s = self.s[len(pattern) :]
        if not self.regex_mode:
            self.s = self.s.lstrip()

    def bracket(self):
        self.skip("[")
        # there should not be any possible nested bracket i guess...
        # TODO don't cheat on this
        raw, s = self.s.split("]", maxsplit=1)
        assert not self.regex_mode
        self.s = s.lstrip()
        for part in raw.split(";"):
            yield part.strip()

    def regex(self):
        self.skip("/")
        self.regex_mode = True
        regular = self.regex_union()
        self.regex_mode = False
        self.skip("/")
        return regular

    def unsigned(self):
        assert self.s[0] in digits
        for i in range(1, len(self.s)):
            if self.s[i] not in digits:
                break
        u, self.s = int(self.s[:i]), self.s[i:]
        if not self.regex_mode:
            self.s = self.s.lstrip()
        return u

    @staticmethod
    def guard(s):
        if ">=" in s:
            variable, low = s.split(">=")
            return variable.strip(), (int(low), None)
        if "<=" in s:
            variable, inclusive_high = s.split("<=")
            return variable.strip(), (None, int(inclusive_high) + 1)
        if "==" in s:  # why not just =
            variable, low = s.split("==")
            return variable.strip(), (int(low), int(low) + 1)
        if ">" in s:
            variable, exclusive_low = s.split(">")
            return variable.strip(), (int(exclusive_low) + 1, None)
        if "<" in s:
            variable, high = s.split("<")
            return variable.strip(), (None, high)
        assert False, f"unsupport guard {s}"

    def rule(self):
        head = self.name()
        guard = {}
        priority = ProductionRule.default_priority
        if self.s[0] == "[":
            # TODO merge guard on same variable instead of override
            guard = dict(Grammar.guard(part) for part in self.bracket())
        if self.s[0] in digits:
            priority = self.unsigned()
        self.skip("->")
        self.prev_item = RuleItem(terminal=Regular.epsilon)
        item_list = self.item_list()
        if self.prev_item.action or not item_list:
            item_list = (self.prev_item, *item_list)
        self.prev_item = None
        return ProductionRule(head, guard, priority, item_list)

    def item_list(self):
        if self.s[0] == ";":
            assert self.extract is None
            self.skip(";")
            return ()
        if self.s[0] == ")":
            assert self.extract is not None
            # i don't fully understand the reason to assign a possible-null
            # value returned by custom extraction action back into `p`
            # i guess the semantic requires there must be an assignment cause
            # that, since `p` is the only variable who can be manipulated
            # without corrupt things by now
            # using a placeholder to make thing explicit
            self.append_action(((self.extract, "_", "p"),))
            self.extract = None
            self.skip(")")
            return self.item_list()

        # we must do `item()` first, which may cause `prev_item` refer to a
        # different item, then save the changed reference
        self.prev_item, old_prev = self.item(), self.prev_item
        item_list = self.item_list()
        item, self.prev_item = self.prev_item, old_prev
        return item, *item_list

    def append_action(self, action):
        assert self.prev_item is not None
        self.prev_item = RuleItem(
            terminal=self.prev_item.terminal,
            nonterminal=self.prev_item.nonterminal,
            action=compose_action(self.prev_item.action, action),
        )

    def item(self):
        if self.s[0] == "/":
            terminal, nonterminal = self.regex(), None
        elif self.s[0] in ascii_letters + "_":
            name = self.name()
            if self.s[0] == "(":
                self.append_action((("pos", "p"),))
                self.extract = name
                self.skip("(")
                return self.item()  # assert at least one item remain (and in
                # the extracting paren)

            nonterminal, terminal = name, None
        else:
            assert (
                False
            ), f"expect terminal/nonterminal/extract: {self.s.splitlines()[0]}"
        action = ()
        if self.s[0] == "[":
            self.skip("[")
            action = self.action()
            self.skip("]")
        return RuleItem(terminal=terminal, nonterminal=nonterminal, action=action)

    # operator prcedence following POSIX extended regular expression syntax
    # https://www.boost.org/doc/libs/1_79_0/libs/regex/doc/html/boost_regex/syntax/basic_extended.html#boost_regex.syntax.basic_extended.operator_precedence
    # skipped unsupported collation-related bracket symbols and anchoring
    def regex_union(self):
        def gen():
            while self.s[0] not in {"/", ")"}:
                yield self.regex_concat()
                if self.s == "|":
                    self.skip("|")

        return Regular.new_union(set(gen()))

    def regex_concat(self):
        def gen():
            while self.s[0] not in {"|", "/", ")"}:
                yield self.regex_postfix()

        concat = tuple(gen())
        assert concat  # epsilon is handled in union
        if len(concat) == 1:
            return concat[0]
        return Regular(concat=concat)

    def regex_postfix(self):
        inner = self.regex_group()
        if self.s[0] == "*":
            self.skip("*")
            return Regular(star=inner)
        if self.s[0] == "+":
            self.skip("+")
            return Regular(concat=(inner, Regular(star=inner)), repr_str=f"{inner}+")
        if self.s[0] == "?":
            self.skip("?")
            return Regular.new_union({inner, Regular.epsilon}, repr_str=f"{inner}?")
        return inner

    def regex_group(self):
        if self.s[0] != "(":
            return self.regex_set()

        self.skip("(")
        if self.s[:3] == "?i:":
            assert not self.case_insensitive
            self.case_insensitive = True
            self.skip("?i:")
        inner = self.regex_union()
        repr_str = f"({inner})" if not self.case_insensitive else f"(?i:{inner})"
        self.case_insensitive = False
        self.skip(")")
        return Regular(
            exact=inner.exact,
            concat=inner.concat,
            union=inner.union,
            star=inner.star,
            repr_str=repr_str,
        )

    def regex_set(self):
        if self.s[0] != "[":
            return self.regex_escape()

        self.skip("[")
        if self.s[0] == "^":
            exclude = True
            self.skip("^")
        else:
            exclude = False

        def gen():
            while self.s[0] != "]":
                char = self.regex_escape()
                if self.s[0] != "-":
                    yield char
                else:
                    assert char.is_exact() and len(char.exact) == 1
                    self.skip("-")
                    char_high = self.regex_escape()
                    assert char_high.is_exact() and len(char_high.exact) == 1
                    (low,), (high,) = char.exact, char_high.exact
                    yield Regular(
                        exact=set(range(low, high + 1)), repr_str=f"[{low}-{high}]"
                    )

        regular = Regular.new_union(set(gen()))
        assert regular.is_exact()
        self.skip("]")
        return regular if not exclude else Regular.new_exclude(regular)

    def regex_escape(self):
        if self.s[:2] == "\\x":
            self.skip("\\x")
            # support single hex digit?
            exact = Regular(exact={int(self.s[:2], base=16)})
            self.s = self.s[2:]
        else:
            if self.s[0] == ".":
                exact = Regular.wildcard
            elif self.s[0] != "\\":
                assert self.s[0] not in {"^", "$"}  # not supported
                exact = Regular(exact={ord(self.s[0])})
                if self.case_insensitive:
                    exact = Regular(
                        exact={ord(self.s[0].lower()), ord(self.s[0].upper())},
                        repr_str=str(exact),  # there should be a outer repr
                    )
            else:
                self.skip("\\")
                exact_table = {
                    "n": Regular(exact={ord("\n")}),
                    "r": Regular(exact={ord("\r")}),
                    "t": Regular(exact={ord("\t")}),
                    "d": Regular(exact={ord(c) for c in digits}, repr_str="\\d"),
                    "w": Regular(
                        exact={ord(c) for c in ascii_letters + digits + "_"},
                        repr_str="\\w",
                    ),
                    "s": Regular(exact={ord(c) for c in whitespace}, repr_str="\\s"),
                }
                exact_table = {
                    **exact_table,
                    "D": Regular.new_exclude(exact_table["d"]),
                    "W": Regular.new_exclude(exact_table["w"]),
                    "S": Regular.new_exclude(exact_table["s"]),
                    # see if there need anything more
                }
                exact = exact_table.get(self.s[0], Regular(exact={ord(self.s[0])}))

            self.s = self.s[1:]
        return exact

    def action(self):  # not skip barcket pair
        if self.s[0] == "]":
            return ()
        step = self.action_step()
        if self.s[0] == ";":
            self.skip(";")
        return *step, *self.action()

    def action_step(self):
        var = self.name()
        self.skip(":=")
        expr, action = self.expr0(var)
        if expr == var:
            return action
        return *action, ("addi", var, expr, 0)

    def expr2(self, var):
        if self.s[0] in digits:
            return var, (("imm", var, self.unsigned()),)

        # consider support parenthese

        assert self.s[0] in ascii_letters + "_"
        name = self.name()
        # underscore inconsistency...why
        if name in {
            "pos",
            "drop_tail",
            "cur_byte",
            "cur_double_byte",
            "getnum",
            "gethex",
        }:
            self.skip("(")
            self.skip(")")
            return var, ((name, var),)
        if name in {"skip", "skip_to", "notify"}:
            self.skip("(")
            arg1, action = self.expr0("$1")
            self.skip(")")
            return var, (*action, (name, var, arg1))
        if name in {"bounds", "save"}:
            self.skip("(")
            arg1, action1 = self.expr0("$1")
            self.skip(",")
            arg2, action2 = self.expr0("$2")
            self.skip(")")
            return var, (*action1, *action2, (name, var, arg1, arg2))

        if self.s[0] == "(":  # user defined extraction
            self.skip("(")
            arg1, action = self.expr0("$1")
            self.skip(")")
            return var, (*action, (name, var, arg1))

        return name, ()

    def expr_infix(self, var, op_table, inner):
        expr, action = inner(var)  # in place to save tmp
        while op := next((op for op in op_table if self.s.startswith(op)), None):
            self.skip(op)
            another_expr, another_action = inner("$2")
            action = *action, *another_action, (op_table[op], var, expr, another_expr)
            expr = var
        return expr, action

    def expr1(self, var):
        return self.expr_infix(var, {"*": "mul"}, self.expr2)

    def expr0(self, var):
        return self.expr_infix(var, {"+": "add", "-": "sub"}, self.expr1)
