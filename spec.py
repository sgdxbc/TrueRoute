"""
spec.py: Frontend of Counting Context Free Grammar, the grammar of protocol and
extraction specifications.

The main interface is `Grammar(s)`, which accept a string of specification text.
The string `s` must start with meaningful character including comment head, i.e.
be `lstrip`ed. It also must end with at least one new line.

`Grammar` instance is iterable of `ProductionRule`. Wrap it into `tuple()` in
order to pass into `crg.optimize`.
"""
from string import ascii_letters, digits, whitespace
from crg import ProductionRule, RuleItem, Regular, compose_action
from re import match


# you should put recusive descent as my epitaph
class Grammar:
    def __init__(self, s):
        self.s = s
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
        raw, s = self.s.split("]", maxsplit=1)
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

    @staticmethod
    def action(s):
        dst, expr = s.split(":=")
        dst, expr = dst.strip(), expr.strip()
        # i start to feel lazy... complete a expr tower for me, thanks
        if expr.isdigit():
            return (("imm", dst, int(expr)),)
        # TODO replace `re` with `lpdfa`, let's bootstrap ^o^
        if m := match(r"(\w+)\s*\*\s*(\w+)\s*(\+\s*(\w+))?", expr):
            mul, src = (int(m[1]), m[2]) if m[1].isdigit() else (int(m[2]), m[1])
            if m[4]:
                return ("muli", dst, src, mul), ("addi", dst, dst, int(m[4]))
            else:
                return (("muli", dst, src, mul),)
        if m := match(r"(\w+)\s*\+\s*(\w+)\s*\*\s*(\w+)", expr):
            mul, src = (int(m[2]), m[3]) if m[2].isdigit() else (int(m[3]), m[2])
            add = int(m[1])
            return ("muli", dst, src, mul), ("addi", dst, dst, add)
        if m := match(r"(\w+)\s*-\s*(\w+)", expr):
            return (("subi", dst, m[1], int(m[2])),)
        if m := match(r"(\w+)\s*\(\s*((\w+\s*,\s*)*(\w+)?)\s*\)", expr):
            arg = tuple(a.strip() for a in m[2].split(",") if a.strip())
            stmt = False
            if m[1] in {"drop_tail"}:
                assert not arg
                stmt = True
            # what the fuck is the standard of adding underscore or not
            elif m[1] in {"pos", "cur_byte", "cur_double_byte", "getnum", "gethex"}:
                assert not arg
            elif m[1] in {"skip", "skip_to", "notify"}:
                assert len(arg) == 1
            elif m[1] in {"bounds", "save"}:
                assert len(arg) == 2
                stmt = True
            else:  # assume invoke extraction as a normal action
                assert len(arg) == 1
                arg = (int(arg[0]),) if arg[0].isdigit() else arg
                stmt = True
            return ((m[1], *arg),) if stmt else ((m[1], dst, *arg),)

        assert False, f"unsupported action: {expr}"

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
            exact = self.unsigned()
            assert 0 <= exact < 256
            exact = Regular(exact={exact})
        else:
            if self.s[0] == "\\":
                self.skip("\\")
                exact_table = {
                    "n": Regular(exact={ord("\n")}),
                    "r": Regular(exact={ord("\r")}),
                    "t": Regular(exact={ord("\t")}),
                    "d": Regular(exact={ord(c) for c in digits}, repr_str="\\d"),
                    "w": Regular(exact={ord(c) for c in ascii_letters}, repr_str="\\w"),
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
            elif self.s[0] == ".":
                exact = Regular.wildcard
            else:
                exact = Regular(exact={ord(self.s[0])})
                if self.case_insensitive:
                    exact = Regular(
                        exact={ord(self.s[0].lower()), ord(self.s[0].upper())},
                        repr_str=str(exact),  # there should be a outer repr
                    )
            self.s = self.s[1:].lstrip()
        return exact

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
            self.append_action(((self.extract, "p"),))
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
            action = sum((self.action(step) for step in self.bracket()), start=())
        return RuleItem(terminal=terminal, nonterminal=nonterminal, action=action)
