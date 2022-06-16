"""
spec.py: Frontend of Counting Context Free Grammar, the grammar of protocol and
extraction specifications.

The main interface is `Grammar(s)`, which accept a string of specification text.
The string `s` must start with meaningful character including comment head, i.e.
be `lstrip`ed. It also must end with at least one new line.

`Grammar` instance is iterable of `ProductionRule`. Wrap it into `tuple()` in 
order to pass into `crg.grammar`.
"""
from string import ascii_letters, digits
from crg import ProductionRule, RuleItem, Regular, compose_action


# you should put recusive descent as my epitaph
class Grammar:
    def __init__(self, s):
        self.s = s
        # maintain line number or something for a better error reporting

        self.prev_item = None
        self.extract = None

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
        self.s = self.s[len(pattern) :].lstrip()

    def bracket(self):
        self.skip("[")
        # there should not be any possible nested bracket i guess...
        raw, s = self.s[1:].split("]", maxsplit=1)
        self.s = s.lstrip()
        for part in raw.split(";"):
            yield part.strip()

    def regex(self):
        self.skip("/")
        regular = self.regex_union()
        self.skip("/")
        return regular

    def unsigned(self):
        assert self.s[0] in digits
        for i in range(1, len(self.s)):
            if self.s[i] not in digits:
                break
        u, self.s = int(self.s[:i]), self.s[i:].lstrip()
        return u

    @staticmethod
    def guard(s):
        if ">=" in s:
            [variable, low] = s.split(">=")
            return variable.strip(), (int(low), None)
        if "<=" in s:
            [variable, inclusive_high] = s.split("<=")
            return variable.strip(), (None, int(inclusive_high) + 1)
        if "==" in s:  # why not just =
            [variable, low] = s.split("==")
            return variable.strip(), (int(low), int(low) + 1)
        if ">" in s:
            [variable, exclusive_low] = s.split(">")
            return variable.strip(), (int(exclusive_low) + 1, None)
        if "<" in s:
            [variable, high] = s.split("<")
            return variable.strip(), (None, high)
        assert False, f"unsupport guard {s}"

    def regex_union(self):
        def gen():
            while self.s != "/":
                yield self.regex_concat()
                if self.s == "|":
                    self.skip("|")

        return Regular.new_union(set(gen()))

    def regex_concat(self):
        def gen():
            while self.s[0] not in {"|", "/"}:
                yield self.regex_postfix()

        return Regular(concat=tuple(gen()))

    def rule(self):
        head = self.name()
        guard = {}
        priority = ProductionRule.default_priority
        if self.s[0] == "[":
            # TODO merge guard on same variable instead of override
            guard = {
                variable: bound
                for variable, bound in (guard(part) for part in self.bracket())
            }
        if self.s[0] in digits:
            priority = self.unsigned()
        self.skip("->")
        self.prev_item = RuleItem(terminal=Regular.epsilon)
        rule = ProductionRule(head, guard, priority, self.item_list())
        self.prev_item = None
        return rule

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
            self.append_action((f"_ := {self.extraction}(p)",))
            self.extraction = None
            self.skip(")")
            return self.item_list()

        old_prev, self.prev_item = self.prev_item, self.item()
        item_list = self.item_list()
        self.prev_item, item = old_prev, self.prev_item
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
                self.append_action(("p := pos()",))
                self.extract = name
                self.skip("(")
                return self.item()  # assert at least one item remain (and in
                # the extracting paren)
            else:
                nonterminal, terminal = name, None
        else:
            assert (
                False
            ), f"expect terminal/nonterminal/extract: {self.s.splitlines()[0]}"
        action = ()
        if self.s[0] == "[":
            action = tuple(self.bracket())  # TODO
        return RuleItem(terminal=terminal, nonterminal=nonterminal, action=action)


# misc
varstring = """
S -> B V;
B -> /0/ [c := c * 2] B;
B -> /1/ [c := c * 2 + 1] B;
B -> / /;
V [c > 0] -> /./ [c := c - 1] V;
V [c == 0] -> ;
"""

dyck = """
S -> ;
S -> I S;
I -> /[/ S /]/;
"""

dyck_extraction = """
X -> /[/ param( S ) /]/ S;
"""

from unittest import TestCase
from crg import varstring as varstring_grammar, dyck as dyck_grammar


class TestSpec(TestCase):
    def test_grammar(self):
        self.assertEqual(tuple(Grammar(varstring.lstrip())), varstring_grammar)
        self.assertEqual(tuple(Grammar(dyck.lstrip())), dyck_grammar)

    def test_extraction(self):
        self.assertEqual(
            tuple(Grammar(dyck_extraction.lstrip())),
            (
                ProductionRule(
                    "X",
                    {},
                    ProductionRule.default_priority,
                    (
                        RuleItem(
                            terminal=Regular.new_literal(b"["), action=("p := pos()",)
                        ),
                        RuleItem(nonterminal="S", action=("_ := param(p)",)),
                        RuleItem(terminal=Regular.new_literal(b"]")),
                        RuleItem(nonterminal="S"),
                    ),
                ),
            ),
        )
