"""
crg.py: Counting Regular Grammars implementation
"""


class RuleItem:
    def __init__(self, terminal, nonterminal, action):
        assert (terminal is None) != (nonterminal is None)
        self.terminal = terminal
        self.nonterminal = nonterminal
        self.action = action

    nonterminal_set = set()

    # this method operates on nonterminator names, not RuleItem objects
    # we allow nonterminator with same name assoicates to difference actions
    @classmethod
    def unique_nonterminal(cls, name_hint):
        id = 0
        [name_hint, *postfix] = name_hint.rsplit("_", maxsplit=1)
        if postfix:
            id = int(postfix[0]) + 1
        while f"{name_hint}_{id}" in cls.nonterminal_set:
            id += 1
        name = f"{name_hint}_{id}"
        cls.nonterminal_set.add(name)
        return name

    counter_count = 0

    @classmethod
    def new_counter(cls):
        counter = f"cnt_{cls.counter_count}"
        cls.counter_count += 1
        return counter

    def __str__(self):
        action = ""
        if self.action:
            action = f" ({self.action})"
        if self.is_terminal():
            return f"{self.terminal}{action}"
        else:
            return f"{self.nonterminal}{action}"

    def __hash__(self):
        return hash((self.terminal, self.nonterminal, self.action))

    def __eq__(self, other):
        return isinstance(other, RuleItem) and (
            self.terminal,
            self.nonterminal,
            self.action,
        ) == (other.terminal, other.nonterminal, other.action)

    def is_terminal(self):
        return self.terminal is not None

    def is_nonterminal(self):
        return self.nonterminal is not None

    def rewrite(self, source, target):
        if self.is_terminal() or self.nonterminal != source:
            return self
        return RuleItem(None, target, self.action)


class ProductionRule:
    def __init__(self, guard, head, body):
        assert body  # do not accept empty production rule
        # epsilon should be considered as a special terminal symbol since we are
        # already use regex as terminal
        self.guard = guard
        self.head = head
        self.body = body

    def __str__(self):
        guard = ""
        if self.guard:
            guard = f"({self.guard}) "
        body = " ".join(str(item) for item in self.body)
        return f"{guard}{self.head} -> {body}"

    def __hash__(self):
        return hash((self.guard, self.head, self.body))

    def __eq__(self, other):
        return isinstance(other, ProductionRule) and (
            self.guard,
            self.head,
            self.body,
        ) == (other.guard, other.head, other.body)

    def is_terminating(self):
        return len(self.body) == 1 and self.body[0].is_terminal()

    def is_nonterminating(self):
        return (
            len(self.body) == 2
            and self.body[0].is_terminal()
            and self.body[1].is_nonterminal()
        )

    def is_regular(self):
        return self.is_terminating() or self.is_nonterminating()

    def rewrite(self, source, target):
        return ProductionRule(
            self.guard,
            target if self.head == source else self.head,
            tuple(item.rewrite(source, target) for item in self.body),
        )


def reachable_table(grammar):
    symbol_set = set(rule.head for rule in grammar)
    old_table, table = None, {
        symbol: {
            item.nonterminal
            for rule in grammar
            if rule.head == symbol
            for item in rule.body
            if item.is_nonterminal() and item.nonterminal != symbol
        }
        for symbol in symbol_set
    }
    while table != old_table:
        old_table, table = table, {
            symbol: child_set
            | {leaf for child in child_set for leaf in table[child] if leaf != symbol}
            for symbol, child_set in table.items()
        }
    return table


def subgrammar_table(grammar):
    reachable = reachable_table(grammar)
    return {
        symbol: tuple(
            rule
            # ensure grammar property i.e. grammar[0].head is start symbol
            for gen in (
                (rule for rule in grammar if rule.head == symbol),
                (rule for rule in grammar if rule.head in reachable[symbol]),
            )
            for rule in gen
        )
        for symbol in reachable
    }


def normal_set(grammar):
    reachable = reachable_table(grammar)
    subgrammar = subgrammar_table(grammar)  # simplicity is over duplication
    # condition #1
    old_set, partial_set = None, {
        symbol
        for symbol in subgrammar
        if all(rule.is_regular() for rule in subgrammar[symbol])
    }

    # iteration helper, return True for `symbol` which is normal under
    # condition #2 and can only reach nonterminal in partial normal set
    # `partial_set`
    def condition2(symbol, partial_set):
        return all(
            not item.is_nonterminal() or item.nonterminal != symbol
            for rule in grammar
            if rule.head == symbol
            for item in rule.body[:-1]
        ) and all(
            reachable_symbol in partial_set
            and symbol not in reachable[reachable_symbol]
            for reachable_symbol in reachable[symbol]
        )

    while partial_set != old_set:
        old_set, partial_set = partial_set, set(
            symbol
            for symbol in subgrammar
            if symbol in partial_set or condition2(symbol, partial_set)
        )
    return partial_set


def regularize(grammar):
    def rewrite_rule(rule, subgrammar, normal):
        assert rule.head in normal
        if rule.is_regular():
            yield rule
            return
        if rule.body[0].is_nonterminal() and rule.body[0].nonterminal not in normal:
            raise Exception(f"incorrect rule: {rule}")
        if rule.body[0].is_nonterminal():  # and is normal
            symbol = rule.body[0].nonterminal
            rename = RuleItem.unique_nonterminal(symbol)
            yield ProductionRule(
                rule.guard, rule.head, (RuleItem(None, rename, rule.body[0].action),)
            )
            rename_grammar = (
                rule.rewrite(symbol, rename) for rule in subgrammar[symbol]
            )
            for rename_rule in rename_grammar:
                if rename_rule.is_terminating():
                    yield ProductionRule(
                        rename_rule.guard,
                        rename_rule.head,
                        rename_rule.body + rule.body[1:],
                    )
                else:
                    # here the paper describes as "for each nonterminaing..."
                    # which possibly imply the rule should be regular
                    # however the case above also produce nonregular rules so
                    # i don't think this is required to be regular either
                    yield rename_rule
        else:
            # not assert n > 2 here, there is a silly case where rule body
            # contains two terminal symbols
            name = RuleItem.unique_nonterminal(rule.head)
            yield ProductionRule(rule.guard, rule.head, (RuleItem(None, name, None),))
            yield ProductionRule(None, name, rule.body[1:])

    subgrammar = subgrammar_table(grammar)
    normal = normal_set(grammar)
    while not all(
        rule.is_regular() or len(rule.body) == 1  # allow idle rule
        for rule in grammar
        if rule.head in normal
    ):
        start = grammar[0].head
        # can we easily avoid yielding duplicated rules?
        # so this `grammar` not necessary to be unordered which break property
        grammar = set(
            rewrite
            for rule in grammar
            for rewrite in rewrite_rule(rule, subgrammar, normal)
        )
        subgrammar = subgrammar_table(grammar)
        grammar = subgrammar[start]  # eliminate dead rules
        # (which is probably nonregular)
        # and rebuild grammar property
        normal = normal_set(grammar)  # how to assert all new symbols are normal?
    return grammar


# precisely, `first_set_table`
def first_table(grammar):
    old_first, first = None, {
        symbol: set() for symbol in set(rule.head for rule in grammar)
    }

    def rule_first(rule, partial_first):
        rule_set = set()
        for item in rule.body:
            if item.is_terminal():
                return rule_set | {item.terminal}
            if epsilon_terminal() not in partial_first[item.nonterminal]:
                return rule_set | partial_first[item.nonterminal]
            rule_set |= partial_first[item.nonterminal] - {epsilon_terminal()}
        return rule_set | {epsilon_terminal()}

    while old_first != first:
        old_first, first = first, {
            symbol: set(
                terminal
                for rule in grammar
                if rule.head == symbol
                for terminal in rule_first(rule, first)
            )  # assert to be superset of previous iteration
            for symbol in first
        }
    return first


def approx(grammar):
    approx_symbol = grammar[0].head

    counter = RuleItem.new_counter()
    start = first_table(grammar)[approx_symbol] - {epsilon_terminal()}
    start_item = RuleItem(union_terminal(start), None, f"{counter} := {counter} + 1")
    stop = first_table(
        tuple(
            ProductionRule(rule.guard, rule.head, tuple(reversed(rule.body)))
            for rule in grammar
        )
    )[approx_symbol] - {epsilon_terminal()}
    stop_item = RuleItem(union_terminal(stop), None, f"{counter} := {counter} - 1")
    other_item = RuleItem(complement_terminal(union_terminal(start | stop)), None, None)
    approx_item = RuleItem(None, approx_symbol, None)
    return (
        ProductionRule(
            f"{counter} = 0", approx_symbol, (RuleItem(epsilon_terminal(), None, None),)
        ),
        ProductionRule(f"{counter} >= 0", approx_symbol, (start_item, approx_item)),
        ProductionRule(f"{counter} > 0", approx_symbol, (stop_item, approx_item)),
        ProductionRule(f"{counter} > 0", approx_symbol, (other_item, approx_item)),
    )


def eliminate_idle(grammar):
    def iteration(grammar):
        subgrammar = subgrammar_table(grammar)
        for rule in grammar:
            if rule.body[0].is_terminal():
                yield rule
                continue

            assert len(rule.body) == 1
            for inline_rule in subgrammar[rule.body[0].nonterminal]:
                inline_last = inline_rule.body[-1]
                yield ProductionRule(
                    merge_predicate(rule.guard, inline_rule.guard),
                    rule.head,
                    (
                        *inline_rule.body[:-1],
                        RuleItem(
                            inline_last.terminal,
                            inline_last.nonterminal,
                            compose_action(inline_last.action, rule.body[0].action),
                        ),
                    ),
                )

    old_grammar = None
    while grammar != old_grammar:
        old_grammar, grammar = grammar, tuple(iteration(grammar))
    return grammar


def optimize(grammar):
    subgrammar = subgrammar_table(grammar)
    normal = normal_set(grammar)
    # TODO reports when no nonterminal can be regularized
    # isn't that exact case for dyck?
    return eliminate_idle(
        subgrammar_table(
            regularize(tuple(rule for rule in grammar if rule.head in normal))
            + tuple(
                rule
                for symbol in subgrammar
                if symbol not in normal
                for rule in approx(subgrammar[symbol])
            )
        )[grammar[0].head]
    )


def epsilon_terminal():
    return "(epsilon)"  # TODO


def union_terminal(terminal_set):
    return f"any of {set(terminal_set)}"  # TODO


def complement_terminal(terminal):
    return f"any except {terminal}"  # TODO


def merge_predicate(guard, another_guard):
    if not guard:
        return another_guard
    if not another_guard:
        return guard
    return f"{guard} and {another_guard}"  # TODO


def compose_action(action, another_action):
    if not action:
        return another_action
    if not another_action:
        return action
    return f"{action}; {another_action}"  # TODO


## tests, cli and shared assets

symbol_b = RuleItem(None, "B", None)
symbol_v = RuleItem(None, "V", None)
varstring = (
    ProductionRule(None, "S", (symbol_b, symbol_v)),
    ProductionRule(None, "B", (RuleItem("0", None, "c := c * 2"), symbol_b)),
    ProductionRule(None, "B", (RuleItem("1", None, "c := c * 2 + 1"), symbol_b)),
    ProductionRule(None, "B", (RuleItem(" ", None, None),)),
    ProductionRule("c > 0", "V", (RuleItem(".", None, "c := c - 1"), symbol_v)),
    ProductionRule("c = 0", "V", (RuleItem(epsilon_terminal(), None, None),)),
)
varstring_extraction = (
    ProductionRule(None, "X", (symbol_b, RuleItem(None, "V", "vstr"))),
)
dyck = (
    ProductionRule(None, "S", (RuleItem(epsilon_terminal(), None, None),)),
    ProductionRule(None, "S", (RuleItem(None, "I", None), RuleItem(None, "S", None))),
    ProductionRule(
        None,
        "I",
        (
            RuleItem("[", None, None),
            RuleItem(None, "S", None),
            RuleItem("]", None, None),
        ),
    ),
)
dyck_extraction = (
    ProductionRule(
        None,
        "X",
        (
            RuleItem("[", None, None),
            RuleItem(None, "S", "param"),
            RuleItem("]", None, None),
            RuleItem(None, "S", None),
        ),
    ),
)

import unittest


class TestCRG(unittest.TestCase):
    def test_is_regular(self):
        self.assertFalse(varstring[0].is_regular())
        for item in varstring[1:]:
            with self.subTest(item=item):
                self.assertTrue(item.is_regular())
        self.assertTrue(varstring[1].is_nonterminating())
        self.assertTrue(varstring[2].is_nonterminating())
        self.assertTrue(varstring[3].is_terminating())
        self.assertTrue(varstring[4].is_nonterminating())
        self.assertTrue(varstring[5].is_terminating())

        self.assertTrue(dyck[0].is_terminating())
        self.assertFalse(dyck[1].is_regular())
        self.assertFalse(dyck[2].is_regular())

    def test_reachable(self):
        self.assertEqual(reachable_table(()), {})
        self.assertEqual(
            reachable_table(varstring), {"S": {"B", "V"}, "B": set(), "V": set()}
        )
        self.assertEqual(reachable_table(dyck), {"S": {"I"}, "I": {"S"}})
        self.assertEqual(reachable_table(dyck_extraction + dyck)["X"], {"S", "I"})

    def test_subgrammar(self):
        self.assertEqual(subgrammar_table(()), {})
        subgrammar = subgrammar_table(varstring)
        self.assertEqual(subgrammar["S"], varstring)
        self.assertEqual(subgrammar["B"], varstring[1:4])
        self.assertEqual(subgrammar["V"], varstring[4:])
        self.assertEqual(subgrammar_table(dyck)["S"], dyck)

    def test_normal(self):
        self.assertEqual(normal_set(()), set())
        self.assertEqual(normal_set(varstring), {"S", "B", "V"})
        self.assertEqual(normal_set(dyck), set())

    def test_first(self):
        self.assertEqual(first_table(()), {})
        self.assertEqual(
            first_table(varstring),
            {
                "S": {"0", "1", " "},
                "B": {"0", "1", " "},
                "V": {".", epsilon_terminal()},
            },
        )
        self.assertEqual(
            first_table(dyck),
            {"S": {"[", epsilon_terminal()}, "I": {"["}},
        )


if __name__ == "__main__":
    for rule in optimize(varstring):
        print(rule)
    print()
    for rule in optimize(dyck):
        print(rule)
    unittest.main()
