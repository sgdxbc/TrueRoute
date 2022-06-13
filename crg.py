"""
crg.py: Counting Regular Grammars implementation
"""


class RuleItem:
    def __init__(self, terminal=None, nonterminal=None, action=None):
        assert (terminal is None) != (nonterminal is None)
        assert terminal is None or isinstance(terminal, Regular)
        assert nonterminal is None or isinstance(nonterminal, str)
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
        action = f" ({self.action})" if self.action else ""
        if self.is_terminal():
            return f"/{self.terminal}/{action}"
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
        return RuleItem(nonterminal=target, action=self.action)


class ProductionRule:
    def __init__(self, guard, head, body):
        assert body  # do not accept empty production rule
        # epsilon should be considered as a special terminal symbol since we are
        # already use regex as terminal
        self.guard = guard
        self.head = head
        self.body = body

    def __str__(self):
        guard = f"({self.guard}) " if self.guard else ""
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
        return (  # we treat idle rule as nonterminating
            len(self.body) == 1
            and self.body[0].is_nonterminal()
            or len(self.body) == 2
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
    subgrammar = subgrammar_table(grammar)  # simplicity over duplication work
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
        return not any(
            item.is_nonterminal() and item.nonterminal == symbol
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
            # `rule.body[0]`'s action would be lost in such rewrite manner?
            yield ProductionRule(rule.guard, rule.head, (RuleItem(nonterminal=rename),))
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
            yield ProductionRule(rule.guard, rule.head, (RuleItem(nonterminal=name),))
            yield ProductionRule(None, name, rule.body[1:])

    subgrammar = subgrammar_table(grammar)
    while not all(rule.is_regular() for rule in grammar):
        # can we easily avoid yielding duplicated rules?
        # so this `rule_set` not necessary to be unordered which break property
        rule_set = set(
            rewrite
            for rule in grammar
            for rewrite in rewrite_rule(rule, subgrammar, normal_set(grammar))
        )
        subgrammar = subgrammar_table(rule_set)
        grammar = subgrammar[grammar[0].head]  # eliminate dead rules (which is
        # probably nonregular) and rebuild grammar property
    return grammar


def first_table(grammar):  # precisely, `first_set_table`
    old_first, first = None, {
        symbol: set() for symbol in set(rule.head for rule in grammar)
    }

    def rule_first(rule, partial_first):
        rule_set = set()
        for item in rule.body:
            if item.is_terminal():
                return rule_set | {item.terminal}
            if Regular.epsilon not in partial_first[item.nonterminal]:
                return rule_set | partial_first[item.nonterminal]
            rule_set |= partial_first[item.nonterminal] - {Regular.epsilon}
        return rule_set | {Regular.epsilon}

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
    start = first_table(grammar)[approx_symbol] - {Regular.epsilon}
    start_item = RuleItem(
        terminal=Regular.new_union(start), action=f"{counter} := {counter} + 1"
    )
    stop = first_table(
        tuple(
            ProductionRule(rule.guard, rule.head, tuple(reversed(rule.body)))
            for rule in grammar
        )
    )[approx_symbol] - {Regular.epsilon}
    stop_item = RuleItem(
        terminal=Regular.new_union(stop), action=f"{counter} := {counter} - 1"
    )
    approx_item = RuleItem(nonterminal=approx_symbol)
    return (
        ProductionRule(
            f"{counter} = 0", approx_symbol, (RuleItem(terminal=Regular.epsilon),)
        ),
        ProductionRule(f"{counter} >= 0", approx_symbol, (start_item, approx_item)),
        ProductionRule(f"{counter} > 0", approx_symbol, (stop_item, approx_item)),
        ProductionRule(
            f"{counter} > 0",
            approx_symbol,
            # according to paper we should use a disjoin "other" terminal
            # instead of a wildcard, however it is impossible to create a
            # complement regular of arbitrary regular set. `Regular.new_exclude`
            # only works on single byte (i.e. `exact`) set
            # although in paper and dyck we actually only need to exclude exact
            # byte, using wildcard accompanied by low priority should be
            # effectively equivalent, while still support `start` and `stop` to
            # contain complex regular. so why not
            # TODO add priority
            (RuleItem(terminal=Regular.wildcard), approx_item),
        ),
    )


def eliminate_idle(grammar):
    def iteration(grammar):
        subgrammar = subgrammar_table(grammar)
        for rule in grammar:
            assert rule.is_regular()
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
                            terminal=inline_last.terminal,
                            nonterminal=inline_last.nonterminal,
                            action=compose_action(
                                inline_last.action, rule.body[0].action
                            ),
                        ),
                    ),
                )

    old_grammar = None
    while grammar != old_grammar:
        old_grammar, grammar = grammar, tuple(iteration(grammar))
    return grammar


def optimize(grammar, extraction_grammar):
    subgrammar = subgrammar_table(grammar)
    normal = normal_set(grammar)
    approx_list = tuple(
        rule
        for symbol in subgrammar  # for nonterminal in extraction assert to be
        # normal
        if symbol not in normal
        for rule in approx(subgrammar[symbol])
    )
    grammar = (
        extraction_grammar
        + tuple(rule for rule in grammar if rule.head in normal)
        + approx_list
    )
    # assert every nonterminal remain in `grammar` is normal, including the
    # original ones from extraction
    # so `grammar` can be passed into `regularize` entirely
    return eliminate_idle(regularize(grammar))


class Regular:
    def __init__(self, exact=None, concat=None, union=None, star=None, repr_str=None):
        assert (
            len(tuple(arg for arg in (exact, concat, union, star) if arg is not None))
            == 1
        )
        assert (
            exact is None
            or isinstance(exact, set)
            and exact
            and all(isinstance(byte, int) and 0 <= byte < 256 for byte in exact)
        )
        assert concat is None or all(isinstance(part, Regular) for part in concat)
        assert (
            union is None
            or isinstance(union, set)
            and union
            and all(isinstance(variant, Regular) for variant in union)
        )
        assert star is None or isinstance(star, Regular)
        self.exact = exact
        self.concat = concat
        self.union = union
        self.star = star
        self.repr_str = repr_str

    @staticmethod
    def new_literal(byte_seq):
        assert isinstance(byte_seq, bytes)
        assert byte_seq  # do not accept empty literal, use Regular.epsilon
        if len(byte_seq) == 1:
            return Regular(exact={byte_seq[0]})
        return Regular(concat=tuple(Regular(exact={byte}) for byte in byte_seq))

    @staticmethod
    def new_exclude(opposite):
        assert isinstance(opposite, Regular)
        assert opposite.is_exact()
        return Regular(
            exact=Regular.wildcard.exact - opposite.exact,
            repr_str="[^{opposite.exact_repr}]",
        )

    @staticmethod
    def new_union(variant_set):
        assert variant_set
        if len(variant_set) == 1:
            return tuple(variant_set)[0]

        union_varaint = set(
            inner_variant
            for variant in variant_set
            if variant.is_union()
            for inner_variant in variant.union
        )
        exact_variant = (
            {
                Regular(
                    exact=set(
                        byte
                        for variant in variant_set
                        if variant.is_exact()
                        for byte in variant.exact
                    )
                )
            }
            if any(variant.is_exact() for variant in variant_set)
            else set()
        )
        other_variant = set(
            variant
            for variant in variant_set
            if not variant.is_union() and not variant.is_exact()
        )
        return Regular(union=union_varaint | exact_variant | other_variant)
        # assert at least one of above is not empty
        # (will be asserted in __init__)

    def __str__(self):
        if self.repr_str:
            return self.repr_str
        if self.is_exact():
            if len(self.exact) == 1:
                return chr(tuple(self.exact)[0])
            exact = "".join(chr(byte) for byte in self.exact)
            return f"[{exact}]"
        if self.is_concat():
            # this is going to be looking sooooo bad for byte string literal
            return "".join(f"({part})" for part in self.concat)
        if self.is_union():
            return "|".join(f"({variant})" for variant in self.union)
        if self.is_star():
            return f"({self.star})*"
        # unreachable

    def __hash__(self):
        exact = self.exact and frozenset(self.exact)
        union = self.union and frozenset(self.union)
        return hash((exact, self.concat, union, self.star))

    def __eq__(self, other):
        return isinstance(other, Regular) and (
            self.exact,
            self.concat,
            self.union,
            self.star,
        ) == (other.exact, other.concat, other.union, other.star)

    def is_exact(self):
        return self.exact is not None

    def is_union(self):
        return self.union is not None

    def is_concat(self):
        return self.concat is not None

    def is_star(self):
        return self.star is not None


# borrow concat corner case for epsilon
# consider make a dedicate argument if not work any more
Regular.epsilon = Regular(concat=(), repr_str="(eps)")
Regular.wildcard = Regular(exact=set(range(256)), repr_str=".")


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

symbol_b = RuleItem(nonterminal="B")
symbol_v = RuleItem(nonterminal="V")
varstring = (
    ProductionRule(None, "S", (symbol_b, symbol_v)),
    ProductionRule(
        None,
        "B",
        (RuleItem(terminal=Regular.new_literal(b"0"), action="c := c * 2"), symbol_b),
    ),
    ProductionRule(
        None,
        "B",
        (
            RuleItem(terminal=Regular.new_literal(b"1"), action="c := c * 2 + 1"),
            symbol_b,
        ),
    ),
    ProductionRule(None, "B", (RuleItem(terminal=Regular.new_literal(b" ")),)),
    ProductionRule(
        "c > 0",
        "V",
        (RuleItem(terminal=Regular.wildcard, action="c := c - 1"), symbol_v),
    ),
    ProductionRule("c = 0", "V", (RuleItem(terminal=Regular.epsilon),)),
)
varstring_extraction = (
    ProductionRule(None, "X", (symbol_b, RuleItem(nonterminal="V", action="vstr"))),
)
dyck = (
    ProductionRule(None, "S", (RuleItem(terminal=Regular.epsilon),)),
    ProductionRule(None, "S", (RuleItem(nonterminal="I"), RuleItem(nonterminal="S"))),
    ProductionRule(
        None,
        "I",
        (
            RuleItem(terminal=Regular.new_literal(b"[")),
            RuleItem(nonterminal="S"),
            RuleItem(terminal=Regular.new_literal(b"]")),
        ),
    ),
)
dyck_extraction = (
    ProductionRule(
        None,
        "X",
        (
            RuleItem(terminal=Regular.new_literal(b"[")),
            RuleItem(nonterminal="S", action="param"),
            RuleItem(terminal=Regular.new_literal(b"]")),
            RuleItem(nonterminal="S"),
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
        # self.assertEqual(
        #     first_table(varstring),
        #     {
        #         "S": {"0", "1", " "},
        #         "B": {"0", "1", " "},
        #         "V": {Regular.wildcard, Regular.epsilon},
        #     },
        # )
        # self.assertEqual(first_table(dyck), {"S": {"[", Regular.epsilon}, "I": {"["}})


if __name__ == "__main__":
    for rule in optimize(varstring, varstring_extraction):
        print(rule)
    print()
    for rule in optimize(dyck, dyck_extraction):
        print(rule)
