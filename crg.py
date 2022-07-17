"""
crg.py: Counting Regular Grammars implementation
(subtitle: how to write Python like SQL)

The main interface is `optimize(grammar, extraction_grammar)`, which do all 
required transformation from a frontend context free grammar to CRG.

`grammar` and `extraction_grammar` has the same representation, which is a tuple
of `ProductionRule`. The constructing arguments of `ProductionRule` and related
`RuleItem` are asserted in `__init__`. For `extraction_grammar` it must also
follow grammar convention, which is nonempty, and the first rule's head must be
start symbol. For `grammar` there is no such requirement since it does not 
define start symbol.

The `guard` argument is a dict whose key is variable identifier, and value is 
2-tuple (low, high), to declare a `low <= variable < high` guard. One of 
low/high bound may be `None` if it's a single side guard. Some example:
{x: (0, 1)} -> x == 0
{x: (1, None)} -> x >= 1, or x > 0 (the two are equal for integer x)
{x: (1, None), y: (None, 1)} -> x > 0 && y < 1
{} -> always true
(TODO: variable as bound?)

(TODO: define `action` representation)

The result of `optimize` is a generator of CRG rules, i.e. 6-tuple
(source state, guard, priority, regular, action, target state). Each state is an 
nonterminal symbol in CRG and is also a state in CA. Initial state is the first 
generated one. The `None` target state is the accepted one.

Notice that the rules produced by `optimize` is "nondeterministic", thus cannot
be directly executed in a pratical way. To be specific, at a time a subset of
rules of a source state may be enabled by current counter values. One possible
work flow is to postprocess the rule list with `gen.relevant`, which is the way 
adpoted by cli's `ca` command. See `gen` module document in advance.
"""
import gen
from object import (
    ProductionRule,
    Regular,
    RuleItem,
    compose_action,
    merge_predicate,
    varstring,
    extr_varstring,
    dyck,
    extr_dyck,
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
        if all(rule.is_triple() for rule in subgrammar[symbol])
    }

    # iteration helper, return True for `symbol` which is normal under
    # condition #2 and can only reach nonterminal in partial normal set
    # `partial_set`
    def condition2(symbol, partial_set):
        return symbol not in (
            item.nonterminal
            for rule in grammar
            if rule.head == symbol
            for item in rule.body[:-1]
            if item.is_nonterminal()
        ) and all(
            reachable_symbol in partial_set
            and symbol not in reachable[reachable_symbol]
            for reachable_symbol in reachable[symbol]
        )

    while partial_set != old_set:
        old_set, partial_set = partial_set, {
            symbol
            for symbol in subgrammar
            if symbol in partial_set or condition2(symbol, partial_set)
        }
    return partial_set


def regularize(grammar):
    # instead of assert normal invariant in every iteration, we only do it once
    # before looping
    # this is because a transformation from X -> aY1Y2...X to X -> aX';
    # X' -> Y1Y2...X breaks normal assertion: now X' is reachable from X while
    # X is also reachable from X', make them not normal
    # for now i choose to believe that the procedure suppose to work as long as
    # initially normal invariant holds
    normal = normal_set(grammar)
    assert all(rule.head in normal for rule in grammar)
    assert all(
        rule.body[0].is_terminal() or rule.body[0].nonterminal in normal
        for rule in grammar
    )

    def rewrite_rule(
        rule,
        subgrammar,
    ):
        if rule.is_triple():
            yield rule
            return
        if rule.body[0].is_nonterminal():  # and is normal
            symbol = rule.body[0].nonterminal
            action = rule.body[0].action
            if not all(rule.is_triple() for rule in subgrammar[symbol]):
                # the rewrite procedure under this condition proposed by paper
                # is only working when subgrammar is regular already
                # however it is guaranteed that the subgrammar will eventually
                # become regular, so just save it for following iteration
                yield rule
                return

            rename = RuleItem.new_nonterminal(symbol)
            yield ProductionRule(
                rule.head, rule.guard, rule.priority, (RuleItem(nonterminal=rename),)
            )

            rename_grammar = tuple(
                rule.rewrite(symbol, rename) for rule in subgrammar[symbol]
            )
            for subgrammar_nonterminal in set(rule.head for rule in rename_grammar):
                if subgrammar_nonterminal == rename:
                    continue
                rename_nonterminal = RuleItem.new_nonterminal(subgrammar_nonterminal)
                rename_grammar = tuple(
                    rule.rewrite(subgrammar_nonterminal, rename_nonterminal)
                    for rule in rename_grammar
                )

            for rename_rule in rename_grammar:
                if not rename_rule.is_terminating():
                    yield rename_rule
                # according to paper's procedure any action following "Y1" (i.e.
                # `rule.body[0]`) would be discard. to avoid it, we modified the
                # procedure here, to compose Y1's action into terminating rule's
                # terminal's action, only then append Y2Y3... to the rule
                else:
                    assert len(rename_rule.body) == 1
                    assert rename_rule.body[0].is_terminal()
                    yield ProductionRule(
                        rename_rule.head,
                        rename_rule.guard,
                        rename_rule.priority,
                        (
                            RuleItem(
                                terminal=rename_rule.body[0].terminal,
                                action=compose_action(
                                    rename_rule.body[0].action, action
                                ),
                            ),
                            *rule.body[1:],
                        ),
                    )
        else:  # rule.body[0] is terminal
            # not assert n > 2 here, there is a silly case where rule body
            # contains two terminal symbols
            name = RuleItem.new_nonterminal(rule.head)
            yield ProductionRule(
                rule.head,
                rule.guard,
                rule.priority,
                (rule.body[0], RuleItem(nonterminal=name)),
            )
            yield ProductionRule(name, {}, rule.priority, rule.body[1:])

    subgrammar = subgrammar_table(grammar)
    while not all(rule.is_triple() for rule in grammar):
        # can we easily avoid yielding duplicated rules?
        # so this `rule_set` not necessary to be unordered which break property
        rule_set = set(
            rewrite for rule in grammar for rewrite in rewrite_rule(rule, subgrammar)
        )
        subgrammar = subgrammar_table(rule_set)
        grammar = subgrammar[grammar[0].head]  # eliminate dead rules (which is
        # probably not in triple form) and rebuild grammar property
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
    start = first_table(grammar)[approx_symbol] - {Regular.epsilon}
    stop = first_table(
        tuple(
            ProductionRule(
                rule.head, rule.guard, rule.priority, tuple(reversed(rule.body))
            )
            for rule in grammar
        )
    )[approx_symbol] - {Regular.epsilon}

    counter = RuleItem.new_counter()
    approx_item = RuleItem(nonterminal=approx_symbol)
    return (
        ProductionRule(
            approx_symbol,
            {counter: (0, 1)},
            ProductionRule.default_priority,
            (RuleItem(terminal=Regular.epsilon),),
        ),
        ProductionRule(
            approx_symbol,
            {counter: (0, None)},
            ProductionRule.default_priority,
            (
                RuleItem(
                    terminal=Regular.new_union(start),
                    action=(("imm", "$1", 1), ("add", counter, counter, "$1")),
                ),
                approx_item,
            ),
        ),
        ProductionRule(
            approx_symbol,
            {counter: (1, None)},
            ProductionRule.default_priority,
            (
                RuleItem(
                    terminal=Regular.new_union(stop),
                    action=(("imm", "$1", 1), ("sub", counter, counter, "$1")),
                ),
                approx_item,
            ),
        ),
        ProductionRule(
            approx_symbol,
            {counter: (1, None)},
            # according to paper we should use a disjoin "other" terminal
            # instead of a wildcard, however in this implementation, we are not
            # yet supporting construct complement regular for arbitrary regular,
            # because this pattern seems not appear in protocol specification.
            #
            # `Regular.new_exclude` only works on single byte (i.e. `exact`) set
            # although in paper and dyck we actually only need to exclude exact
            # byte, using wildcard accompanied by low priority should be
            # effectively equivalent, while still support `start` and `stop` to
            # contain complex regular. so why not
            (ProductionRule.default_priority + 1) // 2,
            (RuleItem(terminal=Regular.wildcard), approx_item),
        ),
    )


def eliminate_idle(grammar):
    def iteration(grammar):
        for rule in grammar:
            assert rule.is_triple()
            if not rule.is_idle():
                yield rule
                continue
            assert rule.body[0].is_nonterminal()
            assert not rule.body[0].action  # because of the modification of
            # "idle" definition in this implementation
            for inline_rule in grammar:
                if inline_rule.head != rule.body[0].nonterminal:
                    continue
                yield ProductionRule(
                    rule.head,
                    merge_predicate(rule.guard, inline_rule.guard),
                    # both rule has at least 1 priority, so new priority will be
                    # at least 2 // 2 = 1
                    (rule.priority + inline_rule.priority) // 2,
                    # compare to paper, this impl omit action composing
                    # we have asserted there is no action above
                    inline_rule.body,
                )

    old_grammar = None
    while grammar != old_grammar:
        old_grammar, grammar = grammar, tuple(iteration(grammar))
    return subgrammar_table(grammar)[grammar[0].head]


def optimize(grammar, extraction_grammar):
    all_subgrammar = subgrammar_table(extraction_grammar + grammar)
    {name: RuleItem.new_nonterminal(name) for name in all_subgrammar}

    subgrammar = subgrammar_table(grammar)
    normal = normal_set(grammar)
    approx_list = tuple(
        rule
        for symbol in subgrammar  # nonterminal in extr not allow to approx
        if symbol in all_subgrammar and symbol not in normal
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
    for rule in eliminate_idle(regularize(grammar)):
        assert rule.body[0].is_terminal()
        if rule.is_terminating():
            nonterminal = None
        else:
            assert rule.body[1].is_nonterminal()
            nonterminal = rule.body[1].nonterminal
        yield (
            rule.head,
            rule.guard,
            rule.priority,
            rule.body[0].terminal,
            rule.body[0].action,
            nonterminal,
        )


def format_str(transition_list):
    def gen_line():
        # the field width is designed for 80 width terminal, only keep toy
        # transitions in mind
        # real world transitions are too complex to be printed nicely
        yield "{:8}{:16}{:4}{:12}{:32}{:8}".format(
            "Source", "Guard", "Pri", "Regular", "Action", "Target"
        )
        for source, guard, priority, regular, action, target in transition_list:
            target = target or "(accept)"
            yield (
                f"{source:8}{gen.guard_str(guard):16}{priority:<4}"
                f"{str(regular):12}{gen.action_str(action):32}{target:8}"
            )

    return "\n".join(gen_line())


if __name__ == "__main__":
    print(format_str(optimize(varstring, extr_varstring)))
    print()
    print(format_str(optimize(dyck, extr_dyck)))
