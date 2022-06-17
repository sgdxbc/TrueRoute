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
import string


class RuleItem:
    def __init__(self, terminal=None, nonterminal=None, action=None):
        assert (terminal is None) != (nonterminal is None)
        assert terminal is None or isinstance(terminal, Regular)
        assert nonterminal is None or isinstance(nonterminal, str)
        action = action or ()
        assert isinstance(action, tuple)  # TODO and all item is Action instance
        self.terminal = terminal
        self.nonterminal = nonterminal
        self.action = action

    nonterminal_set = set()

    # this method operates on nonterminator names, not RuleItem objects
    # we allow nonterminator with same name assoicates to difference actions
    @classmethod
    def new_nonterminal(cls, name_hint):
        id = 0
        [name_hint, *postfix] = name_hint.split("^", maxsplit=1)
        if postfix:
            id = int(postfix[0]) + 1
        while f"{name_hint}^{id}" in cls.nonterminal_set:
            id += 1
        name = f"{name_hint}^{id}"
        cls.nonterminal_set.add(name)
        return name

    counter_count = 0

    @classmethod
    def new_counter(cls):
        counter = f"cnt_{cls.counter_count}"
        cls.counter_count += 1
        return counter

    def __str__(self):
        action = f" ({action_str(self.action)})" if self.action else ""
        if self.is_terminal():
            return f"/{self.terminal}/{action}"
        else:
            return f"{self.nonterminal}{action}"

    def __hash__(self):
        return hash((self.terminal, self.nonterminal, self.action))

    def __eq__(self, other):
        return isinstance(other, RuleItem) and (
            (self.terminal, self.nonterminal, self.action)
            == (other.terminal, other.nonterminal, other.action)
        )

    def is_terminal(self):
        return self.terminal is not None

    def is_nonterminal(self):
        return self.nonterminal is not None

    def rewrite(self, source, target):
        if self.is_terminal() or self.nonterminal != source:
            return self
        return RuleItem(nonterminal=target, action=self.action)


class ProductionRule:
    # although in paper demo snippet show guard before head, in their actual
    # implementation head goes first, which also looks better
    def __init__(self, head, guard, priority, body):
        assert isinstance(guard, dict) and all(
            isinstance(variable, str)
            and (low is None or isinstance(low, int))
            and (high is None or isinstance(high, int))
            for variable, (low, high) in guard.items()
        )
        assert isinstance(head, str)
        assert isinstance(priority, int)
        # do not accept empty production rule
        # epsilon should be considered as a special terminal symbol since we are
        # already use regex as terminal
        assert body and all(isinstance(item, RuleItem) for item in body)
        self.head = head
        self.guard = guard
        self.priority = priority
        self.body = body

    default_priority = 50

    def __str__(self):
        guard = f" ({guard_str(self.guard)})" if self.guard else ""
        body = " ".join(str(item) for item in self.body)
        return f"{self.head}{guard} {self.priority} -> {body}"

    def __hash__(self):
        return hash(
            (self.head, frozenset(self.guard.items()), self.priority, tuple(self.body))
        )

    def __eq__(self, other):
        return isinstance(other, ProductionRule) and (
            (self.head, self.guard, self.priority, self.body)
            == (other.head, other.guard, other.priority, other.body)
        )

    def is_terminating(self):
        return len(self.body) == 1 and self.body[0].is_terminal()

    def is_nonterminating(self):
        return (
            len(self.body) == 2
            and self.body[0].is_terminal()
            and self.body[1].is_nonterminal()
            and not self.body[1].action
        )

    def is_idle(self):
        return (
            len(self.body) == 1
            and self.body[0].is_nonterminal()
            # in section IV.E the paper presents an idle rule with trailing
            # action following nonterminal. however the procedures in paper is
            # not sufficient to convert such idle rule into CRG triple form.
            # hence we exclude trailing action from idle rule, force such form
            # to be regularized
            and not self.body[0].action
        )

    # here is mostly what paper called "regular" in section IV.A. because this
    # causes awful double meaning of "regular", i.e. regular rule and `Regular`
    # which is used as terminal symbol and others, the predicate is renamed to
    # `is_triple`, indicate that the rule is in the form of (terminal, action,
    # nonterminal/next state) triple (where some of the component may be
    # missing).
    # another difference is that `is_triple` includes (tolerances) idle rule
    # which is not included by paper's regular form
    def is_triple(self):
        return self.is_terminating() or self.is_nonterminating() or self.is_idle()

    def rewrite(self, source, target):
        return ProductionRule(
            target if self.head == source else self.head,
            self.guard,
            self.priority,
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
                (
                    rule.body[0],
                    RuleItem(nonterminal=name),
                ),
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
    start_item = RuleItem(
        terminal=Regular.new_union(start), action=(f"{counter} := {counter} + 1",)
    )
    stop_item = RuleItem(
        terminal=Regular.new_union(stop), action=(f"{counter} := {counter} - 1",)
    )
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
            (start_item, approx_item),
        ),
        ProductionRule(
            approx_symbol,
            {counter: (1, None)},
            ProductionRule.default_priority,
            (stop_item, approx_item),
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
            ProductionRule.default_priority // 2,
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
                    (rule.priority + inline_rule.priority) // 2,
                    # omit composing action in this implementation
                    # we have asserted there is no action above
                    inline_rule.body,
                )

    old_grammar = None
    while grammar != old_grammar:
        old_grammar, grammar = grammar, tuple(iteration(grammar))
    return subgrammar_table(grammar)[grammar[0].head]


def optimize(grammar, extraction_grammar):
    all_subgrammar = subgrammar_table(extraction_grammar + grammar)
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


# a relatively conversative string representation
def byte_str(byte):
    assert 0 <= byte < 256
    if chr(byte) in string.ascii_letters + string.digits:
        return chr(byte)
    if chr(byte) in {"[", "]", "\\", "/", "+", "*", "?", "(", ")"}:
        return "\\" + chr(byte)
    return f"\\x{byte:02x}"


class Regular:
    def __init__(self, exact=None, concat=None, union=None, star=None, repr_str=None):
        assert (
            len(tuple(arg for arg in (exact, concat, union, star) if arg is not None))
            == 1
        )
        assert (
            exact is None
            # in this implementation exact is a set of single byte, not single
            # byte which is probably expected
            # the rational is to support excluding pattern (i.e.
            # `[^<char set>]`) without a full support for negative lookahead
            # pattern (which is tentatively provided as `State.new_negate`). The
            # latter one is not presented in protocol specification (at least
            # HTTP protocol), and may require to be constructed into odd form of
            # automata. so it is dropped
            # as a result, i choose to extend the exact argument, into a special
            # single-byte subset of union argument, for:
            # * precisely indicate the regular that is able to be `exclude`,
            #   i.e. all `exact` ones
            # * happen to enable `State.new_exact` to generate more compact
            #   automata for `[<char set>]` and `[^<char set>]` patterns.
            #   however even not `powerset` will do the compaction
            or isinstance(exact, set)
            and exact
            and all(isinstance(byte, int) and 0 <= byte < 256 for byte in exact)
        )
        assert (
            concat is None
            or len(concat) != 1
            and all(isinstance(part, Regular) for part in concat)
        )
        assert (
            union is None
            or isinstance(union, set)
            and len(union) > 1
            and all(isinstance(variant, Regular) for variant in union)
        )
        assert star is None or isinstance(star, Regular)
        assert repr_str is None or isinstance(repr_str, str)
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
        return Regular(
            concat=tuple(Regular(exact={byte}) for byte in byte_seq),
            repr_str="".join(byte_str(byte) for byte in byte_seq),
        )

    @staticmethod
    def new_exclude(opposite):
        assert isinstance(opposite, Regular)
        assert opposite.is_exact()
        opposite_repr = opposite.repr_str or "".join(
            byte_str(byte) for byte in opposite.exact
        )
        return Regular(
            exact=Regular.wildcard.exact - opposite.exact,
            repr_str=f"[^{opposite_repr}]",
        )

    @staticmethod
    def new_union(variant_set, repr_str=None):
        if not variant_set:
            return Regular.epsilon

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
                    ),
                    repr_str=repr_str,  # a little bit tricky here...
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
        union = union_varaint | exact_variant | other_variant
        assert union
        return Regular(union=union, repr_str=repr_str) if len(union) > 1 else max(union)

    def __str__(self):
        if self.repr_str:
            return self.repr_str
        if self.is_exact():
            if len(self.exact) == 1:
                (exact,) = self.exact
                return byte_str(exact)
            if self.exact == Regular.wildcard.exact:
                return "."
            exact = "".join(byte_str(byte) for byte in self.exact)
            return f"[{exact}]"
        if self.is_concat():
            return "".join(str(part) for part in self.concat)
        if self.is_union():
            return "|".join(str(variant) for variant in self.union)
        if self.is_star():
            return f"{self.star}*"
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
        ) == (
            other.exact,
            other.concat,
            other.union,
            other.star,
        )

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
Regular.wildcard = Regular(exact=set(range(256)))


def merge_predicate(guard, another_guard):
    if not guard:
        return another_guard
    if not another_guard:
        return guard

    def merge_bound(bound, another_bound):
        low, high = bound
        another_low, another_high = another_bound
        if another_low is not None:
            low = max(low, another_low) if low is not None else another_low
        if another_high is not None:
            high = min(high, another_high) if high is not None else another_high
        return low, high

    return {
        variable: merge_bound(
            guard.get(variable, (None, None)), another_guard.get(variable, (None, None))
        )
        for variable in guard.keys() | another_guard.keys()
    }


def compose_action(action, another_action):
    if not action:
        return another_action
    if not another_action:
        return action
    return action + another_action


# tests, cli and shared assets


def guard_str(guard):
    if not guard:
        return "true"

    def bound_str(bound):
        low, high = bound
        low = str(low) if low is not None else ""
        high = str(high) if high is not None else ""
        return f"{low}..{high}"

    return "; ".join(
        f"{variable} in {bound_str(bound)}" for variable, bound in guard.items()
    )


def action_str(action):
    return "; ".join(str(step) for step in action)


# shared asset for tests and main

symbol_b = RuleItem(nonterminal="B")
symbol_v = RuleItem(nonterminal="V")
varstring = (
    ProductionRule("S", {}, ProductionRule.default_priority, (symbol_b, symbol_v)),
    ProductionRule(
        "B",
        {},
        ProductionRule.default_priority,
        (
            RuleItem(terminal=Regular.new_literal(b"0"), action=("c := c * 2",)),
            symbol_b,
        ),
    ),
    ProductionRule(
        "B",
        {},
        ProductionRule.default_priority,
        (
            RuleItem(terminal=Regular.new_literal(b"1"), action=("c := c * 2 + 1",)),
            symbol_b,
        ),
    ),
    ProductionRule(
        "B",
        {},
        ProductionRule.default_priority,
        (RuleItem(terminal=Regular.new_literal(b" ")),),
    ),
    ProductionRule(
        "V",
        {"c": (1, None)},
        ProductionRule.default_priority,
        (RuleItem(terminal=Regular.wildcard, action=("c := c - 1",)), symbol_v),
    ),
    ProductionRule(
        "V",
        {"c": (0, 1)},
        ProductionRule.default_priority,
        (RuleItem(terminal=Regular.epsilon),),
    ),
)
extr_varstring = (
    ProductionRule(
        "X",
        {},
        ProductionRule.default_priority,
        (symbol_b, RuleItem(nonterminal="V", action=("vstr",))),
    ),
)
dyck = (
    ProductionRule(
        "S",
        {},
        ProductionRule.default_priority,
        (RuleItem(terminal=Regular.epsilon),),
    ),
    ProductionRule(
        "S",
        {},
        ProductionRule.default_priority,
        (RuleItem(nonterminal="I"), RuleItem(nonterminal="S")),
    ),
    ProductionRule(
        "I",
        {},
        ProductionRule.default_priority,
        (
            RuleItem(terminal=Regular.new_literal(b"[")),
            RuleItem(nonterminal="S"),
            RuleItem(terminal=Regular.new_literal(b"]")),
        ),
    ),
)
extr_dyck = (
    ProductionRule(
        "X",
        {},
        ProductionRule.default_priority,
        (
            RuleItem(terminal=Regular.new_literal(b"["), action=("p := pos()",)),
            RuleItem(nonterminal="S", action=("_ := param(p)",)),
            RuleItem(terminal=Regular.new_literal(b"]")),
            RuleItem(nonterminal="S"),
        ),
    ),
)


def format_str(transition_list):
    def gen():
        # the field width is designed for 80 width terminal, only keep toy
        # transitions in mind
        # real world transitions are too complex to be printed nicely
        yield "{:8}{:24}{:4}{:12}{:24}{:8}".format(
            "Source", "Guard", "Pri", "Regular", "Action", "Target"
        )
        for source, guard, priority, regular, action, target in transition_list:
            target = target or "(accept)"
            yield (
                f"{source:8}{guard_str(guard):24}{priority:<4}"
                f"{str(regular):12}{action_str(action):24}{target:8}"
            )

    return "\n".join(gen())


if __name__ == "__main__":
    print(format_str(optimize(varstring, extr_varstring)))
    print()
    print(format_str(optimize(dyck, extr_dyck)))
