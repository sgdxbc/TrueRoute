import string

# universally, step[1] is place variable name, i.e. name before `:=`
def compile_action(step, var_id):
    # user defined extraction
    if step[0] == "trace":  # preferred
        return 0x70, var_id(step[1]), var_id(step[2])
    if step[0] == "token":  # keep compatibility with FlowSifter
        return 0x70, var_id(step[1]), var_id(step[2])

    # stock operation
    if step[0] == "imm":  # immediate number
        return 0x10, var_id(step[1]), step[2]
    if step[0] == "add":
        return 0x11, var_id(step[1]), var_id(step[2]), var_id(step[3])
    if step[0] == "sub":
        return 0x12, var_id(step[1]), var_id(step[2]), var_id(step[3])
    if step[0] == "mul":
        return 0x13, var_id(step[1]), var_id(step[2]), var_id(step[3])

    if step[0] == "pos":
        return 0x50, var_id(step[1])
    if step[0] == "bounds":
        return 0x51, var_id(step[1]), var_id(step[2]), var_id(step[3])
    if step[0] == "skip":
        return 0x52, var_id(step[1]), var_id(step[2])
    if step[0] == "drop_tail":
        return 0x53, var_id(step[1])
    if step[0] == "skip_to":
        return 0x54, var_id(step[1]), var_id(step[2])
    if step[0] == "notify":
        return 0x55, var_id(step[1]), var_id(step[2])
    if step[0] == "cur_byte":
        return 0x56, var_id(step[1])
    if step[0] == "cur_double_byte":
        return 0x57, var_id(step[1])
    if step[0] == "getnum":
        return 0x58, var_id(step[1])
    if step[0] == "gethex":
        return 0x59, var_id(step[1])
    if step[0] == "save":  # not sure why get commented in FlowSifter but seems
        # really fun
        return 0x5A, var_id(step[1]), var_id(step[2]), var_id(step[3])

    assert False, f"cannot compile operation {step[0]}"


def action_str(action):
    def step_str(step):
        op, *arg = step
        if not arg:
            return op
        return op + " " + ", ".join(str(a) for a in arg)

    return "; ".join(step_str(step) for step in action)


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


class RuleItem:
    def __init__(self, terminal=None, nonterminal=None, action=None):
        assert (terminal is None) != (nonterminal is None)
        assert terminal is None or isinstance(terminal, Regular)
        assert nonterminal is None or isinstance(nonterminal, str)
        action = action or ()
        assert isinstance(action, tuple) and (
            all(isinstance(step, tuple) and isinstance(step[0], str) for step in action)
        )
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
        counter = f"cnt${cls.counter_count}"
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
        assert isinstance(priority, int) and priority > 0
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


# a relatively conversative string representation
def byte_str(byte):
    assert 0 <= byte < 256
    if chr(byte) in string.ascii_letters + string.digits:
        return chr(byte)
    if chr(byte) in {"[", "]", "\\", "/", "+", "*", "?", "(", ")"}:
        return "\\" + chr(byte)
    return f"\\x{byte:02x}"


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
            RuleItem(
                terminal=Regular.new_literal(b"0"),
                action=(("imm", "$2", 2), ("mul", "c", "c", "$2")),
            ),
            symbol_b,
        ),
    ),
    ProductionRule(
        "B",
        {},
        ProductionRule.default_priority,
        (
            RuleItem(
                terminal=Regular.new_literal(b"1"),
                action=(
                    ("imm", "$2", 2),
                    ("mul", "c", "c", "$2"),
                    ("imm", "$2", 1),
                    ("add", "c", "c", "$2"),
                ),
            ),
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
        (
            RuleItem(
                terminal=Regular.wildcard,
                action=(("imm", "$2", 1), ("sub", "c", "c", "$2")),
            ),
            symbol_v,
        ),
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
        (symbol_b, RuleItem(nonterminal="V", action=(("vstr", "_", "p"),))),
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
            RuleItem(terminal=Regular.new_literal(b"["), action=(("pos", "p"),)),
            RuleItem(nonterminal="S", action=(("param", "_", "p"),)),
            RuleItem(terminal=Regular.new_literal(b"]")),
            RuleItem(nonterminal="S"),
        ),
    ),
)
