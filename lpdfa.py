"""
lpdfa.py: Labeled Priority Deterministic Finite Automata implementation.
"""
from crg import Regular, action_str


class State:
    # byte table: {[0-255] | eps => a set of `State` instance}
    # `ahead` is the decision ahead with maximum priority, not include `decision`
    # this enables LPDFA exit immediately when accepting maximum priority
    # decision
    # decision: (priority, action, target)
    def __init__(self, byte_table, ahead=None, decision=None):
        assert byte_table == "invalid" or all(
            target_set for target_set in byte_table.values()
        )
        if not ahead and byte_table != "invalid" and byte_table:
            ahead = max(
                (
                    target.as_ahead()
                    for target_set in byte_table.values()
                    for target in target_set
                ),
                # no `key` argument because tuple always compare from the 1st
                # component. all `max(decisions)` below is the same
                # no `default` argument. here we assert there is at least one
                # decision ahead
            )
        assert ahead or decision
        self.byte_table = byte_table
        self.ahead = ahead
        self.decision = decision

    # a special key in byte table for epsilon transition
    # the "inner" epsilon, not the "outer" one in CRG/CA i.e. `Regular.epsilon`
    epsilon = "eps"

    # use this instead of __str__
    def format_str(self, name_table):
        decision = ""
        if self.decision:
            priority, action, target = self.decision
            action = f" do {action_str(action)}" if action else ""
            decision = f" accept priority {priority}{action} goto {target}"

        def target(name_table, target_set):
            if len(target_set) == 1:
                (target,) = target_set
                return name_table[target]
            return "{" + ", ".join(name_table[target] for target in target_set) + "}"

        def byte_table_str(byte_table):
            wildcard_target = None
            if len(byte_table) == 256 + int(State.epsilon in byte_table):
                # or just use collections.Counter
                wildcard_target = max(
                    byte_table.values(),
                    key=lambda target_set: sum(
                        byte_target == target_set for byte_target in byte_table.values()
                    ),
                )
            for byte, target_set in byte_table.items():
                if target_set == wildcard_target:
                    continue
                if byte != State.epsilon:
                    byte = chr(byte).encode("unicode_escape").decode()
                yield f"  {byte:4} " + target(name_table, target_set)
            if wildcard_target:
                if self.byte_table.get(State.epsilon, None) == wildcard_target:
                    byte = "..e"
                else:
                    byte = "..."
                yield f"  {byte}  " + target(name_table, wildcard_target)

        maxpri = self.ahead[0] if self.ahead else "nil"  # instead of "None" so
        # it looks like intentional (or Ruby)
        return "\n".join(
            (
                f"state {name_table[self]} maxpri {maxpri}{decision}",
                *byte_table_str(self.byte_table),
            )
        )

    # here we finally give up override `__hash__` and `__eq__`, instead just
    # treat every instance of `State` different to each other
    # one possible benefit from custom hash & eq is to determine equivalent
    # states and merge them automatically when they are inserted into set
    # however:
    # * it is impossible to merge all equivalent states in this way, a global
    #   minimization pass is always necessary
    # * even worse it is prone to infinite recursive. although we could simply
    #   stop exploring after certain threshold and fallback to same hash / not
    #   eq, but that hardly merges anything due to automata's nature
    # * we also get a chance to assign name to states (not deployed by now
    #   though), and not need to worry about name broken when aliased states get
    #   merged and everyone but one get discarded

    def as_deterministic(self):
        assert all(
            byte != State.epsilon and len(target_set) == 1
            for byte, target_set in self.byte_table.items()
        )
        return {byte: target for byte, (target,) in self.byte_table.items()}

    def is_accepted(self):
        return self.decision is not None

    def ahead_priority(self):
        return self.ahead[0] if self.ahead else -1  # or 0?

    def as_ahead(self):
        assert self.ahead or self.decision
        if not self.decision:
            return self.ahead
        if not self.ahead:
            return self.decision
        return max(self.ahead, self.decision)

    def reachable(self):
        black_set, gray_set = set(), {self}  # borrow garbage collector terms
        while gray_set:
            black_set, gray_set = black_set | gray_set, {
                target
                for state in gray_set
                for target_set in state.byte_table.values()
                for target in target_set
                if target not in black_set
            }
        return black_set

    @staticmethod
    def new_regular(regular, target):
        if regular.is_exact():
            return State.new_exact(regular.exact, target)
        if regular.is_concat():
            return State.new_concat(regular.concat, target)
        if regular.is_union():
            return State.new_union(regular.union, target)
        if regular.is_star():
            return State.new_star(regular.star, target)
        # unreachable

    @staticmethod
    def new_exact(exact, target):
        return State({byte: {target} for byte in exact})

    @staticmethod
    def new_concat(concat, target):
        if not concat:
            return target
        return State.new_regular(concat[0], State.new_concat(concat[1:], target))

    @staticmethod
    def new_union(union, target):
        return State(
            {State.epsilon: {State.new_regular(regular, target) for regular in union}}
        )

    # secret method prepared for future implementation of pattern negation
    # (something like `(?!<pattern>).*`)
    @staticmethod
    def new_negate(negate, target):
        return State(
            {
                State.epsilon: {
                    State.new_star(Regular.wildcard, target),
                    # using an `ahead` instead of `decision`
                    # so the state adds no accepted decision and leads to the
                    # "real" failure by empty byte table
                    State.new_regular(negate, State({}, ahead=(-1, None, "phantom"))),
                }
            }
        )

    @staticmethod
    def new_star(star, target):
        inner_target = State({State.epsilon: {target}})
        inner = State.new_regular(star, inner_target)
        assert inner.priority == inner_target.priority
        inner_target.byte_table[State.epsilon] |= {inner}
        return inner

    def powerset(self):
        def epsilon_closure(subset):
            for state in subset:
                yield state
                yield from epsilon_closure(state.byte_table.get(State.epsilon, set()))

        def subset_move(subset, byte):
            for state in subset:
                yield from epsilon_closure(state.byte_table.get(byte, ()))

        self_set = frozenset(epsilon_closure({self}))
        # black/gray table:
        #   {a subset of self.reachable() => {byte => another subset}}
        # initial gray table is not a valid black table (epsilon byte is not
        # allowed after det.). but it will overwrite itself for sure
        black_table, gray_table = {}, {self_set: {State.epsilon: self_set}}
        while gray_table:
            black_table, gray_table = black_table | gray_table, {
                subset: {
                    byte: frozenset(subset_move(subset, byte))
                    for byte in set(
                        byte
                        for state in subset
                        for byte in state.byte_table
                        if byte != State.epsilon
                    )
                }
                for byte_table in gray_table.values()
                for subset in byte_table.values()
                if subset not in black_table
            }

        state_table = {
            subset: State(
                "invalid",
                # this priority should be equal to the derived one from byte
                # table. unfortunately byte table is not available at this time
                # so we have to set it manually
                ahead=max(
                    (
                        target.as_ahead()
                        for target_set in byte_table.values()
                        for target in target_set
                    ),
                    default=None,
                ),
                decision=max(
                    (state.decision for state in subset if state.decision), default=None
                ),
            )
            for subset, byte_table in black_table.items()
        }

        for subset, state in state_table.items():
            state.byte_table = {
                byte: {state_table[target]}
                for byte, target in black_table[subset].items()
            }
            # intuitively it is still necessary to maintain priority from the
            # begining NFA form, instead of construct values here
            # because this iteration is in arbitrary order while NFA was
            # strictly backward-constructed. but i don't know for sure
            assert state.byte_table or state.ahead is None
            assert not state.byte_table or state.ahead == max(
                (target.as_ahead() for target in state.as_deterministic().values())
                # assert not empty
            )
        return state_table[self_set]


def construct(transition_list):
    # first this is a divergance from paper: in paper the "fail" transition is
    # called epsilon rule, which happen on empty string. however a default
    # failure on empty string cannot "guarantee the LPDFA will always return
    # a value". so instead, we should fail on wildcard string (i.e. `.*`), which
    # indeed goto failure on anything unspecified
    # then, this is effectively equivalent to having a partial byte table, and
    # goto failure on any byte not present in the table. this is better in
    # practical for:
    # * early stopping, the processing stops at the first unknown byte, instead
    #   of consuming (while self looping on failure state) all remaining content
    # * no need to assign a special "lowest" priority to failure decision.
    #   (currently -1 is in unused `State.new_negate` as lowest priority ahead.)
    # * great improvement on automata constructing performance
    # i guess this is also the choice of original FlowSifter implementation,
    # which may explain why they made the mistake in paper text ^_^

    # notes for runtime: when meet unknown byte, if there is already decision
    # seen before, consider as a normal transition to that decision; otherwise
    # if no decision ever seen then goto failure

    # if not any(regular == Regular.epsilon for _, regular, *_ in transition_list):
    #     transition_list += ((0, Regular(star=Regular.wildcard), None, "fail"),)
    return State(
        {
            State.epsilon: {
                State.new_regular(
                    regular,
                    State({}, decision=(priority, action, target or "done")),
                )
                for priority, regular, action, target in transition_list
            }
        }
    ).powerset()


# misc
from unittest import TestCase


class TestLPDFA(TestCase):
    def test_powerset(self):
        qacc = State({}, decision=(1, None, "done"))
        q0 = State({0: {qacc}})
        s0 = q0.powerset()
        self.assertEqual(s0.ahead, (1, None, "done"))
        sacc = s0.as_deterministic()[0]
        self.assertTrue(sacc.is_accepted())
        self.assertFalse(sacc.byte_table)

        q1 = State({1: {q0, qacc}})
        s1 = q1.powerset()
        s0acc = s1.as_deterministic()[1]
        self.assertTrue(s0acc.is_accepted())
        sacc = s0acc.as_deterministic()[0]
        self.assertTrue(sacc.is_accepted())
        self.assertEqual(s1.reachable(), {s1, s0acc, sacc})

        q2 = State({State.epsilon: {q1}})
        q3 = State({3: {q2}})
        s3 = q3.powerset()
        s12 = s3.as_deterministic()[3]
        s0acc = s12.as_deterministic()[1]
        sacc = s0acc.as_deterministic()[0]
        self.assertEqual(s3.reachable(), {s3, s12, s0acc, sacc})

        # TODO write some real test cases


from crg import parse, dyck, extr_dyck, guard_str

if __name__ == "__main__":
    for source, config_list in parse(dyck, extr_dyck):
        for guard, transition_list in config_list:
            guard = f" if {guard_str(guard)}" if guard else ""
            state = construct(tuple(transition_list))
            reachable = state.reachable() - {state}
            name_table = {
                state: "s",
                **{s: str(i + 1) for i, s in enumerate(reachable)},
            }
            print(f"from {source}{guard}")
            for state in (state, *reachable):
                print(
                    "\n".join(
                        f"  {line}"
                        for line in state.format_str(name_table).splitlines()
                    )
                )
