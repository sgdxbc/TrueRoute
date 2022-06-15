"""
lpdfa.py: Labeled Priority Deterministic Finite Automata implementation.
"""
from crg import Regular


class State:
    # byte table: {[0-255] | eps => a set of `State` instance}
    # priority is the max priority of decisions ahead, NOT include `decision`
    # this enables LPDFA exit immediately when accepting maximum priority
    # decision
    def __init__(self, byte_table, priority=None, decision=None):
        assert byte_table or decision or priority is not None
        assert all(target_set for target_set in byte_table.values())
        if priority is None:
            priority = (
                max(
                    max(target.priority, target.decision[0])
                    if target.decision
                    else target.priority
                    for target_set in byte_table.values()
                    for target in target_set
                )
                if byte_table
                else -1
            )
        self.priority = priority
        self.byte_table = byte_table
        self.decision = decision

    # a special key in byte table for epsilon transition
    # the "inner" epsilon, not the "outer" one in CRG/CA
    epsilon = "eps"

    # use this instead of __str__
    def format_str(self, name_table):
        decision = ""
        if self.decision:
            priority, action, target = self.decision
            action = f" do {action}" if action else ""
            decision = f" accept priority {priority}{action} goto {target}"

        def target(name_table, target_set):
            if len(target_set) == 1:
                return name_table[tuple(target_set)[0]]
            return "{" + ", ".join(name_table[target] for target in target_set) + "}"

        def byte_table_str(byte_table):
            wildcard_target = None
            if len(byte_table) == 256 and State.epsilon not in byte_table:
                # or just use collections.Counter
                wildcard_target = sorted(
                    byte_table.values(),
                    key=lambda target_set: sum(
                        byte_target == target_set for byte_target in byte_table.values()
                    ),
                    reverse=True,
                )[0]
            for byte, target_set in byte_table.items():
                if target_set == wildcard_target:
                    continue
                if byte != State.epsilon:
                    byte = chr(byte).encode("unicode_escape").decode()
                yield f"  {byte:4} " + target(name_table, target_set)
            if wildcard_target:
                yield "  ...  " + target(name_table, wildcard_target)

        return "\n".join(
            (
                f"state {name_table[self]} maxpri {self.priority}{decision}",
                *byte_table_str(self.byte_table),
            )
        )

    def __hash__(self):
        return hash(
            (
                self.priority,
                frozenset(
                    (byte, target)
                    for byte, target_set in self.byte_table.items()
                    for target in target_set
                ),
                self.decision,
            )
        )

    def __eq__(self, other):
        return isinstance(other, State) and (
            (self.priority, self.byte_table, self.decision)
            == (other.priority, other.byte_table, other.decision)
        )

    def is_deterministic(self):
        return all(
            byte != State.epsilon and len(target_set) == 1
            for byte, target_set in self.byte_table.items()
        )

    def is_accepted(self):
        return self.decision is not None

    def reachable(self):
        black_set, gray_set = set(), {self}  # borrow garbage collector terms
        while gray_set:
            black_set, gray_set = black_set | gray_set, {
                target
                for state in gray_set
                for target_set in state.byte_table.values()
                for target in target_set
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
                {},
                # here we must manually set priority instead of automatically
                # derived from decision, because there is a chance to have a
                # higher priority decision ahead, than all decision accepted by
                # this state locally
                # this priority should be equal to the derived one from byte
                # table. unfortunately byte table is not available at this time
                priority=max(state.priority for state in subset),
                # a little bit hacky here: use `next(iter(sorted(...)), state)`
                # to get the first item of sorted list, or `state` if list is
                # empty
                decision=next(
                    iter(
                        sorted(
                            (state for state in subset if state.decision),
                            key=lambda state: state.priority,
                            reverse=True,
                        )
                    ),
                    State({}, priority=0, decision=None),
                ).decision,
            )
            for subset in black_table
        }

        for subset, state in state_table.items():
            state.byte_table = {
                byte: {state_table[target]}
                for byte, target in black_table[subset].items()
            }
            assert state.byte_table or state.priority == -1
            # TODO: do complete assertion (including decision priority part)
            # intuitively it is still necessary to maintain priority from the
            # begining NFA form, instead of construct values here
            # because this iteration is in arbitrary order while NFA was
            # strictly backward-constructed. but i don't know for sure
            assert not state.byte_table or state.priority >= max(
                target.priority
                for target_set in state.byte_table.values()
                for target in target_set
            )
        return state_table[self_set]


def construct(transition_list):
    if not any(regular == Regular.epsilon for _, regular, *_ in transition_list):
        transition_list += ((0, Regular.epsilon, None, "fail"),)
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
        self.assertTrue(all(s.is_deterministic() for s in s0.reachable()))
        self.assertEqual(s0.priority, 1)
        sacc = tuple(s0.byte_table[0])[0]
        self.assertTrue(sacc.is_accepted())
        self.assertFalse(sacc.byte_table)

        q1 = State({1: {q0, qacc}})
        s1 = q1.powerset()
        self.assertTrue(all(s.is_deterministic() for s in s1.reachable()))
        s0acc = tuple(s1.byte_table[1])[0]
        self.assertTrue(s0acc.is_accepted())
        sacc = tuple(s0acc.byte_table[0])[0]
        self.assertTrue(sacc.is_accepted())

        q2 = State({State.epsilon: {q1}})
        q3 = State({3: {q2}})
        s3 = q3.powerset()
        self.assertTrue(all(s.is_deterministic() for s in s3.reachable()))
        s12 = tuple(s3.byte_table[3])[0]
        self.assertEqual(tuple(s12.byte_table[1])[0], s0acc)

        # TODO write some real test cases


from crg import parse, dyck, extr_dyck, guard_str

if __name__ == "__main__":
    for source, config_list in parse(dyck, extr_dyck):
        for guard, transition_list in config_list:
            state = construct(tuple(transition_list))
            reachable = state.reachable()
            name_table = {s: str(i) for i, s in enumerate(reachable)}
            print(f"source {source} guard {guard_str(guard)} start {name_table[state]}")
            for state in reachable:
                print(state.format_str(name_table))
