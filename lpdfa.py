"""
lpdfa.py: Labeled Priority Deterministic Finite Automata implementation.
"""


class State:
    def __init__(self, byte_table, priority=None, decision=None):
        assert byte_table or priority and decision
        assert all(target_set for target_set in byte_table.values())
        priority = priority or max(
            target.priority for target_set in byte_table for target in target_set
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
            action, target = self.decision
            action = f" do {action}" if action else ""
            decision = f"\n  accept{action} {target}"

        def target(name_table, target_set):
            if len(target_set) == 1:
                return name_table[tuple(target_set)[0]]
            return "{" + ", ".join(name_table[target] for target in target_set) + "}"

        return "\n".join(
            (
                f"state {name_table[self]} priority {self.priority}{decision}",
                *(
                    f"  {chr(byte) if byte != State.epsilon else State.epsilon:4} "
                    + target(name_table, target_set)
                    for byte, target_set in self.name_table
                ),
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
        return all(len(target_set) == 1 for target_set in self.byte_table.values())

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
        raise NotImplementedError


def construct(transition_list):
    pass
