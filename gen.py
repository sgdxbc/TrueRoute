"""
gen.py: Serialization protocol for Counting Automata. This is the input prepared
for simulated CA implementation.

TODO: main interface

----

Side node about LPDFA indexing

In paper section VI.B three strategies are proposed for LPDFA indexing: index by
rule, index by predicate (guard), and optimal decision tree. This implementation
provides procedures of the first two. Index by rule is actually used to generate
serialized CA following the paper author's choice, which is behind the main
interface. 

Alternatively, `relevant(transition_list)` do index by predicate: it generates
a list of `(source state, configuration generator)`, where the second element
generates `(guard, headless transition generator)`, that go through all `guard` 
that is possible to be true (e.g. will exclude "x > 3 && x == 0"), along with 
all the rules that is enabled with the guard. Use each of the inner-most 
generator as `lpdfa.construct`'s input will give us every LPDFA that is 
"relevant" to this (source, guard) configuration, according to paper's 
definition.

Notice that the same (headless) CRG rule map appear multiple times in pair with
several `guard`. For example, consider following rules:
S [x < 1; y >= 0] -> ...
S [x < 3; y == 0] -> ...
S -> ...
The guard for each relevant LPDFA would be:
     x < 1;      y < 0      rule 3
     x < 1; 0 <= y < 1 (i.e. y == 1 for int y) rule 1, rule 2, rule 3
     x < 1; 1 <= y          rule 1, rule 3
1 <= x < 3; (all y again)   rule 2, rule 3 or rule 3 depends on y
3 <= x    ; (all y again)   rule 3

The `relevant` is still provided because:
* In the case if we will switch to index by predicate in the future.
* Currently it is used to implement index by rule.
* The splitting is more intuitive and is good for presentation, it is actually
  implemented before LPDFA, and used by cli's `ca` command. (Although some 
  duplication may occur, like the LPDFA for sole rule 3 above.) In comparision
  a splitting of index by rule for the above example would be (suppose p1 means
  x < 1 && y >= 0, !p1 means the opposite):
 p1 &  p2 &  p3          x < 1; 0 <= y < 1      rule 1, rule 2, rule 3
!p1 &  p2 &  p3     1 <= x < 3; 0 <= y < 1      rule 2, rule 3
 p1 & !p2 &  p3          x < 1; 1 <= y          rule 1, rule 3
!p1 & !p2 &  p3     either x >= 3 or y < 0      rule 3
all combination with !p3    impossible
"""


def split_guard(transition_list):
    assert len(set(source for source, *_ in transition_list)) <= 1
    marker_table = {
        variable: sorted(
            {
                marker
                for _, guard, *_ in transition_list
                for marker in guard.get(variable, ())
                if marker is not None
            }
        )
        for _, guard, *_ in transition_list
        for variable in guard.keys()
    }

    def split_bound(marker_list, bound):
        low, high = bound
        level = low
        for marker in marker_list:
            # marker in (level, high]
            if (level is None or level < marker) and (high is None or marker <= high):
                yield level, marker
                level = marker
        assert high is None or level == high
        if high is None:
            yield level, high

    def split(guard):
        split_gen = ({},)
        for variable, bound in guard.items():
            split_gen = tuple(
                {variable: splitted, **prev}
                for prev in split_gen
                for splitted in split_bound(marker_table[variable], bound)
            )
        yield from split_gen

    for _, guard, *rest in transition_list:
        yield tuple(rest), set(frozenset(splitted.items()) for splitted in split(guard))


def relevant(transition_list):
    import sys

    assert sys.version_info >= (3, 7)  # for ordered dict implementation
    for source in dict.fromkeys(source for source, *_ in transition_list):
        # {rest => guard set}, where guard sets may intersect with each other
        split_table = dict(
            split_guard(
                tuple(
                    transition
                    for transition in transition_list
                    if transition[0] == source
                )
            )
        )
        guard_set = {guard for split_set in split_table.values() for guard in split_set}

        # a set of [rest], each [rest] is enabled by either one guard of a guard
        # set, and each guard set does not intersect with others
        relevant_set = {
            tuple(rest for rest, split_set in split_table.items() if guard in split_set)
            for guard in guard_set
        }
        yield source, (
            (
                (
                    dict(guard)
                    for guard in guard_set
                    if all(guard in split_table[rest] for rest in rest_list)
                ),
                rest_list,
            )
            for rest_list in relevant_set
        )


def serialize(transition_list):
    (start_name, *_), *_ = transition_list
    state_list = (
        start_name,
        *sorted({name for name, *_ in transition_list} - {start_name}),
    )
    yield len(state_list)
    state_table = {name: i + 1 for i, name in enumerate(state_list)}
