"""
gen.py: Serialization protocol for Counting Automata. This is the input prepared
for simulated CA implementation.

TODO: main interface

----

Simulating model

The simulating backend is expected to keep following variable data:
* a, the index of current active LPDFA, initialized to 0
* s, the index of current LPDFA state, initialized to 1
* d, the decision seen so far with highest priority, may be null, initialized to
  null. It is composed by:
    * d_pri, decision priority
    * d_act, decision action indexed in action table
    * d_dst, decision destination (aka target), indexed into branching table
* l, the number of lookahead bytes since reaching d or entering this LPDFA, 
  initialized to 0
* c[64], a list of counters, all initialized to 0. Currently input will fail to 
  generate if it requires more than 64 counters
  * c[0] is a special counter indicate start position of current extraction, 
    i.e. "p" counter in paper

The simulating backend is expected to keep following permanent data:
* S[], a list of LPDFA state. It is composed by:
    * S_JMP[][256], state byte table. The value is index of S. Backend may use a
      single dimension flat array to represent S_JMP
    * S_MAX[], maximum priority ahead. If no decision ahead it is set to 0
    * S_ACC[], state decision if it is accepted. It is composed by:
        * S_PRI[], decision priority
        * S_ACT[], decision action indexed by action table
        * S_DST[], decision detination indexed by branching table B[]
* A[], a list of LPDFA, represented as index into S[], which is the index of
  LPDFA's start state minus 1
* A_ACC, a countant not less than len(A[])
* B[], a list of indexes into guard table G[], represents the upper bound of a 
  range of guards
* G[], a list of guard. It is composed by:
    * G_C[], the counter index
    * G_I[], the compared immediate number
    * G_M[], indicate whether it is a lower guard (i.e. G_I <= c[G_C]) where G_M 
      is 0, or higher guard (i.e. c[G_C] < G_I) where G_M is 1. In either case, 
      the guard can be represented as:
        G_M * (G_I - c[G_C]) + (G_M - 1) * (G_I - c[G_C]) >= G_M
    * G_T[], indicate whether this is a termination of guard merging, or this
      guard should be merged with the following one
* J[], a list of destination LPDFA, which is indexed into A[] or is A_ACC
* R[], a list of accumulated exponent of number of rules (the meaning here is 
  not important), which is indexed into J[]
* U[], a list of update (aka action table). Each item of U[] may be one of:
  * the data length of a (composed) action
  * action id
  * counter index
  * immediate number

The simulating backend should also keep (and decide) a proper runtime 
representation of:
* Action interpreter, which accept a valid slice (i.e. an index i where U[i] 
  means data length) of U, i.e. U[i + 1..i + U[i]], and:
  * update c[] accordingly
  * possibly invoke user defined extraction routine
* Data stream, which support:
  * moving forward
  * rollback l bytes from current position
  * preserve all content after position c[0], and the content is accessible from
  * user defined extraction routine

The simulating backend should do:
1. if S_ACC[A[a] + s] is not null, we reach a (possibly new) decision
    1.1. if d is null or S_PRI[A[a] + s] >= d_pri, we reach a higher decision, 
         or an equal one with longer (more greedy) matching
        1.1.1. set d to S_ACC[A[a] + s] 
        1.1.2. set l to 0
    1.2. if d is not null and S_MAX[A[a] + s] < d_pri, we don't have any equal
         or higher decision ahead, goto #5
2. read next byte from data stream and set v2 to it, set l to l + 1
    * if it is the end of stream, goto #5
3. set s to S_JMP[A[a] + s][v2]
4. if s is not 0 goto #1
5. now we are done with current LPDFA. If d is null, we have not reached any 
   decision yet, report this is a dead end
6. ask data stream to rollback l, set l to 0
7. ask action interpreter to execute U[d_act]
8. for every v8 in B[d_dst - 1] (inclusive) to B[d_dst] (exclusive), or 0 to 
   B[d_dst] if d_dst is 0
    8.1. keep evaluate G[v8] as desribed in G_M, until reach a guard with 
         G_T[v8] is true. set v81 to 1 if all evaluated guard are true, 
         otherwise 0
        * it is asserted that G_T[B[d_dst] - 1] must be true
    8.2. set v82 to accumulated bitset of v81, e.g. set v82 to 5 (0b101) if in 
         v8's indexed range there are three terminating values, and the 1st and 
         3rd guard list are evaluated to be all true
9. set a to J[R[d_dst] + v82]
10. if a equals A_ACC, we have finish the whole grammar. Report this is a bad 
    end if data stream is not reaching end, otherwise true end
TODO: find a proper anology for normal/good end
11. set s to 1, goto #1

p.s. In this execution model, S_JMP[][] == 0 has been used as failure transition
indicator, which means there is always s > 0 when A[a] + s is used to index into
S_*[]. If there is always A[] >= 0 (which is the case in this serialization),
it will cause S_*[] unreachable. I just want to claim that I have noticed this 
and by now don't think it would be a problem.

----

Serialization specification

Metadata:
* Value range of d_dst, which is also len(B[]) and len(R[]), number of CA states
* Value range of a, which is also len(A[]), number of LPDFA instances
    * A_ACC uses len(A[])
* len(S[]), total state number of LPDFA instances
* len(J[]), total size of index by rule exponential lookup tables
* len(G[]), number of single counter single side guards
* len(U[]), size of action bytecode data

Followed by content of B[], R[], A[], J[], G_C[], G_I[], G_M[], G_T[], U[], 
S_MAX[], S_PRI[], S_ACT[], S_DST[], S_JMP[].

----

Side node about LPDFA indexing

In paper section VI.B three strategies are proposed for LPDFA indexing: index by
rule, index by predicate (guard), and optimal decision tree. This implementation
provides procedures of the first two. Index by rule is actually used to generate
serialized CA following the paper author's choice, which is behind the main
interface and simulating execution model.

Alternatively, `relevant(transition_list)` do index by predicate: it generates
a list of `(source state, configuration generator)`, where the second element
generates `(guard generator, headless transition list)`, that each headless
transition list is corrsponded to one "relevant" LPDFA as defined by paper, and
the LPDFA can be produced by pass this list into `lpdfa.construct`. The guard
generator produce "splitted" guards that:
* Any two guards cannot be true for the same counter values state, even the 
  guards from different generators.
* The LPDFA is enabled by any guard generated by paired generator.

For example, consider following rules:
S [x < 1; y >= 0] -> ...
S [x < 3; y == 0] -> ...
S -> ...
The splitted guards, and the rules they enable are:
     x < 1;      y < 0      rule 3
     x < 1; 0 <= y < 1 (i.e. y == 1 for int y) rule 1, rule 2, rule 3
     x < 1; 1 <= y          rule 1, rule 3
1 <= x < 3; (all y again)   rule 2, rule 3 or rule 3 depends on y
3 <= x    ; (all y again)   rule 3
So the configuraion generator will produce 4 headless transition list, each
corresponds to (rule 1, rule 2, rule 3), (rule 1, rule 3), (rule 2, rule 3) and
(rule 3,) repectively. The paired guard generator will produce guards that show
up in above table, if the enabled rule list matches the paired one.

Although index by rule is preferred, the `relevant` is still provided because:
* In the case if we will switch to index by predicate in the future.
* Currently it is used to implement index by rule, since it provides all results
  an index by rule implementation requires (and more). Later we may switch to a
  straightforward index by rule procedure if performance issue emerges.
* The splitting is more intuitive and is good for presentation, it is actually
  implemented before LPDFA, and used by cli's `ca` command. In comparision, a 
  splitting of index by rule for the above example would be (suppose p1 means
  x < 1 && y >= 0, !p1 means the opposite):
 p1 &  p2 &  p3          x < 1; 0 <= y < 1      rule 1, rule 2, rule 3
!p1 &  p2 &  p3     1 <= x < 3; 0 <= y < 1      rule 2, rule 3
 p1 & !p2 &  p3          x < 1; 1 <= y          rule 1, rule 3
!p1 & !p2 &  p3     either x >= 3 or y < 0      rule 3
all combination with !p3        impossible
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
    for state, config_list in relevant(transition_list):
        pass
