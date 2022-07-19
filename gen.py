"""
gen.py: Serialization protocol for Counting Automata. This is the input prepared
for simulated CA implementation.

The main interface is `Store`. A `Store` instance should be used in three steps:
first call `push_meta` with transition list returned by `crg.optimize`, which
will update store state, and return a list of headless transition list. Call
`lpdfa.construct` with each headless transition list in order, and pass returned
`State` instance into `push_automata`. Finally, call `finalize` when all LPDFA
are pushed, and it will yield serialized data, i.e. a number sequence. The 
interface is designed in a little bit verbose way, because I want to keep things
modular, to e.g. allow perform unit test on `gen` independently in respect to
the correctness of `lpdfa`.

`Store.finalize` guarantee to produce deterministic serialized sequence. Not
only hold basic correctness that every run generate serialized CA with same
functionality, but also the generated sequence will be exact same for the same
input, i.e. CCFG source file. This property may be helpful for regression tests.

The following document explain what data structures are serialized, how to use
these data structure, and detail serialization format.

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
* DST_ACC, a constant that is not less than len(G_I[]) - 1
* S[], a list of LPDFA state. It is composed by:
    * S_JMP[][256], state byte table. The value is index of S. Backend may use a
      single dimension flat array to represent S_JMP
    * S_MAX[], maximum priority ahead. If no decision ahead it is set to 0
    * S_ACC[], state decision if it is accepted. It is composed by:
        * S_PRI[], decision priority
        * S_ACT[], decision action indexed into action table U[]
        * S_DST[], decision detination indexed into guard offset table G_I[] and
          jump offset table J_I[]
* A[] (maybe a better name is S_I[]), a list of LPDFA, represented as index into 
  S[], which is the index of LPDFA's start state
* G[] the guard table. It is composed by:
    * G_C[], the counter index
    * G_N[], the compared immediate number
    * G_M[], indicate whether it is a lower guard (i.e. G_N <= c[G_C]) where G_M 
      is 0, or higher guard (i.e. c[G_C] < G_N) where G_M is 1. In either case, 
      the guard can be represented as:
        G_M * (G_N - c[G_C]) + (G_M - 1) * (G_N - c[G_C]) >= G_M
    * G_T[], indicate whether this is a termination of guard merging, or this
      guard should be merged with the following one
* G_I[], a list of indexes into G[]
* J[] jump table (technically A_I[] or S_I_I[]), a list of destination LPDFA, 
  which is indexed into A[] or is A_ACC
* J_I[] (technically S_I_I_I[] nevermind ^_^), a list of accumulated exponent 
  of number of rules (the meaning here is not important), indexed into J[]
* U[], a list of update (aka action table). Each item of U[] may be one of:
  * the data length of a (composed) action
  * action id
  * counter index
  * immediate number

The simulating backend should also keep (and decide) a proper runtime 
representation of:
* Action interpreter, which accept a valid slice (i.e. an index i where U[i] 
  means data length) of U, i.e. U[i + 1..i + U[i]], and:
  * manipulate data stream
  * update c[] accordingly
  * possibly invoke user defined extraction routine
* Data stream, which support:
  * moving forward
  * rollback l bytes from current position
  * preserve all content after position c[0], and the content is accessible from
    user defined extraction routine

The simulating backend should do:
1. set v1 to current LPDFA state index A[a] + s - 1
2. if S_ACC[v1] is not null, we reach a (possibly new) decision
    2.1. if d is null or S_PRI[v1] >= d_pri, we reach a higher decision, or an 
         equal one with longer (more greedy) matching
        2.1.1. set d to S_ACC[v1] 
        2.1.2. set l to 0
    2.2. if d is not null and S_MAX[v1] < d_pri, we don't have any equal or 
         higher decision ahead, goto #6
3. read next byte from data stream and set v3 to it, set l to l + 1
    * if it is the end of stream, goto #6
4. set s to S_JMP[v1][v3]
5. if s is not 0 goto #1
6. now we are done with current LPDFA. If d is null, we have not reached any 
   decision yet, report this is a dead end
7. ask data stream to rollback l, set l to 0
8. ask action interpreter to execute U[d_act]
9. if d_dst equals DST_ACC, we have finish the whole grammar. Report this is a
   bad end if data stream is not reaching end, otherwise true end
TODO: find a proper anology for normal/good end
10. for every v10 in G_I[d_dst] (inclusive) to G_I[d_dst + 1] (exclusive)
    10.1. keep evaluate G[v10] as desribed in G_M, until reach a guard with 
         G_T[v10] is true. set v10a to 1 if all evaluated guard are true, 
         otherwise 0
        * it is asserted that G_T[G_I[d_dst + 1] - 1] must be true
    10.2. set v10b to accumulated bitset of v10a, e.g. set v10b to 5 (0b101) if 
         in v10's indexed range there are three terminating guards, and the 1st 
         and 3rd guard list are evaluated to be all true
11. set a to J[J_I[d_dst] + v10b]
12. set s to 1, goto #1

Because certain combination of rules are impossible to be enabled at the same
time, not all index in jump table J[] can map to a valid LPDFA index, and those 
impossible LPDFA are not constructed at all. This implementation uses some value 
>= len(A[]) to fill these "holes", which may be used for assertion in a debug 
build.

----

Serialization specification

A series of discrete meta numbers followed by the lists
* Value range of d_dst, which is also len(G_I[]) - 1, len(J_I[]) - 1, DST_ACC, 
  number of CA states
* Value range of a, which is also len(A[]) - 1, number of LPDFA instances
* len(U[])
* G_I[], J_I[] and A[]
* G_C[], G_I[], G_N[] and G_T[] whose length is G_I[-1]
* J[] whose length is J_I[-1]
* U[]
* S_MAX[], S_PRI[], S_ACT[] and S_DST[] whose length is A[-1] (remember A[] is 
  also S_I[])
* S_JMP[] whose length is A[-1] * 256

A little note: the three index table G_I[], J_I[] and A[] all have one more item
than expected. They maintain an invariant that their first item is always 0, and
their last item is always the length of the list they are indexing into. It is
possible to shift out the first item, but I would keep them rather than 
introduce `... - 1`  in execution model and mess things up.

----

Side node about LPDFA indexing

In paper section VI.B three strategies are proposed for LPDFA indexing: index by
rule, index by predicate (guard), and optimal decision tree. This implementation
provides procedures of the first two. Index by rule is the strategy actually 
used to generate serialized CA following the paper author's choice. It is behind
the main interface and is integrated into simulating execution model.

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
from functools import cmp_to_key
from itertools import count
from object import compare_guard, compile_action


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
    def deterministic_source_gen():
        (start, *_), *_ = transition_list
        yield start
        yield from sorted({source for source, *_ in transition_list if source != start})

    for source in deterministic_source_gen():
        # {rest => guard set}, where guard sets may intersect with each other
        # e.g. R1 => {G1, G2, G3}, R2 => {G2, G3, G4}
        split_table = dict(
            split_guard(
                tuple(
                    transition
                    for transition in transition_list
                    if transition[0] == source
                )
            )
        )
        # {G1, G2, G3, G4}
        guard_set = {guard for split_set in split_table.values() for guard in split_set}

        # a set of [rest] i.e. list of rest, for each [rest], all contained rest
        # are enabled by every guard of a certain guard set.
        # guard sets of each [rest] does not intersect with each other
        # {
        #     [R1] (enabled by {G1}),
        #     [R2] (enabled by {G4}),
        #     [R1, R2] (enabled by {G2, G3}),
        # }
        # the algorithm here is to calculate enabled [rest] for every single
        # guard in guard set, so [R1, R2] will appear twice for both G2 and G3
        # then collect result with a set to deduplicate
        relevant_set = {
            tuple(rest for rest, split_set in split_table.items() if guard in split_set)
            for guard in guard_set
        }

        relevant_list = (
            (
                sorted(
                    (
                        dict(guard)
                        for guard in guard_set
                        if all(guard in split_table[rest] for rest in rest_list)
                    ),
                    key=cmp_to_key(compare_guard),
                ),
                rest_list,
            )
            for rest_list in relevant_set
        )

        def compare_sorted_guard_list(guard_list, another_guard_list):
            if not guard_list:
                return -1
            if not another_guard_list:
                return 1
            return compare_guard(
                guard_list[0], another_guard_list[0]
            ) or compare_sorted_guard_list(guard_list[1:], another_guard_list[1:])

        yield source, sorted(
            relevant_list,
            key=lambda pair: cmp_to_key(compare_sorted_guard_list)(pair[0]),
        )


class Store:
    def __init__(self):
        self.s = ()
        self.s_i = (0,)
        self.state_table = {}
        self.g = ()
        self.g_i = (0,)
        self.j = ()
        self.j_i = (0,)
        self.c_table = {"p": 0}
        self.c_id = count(1)
        self.u = (0,)

    def variable_id(self, variable):
        if variable in self.c_table:
            return self.c_table[variable]
        id = next(self.c_id)
        assert id < 64, "using more than 64 counters"
        self.c_table[variable] = id
        return id

    def push_guard(self, guard):
        def gen():  # (G_C, G_N, G_M)
            for variable, (low, high) in guard.items():
                id = self.variable_id(variable)
                assert low is not None or high is not None
                if low is not None:
                    yield (id, low, 0)
                if high is not None:
                    yield (id, high, 1)

        *merged, last = gen()
        self.g = *self.g, *((*triple, False) for triple in merged), (*last, True)

    def push_action(self, action):
        if not action:
            # U[0] == 0 is the shared "no action" stub
            # it represents a zero length no-op bytecode
            return 0
        u_snippet = sum(
            (compile_action(step, self.variable_id) for step in action), start=()
        )
        i = len(self.u)
        self.u = *self.u, len(u_snippet), *u_snippet
        return i

    def push_meta(self, transition_list):
        construct_list = ()
        for state, config_list in relevant(transition_list):
            # for this state we are generating / will generate:
            # * len(config_list) number of LPDFA
            # * len(config_list) items in A[]. we first preserve these items
            #   with None, then fill them one by `push_automata`
            # * certain number of items in G[], by calling len(rule_table) times
            #   `push_guard`. a small ceveat is len(rule_table) may be less than
            #   the number of production rules produced by `state`, because all
            #   rules with trivial guard are not collected because:
            #   * they incur no items in G[], there's no obvious way to let
            #     runtime know it is evaluating a trivial guard
            #   * they never be false, so it is actually asserted that every
            #     relevant LPDFA will mix in their regular. it's ok that their
            #     truth value not show up in bitset offset
            # * 1 << len(rule_table) items in J[], consist of a snippet of jump
            #   table for all relevant LPDFA
            # * 1 item in J_I[] to show the offset of (next) jump table in J[]
            # * 1 item in G_I[] to show the offset of (next) guard list in G[]

            rule_table = {}
            for i, (guard, rest) in enumerate(
                (guard, rest)
                for source, guard, *rest in transition_list
                if source == state and guard
            ):
                self.push_guard(guard)
                rule_table = {**rule_table, tuple(rest): 1 << i}
            assert (
                1 << len(rule_table) <= 64
            ), "rule table more than 64 entries is unpractical"

            index_table = {}
            for _, rest_list in config_list:
                index = sum(rule_table.get(rule, 0) for rule in rest_list)
                assert index not in index_table
                index_table = {
                    **index_table,
                    index: len(self.s_i) - 1 + len(index_table),
                }
                construct_list = *construct_list, rest_list

            self.j += tuple(
                index_table.get(index, "impossible")
                for index in range(1 << len(rule_table))
            )
            self.s_i += (None,) * len(index_table)
            self.state_table = {**self.state_table, state: len(self.state_table)}
            self.g_i = *self.g_i, len(self.g)
            self.j_i = *self.j_i, len(self.j)

        self.state_table = {**self.state_table, "done": len(self.state_table)}
        assert len(self.g_i) == len(self.j_i) == len(self.state_table)
        return construct_list

    def push_automata(self, index, start_state):
        assert all(self.s_i[1 : index + 1])  # S_I[0] == 0
        assert not self.s_i[index + 1]

        state_list = tuple(start_state.reachable())
        state_table = {state: i for i, state in enumerate(state_list)}

        def gen():
            for state in state_list:
                byte_table = state.as_deterministic()
                s_jmp = tuple(  # we may finalize multiple times, at some time
                    state_table.get(byte_table.get(byte, None), -1) + 1
                    for byte in range(256)
                )
                s_max = state.ahead and state.ahead[0]
                if state.decision:
                    s_pri, act, dst = state.decision
                    yield (
                        s_jmp,
                        s_max,
                        s_pri,
                        self.push_action(act),
                        self.state_table[dst],
                    )
                else:
                    yield s_jmp, s_max, None, None, None

        self.s += tuple(gen())
        self.s_i = tuple(
            len(self.s) if i == index + 1 else original
            for i, original in enumerate(self.s_i)
        )

    def finalize(self):
        assert all(self.s_i[1:])
        assert len(self.g_i) == len(self.j_i) == self.state_table["done"] + 1
        assert len(self.g) == self.g_i[-1]
        assert len(self.j) == self.j_i[-1]
        assert len(self.s) == self.s_i[-1]

        yield len(self.g_i) - 1
        yield len(self.s_i) - 1
        yield len(self.u)
        yield from self.g_i
        yield from self.j_i
        yield from self.s_i
        yield from (g[0] for g in self.g)
        yield from (g[1] for g in self.g)
        yield from (g[2] for g in self.g)
        yield from (int(g[3]) for g in self.g)
        yield from (item if item != "impossible" else len(self.s_i) for item in self.j)
        yield from self.u
        yield from (s[1] or 0 for s in self.s)  # 0 for no decision ahead
        yield from (s[2] or 0 for s in self.s)  # 0 for not accepted
        yield from (s[3] or 0 for s in self.s)  # placeholder
        yield from (s[4] or 0 for s in self.s)  # placeholder
        yield from (d for s in self.s for d in s[0])
