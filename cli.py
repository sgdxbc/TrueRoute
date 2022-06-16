"""
cli.py: The real command line interface of TrueRoute.
"""
import sys
import pathlib
import spec
import crg
import lpdfa
import gen

assert __name__ == "__main__"

if len(sys.argv) >= 3 and sys.argv[1] in {"ccfg", "crg", "ca"}:
    command = sys.argv[1]
    extr_grammar = sys.argv[2]
    grammar = sys.argv[3:]
elif len(sys.argv) >= 2:
    command = "gen"
    extr_grammar = sys.argv[1]
    grammar = sys.argv[2:]
else:
    print("usage: cli.py [ccfg | crg | ca] {extraction spec.} [protocol spec. ...]")
    sys.exit()

extr_grammar = pathlib.Path(extr_grammar).read_text(encoding="utf-8")
grammar = (pathlib.Path(gram).read_text(encoding="utf-8") for gram in grammar)

extr_grammar = tuple(spec.Grammar(extr_grammar.lstrip() + "\n"))
grammar = sum((tuple(spec.Grammar(gram.lstrip() + "\n")) for gram in grammar), start=())
if command == "ccfg":
    for rule in extr_grammar + grammar:
        print(rule)
    sys.exit()

grammar = crg.optimize(grammar, extr_grammar)
if command == "crg":
    print(
        "{:20}{:20}{:4}{:30}{:26}{}".format(
            "Source", "Guard", "Pri", "Regular", "Action", "Target"
        )
    )
    for source, guard, priority, regular, action, target in grammar:
        guard = crg.guard_str(guard) if guard else ""
        action = crg.action_str(action) if action else ""
        target = target or "(accept)"
        regular = str(regular)
        print(f"{source:20}{guard:20}{priority:<4}{regular:30}{action:26}{target}")
    sys.exit()

if command == "ca":
    for source, config_list in gen.relevant(tuple(grammar)):
        config_list = tuple(
            (guard, tuple(transition_list)) for guard, transition_list in config_list
        )
        relevant_set = set(transition_list for _, transition_list in config_list)
        for transition_list in relevant_set:
            print(f"from {source}")
            for guard, trans in config_list:
                if trans != transition_list:
                    continue
                print(f"if {crg.guard_str(guard)}")
            state = lpdfa.construct(tuple(transition_list))
            reachable = state.reachable() - {state}
            name_table = {
                state: "s",
                **{s: str(i + 1) for i, s in enumerate(reachable)},
            }
            for state in (state, *reachable):
                print(
                    "\n".join(
                        f"  {line}"
                        for line in state.format_str(name_table).splitlines()
                    )
                )
    sys.exit()

print("gen: work in progress")
