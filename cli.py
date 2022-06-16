"""
cli.py: The real command line interface of TrueRoute.
"""
import sys
import pathlib
import spec
import crg
import lpdfa

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

extr_grammar = pathlib.Path(extr_grammar).read_text()
grammar = (pathlib.Path(gram).read_text() for gram in grammar)

extr_grammar = tuple(spec.Grammar(extr_grammar.lstrip() + "\n"))
grammar = sum((tuple(spec.Grammar(gram.lstrip() + "\n")) for gram in grammar), start=())
if command == "ccfg":
    for rule in extr_grammar + grammar:
        print(rule)
    sys.exit()

grammar = crg.parse(grammar, extr_grammar)
if command == "crg":
    print(
        "{:20}{:20}{:4}{:30}{:26}{}".format(
            "Source", "Guard", "Pri", "Regular", "Action", "Target"
        )
    )
    for source, config_list in grammar:
        for guard, transition_list in config_list:
            guard = crg.guard_str(guard) if guard else ""
            for priority, regular, action, target in transition_list:
                action = crg.action_str(action) if action else ""
                target = target or "(accept)"
                regular = str(regular)
                print(
                    f"{source:20}{guard:20}{priority:<4}{regular:30}{action:26}{target}"
                )
    sys.exit()

if command == "ca":
    for source, config_list in grammar:
        for guard, transition_list in config_list:
            guard = f" if {crg.guard_str(guard)}" if guard else ""
            state = lpdfa.construct(tuple(transition_list))
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


print("gen: work in progress")
