"""
cli.py: The real command line interface of TrueRoute.
"""
import sys
import pathlib
import spec
import crg

assert __name__ == "__main__"

if len(sys.argv) == 4 and sys.argv[1] == "ccfg":
    grammar = sys.argv[2]
    extr_grammar = sys.argv[3]
    command = "ccfg"
elif len(sys.argv) == 4 and sys.argv[1] == "crg":
    grammar = sys.argv[2]
    extr_grammar = sys.argv[3]
    command = "crg"
elif len(sys.argv) == 3:
    grammar = sys.argv[1]
    extr_grammar = sys.argv[2]
    command = "ca"
else:
    print(
        "usage: cli.py [ccfg | crg] {protocol specification} {extraction specification}"
    )
    sys.exit()

grammar = pathlib.Path(grammar).read_text()
extr_grammar = pathlib.Path(extr_grammar).read_text()

grammar = tuple(spec.grammar(grammar.lstrip() + "\n"))
extr_grammar = tuple(spec.grammar(extr_grammar.lstrip() + "\n"))
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

print("ca: work in progress")
