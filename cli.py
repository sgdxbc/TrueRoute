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
    print(crg.format_str(grammar))
    sys.exit()

if command == "ca":
    for source, config_list in gen.relevant(tuple(grammar)):
        for guard_gen, transition_list in config_list:
            print(f"from {source}")
            for guard in guard_gen:
                print(f"  if {crg.guard_str(guard)}")
            for line in lpdfa.format_str(
                lpdfa.construct(tuple(transition_list))
            ).splitlines():
                print(f"  {line}")
    sys.exit()

print("gen: work in progress")
