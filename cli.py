"""
cli.py: The real command line interface of TrueRoute.
"""
import sys
import pathlib
import spec
import crg
import lpdfa
import gen
import object

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

extr_path = pathlib.Path(extr_grammar)
extr_grammar = extr_path.read_text(encoding="utf-8")
grammar = (pathlib.Path(gram).read_text(encoding="utf-8") for gram in grammar)

extr_grammar = tuple(spec.Grammar(extr_grammar))
grammar = sum((tuple(spec.Grammar(gram)) for gram in grammar), start=())
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
                print(f"  if {object.guard_str(guard)}")
            for line in lpdfa.format_str(
                lpdfa.construct(tuple(transition_list))
            ).splitlines():
                print(f"  {line}")
    sys.exit()

assert command == "gen"
store = gen.Store()
for i, auto in enumerate(store.push_meta(tuple(grammar))):
    store.push_automata(i, lpdfa.construct(auto))
ESCAPE = 0x33  # not using 0xff since that may be more common
# https://space.bilibili.com/351609538
with open(extr_path.with_suffix(".troute"), "wb") as f:
    f.write(bytes((0x01, 0x04, 0x85, 0x96)))  # magic number: steins gate
    number_u8, number_u32 = 0, 0
    for n in store.finalize():
        assert isinstance(n, int), str(n)
        if n > 0xFF or n == ESCAPE:
            f.write(bytes((ESCAPE,)))
            f.write(n.to_bytes(4, "big"))
            number_u32 += 1
        else:
            f.write(bytes((n,)))
            number_u8 += 1
print(f"Single byte: {number_u8}")
print(f"Escaped: {number_u32}")
