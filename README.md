**TrueRoute is an implementation of *FlowSifter*<sup>[1]</sup> in Python.**

Developed with Python 3.10.5, but should work on many recent versions.

**Run unit tests.** `python -m unittest crg.py lpdfa.py spec.py gen.py`

**Run compiler.** Use `python -m cli` with arguments to generate various outputs.

**Process network traffic.** Work in progress.

**About name**. Simple analogy. Process flow stream as galgame gameplay. Counting states as characters' favorability, which go up and down on different conditions, and decide important branching base on instant values. Finally, a successfully processing would be entering true route.

<sub>
[1]: A. X. Liu, C. R. Meiners, E. Norige and E. Torng, "High-Speed Application Protocol Parsing and Extraction for Deep Flow Inspection," in IEEE Journal on Selected Areas in Communications, vol. 32, no. 10, pp. 1864-1880, Oct. 2014, doi: 10.1109/JSAC.2014.2358817.
</sub>