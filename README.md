**TrueRoute is an implementation of *FlowSifter*<sup>[1]</sup> in Python.**

Developed with Python 3.10.5, but should work on many recent versions.

**Sanity check.** Run `python -m unittest`.

**Compare to [MSU-SSL/FlowSifter](https://github.com/MSU-SSL/FlowSifter).**
||FlowShifter|TrueRoute|
|-|-|-|
|Language       |OCaml          |Python + C     |
|Deploy/dev platform|Unknown    |Linux          |
|CCRG frontend  |Yes            |Yes            |
|Simulate       |Yes with OCaml |Yes with C     |
|Compile        |Yes            |No             |
|Serializable   |No<sup>[2]</sup>|Yes           |

**About name**. Simple analogy. Process flow stream as galgame gameplay. Counting states as characters' favorability, which go up and down on different conditions, and decide important branching base on instant values. Finally, a successfully processing would be entering true route.

<sub>
[1]: A. X. Liu, C. R. Meiners, E. Norige and E. Torng, "High-Speed Application Protocol Parsing and Extraction for Deep Flow Inspection," in IEEE Journal on Selected Areas in Communications, vol. 32, no. 10, pp. 1864-1880, Oct. 2014, doi: 10.1109/JSAC.2014.2358817.<br>
[2]: Compiling into C++ source code is not treated as serialization, since there is no effecient way to deserialize.

</sub>

----

**A simple walkthrough of main features.** Check the original paper and repo for more.

Work in progress.