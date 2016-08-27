# scanalyzer
A simple static analyzer for SSA-Form CFGs written in Scala to learn the language.

The actual static analysis follows the well-established Abstract Interpretation[1] framework.
Input format and the CFG structure are inspired by the LLVM Intermediate Representation[2].


---

[1] - Cousot, P. and Cousot, R. (1977). Abstract interpretation: A unified lattice model for static analysis of programs by construction or approximation of fixpoints.

[2] - [LLVM Project](http://llvm.org/), [Language Reference](http://llvm.org/docs/LangRef.html)
