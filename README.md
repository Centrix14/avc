# About
avc (address validator and corrector) is a small in-REPL utility for validation and correction of addresses (especially russian addresses). Besides it's initial purpose avc could be used for validation and correction of arbitrary forms (through `#'validate` power).

# Use cases
avc could be used in two different ways: as utility programm and as library. It possible through symbol-macro and command interface: all commands of a programm is just a functions working in context of REPL. So you can call this functions interactively (utility programm use case) or from your own code (library use case).
