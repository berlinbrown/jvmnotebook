# JVM Notebook (per-demo collection)

This folder contains the collection of demos and example projects referenced from the repository root. The original project site was at http://code.google.com/p/jvmnotebook/.

## Purpose
Each demo is a small, self-contained exploration of a JVM language, library, or tool: examples include ABCL, ANTLR, ASM, Groovy, Jython, Scala, Drools, JGAP, and others.

## Notable demos
- `abcl/` — Common Lisp on the JVM examples and a servlet demo.
- `antlr/` — ANTLR grammars and a small parser demo (`ParseMain.java`).
- `asm_bytecode/` — ASM-based bytecode generation examples.
- `heap_analysis/` — GC parsing and small web UI for heap reports.
- `jython/`, `groovy/`, `scala/` — language-specific examples and tests.

## History / changelog
- Releases in 2008–2009 added many demos and updates; this project is historically focused and not actively maintained.

If you're updating examples, add a `README.md` in the subproject with exact build/run steps for modern macOS/Linux systems.