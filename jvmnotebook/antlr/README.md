# ANTLR demos

Contains ANTLR grammars and a small parser demo.

Files of interest:
- `AnsiC.g` — example grammar
- `ParseMain.java` — demo parser entrypoint
- `run.sh` — convenience script (Windows-oriented: uses `cygpath` and `;` classpath separators)

Quickstart (macOS):
1. Ensure you have a JDK installed.
2. Build manually (recommended on macOS):
   - `javac -classpath ".:antlr-3.0.1.jar:antlr-runtime-3.0.1.jar:stringtemplate-3.1b1.jar" *.java`
   - `java -classpath ".:antlr-3.0.1.jar:antlr-runtime-3.0.1.jar:stringtemplate-3.1b1.jar" ParseMain test.c`

Note: `run.sh` is Windows-friendly; edit it to remove `cygpath` and use `:` separators if you want to run it unchanged on macOS.
