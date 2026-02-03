# Heap analysis tooling

This demo includes simple tools and scripts for parsing JVM garbage collector output and producing charts:

Usage overview:
- `./c.sh` — compile the Java helper programs (generates class files).
- `./r.sh` — run the heap analysis server (visit `http://localhost:7000`).
- `python gc_parse.py` — parse GC logs and extract metrics.
- `./g.sh`, `./plot.sh` — generate gnuplot data and PNG images.

Notes:
- Examples show HotSpot GC output parsing and are tied to specific JVM logging formats. Adjust parser options if you run newer JVMs or different GC flags.
- Example output files live under `example_output1/` and `all_output/`.
