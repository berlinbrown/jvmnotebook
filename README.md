# JVM Notebook

**JVM Notebook** is a collection of small, self-contained demos and example projects that explore JVM languages and tooling (ABCL, Jython, Scala, Groovy, Drools, ANTLR, ASM, JGAP, etc.). This repo is primarily educational and historically focused — last updates are from ~2011. Expect older Java compatibility assumptions and manual build scripts.

## What you'll find here

- Independent demos in `jvmnotebook/<demo>/` (each demo often contains `src/`, `lib/`, and `build.xml` or `run.sh`).
- Language-focused examples: parsers (ANTLR), bytecode (ASM), language implementations (Jython, ABCL), rules (Drools), and algorithm libraries (JGAP).
- Simple scripts and test harnesses rather than a single, unified build system.

## Quickstart (macOS)
1. Inspect the demo README: `cd jvmnotebook/<demo> && cat README` or open `jvmnotebook/<demo>/README.md`.
2. If a subproject has an Ant build: `cd jvmnotebook/abcl/LispCodeGen && ant` (requires a JDK).
3. For scripts (e.g., `run.sh`): open the script—some use Windows tools (`cygpath`) and Windows classpath separators. On macOS, run the Java commands manually or edit the script to use `:` classpath separators.
   - Example (ANTLR):
     - Build: `javac -classpath ".:antlr-3.0.1.jar:antlr-runtime-3.0.1.jar:stringtemplate-3.1b1.jar" *.java`
     - Run: `java -classpath ".:antlr-3.0.1.jar:antlr-runtime-3.0.1.jar:stringtemplate-3.1b1.jar:.;" ParseMain test.c`
4. Heap analysis demo: `cd jvmnotebook/heap_analysis && ./c.sh && ./r.sh` then visit `http://localhost:7000`.

## Contribution notes
- Make minimal, demo-scoped changes. Many examples expect older Java versions; verify locally on a modern JDK before proposing large upgrades.
- Add or update `jvmnotebook/<demo>/README.md` with exact build/run steps when you change a demo.

---

See `.github/copilot-instructions.md` for AI-agent-specific guidance (quick entry points and patterns to follow when making changes).


### JVM Notebook, set1 release (3/27/2008)

    * uploaded JRubySourceCodeManager.zip - example jruby web application oriented j2ee application. an example source code manager for viewing all of your source code in your project. Use of simple jruby/spring framework.
    * uploaded jython_set1_d032008.zip - misc jython oriented examples including struts based application. Junit/Jython example and other notebook samples.
    * uploaded scala_set1_d032008.zip - scala examples including scala/lift web project(note: using older version of scala source code)
    * uploaded misc_NeuralNetworkJava?.zip - misc neural network example in java
    * uploaded abcl_set1_d032008.zip - abcl/lisp examples including abcl oriented web example. 



### JVM Resources

    * http://jruby.codehaus.org/ - JRuby Home
    * http://groovy.codehaus.org/ - Groovy Home
    * http://clojure.org/ - Clojure
    * http://www.scala-lang.org/ - Scala is a general purpose programming language designed to express common programming patterns in a concise, elegant, and type-safe way. 

    * http://www.jython.org - Jython
    * http://common-lisp.net/project/armedbear/ - Armed Bear Common Lisp (ABCL) is an implementation of ANSI Common Lisp that runs in a Java virtual machine. 

    * http://sisc-scheme.org/ - SISC, Second Interpreter of Scheme Code
    * http://asm.objectweb.org/ - ASM, Java bytecode manipulation and analysis.