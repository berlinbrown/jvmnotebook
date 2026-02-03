# Copilot instructions for jvmnotebook 🔧💡

Quick orientation
- This repository is a collection of JVM-language demos (ABCL, Jython, Scala, Groovy, Drools, ANTLR, ASM, JGAP, etc.). See `README.md` and `jvmnotebook/README` for project intent and history. **Note:** the project is deprecated (last updates ~2011) — expect old Java versions, Windows-style scripts, and manual dependency management.

Big-picture architecture & why it looks like this
- It is a monorepo of mostly independent demos and example apps. Each demo is self-contained: source in `src/`, third-party jars in `lib/`, and Ant build files or shell scripts per subproject.
- There is no single top-level build/test system; work is performed per subproject using Ant (`build.xml`) or ad-hoc scripts (e.g., `run.sh`, `clean.sh`). This design keeps demos isolated but means changes must be evaluated per subproject.

Where to start (important paths)
- Root overviews: `README.md` and `jvmnotebook/README`
- Major demo directories: `abcl/`, `antlr/`, `asm_bytecode/`, `common_lisp/`, `drools/`, `groovy/`, `jython/`, `scala/`, `jgap/`, `heap_analysis/`
- Check for build scripts: search for `build.xml`, `run.sh`, `Makefile` in each demo folder.
- Examples of entrypoints:
  - ANTLR: `jvmnotebook/antlr/run.sh` and `ParseMain.java`
  - JGAP: `jvmnotebook/jgap/lib/jgap/readme.txt` (ant build inside)
  - ABCL servlet example: `jvmnotebook/abcl/LispABCLServlet/` (has `web/` & `WEB-INF/`)
  - Jython tests: `jvmnotebook/jython/UnitTestPy/` (uses JUnit/Jython)

Developer workflows (how to build/run/test)
- Usual approach: cd into the target demo folder and run Ant if `build.xml` exists (e.g., `cd jvmnotebook/abcl/LispCodeGen && ant`).
- For small demos, use provided scripts like `run.sh`. Be careful—some scripts use Windows-style classpath separators or `cygpath` and may need adjustments on macOS/Linux.
- To run examples without Ant:
  - Inspect the demo's `README` and `src/` for a `main()` or `ParseMain.java` entrypoint.
  - Build with `javac` and run with `java -cp <classpath> <MainClass>`. Use `lib/` jars as needed.
- Tests: look for JUnit or demo-specific test harnesses in each subproject. There is no repository-wide test runner.

Patterns & project-specific conventions
- Per-demo isolation: many demos bundle their own `lib/` jars — prefer using those over adding global dependencies.
- Older Java compatibility: code may assume Java 1.4/1.5 behavior; refactors that change language level may break demos.
- Minimal automation: no CI, no dependency manager (Maven/Gradle) centralization — dependency updates should be conservative and validated locally.

Integration points & external dependencies
- The project integrates with external JVM-language implementations: ABCL, Jython, JRuby (examples), Scala, Groovy, Drools, ANTLR, ASM, JGAP.
- Many demos expect you to manually download external jars or rely on the `lib/` included in that subproject.
- Web examples (servlets, small web apps) expect a servlet container (Tomcat/Jetty) for manual testing.

What to watch out for (gotchas)
- Deprecated code & Windows-specific scripts: expect `cygpath`, Windows classpath separators, and other platform assumptions.
- Build/log files and example outputs exist in the repo (`log/` directories). Avoid overwriting sample outputs when running tests locally — put temp outputs in a workspace temp dir.
- API and library versions are old — automatic upgrades may require code changes across many demos.

Useful search queries for an AI agent
- Find buildable demos: `grep -R "build.xml"` or `grep -R "run.sh"`.
- Find entry points: `grep -R "public static void main"` or `grep -R "ParseMain"`.
- Find third-party libs: look for `lib/` directories and examine jar names.
- Read per-demo READMEs: search `README` files in subdirectories to surface instructions specific to that demo.

Guidance when changing the repo
- Make minimal, well-scoped changes to a single demo and verify it still builds and runs on at least one local Java version.
- If modernizing (Maven/Gradle, CI), propose the change in a PR with a migration plan that preserves at least one working example per language.
- Update the demo's README with exact commands to reproduce your verification.

If you need clarification or more detail
- Tell me which subproject(s) you want prioritized (e.g., `abcl`, `antlr`, `jython`) and whether you'd like help adding CI, modernizing builds, or creating runnable GitHub Actions workflows.

---

Would you like me to (pick one):
1) Draft a short CI plan to test a subset of demos on modern JDKs ✅
2) Expand per-subproject quickstart snippets (commands that work on macOS) 🔧
3) Keep this file shorter / reword any section ✏️

Please tell me which option you prefer or suggest changes.