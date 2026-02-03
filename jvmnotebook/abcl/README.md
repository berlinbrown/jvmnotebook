# ABCL (Armed Bear Common Lisp)

Armed Bear Common Lisp (ABCL) is an implementation of ANSI Common Lisp that runs on the JVM. This folder contains small ABCL demos and an example servlet.

Structure:
- `LispABCLServlet/` — example servlet web app (contains `web/` and `WEB-INF/`).
- `LispCodeGen/` — code-generation and build examples (contains `build.xml`).
- `swing/` — Swing examples written in Lisp.

Notes:
- These demos were built to run with older JVMs and ABCL versions. To run servlet examples you need a servlet container (Tomcat/Jetty).
- Check each subfolder for a `README` or `build.xml` and follow the local instructions.
