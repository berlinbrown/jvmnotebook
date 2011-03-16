#!/bin/sh

javac CrashJVM.java
javac Normal.java

javac server/LoadTestServer.java
javac server/LoadTestServerThread.java
javac server/SimpleReportBean.java
javac server/BetterReportBean.java

# End of Script
