#!/bin/sh
#
# Additional Notes:
# http://java.sun.com/developer/technicalArticles/Programming/GCPortal/
# http://java.sun.com/developer/technicalArticles/J2SE/jconsole.html
javac CrashJVM.java
javac Normal.java

FORMAT=,format=b

# Add profiling and verbose garbage collection
java -verbosegc -verbosegc -verbosegc -XX:+PrintGCDetails -XX:+PrintGCTimeStamps CrashJVM -Xms24MB -Xmx32MB > gc_anal.txt 

java -verbosegc -verbosegc -verbosegc -XX:+PrintGCDetails -XX:+PrintGCTimeStamps Normal -Xms24MB -Xmx32MB > gc_normal_anal.txt 

# To run the server:
# jhat dump.txt

#jhat dump.txt
