#!/bin/sh
#
# Launch the server application with GC output.


# Add profiling and verbose garbage collection

java -verbosegc -verbosegc -verbosegc -XX:+PrintGCDetails -XX:+PrintGCTimeStamps server.LoadTestServer -Xms16MB -Xmx24MB > gc_server_anal.txt 

