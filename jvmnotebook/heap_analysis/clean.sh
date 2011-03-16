#!/bin/sh
#
# Remove garbage files from the heap analysis test
#

rm -vf server/*.class
rm -vf *.class
rm -vf *.tmp

rm -vf image_gc_analysis.png
rm -vf image_gc_analysis.txt
rm -vf gc_anal.txt
rm -vf gc_normal_anal.txt
rm -vf gc_server_anal.txt
rm -vf dump.txt
