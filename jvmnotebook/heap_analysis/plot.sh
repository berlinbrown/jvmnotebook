#!/bin/sh
# Also see:
# http://www.duke.edu/~hpgavin/gnuplot.html
#
# Use 'eog' eye of gnome to view the images


OUTPUT_IMG_FILE=image_gc_analysis.png
INPUT_PLOT_FILE=image_gc_analysis.txt

# Also edit the script with some of the following params:
#
# set xrange [20:26]
# set yrange [12000:26000]
# 
# Output these commands to a temp file and then run gnu plot 
echo "set terminal png size 800,600 
set output '${OUTPUT_IMG_FILE}'
set title 'GC results'
set size 1,1 
set key left top
set autoscale
set xlabel 'gc_iter'
set ylabel 'gc start occupancy in kb'
plot '${INPUT_PLOT_FILE}' using 1:6 title 'GC Results' with linespoints 
" > gnuplot_tmp_cmd.tmp 

# Run the gnu plot command
gnuplot gnuplot_tmp_cmd.tmp > /dev/null

# For future reference, add a command and title to compare results 
# E.g.
# '/tmp/data2' using 10 with lines title 'Benchmark 2', 

