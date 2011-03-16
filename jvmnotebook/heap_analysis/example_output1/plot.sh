#!/bin/sh
# Also see:
# http://www.duke.edu/~hpgavin/gnuplot.html
#
# Use 'eog' eye of gnome to view the images


OUTPUT_IMG_FILE=image_gc_analysis.png
INPUT_PLOT_FILE=image_gc_analysis.txt

# Output these commands to a temp file and then run gnu plot 
echo "set terminal png
set output '${OUTPUT_IMG_FILE}'
set title 'GC results'
set size 1,1
set key left top
set autoscale
set xlabel 'gc_iter'
set ylabel 'gc_main_paus_time'
plot '${INPUT_PLOT_FILE}' using 1:6 title 'GC Results'
" > gnuplot_tmp_cmd.tmp 

# Run the gnu plot command
gnuplot gnuplot_tmp_cmd.tmp > /dev/null

# For future reference, add a command and title to compare results 
# E.g.
# '/tmp/data2' using 10 with lines title 'Benchmark 2', 

