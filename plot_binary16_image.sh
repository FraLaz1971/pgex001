#!/bin/sh
echo arguments $#
if [ $# -lt 3 ]
then
    echo "usage:$0 <infile.raw> <width> <height>"
    exit 1
fi
echo "set size ratio 1" > plot_binary_image.gp
echo "unset border" >> plot_binary_image.gp
echo "unset xlabel" >> plot_binary_image.gp
echo "unset ylabel" >> plot_binary_image.gp
echo "unset xtics" >> plot_binary_image.gp
echo "unset ytics" >> plot_binary_image.gp
echo "set title ' '" >> plot_binary_image.gp
echo 'set palette gray;' plot \'$1\' binary array=\($2,$3\) format=\'%uchar\' with image >> plot_binary_image.gp
gnuplot -p plot_binary_image.gp
