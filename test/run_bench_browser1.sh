#!/bin/sh
# run_bench1.sh
#
# Usage:
# ./run_benchbrowser1.sh
#

DIR1=`pwd`

echo "----------"
echo "JavaScript (Netscape)"
netscape file:${DIR1}/bmbench.html

echo "----------"
echo "JavaScript (Netscape 6)"
netscape6 file:${DIR1}/bmbench.html

echo "----------"
echo "JavaScript (Mozilla)"
mozilla file:${DIR1}/bmbench.html

echo "----------"
echo "JavaScript (Konqueror)"
konqueror file:${DIR1}/bmbench.html

echo "----------"
echo "finished."
