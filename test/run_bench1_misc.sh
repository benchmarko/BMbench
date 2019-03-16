#!/bin/sh
# run_bench1.sh
#
# Usage:
# ./run_bench1.sh | tee 00results1_log_linux1.txt
# (./run_bench1.sh 2>&1 | tee 00results1_log_linux1.txt)
#


#if [ ! -d ./tmp ]; then
#  echo "local tmp directory not found."
#  exit 1
#fi

#cp -p bmbench.* ./tmp/

if [ -d ./tmp ]; then
  cd ./tmp
fi


SYSNAME=`uname -s`
# e.g. Linux

echo $SYSNAME

#echo "." > /dev/stderr
echo ""
echo "----------"
echo "AWK"
gawk_304 --version
(time gawk_304 -f ./bmbench.awk) 2>&1

echo ""
echo "----------"
echo "BC"
export BC_LINE_LENGTH=80
(time bc_104 ./bmbench.bc) 2>&1

echo "----------"
echo "Perl"
perl_5.005 -v
(time perl_5.005 -I$HOME/bin/5.00503/ ./bmbench.pl) 2>&1

echo "----------"
echo "Tcl"
export LD_LIBRARY_PATH=$HOME/bin # it needs libtcl7.6.so
(time tclsh_76 ./bmbench.tcl) 2>&1

echo "----------"
echo "finished."
