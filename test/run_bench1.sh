#!/bin/sh
# run_bench1.sh
# Marco Vieth, 2002 (http://www.benchmarko.de)
#
# 29.01.2003  0.041 some improvements
# 05.04.2003  0.05  define tools at the beginning
#
#
#
# Usage:
# ./run_bench1.sh > 00results1_log_linux05.txt
# (./run_bench1.sh | tee 00results1_log_linux05.txt)
#

#
#
#
AWK="gawk"
BWBASIC="bwbasic"
BC="bc"
GCC="gcc"
HTCOBOL="htcobol -L$HOME/bin"
GFORTH="gforth"
G77="g77"
F2C="f2c"
JAVADIR_SUN="/usr/lib/SunJava1-1.1.8/bin"
JAVADIR_IBM="/usr/lib/IBMJava2-1.3.0/bin"
GUAVAC="/usr/lib/guavac/bin/guavac"
JS_NGS="js"
JS_RHINOJAR="js.jar"
JS_SPIDERMONKEY="js_sp"
CLISP="clisp"
MOCKA="mocka"
GPC="gpc"
P2C="p2c"
PERL4="perl4"
PERL="perl"
PYTHON="python"
GST="gst"
TCLSH="tclsh"
#
#
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
date

#echo "." > /dev/stderr
echo ""
echo "----------"
echo "AWK"
$AWK --version
(time $AWK -f ./bmbench.awk) 2>&1


echo ""
echo "----------"
echo "bwbasic"
(time $BWBASIC ./bmbench.bas) 2>&1

echo ""
echo "----------"
echo "BC"
export BC_LINE_LENGTH=80
(time $BC ./bmbench.bc) 2>&1

echo "----------"
echo "C (-O0)"
($GCC -v) 2>&1
# -pedantic could slow down code!
$GCC -Wall -Wtraditional bmbench.c -o bmbench_c_o0_${SYSNAME}
(time ./bmbench_c_o0_${SYSNAME}) 2>&1

echo "----------"
echo "C (-O2)"
#($GCC -v) 2>&1
$GCC -Wall -Wtraditional -O2 bmbench.c -o bmbench_c_o2_${SYSNAME}
(time ./bmbench_c_o2_${SYSNAME}) 2>&1


echo "----------"
echo "Cobol (htcobol)"
#($HTCOBOL -hhh) 2>&1
$HTCOBOL -o bmbench_cob_${SYSNAME}
(time ./bmbench_cob_${SYSNAME}) 2>&1


echo "----------"
echo "Forth"
($GFORTH -v) 2>&1
(time $GFORTH ./bmbench.fs -e bye) 2>&1

#Benchmarks:
#bubble.fs siev.fs matrix.fs fib.fs


echo "----------"
echo "Fortran (g77 -O0)"
($G77 -v) 2>&1
#g77 -Wall -Wsurprising -Wunused -fpedantic bmbench.f -o bmbench_f_o0_${SYSNAME}
#-fpedantic seems to compile slow code, so we don't use it!
$G77 -Wall -Wsurprising -Wunused bmbench.f -o bmbench_f_o0_${SYSNAME}
(time ./bmbench_f_o0_${SYSNAME}) 2>&1

echo "----------"
echo "Fortran (g77 -O2)"
$G77 -Wall -Wsurprising -Wunused -O2 bmbench.f -o bmbench_f_o2_${SYSNAME}
(time ./bmbench_f_o2_${SYSNAME}) 2>&1


echo "----------"
echo "Fortran (f2c -O2)"
ln -s bmbench.f bmbench_f_f2c_${SYSNAME}.f
($F2C -A -a bmbench_f_f2c_${SYSNAME}.f) 2>&1
$G77 -O2 bmbench_f_f2c_${SYSNAME}.f -o bmbench_f_f2c_o2_${SYSNAME}
(time ./bmbench_f_f2c_o2_${SYSNAME}) 2>&1



echo "----------"
echo "Java (Sun) (-O)"
(${JAVADIR_SUN}/java -version) 2>&1
ln -s bmbench.java bmbench_sun118.java
${JAVADIR_SUN}/javac -O bmbench_sun118.java
(time ${JAVADIR_SUN}/java bmbench_sun118) 2>&1

echo "----------"
echo "Java (IBM) (-O)"
(${JAVADIR_IBM}/java -version) 2>&1
ln -s bmbench.java bmbench_ibm130.java
${JAVADIR_IBM}/javac -O bmbench_ibm130.java
(time ${JAVADIR_IBM}/java bmbench_ibm130) 2>&1

echo "----------"
echo "Java (IBM, compiled with guavac)"
($GUAVAC -version) 2>&1
ln -s bmbench.java bmbench_guavac12.java
$GUAVAC bmbench_guavac12.java
(time ${JAVADIR_IBM}/java bmbench_guavac12) 2>&1


echo "----------"
echo "JavaScript"
echo "Start browser yourself... (script run_bench_browser1.sh)"

echo "----------"
echo "JavaScript (NGS js) (-O2)"
$JS_NGS -V
$JS_NGS -c -O2 bmbench.js
(time $JS_NGS bmbench.jsc) 2>&1

echo "----------"
echo "JavaScript (Rhino)"
(time ${JAVADIR_IBM}/java -jar $JS_RHINOJAR bmbench.js) 2>&1
# Needs Rhino Java archive: ...rhino1_5R3/js.jar

echo "----------"
echo "JavaScript (SpiderMonkey)"
$JS_SPIDERMONKEY -h
(time $JS_SPIDERMONKEY bmbench.js) 2>&1


echo "----------"
echo "Lisp (clisp)"
($CLISP --version) 2>&1
$CLISP -c bmbench.lisp
(time $CLISP bmbench.fas) 2>&1


echo "----------"
echo "Modula-2"
echo "q" | $MOCKA
$MOCKA -noindex -norange -nog -c bmbench
$MOCKA -p bmbench
mv ./bmbench bmbench_mi_${SYSNAME}
(time ./bmbench_mi_${SYSNAME}) 2>&1


echo "----------"
echo "Pascal (gpc -O2)"
($GPC -v) 2>&1
$GPC bmbench.p -O2 -o bmbench_p_${SYSNAME}
(time ./bmbench_p_${SYSNAME}) 2>&1

echo "----------"
echo "Pascal (p2c -O2)"
$P2C -LHP -a -o bmbench_p2c_${SYSNAME}.c bmbench.p
$GCC -O2 bmbench_p2c_${SYSNAME}.c -lp2c -o bmbench_p2c_${SYSNAME}
# we skip -Wall -Wtraditional
(time ./bmbench_p2c_${SYSNAME}) 2>&1


echo "----------"
echo "Perl 4"
$PERL4 -v
(time $PERL4 ./bmbench.pl4) 2>&1

echo "----------"
echo "Perl"
$PERL -v
(time $PERL ./bmbench.pl) 2>&1


echo "----------"
echo "Python (-O)"
($PYTHON -V) 2>&1
(time $PYTHON -O ./bmbench.py) 2>&1


echo "----------"
echo "Smalltalk (not yet implemented)"
$GST -v
#(time $GST ./bmbench.st) 2>&1


echo "----------"
echo "Tcl"
(time $TCLSH ./bmbench.tcl) 2>&1


echo "----------"
date
echo "finished."
