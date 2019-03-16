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
gawk --version
(time gawk -f ./bmbench.awk) 2>&1


echo ""
echo "----------"
echo "bwbasic"
(time bwbasic ./bmbench_bw.bas) 2>&1

echo ""
echo "----------"
echo "BC"
export BC_LINE_LENGTH=80
(time bc ./bmbench.bc) 2>&1

echo "----------"
echo "C (-O0)"
(gcc -v) 2>&1
#gcc -Wall -pedantic bmbench.c -o bmbench_c_o0_${SYSNAME}
gcc -Wall -Wtraditional bmbench.c -o bmbench_c_o0_${SYSNAME}
(time ./bmbench_c_o0_${SYSNAME}) 2>&1

echo "----------"
echo "C (-O2)"
(gcc -v) 2>&1
gcc -Wall -Wtraditional -O2 bmbench.c -o bmbench_c_o2_${SYSNAME}
(time ./bmbench_c_o2_${SYSNAME}) 2>&1

echo "----------"
echo "Forth"
#gforth bmbench.fs -e bye
(time gforth ./bmbench.fs -e bye) 2>&1

#Benchmarks:
#bubble.fs siev.fs matrix.fs fib.fs


echo "----------"
echo "Fortran (-O0)"
(g77 -v) 2>&1
#g77 -Wall -Wsurprising -Wunused -fpedantic bmbench.f -o bmbench_f_o0_${SYSNAME}
#pedantic seems to compile slow code, so we don't use it!
g77 -Wall -Wsurprising -Wunused bmbench.f -o bmbench_f_o0_${SYSNAME}
(time ./bmbench_f_o0_${SYSNAME}) 2>&1

echo "----------"
echo "Fortran (-O2)"
g77 -Wall -Wsurprising -Wunused bmbench.f -O2 -o bmbench_f_o2_${SYSNAME}
(time ./bmbench_f_o2_${SYSNAME}) 2>&1

echo "----------"
echo "Java (Sun) (-O)"
(/usr/lib/SunJava1-1.1.8/bin/java -version) 2>&1
/usr/lib/SunJava1-1.1.8/bin/javac -O bmbench.java
(time /usr/lib/SunJava1-1.1.8/bin/java bmbench) 2>&1
mv ./bmbench.class ./bmbench.class_sun118

echo "----------"
echo "Java (IBM) (-O)"
(/usr/lib/IBMJava2-1.3.0/bin/java -version) 2>&1
/usr/lib/IBMJava2-1.3.0/bin/javac -O bmbench.java
(time /usr/lib/IBMJava2-1.3.0/bin/java bmbench) 2>&1
mv ./bmbench.class ./bmbench.class_ibm130

echo "----------"
echo "Java (IBM, compiled with guavac)"
(/usr/lib/guavac/bin/guavac -version) 2>&1
/usr/lib/guavac/bin/guavac bmbench.java
(time /usr/lib/IBMJava2-1.3.0/bin/java bmbench) 2>&1
mv ./bmbench.class ./bmbench.class_guavac12


echo "----------"
echo "JavaScript"
echo "Start browser yourself... (script run_bench_browser1.sh)"

echo "----------"
echo "JavaScript (NGS) (-O2)"
js -V
js -c -O2 bmbench.js
(time js bmbench.jsc) 2>&1

echo "----------"
echo "JavaScript (Rhino)"
(time /usr/lib/IBMJava2-1.3.0/bin/java -jar js.jar bmbench.js) 2>&1
# Needs Rhina Java archive: ...rhino1_5R3/js.jar


echo "----------"
echo "Modula-2"
echo "q" | mocka
mocka -noindex -norange -nog -c bmbench
mocka -p bmbench
mv ./bmbench bmbench_mi_${SYSNAME}
(time ./bmbench_mi_${SYSNAME}) 2>&1

echo "----------"
echo "Pascal (-O2)"
(gpc -v) 2>&1
gpc bmbench.p -O2 -o bmbench_p_${SYSNAME}
(time ./bmbench_p_${SYSNAME}) 2>&1

echo "----------"
echo "Perl"
perl -v
(time perl ./bmbench.pl) 2>&1

echo "----------"
echo "Python (-O)"
(python -V) 2>&1
(time python -O ./bmbench.py) 2>&1

echo "----------"
echo "Smalltalk (not yet implemented)"
gst -v
#(time ./bmbench_c_o2_${SYSNAME}) 2>&1

echo "----------"
echo "Tcl"
(time tclsh ./bmbench.tcl) 2>&1

echo "----------"
echo "finished."
