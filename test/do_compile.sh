#!/bin/sh
#do_compile.sh
#


booc
#Boo Compiler version 0.7.8.2559 (CLR v2.0.50727.42)
#Fatal error: No inputs specified.
booc -checked- -debug- -out:bmbench_boo_booc_0.7.8.2559.exe bmbench.boo


mcs --version
#Mono C# compiler version 1.9.0.0
mcs -optimize+ -warn:4 -out:bmbench_cs_mcs_1.9.0.0.exe bmbench.cs
# -optimize+ no effect?


gcc -dumpversion
#4.2.1
gcc -Wall -O0 bmbench.c -o bmbench_c_gcc_4.2.1_O0
gcc -Wall -O2 bmbench.c -o bmbench_c_gcc_4.2.1_O2


g++ -dumpversion
#4.2.1
g++ -Wall -O0 bmbench.cpp -o bmbench_cpp_gpp_4.2.1_O0
g++ -Wall -O2 bmbench.cpp -o bmbench_cpp_gpp_4.2.1_O2


javac -version
#javac 1.5.0_15
#javac: no source files
#...
javac -O bmbench.java
cp -p bmbench.class bmbench_java_javac_1.5.0_15.class

#TTT
#javac -Xlint -O -d t1 bmbench.java
#bmbench.java:33: warning: [serial] serializable class bmbench has no definition of serialVersionUID
#public class bmbench extends java.applet.Applet {
#       ^
#


#js_ngs -help
js_ngs --version
#NGS JavaScript Interpter 0.2.5
#Copyright (C) 1998 New Generation Software (NGS) Oy.
#...
js_ngs -c -O2 bmbench.js
mv bmbench.jsc bmbench_js_js_ngs0.2.5_O2.jsc



#js_sp -h
#JavaScript-C 1.5 pre-release 5 2003-01-10
#usage: js [-s] [-w] [-W] [-b branchlimit] [-c stackchunksize] [-v version] [-f scriptfile] [scriptfile] [scriptarg...]
#js_sp bmbench.js


#js -h
#JavaScript-C 1.5 2004-09-24
#usage: js [-PswW] [-b branchlimit] [-c stackchunksize] [-v version] [-f scriptfile] [-S maxstacksize] [scriptfile] [scriptarg...]
#js bmbench.js


#java -jar /home/ali/usr/bin/js.jar bmbench.js


#Rhino
#java -classpath /home/ali/usr/bin/js.jar org.mozilla.javascript.tools.jsc.Main -h

#java -classpath /home/ali/usr/bin/js.jar org.mozilla.javascript.tools.jsc.Main -opt 0 bmbench.js
#java -classpath /home/ali/usr/bin/js.jar org.mozilla.javascript.tools.jsc.Main -opt 9 bmbench.js
#->bmbench.class
#java -classpath /home/ali/usr/bin/js.jar bmbench


#ds bmbench.js


#???
#perlcc -O -o bmbench_pl_perlcc_xxx bmbench.pl
#-> errors

# perl byte code: ok
#perlcc -B -o bmbench_pl_perlcc_xxx bmbench.pl
###

# perl bmbench.pl4
#gettimeofday: Das Argument ist ungültig at bmbench.pl4 line 346.

#develop/bmbench> find /usr/lib/perl5 -name "syscall.ph" -print
#/usr/lib/perl5/vendor_perl/5.8.8/i586-linux-thread-multi/syscall.ph
#/usr/lib/perl5/vendor_perl/5.8.8/i586-linux-thread-multi/bits/syscall.ph
#/usr/lib/perl5/vendor_perl/5.8.8/i586-linux-thread-multi/sys/syscall.ph

#/usr/lib/perl5/vendor_perl/5.8.8/i586-linux-thread-multi/bits/syscall.ph
#unless(defined(&SYS_gettimeofday)) {
#    sub SYS_gettimeofday () {    &__NR_gettimeofday;}

