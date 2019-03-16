# BMbench - BM Benchmark Suite

Version: 0.40
Date: 20.08.2002

BMbench is a collection of simple benchmarks in different programming languages.
It is a synthetic benchmark which means that it does not represent real workload.
Concerning the tests, it allows to compare the performance of different languages,
different implementations of the same language and maybe
the same language on different operating systems.

## Which benchmark tests are used

- The benchmark consists of up to 6 simple computations/algorithms (usually with n=1000000):
  - 0: (sum of 1..n) mod 65536  (small integer; 16 bit, if available)
  - 1: (sum of 1..n) mod 65536  (machine type integer; 32 bit preferred)
  - 2: (sum of 1..n) mod 65536  (floating point; 64 bit double preferred)
  - 3: Sieve of Eratosthenes: Number of primes below n/2 (bit array preferred; even numbers also stored)
  - 4: n-th pseudo random number from generator by Raj Jain
  - 5: (n=n/500): n over n/2 mod 65536 (Pascal's triangle)

## Implementation Details and Goals

- All benchmarks are programmed in the "best" known way which means:
  - They use the same (optimized) algorithm
  - They are performant (hopefully)
    (Since I do not know all the programming languages very well, there may be some optimizations,
     if you find some, please send me a note!)
  - They should be robust to optimizations (except for loop unrolling)
    - results are used and checked after execution
    - number of loops not a constant
    - loop unrolling should not be used
- It is implemented self-measuring:
  It reports elapsed time reported by functions available in the programming language.
- It is implemented self-calibrating:
  1. For every benchmark test the workload (number of loops) is doubled until the elapsed time
  is greater than 1001 ms (1 ms more than one second because some timing functons have a
  accuracy of 1 second only).
  2. Then the numer of loops is estimated so that the elapsed time will be greater than 10000 ms.
  3. The estimated elapsed time for 10 loops is reported as result.

## Implementation Status

(Status: Development status for every benchmark test 0-5: +=ok, -=problems, .=not implemented)

File |             Status |    Description
--- | --- | ---
README.md    |           |  this readme
bmbench.abap   |  .+++++   | ABAP/4 for SAP R/3 4.x
bmbench.awk    |  .+++++   | AWK for gawk
bmbench.bas    |  .-+...   | BASIC for (Locomotive) BASIC (still v0.1)
bmbench.bc     |  .+....   | BC for bc (arbitrary precision calculator)
bmbench.c      |  ++++++   | C for gcc, Microsoft C, Borland C, ...
bmbench.f      |  .++.+.   | Fortran for g77
bmbench.fs     |  .+....   | Forth for gforth
bmbench.html   |           | HTML page to start JavaScript from browser
bmbench.java   |  -+++++   | Java
bmbench.js     |  .+++++   | JavaScript for Browsers, Rhino, NGS
bmbench.mi     |  +++++-   | Modula-2 for mocka
bmbench.p      |  ++++++   | Pascal for gpc
bmbench.pl     |  .+++++   | Perl for Perl 5
bmbench.pl4    |  .x++++   | Perl for old Perl 4 (no Integer available)
bmbench.py     |  .+++++   | Python for python
bmbench.st     |  .-....   | Smalltalk for gst
bmbench.tcl    |  .+++++   | Tcl for tclsh
bmbench_bw.bas |  .x+...   | BASIC for bwbasic (Bywater BASIC Interpreter; no Integer available)
bmbench_java.html |        |  HTML page to start Java from browser (as an applet, not used)
bmbench_sto.bas | --+-..   | BASIC for Star Office 5.2
bmbench_vba.bas | .-....   | VBA (Visual Basic for Applications, e.g. Excel)
test/00results1.html |           | Benchmark results (HTML)
test/00results1.xls  |           | Benchmark results (Excel)
test/00results1_log_linux1.txt |  | Benchmark measurement log (Linux)
test/run_bench1.sh    |          | Shell script to run tests under UNIX (e.g. Linux)
test/run_bench1_misc.sh |        | Another shell script
test/run_bench_browser1.sh |     | Shell script to run browser tests under UNIX (e.g. Linux)

---

### Other possible languages

- gpp     GNU C++ Compiler
- gcj     The GNU Java Compiler
- clisp   A Common-Lisp interpreter
- m4      GNU m4
- mtc     Modula-2 / C Converter
- gst     GNU Smalltalk

---

### Links

- [BMbench documentation on benchmarko.de](http://www.benchmarko.de/software/bmbench/index_e.html)

### **Marco Vieth, 2002**
