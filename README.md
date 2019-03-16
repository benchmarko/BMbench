# BMbench - BM Benchmark Suite

Version: 0.62
Date: 07.06.2017

BMbench is a collection of simple benchmarks in different programming languages.
It is a synthetic benchmark which means that it does not represent real workload.
Concerning the tests, it allows to compare the performance of different languages,
different implementations of the same language and maybe
the same language on different operating systems.

## Changes

### 0.62 (07.06.20177)

- Adapted scripts: kt (Kotlin)

### 0.61 (01.05.2008)

- Adapted scripts: awk, bash, boo, cob, lisp,...

### 0.60 (03.12.2006)

- Improved calibraton loop to speed up measurement
- New: bmbench.boo Boo scripting language for .Net or Mono
- Not all languages have been adapted, yet. Already adapted:
bmbench.abap, bmbench.boo, bmbench.c, bmbench.cpp, bmbench.cs, bmbench.java, bmbench.js, bmbench.pl, bmbench.pl4, bmbench.py, bmbench.tcl

### 0.51 (21.05.2006)

- New languages
  - bmbench.bash (bash, zsh)
  - bmbench.cpp (C++ version for GNU g++, Visual C++)
  - bmbench.cs (C# version für Mono, Visual C# .NET)
  - bmbench.hs (Haskell, experimenting)
  - bmbench.vb (Visual Basic .NET 2003, not Visual Basic 6.0)
- Updated
  - bmbench.js (support for JScript, DMDscript)
- Todo:
  - Some results

### 0.50 (25.01.2003)

### 0.40 (20.08.2002)

- Initial upload

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

Type: Main data types used:

- ? = unknown
- T = Bit
- B = Byte
- S = short (16 bit)
- I = integer (32 bit)
- L = long integer (any number of bits)
- D = double (e.g. 64 bit)

File            |   Status  | Type   | Description
--- | --- | --- | ---
README.md    |        |   |  this readme
bmbench.abap   |   .+++++  | ?IDTII | ABAP/4 for SAP R/3 4.x
bmbench.awk    |   ++++++  | DDDDDD | AWK for gawk
bmbench.bas    |   --+...  | ?????? | BASIC for Locomotive BASIC or bwbasic (Bywater BASIC Interpreter)
bmbench.bash   |   ++++++  | ?????? | Shell script for bash, zsh
bmbench.bc     |   .+....  | DDDDDD | BC for bc (arbitrary precision calculator, no timing)
bmbench.boo    |           |        | Boo scripting language for .Net or Mono
bmbench.c      |   ++++++  | SIDTII | C for gcc, Microsoft C, Borland C (bcc), ...
bmbench.cpp    |   ++++++  | SIDTII | C++ for g++, Microsoft C++, ...
bmbench.cs     |   ++++++  | IIDBII | C# for Mono, Microsoft C# .NET
bmbench.f      |   ++++++  | SID?II | Fortran for g77 or f2c
bmbench.fs     |   .+....  | ?????? | Forth for gforth (to do)
bmbench.hs     |   ......  | ?????? | Haskell (experimenting with ghc)
bmbench.html   |           | ?????? | HTML page to start JavaScript from browser
bmbench.java   |   ++++++  | SIDTII | Java
bmbench.js     |   ++++++  | IIDIII | JavaScript for Browsers, Rhino, NGS (has integer), JScript, DMDscript
bmbench.kt     |   ......  | ?????? | Kotlin
bmbench.lisp   |   ++++++  | IIDIII | Lisp for clisp
bmbench.mi     |   ++++++  | SIDTII | Modula-2 for mocka
bmbench.p      |   ++++++  | SIDTII | Pascal for gpc, p2c
bmbench.pl     |   ++++++  | IIDIII | Perl for Perl 5
bmbench.pl4    |   ++++++  | DDDDDD | Perl for old Perl 4 (no Integer available)
bmbench.py     |   ++++++  | SLDIII | Python for python
bmbench.st     |   ++++++  | ILDBDI | Smalltalk for gst (I only 30 bit)
bmbench.tcl    |   ++++++  | IIDIII | Tcl for tclsh
bmbench_java.html |        | ?????? | HTML page to start Java from browser (as an applet, not used)
bmbench_sto.bas|   --+-..  | ?????? | BASIC for Star Office 5.2
bmbench.vb     |   ++++++  |SIDBII  | Visual Basic for Microsoft Visual Basic .NET 2003
bmbench_vba.bas|   .-....  | ?????? | VBA (Visual Basic for Applications, e.g. Excel)
test/00results1.html |         |  | Benchmark results (HTML)
test/00results1.xls  |         |  | Benchmark results (Excel)
test/00results1_log_linux1.txt | |  | Benchmark measurement log (Linux)
test/run_bench1.sh    |        |  | Shell script to run tests under UNIX (e.g. Linux)
test/run_bench1_misc.sh |      |  | Another shell script
test/run_bench_browser1.sh |   |  | Shell script to run browser tests under UNIX (e.g. Linux)

---

### Other possible languages

- gcj     The GNU Java Compiler
- clisp   A Common-Lisp interpreter
- m4      GNU m4
- mtc     Modula-2 / C Converter
- gcc-objc    Objective C
- gnat        GNU Ada95
- guile       GNU Scheme
- hugs98      Haskell Interpreter  (others: GHC HBC)
- mtc         Modula 2 to C converter
- ocaml       Objective Caml
- regina      REXX interpreter
- ruby        Ruby
- slib        Scheme Library
- SmallEiffel
- smlnj       ML (to install)
- swi-prolog  Prolog (to install)
- umb-scheme  Scheme
- xscheme     Scheme (to install)
- gambas      BASIC compiler
- pdksh       PD Korn Shell
- shsh        Scheme Shell
- oprofile, km_oprofile
- Cygnus Source Navigator
- selfPHP

---

### Links

- [BMbench documentation on benchmarko.de](http://www.benchmarko.de/software/bmbench/index_e.html)

### **Marco Vieth, 2017**
