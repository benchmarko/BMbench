 IDENTIFICATION DIVISION.
 PROGRAM-ID. bmbench.
 AUTHOR. Marco Vieth.
 DATE-WRITTEN. 05.04.2003.

* BM Bench - bmbench.cob (Cobol)
* (c) Marco Vieth, 2002
* http://www.benchmarko.de
*
* 05.04.2003 0.01 first tests
* 06.04.2003 0.02 bench01 = (sum 1..n) mod 65536 (integer)
* 11.04.2003 0.05 other benchmark tests
*
*
*
* Usage (with TinyCobol):
* - Try to use static libraries!
*   They are twice as fast for bench02!
* (0.58 uses static, 0.60 uses dynamic as default)
* - (Dynamic libraries are used first if available.
*    export LD_LIBRARY_PATH=$HOME/bin ...)
* - htcobol -L$LD_LIBRARY_PATH -F bmbench.cob
* - ./bmbench [bench1] [bench2] [n]
*
*
* Hints to compile TinyCobol (http://www.tinycobol.org/):
* - TinyCobol (needs flex, ncurses, ncurses_devel, db-devel)
* - ./configure --prefix=/home/ali/usr; make
*
* Hints to compile OpenCobol:
* ./configure --prefix=/home/ali/usr
* make
* ...
*


* Notes (TinyCobol):
* - Data definition:
* - '01 x PIC S9(12) value 0'  is slower than
* - '01 x USAGE COMP-5 PIC S9(12) value 0' is slower than
* - '01 x USAGE BINARY PIC S9(12) value 0'
* - [is slower than: '01 x USAGE BINARY PIC S9(10) value 0']
* - Loops:
* - 'PERFORM VARYING i FROM n BY -1 UNTIL i <= 0' is slower than
* - 'PERFORM VARYING i FROM 1 BY 1 UNTIL i > n' is slower than
* - 'PERFORM UNTIL i <= 0 ... SUBTRACT 1 FROM i' is slower than
* - 'PERFORM n TIMES ... ADD 1 TO i'
* - Computing with same data types (e.g. FP) is faster than
*   mixed data types.
*
*
* Tips:
* - 'DIVIDE x-s BY 65536 GIVING x-help REMAINDER x-s'
*
*
*

 ENVIRONMENT DIVISION.

 DATA DIVISION.
 WORKING-STORAGE SECTION.

 01 args-str PIC X(40).
 01 args0 PIC X(40).
 01 args1 PIC X(40).
 01 args2 PIC X(40).
 01 args3 PIC X(40).

 01 w01-date PIC 9(6).

 01 w01-time PIC 9(8).
 01 w01-time-redef REDEFINES w01-time.
   05 w05-hour PIC 9(2).
   05 w05-min PIC 9(2).
   05 w05-sec PIC 9(2).
   05 w05-hsec PIC 9(2).

 01 x  USAGE BINARY PIC S9(12) value 0.
 01 x-s USAGE BINARY PIC S9(4) value 0.
 01 x-d USAGE COMP-2 value 0.
 01 sum1 USAGE BINARY PIC S9(12) value 0.
 01 sum1-s USAGE BINARY PIC S9(4) value 0.
 01 sum1-d USAGE COMP-2 value 0.
 01 x-help USAGE BINARY PIC S9(9) value 0.
 01 i  USAGE BINARY PIC S9(9) value 0.
 01 i-s USAGE BINARY PIC S9(4) value 0.
 01 i-d USAGE COMP-2 value 0.
 01 j  USAGE BINARY PIC S9(9) value 0.
 01 check1 USAGE BINARY PIC S9(12) value 0.

 01 bench1 USAGE BINARY PIC S9(9) value 0.
 01 bench2 USAGE BINARY PIC S9(9) value 5.
 01 bench USAGE BINARY PIC S9(9) value 0.
 01 n  USAGE BINARY PIC S9(9) value 1000000.
 01 min-ms USAGE BINARY PIC S9(9) value 10000.
 01 loops USAGE BINARY PIC S9(9) value 1.
 01 loops-save USAGE BINARY PIC S9(9) value 1.

 01 t1  USAGE BINARY PIC S9(9) value 0.
 01 th  USAGE BINARY PIC S9(9) value 0.
 01 start-t USAGE BINARY PIC S9(9) value 0.

 01 bench-res1-array.
   05 bench-res1 OCCURS 6 TIMES USAGE BINARY PIC S9(9).


* data for bench00:
 01 bench00-n-div-q USAGE BINARY PIC S9(4) value 0.
 01 bench00-n-mod-q USAGE BINARY PIC S9(4) value 0.

* data for bench03:
 01 bench03-save-n USAGE BINARY PIC S9(9) value 0.
 01 bench03-x USAGE BINARY PIC S9(9) value 0.
 01 bench03-sieve1-array.
   05 bench03-sieve1 OCCURS 500001 TIMES PIC X.
 01 bench03-n2 USAGE BINARY PIC S9(9) value 0.

* data for bench04:
 01 bench04-m USAGE BINARY PIC S9(10) value 2147483647.
 01 bench04-a USAGE BINARY PIC S9(10) value 16807.
 01 bench04-q USAGE BINARY PIC S9(10) value 127773.
 01 bench04-r USAGE BINARY PIC S9(10) value 2836.
 01 bench04-x-div-q USAGE BINARY PIC S9(10) value 0.
 01 bench04-x-mod-q USAGE BINARY PIC S9(10) value 0.

* data for bench05:
 01 bench05-save-n USAGE BINARY PIC S9(9) value 0.
 01 bench05-k USAGE BINARY PIC S9(9) value 0.
 01 bench05-pas1-array.
   05 bench05-pas1-row OCCURS 2 TIMES.
     10 bench05-pas1 OCCURS 2001 TIMES USAGE BINARY PIC S9(4).
 01 bench05-n2 USAGE BINARY PIC S9(9) value 0.
 01 bench05-i-mod-2 USAGE BINARY PIC S9(9) value 0.
 01 bench05-i-mod-2-1 USAGE BINARY PIC S9(9) value 0.
 01 bench05-min1 USAGE BINARY PIC S9(9) value 0.


* data for checkbits:
 01 num USAGE BINARY PIC S9(12) value 0.
 01 num-s USAGE BINARY PIC S9(4) value 0.
* 16 bit: 01 num-s USAGE BINARY PIC 9(5) value 0.
 01 num-f USAGE COMP-1 PIC S9(12) value 0.
 01 num-d USAGE COMP-2 PIC S9(12) value 0.
 01 last-num USAGE BINARY PIC S9(12) value 0.
 01 last-num-s USAGE BINARY PIC S9(4) value 0.
 01 last-num-f USAGE COMP-1 PIC S9(9) value 0.
 01 last-num-d USAGE COMP-2 PIC S9(9) value 0.
 01 bits USAGE BINARY PIC S9(9) value 0.


* data for number printing
* 01 num-str PIC Z(19) JUSTIFIED RIGHT.
* 01 h1 USAGE BINARY PIC S9(9) value 0.

* 01 h1 PIC S9(10).
*   88 h1-c value 2147483647.


 PROCEDURE DIVISION.

*  MOVE bench04-r to num-str.
*  INSPECT num-str REPLACING LEADING SPACES BY LOW-VALUE.
*  DISPLAY "num-str='" num-str "'".

*  MOVE 2.6 TO h1.
*  DISPLAY "h1='" h1 "'".

  ACCEPT ARGS-STR FROM COMMAND-LINE.
  PERFORM main-form.
  STOP RUN.

bench00.
  MOVE 0 TO x-s.
  COMPUTE sum1 = ((n / 2) * (n + 1)).
* (sum1..1000000 depends on type: 500000500000 (floating point),
* 1784293664 (32bit), 10528 (16 bit)
  MOVE sum1 TO sum1-s.
  COMPUTE bench00-n-div-q = n / 65536.
  COMPUTE bench00-n-mod-q = n - bench00-n-div-q * 65536.
  PERFORM UNTIL loops <= 0
    PERFORM bench00-n-div-q TIMES
      MOVE 1 TO i-s
      PERFORM 65536 TIMES
        ADD i-s TO x-s
        ADD 1 TO i-s
*  DISPLAY "DEBUG: bench00: x-s=" x-s
      END-PERFORM
    END-PERFORM

    MOVE 1 TO i-s
    PERFORM bench00-n-mod-q TIMES
      ADD i-s TO x-s
      ADD 1 TO i-s
    END-PERFORM

*  DISPLAY "DEBUG: bench00: x-s=" x-s ", sum1-s=" sum1-s ",\
*   n-div-q=" bench00-n-div-q ", n-mod-q=" bench00-n-mod-q ", curr. loop=" loops

    COMPUTE loops = loops - 1

    IF loops > 0
      SUBTRACT sum1-s FROM x-s
      IF (x-s <> 0)
        ADD 1 TO x-s
*  break;
      END-IF
    END-IF
  END-PERFORM
  COMPUTE x-help = x-s / 65536.
  COMPUTE x = x-s - (x-help * 65536).


bench01.
  MOVE 0 TO x.
  COMPUTE sum1 = ((n / 2) * (n + 1)).
* (sum1..1000000 depends on type: 500000500000 (floating point),
* 1784293664 (32bit), 10528 (16 bit)

  PERFORM UNTIL loops <= 0
* or: PERFORM loops TIMES
    MOVE 1 TO i
    PERFORM n TIMES
      ADD i TO x
      ADD 1 TO i
    END-PERFORM

*  DISPLAY "DEBUG: bench01: x=" x ", sum1=" sum1 \
*  ", curr. loop=" loops

    COMPUTE loops = loops - 1

    IF loops > 0
      SUBTRACT sum1 FROM x
      IF (x <> 0)
        ADD 1 TO x
*  break;
      END-IF
    END-IF

  END-PERFORM.

  COMPUTE x-help = x / 65536.
  COMPUTE x = x - (x-help * 65536).


*
*
*
bench02.
* DISPLAY "DEBUG: bench02: loops=" loops ", n=" n.
  MOVE 0 TO x-d.
  COMPUTE sum1-d = ((n / 2.0) * (n + 1.0)).
* (sum1..1000000 depends on type: 500000500000 (floating point),
*  1784293664 (32bit), 10528 (16 bit)

  PERFORM UNTIL loops <= 0
* or: PERFORM loops TIMES
    MOVE 1 TO i-d
    PERFORM n TIMES
      ADD i-d TO x-d
      ADD 1 TO i-d
* using 1.0 as constant is slower, so use 1!
    END-PERFORM

*  DISPLAY "DEBUG: bench02: x=" x-d ", sum1=" sum1-d \
*  ", curr. loop=" loops

* we need to modify loops because we want to detect
* the last loop...
    COMPUTE loops = loops - 1
    IF loops > 0
      SUBTRACT sum1-d FROM x-d
* use a delta to compare...
      IF (x-d > 0.000001)
        ADD 1 TO x-d
*  break;
      END-IF
    END-IF
  END-PERFORM.

  COMPUTE x-help = x-d / 65536.
  COMPUTE x = x-d - (x-help * 65536).


bench03.
  MOVE n TO bench03-save-n.
  COMPUTE n = n / 2.
  MOVE 0 TO bench03-x.
* we don't have index 0, son don't save number 0.
  MOVE 0 TO bench03-sieve1(1).

  PERFORM UNTIL loops <= 0
* Initialize sieve
    MOVE 2 TO i
    COMPUTE bench03-n2 = n - 1
*    DISPLAY "DEBUG: bench03-n2=" bench03-n2 ", loops=" loops
    PERFORM bench03-n2 TIMES
      MOVE 1 TO bench03-sieve1(i)
      ADD 1 TO i
    END-PERFORM
* Compute primes
    MOVE 2 TO i
    PERFORM UNTIL (i * i) > n
      IF (bench03-sieve1(i) > 0)
        COMPUTE j = i * i
        PERFORM UNTIL j > n
          MOVE 0 TO bench03-sieve1(j)
          COMPUTE j = j + i
        END-PERFORM
      END-IF
      COMPUTE i = i + 1
    END-PERFORM
* Count primes
    MOVE 1 TO i
    COMPUTE bench03-n2 = n - 1
    PERFORM bench03-n2 TIMES
      IF bench03-sieve1(i) > 0
        ADD 1 TO bench03-x
      END-IF
      ADD 1 TO i
    END-PERFORM

    COMPUTE loops = loops - 1
    IF loops > 0
      SUBTRACT 41538 FROM bench03-x
      IF (x > 0)
        ADD 1 TO bench03-x
*  break;
      END-IF
    END-IF
  END-PERFORM.
  MOVE bench03-save-n TO n.
  MOVE bench03-x TO x


*
*
* we need 10 digits for x, check1...
bench04.
  MOVE 1 TO x.
* DISPLAY "DEBUG: bench04: TT0: x=" x ", n=" n
  PERFORM UNTIL loops <= 0
    PERFORM n TIMES
      COMPUTE bench04-x-div-q = x / bench04-q
      COMPUTE bench04-x-mod-q = x - bench04-q * bench04-x-div-q
      COMPUTE x = bench04-a * bench04-x-mod-q - bench04-r * bench04-x-div-q
      IF x <= 0
        ADD bench04-m to x
      END-IF
    END-PERFORM
*    DISPLAY "DEBUG: bench04: x=" x ", curr. loop=" loops
    COMPUTE loops = loops - 1
    IF loops > 0
      SUBTRACT 1227283347 FROM x
      IF (x <> 0)
        ADD 1 TO x
*  break;
      END-IF
      ADD 1 TO x
    END-IF
  END-PERFORM


*
*
*
bench05.
  MOVE 0 TO x.
  MOVE n TO bench05-save-n
  COMPUTE n = n / 500.
*  DISPLAY "n=" n.
  COMPUTE bench05-k = n / 2.
  IF ((n - bench05-k) < bench05-k)
    COMPUTE bench05-k = n - bench05-k
  END-IF.

  IF n > 2000
    DISPLAY "WARNING: n > 2000!"
  END-IF

  MOVE 1 TO bench05-pas1(0 + 1, 0 + 1).
  MOVE 1 TO bench05-pas1(1 + 1, 0 + 1).
  PERFORM UNTIL loops <= 0
    MOVE 2 TO i
    COMPUTE bench05-n2 = n - 1
    PERFORM bench05-n2 TIMES
      COMPUTE x-help = i / 2
      COMPUTE bench05-i-mod-2 = i - (x-help * 2)

      COMPUTE bench05-min1 = (i - 1) / 2
      IF (bench05-k < bench05-min1)
        MOVE bench05-k TO bench05-min1
      END-IF
      COMPUTE x-help = (i + 1) / 2
      COMPUTE bench05-i-mod-2-1 = (i + 1) - (x-help * 2)
      MOVE i TO bench05-pas1(bench05-i-mod-2 + 1, 1 + 1)

      MOVE 2 TO j
      COMPUTE bench05-n2 = bench05-min1 - 1
      PERFORM bench05-n2 TIMES
        COMPUTE bench05-pas1(bench05-i-mod-2 + 1, j + 1) =
          (bench05-pas1(bench05-i-mod-2-1 + 1, j - 1 + 1)
          + bench05-pas1(bench05-i-mod-2-1 + 1, j + 1))
        ADD 1 TO j
      END-PERFORM
      IF ((bench05-min1 < bench05-k) AND (bench05-i-mod-2 = 0))
        COMPUTE bench05-pas1(bench05-i-mod-2 + 1, bench05-min1 + 1 + 1) =
          2 * bench05-pas1(bench05-i-mod-2-1 + 1, bench05-min1 + 1)
      END-IF
      ADD 1 TO i
    END-PERFORM
    
    COMPUTE x-help = n / 2
    COMPUTE bench05-i-mod-2 = n - (x-help * 2)
    COMPUTE x = x + bench05-pas1(bench05-i-mod-2 + 1, bench05-k + 1)

    COMPUTE loops = loops - 1
    IF loops > 0
      SUBTRACT 27200 FROM x
      IF (x <> 0)
        ADD 1 TO x
*  break;
      END-IF
    END-IF
  END-PERFORM.
  MOVE bench05-save-n TO n.



*
* run a benchmark
* in: bench = benchmark to use
* loops = number of loops
*  n = maximum number (used in some benchmarks to define size
* of workload)
* out: x = result
*
run-bench.
  MOVE 0 TO x.
  MOVE 0 TO check1.
  MOVE loops TO loops-save.
  EVALUATE bench
   WHEN 0
     PERFORM bench00
     MOVE 10528 TO check1
   WHEN 1
     PERFORM bench01
     MOVE 10528 TO check1
   WHEN 2
     PERFORM bench02
     MOVE 10528 TO check1
   WHEN 3
     PERFORM bench03
     MOVE 41538 TO check1
   WHEN 4
     PERFORM bench04
     MOVE 1227283347 TO check1
   WHEN 5
     PERFORM bench05
     MOVE 27200 TO check1
   WHEN OTHER
     DISPLAY "Error: Unknown benchmark: " bench
     COMPUTE CHECK1 = x + 1
  END-EVALUATE.
  IF check1 <> x
    DISPLAY "Error(bench" bench "): x=" x
    MOVE -1 TO x
  END-IF
  MOVE loops-save TO loops.

*
*
*
get-ms.
  ACCEPT w01-time FROM TIME.
  COMPUTE t1 = (((((w05-hour * 60) + w05-min) * 60) + w05-sec)
    * 1000) + (w05-hsec * 10).
* DISPLAY "DEBUG: Redefined time: " w05-hour ":" w05-min ":"
* w05-sec "." w05-hsec.
* DISPLAY "DEBUG: The system time is: " t1.


*
*
* Here we compute the number of "significant" bits for
* positive numbers (which means 53 for double)
checkbits-short1.
  MOVE 1 TO num-s.
  MOVE 0 TO last-num-s.
  MOVE 0 TO bits.
  PERFORM UNTIL (((num-s - 1) / 2) <> last-num-s) OR (bits >= 101)
    MOVE num-s to last-num-s
    COMPUTE num-s = num-s * 2
    COMPUTE num-s = num-s + 1
    COMPUTE bits = bits + 1
  END-PERFORM

checkbits-int1.
  MOVE 1 TO num.
  MOVE 0 TO last-num.
  MOVE 0 TO bits.
  PERFORM UNTIL (((num - 1) / 2) <> last-num) OR (bits >= 101)
    MOVE num to last-num
    COMPUTE num = num * 2
    COMPUTE num = num + 1
    COMPUTE bits = bits + 1
  END-PERFORM

checkbits-float1.
  MOVE 1 TO num-f.
  MOVE 0 TO last-num-f.
  MOVE 0 TO bits.
  PERFORM UNTIL (((num-f - 1) / 2) <> last-num-f) OR (bits >= 101)
    MOVE num-f to last-num-f
    COMPUTE num-f = num-f * 2
    COMPUTE num-f = num-f + 1
    COMPUTE bits = bits + 1
  END-PERFORM

checkbits-double1.
  MOVE 1 TO num-d.
  MOVE 0 TO last-num-d.
  MOVE 0 TO bits.
  PERFORM UNTIL (((num-d - 1) / 2) <> last-num-d) OR (bits >= 101)
    MOVE num-d to last-num-d
    COMPUTE num-d = num-d * 2
    COMPUTE num-d = num-d + 1
    COMPUTE bits = bits + 1
  END-PERFORM

*
*
*
main-form.

  PERFORM get-ms.
  MOVE t1 TO start-t.

  UNSTRING args-str DELIMITED BY ' ' INTO args0 args1 args2 args3.

* DISPLAY "DEBUG: args1=" args1.
  IF args1 <> SPACE
    MOVE args1 TO bench1
*   COMPUTE bench1 = FUNCTION NUMVAL(args1)
  END-IF

  IF args2 <> SPACE
    MOVE args2 TO bench2
   END-IF

  IF args3 <> SPACE
    MOVE args3 TO n
  END-IF

  DISPLAY "BM Bench v0.5 (Cobol) -- (" WITH NO ADVANCING.
  PERFORM checkbits-short1.
  DISPLAY "short:" bits WITH NO ADVANCING.
  PERFORM checkbits-int1.
  DISPLAY " int:" bits WITH NO ADVANCING.
  PERFORM checkbits-float1.
  DISPLAY " float:" bits WITH NO ADVANCING.
  PERFORM checkbits-double1.
  DISPLAY " double:" bits WITH NO ADVANCING.
  DISPLAY ") --".
  DISPLAY "(c) Marco Vieth, 2002".

  ACCEPT w01-date FROM DATE.
  ACCEPT w01-time FROM TIME.
* MOVE FUNCTION CURRENT-DATE to w01-datex.
  DISPLAY "Date: " w01-date " " w01-time.

  MOVE bench1 to bench.
  PERFORM UNTIL bench > bench2
    MOVE 1 TO loops
    MOVE 0 TO x
    MOVE 0 TO t1

* Calibration
    PERFORM UNTIL (t1 >= 1001) OR (x = -1)
      DISPLAY "Calibrating benchmark " bench " with loops=" loops ", n=" n
      PERFORM get-ms
      MOVE t1 TO th
      PERFORM run-bench
      PERFORM get-ms
      COMPUTE t1 = t1 - th
      DISPLAY "x=" x " (time: " t1 " ms)"
      COMPUTE loops = loops * 2
*  DISPLAY "DEBUG: x=" x ", loops=" loops ", t1=" t1
    END-PERFORM

    IF x <> -1
      COMPUTE loops = loops / 2
      COMPUTE x-help = (min-ms / t1)
* int!
      COMPUTE loops = loops * x-help + 1
      DISPLAY "Calibration done. Starting measurement with " loops " loops to get >=" min-ms " ms"

* Measurement
      PERFORM get-ms
      MOVE t1 TO th
      PERFORM run-bench
      PERFORM get-ms
      COMPUTE t1 = t1 - th
      DISPLAY "x=" x " (time: " t1 " ms)"
      COMPUTE bench-res1(bench + 1) = (t1 * 10 / loops)
* int!
      DISPLAY "Elapsed time for " loops " loops: " t1 " ms; estimation for 10 loops: " bench-res1(bench + 1) " ms"
    ELSE
      COMPUTE bench-res1(bench + 1) = -1
      MOVE x TO x
    END-IF

    ADD 1 TO bench
  END-PERFORM.

  DISPLAY "Times for all benchmarks (10 loops, ms):".
  DISPLAY "BM Results (Cobol) : " WITH NO ADVANCING
  MOVE bench1 to bench.
  PERFORM UNTIL bench > bench2
    DISPLAY bench-res1(bench + 1) " " WITH NO ADVANCING
    ADD 1 TO bench
  END-PERFORM.
  DISPLAY "".
  PERFORM get-ms.

  COMPUTE t1 = t1 - start-t.
  DISPLAY "Total elapsed time: " t1 " ms".

END PROGRAM bmbench.
