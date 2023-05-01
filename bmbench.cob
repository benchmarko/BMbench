      >>SOURCE FORMAT IS FREE
 IDENTIFICATION DIVISION.
 PROGRAM-ID. bmbench.
 AUTHOR. Marco Vieth.
 DATE-WRITTEN. 05.04.2003.

*> BM Bench - bmbench.cob (Cobol)
*> (c) Marco Vieth, 2002-2006
*> http://www.benchmarko.de
*>
*> 05.04.2003 0.01  first tests
*> 06.04.2003 0.02  bench01 = (sum 1..n) mod 65536 (integer)
*> 11.04.2003 0.05  other benchmark tests
*> 01.05.2008 0.06  based on version 0.05
*> 25.03.2023 0.08  adapted for new version; bench05 optimized
*>
*>
*>
*> Usage (with TinyCobol):
*> - Try to use static libraries!
*>   They are twice as fast for bench02!
*> (0.58 uses static, 0.60 uses dynamic as default)
*> - (Dynamic libraries are used first if available.
*>    export LD_LIBRARY_PATH=$HOME/usr/lib ...)
*> - htcobol -L$LD_LIBRARY_PATH -F bmbench.cob
*> - ./bmbench [bench1] [bench2] [n]
*>
*>
*> Hints to compile TinyCobol (http://www.tinycobol.org/):
*> - TinyCobol (needs flex, ncurses, ncurses_devel, db-devel/libdb-4_5-devel)
*> - ./configure --prefix=/home/ali/usr; make
*>
*> Hints to compile OpenCobol (http://www.opencobol.org/):
*> - OpenCobol (needs libgmp/gmp-devel 4.1.2, ...)
*> ./configure --prefix=/home/ali/usr
*> make
*> ...
*>
*> https://www.onlinegdb.com/online_cobol_compiler
*>

*> Notes (TinyCobol):
*> - Data definition:
*> - '01 x PIC S9(12) value 0'  is slower than
*> - '01 x USAGE COMP-5 PIC S9(12) value 0' is slower than
*> - '01 x USAGE BINARY PIC S9(12) value 0'
*> - [is slower than: '01 x USAGE BINARY PIC S9(10) value 0']
*> - Loops:
*> - 'PERFORM VARYING i FROM n BY -1 UNTIL i <= 0' is slower than
*> - 'PERFORM VARYING i FROM 1 BY 1 UNTIL i > n' is slower than
*> - 'PERFORM UNTIL i <= 0 ... SUBTRACT 1 FROM i' is slower than
*> - 'PERFORM n TIMES ... ADD 1 TO i'
*> - Computing with same data types (e.g. FP) is faster than
*>   mixed data types.
*>
*>
*> Tips:
*> - 'DIVIDE x-s BY 65536 GIVING x-help REMAINDER x-s'
*> - there is no ELSE IF
*>
*>
*>

 ENVIRONMENT DIVISION.

 DATA DIVISION.
 WORKING-STORAGE SECTION.

 01 prg-version PIC X(4) value "0.08".
 01 prg-language PIC X(5) value "Cobol".

 01 args-str PIC X(40).
 01 args0 PIC X(40).
 01 args1 PIC X(40).
 01 args2 PIC X(40).
 01 args3 PIC X(40).
 01 args4 PIC X(40).

 01 w01-date PIC 9(6).

 01 w01-time PIC 9(8).
 01 w01-time-redef REDEFINES w01-time.
   05 w05-hour PIC 9(2).
   05 w05-min PIC 9(2).
   05 w05-sec PIC 9(2).
   05 w05-hsec PIC 9(2).

 01 x  USAGE BINARY PIC S9(12) value 0.
 01 x-s USAGE COMP-5 PIC S9(4) value 0.
 01 x-d USAGE COMP-2 value 0.
 01 sum1 USAGE BINARY PIC S9(12) value 0.
 *> 01 sum1-s USAGE BINARY PIC S9(4) value 0.
 01 sum1-d USAGE COMP-2 value 0.
 01 x-help USAGE BINARY PIC S9(9) value 0.
 01 i  USAGE BINARY PIC S9(9) value 0.
 01 i-s USAGE COMP-5 PIC S9(4) value 0.
 01 i-d USAGE COMP-2 value 0.
 01 j  USAGE BINARY PIC S9(9) value 0.
 01 check1 USAGE BINARY PIC S9(12) value 0.

 01 bench1 USAGE BINARY PIC 9 value 0.
 01 bench2 USAGE BINARY PIC 9 value 5.
 01 max-bench USAGE BINARY PIC 9 value 6.
 01 bench USAGE BINARY PIC 9 value 0.
 01 n  USAGE BINARY PIC 9(9) value 1000000.
 01 max-ms USAGE BINARY PIC S9(9) value 10000.
 01 cali-ms USAGE BINARY PIC S9(9) value 1001.
 01 delta-ms USAGE COMP-2 value 100.
 01 scale-fact USAGE BINARY PIC S9(9) value 0.
 
 *> 01 measCount USAGE BINARY PIC S9(9) value 0.
 01 g-tsMeasCnt USAGE BINARY PIC 9(9) value 0.
 01 g-tsPrecCnt USAGE BINARY PIC 9(9) value 0.
 01 g-tsPrecMs USAGE COMP-2 value 0.
 01 loops USAGE BINARY PIC S9(9) value 1.
 01 loops-save USAGE BINARY PIC S9(9) value 1.

 01 t0  USAGE BINARY PIC S9(9) value 0.
 01 t1  USAGE BINARY PIC S9(9) value 0.
 01 t1-d  USAGE COMP-2 value 0.
 01 tMeas0 USAGE COMP-2 value 0.
 01 tMeas1 USAGE COMP-2 value 0.
 01 tMeas USAGE COMP-2 value 0.
 01 tEsti USAGE COMP-2 value 0.
*> 01 loops-p-sec USAGE BINARY PIC 9(6)V9(3) value 0.
 01 loops-p-sec USAGE COMP-2 value 0.
 01 throughput USAGE COMP-2 value 0.
 01 t-delta USAGE COMP-2 value 0.
 
 01 start-t USAGE BINARY PIC S9(9) value 0.

 01 bench-res1-array.
   05 bench-res1 OCCURS 7 TIMES USAGE COMP-2.


*> data for bench00:
 01 bench00-n-div-q USAGE COMP-5 PIC S9(4) value 0.
 01 bench00-n-mod-q USAGE COMP-5 PIC S9(4) value 0.

*> data for bench03:
 01 bench03-save-n USAGE BINARY PIC S9(9) value 0.
 01 bench03-nHalf USAGE BINARY PIC S9(9) value 0.
 01 bench03-m USAGE BINARY PIC S9(9) value 0.
 01 bench03-x USAGE BINARY PIC S9(9) value 0.
 01 bench03-sieve1-array.
   05 bench03-sieve1 OCCURS 250001 TIMES PIC X.
 01 bench03-n2 USAGE BINARY PIC S9(9) value 0.

*> data for bench04:
 01 bench04-m USAGE BINARY PIC S9(10) value 2147483647.
 01 bench04-a USAGE BINARY PIC S9(10) value 16807.
 01 bench04-q USAGE BINARY PIC S9(10) value 127773.
 01 bench04-r USAGE BINARY PIC S9(10) value 2836.
 01 bench04-x-div-q USAGE BINARY PIC S9(10) value 0.
 01 bench04-x-mod-q USAGE BINARY PIC S9(10) value 0.

*> data for bench05:
 01 bench05-save-n USAGE BINARY PIC S9(9) value 0.
 01 bench05-k USAGE BINARY PIC S9(9) value 0.
 01 bench05-pas1-array.
*>   05 bench05-pas1-row OCCURS 2 TIMES.
*>   05 bench05-pas1 OCCURS 1251 TIMES USAGE BINARY PIC S9(4).
   05 bench05-pas1 OCCURS 1251 TIMES USAGE COMP-5 PIC S9(4).
 01 bench05-n2 USAGE BINARY PIC S9(9) value 0.
 01 bench05-i-mod-2 USAGE BINARY PIC S9(9) value 0.
*> 01 bench05-i-mod-2-1 USAGE BINARY PIC S9(9) value 0.
 01 bench05-min1 USAGE BINARY PIC S9(9) value 0.
 01 bench05-prev USAGE BINARY PIC S9(9) value 0.
 01 bench05-num USAGE BINARY PIC S9(9) value 0.
 
 01 bench06-flip1-d USAGE COMP-2 value 0.
 
*> data for bench03Check:
 01 bench03Check-isPrime PIC X.

*> data for checkbits:
 01 num USAGE BINARY PIC S9(12) value 0.
 01 num-s USAGE COMP-5 PIC S9(4) value 0.
*> 16 bit: 01 num-s USAGE BINARY PIC 9(5) value 0.
 01 num-f USAGE COMP-1 value 0.
 01 num-d USAGE COMP-2 value 0.
 01 last-num USAGE BINARY PIC S9(12) value 0.
 01 last-num-s USAGE COMP-5 PIC S9(4) value 0.
 01 last-num-f USAGE COMP-1 value 0.
 01 last-num-d USAGE COMP-2 value 0.
 01 bits USAGE BINARY PIC 9(9) value 0.


 01 measureBench-tMeas0 USAGE COMP-2 value 0.

 01 start-bench-n USAGE BINARY PIC S9(9) value 0.

 01 fmt10 USAGE BINARY PIC 9(7)V9(3) value 0.
 01 fmt09 USAGE BINARY PIC 9(6)V9(3) value 0.
 01 fmt09_2 USAGE BINARY PIC 9(6)V9(3) value 0.

*> data for number printing
*> 01 num-str PIC Z(19) JUSTIFIED RIGHT.
*> 01 h1 USAGE BINARY PIC S9(9) value 0.

*> 01 h1 PIC S9(10).
*>   88 h1-c value 2147483647.


 PROCEDURE DIVISION.

*>  MOVE bench04-r to num-str.
*>  INSPECT num-str REPLACING LEADING SPACES BY LOW-VALUE.
*>  DISPLAY "num-str='" num-str "'".

*>  MOVE 2.6 TO h1.
*>  DISPLAY "h1='" h1 "'".

  ACCEPT ARGS-STR FROM COMMAND-LINE.
  PERFORM main-form.
  STOP RUN.

bench00.
  MOVE 0 TO x-s
  COMPUTE bench00-n-div-q = n / 65536
  COMPUTE bench00-n-mod-q = n - bench00-n-div-q * 65536
*>  DISPLAY "DEBUG: dDivq=" bench00-n-div-q ", nModq=" bench00-n-mod-q
  PERFORM bench00-n-div-q TIMES
    MOVE 1 TO i-s
    PERFORM 32767 TIMES
      ADD i-s TO x-s
      ADD 1 TO i-s
    END-PERFORM
    MOVE -32768 TO i-s
    PERFORM 32768 TIMES
      ADD i-s TO x-s
      ADD 1 TO i-s
    END-PERFORM
*> DISPLAY "DEBUG: 1. x-s " x-s
  END-PERFORM

  MOVE 1 TO i-s
  PERFORM bench00-n-mod-q TIMES
    ADD i-s TO x-s
    ADD 1 TO i-s
  END-PERFORM
  COMPUTE x-help = x-s / 65536
  COMPUTE x = x-s - (x-help * 65536)
  .


bench01.
  MOVE 0 TO x
  MOVE 0 TO sum1

  MOVE 1 TO i
  PERFORM n TIMES
    ADD i TO sum1
    IF sum1 >= n
      SUBTRACT n FROM sum1
      ADD 1 TO x
    END-IF
    ADD 1 TO i
  END-PERFORM
  .


*>
*>
*>
bench02.
  MOVE 0 TO x
  MOVE 0 TO sum1-d
  
  MOVE 1 TO i-d
  PERFORM n TIMES
    ADD i-d TO sum1-d
    IF sum1-d >= n
      SUBTRACT n FROM sum1-d
      ADD 1 TO x
    END-IF
    ADD 1 TO i-d
  END-PERFORM
  .


bench03.
  COMPUTE bench03-nHalf = n / 2
  MOVE 0 TO bench03-x
*> we don't have index 0, so don't save number 0.
  MOVE 0 TO bench03-sieve1(1)

*> Initialize sieve
  MOVE 0 TO i
  COMPUTE bench03-n2 = bench03-nHalf + 1
  PERFORM bench03-n2 TIMES
    MOVE 0 TO bench03-sieve1(i + 1)
    ADD 1 TO i
  END-PERFORM

*> Compute primes
  MOVE 0 TO i
  MOVE 3 TO bench03-m
  MOVE 1 to x
  PERFORM UNTIL (bench03-m * bench03-m) > n
    IF bench03-sieve1(i + 1) = 0
      COMPUTE x = x + 1
      COMPUTE j = (bench03-m * bench03-m - 3) / 2
      PERFORM UNTIL j >= bench03-nHalf
        MOVE 1 TO bench03-sieve1(j + 1)
        COMPUTE j = j + bench03-m
      END-PERFORM
    END-IF
    COMPUTE i = i + 1
    COMPUTE bench03-m = bench03-m + 2
  END-PERFORM

*> Count remaining primes
  PERFORM UNTIL bench03-m > n
    IF bench03-sieve1(i + 1) = 0
      ADD 1 TO x
    END-IF
    ADD 1 TO i
    ADD 2 TO bench03-m
  END-PERFORM
  .


*>
*>
*> we need 10 digits for x, check1
bench04.
  MOVE 1 TO x
  PERFORM n TIMES
    COMPUTE bench04-x-div-q = x / bench04-q
    COMPUTE bench04-x-mod-q = x - bench04-q * bench04-x-div-q
    COMPUTE x = bench04-a * bench04-x-mod-q - bench04-r * bench04-x-div-q
    IF x <= 0
      ADD bench04-m to x
    END-IF
  END-PERFORM
  .


*>
*>
*>
bench05.
  MOVE n TO bench05-save-n
  COMPUTE n = n / 2
  COMPUTE bench05-k = n / 2
  IF ((n - bench05-k) < bench05-k)
    COMPUTE bench05-k = n - bench05-k
  END-IF

  IF bench05-k > 1250
    DISPLAY "WARNING: k > 1250!"
  END-IF

*> initialize (not needed)
  MOVE 0 TO i
  COMPUTE bench05-n2 = bench05-k + 1
  PERFORM bench05-n2 TIMES
    MOVE 0 TO bench05-pas1(i + 1)
    ADD 1 TO i
  END-PERFORM

  MOVE 1 TO bench05-pas1(0 + 1)
  MOVE 2 TO bench05-pas1(1 + 1)

*> compute lines of Pascal's triangle
  MOVE 3 TO i
  COMPUTE bench05-n2 = n - 2
  PERFORM bench05-n2 TIMES
    COMPUTE bench05-min1 = (i - 1) / 2
    COMPUTE x-help = i / 2
    COMPUTE bench05-i-mod-2 = i - (x-help * 2)
*>    COMPUTE x-help = (i + 1) / 2
*>    COMPUTE bench05-i-mod-2-1 = (i + 1) - (x-help * 2)
*>    MOVE i TO bench05-pas1(bench05-i-mod-2 + 1)

    IF bench05-i-mod-2 = 0
      COMPUTE bench05-pas1(bench05-min1 + 1 + 1) =
        2 * bench05-pas1(bench05-min1 + 1)
    END-IF
    
    MOVE bench05-pas1(1 + 1) TO bench05-prev
    MOVE 2 TO j
    COMPUTE bench05-n2 = bench05-min1 - 1
    PERFORM bench05-n2 TIMES
      MOVE bench05-pas1(j + 1) TO bench05-num
      COMPUTE bench05-pas1(j + 1) = bench05-pas1(j + 1) + bench05-prev
      MOVE bench05-num TO bench05-prev
      ADD 1 TO j
    END-PERFORM
    MOVE i TO bench05-pas1(1 + 1)
    ADD 1 TO i
  END-PERFORM
  
*> compute sum of ((n/2)Ck)^2 mod 65536 for k=0..n/2  
  MOVE 0 TO x
  MOVE 0 TO j
  COMPUTE bench05-n2 = bench05-k
  PERFORM bench05-n2 TIMES
    COMPUTE x = x + 2 * bench05-pas1(j + 1) * bench05-pas1(j + 1)
    ADD 1 TO j
  END-PERFORM
  COMPUTE x = x + bench05-pas1(bench05-k + 1) * bench05-pas1(bench05-k + 1)
*>  COMPUTE x-help = n / 2
*>  COMPUTE bench05-i-mod-2 = n - (x-help * 2)
*>  COMPUTE x = x + bench05-pas1(bench05-i-mod-2 + 1)
  COMPUTE x-help = x / 65536
  COMPUTE x = x - (x-help * 65536)
  MOVE bench05-save-n TO n
  .

*>
*>
*>
bench06.
  MOVE 0.0 TO sum1-d
  MOVE -1.0 TO bench06-flip1-d
  
  MOVE 1 TO i-d
*>  DISPLAY "DDD: " i-d
  PERFORM n TIMES
    COMPUTE bench06-flip1-d = bench06-flip1-d * (-1.0)
    COMPUTE sum1-d = sum1-d + bench06-flip1-d / (2 * i-d - 1)
    ADD 1 TO i-d
  END-PERFORM
  COMPUTE x = ((sum1-d * 4.0) * 100000000)
  .

*>
*> run a benchmark
*> in: bench = benchmark to use
*> loops = number of loops
*>  n = maximum number (used in some benchmarks to define size
*> of workload)
*>  check1 = check value
*> out: x = result
*>
run-bench.
  MOVE 0 TO x
  MOVE loops TO loops-save
  PERFORM UNTIL loops <= 0 OR x <> 0
    EVALUATE bench
    WHEN 0
      PERFORM bench00
    WHEN 1
      PERFORM bench01
    WHEN 2
      PERFORM bench02
    WHEN 3
      PERFORM bench03
    WHEN 4
      PERFORM bench04
    WHEN 5
      PERFORM bench05
    WHEN 6
      PERFORM bench06
    WHEN OTHER
      DISPLAY "Error: Unknown benchmark: " bench
      MOVE -1 TO x
    END-EVALUATE
    SUBTRACT check1 FROM x
    SUBTRACT 1 FROM loops
  END-PERFORM
  ADD check1 TO x
  IF x <> check1
    DISPLAY "Error(bench" bench "): x=" x
    MOVE -1 TO x
  END-IF
  MOVE loops-save TO loops
  .


bench03Check.
*>  display "n " n "," x
  IF n = 500000
    MOVE 41538 to x
  ELSE
    MOVE 1 to x
    PERFORM WITH TEST BEFORE VARYING j FROM 3 BY 2 UNTIL j > n
      MOVE 1 TO bench03Check-isPrime
      MOVE 3 TO i
      PERFORM UNTIL i * i > j OR bench03Check-isPrime = 0
*>        DISPLAY "DDD: j=" j ", i=" i
        COMPUTE x-help = j / i
        COMPUTE x-help = j - (x-help * i)
        IF x-help = 0
          MOVE 0 TO bench03Check-isPrime
        END-IF
        ADD 2 TO i
      END-PERFORM
      IF bench03Check-isPrime = 1
        ADD 1 TO x
      END-IF
    END-PERFORM
  END-IF
  MOVE x TO check1
  .


*>
*> get check
*> in: bench = benchmark to use
*>  n = maximum number
*>  check = check value
*> out: check1 = check value
*>
getCheck.
  MOVE 0 TO check1
  EVALUATE bench
   WHEN 0
    COMPUTE check1 = (n / 2) * (n + 1)
    COMPUTE x-help = check1 / 65536
    COMPUTE check1 = check1 - (x-help * 65536)
   WHEN 1
    COMPUTE check1 = (n + 1) / 2
   WHEN 2
    COMPUTE check1 = (n + 1) / 2
   WHEN 3
    PERFORM bench03Check
   WHEN 4
    IF n = 1000000
      MOVE 1227283347 TO check1
    ELSE
      PERFORM bench04
      MOVE x TO check1
    END-IF
   WHEN 5
    IF n = 5000
      MOVE 17376 TO check1
    ELSE
      PERFORM bench05
      MOVE x TO check1
    END-IF
   WHEN 6
    IF n = 1000000
      MOVE 314159165 TO check1
    ELSE
      PERFORM bench06
      MOVE x TO check1
    END-IF
   WHEN OTHER
     DISPLAY "Error: Unknown benchmark: " bench
     MOVE -1 TO check1
  END-EVALUATE
  .


*>
*>
*>throughput
get-raw-ts.
  ACCEPT w01-time FROM TIME
  COMPUTE t1 = (((((w05-hour * 60) + w05-min) * 60) + w05-sec)
    * 1000) + (w05-hsec * 10)
*> DISPLAY "DEBUG: Redefined time: " w05-hour ":" w05-min ":"
*> w05-sec "." w05-hsec.
*> DISPLAY "DEBUG: The system time is: " t1.
  .

get-ts.
  PERFORM get-raw-ts
  COMPUTE t1 = t1 - start-t
  .

*> convMs.

getPrecMs.
  MOVE 0 TO g-tsMeasCnt
  PERFORM get-ts
  MOVE t1 TO t0
  PERFORM UNTIL t1 > t0
    PERFORM get-ts
    ADD 1 TO g-tsMeasCnt
  END-PERFORM
  MOVE t1 TO t1-d
  .


correctTime.
*> move g-tsPrecCnt  TO g-tsMeasCnt
  IF g-tsMeasCnt < g-tsPrecCnt
    MOVE t0 TO tMeas0
    COMPUTE tMeas0 = tMeas0 + g-tsPrecMs * ((g-tsPrecCnt - g-tsMeasCnt) / g-tsPrecCnt)
*>    DISPLAY tMeas0 " " t1-d " -- " g-tsMeasCnt
    IF tMeas0 < t1-d
      MOVE tMeas0 TO t1-d
    END-IF
  END-IF
*>  DISPLAY t0 ": " t1 " => " t1-d " (" g-tsMeasCnt ")"
  .


determineTsPrecision.
  PERFORM get-raw-ts
  MOVE t1 TO start-t

  PERFORM getPrecMs
  MOVE t1-d TO tMeas0
  PERFORM getPrecMs
  MOVE t1-d TO tMeas1
  COMPUTE g-tsPrecMs = tMeas1 - tMeas0
  MOVE g-tsMeasCnt TO g-tsPrecCnt

*> do it again
  MOVE tMeas1 TO tMeas0
  PERFORM getPrecMs
  MOVE t1-d TO tMeas1
  IF g-tsMeasCnt > g-tsPrecCnt
    MOVE g-tsMeasCnt TO g-tsPrecCnt
    COMPUTE g-tsPrecMs = tMeas1 - tMeas0
  END-IF
  .


*>
*>
*> Here we compute the number of "significant" bits for
*> positive numbers (which means 53 for double)
checkbits-short1.
  MOVE 1 TO num-s
  MOVE 0 TO last-num-s
  MOVE 0 TO bits
  PERFORM UNTIL (((num-s - 1) / 2) <> last-num-s) OR (bits >= 101)
    MOVE num-s to last-num-s
    COMPUTE num-s = num-s * 2
    COMPUTE num-s = num-s + 1
    COMPUTE bits = bits + 1
  END-PERFORM
  .

checkbits-int1.
  MOVE 1 TO num
  MOVE 0 TO last-num
  MOVE 0 TO bits
  PERFORM UNTIL (((num - 1) / 2) <> last-num) OR (bits >= 101)
    MOVE num to last-num
    COMPUTE num = num * 2
    COMPUTE num = num + 1
    COMPUTE bits = bits + 1
  END-PERFORM
  .

checkbits-float1.
  MOVE 1 TO num-f
  MOVE 0 TO last-num-f
  MOVE 0 TO bits
  PERFORM UNTIL (((num-f - 1) / 2) <> last-num-f) OR (bits >= 101)
    MOVE num-f to last-num-f
    COMPUTE num-f = num-f * 2
    COMPUTE num-f = num-f + 1
    COMPUTE bits = bits + 1
  END-PERFORM
  .

checkbits-double1.
  MOVE 1 TO num-d
  MOVE 0 TO last-num-d
  MOVE 0 TO bits
  PERFORM UNTIL (((num-d - 1) / 2) <> last-num-d) OR (bits >= 101)
    MOVE num-d to last-num-d
    COMPUTE num-d = num-d * 2
    COMPUTE num-d = num-d + 1
    COMPUTE bits = bits + 1
  END-PERFORM
  .


get-info.
  DISPLAY "BM Bench v" prg-version " (" prg-language ") -- (" WITH NO ADVANCING
  PERFORM checkbits-short1
  DISPLAY "short:" bits WITH NO ADVANCING
  PERFORM checkbits-int1
  DISPLAY " int:" bits WITH NO ADVANCING
  PERFORM checkbits-float1
  DISPLAY " float:" bits WITH NO ADVANCING
  PERFORM checkbits-double1
  DISPLAY " double:" bits WITH NO ADVANCING
  DISPLAY " tsMs:" g-tsPrecMs " tsCnt:" g-tsPrecCnt ") --"
  DISPLAY "(c) Marco Vieth, 2002-2023"

  ACCEPT w01-date FROM DATE
  ACCEPT w01-time FROM TIME
*> MOVE FUNCTION CURRENT-DATE to w01-datex.
  DISPLAY "Date: " w01-date " " w01-time
  .


print-results.
  DISPLAY "Throughput for all benchmarks (loops per sec):"
  DISPLAY "BMR (" prg-language ") : " WITH NO ADVANCING
  MOVE bench1 to bench
  PERFORM UNTIL bench > bench2
    MOVE bench-res1(bench + 1) TO fmt09
    DISPLAY fmt09 " " WITH NO ADVANCING
    ADD 1 TO bench
  END-PERFORM
  DISPLAY " "
  .

*>
*>
*>
measureBench.
*>  MOVE 100 TO delta-ms
*>  MOVE 10000 TO max-ms

  MOVE 1 TO loops
  MOVE 0 TO tEsti
  MOVE 0 to throughput

  DISPLAY "Calibrating benchmark " bench " with loops=" loops ", n=" n ", check=" check1
  PERFORM UNTIL throughput <> 0
*>    DISPLAY "DDD0: l=" loops
    PERFORM getPrecMs
    MOVE t1-d TO measureBench-tMeas0
    PERFORM run-bench
    PERFORM getPrecMs
    PERFORM correctTime
    COMPUTE tMeas = t1-d - measureBench-tMeas0
  
    IF tEsti > tMeas
      COMPUTE t-delta = tEsti - tMeas
    ELSE 
      COMPUTE t-delta = tMeas - tEsti
    END-IF

    IF tMeas > 0
      COMPUTE loops-p-sec = (loops * 1000) / tMeas
    ELSE
      MOVE 0 TO loops-p-sec
    END-IF
    
    MOVE loops-p-sec TO fmt10
    MOVE tMeas TO fmt09
    MOVE t-delta TO fmt09_2
    DISPLAY fmt10 "/s (time=" fmt09 " ms, loops=" loops ", delta=" fmt09_2 " ms)"

    IF x = -1
      MOVE -1 TO throughput
    ELSE
      IF tEsti > 0 AND t-delta < delta-ms
        MOVE loops-p-sec TO throughput

        MOVE loops-p-sec TO fmt10
        MOVE tMeas TO fmt09
        MOVE t-delta TO fmt09_2
        DISPLAY "Benchmark " bench " (" prg-language "): " fmt10 "/s (time=" fmt09 " ms, loops=" loops ", delta=" fmt09_2 " ms)"
      ELSE
        IF tMeas > max-ms
          DISPLAY "Benchmark " bench " (" prg-language "): Time already > " max-ms " ms. No measurement possible."
          IF loops-p-sec > 0
            COMPUTE throughput = -loops-p-sec
          ELSE
            MOVE -1 TO throughput
          END-IF
        ELSE
          IF tMeas = 0
            MOVE 50 TO scale-fact
          ELSE
            IF tMeas < cali-ms
              COMPUTE scale-fact = (cali-ms + 100) / tMeas
              ADD 1 TO scale-fact
*>              COMPUTE scale-fact = (cali-ms + 100)
*>              COMPUTE scale-fact = scale-fact / tMeas
            ELSE
              MOVE 2 TO scale-fact
            END-IF
          END-IF
*>          DISPLAY "DDD1: l=" loops ", te=" tEsti
          COMPUTE loops = loops * scale-fact
          COMPUTE tEsti = tMeas * scale-fact
*>          DISPLAY "DDD2: sf=" scale-fact ", l=" loops ", te=" tEsti
        END-IF
      END-IF
    END-IF
  END-PERFORM
  .


*>
*>
start-bench.
  PERFORM determineTsPrecision
  PERFORM get-info

  MOVE n TO start-bench-n
  MOVE bench1 to bench

  PERFORM UNTIL bench > bench2
    MOVE start-bench-n TO n
    IF bench = 3
      COMPUTE n = n / 2
    ELSE
      IF bench = 5
        COMPUTE n = n / 200
      END-IF
    END-IF

    PERFORM getCheck
    IF check1 > 0
      PERFORM measureBench
    ELSE
      MOVE -1 TO throughput
    END-IF
    COMPUTE bench-res1(bench + 1) = throughput
    
    ADD 1 TO bench
  END-PERFORM
  PERFORM print-results
  .


*>
*>
*>
main-form.
  IF args-str <> SPACE AND args-str(1:1) >= "0" AND args-str(1:1) <= "6"
     UNSTRING args-str DELIMITED BY ' ' INTO args1 args2 args3 args4
  ELSE
    UNSTRING args-str DELIMITED BY ' ' INTO args0 args1 args2 args3 args4
  END-IF

*> DISPLAY "DEBUG: args1=" args1.
  IF args1 <> SPACE
    MOVE args1 TO bench1
*>   COMPUTE bench1 = FUNCTION NUMVAL(args1)
  END-IF

  IF args2 <> SPACE
    MOVE args2 TO bench2
   END-IF

  IF args3 <> SPACE
    MOVE args3 TO n
  END-IF

  IF args4 <> SPACE
    MOVE args4 TO cali-ms
  END-IF

  PERFORM start-bench
  
  PERFORM get-ts
  DISPLAY "Total elapsed time: " t1 " ms"
  .

END PROGRAM bmbench.
