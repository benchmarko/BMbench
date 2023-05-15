#Smalltalk ==                     "start of -*- Smalltalk -*- comment
exec gst "$0" -aQ $@              # end  of     Smalltalk     comment"!

"======================================================================
| BM Bench - bmbench.st (Smalltalk)
| (c) Marco Vieth, 2002
| http://www.benchmarko.de
|
| 25.05.2002  0.01
| 05.02.2003  0.05
| 10.05.2023  0.08  adapted for new version
|
|
| Usage:
| gst bmbench.st -a [bench1] [bench2] [n]
|
| Information
| - info gst
| - man gst
| - /usr/share/smalltalk/examples/  (e.g. Bench.st)
|
| Notes:
| - Comments are enclosed in double quotes (spanning many lines)
| - Identifiers are case-sensitive and consist of letters and digits
| - Local variables are enclosed in | ... |
| - Statements are seperated by periods '.'
| - Assignment: ':=' or '_'
| - Messages (to the same object) are separated by semicolons ';'
| - Binary messages (e.g. +. *) are always parsed left to right!
| - Text output: 'stdout print: <object>; nextPutAll: '<string>'; nl.' is buffered output.
|
| - Print comment of (information about) class Integer:  '(Integer comment) printNl !'
| - Inspecting objects: 'inspect' message: 'Integer inspect !'
| - Transcript using Float number prodces divide by zero error (?)
|
| - Defining method 'new' for class <classname>:
|   !<classname> class methodsFor: 'instance creation'!
|   new
|     | r |  ""local varible r""
|     r := super new. ""send message new to superclass(=Object) and assign newly created object to r""
|     r init. ""send init message to object r""
|     ^r  ""return r""
|   ! !
|
| - Defining method 'init' for objects created from class <classname>:
|   !<classname> methodsFor: 'instance initialization'!
|    init
|      balance := 0 ""set variable 'balance'""
|    ! !
|
| - SmallInteger: 30 bit signed integer...
|
| Performance:
| - ShortInteger is a factor of 100 faster than LongInteger! (gst 1.95.9) (1778 cmp. 187490) (2137 compared to 225210)
| - Counting up '1 to: n do:' seems to be a bit faster than counting down: 'n to: 1 by: -1 do:'
|
|
|  - bitAnd: aNumber
|  - anArray inject: 0 into: [:sum :item | sum + item ].
|  - There was a segmentation fault when running bench03 (with normal Array), bench04...
|
|  - https://www.tutorialspoint.com/execute_smalltalk_online.php
|    Arguments: -a 6 6
|
======================================================================"


"
| Time class: clocks
| millisecondClock -      Answer the number of milliseconds since startup.

  [t1 := Time millisecondsToRun: [n1 run_bench].
    t1 < 5000] whileTrue:[ n1 := n1 * 2 ].

  ^((n1 * 500000 * 1000) // t1) printString, ' bytecodes/sec; ' !

   ifAbsent: [ ^self error: 'No such check #' ].
"


"Benchmark"


Smalltalk at: #gState_cali_ms put: 1001.
Smalltalk at: #gState_delta_ms put: 100.

Smalltalk at: #gState_startTs put: 0.
Smalltalk at: #gState_tsMeasCnt put: 0.
Smalltalk at: #gState_tsLastMs put: 0.
Smalltalk at: #gState_tsPrecMs put: 0.
Smalltalk at: #gState_tsPrecCnt put: 0.
Smalltalk at: #gState_bench03Sieve1 put: nil.
Smalltalk at: #gState_bench05Line1 put: nil.

!Integer methodsFor: 'bmbench_xx'!

"bench00(num_t n)"
"same as bench01"
bench00
  | n x |

  n := self.
  x := 0.
  1 to: n do: [:i | x := x + i bitAnd: 65535 ].

  ^x bitAnd: 65535. "or: ^x rem: 65536."
!


"bench01(num_t n)"
"ShortInteger is MUCH faster than LongInteger but we use LongInteger here to show the difference..."
bench01
  | n x sum1 |

  n := self.
  x := 0.
  sum1 := 0.
  "Transcript showCr: 'DEBUG:', (x inspect) printString."
  "(sum1..1000000 depends on type: 500000500000 (floating point), 1784293664 (32bit), 10528 (16 bit)"
  "Transcript showCr: 'DEBUG: bench01: loops=', loops printString, ', n=', n printString, ', sum1=', sum1 printString."

  1 to: n do: [:i |
    sum1 := sum1 + i.
    (sum1 >= n) ifTrue: [
        sum1 := sum1 - n.
        x := x + 1.
      ].
  ].
    "Transcript showCr: 'DEBUG: bench01: loop=', loop printString, ', x=', x printString."
  ^x.
!


"bench02(num_t n)"
bench02
  | n x sum1 |

  n := self.
  x := 0.
  sum1 := 0.0.
  "Transcript showCr: 'DEBUG:', (x inspect) printString."
  "(sum1..1000000 depends on type: 500000500000 (floating point), 1784293664 (32bit), 10528 (16 bit)"
  "Transcript showCr: 'DEBUG: bench01: loops=', loops printString, ', n=', n printString, ', sum1=', sum1 printString."

  1 to: n do: [:i |
    sum1 := sum1 + i.
    (sum1 >= n) ifTrue: [
        sum1 := sum1 - n.
        x := x + 1.
      ].
  ].
    "Transcript showCr: 'DEBUG: bench01: loop=', loop printString, ', x=', x printString."
  ^x.
!

"bench03(num_t n)"
"number of primes below n (Sieve of Eratosthenes)"
"Example: n=500000 => x=41538 (expected), n=1000000 => x=78498"
bench03
  | n nH sieve1 i m x j |
  n := self.
  nH := n // 2. "compute only up to n/2"
  
  (gState_bench03Sieve1 = nil) ifTrue: [ gState_bench03Sieve1 := ByteArray new: (nH + 1)]. "arrays starting from 1; we try ByteArray (alternative: Array withtrue, false)..."
  sieve1 := gState_bench03Sieve1.
   
    "initialize sieve"
    1 to: (nH + 1) do: [:j | sieve1 at: j put: 0 ].

    "compute primes"
    i := 0.
    m := 3.
    x := 1. "number of primes below n (2 is prime)"
    [(m * m) <= n] whileTrue: [
      ((sieve1 at: (i + 1)) = 0) ifTrue: [
        x := x + 1. "m is prime"
        "j := (m * m) quo: 2."
        ((m * m - 3) quo: 2) to: (nH - 1) by: m do: [:j | sieve1 at: (j + 1) put: 1 ].
      ].
      i := i + 1.
      m := m + 2.
    ].

    "count remaining primes"
    m to: n by: 2 do: [:j | 
      ((sieve1 at: (i + 1)) = 0) ifTrue: [ x := x + 1 ].
      i := i + 1.
    ].
    "1 to: n do: [:i | ((sieve1 at: i) > 0) ifTrue: [ x := x + 1 ] ]."
  ^x.
!


"bench04(num_t n)"
" nth random number number
| Random number generator taken from
| Raj Jain: The Art of Computer Systems Performance Analysis, John Wiley & Sons, 1991, page 442-444.
| It needs longs with at least 32 bit.
| Starting with x0=1, x10000 should be 1043618065, x1000000 = 1227283347.
| Note: We have just 30 Bit SmallInteger and LongInteger is very slow, so use Float...
"
bench04
  | n m a q r x xDivq xModq |
  n := self.
  m := 2147483647. "modulus, do not change!" "hey, this is Longinteger because we have only 30 bits!"
  a := 16807. "multiplier"
  q := 127773. "m div a"
  r := 2836. "m mod a"
  x := 1. "last random value"
  "Transcript showCr: 'DEBUG bench04: x=', x printString."
  n timesRepeat: [
    xDivq := x quo: q.
    "Transcript showCr: 'DEBUG: xDivq=', xDivq printString."
    xModq := x - (q * xDivq).
    x := (a * xModq) - (r * xDivq).
    "Transcript showCr: 'DEBUG: m=', (m inspect) printString."
    (x <= 0) ifTrue: [ x := x + m ].  "x is new random number"
    "Transcript showCr: 'DEBUG: x=', (x inspect) printString."
    "(tmp1 > 24501) ifTrue: [ Transcript showCr: 'DEBUG: tmp1=', tmp1 printString.]."
  ].
  ^(x asInteger).
!


"bench05(num_t n)"
bench05
  | nP x n k line1 min1 prev1 num1 |
  nP := self.

  n := nP quo: 2.
  k := n bitShift: -1. "div 2"
  "Transcript showCr: 'DEBUG: n=', (n inspect) printString, ', k=', (k inspect) printString."
  "Transcript showCr: 'DEBUG: n=', n printString, ', k=', k printString."

  ((n - k) < k) ifTrue: [
    k := n - k. "keep k minimal with  n over k  =  n over n-k"
  ].

  "allocate memory..."
  (gState_bench05Line1 = nil) ifTrue: [ gState_bench05Line1 := Array new: (k + 1)]. "arrays starting from 1"
  line1 := gState_bench05Line1.
  
  "initialize sieve (not needed)"
  1 to: (k + 1) do: [:j | line1 at: j put: 0 ].
  
  line1 at: 0 + 1 put: 1.
  (k >= 1) ifTrue: [ line1 at: 1 + 1 put: 2 ].

    3 to: n do: [:i |
      min1 := (i - 1) bitShift: -1. "min1 := (i - 1) quo: 2."
      "Transcript showCr: 'DEBUG: in=', i printString, ', min1=', min1 printString."
      ((i bitAnd: 1) = 0) ifTrue: [ "new element"
        line1 at: (min1 + 1 + 1) put: ((line1 at: (min1 + 1)) * 2).
        "Transcript showCr: 'DEBUG: l1x=', line1 at: (min1 + 1 + 1) printString."
      ].

      prev1 := line1 at: (1 + 1).
      2 to: min1 do: [:j |  "up to min((i-1)/2, k)"
        num1 := line1 at: (j + 1).
        line1 at: (j + 1) put: ((num1 + prev1) bitAnd: 65535). "avoid overflow"
        prev1 := num1.
        "Transcript showCr: 'DEBUG: l2x=', line1 at: (j + 1) printString."
      ].
      line1 at: (1 + 1) put: i. "second column is i"
      "Transcript showCr: 'DEBUG: l3x=', line1 at: (1 + 1) printString."
    ].
    
    "compute sum of ((n/2)Ck)^2 mod 65536 for k=0..n/2"
    x := 0.
    1 to: k do: [:j |
      x := (x + ((line1 at: j) * (line1 at: j) * 2)) bitAnd: 65535.
      "Transcript showCr: 'DEBUG: x=', x printString."
    ].
    x := (x + ((line1 at: (k + 1)) * (line1 at: (k + 1)))) bitAnd: 65535.
    "Transcript showCr: 'DEBUG: k=', k printString, ', x2=', x printString."
    "x := x + ((line1 at: ((n bitAnd: 1) + 1)) at: (k+1)) bitAnd: 65535."

  ^x bitAnd: 65535.
!


"bench06(int n)"
bench06
  | n sum1 flip1 |
  n := self.
  sum1 := 0.0.
  flip1 := -1.0.
  1 to: n do: [:i |
    flip1 := flip1 * -1.0.
    sum1 := sum1 + (flip1 / (2 * i - 1)).
  ].
  ^((sum1 * 4.0) * 100000000.0) truncated.
!


"run_bench(int bench, int loops, num_t n, int check1)"
runBench: loops num1: n check1: check1
  | bench x |
  bench := self.
  "Transcript showCr: 'DEBUG: runBench: bench=', bench printString, ', loops=', loops printString, ', n=', n printString."

  "Smalltalk compact."  "TEST..."
  x := 0.
  
  loops timesRepeat: [
    (bench = 0) ifTrue: [ x := n bench00. ].
    (bench = 1) ifTrue: [ x := n bench01. ].
    (bench = 2) ifTrue: [ x := n bench02. ].
    (bench = 3) ifTrue: [ x := n bench03. ].
    (bench = 4) ifTrue: [ x := n bench04. ].
    (bench = 5) ifTrue: [ x := n bench05. ].
    (bench = 6) ifTrue: [ x := n bench06. ].
    x := x - check1.
  ].
  x := x + check1.
  (check1 = x) ifFalse: [
     Transcript showCr: 'Error(bench', bench printString, '): x=', x printString.
     x := -1.
  ].

  ^x
!


"bench03Check(int n)"
bench03Check
| n x isPrime i |
  n := self.
  x := 1. "2 is prime"
  3 to: n by: 2 do: [:j |
    isPrime := true.
    i := 3.
    [(i * i) <= j and: [ isPrime = true]] whileTrue: [
        ((j rem: i) = 0) ifTrue: [
            isPrime := false.
        ].
        i := i + 2.
    ].
    (isPrime = true) ifTrue: [
        x := x + 1.
    ].
  ].
  ^x
!


"getCheck(int bench, num_t n)"
getCheck: n
  | bench check1 |
  bench := self.
  "Transcript showCr: 'DEBUG: getCheck: bench=', bench printString, ', n=', n printString."

  "Smalltalk compact."  "TEST..."
  check1 := 0.
  (bench = 0) ifTrue: [
      (n = 1000000) ifTrue: [ check1 := 10528 ] ifFalse: [ check1 := ((n / 2) * (n + 1)) bitAnd: 65535 ]
  ].
  (bench = 1) ifTrue: [ check1 := (n + 1) quo: 2 ].
  (bench = 2) ifTrue: [ check1 := (n + 1) quo: 2 ].
  (bench = 3) ifTrue: [
      (n = -500000) ifTrue: [ check1 := 41538 ] ifFalse: [ check1 := n bench03Check ]
  ].
  (bench = 4) ifTrue: [
      (n = 1000000) ifTrue: [ check1 := 1227283347 ] ifFalse: [ check1 := n bench04 ]
  ].
  (bench = 5) ifTrue: [ 
      (n = 5000) ifTrue: [ check1 := 17376 ] ifFalse: [ check1 := n bench05 ]
  ].
  (bench = 6) ifTrue: [
      (n = 1000000) ifTrue: [ check1 := 314159165 ] ifFalse: [ check1 := n bench06 ]
   ].

  (check1 = 0) ifTrue: [
     Transcript showCr: 'Error: Unknown benchmark ', bench printString.
     Transcript cr.
     check1 := -1.
  ].

  ^check1.
!

getTs
  ^Time millisecondClock - gState_startTs.
!

"correctTime(double tMeas, double tMeas2, int measCount)"
correctTime: tMeas2
  | tMeas |
  tMeas := self.

  "Transcript showCr: 'DEBUG: tMeas=', tMeas printString, ', tMeas2=', tMeas2 printString."
  (gState_tsMeasCnt < gState_tsPrecCnt) ifTrue: [
    "Transcript showCr: 'DEBUG: tMeas=', tMeas printString, ', tMeasCorr=', ((((gState_tsPrecMs * (gState_tsPrecCnt - gState_tsMeasCnt)) / gState_tsPrecCnt) + tMeas) * 1000.0) truncated printString."
    tMeas := tMeas + ((gState_tsPrecMs * (gState_tsPrecCnt - gState_tsMeasCnt)) // gState_tsPrecCnt).
    "Cannot set a fraction with '/' here because we cannot print float"
    "Transcript showCr: 'DEBUG: tMeas=', tMeas printString."
    "Transcript showCr: 'DEBUG: tMeas=', (tMeas * 1000.0) truncated printString."
    (tMeas > tMeas2) ifTrue: [
      tMeas := tMeas2. "cannot correct"
    ].
  ].
  ^tMeas.
!


getPrecMs
  | tMeas0 tMeas measCount |
  
  measCount := 0.
  tMeas0 := 0 getTs.
  tMeas := tMeas0.
  [tMeas <= tMeas0] whileTrue: [
    tMeas := 0 getTs.
    measCount := measCount + 1.
  ].
  "Transcript showCr: 'DEBUG: tMeas=', tMeas printString."
  gState_tsMeasCnt := measCount.
  gState_tsLastMs := tMeas0.
  "Transcript showCr: 'DEBUG: getPrecMs: tMeas0=', tMeas0 printString, ', tMeas=', tMeas printString."
  ^tMeas.
!


"
getMs
  ^Time millisecondClock
!
"

determineTsPrecision
  | tMeas0 tMeas1 |
  gState_startTs := 0.
  gState_startTs := 0 getTs.
  
  tMeas0 := 0 getPrecMs.
  tMeas1 := 0 getPrecMs.
  gState_tsPrecMs := tMeas1 - tMeas0.
  gState_tsPrecCnt := gState_tsMeasCnt.

  "do it again"
  tMeas0 := tMeas1.
  tMeas1 := 0 getPrecMs.
  (gState_tsMeasCnt > gState_tsPrecCnt) ifTrue: [ "take maximum count"
      gState_tsPrecCnt := gState_tsMeasCnt.
      gState_tsPrecMs := tMeas1 - tMeas0.
  ].
  ^0.
!


"Here we compute the number of 'significant' bits for positive numbers (which means 53 for double)"
"A SmallInteger is automatically converted to LongInteger if it gets too long..."
checkbitsSmall1
  | num lastNum bits |
  num := 1.
  lastNum := 0.
  bits := 0.
  [
    lastNum := num.
    num := num * 2.
    num := num + 1.
    bits := bits + 1.
    "Transcript showCr: 'DEBUG: checkbitsInt1: num=', num printString, ', is..=', (num isSmallInteger) printString."
    "num inspect."
    "isSmallInteger seems not work correctly, so we check size with validSize..."
    (num validSize = 0) and: [bits < 101].
  ] whileTrue.
  ^bits.
!


checkbitsInt1
  | num lastNum bits |
  num := 1.
  lastNum := 0.
  bits := 0.
  [
    lastNum := num.
    num := num * 2.
    num := num + 1.
    bits := bits + 1.
    (((num - 1) / 2) = lastNum) and: [bits < 101].
  ] whileTrue.
  ^bits.
!



"Here we compute the number of 'significant' bits for positive numbers (which means 53 for double)"
checkbitsDouble1
  | num lastNum bits |
  num := 1.0.
  lastNum := 0.0.
  bits := 0.
  [
    lastNum := num.
    num := num * 2.0.
    num := num + 1.0.
    bits := bits + 1.
    (((num - 1.0) / 2.0) = lastNum) and: [bits < 101].
  ] whileTrue.
  ^bits.
!

"print_info()"
print_info
  |  |
  Transcript showCr: 'BM Bench v0.8 (Smalltalk) -- (short:', (0 checkbitsSmall1) printString,
    ' int:', (0 checkbitsInt1) printString,
    ' double:', (0 checkbitsDouble1) printString,
    ' tsMs:', gState_tsPrecMs printString,
    ' tsCnt:', gState_tsPrecCnt printString,
    ') ', (Smalltalk version);
    showCr: '(c) Marco Vieth, 2002-2023';
    showCr: (DateTime now) printString.
  ^1.
!


"measureBench(int bench, int n, int check)"
measureBench: n check1: check1
  | bench delta_ms max_ms cali_ms loops tMeas0 tMeas tEsti throughput x t_delta loops_p_sec scale_fact prg_language |
  bench := self.
  cali_ms := gState_cali_ms.
  delta_ms := gState_delta_ms.
  max_ms := 10000.
  prg_language := Smalltalk.
  
  loops := 1.
  tMeas := 0.
  tEsti := 0.
  throughput := 0.
  
  Transcript showCr: 'Calibrating benchmark ', bench printString, ' with loops=', loops printString, ', n=', n printString, ', check=', check1 printString.

  [throughput = 0] whileTrue: [
      tMeas0 := 0 getPrecMs.
      x := bench runBench: loops num1: n check1: check1.
      tMeas := 0 getPrecMs.
      tMeas := gState_tsLastMs correctTime: tMeas.
      tMeas := tMeas - tMeas0.

      t_delta := (tEsti > tMeas) ifTrue: [tEsti - tMeas] ifFalse: [tMeas - tEsti].
      "Transcript showCr: 'DEBUG: measureBench: tMeas=', tMeas printString, ', t_delta=', t_delta printString; cr."
      loops_p_sec := (tMeas > 0) ifTrue: [loops * 1000 quo: tMeas] ifFalse: [0].
      
      "Transcript showCr: 'DEBUG: measureBench: loops_p_sec=', loops_p_sec printString; cr."
      
      Transcript showCr: loops_p_sec printString, '/s (time=', tMeas printString, ' ms, loops=', 
        loops printString, ', delta=', t_delta printString, ' ms)'.

      (x = -1)
      ifTrue: [
        throughput := -1.
      ]
      ifFalse: [
        ((tEsti > 0) and: [t_delta < delta_ms])
        ifTrue: [
          throughput := loops_p_sec.
          Transcript showCr: 'Benchmark ', bench printString, ' (', prg_language printString, '): ',
            loops_p_sec printString, '/s (time=', tMeas printString, ' ms, loops=', 
            loops printString, ', delta=', t_delta printString, ' ms)'.
        ]
        ifFalse: [
            (tMeas > max_ms)
            ifTrue: [
                Transcript showCr: 'Benchmark ', bench printString, ' (', prg_language printString, '): ',
                  'Time already > ', max_ms printString, ' ms. No measurement possible.'.
                throughput := (loops_p_sec > 0) ifTrue: [ 0 - loops_p_sec ] ifFalse: [ -1.0 ].
            ].
        ].
      ].

      (throughput = 0)
      ifTrue: [
          (tMeas = 0)
          ifTrue: [
              scale_fact := 50.
          ]
          ifFalse: [ 
              (tMeas < cali_ms)
              ifTrue: [
                  scale_fact := ((cali_ms + 100) quo: tMeas) + 1.
              ]
              ifFalse: [
                  scale_fact := 2.
              ].
          ].
          "Transcript showCr: 'DEBUG: measureBench: scale_fact=', scale_fact printString."
          loops := loops * scale_fact.
          tEsti := tMeas * scale_fact.
      ].

      "throughput := 10."
  ].
  ^throughput.
!


"print_results(int bench1, int bench2, double bench_res1[])"
print_results: bench2 benchRes: benchRes
 | bench1 |
 bench1 := self.
  Transcript cr.
  Transcript showCr: 'Times for all benchmarks (loops per sec):';
    show: 'BM Results (Smalltalk)   : '.

  bench1 to: bench2 do: [:bench |
    Transcript show: (benchRes at: (bench + 1)) printString, ' '.
  ].
  Transcript cr.
  ^1.
!

"start_bench(int bench1, int bench2, int n, String argStr)"  "argStr: argStr"
start_bench: bench2 num1: n
  | bench1 bench n2 benchRes check1 throughput |
  bench1 := self.
  0 determineTsPrecision.
  0 print_info.
  
  benchRes := Array new: bench2 + 1.  "benchmark results"
  "Transcript showCr: 'DEBUG: start_bench: bench1=', bench1 printString."
    
  bench1 to: bench2 do: [:bench |
    n2 := n.
    (bench = 3) ifTrue: [ n2 := n2 quo: 2 ].
    (bench = 5) ifTrue: [ n2 := n2 quo: 200 ].
    check1 := bench getCheck: n2.
    throughput := (check1 > 0) ifTrue: [ bench measureBench: n2 check1: check1 ] ifFalse: [ -1 ].
    "Transcript showCr: 'DDD: check1=', check1 printString, ' tput=', throughput printString."
    benchRes at: (bench + 1) put: throughput.
  ].
  bench1 print_results: bench2 benchRes: benchRes.

  Transcript cr.
  ^0.
!

main: argc args: argv
  | startT bench1 bench2 n rc |
  bench1 := 0.    "first benchmark to test"
  bench2 := 5.    "last benchmark to test"
  n := 1000000.   "maximum number"

  "Smalltalk arguments."
  (argc >= 1) ifTrue: [ bench1 := (argv at: 1) asInteger ].
  (argc >= 2) ifTrue: [ bench2 := (argv at: 2) asInteger ].
  (argc >= 3) ifTrue: [ n := (argv at: 3) asInteger ].
  (argc >= 4) ifTrue: [ gState_cali_ms := (argv at: 4) asInteger ].
  (argc >= 5) ifTrue: [ gState_delta_ms := (argv at: 5) asInteger ].
  
  rc := bench1 start_bench: bench2 num1: n.
  
  Transcript showCr: 'Total elapsed time: ', (0 getTs) printString, ' ms'.
  ^rc.
! !

"start main..."

"|iter|"
"iter := (Smalltalk hasFeatures: #JIT) ifTrue: [ 2 ] ifFalse: [ 1 ]."
"Smalltalk arguments printNl."
"DateTime now printNl."

"Transcript show: 'DEBUG: The arguments were ', (Smalltalk arguments) printString; cr."

"Transcript showCr: (0 main)."
"Smalltalk at: #gState_startTs put: 0."
"Transcript show: 'DEBUG: The arguments were ', (Smalltalk at: #gState_startTs) printString; cr."
0 main: (Smalltalk arguments size) args: (Smalltalk arguments).
!
