#Smalltalk ==                     "start of -*- Smalltalk -*- comment
exec gst "$0" -aQ $@              # end  of     Smalltalk     comment"!

"======================================================================
| BM Bench - bmbench.st (Smalltalk)
| (c) Marco Vieth, 2002
| http://www.benchmarko.de
|
| 25.05.2002  0.01
| 05.02.2003  0.05
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


"Benchmark  "


!Integer methodsFor: 'bmbench_xx'!



"bench00(int loops, num_t n)"
"same as bench01"
bench00: n
  | loops x sum1 |

  loops := self.
  x := 0.
  sum1 := (n / 2) * (n + 1) bitAnd: 65535.  "assuming n even!"
  "(sum1..1000000 depends on type: 500000500000 (floating point), 1784293664 (32bit), 10528 (16 bit)"
  loops to: 1 by: -1 do: [:loop |
    1 to: n do: [:i | x := x + i bitAnd: 65535 ].
    (loop > 1)  "Some more loops left?"
      ifTrue: [
        x := x - sum1. "yes, set x back to 0 (assuming n even)"
        (x = 0)  "now x must be 0 again"
          ifFalse: [
            x := x + 1. "error"
            ^x bitAnd: 65535. "or: ^x rem: 65536."
          ].
      ].
  ].
  ^x bitAnd: 65535. "or: ^x rem: 65536."
!


"bench01(int loops, num_t n)"
"ShortInteger is MUCH faster than LongInteger but we use LongInteger here to show the difference..."
bench01: n
  | loops x sum1 |

  loops := self.
  x := 0.
  sum1 := (n / 2) * (n + 1).  "assuming n even!"
  "Transcript showCr: 'DEBUG:', (x inspect) printString."
  "(sum1..1000000 depends on type: 500000500000 (floating point), 1784293664 (32bit), 10528 (16 bit)"
  "Transcript showCr: 'DEBUG: bench01: loops=', loops printString, ', n=', n printString, ', sum1=', sum1 printString."

  loops to: 1 by: -1 do: [:loop |
    1 to: n do: [:i | x := x + i ].
    "Transcript showCr: 'DEBUG: bench01: loop=', loop printString, ', x=', x printString."
    (loop > 1)  "Some more loops left?"
      ifTrue: [
        x := x - sum1. "yes, set x back to 0 (assuming n even)"
        (x = 0)  "now x must be 0 again"
          ifFalse: [
            x := x + 1. "error"
            ^x bitAnd: 65535. "or: ^x rem: 65536."
          ].
      ].
  ].
  ^x bitAnd: 65535. "or: ^x rem: 65536."
!


"bench02(int loops, num_t n)"
bench02: n
  | loops x sum1 |

  loops := self.
  x := 0.0.
  sum1 := (n / 2.0) * (n + 1.0). "assuming n even!"
  "(sum1..1000000 depends on type: 500000500000 (floating point), 1784293664 (32bit), 10528 (16 bit)"

  loops to: 1 by: -1 do: [:loop |
    1 to: n do: [:i | x := x + i ].
    (loop > 1)  "Some more loops left?"
      ifTrue: [
        x := x - sum1. "yes, set x back to 0 (assuming n even)"
        (x = 0.0)  "now x must be 0 again"
          ifFalse: [
            x := x + 1. "error"
            ^(x rem: 65536) asInteger.
          ].
      ].
  ].
  ^(x rem: 65536) asInteger.
!


"bench03(int loops, num_t n)"
"number of primes below n (Sieve of Eratosthenes)"
"Example: n=500000 => x=41538 (expected), n=1000000 => x=78498"
bench03_hmm1: nP
  | loops n x sieve1 i |
  loops := self.
  n := nP / 2. "compute only up to n/2"
  x := 0. "number of primes below n"
  sieve1 := Array new: n. "arrays starting from 1"
  "We know that 0 is not a prime, so don't count it."
  sieve1 at: 1 put: false. "1 is not a prime"
  loops to: 1 by: -1 do: [:loop |
    "initialize sieve"
    2 to: n do: [:j | sieve1 at: j put: true ].
    "compute primes"
    i := 2.
    [(i * i) <= n] whileTrue: [
      (sieve1 at: i) ifTrue: [
        (i * i) to: n by: i do: [:j | sieve1 at: j put: false ].
      ].
      i := i + 1.
    ].
    "count primes"
    1 to: n do: [:i | (sieve1 at: i) ifTrue: [ x := x + 1 ] ].
    "check prime count"
    (loop > 1)  "Some more loops left?"
      ifTrue: [
        x := x - 41538. "yes, set x back to 0 (number of primes below 1000000)"
        (x = 0)  "now x must be 0 again"
          ifFalse: [
            x := x + 1. "error"
            ^x.
          ].
      ].
  ].
  ^x.
!


bench03: nP
  | loops n x sieve1 i |
  loops := self.
  n := nP / 2. "compute only up to n/2"
  x := 0. "number of primes below n"
  sieve1 := ByteArray new: n. "arrays starting from 1; we try ByteArray..."
  "We know that 0 is not a prime, so don't count it."
  sieve1 at: 1 put: 0. "1 is not a prime"
  loops to: 1 by: -1 do: [:loop |
    "initialize sieve"
    2 to: n do: [:j | sieve1 at: j put: 1 ].
    "compute primes"
    i := 2.
    [(i * i) <= n] whileTrue: [
      ((sieve1 at: i) > 0) ifTrue: [
        (i * i) to: n by: i do: [:j | sieve1 at: j put: 0 ].
      ].
      i := i + 1.
    ].
    "count primes"
    1 to: n do: [:i | ((sieve1 at: i) > 0) ifTrue: [ x := x + 1 ] ].
    "check prime count"
    (loop > 1)  "Some more loops left?"
      ifTrue: [
        x := x - 41538. "yes, set x back to 0 (number of primes below 1000000)"
        (x = 0)  "now x must be 0 again"
          ifFalse: [
            x := x + 1. "error"
            ^x.
          ].
      ].
  ].
  ^x.
!



"bench04(int loops, num_t n)"
" nth random number number
| Random number generator taken from
| Raj Jain: The Art of Computer Systems Performance Analysis, John Wiley & Sons, 1991, page 442-444.
| It needs longs with at least 32 bit.
| Starting with x0=1, x10000 should be 1043618065, x1000000 = 1227283347.
| Note: We have just 30 Bit SmallInteger and LongInteger is very slow, so use Float...
"
bench04: n
  | loops m a q r x xDivq xModq |
  loops := self.
  m := 2147483647.0. "modulus, do not change!" "hey, this is Longinteger because we have only 30 bits!"
  a := 16807.0. "multiplier"
  q := 127773.0. "m div a"
  r := 2836.0. "m mod a"
  x := 1.0. "last random value"
  "Transcript showCr: 'DEBUG bench04: x=', x printString."
  loops to: 1 by: -1 do: [:loop |
    n timesRepeat: [
      xDivq := x quo: q.
      "Transcript showCr: 'DEBUG: xDivq=', xDivq printString."
      xModq := x - (q * xDivq).
      x := (a * xModq) - (r * xDivq).
      "Transcript showCr: 'DEBUG: m=', (m inspect) printString."
      (x <= 0.0) ifTrue: [ x := x + m ].  "x is new random number"
      "Transcript showCr: 'DEBUG: x=', (x inspect) printString."
      "(tmp1 > 24501) ifTrue: [ Transcript showCr: 'DEBUG: tmp1=', tmp1 printString.]."
    ].
    "Transcript showCr: 'DEBUG bench04: x2=', x printString."
    (loop > 1)  "Some more loops left?"
      ifTrue: [
        x := x - 1227283347. "yes, set x back to 0"
        (x = 0)  "now x must be 0 again"
          ifFalse: [
            x := x + 1. "error"
            ^(x asInteger).
          ].
        x := x + 1. "start with 1 again"
      ].
  ].
  ^(x asInteger).
!


"bench05(int loops, num_t n)"
bench05: nP
  | loops x n k pas1 iMod2 i1Mod2 min1 |
  loops := self.

  x := 0.
  n := nP quo: 500.
  k := n bitShift: -1. "div 2"
  "Transcript showCr: 'DEBUG: n=', (n inspect) printString, ', k=', (k inspect) printString."
  "Transcript showCr: 'DEBUG: n=', n printString, ', k=', k printString."

  ((n - k) < k) ifTrue: [
    k := n - k. "keep k minimal with  n over k  =  n over n-k"
  ].

  "allocate memory..."
  pas1 := Array new: 2.
  pas1 at: 1 put: (Array new: (k + 1)).
  pas1 at: 2 put: (Array new: (k + 1)).
  (pas1 at: 1) at: 1 put: 1.  (pas1 at: 2) at: 1 put: 1.  "set first column"

  loops to: 1 by: -1 do: [:loop |
    3 to: n do: [:i |
      iMod2 := i bitAnd: 1.
      i1Mod2 := iMod2 bitXor: 1.
      min1 := (i - 1) quo: 2.
      (k < min1) ifTrue: [ min1 := k ].
      (pas1 at: (iMod2+1)) at: (1+1) put: i. "second column is i"
      2 to: min1 do: [:j |  "up to min((i-1)/2, k)"
        "Transcript showCr: 'DEBUG: iMod2=', iMod2 printString, ', i1Mod2=', i1Mod2 printString, ', i=', i printString, ', min1=', min1 printString, ', j=', j printString."
        (pas1 at: (iMod2+1)) at: (j+1) put: (((pas1 at: (i1Mod2 + 1)) at: (j - 1 + 1)) + ((pas1 at: (i1Mod2 + 1)) at: (j+1)) bitAnd: 65535).
        "we need mod here to avoid overflow"
      ].
      ((min1 < k) and: [iMod2 = 0]) ifTrue: [   "new element"
        (pas1 at: (iMod2+1)) at: (min1+1+1) put: ((pas1 at: (i1Mod2+1)) at: (min1+1)) * 2.
      ].
    ].
    x := x + ((pas1 at: ((n bitAnd: 1) + 1)) at: (k+1)) bitAnd: 65535.

    (loop > 1)  "Some more loops left?"
      ifTrue: [
        x := x - 27200. "yes, set x back to 0"
        (x = 0)  "now x must be 0 again"
          ifFalse: [
            x := x + 1. "error"
            ^x.
          ].
      ].

  ].
  ^x.
!


"run_bench(int bench, int loops, num_t n)"
runBench: loops num1: n
  | bench x check1 |
  bench := self.
  "Transcript showCr: 'DEBUG: runBench: bench=', bench printString, ', loops=', loops printString, ', n=', n printString."

  "Smalltalk compact."  "TEST..."
  x := 0.
  check1 := 0.
  (bench = 0) ifTrue: [ x := loops bench00: n. check1 := 10528 ].
  (bench = 1) ifTrue: [ x := loops bench01: n. check1 := 10528 ].
  (bench = 2) ifTrue: [ x := loops bench02: n. check1 := 10528 ].
  (bench = 3) ifTrue: [ x := loops bench03: n. check1 := 41538 ].
  (bench = 4) ifTrue: [ x := loops bench04: n. check1 := 1227283347 ].
  (bench = 5) ifTrue: [ x := loops bench05: n. check1 := 27200 ].
  "TO DO: 'Error: Unknown benchmark: %d\n', bench); check1 = x + 1; /* force error */"

  (check1 = x) ifFalse: [
     Transcript showCr: 'Error(bench', bench printString, '): x=', x printString.
     x := -1.
  ].

  ^x
!


getMs
  ^Time millisecondClock
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


main: argc args: argv
  | startT bench1 bench2 n minMs benchRes loops x t1 bench |
  startT := 0 getMs. "memorize start time"
  bench1 := 0.    "first benchmark to test"
  bench2 := 5.    "last benchmark to test"
  n := 1000000.   "maximum number"
  minMs := 10000. "minimum runtime for measurement in ms"
  benchRes := Array new: bench2 + 1.  "benchmark results"

  "Smalltalk arguments."
  (argc >= 1) ifTrue: [ bench1 := (argv at: 1) asInteger ].
  (argc >= 2) ifTrue: [ bench2 := (argv at: 2) asInteger ].
  (argc >= 3) ifTrue: [ n := (argv at: 3) asInteger ].

  Transcript showCr: 'BM Bench v0.5 (Smalltalk) -- (short:', (0 checkbitsSmall1) printString,
    ', int:', (0 checkbitsInt1) printString,
    ', double:', (0 checkbitsDouble1) printString,') ', (Smalltalk version);
    showCr: '(c) Marco Vieth, 2002';
    showCr: (DateTime now) printString.

  bench1 to: bench2 do: [:bench |
    loops := 1. "number of loops"
    x := 0. "result from benchmark"
    t1 := 0. "timestamp"

    [t1 < 1001 and: [(x = -1) not]] whileTrue: [
      Transcript showCr: 'Calibrating benchmark ', bench printString, ' with loops=', loops printString, ', n=', n printString.
      t1 := 0 getMs.
      x := bench runBench: loops num1: n.
      t1 := 0 getMs - t1.
      Transcript showCr: 'x=', x printString, ' (time: ', t1 printString, ' ms)'.
      loops := loops * 2.
    ].

    (x = -1)
    ifFalse: [
      loops := loops / 2.
      loops := ((loops * minMs) quo: t1) + 1.

      Transcript showCr: 'Calibration done. Starting measurement with ', loops printString, ' loops to get ', minMs printString, ' ms'.

      t1 := 0 getMs.
      "Transcript showCr: 'DEBUG: bench=', bench printString, ', loops=', loops printString, ', n=', n printString."
      x := bench runBench: loops num1: n.
      t1 := 0 getMs - t1.
      benchRes at: (bench + 1) put: (t1 * 10 quo: loops).
      Transcript showCr: 'x=', x printString, ' (time: ', t1 printString, ' ms)';
        showCr: 'Elapsed time for ', loops printString, ' loops: ', t1 printString,
        ' ms; estimation for 10 loops: ', (benchRes at: (bench + 1)) printString, ' ms'.
    ]
    ifTrue: [
      benchRes at: (bench + 1) put: -1.
    ].
  ].

  Transcript showCr: 'Times for all benchmarks (10 loops, ms):';
    show: 'BM Results (Smalltalk)   : '.

  bench1 to: bench2 do: [:bench |
    Transcript show: (benchRes at: (bench + 1)) printString, ' '.
  ].

  Transcript cr; showCr: 'Total elapsed time: ', (0 getMs - startT) printString, ' ms'.

  ^0.
! !

"start main..."

"|iter|"
"iter := (Smalltalk hasFeatures: #JIT) ifTrue: [ 2 ] ifFalse: [ 1 ]."
"Smalltalk arguments printNl."
"DateTime now printNl."

"Transcript show: 'DEBUG: The arguments were ', (Smalltalk arguments) printString; cr."

"Transcript showCr: (0 main)."
0 main: (Smalltalk arguments size) args: (Smalltalk arguments).
!
