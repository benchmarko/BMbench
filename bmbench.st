"======================================================================
| bmbench.st (Smalltalk)
|
| (c) Benchmarko, 2002
|
| 25.05.2002  0.01
|
| Usage:
| gst bmbench.st
|

|
| 20 to: 1 by: -1 do: [:x | x printNl ] !
|
======================================================================"


"
| Time class: clocks
| millisecondClock -      Answer the number of milliseconds since startup.

  [t1 _ Time millisecondsToRun: [n1 run_bench].
    t1 < 5000] whileTrue:[ Smalltalk compact. n1 _ n1 * 2 ].

  ^((n1 * 500000 * 1000) // t1) printString, ' bytecodes/sec; ' !

   ifAbsent: [ ^self error: 'No such check #' ].
"


"Benchmark  "

!Integer methodsFor: 'bmbench_xx'!

"bench01(int loops, num_t n)"
bench01: loops amount: n
  | x sum1 i |
  "loops _ self."
  "n _ 10."

  x _ 0.
  sum1 _ (n / 2) * (n + 1).  "assuming n even!"
  "(sum1..1000000 depends on type: 500000500000 (floating point), 1784293664 (32bit), 10528 (16 bit)"

  loops timesRepeat: [
    n to: 1 by: -1 do: [:i | x _ x + i ].
    x printLn.
  ].

  ^x
!

"
    while (loops-- > 0) {
      num_t i;
      for (i = n; i > 0; i--) {
        x += (int)i;
      }
      if (loops > 0) { /* some more loops left? */
        x -= sum;      /* yes, set x back to 0 (assuming n even) */
        if (x != 0) {  /* now x must be 0 again */
          return x;    /* error */
        }
      }
    }
    return(x & 0xffff);
  }
"


"run_bench(int bench, int loops, num_t n)"
runbench: bench l1: loops amount: n
  | x errf |
  "loops _ 10."
  x _ 0.
  errf _ 0.
  loops printNl.
  x _ [loops bench01].

  ^x
!


get_ms
  ^Time millisecondClock
!


main
  | start_t bench n min_ms loops x t1 |
  start_t _ 0 get_ms.
  bench _ 1. "benchmark to test"
  n _ 1000000. "maximum number"
  min_ms _ 10000. "minimum runtime for measurement in ms"

  bench _ self.


  'BM Bench v0.3 (Smalltalk)' printNl.

    loops _ 1. "number of loops"
    x _ 0. "result from benchmark"
    t1 _ 0. "timestamp"
"
    'calibration' printNl.

    (t1 < 1000)
    ifTrue: [
      'we want at least 1 sec calibration time' printNl.
      'Calibrating benchmark %d with loops=%d, n=%ld , bench, loops, n' printNl.
      t1 _ 0 get_ms.
      x _ bench runbench lp: loops n1: n.
      t1 _ 0 get_ms - t1.
      'x=d (time: d ms)', x, t1' printNl.
      loops _ loops * 2.
    ].
    loops _ loops / 2.
    loops _ loops * (min_ms / t1) + 1. "integer division"
    'Calibration done  Starting measurement with d loops to get ge d ms  loops min_ms' printNl.

    'measurement' printNl.
    t1 _ 0 get_ms.
    x _ bench runbench lp: loops n1: n.
    t1 _ 0 get_ms - t1.
    'x=d (time: d ms) , x, t1' printNl.
    'Elapsed time for d loops: ld ms; estimation for 10 loops: ld ms , loops, t1, (t1 * 10 / loops)' printNl.

  'Total elapsed time: ld ms , (get_ms() - start_t)' printNl.
"

  ^bench printString, ' bla - exit'!
! !

"start main..."

|iter|
iter := (Smalltalk hasFeatures: #JIT) ifTrue: [ 2 ] ifFalse: [ 1 ].
Transcript showCr: iter main!