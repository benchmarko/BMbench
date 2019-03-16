\ bmbench.fs (Forth)
\ (c) Benchmarko, 2002
\
\ 12.05.2002  0.01
\
\ Usage:
\ gforth bmbench.fs -e bye
\
\ Some hints for me:
\ Forth has a stack architecture
\ - \  ( ) are comments
\ - separate all things with space
\ - see (on any defined object, operator) is nice...
\ /usr/share/gforth/0.5.0/   /usr/share/info/

\ compute sum 1..n
: sum ( n -- sum )
  0 SWAP   \ put 0 (our current sum) on stack, get loop limit to top
  1+       \ including limit n
  1 ?DO    \ loop from 1 to limit
    i +    \ add current loop index
  LOOP
;

\ compute (n / 2) * (n + 1)  [ DUP 2 / SWAP 1 + * ]
\ n must be even!
: esum ( n -- x )
  DUP 2 / SWAP 1 + *
;

variable max_loop \ ugly but to put it also on stack seems to complicated...

\ benchmark 01
: bench01 ( n loops -- x )
  0 SWAP   \ 0 = current sum to top-1 ( stack: n sum=0 loops )
  DUP 1- max_loop ! \ we need max_loop inside loop ( stack: n sum loops )
  0 ?DO    \ from 0 to loops ( stack: n sum )
    OVER   \ copy top-1 (n) to top ( stack: n sum n )
    sum    \ consume n, replace by sum ( stack: n sum sum2 )
    +      \ add sum ( stack: n sum )
    i max_loop @ < IF
      OVER   \ ( stack: n sum n )
      esum   \ ( stack: n sum sum2 )
      -      \ ( stack: n sum )
      DUP 0<> IF
        s" Error in bench01!" exception throw
        UNLOOP
        EXIT
      THEN \ endif
    THEN \ endif
  LOOP
  NIP      \ remove top-1 (n) (stack: sum)
  65536 MOD
;


\ benchmark 02 (floating point arithmetic to do...)
: bench02 ( n loops -- x )
  NIP
  NIP
  0
;



: run_bench ( n loops bench -- x )
  CASE \ switch on bench
    1 OF bench01 ENDOF
    2 OF bench02 ENDOF
    ( n ) s" Error: Unknown benchmark: " exception throw ( n )
  ENDCASE
;

\ test: 1000000 5 1 run_bench


: get_ms ( -- ms )
\  TIME&DATE
\  SWAP 60 * + SWAP 3600 * + \ compute seconds
\  1000 *  \ convert to ms
\  NIP NIP NIP \ throw away date components
  UTIME  \ get microseconds  (only gforth, info section 10, or cputime.. user/sys)
  DROP   \ throw away high part
  1000 /  \ convert to ms
  \ .s cr
;


variable bench
variable n
variable min_ms

: main ( )
  get_ms         \ memorize start time
  1 bench !      \ benchmark to test
  1000000 n !    \ maximum number
  10000 min_ms ! \ minimum runtime for measurement in ms

  ." BM Bench v0.2 (Forth)" cr
  \ s" gforth" environment? [IF] .( Gforth version ) TYPE
  \                         [ELSE] .( Not Gforth..) [THEN]

  1   \ number of loops
  0   \ time
  \ Calibration
  BEGIN
    DUP 1001 <
  WHILE
    DROP   \ throw away time, will be computed new...
    ." Calibrating benchmark " bench @ . ." with loops=" DUP . ." , n=" n @ . cr
    get_ms
    \ .s cr
    OVER    \ loops
    n @  SWAP  bench @  run_bench
    ." x=" .
    get_ms SWAP -
    ."  (time: " DUP . ." ms)" cr
    SWAP
    2 *   \ loops *= 2
    SWAP
  REPEAT

  min_ms @ SWAP / 1+   \  min_ms/t1 + 1
  SWAP
  2 /   \ loops /= 2
  *
  ." Calibration done. Starting measurement with " DUP . ." loops to get >=" min_ms @ . ." ms" cr

  \ measurement
  get_ms
  OVER
  n @  SWAP  bench @  run_bench
  ." x=" .
  get_ms SWAP -
  ."  (time: " DUP . ." ms)" cr

  ." Elapsed time for " SWAP DUP . SWAP ." loops: " DUP . ." ms; estimation for 10 loops: "
  10 *
  SWAP /
  . ."  ms" cr

  get_ms SWAP -
  ." Total elapsed time: " . ." ms" cr
;


main

\ end
