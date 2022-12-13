#!/bin/sh
# the next line restarts using tclsh \
exec tclsh "$0" "$@"

#
# BM Bench - bmbench1.pl (Tcl)
# (c) Marco Vieth, 2002-2006
# http://www.benchmarko.de
#
# 06.05.2002 0.01
# 11.05.2002 0.02  bench01 = (sum 1..n) mod 65536 (integer)
# 22.05.2002 0.03  bench02 = (sum 1..n) mod 65536 (floating point), bench03 = Sieve of Eratosthenes
# 20.07.2002 0.04  some errors corrected
# 24.01.2003 0.05  output format changed
# 30.05.2006 0.06  based on version 0.05
#
# Usage:
# tclsh bmbench.tcl [bench1] [bench2] [n]
#
#

#
# Note:
# - This was hard to code if you don't know Tcl and it is your first program.
#   On my Linux system  man n <tcl command> was a good starting point.
# - There is no need for ';' at end of instructions but if you want some comments...
# - Do not forget to put arguments of expr in braces {}! Otherwise you will see a large performance degradion...
#   (See man n expr)
#


# also possible?
# https://www.tcl.tk/man/tcl8.6/TclCmd/mathop.htm#M32
#
#namespace path {::tcl::mathop ::tcl::mathfunc};
#would allow: set sum [+ $sum $i];

#
# General description for benchmark test functions
# benchxx - benchmark
# <description>
# in: loops = number of loops
#         n = maximum number (assumed even, normally n=1000000)
# out:    x = <output decription>
#
# loops may be increased to produce a longer runtime without changing the result.
#

set PRG_VERSION "0.072";
set PRG_LANGUAGE "Tcl";

set gState(tsType) "msec";
set gState(tsPrecCnt) 0;
set gState(tsMeasCnt) 0;
set gState(fact) {0 0 0 20 0 0}; # benchmark simplification factors for n

#
# bench00 (Integer 16 bit)
# (sum of 1..n) mod 65536
#
# (Tcl tries to compute with C longs as long as no floating point number is introdced.)
proc bench00 {loops n check} {
  set x 0;
  #set sum1 [expr {(($n / 2) * ($n + 1)) & 0xffff}];
  set n_div_65536 [expr {($n >> 16) & 0xffff}];
  set n_mod_65536 [expr {$n & 0xffff}];
  #puts "DEBUG: sum1=$sum1, n_div=$n_div_65536, n_mod=$n_mod_65536";
  while {$loops > 0 && $x == 0} {
    incr loops -1;
    for {set i $n_div_65536} {$i > 0} {incr i -1} {
      for {set j 65535} {$j > 0} {incr j -1} {
        incr x $j; # a bit faster than "set x [expr {$x + $j}]"
      }
    }
    for {set j $n_mod_65536} {$j > 0} {incr j -1} {
      incr x $j; # a bit faster than "set x [expr {$x + $j}]"
    }
    set x [expr {$x & 0xffff}];
    incr x -$check;
  }
  return [expr {$x & 0xffff}];
}


#
# bench01 (Integer 16/32 bit)
# (arithmetic mean of 1..n)
#
# (Tcl tries to compute with C longs as long as no floating point number is introduced)
#
proc bench01 {loops n check} {
  set x 0;
  while {$loops > 0 && $x == 0} {
    incr loops -1;
    set sum 0;
    for {set i 1} {$i <= $n} {incr i} {
      incr sum $i; # a bit faster than "set sum [expr {$sum + $i}]"
      if {$sum >= $n} {
        incr sum -$n;
        incr x;
      }
    }
    incr x -$check;
  }
  return $x;
}


#
# bench02 (Floating Point, normally 64 bit)
# (arithmetic mean of 1..n)
#
proc bench02 {loops n check} {
  set x 0;
  while {$loops > 0 && $x == 0} {
    incr loops -1;
    set sum 0.0; # force floating point
    for {set i 1} {$i <= $n} {incr i} {
      set sum [expr {$sum + $i}]; # cannot use incr for fp
      if {$sum >= $n} {
        set sum [expr {$sum - $n}]; # cannot use incr for fp
        incr x;
      }
    }
    incr x -$check;
  }
  return $x;
}


#TTT remove:
proc bench02_old1 {loops n check} {
  set x 0.0; # force floating point
  set sum1 [expr {($n / 2.0) * ($n + 1.0)}]
  while {$loops > 0} {
    incr loops -1;
    for {set i $n} {$i > 0} {incr i -1} {
      set x [expr {$x + $i}];
    }
    if {$loops > 0} { # some more loops left?
      set x [expr {$x - $sum1}];  # yes, set x back to 0 (assuming n even)
      if {$x != 0.0} { # now x must be 0 again
        incr x;
        break; #error
      }
    }
  }
  return [expr {int($x - int($x / 65536.0) * 65536.0)}]; # % is for integer only, could use fmod()
}


#
# bench03 (Integer)
# number of primes below n (Sieve of Eratosthenes)
# Example: n=500000 => x=41538 (expected), n=1000000 => x=78498
#
# I have not found bit arrays in Tcl so I used lists, but it is very slow, if we need to
# replace elements with lreplace.
# Strings could also be used but are even slower:
# - init sieve: set sieve1 [string repeat 1 [expr {$n + 1}]];
#               set sieve1 [string replace $sieve1 0 0 0]; set sieve1 [string replace $sieve1 1 1 0];
# - res bit:    set sieve1 [string replace $sieve1 $j $j 0];
# - test bit:   [string index $sieve1 $i]
#
proc bench03 {loops n check} {
  set n [expr {$n / 2}]; # compute only up to n/2
  set x 0; # number of primes below n
  #set sieve1 [list 0 0]; # set first 2 elements

  #set n [expr {$n / 40}]; # Tcl is slow, so use this factor
  #puts "Important note: For Tcl we use 1/40 of the problem to get to an end (n=$n)...";

  set nHalf [expr {$n / 2}]; 

  while {$loops > 0 && $x == 0} {
    incr loops -1
    # initialize sieve
    set sieve1 {0 0}; # set first 2 elements
    for {set i 2} {$i <= $nHalf} {incr i} {
      lappend sieve1 0;
    }
    # compute primes
    set i 0; 
    set m 3; 
    incr x; # 2 is prime
    while {[expr {$i * $i}] <= $n} {
      if {![lindex $sieve1 $i]} {
        incr x; # m is prime
        set j [expr {($m * $m - 3) >> 1}];
        while {$j < $nHalf} {
          set sieve1 [lreplace $sieve1 $j $j 1];
          #puts "DEBUG: res (j=$j): sieve=$sieve1";
          incr j $m; #set j [expr {$j + $m}];
        }
      }
      incr i;
      incr m 2;
    }

    # count remaining primes
    while {$m < $n} {
      if {![lindex $sieve1 $i]} {
        incr x;
      }
      incr i;
      incr m 2;
    }

    incr x -$check;
  }
  #puts "DEBUG: sieve=$sieve1";
  return $x;
}

proc bench03_old_ok1 {loops n check} {
  set n [expr {$n / 2}]; # compute only up to n/2
  set x 0; # number of primes below n
  #set sieve1 [list 0 0]; # set first 2 elements

  set n [expr {$n / 40}]; # Tcl is slow, so use this factor
  puts "Important note: For Tcl we use 1/40 of the problem to get to an end (n=$n)...";

  while {$loops > 0 && $x == 0} {
    incr loops -1
    # initialize sieve
    set sieve1 {0 0}; # set first 2 elements
    for {set i 2} {$i <= $n} {incr i} {
      lappend sieve1 1; #$sieve1[$i] = 1;
    }
    # compute primes
    for {set i 2} {[expr {$i * $i}] <= $n} {incr i} {
      if {[lindex $sieve1 $i]} {
        for {set j [expr {$i * $i}]} {$j <= $n} {incr j $i} {
          set sieve1 [lreplace $sieve1 $j $j 0];
          #puts "DEBUG: res (j=$j): sieve=$sieve1";
        }
      }
    }
    # count primes
    for {set i 0} {$i <= $n} {incr i} {
      if {[lindex $sieve1 $i]} {
        incr x;
      }
    }
    incr x -$check;
  }
  #puts "DEBUG: sieve=$sieve1";
  return $x;
}


#
# bench04 (Integer 32 bit)
# nth random number number
# Random number generator taken from
# Raj Jain: The Art of Computer Systems Performance Analysis, John Wiley & Sons, 1991, page 442-444.
# It needs longs with at least 32 bit.
# Starting with x0=1, x10000 should be 1043618065, x1000000 = 1227283347.
#
proc bench04 {loops n check} {
  set m 2147483647; # modulus, do not change!
  set a 16807;      # multiplier
  set q 127773;     # m div a
  set r 2836;       # m mod a
  set x 0;          # last random value
  while {$loops > 0 && $x == 0} {
    incr loops -1;
    incr x; # start with 1
    for {set i $n} {$i > 0} {incr i -1} {
      set x [expr {$a * ($x % $q) - $r * ($x / $q)}]; # x div q
      if {$x <= 0} {
        incr x $m; # x is new random number
      }
    }
    incr x -$check;
  }
  return $x;
}


#
# bench05 (Integer 32 bit) (list implementation)
# n over n/2 mod 65536 (Pascal's triangle)
# We start with an empty list and append elements, this will be much faster than replacing elements in a list with
# e.g.  set pas1 [lreplace $pas1 $j $j [expr {[lindex $pas2 $j_1] + [lindex $pas2 $j]}]];
#
proc bench05 {loops n check} {
  set x 0;
  set n [expr {$n / 500}];
  set k [expr {$n / 2}];

  if { (($n - $k) < $k) } {
    set k [expr {$n - $k}]; # keep k minimal with  n over k  =  n over n-k
  }
  
  # allocate memory... (not needed for Tcl)
  #int pas1[][] = new int[2][k + 1];
  #pas1[0][0] = 1; pas1[1][0] = 1; // set first column

  while {$loops > 0 && $x == 0} {
    incr loops -1;
    set pas1 {1};
    for {set i 2} {$i <= $n} {incr i} {
      set pas2 $pas1; # get last line to pas2
      set pas1 {1};   # and restart with new list
      set i_mod_2 [expr {$i % 2}];
      set min1 [expr {($i - 1) / 2}];
      if {$k < $min1} {
        set min1 $k;
      }

      #pas1[i_mod_2][1] = i; # second column is i
      lappend pas1 $i;

      #puts "DEBUG -- (i=$i): pas1=$pas1, pas2=$pas2";
      for {set j 2} {$j <= $min1} {incr j} { # up to min((i-1)/2, k)
        #pas1[i_mod_2][j] = (pas1[i_mod_2 ^ 1][j - 1] + pas1[i_mod_2 ^ 1][j]);
        lappend pas1 [expr {[lindex $pas2 [expr {$j - 1}]] + [lindex $pas2 $j]}];
        #puts "DEBUG (j=$j): pas1=$pas1, pas2=$pas2";
      }
      if { [expr {($min1 < $k) && ($i_mod_2 == 0)}] } { # new element
        #pas1[i_mod_2][min1 + 1] = 2 * pas1[i_mod_2 ^ 1][min1];
        #lappend pas1 [expr {2 * [lindex $pas2 [expr {($i - 1) / 2}]]}];
        lappend pas1 [expr {2 * [lindex $pas2 $min1]}];
      }
    }

    #puts "DEBUG: pas1=$pas1, pas2=$pas2";

    #x += pas1[n % 2][k] & 0xffff; // % 65536
    set x [expr {$x + [lindex $pas1 $k] & 0xffff}];

    incr x -$check;
  }
  return $x;
}


#
# run a benchmark
# in: bench = benchmark to use
#     loops = number of loops
#         n = maximum number (used in some benchmarks to define size of workload)
# out:    x = result
#
proc run_bench {bench loops n check} {
  set x 0;

  if {$bench == 0} {
    set x [bench00 $loops $n $check];

  } elseif {$bench == 1} {
    set x [bench01 $loops $n $check];

  } elseif {$bench == 2} {
    set x [bench02 $loops $n $check];

  } elseif {$bench == 3} {
    set x [bench03 $loops $n $check];

  } elseif {$bench == 4} {
    set x [bench04 $loops $n $check];

  } elseif {$bench == 5} {
    set x [bench05 $loops $n $check];

  } else {
    puts "Error: Unknown benchmark: $bench";
  }
  # can we use Tcl switch?

  incr x $check;
  if {$x != $check} {
    puts "Error(bench $bench): x=$x";
    # error "Error in benchmark.";
    set x -1;
  }
  return $x;
}


proc bench03Check {n} {
  set check 0;

  set n [expr {$n / 2}];

  for {set j 2} {$j <= $n} {incr j} {
    set isPrime 1;
    for {set i 2} {[expr {$i * $i}] <= $j} {incr i} {
      if {![expr {$j % $i}]} {
        set isPrime 0;
        break;
      }
    }
    if {$isPrime} {
      incr x;
    }
  }
  return $x;
}

proc getCheck {bench n} {
  global gState PRG_LANGUAGE;
  set check 0;

  set fact [lindex $gState(fact) $bench];
  if {$fact} {
    set n [expr {$n / $fact}];
    puts "Note: $PRG_LANGUAGE is rather slow, so reduce n by factor $fact to $n.";
  }

  if {$bench == 0} {
    #set check 10528;
    set check [expr {(($n / 2) * ($n + 1)) & 0xffff}];

  } elseif {$bench == 1} {
    #set check 10528;
    set check [expr {($n + 1) / 2}];

  } elseif {$bench == 2} {
    set check [expr {($n + 1) / 2}];

  } elseif {$bench == 3} {
    set check [expr $n == 1000000 ? 41538 : [bench03Check $n]];

  } elseif {$bench == 4} {
    #set check 1227283347;
    set check [expr {$n == 1000000 ? 1227283347 : [bench04 1 $n 0]}]; # bench04 not a real check

  } elseif {$bench == 5} {
    #set check 27200;
    set check [expr {$n == 1000000 ? 27200 : [bench05 1 $n 0]}]; # bench045not a real check

  } else {
    puts "Error: Unknown benchmark: $bench";
    set check -1;
  }
  return $check;
}


#
# get timestamp in milliseconds
# out: x = time in ms
#
proc get_ms {} {
  clock clicks -milliseconds;
}

# Try millisecond timer.
# If Tcl version does not support it, use timer with seconds...
if {[catch get_ms]} {
  proc get_ms {} {expr {[clock seconds] * 1000}}
  set gState(tsType) "sec";
}


proc correctTime {tMeas measCount} {
  global gState;
	set tsPrecCnt $gState(tsPrecCnt);

	if {$measCount < $tsPrecCnt} {
		set tMeas [expr {$tMeas + $gState(tsPrecMs) * ((($tsPrecCnt - $measCount) * 1.0) / $tsPrecCnt)}]; # ts + correction
      # make sure to use fp division by converting an operand first!
	}
	return $tMeas;
}

proc getPrecMs {stopFlg} {
  global gState;
	set measCount 0;

	set tMeas0 [get_ms];
	set tMeas $tMeas0;
	while {$tMeas <= $tMeas0} {
		set tMeas [get_ms];
		incr measCount;
	}

	if {$stopFlg == 1} {
		set tMeas [correctTime $tMeas0 $measCount]; # for stop: use first ts + correction
	}
	set gState(tsMeasCnt) $measCount; # memorize count
  #puts "DEBUG: getPrecMs";
	return $tMeas;
}

# usually only neede if time precision is low, e.g. one second
proc determineTsPrecision {} {
  global gState;
	set tMeas0 [getPrecMs 0];
  set tMeas1 [getPrecMs 0];
	set gState(tsPrecMs) [expr {$tMeas1 - $tMeas0}];
	set gState(tsPrecCnt) $gState(tsMeasCnt);

	# do it again
	set tMeas0 $tMeas1;
	set tMeas1 [getPrecMs 0];
	if {$gState(tsMeasCnt) > $gState(tsPrecCnt)} { # take maximum count
	  set	gState(tsPrecCnt) $gState(tsMeasCnt);
		set gState(tsPrecMs) [expr {$tMeas1 - $tMeas0}];
	}
}


proc checkbits_int1 {} {
  set num 1;
  set last_num 0;
  set bits 0;
  while {$bits < 101} {
    set last_num $num;
    set num [expr {$num * 2}];
    incr num;
    incr bits;
    set oldnum [expr {($num - 1) / 2}];
    if {$oldnum != $last_num} {
      break;
    }
  }
  return $bits;
}

proc checkbits_double1 {} {
  set num 1.0;
  set last_num 0.0;
  set bits 0;
  while {$bits < 101} {
    set last_num $num;
    set num [expr {$num * 2.0}];
    set num [expr {$num + 1.0}];
    incr bits;
    set oldnum [expr {($num - 1.0) / 2.0}];
    if {$oldnum != $last_num} {
      break;
    }
  }
  return $bits;
}


proc print_info {} {
  global PRG_VERSION;
  global PRG_LANGUAGE;
  global gState;
  puts "BM Bench v$PRG_VERSION ($PRG_LANGUAGE) -- (int:[checkbits_int1] double:[checkbits_double1] tsType:$gState(tsType) tsMs:$gState(tsPrecMs) tsCnt:$gState(tsPrecCnt)) Tcl [info tclversion] patchlevel [info patchlevel]; library: [info library]; hostname: [info hostname]";
  puts "Date: [clock format [clock seconds]]";
#  puts "t1: $tcl_precision";
#  puts "on platform [$tcl_platform{2}]";
}


proc print_results {bench_res} {
  global PRG_LANGUAGE;
  set MAX_LANGUAGE_LEN1 10;
  puts "\nThroughput for all benchmarks (loops per sec):";

  set str_len [expr { $MAX_LANGUAGE_LEN1 - [string length $PRG_LANGUAGE] }];
  #set str [expr { [string repeat " " $str_len] }]; # works for Tcl 8.x
  set str "";
  for {set i 0} {$i < $str_len} {incr i} {
    set str "$str ";
  }

  puts -nonewline "BMR ($PRG_LANGUAGE)$str: ";

  foreach t1 $bench_res {
    puts -nonewline [format "%9.3f " $t1];
  }
  puts "";
  puts "";
}


proc measureBench {bench n check} {
  global gState PRG_LANGUAGE;

  set caliMs 1001; # const
  set deltaMs 100; # const
  set maxMs 10000; # const

  set loops 1; # number of loops
  set x 0;     # result from benchmark
  set tMeas 0;    # measured time
  set tEsti 0;    # estimated time
  set throughput 0;

  set fact [lindex $gState(fact) $bench];
  if {$fact} {
    set n [expr {$n / $fact}];
    puts "Note: $PRG_LANGUAGE is rather slow, so reduce n by factor $fact to $n.";
  }

  puts "Calibrating benchmark $bench with n=$n, check=$check";
  while {!$throughput} {
    set tMeas [getPrecMs 0];
    set x [run_bench $bench $loops $n $check];
    set tMeas [expr {[getPrecMs 1] - $tMeas}];

    set tDelta [expr {($tEsti > $tMeas) ? ($tEsti - $tMeas) : ($tMeas - $tEsti)}]; # compute difference abs(measures-estimated)
    set loopsPerSec [expr {($tMeas > 0) ? ($loops * 1000.0 / $tMeas) : 0}];

    puts "[format %10.3f $loopsPerSec]/s (time= [format %9.3f $tMeas] ms, loops= [format %7d $loops], delta=[format %9.3f $tDelta] ms)";
    if {$x == -1} { # some error?
      set throughput -1;
    } elseif {($tEsti > 0) && ($tDelta < $deltaMs)} { # do we have some estimated/expected time, smaller than deltaMs=100?
      set throughput $loopsPerSec
      puts "Benchmark $bench ($PRG_LANGUAGE): [format %.3f $loopsPerSec]/s (time=[format %.3f $tMeas] ms, loops=$loops, delta=[format %.3f $tDelta] ms)";
    } elseif {$tMeas > $maxMs} {
      puts "Benchmark  $bench ($PRG_LANGUAGE): Time already > $maxMs ms. No measurement possible.";
      if {$loopsPerSec > 0} {
        set throughput -$loopsPerSec;
      } else {
        set throughput -1;
      }
    } else {
      set scaleFact 0;
      if {$tMeas == 0} {
				set scaleFact 50;
			} elseif {$tMeas < $caliMs} {
				set scaleFact [expr {int(($caliMs + 100) / $tMeas) + 1}]; # scale a bit up to 1100 ms (caliMs+100)
			} else {
        set scaleFact 2;
			}
      set loops [expr {$loops * $scaleFact}];
      set tEsti [expr {$tMeas * $scaleFact}];
    }
  }

  # correction factor (only useful for linear problems)
  if {$throughput >= 0 && $fact} {
    set throughput [expr {$throughput / $fact}];
    puts "Note: Estimated runtime for benchmark $bench corrected by factor $fact: [format %10.3f $throughput]";
  }

  return $throughput;
}


proc start_bench {bench1 bench2 n} {
  print_info;

  set bench_res [list ];
  for {set bench $bench1} {$bench <= $bench2} {incr bench} {
    set check [getCheck $bench $n];
    if {$check > 0} {
      set throughput [measureBench $bench $n $check];
    } else {
      set throughput -1;
    }
    lappend bench_res $throughput;
  }
  print_results $bench_res;
  return 0;  
}


proc main {argc argv} {
  set start_t [get_ms]; # memorize start time
  set bench1 0;       # first benchmark to test
  set bench2 5;       # last benchmark to test
  set n 1000000;      # maximum number

  if {$argc > 0} {
    set bench1 [lindex $argv 0];
    set bench2 $bench1;
  }
  if {$argc > 1} {
    set bench2 [lindex $argv 1];
  }
  if {$argc > 2} {
    set n [lindex $argv 2];
  }

  determineTsPrecision;
  set rc [start_bench $bench1 $bench2 $n];
  set t1 [expr {[get_ms] - $start_t}];
  puts "Total elapsed time: $t1 ms";
  return rc;
}


main $argc $argv;

# end
