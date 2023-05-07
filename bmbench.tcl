#!/usr/local/bin/tclsh
#
# BM Bench - bmbench1.pl (Tcl) # TODO
# (c) Marco Vieth, 2002-2006
# http://www.benchmarko.de
#
# 06.05.2002 0.01
# 11.05.2002 0.02  bench01 = (sum 1..n) mod 65536 (integer)
# 22.05.2002 0.03  bench02 = (sum 1..n) mod 65536 (floating point), bench03 = Sieve of Eratosthenes
# 20.07.2002 0.04  some errors corrected
# 24.01.2003 0.05  output format changed
# 30.05.2006 0.06  based on version 0.05
# 19.02.2023 0.08  bench05 optimized
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

# Tcl global:
# https://wiki.tcl-lang.org/page/global
# https://wiki.tcl-lang.org/page/Can+you+run+this+benchmark+10+times+faster

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

set PRG_VERSION "0.08";
set PRG_LANGUAGE "Tcl";

set gState(tsType) "msec";
set gState(tsPrecCnt) 0;
set gState(tsMeasCnt) 0;
set gState(caliMs) 1001;

#
# bench00 (Integer 16 bit)
# (sum of 1..n) mod 65536
#
# (Tcl tries to compute with C longs as long as no floating point number is introdced)
proc bench00 {n} {
  set x 0;
  #set sum1 [expr {(($n / 2) * ($n + 1)) & 0xffff}];
  set n_div_65536 [expr {($n >> 16) & 0xffff}];
  set n_mod_65536 [expr {$n & 0xffff}];
  #puts "DEBUG: sum1=$sum1, n_div=$n_div_65536, n_mod=$n_mod_65536";
  for {set i $n_div_65536} {$i > 0} {incr i -1} {
    for {set j 65535} {$j > 0} {incr j -1} {
      incr x $j; # a bit faster than "set x [expr {$x + $j}]"
    }
  }
  for {set j $n_mod_65536} {$j > 0} {incr j -1} {
    incr x $j; # a bit faster than "set x [expr {$x + $j}]"
  }
  return [expr {$x & 0xffff}];
}


#
# bench01 (Integer 16/32 bit)
# (arithmetic mean of 1..n)
#
# (Tcl tries to compute with C longs as long as no floating point number is introduced)
#
proc bench01 {n} {
  set x 0;
  set sum 0;
  for {set i 1} {$i <= $n} {incr i} {
    incr sum $i; # a bit faster than "set sum [expr {$sum + $i}]"
    if {$sum >= $n} {
      incr sum -$n;
      incr x;
    }
  }
  return $x;
}


#
# bench02 (Floating Point, normally 64 bit)
# (arithmetic mean of 1..n)
#
proc bench02 {n} {
  set x 0;
  set sum 0.0; # force floating point
  for {set i 1} {$i <= $n} {incr i} {
    set sum [expr {$sum + $i}]; # cannot use incr for fp
    if {$sum >= $n} {
      set sum [expr {$sum - $n}]; # cannot use incr for fp
      incr x;
    }
  }
  return $x;
}


#
# bench03 (Integer)
# number of primes below n (Sieve of Eratosthenes)
# Example: n=500000 => x=41538 (expected), n=1000000 => x=78498
#
# We can use lset (old comment: I have not found bit arrays in Tcl so I used lists, but it is very slow, if we need to
# replace elements with lreplace.)
# Strings could also be used but are even slower:
# - init sieve: set sieve1 [string repeat 1 [expr {$n + 1}]];
#               set sieve1 [string replace $sieve1 0 0 0]; set sieve1 [string replace $sieve1 1 1 0];
# - res bit:    set sieve1 [string replace $sieve1 $j $j 0];
# - test bit:   [string index $sieve1 $i]
#
proc bench03 {n} {
  #set n [expr {$n / 2}]; # compute only up to n/2

  set nHalf [expr {$n / 2}]; 

  # initialize sieve
  set sieve1 {0 0}; # set first 2 elements
  for {set i 2} {$i <= $nHalf} {incr i} {
    lappend sieve1 0;
  }
  # compute primes
  set i 0; 
  set m 3; 
  set x 1; # number of primes below n (2 is prime)
  while {[expr {$m * $m}] <= $n} {
    if {![lindex $sieve1 $i]} {
      incr x; # m is prime
      set j [expr {($m * $m - 3) >> 1}];
      while {$j < $nHalf} {
        lset sieve1 $j 1;
        #puts "DEBUG: res (j=$j): sieve=$sieve1";
        incr j $m; #set j [expr {$j + $m}];
      }
    }
    incr i;
    incr m 2;
  }

  # count remaining primes
  while {$m <= $n} {
    if {![lindex $sieve1 $i]} {
      incr x;
    }
    incr i;
    incr m 2;
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
proc bench04 {n} {
  set m 2147483647; # modulus, do not change!
  set a 16807;      # multiplier
  set q 127773;     # m div a
  set r 2836;       # m mod a
  set x 1;          # last random value
  for {set i $n} {$i > 0} {incr i -1} {
    set x [expr {$a * ($x % $q) - $r * ($x / $q)}]; # x div q (using additional variables xDivQ, xModQ and multiple expressions would be slower)
    if {$x <= 0} {
      incr x $m; # x is new random number
    }
  }
  return $x;
}

#set gState(benchxxLine) {}; 

# bench05 (Integer 32 bit)
# (n choose n/2) mod 65536 (Central Binomial Coefficient mod 65536)
# Using dynamic programming and Pascal's triangle, storing only one line
# Instead of nCk mod 65536 with k=n/2, we compute the product of (n/2)Ck mod 65536 with k=0..n/4 (Vandermonde folding)
# Example: (2000 choose 1000) mod 65536 = 27200
#
# (We use lset, which is probably as good as lappend, which is much faster than lreplace)
#
proc bench05 {n} {
  # Instead of nCk with k=n/2, we compute the product of (n/2)Ck with k=0..n/4
  set n [expr {$n / 2}];

  set k [expr {$n / 2}];
  if { (($n - $k) < $k) } {
    set k [expr {$n - $k}]; # keep k minimal with  n over k  =  n over n-k
  }

  # initialize (not needed)
  #for {set j 0} {$j < $k} {incr j} {
  #  lset line $j 0;
  #}
  
  set line {};
  lset line 0 1;
  if {$k >= 1} {
    lset line 1 2;
  }

  for {set i 3} {$i <= $n} {incr i} {
    set min1 [expr {($i - 1) / 2}];

    if { [expr {($i % 2 == 0)}] } { # new element
      set min1_plus1 $min1;
      incr min1_plus1;
      lset line $min1_plus1 [expr {2 * [lindex $line $min1]}];
    }

    set prev [lindex $line 1];
    for {set j 2} {$j <= $min1} {incr j} {
      set num [lindex $line $j];
      lset line $j [expr {($prev + [lindex $line $j]) & 0xffff}];
      set prev $num;
    }
    lset line 1 $i;
    #puts "DEBUG: line=$line";
  }

  # compute sum of ((n/2)Ck)^2 mod 65536 for k=0..n/2
  set x 0;
  for {set j 0} {$j < $k} {incr j} {
    set num [lindex $line $j];
    set x [expr {($x + 2 * $num * $num) & 0xffff}]; 
  }

  set num [lindex $line $k];
  set x [expr {($x + $num * $num) & 0xffff}]; 

  return [expr {$x & 0xffff}];
}

proc bench06 {n} {
  set sum 0.0; # force floating point
  set flip 1.0;
  for {set i 1} {$i <= $n} {incr i} {
    set sum [expr { $sum + $flip / (2 * $i - 1)}];
    set flip [expr { $flip * -1.0}];
  }
  return [expr {int(($sum * 4.0) * 100000000)}];
}

set gState(benchList) {bench00 bench01 bench02 bench03 bench04 bench05 bench06};

#
# run a benchmark
# in: bench = benchmark to use
#     loops = number of loops
#         n = maximum number (used in some benchmarks to define size of workload)
# out:    x = result
#
proc run_bench {bench loops n check} {
  global gState;

  if {$bench >= [llength $gState(benchList)]} {
    puts "Error: Unknown benchmark: $bench";
  }

  set bench_func [lindex $gState(benchList) $bench];

  set x 0;
  while {$loops > 0 && $x == 0} {
    incr loops -1;
    set x [$bench_func $n];
    incr x -$check;
  }

  incr x $check;
  if {$x != $check} {
    puts "Error(bench $bench): x=$x";
    # error "Error in benchmark.";
    set x -1;
  }
  return $x;
}


proc bench03Check {n} {
  set x 0;

  if {$n == 500000} {
    set x 41538;
  } else {
    set x 1;
    for {set j 3} {$j <= $n} {incr j 2} {
      set isPrime 1;
      for {set i 3} {[expr {$i * $i}] <= $j} {incr i 2} {
        if {![expr {$j % $i}]} {
          set isPrime 0;
          break;
        }
      }
      if {$isPrime} {
        incr x;
      }
    }
  }
  return $x;
}

proc getCheck {bench n} {
  set check 0;

  if {$bench == 0} { # ($n / 2) * ($n + 1)
    set check [expr {((($n + ($n & 1)) >> 1) * ($n + 1 - ($n & 1))) & 0xffff}]; # 10528 for n=1000000

  } elseif {$bench == 1} {
    set check [expr {($n + 1) / 2}];

  } elseif {$bench == 2} {
    set check [expr {($n + 1) / 2}];

  } elseif {$bench == 3} {
    set check [bench03Check $n];

  } elseif {$bench == 4} {
    set check [expr {$n == 1000000 ? 1227283347 : [bench04 $n]}]; # bench04 not a real check

  } elseif {$bench == 5} {
    set check [expr {$n == 5000 ? 17376 : [bench05 $n]}]; # bench05 not a real check

  } elseif {$bench == 6} {
    set check [expr {$n == 1000000 ? 314159165 : [bench06 $n]}]; # bench06 not a real check

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


proc get_info {} {
  global PRG_VERSION;
  global PRG_LANGUAGE;
  global gState;
  set str "BM Bench v$PRG_VERSION ($PRG_LANGUAGE) -- (int:[checkbits_int1] double:[checkbits_double1] tsType:$gState(tsType) tsMs:$gState(tsPrecMs) tsCnt:$gState(tsPrecCnt)) Tcl [info tclversion] patchlevel [info patchlevel]; library: [info library]; hostname: [info hostname]";
  set str "$str\n(c) Marco Vieth, 2002-2023\nDate: [clock format [clock seconds] -format {%Y-%m-%d %H:%M:%S}]";
  return $str;
}


proc print_results {bench_res} {
  global PRG_LANGUAGE;
  set MAX_LANGUAGE_LEN1 10;
  puts "\nThroughput for all benchmarks (loops per sec):";

  set str_len [expr { $MAX_LANGUAGE_LEN1 - [string length $PRG_LANGUAGE] }]; # possible for Tcl 8.x: string repeat " " $str_len
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

  set deltaMs 100; # const
  set maxMs 10000; # const
  set caliMs $gState(caliMs);

  set loops 1; # number of loops
  set x 0;     # result from benchmark
  set tMeas 0;    # measured time
  set tEsti 0;    # estimated time
  set throughput 0;

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

  return $throughput;
}


proc start_bench {bench1 bench2 n argStr} {
  puts [get_info];
  if {$argStr != ""} {
    puts "Args: $argStr";
  }

  set bench_res [list];
  for {set bench $bench1} {$bench <= $bench2} {incr bench} {
    set n2 $n;
    if {$bench == 3} {
		  set n2 [expr {$n / 2}];
	  } elseif {$bench == 5} {
		  set n2 [expr {$n / 200}]; 
	  }

    set check [getCheck $bench $n2];
    if {$check > 0} {
      set throughput [measureBench $bench $n2 $check];
    } else {
      set throughput -1;
    }
    lappend bench_res $throughput;
  }
  print_results $bench_res;
  return 0;  
}


proc main {argc argv} {
  global gState;
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
  if {$argc > 3} {
    set gState(caliMs) [lindex $argv 3];
  }

  determineTsPrecision;
  set argStr [concat $argv];

  set rc [start_bench $bench1 $bench2 $n $argStr];
  set t1 [expr {[get_ms] - $start_t}];
  puts "Total elapsed time: $t1 ms";
  return rc;
}

if {![info exists argc]} { # no argc?
  set argc 0;
  set argv {};
}

main $argc $argv;

#
# https://replit.com/languages/tcl
#

# end
