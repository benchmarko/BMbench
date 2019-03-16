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


set PRG_VERSION "0.06";
set PRG_LANGUAGE "Tcl";




  #
  # bench00 (Integer 16 bit)
  # (sum of 1..n) mod 65536
  #
  # (Tcl tries to compute with C longs as long as no floating point number is introdced.)
  proc bench00 {loops n} {
    set x 0;
    set sum1 [expr {(($n / 2) * ($n + 1)) & 0xffff}];
    set n_div_65536 [expr {($n >> 16) & 0xffff}];
    set n_mod_65536 [expr {$n & 0xffff}];
    #puts "DEBUG: sum1=$sum1, n_div=$n_div_65536, n_mod=$n_mod_65536";
    while {$loops > 0} {
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
      if {$loops > 0} { # some more loops left?
        incr x -$sum1;  #set x [expr {$x - $sum1}]; # yes, set x back to 0 (assuming n even)
        if {$x != 0} {  # now x must be 0 again
          incr x;       # force error for many wrong computations
          break; #error
        }
      }
    }
    return [expr {$x & 0xffff}];
  }


  #
  # bench01 (Integer 16/32 bit)
  # (sum of 1..n) mod 65536
  #
  # (Tcl tries to compute with C longs as long as no floating point number is introdced.)
  proc bench01 {loops n} {
    set x 0;
    set sum1 [expr {($n / 2) * ($n + 1)}]; # sum1=1784293664
    while {$loops > 0} {
      incr loops -1;
      for {set i $n} {$i > 0} {incr i -1} {
        incr x $i; # a bit faster than "set x [expr {$x + $i}]"
      }
      if {$loops > 0} { # some more loops left?
        incr x -$sum1;  #set x [expr {$x - $sum1}]; # yes, set x back to 0 (assuming n even)
        if {$x != 0} {  # now x must be 0 again
          incr x;       # force error for many wrong computations
          break; #error
        }
      }
    }
    return [expr {$x & 0xffff}];
  }


  #
  # bench02 (Floating Point, normally 64 bit)
  # (sum of 1..n) mod 65536
  #
  proc bench02 {loops n} {
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
  # replace elements witl lreplace.
  # Strings could also be used but are even slower:
  # - init sieve: set sieve1 [string repeat 1 [expr {$n + 1}]];
  #               set sieve1 [string replace $sieve1 0 0 0]; set sieve1 [string replace $sieve1 1 1 0];
  # - res bit:    set sieve1 [string replace $sieve1 $j $j 0];
  # - test bit:   [string index $sieve1 $i]
  #
  proc bench03 {loops n} {
    set n [expr {$n / 2}]; # compute only up to n/2
    set x 0; # number of primes below n
    #set sieve1 [list 0 0]; # set first 2 elements

    set n [expr {$n / 40}]; # Tcl is slow, so use this factor
    puts "Important note: For Tcl we use 1/40 of the problem to get to an end (n=$n)...";

    while {$loops > 0} {
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
      # check prime count
      if {$loops > 0} {         # some more loops left?
        #set x [expr {$x - 41538}]; # yes, set x back to 0 (number of primes below 1000000/2)
        incr x -1492; # yes, set x back to 0
        if {$x != 0} {   # now x must be 0 again
          incr x;
          break;         # Error
        }
      }
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
  proc bench04 {loops n} {
    set m 2147483647; # modulus, do not change!
    set a 16807;      # multiplier
    set q 127773;     # m div a
    set r 2836;       # m mod a
    set x 1;          # last random value
    while {$loops > 0} {
      incr loops -1;
      for {set i $n} {$i > 0} {incr i -1} {
        set x_div_q [expr {$x / $q}];
        set x_mod_q [expr {$x - $q * $x_div_q}];
        set x [expr {$a * $x_mod_q - $r * $x_div_q}];
        if {$x <= 0} {
          incr x $m; # x is new random number
        }
      }
      if {$loops > 0} {
        incr x -1227283347;
        if {$x != 0} { # now x must be 0 again
          incr x;      # force error for many wrong computations
          break; # error
        }
        incr x; # start with 1 again
      }
    }
    return $x;
  }



  #
  # bench05 (Integer 32 bit) (list implementation)
  # n over n/2 mod 65536 (Pascal's triangle)
  # We start with an empty list and append elements, this will be much faster than replacing elements in a list with
  # e.g.  set pas1 [lreplace $pas1 $j $j [expr {[lindex $pas2 $j_1] + [lindex $pas2 $j]}]];
  #
  proc bench05 {loops n} {
    set x 0;
    set n [expr {$n / 500}];
    set k [expr {$n / 2}];

    if { (($n - $k) < $k) } {
      set k [expr {$n - $k}]; # keep k minimal with  n over k  =  n over n-k
    }
    
    # allocate memory... (not needed for Tcl)
    #int pas1[][] = new int[2][k + 1];
    #pas1[0][0] = 1; pas1[1][0] = 1; // set first column

    while {$loops > 0} {
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

      if {$loops > 0} {
        set x [expr {$x - 27200}];
        if {$x != 0} { # now x must be 0 again
          incr x;       # force error for many wrong computations
          break; #error
        }
      }
    }
    return $x;
  }


  #unused:
  proc bench05_slow_lreplace {loops n} {
    set x 0;
    set n [expr {$n / 500}];
    set k [expr {$n / 2}];

    # allocate memory...
    #int pas1[][] = new int[2][k + 1];
    #pas1[0][0] = 1; pas1[1][0] = 1; // set first column
    set pas1 {1}; # set first column
    #set p pas1; # for testing
    for {set i 1} {$i <= $k} {incr i} {
      lappend pas1 0;
    }
    set pas2 $pas1;
    #puts "DEBUG: pas1=$pas1, pas2=$pas2";

    while {$loops > 0} {
      incr loops -1;
      for {set i 2} {$i <= $n} {incr i} {
        set i_mod_2 [expr {$i % 2}];
        set min1 [expr {($i - 1) / 2}];
        if {$k < $min1} {
          set min1 $k;
        }

        #pas1[i_mod_2][1] = i; # second column is i
        if {$i_mod_2 == 0} {
          set pas1 [lreplace $pas1 1 1 $i];
          #puts "DEBUG -- (i=$i): pas1=$pas1, pas2=$pas2";
          for {set j 2} {$j <= $min1} {incr j} { # up to min((i-1)/2, k)
            #pas1[i_mod_2][j] = (pas1[i_mod_2 ^ 1][j - 1] + pas1[i_mod_2 ^ 1][j]);
            set j_1 [expr {$j - 1}];
            set pas1 [lreplace $pas1 $j $j [expr {[lindex $pas2 $j_1] + [lindex $pas2 $j]}]];
            #puts "DEBUG (j=$j): pas1=$pas1, pas2=$pas2";
          }
          if { [expr {($min1 < $k) && ($i_mod_2 == 0)}] } { # new element
            #pas1[i_mod_2][i / 2] = 2 * pas1[i_mod_2 ^ 1][(i - 1) / 2];
            set i_2 [expr {$i / 2}];
            set pas1 [lreplace $pas1 $i_2 $i_2 [expr {2 * [lindex $pas2 [expr {($i - 1) / 2}]]}]];
          }
        } else { # same for i odd (other list)
          set pas2 [lreplace $pas2 1 1 $i];
          for {set j 2} {$j <= $min1} {incr j} { # up to min((i-1)/2, k)
            set j_1 [expr {$j - 1}];
            set pas2 [lreplace $pas2 $j $j [expr {[lindex $pas1 $j_1] + [lindex $pas1 $j]}]];
          }
          if { [expr {($min1 < $k) && ($i_mod_2 == 0)}] } { # new element
            set i_2 [expr {$i / 2}];
            set pas2 [lreplace $pas2 $i_2 $i_2 [expr {2 * [lindex $pas1 [expr {($i - 1) / 2}]]}]];
          }
        }
      }

      #puts "DEBUG: pas1=$pas1, pas2=$pas2";

      #x += pas1[n % 2][k] & 0xffff; // % 65536
      set n_mod_2 [expr {$n % 2}];
      if {$n_mod_2 == 0} {
        set x [expr {[lindex $pas1 $k] & 0xffff}];
      } else {
        set x [expr {[lindex $pas2 $k] & 0xffff}];
      }

      if {$loops > 0} {
        set x [expr {$x - 27200}];
        if {$x != 0} { # now x must be 0 again
          incr x;       # force error for many wrong computations
          break; #error
        }
      }
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
proc run_bench {bench loops n} {
  set x 0;
  set check1 0;

  if {$bench == 0} {
    set x [bench00 $loops $n];
    set check1 10528;

  } elseif {$bench == 1} {
    set x [bench01 $loops $n];
    set check1 10528;

  } elseif {$bench == 2} {
    set x [bench02 $loops $n];
    set check1 10528;

  } elseif {$bench == 3} {
    set x [bench03 $loops $n];
    set check1 1492;
    #set check1 41538; # for the full problem

  } elseif {$bench == 4} {
    set x [bench04 $loops $n];
    set check1 1227283347;

  } elseif {$bench == 5} {
    set x [bench05 $loops $n];
    set check1 27200;

  } else {
    puts "Error: Unknown benchmark: $bench";
    set check1 $x;
    incr check1; # force error
  }
# can we use Tcl switch?
  if {$check1 != $x} {
    puts "Error(bench $bench): x=$x";
    # error "Error in benchmark.";
    set x -1;
  }
  return $x;
}



#
# get timestamp in milliseconds
# out: x = time in ms
#
# Maybe it would also be possible to use execution time:
# my($user, $system) = times(); return(($user+$system) * 1000);
#
proc get_ms {} {
  clock clicks -milliseconds
}

# Try millisecond timer.
# If Tcl version does not support it, use timer with seconds...
if {[catch get_ms]} {
  proc get_ms {} {expr {[clock seconds] * 1000}}
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
  puts "BM Bench v$PRG_VERSION ($PRG_LANGUAGE) -- (int:[checkbits_int1] double:[checkbits_double1]) Tcl [info tclversion] patchlevel [info patchlevel]; library: [info library]; hostname: [info hostname]";
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
    puts -nonewline [format "%9.2f " $t1];
  }
  puts "";
  puts "";
}


proc start_bench {bench1 bench2 n} {
  global PRG_LANGUAGE;
  set cali_ms 1001; # const
  set delta_ms 100; # const
  set max_ms 10000; # const

  print_info;

  set bench_res1 [list ];
  for {set bench $bench1} {$bench <= $bench2} {incr bench} {
    set loops 1; # number of loops
    set x 0;     # result from benchmark
    set t1 0;    # measured time
    set t2 0;    # estimated time

    puts "Calibrating benchmark $bench with n=$n";
    while {1} {
      set t1 [get_ms];
      set x [run_bench $bench $loops $n];
      set t1 [expr {[get_ms] - $t1}];

      set t_delta [expr {($t2 > $t1) ? ($t2 - $t1) : ($t1 - $t2)}]; # compute difference abs(measures-estimated)
      set loops_p_sec [expr {($t1 > 0) ? ($loops * 1000.0 / $t1) : 0}];

      puts "[format %10.3f $loops_p_sec]/s (time= [format %5d $t1] ms, loops= [format %7d $loops], delta=[format %5d $t_delta] ms, x=$x)";
      if {$x == -1} { # some error?
        #$bench_res1[$bench] = -1;
        lappend bench_res1 -1;
        break; # exit while
      }
      if {$t2 > 0} { # do we have some estimated/expected time? 
        if {$t_delta < $delta_ms} { # smaller than delta_ms=100?
          #$bench_res1[$bench] = $loops_p_sec; # set loops per sec
          lappend bench_res1 $loops_p_sec;
          puts "Benchmark $bench ($PRG_LANGUAGE): [format %.3f $loops_p_sec]/s (time=$t1 ms, loops=$loops, delta=$t_delta ms)";
          break;
        }
      }

      if {$t1 > $max_ms} {
        puts "Benchmark  $bench ($PRG_LANGUAGE): Time already > $max_ms ms. No measurement possible.";
        #$bench_res1[$bench] = -1;
        lappend bench_res1 -1;
        break;
      }

      set scale_fact [expr {(($t1 < $cali_ms) && ($t1 > 0)) ? int((($cali_ms + 100) / $t1) + 1) : 2}];
        # scale a bit up to 1100 ms (cali_ms+100)
      set loops [expr {$loops * $scale_fact}];
      set t2 [expr {$t1 * $scale_fact}];
    }
  }

  print_results $bench_res1;

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

  set rc [start_bench $bench1 $bench2 $n];
  set t1 [expr {[get_ms] - $start_t}];
  puts "Total elapsed time: $t1 ms";
  return rc;
}



main $argc $argv;

# end
