#!/usr/bin/perl -w
# BM Bench - bmbench.pl (Perl 5)
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
# perl bmbench.pl [bench1] [bench2] [n]
#
#

use strict;

my $PRG_VERSION = "0.06";
my $PRG_LANGUAGE = "Perl";


$::TimeHiResFunc = undef();


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



#
# bench00 (Integer 16 bit)
# (sum of 1..n) mod 65536
#
sub bench00($$) {
  use integer; # it is possible to use integer arithmetic
  my($loops, $n) = @_;
  my $x = 0;
  my $sum1 = (($n / 2) * ($n + 1)) & 0xffff; # assuming n even!
  # sum1..1000000 depends on type: 500000500000 (floating point), 1784293664 (32bit), 10528 (16 bit)
  my $n_div_65536 = ($n >> 16) & 0xffff;
  my $n_mod_65536 = $n & 0xffff;
  #print "DEBUG: n=$n, sum1=$sum1, n_div_65536=$n_div_65536, n_mod_65536=$n_mod_65536\n";
  while ($loops-- > 0) {
    # simulate summation with 16 bit borders...
    for (my $i = $n_div_65536; $i > 0; $i--) {
      for (my $j = 65535; $j > 0; $j--) {
        $x += $j;
        #$x &= 0xffff;
      }
    }
    for (my $j = $n_mod_65536; $j > 0; $j--) {
      $x += $j;
      #$x &= 0xffff;
    }
    $x &= 0xffff;
    #print "DEBUG: x0=$x, x-sum1=". ($x - $sum1) ."\n";
    if ($loops > 0) {  # some more loops left?
      #print "DEBUG: x=$x, x-sum1=". ($x - $sum1) ."\n";
      $x -= $sum1;     # yes, set x back to 0 (assuming n even)
      if ($x != 0) {   # now x must be 0 again
        $x++;          # force error for many wrong computations
        last;          # Error
      }
    }
  }
  return ($x & 0xffff);
}


#
# bench01 (Integer 16/32 bit)
# (sum of 1..n) mod 65536
#
sub bench01($$) {
  use integer; # it is possible to use integer arithmetic
  my($loops, $n) = @_;
  my $x = 0;
  my $sum1 = ($n / 2) * ($n + 1); # assuming n even! (32 bit sum should be 1784293664)
  # (sum1..1000000 depends on type: 500000500000 (floating point), 1784293664 (32bit), 10528 (16 bit)
  while ($loops-- > 0) {
    for (my $i = $n; $i > 0; $i--) {
      $x += $i;
    }
    if ($loops > 0) {  # some more loops left?
      $x -= $sum1;     # yes, set x back to 0 (assuming n even)
      if ($x != 0) {   # now x must be 0 again
        $x++;          # force error for many wrong computations
        last;          # Error
      }
    }
  }
  return ($x & 0xffff);
}


#
# bench02 (Floating Point, normally 64 bit)
# (sum of 1..n) mod 65536
#
sub bench02($$) {
  my($loops, $n) = @_;
  my $x = 0;
  my $sum1 = ($n / 2) * ($n + 1); # assuming n even! (32 bit sum should be 1784293664)
  # (sum1..1000000 depends on type: 500000500000 (floating point), 1784293664 (32bit), 10528 (16 bit)
  while ($loops-- > 0) {
    for (my $i = $n; $i > 0; $i--) {
      $x += $i;
    }
    if ($loops > 0) {  # some more loops left?
      $x -= $sum1;     # yes, set x back to 0 (assuming n even)
      if ($x != 0) {   # now x must be 0 again
        $x++;          # force error for many wrong computations
        last;          # Error
      }
    }
  }
  return ($x - int($x / 65536) * 65536); # return ($x % 65536);
  # some Perl versions (e.g. 5.00503) seem to have a bug with modulus so use explicit computation...
}


#
# bench03 (Integer)
# number of primes below n (Sieve of Eratosthenes)
# Example: n=500000 => x=41538 (expected), n=1000000 => x=78498
#
sub bench03($$) {
  use integer; # it is possible to use integer arithmetic
  my($loops, $n) = @_;
  $n /= 2;    # compute only up to n/2 
  my $x = 0; # number of primes below n
  my $sieve = 0;
  vec($sieve, 0, 1) = 0;
  vec($sieve, 1, 1) = 0;
  while ($loops-- > 0) {
    # initialize sieve
    for (my $i = 2; $i <= $n; $i++) {
      vec($sieve, $i, 1) = 1;
    }
    # this init does not work:
    # my $sieve = chr(0xff) x (($n / 16) + 1); vec($sieve, 0, 1) = 0; vec($sieve, 1, 1) = 0;

    # compute primes
    for (my $i = 2; ($i * $i) <= $n; $i++) {
      if (vec($sieve, $i, 1)) {
        for (my $j = $i * $i; $j <= $n; $j += $i) {
          vec($sieve, $j, 1) = 0;
        }
      }
    }
    # count primes
    for (my $i = 0; $i <= $n; $i++) {
      if (vec($sieve, $i, 1)) {
        $x++;
      }
    }
    # check prime count
    if ($loops > 0) {  # some more loops left?
      $x -= 41538;     # yes, set x back to 0 (number of primes below 1000000)
      if ($x != 0) {   # now x must be 0 again
        $x++;          # force error for many wrong computations
        last;          # Error
      }
    }
  }
  return($x);
}



#
# bench04 (Integer 32 bit)
# nth random number number
# Random number generator taken from
# Raj Jain: The Art of Computer Systems Performance Analysis, John Wiley & Sons, 1991, page 442-444.
# It needs longs with at least 32 bit.
# Starting with x0=1, x10000 should be 1043618065, x1000000 = 1227283347.
#
sub BENCH04_M() { 2147483647; } # modulus, do not change!
sub BENCH04_A() { 16807; }      # multiplier
sub BENCH04_Q() { 127773; }     # m div a
sub BENCH04_R() { 2836; }       # m mod a
sub bench04($$) {
  use integer; # it is possible to use integer arithmetic
  my($loops, $n) = @_;
  my $x = 1; # last random value
  while ($loops-- > 0) {
    for (my $i = 1; $i <= $n; $i++) {
      my $x_div_q = $x / BENCH04_Q();  # int(...)
      my $x_mod_q = $x - BENCH04_Q() * $x_div_q;
      $x = BENCH04_A() * $x_mod_q - BENCH04_R() * $x_div_q;
      if ($x <= 0) {
        $x += BENCH04_M(); # x is new random number
      }
    }
    if ($loops > 0) {
      $x -= 1227283347;
      if ($x != 0) {   # now x must be 0 again
        $x++;
        last;          # Error
      }
      $x++; # start with 1 again
    }
  }
  return $x;
}


#
# bench05 (Integer 32 bit) (list implementation)
# n over n/2 mod 65536 (Pascal's triangle)
# (we just need to store the last 2 lines of computation)
#
sub bench05($$) {
  use integer; # we need integer arithmetic
  my($loops, $n) = @_;
  my $x = 0;
  $n = int($n / 500);
  my $k = int($n / 2);

  if (($n - $k) < $k) {
    $k = $n - $k; # keep k minimal with  n over k  =  n over n-k
  }
  my @pas1 = ();
  my @pas2 = ();

  while ($loops-- > 0) {
    @pas1 = (1);
    for (my $i = 2; $i <= $n; $i++) {
      @pas2 = @pas1; # get last line to pas2
      @pas1 = (1); # and restart with new list
      my $min1 = ($i - 1) / 2; # int(...)
      if ($k < $min1) {
        $min1 = $k;
      }
      push(@pas1, $i); # second column is i
      for (my $j = 2; $j <= $min1; $j++) { # up to min((i-1)/2, k)
        push(@pas1, $pas2[$j - 1] + $pas2[$j]);
      }
      if (($min1 < $k) && (($i % 2) == 0)) { # new element
        push(@pas1, 2 * $pas2[$min1]);
      }
    }
    $x += $pas1[$k] & 0xffff; # % 65536
    if ($loops > 0) {
      $x -= 27200;
      if ($x != 0) {   # now x must be 0 again
        $x++;
        last;          # Error
      }
    }
  }
  return $x;
}


=for nobody
#
# bench05 (Integer 32 bit) (Array implementation (is slower))
# n over n/2 mod 65536 (Pascal's triangle)
# (we just need to store the last 2 lines of computation)
#
sub bench05_array_ok1($$) {
  use integer; # we need integer arithmetic
  my($loops, $n) = @_;
  my $x = 0;
  $n = int($n / 500);
  my $k = int($n / 2);

  if (($n - $k) < $k) {
    $k = $n - $k; # keep k minimal with  n over k  =  n over n-k
  }
  my @pas1;

  $pas1[0][0] = 1; $pas1[1][0] = 1; # set first column

  while ($loops-- > 0) {
    for (my $i = 2; $i <= $n; $i++) {
      my $i_mod_2 = $i % 2;
      my $min1 = ($i - 1) / 2; # int(...)
      if ($k < $min1) {
        $min1 = $k;
      }
      $pas1[$i_mod_2][1] = $i; # second column is i
      for (my $j = 2; $j <= $min1; $j++) { # up to min((i-1)/2, k)
        $pas1[$i_mod_2][$j] = ($pas1[$i_mod_2 ^ 1][$j - 1] + $pas1[$i_mod_2 ^ 1][$j]);
      }
      if (($min1 < $k) && ($i_mod_2 == 0)) { # new element
        $pas1[$i_mod_2][$min1 + 1] = 2 * $pas1[$i_mod_2 ^ 1][$min1];
      }
    }
    $x += $pas1[$n % 2][$k] & 0xffff; # % 65536
    if ($loops > 0) {
      $x -= 27200;
      if ($x != 0) {   # now x must be 0 again
        $x++;
        last;          # Error
      }
    }
  }
  return $x;
}
#=begin comment text
#=end comment text
=cut


#
# run a benchmark
# in: bench = benchmark to use
#     loops = number of loops
#         n = maximum number (used in some benchmarks to define size of workload)
# out:    x = result
#
sub run_bench($$$) {
  my($bench, $loops, $n) = @_;
  my $x = 0;
  my $check1 = 0;
  if ($bench == 0) {
    $x = bench00($loops, $n);
    $check1 = 10528;

  } elsif ($bench == 1) {
    $x = bench01($loops, $n);
    $check1 = 10528;

  } elsif ($bench == 2) {
    $x = bench02($loops, $n);
    $check1 = 10528;

  } elsif ($bench == 3) {
    $x = bench03($loops, $n);
    $check1 = 41538;

  } elsif ($bench == 4) {
    $x = bench04($loops, $n);
    $check1 = 1227283347;

  } elsif ($bench == 5) {
    $x = bench05($loops, $n);
    $check1 = 27200;
  
  } else {
    print STDERR "Error: Unknown benchmark ", $bench, "\n";
    $check1 = $x + 1; # force error
  }

  if ($check1 != $x) {
    print STDERR "Error(bench", $bench, "): x=", $x, "\n";
    $x = -1; # exit
  }
  return($x);
}


#
# Initialize HiRes timer
# Set $::TimeHiResFunc to the timing function to use
# Time::HiRes::time, if available
# syscall(gettimeofday), if available
# otherwise time()
#
sub private_init_HiRes() {
  if (eval { require Time::HiRes; }) { # Time::HiRes::time() will be a float to 6 decimal places
    $::TimeHiResFunc = \&Time::HiRes::time;
  } elsif (eval { require 'syscall.ph'; }) {
    # ...otherwise try to use a syscall to gettimeofday, which will also return a float
    my $TIMEVAL_T = "QQ"; # for 64 bit Perl
    if (!eval { pack($TIMEVAL_T, ()); }) {
      $TIMEVAL_T = "LL"; # for 32 bit
    }
    $::TimeHiResFunc = sub {
      my $tval = pack($TIMEVAL_T, ());
      syscall(&SYS_gettimeofday, $tval, 0) != -1 or die "gettimeofday: $!";
      my @time1 = unpack($TIMEVAL_T, $tval);
      return $time1[0] + ($time1[1] / 1_000_000);
    }
  } else {
    # ...otherwise use time() to return an integral number of seconds.
    $::TimeHiResFunc = sub { time(); }; # is it possible to get a function pointer directly on time()?
  }
}


#
# get timestamp in milliseconds
# out: x = time in ms
#
# Maybe it would also be possible to use execution time:
# my($user, $system) = times(); return(($user+$system) * 1000);
#
sub get_ms() {
  if (!defined $::TimeHiResFunc) {
    private_init_HiRes();
  }
  return(&$::TimeHiResFunc() * 1000);
}


# Here we compute the number of "significant" bits for positive numbers (which means 53 for double)
sub checkbits_int1() {
  use integer; # we need integer
  my $num = 1;
  my $last_num = 0;
  my $bits = 0;
  do {
    $last_num = $num;
    $num *= 2;
    $num++;
    $bits++;
  } while ( ((($num - 1) / 2) == $last_num) && ($bits < 101) );
  return $bits;
}

sub checkbits_double1() {
  my $num = 1.0;
  my $last_num = 0.0;
  my $bits = 0;
  do {
    $last_num = $num;
    $num *= 2.0;
    $num++;
    $bits++;
  } while ( ((($num - 1.0) / 2.0) == $last_num) && ($bits < 101) );
  return $bits;
}


sub print_info() {
  my $perl_version = $];
  $perl_version =~ tr/\n/;/;
  print("BM Bench v", $PRG_VERSION, " (", $PRG_LANGUAGE, ") -- (int:", checkbits_int1(), " double:", checkbits_double1(), ") $perl_version, osname: $^O\n");
  print("(c) Marco Vieth, 2006\n");
  print("Date: ". localtime(time()) ."\n");
  #system("uname -a");        
}


sub print_results($$$) {
  my($bench1, $bench2, $bench_res1_r) = @_;
  my $max_language_len1 = 10;
  
  print("\nThroughput for all benchmarks (loops per sec):\n");
  print "BMR (", $PRG_LANGUAGE .")". (' ' x ($max_language_len1 - length($PRG_LANGUAGE))), ": ";
  for (my $bench = $bench1; $bench <= $bench2; $bench++) {
    printf("%9.2f ", $bench_res1_r->[$bench]);
  }
  print "\n";
  print "\n";
}


sub start_bench($$$) {
  my($bench1, $bench2, $n) = @_;
  my $cali_ms = 1001; # const
  my $delta_ms = 100; # const
  my $max_ms = 10000; # const

  print_info();

  my @bench_res1 = ();
  for (my $bench = $bench1; $bench <= $bench2; $bench++) {
    my $loops = 1; # number of loops
    my $x = 0;     # result from benchmark
    my $t1 = 0;    # measured time
    my $t2 = 0;    # estimated time

    printf("Calibrating benchmark %d with n=%d\n", $bench, $n);
    while (1) {
      $t1 = get_ms();
      $x = run_bench($bench, $loops, $n);
      $t1 = get_ms() - $t1;

      my $t_delta = ($t2 > $t1) ? ($t2 - $t1) : ($t1 - $t2); # compute difference abs(measures-estimated)
      my $loops_p_sec = ($t1 > 0) ? ($loops * 1000.0 / $t1) : 0;
      printf("%10.3f/s (time=%5ld ms, loops=%7d, delta=%5d ms, x=%d)\n", $loops_p_sec, $t1, $loops, $t_delta, $x);
      if ($x == -1) { # some error?
        $bench_res1[$bench] = -1;
        last; # (can only exit while, if not in sub block)
      }
      if ($t2 > 0) { # do we have some estimated/expected time? 
        if ($t_delta < $delta_ms) { # smaller than delta_ms=100? 
          $bench_res1[$bench] = $loops_p_sec; # set loops per sec
          printf("Benchmark %d (%s): %.3f/s (time=%ld ms, loops=%d, delta=%d ms)\n", $bench, $PRG_LANGUAGE, $bench_res1[$bench], $t1, $loops, $t_delta);
          last;
        }
      }

      if ($t1 > $max_ms) {
        printf("Benchmark %d (%s): Time already > %d ms. No measurement possible.\n", $bench, $PRG_LANGUAGE, $max_ms);
        $bench_res1[$bench] = -1;
        last;
      }
      {
        my $scale_fact = (($t1 < $cali_ms) && ($t1 > 0)) ? int((($cali_ms + 100) / $t1) + 1) : 2;
          # scale a bit up to 1100 ms (cali_ms+100)
        $loops *= $scale_fact;
        $t2 = $t1 * $scale_fact;
      }
    }
  }

  print_results($bench1, $bench2, \@bench_res1);
  return 0;
}



sub main($) {
  #my(@ARGV) = @_;
  my $start_t = get_ms(); # memorize start time
  my $bench1 = 0;     # first benchmark to test
  my $bench2 = 5;     # last benchmark to test
  my $n = 1000000;    # maximum number

  if ($#ARGV > -1) {
    $bench1 = $ARGV[0];
    $bench2 = $bench1;
  }
  if ($#ARGV > 0) {
    $bench2 = $ARGV[1];
  }

  if ($#ARGV > 1) {
    $n = $ARGV[2];
  }
  
  my $rc = start_bench($bench1, $bench2, $n);
  
  printf("Total elapsed time: %d ms\n", get_ms() - $start_t);
  return $rc;
}


main(@ARGV);

__END__
# end
