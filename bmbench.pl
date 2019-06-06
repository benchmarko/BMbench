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

my $G_PRG_VERSION = "0.07";
my $G_PRG_LANGUAGE = "Perl";

my $g_allowHiRes = 1; #1; # can be switched off for testing time with second resolution
my $g_TimeHiResFunc = undef();
my $g_tsType = ''; # type of time stamp source
my $g_startTs = 0;
my $g_tsPrecMs = 0; # measured time stamp precision
my $g_tsPrecCnt = 0; # time stamp count (calls) per precision interval (until time change)
my $g_tsMeasCnt = 0; # last measured count

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
sub bench00($$$) {
  use integer; # it is possible to use integer arithmetic
  my($loops, $n, $check) = @_;
  my $x = 0;
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
    $x -= $check;
  }
  return $x & 0xffff;
}


#
# bench01 (Integer 32 bit)
# (average of 1..n)
#
sub bench01($$$) {
  use integer; # it is possible to use integer arithmetic
  my($loops, $n, $check) = @_;
  my $x = 0;
  while ($loops-- > 0 && $x == 0) {
    my $sum = 0;
    for (my $i = 1; $i <= $n; $i++) {
      $sum += $i;
			if ($sum >= $n) { # to avoid numbers above 2*n, divide by n using subtraction
				$sum -= $n;
				$x++;
			}
    }
    $x -= $check;
  }
  return $x;
}


#
# bench02 (Floating Point, normally 64 bit)
# (average of 1..n)
#
sub bench02($$$) {
  my($loops, $n, $check) = @_;
  my $x = 0;
  while ($loops-- > 0 && $x == 0) {
    my $sum = 0.0;
    for (my $i = 1; $i <= $n; $i++) {
      $sum += $i;
			if ($sum >= $n) { # to avoid numbers above 2*n, divide by n using subtraction
				$sum -= $n;
				$x++;
			}
    }
    $x -= $check;
  }
  return $x;
}


#
# bench03 (Integer)
# number of primes below n (Sieve of Eratosthenes)
# Example: n=500000 => x=41538 (expected), n=1000000 => x=78498
# (We could use bit vector vec($s, 0, 1), but it is slower than array access.)
#
sub bench03($$$) {
  use integer; # it is possible to use integer arithmetic
  my($loops, $n, $check) = @_;
  $n /= 2; # compute only up to n/2 
  my $nHalf = $n >> 1; # div 2
  my $x = 0; # number of primes below n
  my $m;
  my $i;
  my $j;
  my @sieve = ();

  while ($loops-- > 0 && $x == 0) {
    # initialize sieve
    for ($i = 0; $i <= $nHalf; $i++) {
      $sieve[$i] = 0; # odd numbers are possible primes
    }
    # compute primes
    $i = 0;
		$m = 3;
		while ($m * $m < $n) {
      if (!$sieve[$i]) {
        $x++;
				$j = ($m * $m - 3) >> 1; # div 2
				while ($j < $nHalf) {
          $sieve[$j] = 1;
					$j += $m;
				}
			}
			$i++;
			$m += 2; # or: =2 * $i + 3;
    }

    # count remaining primes
    while ($i <= $nHalf) {
      if (!$sieve[$i]) {
        $x++;
      }
      $i++;
    }
    $x -= $check;
  }
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
sub BENCH04_M() { 2147483647; } # modulus, do not change!
sub BENCH04_A() { 16807; }      # multiplier
sub BENCH04_Q() { 127773; }     # m div a
sub BENCH04_R() { 2836; }       # m mod a

sub bench04($$$) {
  use integer; # it is possible to use integer arithmetic
  my($loops, $n, $check) = @_;
  my $x = 0; # last random value
  while ($loops-- > 0) {
		$x++; # start with 1=last random value
    for (my $i = 1; $i <= $n; $i++) {
      $x = BENCH04_A() * ($x % BENCH04_Q()) - BENCH04_R() * (($x / BENCH04_Q()) | 0);
      if ($x <= 0) {
        $x += BENCH04_M();
      }

    }
    $x -= $check;
  }
  return $x;
}


#
# bench05 (Integer 32 bit) (list implementation)
# n over n/2 mod 65536 (Pascal's triangle)
# (we just need to store the last 2 lines of computation)
#
sub bench05($$$) {
  use integer; # we need integer arithmetic
  my($loops, $n, $check) = @_;
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
    $x -= $check;
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
sub run_bench($$$) {
  my($bench, $loops, $n) = @_;
  my $x = 0;
  my $check = 0;
  if ($bench == 0) {
    $check = (($n / 2) * ($n + 1)) & 0xffff; #10528
    $x = bench00($loops, $n, $check);

  } elsif ($bench == 1) {
    $check = int(($n + 1) / 2);
    $x = bench01($loops, $n, $check);

  } elsif ($bench == 2) {
    $check = int(($n + 1) / 2);
    $x = bench02($loops, $n, $check);

  } elsif ($bench == 3) {
    $check = 41538;
    $x = bench03($loops, $n, $check);

  } elsif ($bench == 4) {
    $check = 1227283347;
    $x = bench04($loops, $n, $check);

  } elsif ($bench == 5) {
    $check = 27200;
    $x = bench05($loops, $n, $check);
  
  } else {
    print STDERR "Error: Unknown benchmark ", $bench, "\n";
    $check = -1;
  }

  $x += $check;
  if ($check != $x) {
    print STDERR "Error(bench", $bench, "): x=", $x, "\n";
    $x = -1; # exit
  }
  return $x;
}


#
# Initialize HiRes timer
# Set $g_TimeHiResFunc to the timing function to use
# Time::HiRes::time, if available
# syscall(gettimeofday), if available
# otherwise time()
#
sub private_init_HiRes() {
  if ($g_allowHiRes && eval { require Time::HiRes; }) { # Time::HiRes::time() will be a float to 6 decimal places
    $g_TimeHiResFunc = \&Time::HiRes::time;
    $g_tsType = 'HiRes';
  } elsif (eval { require 'syscall.ph'; }) {
    # ...otherwise try to use a syscall to gettimeofday, which will also return a float
    my $TIMEVAL_T = "QQ"; # for 64 bit Perl
    if (!eval { pack($TIMEVAL_T, ()); }) {
      $TIMEVAL_T = "LL"; # for 32 bit
    }
    $g_TimeHiResFunc = sub {
      my $tval = pack($TIMEVAL_T, ());
      syscall(&SYS_gettimeofday, $tval, 0) != -1 or die "gettimeofday: $!";
      my @time1 = unpack($TIMEVAL_T, $tval);
      return $time1[0] + ($time1[1] / 1_000_000);
    };
    $g_tsType = 'syscall';
  } else {
    # ...otherwise use time() to return an integral number of seconds.
    $g_TimeHiResFunc = sub { time(); }; # is it possible to get a function pointer directly on time()?
    $g_tsType = 'time';
  }
}


#
# get timestamp in milliseconds
# out: x = time in ms
#
# Maybe it would also be possible to use execution time:
# my($user, $system) = times(); return(($user+$system) * 1000);
#
sub get_raw_ts() {
  # call private_init_HiRes() first!
  return &$g_TimeHiResFunc();
}

sub get_ts() {
      return get_raw_ts() - $g_startTs;
}

sub conv_ms($) {
    my($ts) = @_;
    return $ts * 1000;
}


sub correctTime($$$) {
  my($tMeas, $tMeas2, $measCount) = @_;
	my $tsPrecCnt = $g_tsPrecCnt;

	if ($measCount < $tsPrecCnt) {
  	$tMeas += $g_tsPrecMs * (($tsPrecCnt - $measCount) / $tsPrecCnt); # use start ts + correction
    if ($tMeas > $tMeas2) {
        $tMeas = $tMeas2; # cannot correct
    }
	}
	return $tMeas;
}

sub getPrecMs($) {
  my($stopFlg) = @_;
	my $measCount = 0;
  my $tMeas;

	my $tMeas0 = get_ts();
  $tMeas = $tMeas0;
	while ($tMeas <= $tMeas0) {
		$tMeas = get_ts();
		$measCount++;
	}
	$g_tsMeasCnt = $measCount; # memorize last count

	# if ($stopFlg) {
	# 	$tMeas = correctTime(conv_ms($tMeas0), conv_ms($tMeas), $measCount);
	# }
  my $tMeasD = (!$stopFlg) ? conv_ms($tMeas) : correctTime(conv_ms($tMeas0), conv_ms($tMeas), $measCount);
	return $tMeasD;
}

sub determineTsPrecision() {
  private_init_HiRes();
  $g_startTs = get_raw_ts(); # memorize start time

	my $tMeas0 = getPrecMs(0);
	my $tMeas1 = getPrecMs(0);
	$g_tsPrecMs = $tMeas1 - $tMeas0;
	$g_tsPrecCnt = $g_tsMeasCnt;

  # do it again
	$tMeas0 = $tMeas1;
	$tMeas1 = getPrecMs(0);
	if ($g_tsMeasCnt > $g_tsPrecCnt) { # taker maximum count
		$g_tsPrecCnt = $g_tsMeasCnt;
		$g_tsPrecMs = $tMeas1 - $tMeas0;
	}
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
  local $SIG{__WARN__} = sub { warn $_[0] }; #we could catch warning "Lost precision when incrementing"
  my $num = 1.0;
  my $last_num = 0.0;
  my $bits = 0;
  my $numBeforeInc;
  do {
    $last_num = $num;
    $num *= 2.0;
    $numBeforeInc = $num; # to stop after first warning "Lost precision when incrementing"
    $num++;
    $bits++;
  } while ( ($num != $numBeforeInc) && ((($num - 1.0) / 2.0) == $last_num) && ($bits < 101) );
  return $bits;
}


sub print_info() {
  my $perl_version = $];
  $perl_version =~ tr/\n/;/;
  print("BM Bench v", $G_PRG_VERSION, " (", $G_PRG_LANGUAGE, ") -- (int:", checkbits_int1(), " double:", checkbits_double1(),  " tsType:", $g_tsType, " tsMs:", $g_tsPrecMs, " tsCnt:", $g_tsPrecCnt, ") $perl_version, osname: $^O\n");
  print("(c) Marco Vieth, 2002-2019\n");
  print("Date: ". localtime(time()) ."\n");
  #system("uname -a");        
}


sub print_results($$$) {
  my($bench1, $bench2, $bench_res1_r) = @_;
  my $max_language_len1 = 10;
  
  print("\nThroughput for all benchmarks (loops per sec):\n");
  print "BMR (", $G_PRG_LANGUAGE .")". (' ' x ($max_language_len1 - length($G_PRG_LANGUAGE))), ": ";
  for (my $bench = $bench1; $bench <= $bench2; $bench++) {
    printf("%9.3f ", $bench_res1_r->[$bench]);
  }
  print "\n";
  print "\n";
}


sub measureBench($$) {
  my($bench, $n) = @_;
  my $cali_ms = 1001; # const
  my $delta_ms = 100; # const
  my $max_ms = 10000; # const

  my $loops = 1; # number of loops
  my $x = 0;     # result from benchmark
  my $tMeas = 0;    # measured time
  my $tEsti = 0;    # estimated time
  my $throughput = 0;

  printf("Calibrating benchmark %d with n=%d\n", $bench, $n);
  while (!$throughput) {
    $tMeas = getPrecMs(0); #get_ms()
    $x = run_bench($bench, $loops, $n);
    $tMeas = getPrecMs(1) - $tMeas; #get_ms() - $tMeas

    my $t_delta = ($tEsti > $tMeas) ? ($tEsti - $tMeas) : ($tMeas - $tEsti); # compute difference abs(measures-estimated)
    my $loops_p_sec = ($tMeas > 0) ? ($loops * 1000.0 / $tMeas) : 0;
    printf("%10.3f/s (time=%9.3f ms, loops=%7d, delta=%9.3f ms, x=%d)\n", $loops_p_sec, $tMeas, $loops, $t_delta, $x);
    if ($x == -1) { # some error?
      $throughput = -1;
    } elsif (($tEsti > 0) &&  ($t_delta < $delta_ms)) { # do we have some estimated/expected time smaller than delta_ms=100? 
      $throughput = $loops_p_sec; # yeah, set measured loops per sec
      printf("Benchmark %d (%s): %.3f/s (time=%9.3f ms, loops=%d, delta=%9.3f ms)\n", $bench, $G_PRG_LANGUAGE, $loops_p_sec, $tMeas, $loops, $t_delta);
    } elsif ($tMeas > $max_ms) {
      printf("Benchmark %d (%s): Time already > %d ms. No measurement possible.\n", $bench, $G_PRG_LANGUAGE, $max_ms);
      #$throughput = -1;
      $throughput = ($loops_p_sec) ? -$loops_p_sec : -1; # cannot rely on measurement, so set to negative
    } else {
      #my $scale_fact = (($tMeas < $cali_ms) && ($tMeas > 0)) ? int((($cali_ms + 100) / $tMeas) + 1) : 2;
      #my $scale_fact = ($tMeas == 0) ? 50 : ($tMeas < $cali_ms) ? int((($cali_ms + 100) / $tMeas) + 1) : 2;
      my $scale_fact;
      if ($tMeas == 0) {
        $scale_fact = 50;
      } elsif ($tMeas < $cali_ms) {
        $scale_fact = int((($cali_ms + 100) / $tMeas) + 1); # scale a bit up to 1100 ms (cali_ms+100)
      } else {
        $scale_fact = 2;
      }
      $loops *= $scale_fact;
      $tEsti = $tMeas * $scale_fact;
    }
  }
  return $throughput;
}


sub start_bench($$$) {
  my($bench1, $bench2, $n) = @_;
  my $cali_ms = 1001; # const
  my $delta_ms = 100; # const
  my $max_ms = 10000; # const

  print_info();

  my @bench_res1 = ();
  for (my $bench = $bench1; $bench <= $bench2; $bench++) {
    my $throughput =  measureBench($bench, $n);
    $bench_res1[$bench] = $throughput;
  }

  print_results($bench1, $bench2, \@bench_res1);
  return 0;
}


sub main($) {
  #my(@ARGV) = @_;
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
  
  determineTsPrecision();
  my $rc = start_bench($bench1, $bench2, $n);
  
  printf("Total elapsed time: %d ms\n", conv_ms(get_ts()) | 0);
  return $rc;
}


main(@ARGV);

__END__
# end
