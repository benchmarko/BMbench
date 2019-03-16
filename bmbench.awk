#! /bin/awk -f
# BM Bench - bmbench.awk (awk)
# (c) Marco Vieth, 2002-2006
# http://www.benchmarko.de
#
# 24.07.2002 0.01
# 10.08.2002 0.04  bench03; some errors corrected
# 24.01.2003 0.05  output format changed
# 30.04.2008 0.06  based on version 0.05
#
#
# Usage:
# gawk -f bmbench1.awk [bench1] [bench2] [n]
#
# With check:
# gawk -W lint --dump-variables -f bmbench1.awk
#

# Compatibility:
# - Uses 'function', 'return' which is not supported by old awk
# - Uses split("", <array>) as a portable way (for delete <array>) to delete array
# - Uses systime() (and optional strftime()) which is a gawk extension
#

#
# Info:
# man gawk
# info gawk
# /usr/share/doc/packages/gawk-doc/gawk.ps.gz (GAWK: Effective AWK Programming) (sophisticated!)
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

BEGIN {
  PRG_VERSION = "0.06";
  PRG_LANGUAGE = "awk";
}

#
# bench00 (Integer 16 bit) (awk computes always in floating point!)
# (sum of 1..n) mod 65536
#
function bench00(loops, n,    x, sum1, n_div_65536, n_mod_65536, i, j) {
  x = 0;
  sum1 = ((n / 2) * (n + 1)) % 65536; # assuming n even!
  # (sum1..1000000 depends on type: 500000500000 (floating point), 1784293664 (32bit), 10528 (16 bit)
  n_div_65536 = int(n / 65536.0);
  n_mod_65536 = (n % 65536.0);
  while (loops-- > 0) {
    for (i = n_div_65536; i > 0; i--) {
      for (j = 65535; j > 0; j--) {
        x += j;
      }
    }
    for (j = n_mod_65536; j > 0; j--) {
      x += j;
    }
    x %= 65536;
    #printf("DEBUG: x=%d, sum1=%d\n", x, sum1);
    if (loops > 0) { # some more loops left?
      x -= sum1;     # yes, set x back to 0 (assuming n even)
      if (x != 0) {  # now x must be 0 again
        x++;
        break;       # error */
      }
    }
  }
  return(x % 65536);
}


#
# bench01 (Integer 16/32 bit) (awk computes always in floating point!)
# (sum of 1..n) mod 65536
#
function bench01(loops, n,    x, sum1, i) {
  x = 0;
  sum1 = ((n / 2) * (n + 1)); # assuming n even!
  # (sum1..1000000 depends on type: 500000500000 (floating point), 1784293664 (32bit), 10528 (16 bit)
  while (loops-- > 0) {
    for (i = n; i > 0; i--) {
      x += i;
    }
    if (loops > 0) { # some more loops left?
      x -= sum1;     # yes, set x back to 0 (assuming n even)
      if (x != 0) {  # now x must be 0 again
        x++;
        break;       # error */
      }
    }
  }
  return(x % 65536);
}


#
# bench02 (Floating Point, normally 64 bit)
# (sum of 1..n) mod 65536
#
function bench02(loops, n,    x, sum1, i) {
  x = 0.0;
  sum1 = (n / 2.0) * (n + 1.0); # assuming n even!
  # (sum1..1000000 depends on type: 500000500000 (floating point), 1784293664 (32bit), 10528 (16 bit)
  while (loops-- > 0) {
    for (i = n; i > 0; i--) {
      x += i;
    }
    if (loops > 0) {    # some more loops left?
      x -= sum1;        # yes, set x back to 0 (assuming n even)
      if (x != 0.0) {   # now x must be 0 again
        x++;
        break;          # error
      }
    }
  }
  # fprintf(stderr, "DEBUG(bench%d): x=%f, x(int)=%d  %f\n", 2, x, (int)(x / 65536), (x - ((int)(x / 65536.0) * 65536.0)));
  return (x - (int(x / 65536.0) * 65536.0)); # or use fmod()...
}


#
# bench03 (Integer)
# number of primes below n (Sieve of Eratosthenes)
# Example: n=500000 => x=41538 (expected), n=1000000 => x=78498
# (No bit array, so we use a normal array...)
function bench03(loops, n,    x, sieve1, i, j) {
  n /= 2; # compute only up to n/2

  x = 0; # number of primes below n
  #sieve1 = new Array(n + 1); #
  sieve1[0] = 0;
  sieve1[1] = 0;

  while (loops-- > 0) {
    # initialize sieve
    for (i = 2; i <= n; i++) {
      sieve1[i] = 1;
    }
    # compute primes
    for (i = 2; (i * i) <= n; i++) {
      if (sieve1[i]) {
        for (j = i * i; j <= n; j += i) {
          sieve1[j] = 0;
        }
      }
    }
    # count primes
    for (i = 0; i <= n; i++) {
      if (sieve1[i]) {
        x++;
      }
    }
    # check prime count
    if (loops > 0) {  # some more loops left?
      x -= 41538;     # yes, set x back to 0 (number of primes below 1000000)
      if (x != 0) {   # now x must be 0 again
        x++;
        break;        # Error
      }
    }
  }
  return x;
}


#
# bench04 (Integer 32 bit)
# nth random number number
# Random number generator taken from
# Raj Jain: The Art of Computer Systems Performance Analysis, John Wiley & Sons, 1991, page 442-444.
# It needs longs with at least 32 bit.
# Starting with x0=1, x10000 should be 1043618065, x1000000 = 1227283347.
#
function bench04(loops, n,    x, i, M, A, Q, R, x_div_q, x_mod_q) {
  x = 1;                # last random value
  M = 2147483647; # modulus, do not change!
  A = 16807;      # multiplier
  Q = 127773;     # m div a
  R = 2836;       # m mod a
  while (loops-- > 0) {
    for (i = 1; i <= n; i++) {
      x_div_q = int(x / Q);
      x_mod_q = x - Q * x_div_q;
      x = A * x_mod_q - R * x_div_q;
      if (x <= 0) {
        x += M; # x is new random number
      }
    }
    if (loops > 0) {
      x -= 1227283347;
      if (x != 0) {   # now x must be 0 again
        x++;
        break;        # Error
      }
      x++; # start with 1 again
    }
  }
  # fprintf(stderr, "Test(bench%d): x=%ld\n", 4, (long)x);
  return x;
}


#
# bench05 (Integer 32 bit) (list implementation)
# n over n/2 mod 65536 (Pascal's triangle)
# (we just need to store the last 2 lines of computation)
#
function bench05(loops, n,    x, k, pas1, pas2, i, min1, j) {
  x = 0;
  n = int(n / 500);
  k = int(n / 2);

  if ((n - k) < k) {
    k = n - k; # keep k minimal with  n over k  =  n over n-k
  }
  #pas1 = ();
  #pas2 = ();
  
  while (loops-- > 0) {
    pas1[0] = 1;
    for (i = 2; i <= n; i++) {

      # get last line to pas2 (pas2 = pas1) ...
      #delete pas2; # don't need this because we just grow and delete pas1...
      for (j in pas1) {
        pas2[j] = pas1[j]; # copy elements from pas2 into pas1
      }

      # (We do not need to delete the array since we overwrite all elements...)
      #split("", pas1); # portable way to 'delete pas1'

      pas1[0] = 1; # and restart with new list
      min1 = int((i - 1) / 2); # int(...)
      if (k < min1) {
        min1 = k;
      }
      pas1[1] = i; # second column is i
      for (j = 2; j <= min1; j++) { # up to min((i-1)/2, k)
        pas1[j] = (pas2[j - 1] + pas2[j]) % 65536; # modulus to avoid nan
      }
      if ((min1 < k) && ((i % 2) == 0)) { # new element
        pas1[j] = 2 * pas2[min1];
      }
    }
    x += pas1[k] % 65536;
    if (loops > 0) {
      x -= 27200;
      if (x != 0) {   # now x must be 0 again
        x++;
        break;          # Error
      }
    }
  }
  return x;
}


#
# bench05 (Integer 32 bit) (Array implementation)
# n over n/2 mod 65536 (Pascal's triangle)
# (we just need to store the last 2 lines of computation)
#
function bench05_array_ok1(loops, n,    x, k, pas1, i, i_mod_2, i_mod_2_1, min1, j) {
  x = 0;
  n = int(n / 500);
  k = int(n / 2);

  if ((n - k) < k) {
    k = n - k; # keep k minimal with  n over k  =  n over n-k
  }

  pas1[0, 0] = 1; pas1[1, 0] = 1; # set first column

  while (loops-- > 0) {
    for (i = 2; i <= n; i++) {
      i_mod_2 = i % 2;
      i_mod_2_1 = (i + 1) % 2;
      min1 = int((i - 1) / 2); # int(...)
      if (k < min1) {
        min1 = k;
      }
      pas1[i_mod_2, 1] = i; # second column is i
      for (j = 2; j <= min1; j++) { # up to min((i-1)/2, k)
        pas1[i_mod_2, j] = (pas1[i_mod_2_1, j - 1] + pas1[i_mod_2_1, j]) % 65536; # modulus to avoid nan
      }
      if ((min1 < k) && (i_mod_2 == 0)) { # new element
        pas1[i_mod_2, min1 + 1] = 2 * pas1[i_mod_2_1, min1];
      }
    }
    x += pas1[n % 2, k] % 65536;
    if (loops > 0) {
      x -= 27200;
      if (x != 0) {   # now x must be 0 again
        x++;
        break;          # Error
      }
    }
  }
  return x;
}


#
# run a benchmark
# in: bench = benchmark to use
#     loops = number of loops
#         n = maximum number (used in some benchmarks to define size of workload)
# out:    x = result
#
function run_bench(bench, loops, n,    x, check1) {
  x = 0;
  check1 = 0;
  if (bench == 0) {
      x = bench00(loops, n); # special version optimized for 16 bit
      check1 = 10528;

  } else if (bench == 1) {
      x = bench01(loops, n);
      check1 = 10528;

  } else if (bench == 2) {
      x = bench02(loops, n);
      check1 = 10528;

  } else if (bench == 3) {
      x = bench03(loops, n);
      check1 = 41538;

  } else if (bench == 4) {
      x = bench04(loops, n);
      check1 = 1227283347;

  } else if (bench == 5) {
      x = bench05(loops, n);
      check1 = 27200; # 58336; 43584;

  } else {
      printf("Error: Unknown benchmark: %d\n", bench);
      check1 = x + 1; # force error
  }
  if (check1 != x) {
    printf("Error(bench%d): x=%d\n", bench, x);
    x = -1; # exit
  }
  return(x);
}


#
# get timestamp in milliseconds
# out: x = time in ms
#
# systime() is gawk extension.
#
function get_ms() {
  if (g_use_gawk) {
    return(systime() * 1000); # for gawk we have systime()! (but still undefined on other systems...)
  } else {
    g_last_time += 5;
    return(g_last_time * 1000);
  }
}

function getdate1() {
  if (g_use_gawk) {
    return strftime(); # gawk extension
  } else {
    return "";
  }
}


/* Here we compute the number of "significant" bits for positive numbers (which means 53 for double) */
function checkbits_int1(    num, last_num, bits) {
  num = 1;
  last_num = 0;
  bits = 0;
  do {
    last_num = num;
    num *= 2;
    num++;
    bits++;
  } while ( (((num - 1) / 2) == last_num) && (bits < 101) );
  return bits;
}

function checkbits_double1(    num, last_num, bits) {
  num = 1.0;
  last_num = 0.0;
  bits = 0;
  do {
    last_num = num;
    num *= 2.0;
    num++;
    bits++;
  } while ( (((num - 1.0) / 2.0) == last_num) && (bits < 101) );
  return bits;
}


function print_info() {
  printf("BM Bench v%s (%s) -- (int:%d double:%d) %s\n", PRG_VERSION, PRG_LANGUAGE, checkbits_int1(), checkbits_double1(), g_awk_version);
  print("(c) Marco Vieth, 2006"); # print appends \n!
  print("Date:", getdate1());
}


function print_results(bench1, bench2, bench_res1,    max_language_len1, bench) {
  max_language_len1 = 10;

  print("\nThroughput for all benchmarks (loops per sec):");
  printf("BMR (%s)%*s: ", PRG_LANGUAGE, max_language_len1 - length(PRG_LANGUAGE), "");
  # could also program a loop for formatting
  for (bench = bench1; bench <= bench2; bench++) {
    printf("%9.2f ", bench_res1[bench]);
  }
  print;
  print;
}


#function main(argc, argv,    start_t, bench1, bench2, n, min_ms, bench, bench_res1, loops, x, t1) {
function start_bench(bench1, bench2, n,    cali_ms, delta_ms, max_ms, bench, loops, x, t1, t2, t_delta, loops_p_sec, scale_fact) {
  cali_ms = 1001; # const
  delta_ms = 100; # const
  max_ms = 10000; # const

  print_info();

  #bench_res1[]=
  for (bench = bench1; bench <= bench2; bench++) {
    loops = 1; # number of loops
    x = 0;     # result from benchmark
    t1 = 0;    # measured time
    t2 = 0;    # estimated time

    printf("Calibrating benchmark %d with n=%d\n", bench, n);
    while (1) {
      t1 = get_ms();
      x = run_bench(bench, loops, n);
      t1 = get_ms() - t1;

      t_delta = (t2 > t1) ? (t2 - t1) : (t1 - t2); # compute difference abs(measures-estimated)
      loops_p_sec = (t1 > 0) ? (loops * 1000.0 / t1) : 0;
      printf("%10.3f/s (time=%5ld ms, loops=%7d, delta=%5d ms, x=%d)\n", loops_p_sec, t1, loops, t_delta, x);
      if (x == -1) { # some error?
        bench_res1[bench] = -1;
        break; # (check: can only exit while, if not in sub block?)
      }
      if (t2 > 0) { # do we have some estimated/expected time? 
        if (t_delta < delta_ms) { # smaller than delta_ms=100? 
          bench_res1[bench] = loops_p_sec; # set loops per sec
          printf("Benchmark %d (%s): %.3f/s (time=%ld ms, loops=%d, delta=%d ms)\n", bench, PRG_LANGUAGE, bench_res1[bench], t1, loops, t_delta);
          break;
        }
      }

      if (t1 > max_ms) {
        printf("Benchmark %d (%s): Time already > %d ms. No measurement possible.\n", bench, PRG_LANGUAGE, max_ms);
        bench_res1[bench] = -1;
        break;
      }
      {
        scale_fact = ((t1 < cali_ms) && (t1 > 0)) ? int(((cali_ms + 100) / t1) + 1) : 2;
          # scale a bit up to 1100 ms (cali_ms+100)
        loops *= scale_fact;
        t2 = t1 * scale_fact;
      }
    }
  }

  print_results(bench1, bench2, bench_res1);
  return 0;
}



function main(argc, argv,    start_t, bench1, bench2, n, rc ) {
  start_t = get_ms(); # memorize start time
  bench1 = 0;       # first benchmark to test
  bench2 = 5;       # last benchmark to test
  n = 1000000;      # maximum number

  if (argc > 1) {
    bench1 = argv[1];
    bench2 = bench1;
  }
  if (argc > 2) {
    bench2 = argv[2];
  }
  if (argc > 3) {
    n = argv[3];
  }

  rc = start_bench(bench1, bench2, n);

  printf("Total elapsed time: %d ms\n", (get_ms() - start_t));
  return rc;
}


BEGIN {
  if (TEXTDOMAIN) { # TEXTDOMAIN is defined for gawk!
    g_use_gawk = 1;
    # assume gawk and get first line with version info...
    "gawk --version" | getline g_awk_version;
    #g_awk_version = "gawk version ?";
  } else {
    g_use_gawk = 0;
    g_awk_version = "awk version ?";
  }
  main(ARGC, ARGV);
  exit;
}
# end
