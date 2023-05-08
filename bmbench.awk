#! /bin/awk -f
# BM Bench - bmbench.awk (awk)
# (c) Marco Vieth, 2002-2006
# http://www.benchmarko.de
#
# 24.07.2002 0.01
# 10.08.2002 0.04  bench03; some errors corrected
# 24.01.2003 0.05  output format changed
# 30.04.2008 0.06  based on version 0.05
# 15.03.2023 0.08  adapted for new version; bench05 optimized
#
#
# Usage:
# gawk -f bmbench.awk [bench1] [bench2] [n]
#
# With check:
# gawk -W lint --dump-variables -f bmbench.awk
#

# Compatibility:
# - Uses 'function', 'return' which is not supported by old awk
# - Uses split("", <array>) as a portable way (for delete <array>) to delete array
# - Uses systime() (and optional strftime()) which is a gawk extension
# - gawk >= 4.1: awk '@load "time"; BEGIN{printf "%.6f", gettimeofday()}'
# - ( https://www.gnu.org/software/gawk/manual/html_node/Extension-Sample-Time.html )
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

# comment the following line if you do not use Gawk >= 4.1
@load "time" # precise timing could be used for Gawk >= 4.1, cannot set dynamically

BEGIN {
  g_use_gettimeofday = 1 # 0 = off, 1= use gettimeofday from the "time" library of Gawk >= 4.1

  PRG_VERSION = "0.08";
  PRG_LANGUAGE = "awk";

  g_tsPrecMs = 0; # measured time stamp precision
  g_tsPrecCnt = 0; # time stamp count (calls) per precision interval (until time change)
  g_tsMeasCnt = 0; # last measured count
  g_cali_ms = 1001;
}

#
# bench00 (Integer 16 bit) (awk computes always in floating point!)
# (sum of 1..n) mod 65536
#
function bench00(n,    x, n_div_65536, n_mod_65536, i, j) {
  x = 0;
  n_div_65536 = int(n / 65536.0);
  n_mod_65536 = (n % 65536.0);
  for (i = n_div_65536; i > 0; i--) {
    for (j = 65535; j > 0; j--) {
      x += j;
    }
  }
  for (j = n_mod_65536; j > 0; j--) {
    x += j;
  }
  #printf("DEBUG: x=%d\n", x);
  return x % 65536;
}


#
# bench01 (Integer 16/32 bit) (awk computes always in floating point!)
# (arithmetic mean of 1..n)
#
function bench01(n,    x, sum, i) {
  x = 0;
  sum = 0;
  for (i = 1; i <= n; i++) {
    sum += i;
    if (sum >= n) {
      sum -= n;
      x++;
    }
  }
  return x;
}


#
# bench02 (Floating Point, normally 64 bit)
# (sum of 1..n) mod 65536
#
function bench02(n,    x, sum, i) {
  x = 0;
  sum = 0.0;
  for (i = 1; i <= n; i++) {
    sum += i;
    if (sum >= n) {
      sum -= n;
      x++;
    }
  }
  return x;
}


#
# bench03 (Integer)
# number of primes below n (Sieve of Eratosthenes)
# Example: n=500000 => x=41538 (expected), n=1000000 => x=78498
# (No bit array, so we use a normal array...)
function bench03(n,    nHalf, m, x, sieve1, i, j) {
  nHalf = n / 2;

  #sieve1 = new Array(nHalf + 1);

  # initialize sieve
  for (i = 0; i <= nHalf; i++) {
    sieve1[i] = 0;
  }

  # compute primes
  i = 0;
  m = 3;
  x = 1; # number of primes below n (2 is prime)

  while (m * m <= n) {
    if (!sieve1[i]) {
      x++; # m is prime
      j = int((m * m - 3) / 2); # div 2
      while (j < nHalf) {
        sieve1[j] = 1;
        j += m;
      }
    }
    i++;
    m += 2;
  }

  # count remaining primes
  while (m <= n) {
    if (!sieve1[i]) {
      x++; # m is prime
    }
    i++;
    m += 2;
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
function bench04(n,    x, i, M, A, Q, R, x_div_q, x_mod_q) {
  x = 1;                # last random value
  M = 2147483647; # modulus, do not change!
  A = 16807;      # multiplier
  Q = 127773;     # m div a
  R = 2836;       # m mod a
  for (i = 1; i <= n; i++) {
    x_div_q = int(x / Q);
    x_mod_q = x - Q * x_div_q;
    x = A * x_mod_q - R * x_div_q;
    if (x <= 0) {
      x += M; # x is new random number
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
function bench05(n,    x, k, line, i, min1, prev, j, num) {
  # Instead of nCk with k=n/2, we compute the product of (n/2)Ck with k=0..n/4
  n = int(n / 2);

  k = int(n / 2);
  if ((n - k) < k) {
    k = n - k; # keep k minimal with  n over k  =  n over n-k
  }

  #line[]=
  line[0] = 1;
  if (k >= 1) {
    line[1] = 2; # for line 2, second column is 2
  }

  # compute lines of Pascal's triangle
  for (i = 3; i <= n; i++) {
    min1 = int((i - 1) / 2);

    if ((i % 2) == 0) { # even => new element?
      line[min1 + 1] = 2 * line[min1];
    }

    prev = line[1];
    for (j = 2; j <= min1; j++) {
      num = line[j];
      line[j] = (line[j] + prev) % 65536;
      prev = num;
    }
    line[1] = i; # second column is i
  }

  # compute sum of ((n/2)Ck)^2 mod 65536 for k=0..n/2
  x = 0;
  for (j = 0; j < k; j++) {
    x = (x + 2 * line[j] * line[j]) % 65536; # add nCk and nC(n-k)
	}
	x += line[k] * line[k]; # we assume that k is even, so we need to take the middle element

  return x % 65536;
}

function bench06(n,    sum, flip, i) {
  sum = 0.0;
	flip = 1.0;
  for (i = 1; i <= n; i++) {
    sum += flip / (2 * i - 1);
		flip *= -1.0;
  }
  return int((sum * 4.0) * 100000000);
}


#
# run a benchmark
# in: bench = benchmark to use
#     loops = number of loops
#         n = maximum number (used in some benchmarks to define size of workload)
# out:    x = result
#
function run_bench(bench, loops, n, check,    x) {
  x = 0;
  while (loops-- > 0 && x == 0) {
    if (bench == 0) {
        x = bench00(n); # special version optimized for 16 bit

    } else if (bench == 1) {
        x = bench01(n);

    } else if (bench == 2) {
        x = bench02(n);

    } else if (bench == 3) {
        x = bench03(n);

    } else if (bench == 4) {
        x = bench04(n);

    } else if (bench == 5) {
        x = bench05(n);

    } else if (bench == 6) {
        x = bench06(n);

    } else {
        printf("Error: Unknown benchmark: %d\n", bench);
        check = -1;
    }
    x -= check;
  }

  x += check;
  if (x != check) {
    printf("Error(bench%d): x=%d\n", bench, x);
    x = -1;
  }
  return(x);
}


function bench03Check(n,  x, j, i, isPrime) {
  x = 1; # 2 is prime
  for (j = 3; j <= n; j += 2) {
    isPrime = 1;
    for (i = 3; i * i <= j; i += 2) {
      if (j % i == 0) {
        isPrime = 0;
        break;
      }
    }
    if (isPrime) {
      x++;
    }
  }
  return x;
}


function getCheck(bench, n,   check) {
  check = 0;

  if (bench == 0) {
    check = (int((n + (n % 2)) / 2) * (n + 1 - (n % 2))) % 65536; # 10528 for n=1000000

  } else if (bench == 1) {
    check = int((n + 1) / 2); # 10528

  } else if (bench == 2) {
    check = int((n + 1) / 2);

  } else if (bench == 3) {
    check = (n == 500000) ? 41538 : bench03Check(n);

  } else if (bench == 4) {
    check = (n == 1000000) ? 1227283347 : bench04(n); # bench04 not a real check

  } else if (bench == 5) {
    check = (n == 5000) ? 17376 : bench05(n); # bench05 not a real check

  } else if (bench == 6) {
    check = (n == 1000000) ? 314159165 : bench06(n); # bench06 not a real check

  } else {
    printf("Error: Unknown benchmark: %d\n", bench);
    check = -1;
  }
  return check;
}

function get_raw_ts() {
  if (g_use_gettimeofday) {
    return(gettimeofday());
  } else if (g_use_systime) {
    return(systime()); # for gawk we have systime()! (but still undefined on other systems...)
  } else {
    g_last_time += 0.5; # can only simulate time
    return(g_last_time);
  }
}

# get timestamp since program start
function get_ts() {
  return (get_raw_ts() - g_startTs);
}

# convert timestamp to ms
function conv_ms(ts) {
    return ts * 1000;
  }

function correctTime(tMeas, tMeas2, measCount,   tsPrecCnt) {
  tsPrecCnt = g_tsPrecCnt;

  if (measCount < tsPrecCnt) {
    tMeas += g_tsPrecMs * ((tsPrecCnt - measCount) / tsPrecCnt); # ts + correction
    if (tMeas > tMeas2) {
      tMeas = tMeas2; # cannot correct
    }
  }
  return(tMeas);
}

function getPrecMs(stopFlg,    measCount, tMeas0, tMeas, tMeasD) {
  measCount = 0;

  tMeas0 = get_ts();
  tMeas = tMeas0;
  while (tMeas <= tMeas0) {
    tMeas = get_ts();
    measCount++;
  }
  g_tsMeasCnt = measCount; # memorize count
  #Console.WriteLine("DEBUG: getPrecMs: measCount=" + measCount + " ts=" + tMeas);

  # for stop: use first ts + correction
  tMeasD = (!stopFlg) ? conv_ms(tMeas) : correctTime(conv_ms(tMeas0), conv_ms(tMeas), measCount);
  return tMeasD;
}

# usually only needed if time precision is low, e.g. one second
function determineTsPrecision(   tMeas0, tMeas1) {
  g_startTs = get_raw_ts(); #/ memorize start time

  tMeas0 = getPrecMs(0);
  tMeas1 = getPrecMs(0);
  g_tsPrecMs = tMeas1 - tMeas0;
  g_tsPrecCnt = g_tsMeasCnt;

  # do it again
  tMeas0 = tMeas1;
  tMeas1 = getPrecMs(0);
  if (g_tsMeasCnt > g_tsPrecCnt) { # taker maximum count
    g_tsPrecCnt = g_tsMeasCnt;
    g_tsPrecMs = tMeas1 - tMeas0;
  }
}


# Here we compute the number of "significant" bits for positive numbers (which means 53 for double)
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

function getdate1() {
  if (g_use_strftime) {
    return strftime("%Y-%m-%d %H:%M:%S"); # gawk extension
  } else {
    return "";
  }
}

function print_info(   awk_info) {
  if (g_awk_version) {
    awk_info = "GNU Awk " g_awk_version " API: " PROCINFO["api_major"] "." PROCINFO["api_minor"] " (" PROCINFO["mpfr_version"] ", " PROCINFO["gmp_version"] ") " PROCINFO["platform"]
  } else {
    awk_info = "awk version ?";
  }

  printf("BM Bench v%s (%s) -- (int:%d double:%d tsMs:%f tsCnt:%d) %s\n", PRG_VERSION, PRG_LANGUAGE, checkbits_int1(), checkbits_double1(),  g_tsPrecMs, g_tsPrecCnt, awk_info);
  print("(c) Marco Vieth, 2006-2023"); # print appends \n
  print("Date:", getdate1());
}


function print_results(bench1, bench2, bench_res1,    max_language_len1, bench) {
  max_language_len1 = 10;

  print("\nThroughput for all benchmarks (loops per sec):");
  printf("BMR (%s)%*s: ", PRG_LANGUAGE, max_language_len1 - length(PRG_LANGUAGE), "");
  # could also program a loop for formatting
  for (bench = bench1; bench <= bench2; bench++) {
    printf("%9.3f ", bench_res1[bench]);
  }
  print "";
  print "";
}


function measureBench(bench, n, check,   delta_ms, max_ms, cali_ms, loops, x, t1, t2, throughput, t_delta, loops_p_sec, scale_fact) {
  delta_ms = 100; # const
  max_ms = 10000; # const
  cali_ms = g_cali_ms;

  loops = 1; # number of loops
  x = 0;     # result from benchmark
  t1 = 0.0;    # measured time
  t2 = 0.0;    # estimated time
  throughput = 0;

  printf("Calibrating benchmark %d with n=%d, check=%d\n", bench, n, check);
  while (throughput == 0) {
    t1 = getPrecMs(0);
    x = run_bench(bench, loops, n, check);
    t1 = getPrecMs(1) - t1;

    t_delta = (t2 > t1) ? (t2 - t1) : (t1 - t2); # compute difference abs(measures-estimated)
    loops_p_sec = (t1 > 0) ? (loops * 1000.0 / t1) : 0;
    printf("%10.3f/s (time=%9.3f ms, loops=%7d, delta=%9.3f ms, x=%d)\n", loops_p_sec, t1, loops, t_delta, x);
    if (x == -1) { # some error?
      throughput = -1;
    } else if ((t2 > 0) && (t_delta < delta_ms)) {
      throughput = loops_p_sec;
      printf("Benchmark %d (%s): %.3f/s (time=%9.3f ms, loops=%d, delta=%.3f ms)\n", bench, PRG_LANGUAGE, loops_p_sec, t1, loops, t_delta);
    } else if (t1 > max_ms) {
      printf("Benchmark %d (%s): Time already > %d ms. No measurement possible.\n", bench, PRG_LANGUAGE, max_ms);
      throughput = (loops_p_sec > 0) ? -loops_p_sec : -1; # cannot rely on measurement, so set to negative
    } else {
      if (t1 == 0) {
        scale_fact = 50;
      } else if (t1 < cali_ms) {
        scale_fact = int((cali_ms + 100) / t1) + 1; # scale a bit up to 1100 ms (cali_ms+100)
      } else {
        scale_fact = 2;
      }
      loops *= scale_fact;
      t2 = t1 * scale_fact;
    }
  }
  return throughput;
}

function start_bench(bench1, bench2, n, argStr,  bench_res1, bench, n2, check, throughput) {
  print_info();
  if (argStr != "") {
    print "Args:" argStr;
  }

  #bench_res1[]=

  for (bench = bench1; bench <= bench2; bench++) {
    n2 = n;

    if (bench == 3) {
      n2 = n2 / 2;
    } else if (bench == 5) {
      n2 = n2 / 200;
    }

    check = getCheck(bench, n2);
    if (check > 0) {
      throughput = measureBench(bench, n2, check);
     } else {
      throughput = -1;
    }
    bench_res1[bench] = throughput;
  }

  print_results(bench1, bench2, bench_res1);
  return 0;
}



function main(argc, argv,    bench1, bench2, n, rc) {
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
   if (argc > 4) {
    g_cali_ms = argv[4];
  }

  argStr = "";
  for (i = 1; i < argc; i++) {
    argStr = argStr " " argv[i];
  }

  determineTsPrecision();
  rc = start_bench(bench1, bench2, n, argStr);

  printf("Total elapsed time: %d ms\n", conv_ms(get_ts()));
  return rc;
}

BEGIN {
  if (!g_use_gettimeofday) { # not set to 1 on top?
    g_use_gettimeofday = 0
  }

  if (PROCINFO["version"]) { # defined since Gawk 3.1.4
    g_awk_version = PROCINFO["version"]
    g_use_systime = 1 # can be used since gawk 2.1.3
    g_use_strftime = 1

    if (g_awk_version >= 4.1) {
      #@load "time"; # cannot load here
      #g_use_gettimeofday = 1
    }
    # alternative way to get version info from first line of "gawk --version":
    # "gawk --version" | getline g_awk_info;
  } else {
    g_use_systime = 0;
    g_use_strftime = 0;
    g_awk_version = 0;
  }
  main(ARGC, ARGV);
  exit;
}
# end
