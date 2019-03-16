# bmbench.awk
# (c) Benchmarko, 2002
#
# 24.07.2002  0.01
# 10.08.2002  0.04  bench03; some errors corrected
#
#
# Usage:
# gawk -f bmbench.awk [bench1] [bench2] [n]
#
# With check:
# gawk -W lint -f bmbench.awk
#

# Compatibility:
# - Uses 'function', 'return' which is not supported by old awk
# - Uses systime(), delete <array> which is a gawk extension
#

#
# Info:
# man gawk
# info gawk
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


  #
  # bench01 (Integer 16/32 bit) (awk computes always in floating pont!)
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
        #delete pas2; # don't need tihs because we just grow and delete pas1...
        for (j in pas1) {
          pas2[j] = pas1[j]; # copy elements from pas2 into pas1
        }
        delete pas1;

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
        #x = bench00(loops, n); # special version optimized for 16 bit
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
      break;
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
  # g_time += 5; return(g_time * 1000);  # test
  return(systime() * 1000);
}


function main(argc, argv,    start_t, bench1, bench2, n, min_ms, bench, bench_res, loops, x, t1) {
  start_t = get_ms(); # memorize start time
  bench1 = 1;       # first benchmark to test
  bench2 = 5;       # last benchmark to test
  n = 1000000;      # maximum number
  min_ms = 10000;   # minimum runtime for measurement in ms
  bench = 0;
  #bench_res1[] = NULL;

  if (argc > 1) {
    bench1 = argv[1];
    bench2 = bench1; # set also last benchmark
  }
  if (argc > 2) {
    bench2 = argv[2];
  }
  if (argc > 3) {
    n = argv[3];
  }

  printf("BM Bench v0.4 (awk)\n");

  for (bench = bench1; bench <= bench2; bench++) {
    loops = 1;   # number of loops
    x = 0;     # result from benchmark
    t1 = 0; # timestamp
    # calibration
    while (t1 < 1001) { # we want at least 1 sec calibration time
      printf("Calibrating benchmark %d with loops=%d, n=%d\n", bench, loops, n);
      t1 = get_ms();
      x = run_bench(bench, loops, n);
      t1 = get_ms() - t1;
      printf("x=%d (time: %d ms)\n", x, t1);
      loops *= 2;
      if (x == -1) {
        break;
      }
    }
    if (x != -1) {
      loops /= 2; # div 2
      loops *= int((min_ms / t1) + 1); # integer division!
      printf("Calibration done. Starting measurement with %d loops to get >=%d ms\n", loops, min_ms);

      # measurement
      t1 = get_ms();
      x = run_bench(bench, loops, n);
      t1 = get_ms() - t1;
      printf("x=%d (time: %d ms)\n", x, t1);
      printf("Elapsed time for %d loops: %d ms; estimation for 10 loops: %d ms\n", loops, t1, (t1 * 10 / loops));
      bench_res1[bench] = int(t1 * 10 / loops); # int
    } else {
      bench_res1[bench] = -1;
    }
  }
  printf("Summary for 10 Loops:\n");
  for (bench = bench1; bench <= bench2; bench++) {
    printf("Benchmark %d: %d ms\n", bench, bench_res1[bench]);
  }
  printf("Total elapsed time: %d ms\n", (get_ms() - start_t));
  return 0;
}


BEGIN {
  main(ARGC, ARGV);
  exit;
}
# end
