/*
 * BM Bench - bmbench.c (C)
 * (c) Marco Vieth, 2002-2022
 * http://www.benchmarko.de
 *
 * 06.05.2002 0.01
 * 11.05.2002 0.02  bench01 = (sum 1..n) mod 65536 (integer)
 * 22.05.2002 0.03  bench02 = (sum 1..n) mod 65536 (floating point), bench03 = Sieve of Eratosthenes
 * 20.07.2002 0.04  some errors corrected
 * 24.01.2003 0.05  output format changed
 * 13.04.2003       malloc size for bench_res1 corrected
 * 30.05.2006 0.06  based on version 0.05
 * 05.05.2019 0.07  changed bench 01-03; time interval estimation
 * 03.12.2022 0.072 bench03 corrected, bench05 improved
 *
 *
 * Usage:
 * bmbench [bench1] [bench2] [n]
 *
 */

/*
 * Compile:
 * - UNIX: gcc -Wall -Wtraditional -O2 bmbench.c -o bmbench
 *   (Do NOT use "-pedantic", this will nearly double runtime of benchmark 4!)
 *   (-funroll-loops has a positive impact on bench 00, 01 but should not be used.)
 * - NT (MS C++): cl -W4 -DUse_Windows bmbench.c -o bmbench
 * - Windows/DOS (BC++):  bcc -3 -a -k- -O -Og -Oe -Om -Ov -Ol -Ob -Op -Oi -Z -G -v- -vi- bmbench.c
 *   (-3 is to use x386 instructions...)
 *   (or use bcw...)
 */

 /*
  * All types of gcc warnings:
  * gcc -Wall -Wtraditional -Wshadow -Wpointer-arith -Wcast-qual -Wcast-align -Wconversion -Waggregate-return -Wstrict-prototypes -Wmissing-prototypes -Wmissing-declarations -Wredundant-decls -Wnested-externs -O2 bmbench.c -o bmbench
  *
  */

#define PRG_VERSION "0.072"
#define PRG_LANGUAGE "C"

#include <stdio.h>
#include <stdlib.h>
#include <time.h>  /* only to get date with ctime(), time() */
#include <string.h> /* strcat */

#ifdef _WIN32 /* Visual C++ */
#include <sys/timeb.h>
#if _MSC_VER
#define MY_VERSION "Microsoft C/C++-Compiler %d\n", _MSC_VER
#endif
#else
#include <sys/time.h> /* gettimeofday */
#endif

#ifdef __GNUC__
#define MY_VERSION "GCC %d.%d\n", __GNUC__, __GNUC_MINOR__
#endif

#if !defined MY_VERSION
#define MY_VERSION "Compiled on ??\n"
#endif


struct bm_timeval {
  long tv_sec;
  long tv_usec;
};

static struct bm_timeval g_start_ts = { 0, 0 };

/* static char g_tsType[20] = ""; */ /* type of time stamp source */
static double g_tsPrecMs = 0; /* measured time stamp precision */
static int g_tsPrecCnt = 0; /* time stamp count (calls) per precision interval (until time change) */
static int g_tsMeasCnt = 0; /* last measured count */


/*
 * General description for benchmark test functions
 * benchxx - benchmark
 * <description>
 * in: loops = number of loops
 *         n = maximum number (assumed even, normally n=1000000)
 * out:    x = <output decription>
 *
 * loops may be increased to produce a longer runtime without changing the result.
 */


/*
 * bench00 (Integer 16 bit)
 * (sum of 1..n) mod 65536
 */
static int bench00(int loops, int n, unsigned short int check) {
  unsigned short int x = 0;
  /* short int sum1 = (short int)((n / 2) * (n + 1)); */ /* assuming n even! */
  /* (sum1..1000000 depends on type: 500000500000 (floating point), 1784293664 (32bit), 10528 (16 bit) */
  unsigned int n_div_65536 = (unsigned int)(n >> 16);
  unsigned int n_mod_65536 = (unsigned int)(n & 0xffff);
  /* fprintf(stderr, "Test(bench%d): x=%f, %ld, %ld\n", 1, (double)sum, (long)fmod(sum, 2147483648L), (long)fmod(sum, 65536L)); */
  while (loops-- > 0 && x == 0) {
    unsigned int i;
    unsigned int j;
    for (i = n_div_65536; i > 0; i--) {
      for (j = 65535U; j > 0; j--) {
        x += j;
      }
    }
    for (j = n_mod_65536; j > 0; j--) {
      x += j;
    }
	x -= check;
  }
  return (int)(x & 0xffff);
}


/*
 * bench01 (Integer 16/32 bit)
 * (arithmetic mean of 1..n)
 */
static int bench01(int loops, int n, int check) {
  int x = 0;
  while (loops-- > 0 && x == 0) {
    int sum = 0;
    int i;
    for (i = 1; i <= n; i++) {
      sum += i;
      if (sum >= n) { // to avoid numbers above 2*n, divide by n using subtraction
        sum -= n;
        x++;
      }
    }
    x -= check;
  }
  return x;
}


/*
 * bench02 (Floating Point, normally 64 bit)
 * (arithmetic mean of 1..n) mod 65536
 */
static int bench02(int loops, int n, int check) {
  int x = 0;
  while (loops-- > 0 && x == 0) {
    double sum = 0;
    int i;
    for (i = 1; i <= n; i++) {
      sum += i;
      if (sum >= n) { // to avoid numbers above 2*n, divide by n using subtraction
        sum -= n;
        x++;
      }
    }
    x -= check;
  }
  return x;
}


/*
 * bench03 (Integer)
 * number of primes below n (Sieve of Eratosthenes)
 * Example: n=500000 => x=41538 (expected), n=1000000 => x=78498
 */
static int bench03(int loops, int n, int check) {
  typedef unsigned char sieve_t;
  int x = 0; /* number of primes below n */

  n /= 2; /* compute only up to n/2 */
  int nHalf = n >> 1;

  sieve_t *sieve = (sieve_t *)malloc(((unsigned)nHalf + 1) * sizeof(sieve_t));
  if (sieve == NULL) {
    return -1; /* error */
  }

  while (loops-- > 0 && x == 0) {
    int i;
    /* initialize sieve */
    for (i = 0; i <= nHalf; i++) {
      sieve[i] = 0;
    }
    
    /* compute primes */
    i = 0;
    int m = 3;
    x++; /* 2 is prime */
    while (m * m < n) {
      if (!sieve[i]) {
        x++; /* m is prime */
        int j = (m * m - 3) >> 1; /* div 2 */
        while (j < nHalf) {
          sieve[j] = 1;
          j += m;
        }
      }
      i++;
      m += 2;
    }

    /* count remaining primes */
    while (m <= n) {
      if (!sieve[i]) {
        x++; /* m is prime */
      }
      i++;
      m += 2;
    }
    x -= check;
  }

  /* free memory */
  if (sieve != NULL) {
    free(sieve);
    sieve = NULL;
  }
  return x;
}


/*
 * bench04 (Integer 32 bit)
 * nth random number number
 * Random number generator taken from
 * Raj Jain: The Art of Computer Systems Performance Analysis, John Wiley & Sons, 1991, page 442-444.
 * It needs longs with at least 32 bit.
 * Starting with x0=1, x10000 should be 1043618065, x1000000 = 1227283347.
 */
#define BENCH04_M 2147483647L /* modulus, do not change! */
#define BENCH04_A 16807       /* multiplier */
#define BENCH04_Q 127773L     /* m div a */
#define BENCH04_R 2836        /* m mod a */
static int bench04(int loops, int n, int check) {
  int x = 0; 
  int i;
  while (loops-- > 0 && x == 0) {
    x++;
    for (i = 1; i <= n; i++) {
      x = BENCH04_A * (x % BENCH04_Q) - BENCH04_R * (x / BENCH04_Q); /* x div q */
      if (x <= 0) {
        x += BENCH04_M; /* x is new random number */
      }
    }
    x -= check;
  }
  return x;
}


/*
 * bench05 (Integer 32 bit)
 * n over n/2 mod 65536 (Pascal's triangle)
 * (we just need to store the last 2 lines of computation)
 */
 static int bench05(int loops, int n_p, int check) {
  int x = 0;
  typedef int line_t;
  int n = (int)(n_p / 500);
  int k = n / 2;
  int i, j, min1;
  /* allocate memory ... */
  /* pas_t  *pas1[2]; */
  if ((n - k) < k) {
    k = n - k; /* keep k minimal with  n over k  =  n over n-k */
  }

  line_t *line = (line_t *)malloc(((unsigned)k + 1) * sizeof(line_t));
  if (line == NULL) {
    return -1; /* error */
  }

  line_t *lastLine = (line_t *)malloc(((unsigned)k + 1) * sizeof(line_t));
  if (lastLine == NULL) {
    return -1; /* error */
  }
     
  line[0] = 1;
  lastLine[0] = 1; /* set first column */

  while (loops-- > 0 && x == 0) {
    for (i = 3; i <= n; i++) {
      //i_mod_2 = i % 2;
      min1 = (i - 1) / 2;
      if (k < min1) {
        min1 = k;
      }
      line[1] = i; /* second column is i */
      for (j = 2; j <= min1; j++) { /* up to min((i-1)/2, k) */
        line[j] = (lastLine[j - 1] + lastLine[j]);
      }
      if ((min1 < k) && ((i & 1) == 0)) { /* new element */
        line[min1 + 1] = 2 * lastLine[min1];
      }
      line_t *tempLine = lastLine;
      lastLine = line;
      line = tempLine;
    }
    x += lastLine[k] & 0xffff;
    x -= check;
  }

  /* free memory */
  if (line != NULL) {
    free(line);
    line = NULL;
  }
  if (lastLine != NULL) {
    free(lastLine);
    lastLine= NULL;
  }
  return x;
}


/*
 * run a benchmark
 * in: bench = benchmark to use
 *     loops = number of loops
 *         n = maximum number (used in some benchmarks to define size of workload)
 * out:    x = result
 */
static int run_bench(int bench, int loops, int n, int check) {
  int x = 0;
  switch(bench) {
    case 0:
      x = bench00(loops, n, (unsigned short int)check);
    break;

    case 1:
      x = bench01(loops, n, check);
    break;

    case 2:
      x = bench02(loops, n, check);
    break;

    case 3:
      x = bench03(loops, n, check);
    break;

    case 4:
      x = bench04(loops, n, check);
    break;

    case 5:
      x = bench05(loops, n, check);
    break;
	
    default:
      fprintf(stderr, "Error: Unknown benchmark: %d\n", bench);
    break;
  }

  x += check;
  if (x != check) {
    fprintf(stderr, "Error(bench%d): x=%d\n", bench, x);
    x = -1; /* exit */
  }
  return x;
}


static int bench03Check(int n) {
  int x = 0, j, i, isPrime;

  n = (n / 2) | 0; // compute only up to n/2

  for (j = 2; j <= n; j++) {
    isPrime = 1;
    for (i = 2; i * i <= j; i++) {
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


static int getCheck(int bench, int n) {
  int check;
  switch(bench) {
    case 0:
	    check = ((n / 2) * (n + 1)) & 0xffff; // short int
    break;

    case 1:
	    check = (n + 1) / 2;
    break;

    case 2:
      check = (n + 1) / 2;
    break;

    case 3:
      check = (n == 1000000) ? 41538 : bench03Check(n);
    break;

    case 4:
      check = (n == 1000000) ? 1227283347 : bench04(1, n, 0); // bench04 not a real check
    break;

    case 5:
      check = (n == 1000000) ? 27200 : bench05(1, n, 0); // bench05 not a real check
    break;
	
    default:
      fprintf(stderr, "Error: Unknown benchmark: %d\n", bench);
      check = -1;
    break;
  }

  return check;
}

static struct bm_timeval get_raw_ts(void) {
  struct bm_timeval bmtv;
#ifdef _WIN32
  struct _timeb tb;
  _ftime(&tb);
  //ltime = (bmtime_t)tb.time * 1000.0 + tb.millitm;
  bmtv.tv_sec = tb.time;
  bmtv.tv_usec = tb.millitm * 1000;
#else // e.g. gcc
  struct timeval tv;
  gettimeofday(&tv, NULL);
  //ltime = tv.tv_sec * 1000.0 + tv.tv_usec / 1000.0;
  bmtv.tv_sec = tv.tv_sec;
  bmtv.tv_usec = tv.tv_usec;
  /*
  if (!initialTime) {
    initialTime = ltime;
  }
  ltime -= initialTime;
  */
#endif
  return bmtv;
}


/* https://www.gnu.org/software/libc/manual/html_node/Elapsed-Time.html */
static int timeval_subtract(struct bm_timeval *result, struct bm_timeval *x, struct bm_timeval *y) {
  /* Perform the carry for the later subtraction by updating y. */
  if (x->tv_usec < y->tv_usec) {
    int nsec = (y->tv_usec - x->tv_usec) / 1000000 + 1;
    y->tv_usec -= 1000000 * nsec;
    y->tv_sec += nsec;
  }
  if (x->tv_usec - y->tv_usec > 1000000) {
    int nsec = (x->tv_usec - y->tv_usec) / 1000000;
    y->tv_usec += 1000000 * nsec;
    y->tv_sec -= nsec;
  }

  /* Compute the time remaining to wait. tv_usec is certainly positive. */
  result->tv_sec = x->tv_sec - y->tv_sec;
  result->tv_usec = x->tv_usec - y->tv_usec;

  /* Return 1 if result is negative. */
  return x->tv_sec < y->tv_sec;
}

static int get_ts(void) {
  struct bm_timeval bmtv = get_raw_ts();
  struct bm_timeval restv;

  timeval_subtract(&restv, &bmtv, &g_start_ts);
  return restv.tv_sec * 1000000 + restv.tv_usec;
}

static double conv_ms(int ts) {
    return ts / 1000.0;
}


static time_t *get_time1(void) {
  static time_t t;  /* need static because we return a pointer... */
  t = time(NULL);
  return(&t);
}


static double correctTime(double tMeas, double tMeas2, int measCount) {
  int tsPrecCnt = g_tsPrecCnt;

  if (measCount < tsPrecCnt) {
    tMeas += g_tsPrecMs * ((tsPrecCnt - measCount) / (double)tsPrecCnt); // ts + correction
    //printf("DEBUG: correctTime: tMeas=%f cntDiff=%d corr=%f\n", tMeas, (tsPrecCnt - measCount), tsPrecMs * ((tsPrecCnt - measCount) / (double)tsPrecCnt));
    //printf("DEBUG: \n");
    //TTT bench01(1000, 1000000, ((1000000 + 1) / 2) | 0); 
    if (tMeas > tMeas2) {
        tMeas = tMeas2; // cannot correct
    }   
  }
  return tMeas;
}

static double getPrecMs(int stopFlg) {
  int measCount = 0;

  int tMeas0 = get_ts();
  int tMeas = tMeas0;
  while (tMeas <= tMeas0) {
    tMeas = get_ts();
    measCount++;
  }
  g_tsMeasCnt = measCount; /* memorize count */

  double tMeasD = (!stopFlg) ? conv_ms(tMeas) : correctTime(conv_ms(tMeas0), conv_ms(tMeas), measCount);
  return tMeasD;
}

/* usually only needed if time precision is low, e.g. one second */
static void determineTsPrecision(void) {
  g_start_ts = get_raw_ts(); // memorize start time //TTT copy?

  double tMeas0 = getPrecMs(0);
  double tMeas1 = getPrecMs(0);
  g_tsPrecMs = tMeas1 - tMeas0;
  g_tsPrecCnt = g_tsMeasCnt;

  /* do it again */
  tMeas0 = tMeas1;
  tMeas1 = getPrecMs(0);
  if (g_tsMeasCnt > g_tsPrecCnt) { /* taker maximum count */
    //printf("DEBUG: determineTsPrecision: Overwriting old measurement: tsPrecMs=%f tsPrecCnt=%d\n", g_tsPrecMs, g_tsPrecCnt);
    g_tsPrecCnt = g_tsMeasCnt;
    g_tsPrecMs = tMeas1 - tMeas0;
  }
  //printf("DEBUG: determineTsPrecision: tsPrecMs=%f tsPrecCnt=%d, tmeas0=%f tMeas1=%f tsMeasCnt=%d\n", g_tsPrecMs, g_tsPrecCnt, tMeas0, tMeas1, g_tsMeasCnt);
}


/* Here we compute the number of "significant" bits for positive numbers (which means 53 for double) */
static int checkbits_short1(void) {
  short num = 1;
  short last_num = 0;
  int bits = 0;
  do {
    last_num = num;
    num *= 2;
    num++;
    bits++;
	//printf("DEBUG: bits=%d num=%d last_num=%d, (num-1)/2=%d\n", bits, num, last_num, (num - 1) / 2);
  } while ( (((num - 1) / 2) == last_num) && (bits < 101) );
  return bits;
}

static int checkbits_int1(void) {
  int num = 1;
  int last_num = 0;
  int bits = 0;
  do {
    last_num = num;
    num *= 2;
    num++;
    bits++;
	//printf("DEBUG: bits=%d num=%u last_num=%u, (num-1)/2=%u\n", bits, num, last_num, (num - 1) / 2);
  } while ( (num > last_num) && (((num - 1) / 2) == last_num) && (bits < 101) );
  return bits;
}

/*
static int checkbits_ulonglong(void) {
  unsigned long long num = 1;
  unsigned long long last_num = 0;
  int bits = 0;
  do {
    last_num = num;
    num *= 2;
    num++;
    bits++;
	//printf("DEBUG: bits=%d num=%u last_num=%u, (num-1)/2=%u\n", bits, num, last_num, (num - 1) / 2);
  } while ( (num > last_num) && (((num - 1) / 2) == last_num) && (bits < 101) );
  return bits;
}
*/


static int checkbits_float1(void) {
  float num = 1.0;
  float last_num = 0.0;
  int bits = 0;
  do {
    last_num = num;
    num *= 2.0F;
    num++;
    bits++;
  } while ( (((num - 1.0) / 2.0) == last_num) && (bits < 101) );
  return bits;
}

static int checkbits_double1(void) {
  double num = 1.0;
  double last_num = 0.0;
  int bits = 0;
  do {
    last_num = num;
    num *= 2.0;
    num++;
    bits++;
  } while ( (((num - 1.0) / 2.0) == last_num) && (bits < 101) );
  return bits;
}



static void print_info(void) {
  printf("BM Bench v%s (%s) -- (short:%d int:%d float:%d double:%d tsMs:%lf tsCnt:%d) ", PRG_VERSION, PRG_LANGUAGE,
    checkbits_short1(), checkbits_int1(), checkbits_float1(), checkbits_double1(),
    g_tsPrecMs, g_tsPrecCnt);
  printf(MY_VERSION); // maybe multiple arguments!
  printf("(c) Marco Vieth, 2002-2022\n");
  printf("Date: %s", ctime(get_time1()));
}


#define MAX_LANGUAGE_LEN1 10

static void print_results(int bench1, int bench2, double *bench_res1) {
  char str[MAX_LANGUAGE_LEN1 + 1] = "";
  int i;
  int bench;
  printf("\nThroughput for all benchmarks (loops per sec):\n");
  for (i = (int)strlen(PRG_LANGUAGE); i < MAX_LANGUAGE_LEN1; i++) {
    strcat(str, " ");
  }
  printf("BMR (%s)%s: ", PRG_LANGUAGE, str);

  for (bench = bench1; bench <= bench2; bench++) {
    printf("%9.3f ", bench_res1[bench]);
  }
  printf("\n");
  printf("\n");
}


/* -------------------------------------------------------- */


static double measureBench(int bench, int n, int check) {
  int cali_ms = 1001; /* const */
  int delta_ms = 100; /* const */
  int max_ms = 10000; /* const */
  
  int loops = 1;   /* number of loops */
  int x = 0;     /* result from benchmark */
  double tMeas = 0; /* measured time */
  double tEsti = 0; /* estimated time */
  double throughput = 0;

  printf("Calibrating benchmark %d with n=%d, check=%d\n", bench, n, check);
  while (throughput == 0) {
    tMeas = getPrecMs(0);
    x = run_bench(bench, loops, n, check);
    tMeas = getPrecMs(1) - tMeas;

    double t_delta = (tEsti > tMeas) ? (tEsti - tMeas) : (tMeas - tEsti); /* compute difference abs(measures-estimated) */
    double loops_p_sec = (tMeas > 0) ? ((loops * 1000.0) / tMeas) : 0;
    //printf("DEBUG: (time=%9.3f ms, delta=%9.3f ms, tsMeasCnt=%d)\n",tMeas, t_delta, g_tsMeasCnt);
    printf("%10.3f/s (time=%9.3f ms, loops=%7d, delta=%9.3f ms)\n", loops_p_sec, tMeas, loops, t_delta);
  
    if (x == -1) { /* some error? */
      throughput = -1;
    } else if ((tEsti > 0) && (t_delta < delta_ms)) { /* do we have some estimated/expected time smaller than delta_ms=100? */
      throughput = loops_p_sec; /* yeah, set measured loops per sec */
      printf("Benchmark %d (%s): %.3f/s (time=%.3f ms, loops=%d, delta=%.3f ms)\n", bench, PRG_LANGUAGE, loops_p_sec, tMeas, loops, t_delta);
    } else if (tMeas > max_ms) {
      printf("Benchmark %d (%s): Time already > %d ms. No measurement possible.\n", bench, PRG_LANGUAGE, max_ms);
      throughput = (loops_p_sec > 0) ? -loops_p_sec : -1; /* cannot rely on measurement, so set to negative */
    } else {
      int scale_fact;
      if (tMeas == 0) {
        scale_fact = 50;
      } else if (tMeas < cali_ms) {
        scale_fact = (int)((cali_ms + 100) / tMeas) + 1; // scale a bit up to 1100 ms (cali_ms+100)
      } else {
        scale_fact = 2;
      }
      /* scale a bit up to 1100 ms (cali_ms+100) */
      //printf("DEBUG: scale_fact=%lf, tMeas=%lf, tEsti=%lf\n", (double)scale_fact, tMeas, tEsti);
      loops *= scale_fact;
      tEsti = tMeas * scale_fact;
    }
    fflush(stdout);
  }
  return throughput;
}


/* #define MAX_BENCH (5 + 1) */

static int start_bench(int bench1, int bench2, int n) {
  double *bench_res = NULL;
  int bench;
  
  print_info();

  unsigned int bench_res_size = ((unsigned)bench2 + 1) * sizeof(double);
  if ((bench_res = (double *)malloc(bench_res_size)) == NULL) {
    fprintf(stderr, "Error: malloc(bench_res)\n");
    exit(1);
  }

  for (bench = bench1; bench <= bench2; bench++) {
    int check = getCheck(bench, n);
    double throughput = (check > 0) ? measureBench(bench, n, check) : -1;
    bench_res[bench] = throughput;
  }

  print_results(bench1, bench2, bench_res);

  if (bench_res != NULL) {
    free(bench_res);
    bench_res = NULL;
  }
  return 0;
}


int main(int argc, char **argv) {
  int bench1 = 0;       /* first benchmark to test */
  int bench2 = 5;       /* last benchmark to test */
  int n = 1000000L;   /* maximum number */
  int rc;

  if (argc > 1) {
    bench1 = atoi(argv[1]);
    bench2 = bench1; /* set also last benchmark */
  }
  if (argc > 2) {
    bench2 = atoi(argv[2]);
  }
  if (argc > 3) {
    n = atol(argv[3]);
  }
  
  determineTsPrecision();
  rc = start_bench(bench1, bench2, n);
    
  printf("Total elapsed time: %d ms\n", (int)(conv_ms(get_ts())));
  return rc;
}
/* end */
