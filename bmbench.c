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
 * 19.02.2023 0.08  bench05 optimized
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
 * - (Digital Mars C++ compiler) dmc.exe -o bmbench.c
 * - Windows/DOS (BC++):  bcc -3 -a -k- -O -Og -Oe -Om -Ov -Ol -Ob -Op -Oi -Z -G -v- -vi- bmbench.c
 *   (-3 is to use x386 instructions...)
 *   (or use bcw...)
 */

/*
// Turbo C
// https://stackoverflow.com/questions/45914070/how-to-get-current-time-in-c-using-gettime
// #include <dos.h>
// struct time tm;
// gettime(&tm);
// printf("System time is: %d : %d : %d\n",tm.ti_hour, tm.ti_min, tm.ti_sec);
*/

/* #define _TURBOC */ /* define this, if needed */


 /*
  * All types of gcc warnings:
  * gcc -Wall -Wtraditional -Wshadow -Wpointer-arith -Wcast-qual -Wcast-align -Wconversion -Waggregate-return -Wstrict-prototypes -Wmissing-prototypes -Wmissing-declarations -Wredundant-decls -Wnested-externs -O2 bmbench.c -o bmbench
  *
  */

#define PRG_VERSION "0.08"
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
#ifdef _TURBOC /* need to be defined, if needed */
#include <dos.h>
#else 
#include <sys/time.h> /* gettimeofday */
#endif
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

static double g_tsPrecMs = 0; /* measured time stamp precision */
static int g_tsPrecCnt = 0; /* time stamp count (calls) per precision interval (until time change) */
static int g_tsMeasCnt = 0; /* last measured count */
static int g_cali_ms = 1001;


/*
 * General description for benchmark test functions
 * benchxx - benchmark
 * <description>
 * in:  n = maximum number (assumed even, normally n=1000000)
 * out: x = <output decription>
 *
 */


/*
 * bench00 (Integer 16 bit)
 * (sum of 1..n) mod 65536
 */
static int bench00(int n) {
  unsigned short int x = 0;
  unsigned int n_div_65536 = (unsigned int)(n >> 16);
  unsigned int n_mod_65536 = (unsigned int)(n & 0xffff);
  /* fprintf(stderr, "Test(bench%d): x=%f, %ld, %ld\n", 1, (double)sum, (long)fmod(sum, 2147483648L), (long)fmod(sum, 65536L)); */
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
  return (int)(x & 0xffff);
}


/*
 * bench01 (Integer 16/32 bit)
 * (arithmetic mean of 1..n)
 */
static int bench01(int n) {
  int x = 0;
  int sum = 0;
  int i;
  for (i = 1; i <= n; i++) {
    sum += i;
    if (sum >= n) { /* to avoid numbers above 2*n, divide by n using subtraction */
      sum -= n;
      x++;
    }
  }
  return x;
}


/*
 * bench02 (Floating Point, normally 64 bit)
 * (arithmetic mean of 1..n)
 */
static int bench02(int n) {
  int x = 0;
  double sum = 0;
  int i;
  for (i = 1; i <= n; i++) {
    sum += i;
    if (sum >= n) { /* to avoid numbers above 2*n, divide by n using subtraction */
      sum -= n;
      x++;
    }
  }
  return x;
}


static int *benchMemPtr = NULL; /* reuse memory for bench03, bench05 */

/*
 * bench03 (Integer)
 * number of primes below n (Sieve of Eratosthenes)
 * Example: n=500000 => x=41538 (expected), n=1000000 => x=78498
 */
static int bench03(int n) {
  typedef unsigned char sieve_t;
  int nHalf = n >> 1;
  int i, j, m, x;
  sieve_t *sieve;

  /* allocate memory ... */
  if (benchMemPtr == NULL) {
    benchMemPtr = (int *)malloc(((unsigned)nHalf + 1) * sizeof(sieve_t));
  }
  sieve = (sieve_t *)benchMemPtr;
  if (sieve == NULL) {
    return -1; /* error */
  }

  /* initialize sieve */
  for (i = 0; i <= nHalf; i++) {
    sieve[i] = 0;
  }
  
  /* compute primes */
  i = 0;
  m = 3;
  x = 1; /* number of primes below n (2 is prime) */

  while (m * m <= n) {
    if (!sieve[i]) {
      x++; /* m is prime */
      j = (m * m - 3) >> 1; /* div 2 */
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

  /* free memory */
  /*
  if (sieve != NULL) {
    free(sieve);
    sieve = NULL;
  }
  */
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
static int bench04(int n) {
  int i;
  int x = 1; // 1=Last random value
  for (i = 1; i <= n; i++) {
    x = BENCH04_A * (x % BENCH04_Q) - BENCH04_R * (x / BENCH04_Q); /* x div q */
    if (x <= 0) {
      x += BENCH04_M; /* x is new random number */
    }
  }
  return x;
}


/*
 * bench05 (Integer 32 bit)
 * (n choose n/2) mod 65536 (Central Binomial Coefficient mod 65536)
 * Using dynamic programming and Pascal's triangle, storing only one line
 * Instead of nCk mod 65536 with k=n/2, we compute the product of (n/2)Ck mod 65536 with k=0..n/4 (Vandermonde folding)
 * Example: (2000 choose 1000) mod 65536 = 27200
 */
  static int bench05(int n) {
  typedef int line_t;
  int k, i, j, min1, prev, num, x;
  line_t *line;

  /* Instead of nCk with k=n/2, we compute the product of (n/2)Ck with k=0..n/4 */
  n /= 2;

  k = n / 2;
  if ((n - k) < k) {
    k = n - k; /* keep k minimal with  n over k  =  n over n-k */
  }

  /* allocate memory ... */
  if (benchMemPtr == NULL) {
    benchMemPtr = (line_t *)malloc(((unsigned)k + 1) * sizeof(line_t));
  }
  line = benchMemPtr;
  if (line == NULL) {
    return -1; /* error */
  }

  line[0] = 1;
  line[1] = 2; /* for line 2, second column is 2 */

  /* compute lines of Pascal's triangle */
  for (i = 3; i <= n; i++) {
    min1 = (i - 1) / 2;
    if ((i & 1) == 0) { /* new element? */
      line[min1 + 1] = 2 * line[min1];
    }
    prev = line[1];
    for (j = 2; j <= min1; j++) {
      num =  line[j];
      line[j] += prev;
      prev = num;
    }
    line[1] = i; /* second column is i */
  }

  /* compute sum of ((n/2)Ck)^2 mod 65536 for k=0..n/2 */
  x = 0;
  for (j = 0; j < k; j++) {
    x += 2 * line[j] * line[j]; /* add nCk and nC(n-k) */
	}
	x += line[k] * line[k]; /* we assume that k is even, so we need to take the middle element */

  return x & 0xffff;
}


static int (*benchList[5+1])(int) = {
  bench00, bench01, bench02, bench03, bench04, bench05
};

/*
 * run a benchmark
 * in: bench = benchmark to use
 *     loops = number of loops
 *         n = maximum number (used in some benchmarks to define size of workload)
 * out:    x = result
 */
static int run_bench(int bench, int loops, int n, int check) {
  int x = 0;
  int (*benchPtr)(int);

  if (bench > 5) {
    fprintf(stderr, "Error: Unknown benchmark: %d\n", bench);
  }
  benchPtr = benchList[bench];

  if (benchMemPtr != NULL) { /* should not occur */
    fprintf(stderr, "Error(bench%d): benchMemPtr != NULL\n", bench);
  }

  while (loops-- > 0 && x == 0) {
    x = benchPtr(n);
    x -= check;
  }

  x += check;
  if (x != check) {
    fprintf(stderr, "Error(bench%d): x=%d\n", bench, x);
    x = -1; /* exit */
  }

  /* free memory */
  if (benchMemPtr != NULL) {
    free(benchMemPtr);
    benchMemPtr = NULL;
  }

  return x;
}


static int bench03Check(int n) {
  int x, j, i, isPrime;

  if (n == 500000) {
    x = 41538;
  } else {
    x = 1; // 2 is prime
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
  }
  return x;
}


static int getCheck(int bench, int n) {
  int check;

  if (benchMemPtr != NULL) { /* should not occur */
    fprintf(stderr, "Error(bench%d): benchMemPtr != NULL\n", bench);
  }

  switch(bench) {
    case 0: /* (n / 2) * (n + 1) */
	    //check = ((n / 2) * (n + 1)) & 0xffff; /* short int */
      check = (((n + (n & 1)) >> 1) * (n + 1 - (n & 1))) & 0xffff; /* 10528 for n=1000000 */
    break;

    case 1:
	    check = (n + 1) / 2;
    break;

    case 2:
      check = (n + 1) / 2;
    break;

    case 3:
      check = bench03Check(n);
    break;

    case 4:
      check = (n == 1000000) ? 1227283347 : bench04(n); /* bench04 not a real check */
    break;

    case 5:
      check = (n == 5000) ? 17376 : bench05(n); /* bench05 not a real check */
    break;
	
    default:
      fprintf(stderr, "Error: Unknown benchmark: %d\n", bench);
      check = -1;
    break;
  }

  /* free memory */
  if (benchMemPtr != NULL) {
    free(benchMemPtr);
    benchMemPtr = NULL;
  }

  return check;
}


static struct bm_timeval get_raw_ts(void) {
  struct bm_timeval bmtv;
#ifdef _WIN32
  struct _timeb tb;
  _ftime(&tb);
  /* ltime = (bmtime_t)tb.time * 1000.0 + tb.millitm; */
  bmtv.tv_sec = tb.time;
  bmtv.tv_usec = tb.millitm * 1000;
#else
#ifdef _TURBOC
  struct time tm;
  gettime(&tm);
  bmtv.tv_sec = tm.ti_hour * 3600 + tm.ti_min * 60 + tm.ti_sec;
  bmtv.tv_usec = 0;
#else /* e.g. gcc */
  struct timeval tv;
  gettimeofday(&tv, NULL);
  /* ltime = tv.tv_sec * 1000.0 + tv.tv_usec / 1000.0; */
  bmtv.tv_sec = tv.tv_sec;
  bmtv.tv_usec = tv.tv_usec;
  /*
  if (!initialTime) {
    initialTime = ltime;
  }
  ltime -= initialTime;
  */
#endif
#endif
  return bmtv;
}


/* https://www.gnu.org/software/libc/manual/html_node/Elapsed-Time.html */
static int timeval_subtract(struct bm_timeval *result, struct bm_timeval *x, struct bm_timeval *y) {
  /* Perform the carry for the later subtraction by updating y */
  if (x->tv_usec < y->tv_usec) {
    int nsec = (int)((y->tv_usec - x->tv_usec) / 1000000 + 1);
    y->tv_usec -= 1000000 * nsec;
    y->tv_sec += nsec;
  }
  if (x->tv_usec - y->tv_usec > 1000000) {
    int nsec = (int)((x->tv_usec - y->tv_usec) / 1000000);
    y->tv_usec += 1000000 * nsec;
    y->tv_sec -= nsec;
  }

  /* Compute the time remaining to wait. tv_usec is certainly positive */
  result->tv_sec = x->tv_sec - y->tv_sec;
  result->tv_usec = x->tv_usec - y->tv_usec;

  /* Return 1 if result is negative */
  return x->tv_sec < y->tv_sec;
}

static int get_ts(void) {
  struct bm_timeval bmtv = get_raw_ts();
  struct bm_timeval restv;

  timeval_subtract(&restv, &bmtv, &g_start_ts);
  return (int)(restv.tv_sec * 1000000 + restv.tv_usec);
}

static double conv_ms(int ts) {
    return ts / 1000.0;
}


static double correctTime(double tMeas, double tMeas2, int measCount) {
  int tsPrecCnt = g_tsPrecCnt;

  if (measCount < tsPrecCnt) {
    tMeas += g_tsPrecMs * ((tsPrecCnt - measCount) / (double)tsPrecCnt); /* ts + correction */
    if (tMeas > tMeas2) {
        tMeas = tMeas2; /* cannot correct */
    }   
  }
  return tMeas;
}

static double getPrecMs(int stopFlg) {
  int measCount = 0;
  double tMeasD;

  int tMeas0 = get_ts();
  int tMeas = tMeas0;
  while (tMeas <= tMeas0) {
    tMeas = get_ts();
    measCount++;
  }
  g_tsMeasCnt = measCount; /* memorize count */

  tMeasD = (!stopFlg) ? conv_ms(tMeas) : correctTime(conv_ms(tMeas0), conv_ms(tMeas), measCount);
  return tMeasD;
}

/* usually only needed if time precision is low, e.g. one second */
static void determineTsPrecision(void) {
  double tMeas0, tMeas1;
  g_start_ts = get_raw_ts(); /* memorize start time */

  tMeas0 = getPrecMs(0);
  tMeas1 = getPrecMs(0);
  g_tsPrecMs = tMeas1 - tMeas0;
  g_tsPrecCnt = g_tsMeasCnt;

  /* do it again */
  tMeas0 = tMeas1;
  tMeas1 = getPrecMs(0);
  if (g_tsMeasCnt > g_tsPrecCnt) { /* taker maximum count */
    /* printf("DEBUG: determineTsPrecision: Overwriting old measurement: tsPrecMs=%f tsPrecCnt=%d\n", g_tsPrecMs, g_tsPrecCnt); */
    g_tsPrecCnt = g_tsMeasCnt;
    g_tsPrecMs = tMeas1 - tMeas0;
  }
  /* printf("DEBUG: determineTsPrecision: tsPrecMs=%f tsPrecCnt=%d, tmeas0=%f tMeas1=%f tsMeasCnt=%d\n", g_tsPrecMs, g_tsPrecCnt, tMeas0, tMeas1, g_tsMeasCnt); */
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
	/* printf("DEBUG: bits=%d num=%d last_num=%d, (num-1)/2=%d\n", bits, num, last_num, (num - 1) / 2); */
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


/*
static time_t *get_time1(void) {
  static time_t t;  / * need static because we return a pointer... * /
  t = time(NULL);
  return(&t);
}
*/

static void print_info(void) {
  char str[40];
  time_t t = time(NULL);
  struct tm *tm1;
  
  printf("BM Bench v%s (%s) -- (short:%d int:%d float:%d double:%d tsMs:%lf tsCnt:%d) ", PRG_VERSION, PRG_LANGUAGE,
    checkbits_short1(), checkbits_int1(), checkbits_float1(), checkbits_double1(),
    g_tsPrecMs, g_tsPrecCnt);
  printf(MY_VERSION); /* maybe multiple arguments! */
  printf("(c) Marco Vieth, 2002-2023\n");

  tm1 = localtime(&t);
  if (tm1 == NULL) {
    perror("localtime");
    return;
  }

  /* https://www.tutorialspoint.com/c_standard_library/c_function_strftime.htm */
  if (strftime(str, sizeof(str), "%Y-%m-%d %H:%M:%S\n", tm1) == 0) {
    perror("localtime");
    return;
  }

  /* printf("Date: %s", ctime(get_time1())); */
  printf("Date: %s", str);
}


#define MAX_LANGUAGE_LEN1 10

static void print_results(int bench1, int bench2, double *bench_res1) {
  char str[MAX_LANGUAGE_LEN1 + 1] = "";
  int i;
  int bench;
  printf("\nThroughput for all benchmarks (loops per sec):\n");
  for (i = (int)strlen(PRG_LANGUAGE); i < (int)sizeof(str); i++) {
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
  int delta_ms = 100; /* const */
  int max_ms = 10000; /* const */
  int cali_ms = g_cali_ms;
  
  int loops = 1;   /* number of loops */
  int x = 0;     /* result from benchmark */
  double tMeas = 0; /* measured time */
  double tEsti = 0; /* estimated time */
  double throughput = 0;
  double t_delta, loops_p_sec;

  printf("Calibrating benchmark %d with n=%d, check=%d\n", bench, n, check);
  while (throughput == 0) {
    tMeas = getPrecMs(0);
    x = run_bench(bench, loops, n, check);
    tMeas = getPrecMs(1) - tMeas;

    t_delta = (tEsti > tMeas) ? (tEsti - tMeas) : (tMeas - tEsti); /* compute difference abs(measures-estimated) */
    loops_p_sec = (tMeas > 0) ? ((loops * 1000.0) / tMeas) : 0;
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
        scale_fact = (int)((cali_ms + 100) / tMeas) + 1; /* scale a bit up to 1100 ms (cali_ms+100) */
      } else {
        scale_fact = 2;
      }
      /* scale a bit up to 1100 ms (cali_ms+100) */
      /* printf("DEBUG: scale_fact=%lf, tMeas=%lf, tEsti=%lf\n", (double)scale_fact, tMeas, tEsti); */
      loops *= scale_fact;
      tEsti = tMeas * scale_fact;
    }
    fflush(stdout);
  }
  return throughput;
}


static int start_bench(int bench1, int bench2, int n) {
  double *bench_res = NULL;
  int bench, n2, check;
  unsigned int bench_res_size;
  double throughput;
  
  print_info();

  bench_res_size = ((unsigned)bench2 + 1) * sizeof(double);
  if ((bench_res = (double *)malloc(bench_res_size)) == NULL) {
    fprintf(stderr, "Error: malloc(bench_res)\n");
    exit(1);
  }

  for (bench = bench1; bench <= bench2; bench++) {
    n2 = n;
    if (bench == 3) {
      n2 = n2 / 2;
    } else if (bench == 5) {
      n2 = n2 / 200;
    }
    check = getCheck(bench, n2);
    throughput = (check > 0) ? measureBench(bench, n2, check) : -1;
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
    n = atoi(argv[3]);
  }
  if (argc > 4) {
    g_cali_ms = atoi(argv[4]);
  }
  
  determineTsPrecision();
  rc = start_bench(bench1, bench2, n);
    
  printf("Total elapsed time: %d ms\n", (int)(conv_ms(get_ts())));
  return rc;
}
/* end */
