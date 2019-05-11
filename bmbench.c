/*
 * BM Bench - bmbench.c (C)
 * (c) Marco Vieth, 2002-2019
 * http://www.benchmarko.de
 *
 * 06.05.2002 0.01
 * 11.05.2002 0.02  bench01 = (sum 1..n) mod 65536 (integer)
 * 22.05.2002 0.03  bench02 = (sum 1..n) mod 65536 (floating point), bench03 = Sieve of Eratosthenes
 * 20.07.2002 0.04  some errors corrected
 * 24.01.2003 0.05  output format changed
 * 13.04.2003       malloc size for bench_res1 corrected
 * 30.05.2006 0.06  based on version 0.05
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

#define PRG_VERSION "0.07"
#define PRG_LANGUAGE "C"

#include <stdio.h>
#include <stdlib.h>
#include <time.h>  /* only to get date with ctime(), time() */
#include <string.h> /* strcat */

/* #define Use_Windows */
#ifdef Use_Windows
#include <sys/timeb.h>
#else
#include <sys/time.h> /* gettimeofday */
#endif

#ifdef __GNUC__
#define MY_VERSION "Compiled with GCC %d.%d\n", __GNUC__, __GNUC_MINOR__
#endif


#if !defined MY_VERSION
#define MY_VERSION "Compiled on ??\n"
#endif


typedef /* unsigned long */ double bmtime_t;

struct gState_t {
	char tsType[20]; /* type of time stamp source */
	bmtime_t tsPrecMs; /* measured time stamp precision */
	int tsPrecCnt; /* time stamp count (calls) per precision interval (until time change) */
	int tsMeasCnt; /* last measured count */
};

static struct gState_t gState = {
	"", 0, 0, 0
};


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
static int bench00(int loops, int n, short int check) {
  short int x = 0;
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

  sieve_t *sieve = malloc((nHalf + 1) * sizeof(sieve_t));
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
    while (m * m < n) {
      if (!sieve[i]) {
        x++;
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
    while (i <= nHalf) {
      if (!sieve[i]) {
        x++;
      }
      i++;
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
  typedef int pas_t;
  int n = (int)(n_p / 500);
  int k = n / 2;
  int i, j, i_mod_2, min1;
  /* allocate memory ... */
  pas_t  *pas1[2];
  if ((n - k) < k) {
    k = n - k; /* keep k minimal with  n over k  =  n over n-k */
  }

  for (i = 0; i < 2; i++) {
    pas1[i] = malloc((k + 1) * sizeof(pas_t));
    if (pas1[i] == NULL) {
      return -1; /* error */
    }
  }

  pas1[0][0] = 1; pas1[1][0] = 1; /* set first column */

  while (loops-- > 0 && x == 0) {
    for (i = 2; i <= n; i++) {
      i_mod_2 = i % 2;
      min1 = (i - 1) / 2;
      if (k < min1) {
        min1 = k;
      }
      pas1[i_mod_2][1] = i; /* second column is i */
      for (j = 2; j <= min1; j++) { /* up to min((i-1)/2, k) */
        pas1[i_mod_2][j] = (pas1[i_mod_2 ^ 1][j - 1] + pas1[i_mod_2 ^ 1][j]);
      }
      if ((min1 < k) && (i_mod_2 == 0)) { /* new element */
        /* pas1[i_mod_2][i / 2] = 2 * pas1[i_mod_2 ^ 1][(i - 1) / 2]; */
        pas1[i_mod_2][min1 + 1] = 2 * pas1[i_mod_2 ^ 1][min1];
      }
    }
    x += pas1[n % 2][k] & 0xffff;
    x -= check;
  }

  /* free memory */
  for (i = 0; i < 2; i++) {
    if (pas1[i] != NULL) {
      free(pas1[i]);
      pas1[i] = NULL;
    }
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
static int run_bench(int bench, int loops, int n) {
  int x = 0;
  int check;
  switch(bench) {
    case 0:
	    check = ((n / 2) * (n + 1)) & 0xffff; // short int
      x = bench00(loops, n, check);
    break;

    case 1:
	    check = (n + 1) / 2;
      x = bench01(loops, n, check);
    break;

    case 2:
      check = (n + 1) / 2;
      x = bench02(loops, n, check);
    break;

    case 3:
      check = 41538;
      x = bench03(loops, n, check);
    break;

    case 4:
      check = 1227283347;
      x = bench04(loops, n, check);
    break;

    case 5:
      check = 27200;
      x = bench05(loops, n, check);
    break;
	
    default:
      fprintf(stderr, "Error: Unknown benchmark: %d\n", bench);
      check = -1;
    break;
  }

  x += check;
  if (x != check) {
    fprintf(stderr, "Error(bench%d): x=%d\n", bench, x);
    x = -1; /* exit */
  }
  return x;
}


/*
 * get timestamp in milliseconds
 * out: x = time in ms
 *
 * We return double even if we only do short measurements
 */
static bmtime_t get_ms(void) {
  //static bmtime_t initialTime = 0;
  bmtime_t ltime;
#ifndef Use_Windows
  struct timeval tv;
  gettimeofday(&tv, NULL);
  ltime = tv.tv_sec * 1000.0 + tv.tv_usec / 1000.0;
  /*
  if (!initialTime) {
    initialTime = ltime;
  }
  ltime -= initialTime;
  */
#else
  struct _timeb tb;
  _ftime(&tb);
  ltime = (bmtime_t)tb.time * 1000.0 + tb.millitm;
#endif
  return(ltime);
}


static time_t *get_time1(void) {
  static time_t t;  /* need static because we return a pointer... */
  t = time(NULL);
  return(&t);
}


static bmtime_t correctTime(bmtime_t tMeas, int measCount) {
  int tsPrecCnt = gState.tsPrecCnt;

  if (measCount < tsPrecCnt) {
    tMeas += gState.tsPrecMs * ((tsPrecCnt - measCount) / (double)tsPrecCnt); // ts + correction
    //printf("DEBUG: correctTime: tMeas=%f cntDiff=%d corr=%f\n", tMeas, (tsPrecCnt - measCount), tsPrecMs * ((tsPrecCnt - measCount) / (double)tsPrecCnt));
    //printf("DEBUG: \n");
    //TTT bench01(1000, 1000000, ((1000000 + 1) / 2) | 0);    
  }
  return tMeas;
}

static bmtime_t getPrecMs(int stopFlg) {
  int measCount = 0;

//return get_ms(); //TTT
  bmtime_t tMeas0 = get_ms();
  bmtime_t tMeas = tMeas0;
  while (tMeas <= tMeas0) {
    tMeas = get_ms();
    measCount++;
  }

  if (stopFlg) {
    tMeas = correctTime(tMeas0, measCount); /* for stop: use first ts + correction */
  }
  gState.tsMeasCnt = measCount; /* memorize count */
  return tMeas;
}

/* usually only needed if time precision is low, e.g. one second */
static void determineTsPrecision() {
  bmtime_t tMeas0 = getPrecMs(0);
  bmtime_t tMeas1 = getPrecMs(0);
  gState.tsPrecMs = tMeas1 - tMeas0;
  gState.tsPrecCnt = gState.tsMeasCnt;

  /* do it again */
  tMeas0 = tMeas1;
  tMeas1 = getPrecMs(0);
  if (gState.tsMeasCnt > gState.tsPrecCnt) { /* taker maximum count */
    //printf("DEBUG: determineTsPrecision: Overwriting old measurement: tsPrecMs=%f tsPrecCnt=%d\n", gState.tsPrecMs, gState.tsPrecCnt);
    gState.tsPrecCnt = gState.tsMeasCnt;
    gState.tsPrecMs = tMeas1 - tMeas0;
  }
  //printf("DEBUG: determineTsPrecision: tsPrecMs=%f tsPrecCnt=%d, tmeas0=%f tMeas1=%f tsMeasCnt=%d\n", gState.tsPrecMs, gState.tsPrecCnt, tMeas0, tMeas1, gState.tsMeasCnt);
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
    num *= 2.0;
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



static void print_info() {
  printf("BM Bench v%s (%s) -- (short:%d int:%d float:%d double:%d tsMs:%lf tsCnt:%d) ", PRG_VERSION, PRG_LANGUAGE,
    checkbits_short1(), checkbits_int1(), checkbits_float1(), checkbits_double1(),
    gState.tsPrecMs, gState.tsPrecCnt);
  printf(MY_VERSION); // maybe multiple arguments!
  printf("(c) Marco Vieth, 2006\n");
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


static double measureBench(int bench, int n) {
  int cali_ms = 1001; /* const */
  int delta_ms = 100; /* const */
  int max_ms = 10000; /* const */
  
  int loops = 1;   /* number of loops */
  int x = 0;     /* result from benchmark */
  bmtime_t t1 = 0; /* measured time */
  bmtime_t t2 = 0; /* estimated time */
  double throughput = 0;

  printf("Calibrating benchmark %d with n=%d\n", bench, n);
  while (throughput == 0) {
    t1 = getPrecMs(0); //get_ms(); //getPrecMs(0);
    x = run_bench(bench, loops, n);
    t1 = getPrecMs(1) - t1; //get_ms() - t1; //getPrecMs(1) - t1;

    bmtime_t t_delta = (t2 > t1) ? (t2 - t1) : (t1 - t2); /* compute difference abs(measures-estimated) */
    double loops_p_sec = (t1 > 0) ? ((loops * 1000.0) / t1) : 0;
    //printf("DEBUG: (time=%9.3f ms, delta=%9.3f ms, tsMeasCnt=%d)\n",t1, t_delta, gState.tsMeasCnt);
    printf("%10.3f/s (time=%9.3f ms, loops=%7d, delta=%9.3f ms, x=%d)\n", loops_p_sec, t1, loops, t_delta, x);
    if (x == -1) { /* some error? */
      throughput = -1;
    } else if ((t2 > 0) && (t_delta < delta_ms)) { /* do we have some estimated/expected time smaller than delta_ms=100? */
      throughput = loops_p_sec; /* yeah, set measured loops per sec */
      printf("Benchmark %d (%s): %.3f/s (time=%.3f ms, loops=%d, delta=%.3f ms)\n", bench, PRG_LANGUAGE, loops_p_sec, t1, loops, t_delta);
    } else if (t1 > max_ms) {
      printf("Benchmark %d (%s): Time already > %d ms. No measurement possible.\n", bench, PRG_LANGUAGE, max_ms);
      throughput = (loops_p_sec > 0) ? -loops_p_sec : 0; /* cannot rely on measurement, so set to negative */
    } else {
      int scale_fact = ((t1 < cali_ms) && (t1 > 0)) ? (int)((cali_ms + 100) / t1) + 1 : 2;
      //double scale_fact = ((t1 < cali_ms) && (t1 > 0)) ? (int)((cali_ms + 100) / t1) + 1 : 2; //TTTTTTTTTTTTTT check!
        /* scale a bit up to 1100 ms (cali_ms+100) */
      //printf("DEBUG: scale_fact=%lf, t1=%lf, t2=%lf\n", (double)scale_fact, t1, t2);
      loops *= scale_fact;
      t2 = t1 * scale_fact;
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

  if ((bench_res = malloc((bench2 + 1) * sizeof(double))) == NULL) {
    fprintf(stderr, "Error: malloc(bench_res)\n");
    exit(1);
  }

  for (bench = bench1; bench <= bench2; bench++) {
    double throughput = measureBench(bench, n);
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
  bmtime_t start_t = get_ms(); /* memorize start time */
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
    
  printf("Total elapsed time: %ld ms\n", (long)(get_ms() - start_t));
  return rc;
}
/* end */
