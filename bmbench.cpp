//
// BM Bench - bmbench.cpp (C++)
// (c) Marco Vieth, 2006
// http://www.benchmarko.de
//
// 03.05.2006       created C++ version from C version
// 23.05.2006 0.06  based on version 0.05
//
//
// Usage:
// bmbench [bench1] [bench2] [n]
//
//

//
// Compile (C version):
// - UNIX: g++ -Wall -O2 bmbench.cpp -o bmbench
//   (-funroll-loops has a positive impact on bench 00, 01 but should not be used.)
// - Visual C++:
//   ...
//
// check...
// - NT (MS C++): cl -W4 -DUse_Windows bmbench.c -o bmbench
// - Windows/DOS (BC++):  bcc -3 -a -k- -O -Og -Oe -Om -Ov -Ol -Ob -Op -Oi -Z -G -v- -vi- bmbench.c
//   (-3 is to use x386 instructions...)
//   (or use bcw...)
//

//
// All types of gcc warnings:
// gcc -Wall -Wtraditional -Wshadow -Wpointer-arith -Wcast-qual -Wcast-align -Wconversion -Waggregate-return -Wstrict-prototypes -Wmissing-prototypes -Wmissing-declarations -Wredundant-decls -Wnested-externs -O2 bmbench.c -o bmbench
//
//

// #define BENCH03_ODD // bench03: put only odd numbers in sieve?

#define PRG_VERSION "0.06"
#define PRG_LANGUAGE "C++"


#include <stdio.h>
#include <stdlib.h>
#include <malloc.h>
#include <string.h>

#include <time.h>  // only to get date with ctime(), time()

#ifdef __BORLANDC__
#define Use_Windows
#define _ftime ftime
#define _timeb timeb
#include <alloc.h>  // farmalloc, farfree
#include <dos.h>  // _8087
#define MY_MALLOC farmalloc
#define MY_FREE farfree
#define MY_FAR far
#define MY_VERSION "Compiled with Borland C %x.%x; DOS %d.%d; FPU: %d\n", (__BORLANDC__ >> 8), (__BORLANDC__ & 0xff), _osmajor, _osminor, _8087
#else // UNIX, windows 32 bit
#define MY_MALLOC malloc
#define MY_FREE free
#define MY_FAR
#endif

#ifdef _MSC_VER
#define Use_Windows 
#define MY_VERSION "Compiled with MSC %d\n", _MSC_VER
#endif

// #define Use_Windows
#ifdef Use_Windows
#include <sys/timeb.h>
#else
#include <sys/time.h> // gettimeofday
#endif

#ifdef __GNUC__
#define MY_VERSION "Compiled with GCC %d.%d\n", __GNUC__, __GNUC_MINOR__
#endif


#if !defined MY_VERSION
#define MY_VERSION "Compiled on ??\n"
#endif



typedef long num_t; // we need 32 bit


//
// General description for benchmark test functions
// benchxx - benchmark
// <description>
// in: loops = number of loops
//         n = maximum number (assumed even, normally n=1000000)
// out:    x = <output decription>
//
// loops may be increased to produce a longer runtime without changing the result.
//


//
// bench00 (Integer 16 bit)
// (sum of 1..n) mod 65536
//
static num_t bench00(int loops, num_t n) {
  short int x = 0;
  short int sum1 = (short int)((n / 2) * (n + 1)); // assuming n even!
  // (sum1..1000000 depends on type: 500000500000 (floating point), 1784293664 (32bit), 10528 (16 bit)
  unsigned int n_div_65536 = (unsigned int)(n >> 16);
  unsigned int n_mod_65536 = (unsigned int)(n & 0xffff);
  // fprintf(stderr, "Test(bench%d): x=%f, %ld, %ld\n", 1, (double)sum, (long)fmod(sum, 2147483648L), (long)fmod(sum, 65536L));
  while (loops-- > 0) {
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

    if (loops > 0) { // some more loops left?
      x -= sum1;     // yes, set x back to 0 (assuming n even)
      if (x != 0) {  // now x must be 0 again
        x++;         // force error for many wrong computations
        break;       // error
      }
    }
  }
  return (num_t)(x & 0xffff);
}


//
// bench01 (Integer 16/32 bit)
// (sum of 1..n) mod 65536
//
static num_t bench01(int loops, num_t n) {
  int x = 0;
  int sum1 = (int)((n / 2) * (n + 1)); // assuming n even!
  // (sum1..1000000 depends on type: 500000500000 (floating point), 1784293664 (32bit), 10528 (16 bit)
  while (loops-- > 0) {
    num_t i;
    for (i = n; i > 0; i--) {
      x += (int)i;
    }
    if (loops > 0) { // some more loops left?
      x -= sum1;     // yes, set x back to 0 (assuming n even)
      if (x != 0) {  // now x must be 0 again
        x++;
        break;       // error
      }
    }
  }
  return (num_t)(x & 0xffff);
}


//
// bench02 (Floating Point, normally 64 bit)
// (sum of 1..n) mod 65536
//
static num_t bench02(int loops, num_t n) {
  double x = 0.0;
  double sum1 = ((double)n / 2.0) * (n + 1.0); // assuming n even!
  // (sum1..1000000 depends on type: 500000500000 (floating point), 1784293664 (32bit), 10528 (16 bit)
  while (loops-- > 0) {
    num_t i;
    for (i = n; i > 0; i--) {
      x += (double)i;
    }
    if (loops > 0) {    // some more loops left?
      x -= sum1;        // yes, set x back to 0 (assuming n even)
      if (x != 0.0) {   // now x must be 0 again
        x++;
        break;          // error
      }
    }
  }
  // fprintf(stderr, "DEBUG(bench%d): x=%f, x(int)=%d  %f\n", 2, x, (int)(x / 65536), (x - ((int)(x / 65536.0) * 65536.0)));
  return (num_t)(x - ((int)(x / 65536.0) * 65536.0)); // or use fmod()...
}


#ifndef BENCH03_ODD // not defined
//
// bench03 (Integer)
// number of primes below n (Sieve of Eratosthenes)
// Example: n=500000 => x=41538 (expected), n=1000000 => x=78498
//
#define SIEVE_SET_BIT(sieve, a) (sieve[(unsigned)((a) >> 3)] |= (1 << ((a) & 7)))
#define SIEVE_RES_BIT(sieve, a) (sieve[(unsigned)((a) >> 3)] &= ~(1 << ((a) & 7)))
#define SIEVE_TEST_BIT(sieve, a) (sieve[(unsigned)((a) >> 3)] & (1 << ((a) & 7)))
static num_t bench03(int loops, num_t n) {
  typedef unsigned char sieve_t;
  num_t x = 0; // number of primes below n
  unsigned int sieve1_size = (unsigned int)(((n / 2) / 8) + 1);
  num_t i, j;
  // allocate memory ...
  sieve_t MY_FAR *sieve1 = (sieve_t MY_FAR *)MY_MALLOC(sieve1_size * sizeof(sieve_t));
  // For some reason it would be faster on my machine to use n instead of n/8; valloc() is no improvement
  n /= 2; // compute only up to n/2
  // fprintf(stderr, "DDD: %p\n", sieve);

  if (sieve1 == NULL) {
    return -1; // error
  }
  // fprintf(stderr, "DDD: %d\n", (n >> 3));
  SIEVE_RES_BIT(sieve1, 0);
  SIEVE_RES_BIT(sieve1, 1);
  while (loops-- > 0) {
    // initialize sieve
    for (i = 2; i < n; i ++) {
      SIEVE_SET_BIT(sieve1, i);
    }
    // compute primes
    for (i = 2; (i * i) < n; i++) {
      if (SIEVE_TEST_BIT(sieve1, i) > 0) {
        // fprintf(stderr, "[%ld] ", (long)i);

        for (j = i * i; j < n; j += i) {
          SIEVE_RES_BIT(sieve1, j);
        }
      }
    }
    
    // count primes
    for (i = 2; i < n; i++) {
      if (SIEVE_TEST_BIT(sieve1, i) > 0) {
        x++;
      }
    }
    // check prime count
    if (loops > 0) {  // some more loops left?
      x -= 41538L;    // yes, set x back to 0 (number of primes below 1000000)
      if (x != 0) {   // now x must be 0 again
        x++;
        break;        // Error (cannot exit here because of malloc...)
      }
    }
  }

  // free memory
  if (sieve1 != NULL) {
    MY_FREE(sieve1);
    sieve1 = NULL;
  }
  // fprintf(stderr, "end: x=%ld\n", (long)x);
  return x;
}


#else // BENCH03_ODD


//
// bench03_odd (Integer)
// 13.04.2003 put only odd numbers in sieve. Certainly this is faster but we don't use it.
//
#define SIEVE_SET_BIT(sieve, a) (sieve[(unsigned)((a>>1) >> 3)] |= (1 << ((a>>1) & 7)))
#define SIEVE_RES_BIT(sieve, a) (sieve[(unsigned)((a>>1) >> 3)] &= ~(1 << ((a>>1) & 7)))
#define SIEVE_TEST_BIT(sieve, a) (sieve[(unsigned)((a>>1) >> 3)] & (1 << ((a>>1) & 7)))
static num_t bench03(int loops, num_t n) {
  typedef unsigned char sieve_t;
  num_t x = 0; // number of primes below n
  unsigned int sieve1_size = (unsigned int)(((n / 2) / 2 / 8) + 1);
  num_t i, j;
  // allocate memory ... //
  sieve_t MY_FAR *sieve1 = MY_MALLOC(sieve1_size * sizeof(sieve_t));
  // For some reason it would be faster on my machine to use n instead of n/8; valloc() is no improvement
  n /= 2; // compute only up to n/2
  // fprintf(stderr, "DDD: %p\n", sieve);

  if (sieve1 == NULL) {
    return -1; // error //
  }
  // fprintf(stderr, "DDD: %d\n", (n >> 3));
  // SIEVE_RES_BIT(sieve1, 0); // slot 0: 0 and 1 are no primes; unused
  SIEVE_SET_BIT(sieve1, 0); // we save prime 2 in slot 0!

  // we don't put the prime 2 in the sieve
  while (loops-- > 0) {
    // initialize sieve
    for (i = 3; i < n; i += 2) {
      SIEVE_SET_BIT(sieve1, i);
    }
    // compute primes
    for (i = 3; (i * i) < n; i += 2) {
      if (SIEVE_TEST_BIT(sieve1, i) > 0) {
        // fprintf(stderr, "[%ld] ", (long)i);

        for (j = i * i; j < n; j += 2 * i) {
          SIEVE_RES_BIT(sieve1, j);
        }
      }
    }
    
    // x++; // // we know that 2 is a prime
    // count primes
    // For some reason this is very fast with gcc -O2... (slot floor(1/2)=0 counts for prime 2!)
    // if you modify x here (e.g. set to 0) -> it will get slower!!??
    for (i = 1; i < n; i += 2) {
      if (SIEVE_TEST_BIT(sieve1, i) > 0) {
        // fprintf(stderr, "DEBUG: %d\n", (int)i);
        x++;
      }
    }

    // check prime count
    if (loops > 0) {  // some more loops left?
      x -= 41538L;    // yes, set x back to 0 (number of primes below 1000000)
      if (x != 0) {   // now x must be 0 again
        x++;
        break;        // Error (cannot exit here because of malloc...)
      }
    }
  }

  // free memory
  if (sieve1 != NULL) {
    MY_FREE(sieve1);
    sieve1 = NULL;
  }
  // fprintf(stderr, "end: x=%ld\n", (long)x);
  return x;
}
#endif // BENCH03_ODD


//
// bench04 (Integer 32 bit)
// nth random number number
// Random number generator taken from
// Raj Jain: The Art of Computer Systems Performance Analysis, John Wiley & Sons, 1991, page 442-444.
// It needs longs with at least 32 bit.
// Starting with x0=1, x10000 should be 1043618065, x1000000 = 1227283347.
//
#define BENCH04_M 2147483647L // modulus, do not change!
#define BENCH04_A 16807       // multiplier
#define BENCH04_Q 127773L     // m div a
#define BENCH04_R 2836        // m mod a
static num_t bench04(int loops, num_t n) {
  num_t x = 1;                // last random value
  num_t i;
  while (loops-- > 0) {
    for (i = 1; i <= n; i++) {
      num_t x_div_q = x / BENCH04_Q;
      num_t x_mod_q = x - BENCH04_Q * x_div_q;
      x = BENCH04_A * x_mod_q - BENCH04_R * x_div_q;
      if (x <= 0) {
        x += BENCH04_M; // x is new random number
      }
    }
    if (loops > 0) {
      x -= 1227283347L;
      if (x != 0) {   // now x must be 0 again
        x++;
        break;        // Error
      }
      x++; // start with 1 again
    }
  }
  // fprintf(stderr, "Test(bench%d): x=%ld\n", 4, (long)x);
  return x;
}


//
// bench05 (Integer 32 bit)
// n over n/2 mod 65536 (Pascal's triangle)
// (we just need to store the last 2 lines of computation)
//
static num_t bench05(int loops, num_t n_p) {
  num_t x = 0;
  typedef int pas_t;
  int n = (int)(n_p / 500);
  int k = n / 2;
  int i, j, i_mod_2, min1;
  // allocate memory ... //
  pas_t MY_FAR *pas1[2];
  if ((n - k) < k) {
    k = n - k; // keep k minimal with  n over k  =  n over n-k
  }

  for (i = 0; i < 2; i++) {
    pas1[i] = (pas_t MY_FAR *)MY_MALLOC((k + 1) * sizeof(pas_t));
    if (pas1[i] == NULL) {
      return -1; // error //
    }
  }

  pas1[0][0] = 1; pas1[1][0] = 1; // set first column

  while (loops-- > 0) {
    for (i = 2; i <= n; i++) {
      i_mod_2 = i % 2;
      min1 = (i - 1) / 2;
      if (k < min1) {
        min1 = k;
      }
      pas1[i_mod_2][1] = i; // second column is i
      for (j = 2; j <= min1; j++) { // up to min((i-1)/2, k)
        pas1[i_mod_2][j] = (pas1[i_mod_2 ^ 1][j - 1] + pas1[i_mod_2 ^ 1][j]);
      }
      if ((min1 < k) && (i_mod_2 == 0)) { // new element
        // pas1[i_mod_2][i / 2] = 2 * pas1[i_mod_2 ^ 1][(i - 1) / 2];
        pas1[i_mod_2][min1 + 1] = 2 * pas1[i_mod_2 ^ 1][min1];
      }
    }
    x += pas1[n % 2][k] & 0xffff; // % 65536
    if (loops > 0) {
      x -= 27200;
      if (x != 0) {   // now x must be 0 again
        x++;
        break;        // Error
      }
    }
  }

// #define DEBUG
#ifdef DEBUG
  for (i = 0; i < 2; i++) {
    printf("%ld: ", (num_t)i);
    for (j = 0; j <= k; j++) {
      printf("%d ", pas1[i][j]);
    }
    printf("\n");
  }
#endif

  // free memory
  for (i = 0; i < 2; i++) {
    if (pas1[i] != NULL) {
      MY_FREE(pas1[i]);
      pas1[i] = NULL;
    }
  }
  return x;
}


//
// run a benchmark
// in: bench = benchmark to use
//     loops = number of loops
//         n = maximum number (used in some benchmarks to define size of workload)
// out:    x = result
//
static num_t run_bench(int bench, int loops, num_t n) {
  num_t x = 0;
  num_t check1 = 0;
  switch(bench) {
    case 0:
      x = bench00(loops, n); // special version optimized for 16 bit
      check1 = 10528;
    break;

    case 1:
      x = bench01(loops, n);
      check1 = 10528;
    break;

    case 2:
      x = bench02(loops, n);
      check1 = 10528;
    break;

    case 3:
      x = bench03(loops, n);
      check1 = 41538L;
    break;

    case 4:
      x = bench04(loops, n);
      check1 = 1227283347L;
    break;

    case 5:
      x = bench05(loops, n);
      check1 = 27200; // 58336; 43584;
    break;

    default:
      fprintf(stderr, "Error: Unknown benchmark: %d\n", bench);
      check1 = x + 1; // force error
    break;
  }
  if (check1 != x) {
    fprintf(stderr, "Error(bench%d): x=%ld\n", bench, (long)x);
    x = -1; // exit //
  }
  return(x);
}


typedef long bmtime_t;
//
// get timestamp in milliseconds
// out: x = time in ms
//
// This function is intended for short measurements only so we
// can return it as an integer.
//
static bmtime_t get_ms(void) {
  long ltime;
#ifndef Use_Windows
  struct timeval tv;
  gettimeofday(&tv, NULL);
  ltime = (long)tv.tv_sec * 1000 + tv.tv_usec / 1000;
#else
  struct _timeb tb;
  _ftime(&tb);
  ltime = (long)tb.time * 1000 + tb.millitm;
#endif
  return((bmtime_t)ltime);
}


static time_t *get_time1(void) {
  static time_t t;  // need static because we return a pointer...
  t = time(NULL);
  return(&t);
}


// Here we compute the number of "significant" bits for positive numbers (which means 53 for double)
static int checkbits_short1(void) {
  short num = 1;
  short last_num = 0;
  int bits = 0;
  do {
    last_num = num;
    num *= 2;
    num++;
    bits++;
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
  } while ( (((num - 1) / 2) == last_num) && (bits < 101) );
  return bits;
}

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

// --------------------------------------------------------

static void print_info() {
  printf("BM Bench v%s (%s) -- (short:%d int:%d float:%d double:%d) ", PRG_VERSION, PRG_LANGUAGE, checkbits_short1(), checkbits_int1(),
    checkbits_float1(), checkbits_double1());
  printf(MY_VERSION); // maybe multiple arguments!
  printf("(c) Marco Vieth, 2006\n");
  printf("Date: %s", ctime(get_time1()));
}


#define MAX_LANGUAGE_LEN1 10

static void print_results(int bench1, int bench2, double bench_res1[]) {
  printf("\nThroughput for all benchmarks (loops per sec):\n");
  char str[MAX_LANGUAGE_LEN1 + 1] = "";
  for (int i = (int)strlen(PRG_LANGUAGE); i < MAX_LANGUAGE_LEN1; i++) {
    strcat(str, " ");
  }
  printf("BMR (%s)%s: ", PRG_LANGUAGE, str);

  for (int bench = bench1; bench <= bench2; bench++) {
    printf("%9.2f ", bench_res1[bench]);
  }
  printf("\n");
  printf("\n");
}


static int start_bench(const int bench1, const int bench2, const num_t n) {
  const int cali_ms = 1001;
  const int delta_ms = 100;
  const int max_ms = 10000;

  print_info();

  double *bench_res1 = NULL;
  if ((bench_res1 = (double *)malloc((bench2 + 1) * sizeof(double))) == NULL) {
    fprintf(stderr, "Error: malloc(bench_res1)\n");
    exit(1);
  }

  for (int bench = bench1; bench <= bench2; bench++) {
    int loops = 1;   // number of loops
    num_t x = 0;     // result from benchmark
    bmtime_t t1 = 0; // measured time
    bmtime_t t2 = 0; // estimated time

    printf("Calibrating benchmark %d with n=%ld\n", bench, (long)n);
    while (1) {
      t1 = get_ms();
      x = run_bench(bench, loops, n);
      t1 = get_ms() - t1;

      long t_delta = (t2 > t1) ? (t2 - t1) : (t1 - t2); // compute difference abs(measures-estimated)
      double loops_p_sec = (t1 > 0) ? (loops * 1000.0 / t1) : 0;
      printf("%10.3f/s (time=%5ld ms, loops=%7d, delta=%5ld ms, x=%d)\n", loops_p_sec, t1, loops, t_delta, (int)x);
      if (x == -1) { // some error?
        bench_res1[bench] = -1;
        break;
      }
      if (t2 > 0) { // do we have some estimated/expected time?
        if (t_delta < delta_ms) { // smaller than delta_ms=100?
          bench_res1[bench] = loops_p_sec; // set loops per sec
          printf("Benchmark %d (%s): %.3f/s (time=%ld ms, loops=%d, delta=%ld ms)\n", bench, PRG_LANGUAGE, bench_res1[bench], t1, loops, t_delta);
          break;
        }
      }
      if (t1 > max_ms) {
        printf("Benchmark %d (%s): Time already > %d ms. No measurement possible.\n", bench, PRG_LANGUAGE, max_ms);
        bench_res1[bench] = -1;
        break;
      }
      {
        int scale_fact = ((t1 < cali_ms) && (t1 > 0)) ? (int)((cali_ms + 100) / t1) + 1 : 2;
          // scale a bit up to 1100 ms (cali_ms+100)
        loops *= scale_fact;
        t2 = t1 * scale_fact;
      }
    }
  }

  print_results(bench1, bench2, bench_res1);

  if (bench_res1 != NULL) {
    free(bench_res1);
    bench_res1 = NULL;
  }
  return 0;
}



int main(int argc, char **argv) {
  bmtime_t start_t = get_ms(); // memorize start time
  int bench1 = 0;       // first benchmark to test
  int bench2 = 5;       // last benchmark to test
  num_t n = 1000000L;   // maximum number
  
  if (argc > 1) {
    bench1 = atoi(argv[1]);
    bench2 = bench1; // set also last benchmark
  }
  if (argc > 2) {
    bench2 = atoi(argv[2]);
  }
  if (argc > 3) {
    n = atol(argv[3]);
  }
  
  int rc = start_bench(bench1, bench2, n);
    
  printf("Total elapsed time: %ld ms\n", (long)(get_ms() - start_t));
  return rc;
}
  

// end
