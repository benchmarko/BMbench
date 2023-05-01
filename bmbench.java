//
// BM Bench - bmbench.java (Java)
// (c) Marco Vieth, 2002-2022
// http://www.benchmarko.de
//
// 06.05.2002 0.01
// 11.05.2002 0.02  bench01 = (sum 1..n) mod 65536 (integer)
// 22.05.2002 0.03  bench02 = (sum 1..n) mod 65536 (floating point), bench03 = Sieve of Eratosthenes
// 20.07.2002 0.04  some errors corrected
// 24.01.2003 0.05  output format changed
// 23.05.2006 0.06  based on version 0.05
// 05.05.2019 0.07  changed bench 00-03; time interval estimation
// 03.12.2022 0.072 bench03 corrected, bench05 improved
// 19.02.2023 0.08  bench05 optimized
//
// Compile & Run:
// java bmbench.java [bench] [n]
// (or as an applet in a HTML page, deprecated)
//
// javac -O -d . -Xlint:all bmbench.java   (compile)
// java bmbench                 (run)
//
// (/usr/lib/IBMJava2-1.3.0/bin or /usr/lib/SunJava1-1.1.8/bin/)
// (guavac, gcj, ...)
//

import java.text.SimpleDateFormat; // just to print date

class bmbench {
  static String prg_version = "0.08";
  static String prg_language = "Java";

  private static long gState_startTs = 0;
  private static double gState_tsPrecMs = 0.0; // measured time stamp precision
  private static int gState_tsPrecCnt = 0; // time stamp count (calls) per precision interval (until time change)
  private static int gState_tsMeasCnt = 0; // last measured count
  private static int g_cali_ms = 1001; //
  private static int maxBench = 6;
  
  /*
  private static class gState {
    int tsMeasCnt = 0;
  };
  */

  //
  // General description for benchmark test functions
  // benchxx - benchmark
  // <description>
  // in: loops = number of loops
  //         n = maximum number (assumed even, normally n=1000000)
  //     check = expected value for x
  // out:    x = <output>
  //
  // loops may be increased to produce a longer runtime without changing the result.
  //


  //
  // bench00 (Integer 16 bit)
  // (sum of 1..n) mod 65536
  // can be computed with short
  //
  private static int bench00(int n) {
    short x = 0; // short is -32767..32768
    short n_div_65536 = (short)(n >> 16);
    short n_mod_65536 = (short)(n & 0xffff);
    // System.out.println("DEBUG: ndiv="+ n_div_65536 +", nmod="+ n_mod_65536);
    for (int i = n_div_65536; i > 0; i--) {
      for (short j = 32767; j > 0; j--) {
        x += j;
      }
      for (short j = -32768; j < 0; j++) {
        x += j;
      }
     // System.out.println("DEBUG: x="+ x);
    }
    for (short j = n_mod_65536; j > 0; j--) {
      x += j;
    }
    return x & 0xffff;
  }


  //
  // bench01 (Integer 16/32 bit)
  // (arithmetic mean of 1..n)
  //
  private static int bench01(int n) {
    int x = 0;
    int sum = 0;
    for (int i = 1; i <= n; i++) {
      sum += i;
      if (sum >= n) { // to avoid numbers above 2*n, divide by n using subtraction
        sum -= n;
        x++;
      }
    }
    return x;
  }


  //
  // bench02 (Floating Point, normally 64 bit)
  // (arithmetic mean of 1..n)
  //
  private static int bench02(int n) {
    int x = 0;
    double sum = 0.0;
    for (int i = 1; i <= n; i++) {
      sum += i;
      if (sum >= n) {
        sum -= n;
        x++;
      }
    }
    return x;
  }

  private static boolean bench03Sieve1[];

  //
  // bench03 (Integer)
  // number of primes less than or equal to n (prime-counting function)
  // Example: n=500000 => x=41538 (expected), n=1000000 => x=78498
  // (Sieve of Eratosthenes, no multiples of 2 are stored)
  //
  private static int bench03(int n) {
    int nHalf = n >> 1;

    // allocate memory...
    if (bench03Sieve1 == null) {
      bench03Sieve1 = new boolean[nHalf + 1];
    }
    boolean sieve1[] = bench03Sieve1;

    int i;
    // initialize sieve
    for (i = 0; i <= nHalf; i++) {
      sieve1[i] = false;
    }

    // compute primes
    i = 0;
    int m = 3;
    int x = 1; // number of primes below n (2 is prime)
    while (m * m <= n) {
      if (!sieve1[i]) {
        x++; // m is prime
        int j = (m * m - 3) >> 1; // div 2
        while (j < nHalf) {
          sieve1[j] = true;
          j += m;
        }
      }
      i++;
      m += 2;
    }

    // count remaining primes
    while (m <= n) {
      if (!sieve1[i]) {
        x++; // m is prime
      }
      i++;
      m += 2;
    }
    return x;
  }

 
  //
  // bench04 (Integer 32 bit)
  // nth random number number
  // Random number generator taken from
  // Raj Jain: The Art of Computer Systems Performance Analysis, John Wiley & Sons, 1991, page 442-444.
  // It needs longs with at least 32 bit.
  // Starting with x0=1, x10000 should be 1043618065, x1000000 = 1227283347.
  //
  private static int bench04(int n) {
    final int m = 2147483647; // modulus, do not change!
    final int a = 16807; // multiplier
    final int q = 127773; // m div a
    final int r = 2836; // m mod a
    int x = 1; // 1=last random value

    for (int i = n; i > 0; i--) {
      int xDivQ = x / q;
		  int xModQ = x - q * xDivQ;
		  x = a * xModQ - r * xDivQ;
      //x = a * (x % q) - r * (x / q); // x div q
      if (x <= 0) {
        x += m; // x is new random number
      }
    }
    return x;
  }


  private static int bench05Line1[];

  //
  // bench05 (Integer 32 bit)
  // (n choose n/2) mod 65536 (Central Binomial Coefficient mod 65536)
  // Using dynamic programming and Pascal's triangle, storing only one line
  // Instead of nCk mod 65536 with k=n/2, we compute the product of (n/2)Ck mod 65536 with k=0..n/4 (Vandermonde folding)
  // Example: (2000 choose 1000) mod 65536 = 27200
  //
  private static int bench05(int n) {
    // Instead of nCk with k=n/2, we compute the product of (n/2)Ck with k=0..n/4
	  n /= 2;

    int k = n / 2;
    if ((n - k) < k) {
      k = n - k; // keep k minimal with  n over k  =  n over n-k
    }

    // allocate memory...
    if (bench05Line1 == null) {
      bench05Line1 = new int[k + 1];
    }
    int line[] = bench05Line1;

    // initialize (not needed)
    for (int j = 0; j <= k; j++) {
      line[j] = 0;
    }

    line[0] = 1;
    line[1] = 2; // for line 2, second column is 2

    // compute lines of Pascal's triangle
    for (int i = 3; i <= n; i++) {
      int min1 = (i - 1) / 2;
      if ((i & 1) == 0) { // new element?
        line[min1 + 1] = 2 * line[min1];
      }
      
      int prev = line[1];
      for (int j = 2; j <= min1; j++) {
        int num =  line[j];
        line[j] += prev;
        prev = num;
      }
      line[1] = i; // second column is i
    }

    // compute sum of ((n/2)Ck)^2 mod 65536 for k=0..n/2
    int x = 0;
    for (int j = 0; j < k; j++) {
      x += 2 * line[j] * line[j]; /* add nCk and nC(n-k) */
	  }
	  x += line[k] * line[k]; /* we assume that k is even, so we need to take the middle element */

    return x & 0xffff;
  }

  private static int bench06(int n) {
    double sum = 0.0;
    double flip = -1.0;
    for (int i = 1; i <= n; i++) {
      flip *= -1.0;
      sum += flip / (2 * i - 1);
    }
    return (int)((sum * 4.0) * 100000000);
  }

  //
  // run a benchmark
  // in: bench = benchmark to use
  //     loops = number of loops
  //         n = maximum number (used in some benchmarks to define size of workload)
  // out:    x = result
  //
  public static int run_bench(int bench, int loops, int n, int check) {
    if (bench > maxBench) {
      System.out.println("Error: Unknown benchmark " + bench);
    }

    int x = 0;
    while (loops-- > 0 && x == 0) {
      switch (bench) {
        case 0:
          x = bench00(n);
          break;

        case 1:
          x = bench01(n);
          break;

        case 2:
          x = bench02(n);
          break;

        case 3:
          x = bench03(n);
          break;

        case 4:
          x = bench04(n);
          break;

        case 5:
          x = bench05(n);
          break;

        case 6:
          x = bench06(n);
          break;

        default:
          System.out.println("Error: Unknown benchmark " + bench);
          break;
      }
      x -= check;
    }

    x += check;
    if (x != check) {
      System.out.println("Error(bench" + bench + "): x=" + x);
      x = -1; // exit
    }
    return x;
  }


  public static int bench03Check(int n) {
		int x;

   	if (n == 500000) {
	  	x = 41538;
	  } else {
		  x = 1; // 2 is prime
      for (int j = 3; j <= n; j += 2) {
        boolean isPrime = true;
        for (int i = 3; i * i <= j; i += 2) {
          if (j % i == 0) {
            isPrime = false;
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

  public static int getCheck(int bench, int n) {
    int check;

    switch (bench) {
      case 0: // (n / 2) * (n + 1)
        check = (((n + (n & 1)) >> 1) * (n + 1 - (n & 1))) & 0xffff; // 10528 for n=1000000
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
        check = (n == 1000000) ? 1227283347 : bench04(n); // bench04 not a real check
        break;

      case 5:
        check = (n == 5000) ? 17376 : bench05(n); // bench05 not a real check
        break;

      case 6:
        check = (n == 1000000) ? 314159165 : bench06(n); // bench06 not a real check
      break;

      default:
        System.out.println("Error: Unknown benchmark " + bench);
        check = -1;
        break;
    }
    return check;
  }


  //
  // get timestamp in milliseconds
  // out: x = time in ms
  //
  // Even if the function is intended for short measurements we should
  // return a long to avoid overflows...
  //


  // get timestamp with full precision
  private static long get_raw_ts() {
    return System.currentTimeMillis();
    //return (System.DateTime.Now.Ticks / 10) * 10; // Test: simulate 10 ms resolution
  }

  private static int get_ts() {
    return (int)(get_raw_ts() - gState_startTs);
  }

  static double conv_ms(long ts) {
    return ts;
  }

  private static double correctTime(double tMeas, double tMeas2, int measCount) {
    int tsPrecCnt = gState_tsPrecCnt;

    //System.out.println("DEBUG: correctTime1: tMeas=" + tMeas + ", tMeas2=" + tMeas2 + ", measCount=" + measCount + ", tsPrecCnt=" + tsPrecCnt);
    if (measCount < tsPrecCnt) {
      tMeas += gState_tsPrecMs * ((tsPrecCnt - measCount) / (double)tsPrecCnt); // ts + correction
      //System.out.println("DEBUG: correctTime2: tMeas=" + tMeas + ", tMeas2=" + tMeas2 +", measCount=" + measCount);
      if (tMeas > tMeas2) {
        tMeas = tMeas2; // cannot correct
        //System.out.println("DEBUG: correctTime3: cannot correct, using tMeas=" + tMeas);
      }
    }
    return tMeas;
  }

  private static double getPrecMs(boolean stopFlg) {
    int measCount = 0;

    int tMeas0 = get_ts();
    int tMeas = tMeas0;
    while (tMeas <= tMeas0) {
      tMeas = get_ts();
      measCount++;
    }
    gState_tsMeasCnt = measCount; // memorize count

    double tMeasD = (!stopFlg) ? conv_ms(tMeas) : correctTime(conv_ms(tMeas0), conv_ms(tMeas), measCount); // for stop: use first ts + correction
    return tMeasD;
  }

  // usually only needed if time precision is low, e.g. one second
  private static void determineTsPrecision() {
    gState_startTs = get_raw_ts(); // memorize start time

    double tMeas0 = getPrecMs(false);
    double tMeas1 = getPrecMs(false);
    gState_tsPrecMs = tMeas1 - tMeas0;
    gState_tsPrecCnt = gState_tsMeasCnt;

    // do it again
    tMeas0 = tMeas1;
    tMeas1 = getPrecMs(false);
    if (gState_tsMeasCnt > gState_tsPrecCnt) { // taker maximum count
      gState_tsPrecCnt = gState_tsMeasCnt;
      gState_tsPrecMs = tMeas1 - tMeas0;
    }
  }

  // --------------------------------------------------------

  // in Java the sizes of the types is specified (int: 32, double: 64)
  // Here we compute the number of "significant" bits for positive numbers (which means 53 for double)
  private static int checkbits_short1() {
    short num = 1;
    short last_num = 0;
    int bits = 0;
    do {
      last_num = num;
      num *= 2;
      num++;
      bits++;
      //System.out.println("DEBUG: bits="+ bits +", num="+ num);
    } while ((((num - 1) / 2) == last_num) && (bits < 101));
    return bits;
  }

  private static int checkbits_int1() {
    int num = 1;
    int last_num = 0;
    int bits = 0;
    do {
      last_num = num;
      num *= 2;
      num++;
      bits++;
      //System.out.println("DEBUG: bits="+ bits +", num="+ num);
    } while ((((num - 1) / 2) == last_num) && (bits < 101));
    return bits;
  }

  private static int checkbits_long1() {
    long num = 1;
    long last_num = 0;
    int bits = 0;
    do {
      last_num = num;
      num *= 2;
      num++;
      bits++;
      //System.out.println("DEBUG: bits="+ bits +", num="+ num);
    } while ((((num - 1) / 2) == last_num) && (bits < 101));
    return bits;
  }

  private static int checkbits_float1() {
    float num = 1.0f;
    float last_num = 0.0f;
    int bits = 0;
    do {
      last_num = num;
      num *= 2.0f;
      num++;
      bits++;
      //System.out.println("DEBUG: bits="+ bits +", num="+ num);
    } while ((((num - 1.0f) / 2.0f) == last_num) && (bits < 101));
    return bits;
  }

  private static int checkbits_double1() {
    double num = 1.0;
    double last_num = 0.0;
    int bits = 0;
    do {
      last_num = num;
      num *= 2.0;
      num++;
      bits++;
      //System.out.println("DEBUG: bits="+ bits +", num="+ num);
    } while ((((num - 1.0) / 2.0) == last_num) && (bits < 101));
    return bits;
  }

  // --------------------------------------------------------

  // or use NumberFormat?
  private static String mynumformat1_i(int val, int digits) {
    StringBuffer str = new StringBuffer(); // buffer for one formatted value
    str.append(val);
    for (int i = str.length(); i < digits; i++) {
      str.insert(0, ' ');
    }
    return (str.toString());
  }

  private static String mynumformat1_d(double val, int digits, int prec) {
    StringBuffer str = new StringBuffer(); // buffer for one formatted value
    double displ_prec_after = Math.pow(10, 3);  // display precision after decimal point
    str.append(Math.round(val * displ_prec_after) / (displ_prec_after * 1.0));

    if (str.toString().indexOf('.') < 0) { // should not occur
      System.out.println("WARNING: str does not contain a dot: " + str);
      str.append('.');
    }

    // format to prec digits after comma
    while ((str.length() <= prec) || (str.charAt(str.length() - (prec + 1)) != '.')) {
      str.append("0");
    }

    for (int i = str.length(); i < digits; i++) {
      str.insert(0, ' ');
    }
    return (str.toString());
  }


  private static String get_info() {
    String str = "BM Bench v" + prg_version + " (" + prg_language + ") -- (short:" + checkbits_short1() + " int:" + checkbits_int1()
      + " long:" + checkbits_long1() + " float:" + checkbits_float1() + " double:" + checkbits_double1() + " tsMs:" + gState_tsPrecMs + " tsCnt:" + gState_tsPrecCnt + ")"
      + " java.version=" + System.getProperty("java.version") + ", java.vendor=" + System.getProperty("java.vendor") + " "
      + ", os.name=" + System.getProperty("os.name") + ", os.arch=" + System.getProperty("os.arch")
      + ", os.version=" + System.getProperty("os.version") + "\n"
      + "(c) Marco Vieth, 2002-2022\n"
      + new SimpleDateFormat("yyyy-MM-dd HH:mm:ss").format(new java.util.Date(System.currentTimeMillis()));
    
    //System.out.println("properties="+ System.getProperties());
    return str;
  }


  private static void print_results(int bench1, int bench2, double bench_res1[]) {
    final int max_language_len1 = 10;
    System.out.println("\nThroughput for all benchmarks (loops per sec):");
    String str = "BMR (" + prg_language + ")";
    for (int i = prg_language.length(); i < max_language_len1; i++) {
      str += " ";
    }
    str += ": ";
    for (int bench = bench1; bench <= bench2; bench++) {
      str += mynumformat1_d(bench_res1[bench], 9, 3) + ' ';
    }
    System.out.println(str);
    System.out.println("");
  }

  private static double measureBench(int bench, int n, int check) {
    final int delta_ms = 100;
    final int max_ms = 10000;
    final int cali_ms = g_cali_ms;

    int loops = 1; // number of loops
    int x;     // result from benchmark
    double tMeas = 0.0;   // measured time
    double tEsti = 0.0;   // estimated time
    double throughput = 0.0;

    System.out.println("Calibrating benchmark " + bench + " with n=" + n + ", check=" + check);
    while (throughput == 0) {
      tMeas = getPrecMs(false);
      x = run_bench(bench, loops, n, check);
      tMeas = getPrecMs(true) - tMeas;

      double t_delta = (tEsti > tMeas) ? (tEsti - tMeas) : (tMeas - tEsti); // compute difference abs(measures-estimated)
      double loops_p_sec = (tMeas > 0) ? (loops * 1000.0 / tMeas) : 0;
      System.out.println(mynumformat1_d(loops_p_sec, 10, 3) + "/s (time=" + mynumformat1_d(tMeas, 9, 3) + " ms, loops=" + mynumformat1_i(loops, 7) + ", delta=" + mynumformat1_d(t_delta, 9, 3) + " ms)");

      if (x == -1) { // some error?
        throughput = -1;
      } else if ((tEsti > 0) && (t_delta < delta_ms)) { // do we have some estimated/expected time smaller than delta_ms=100?
        throughput = loops_p_sec; // yeah, set measured loops per sec
        System.out.println("Benchmark " + bench + " (" + prg_language + "): " + mynumformat1_d(loops_p_sec, 0, 3) + "/s (time=" + mynumformat1_d(tMeas, 9, 3) + " ms, loops=" + loops + ", delta=" + mynumformat1_d(t_delta, 9, 3) + " ms)");
      } else if (tMeas > max_ms) {
        System.out.println("Benchmark " + bench + " (" + prg_language + "): Time already > " + max_ms + " ms. No measurement possible.");
        throughput = (loops_p_sec > 0) ? -loops_p_sec : -1; // cannot rely on measurement, so set to negative
      } else {
        int scale_fact;
        if (tMeas == 0) {
				  scale_fact = 50;
			  } else if (tMeas < cali_ms) {
  				scale_fact = (int)((cali_ms + 100) / tMeas) + 1; // scale a bit up to 1100 ms (cali_ms+100)
        } else {
          scale_fact = 2;
        }
        // scale a bit up to 1100 ms (cali_ms+100)
        loops *= scale_fact;
        tEsti = tMeas * scale_fact;
      }
    }
    return throughput;
  }

  private static int start_bench(int bench1, int bench2, int n, String argStr) {
    determineTsPrecision();
    System.out.println(get_info());
  	if (argStr.length() > 0) {
	  	System.out.println("Args:" + argStr);
	  }

    double bench_res[] = new double[bench2 + 1];

    for (int bench = bench1; bench <= bench2; bench++) {
      int n2 = n;
      if (bench == 3) {
        n2 = n2 / 2;
      } else if (bench == 5) {
        n2 = n2 / 200;
      }
      int check = getCheck(bench, n2);
      double throughput = (check > 0) ? measureBench(bench, n2, check) : -1;
      bench_res[bench] = throughput;
    }

    print_results(bench1, bench2, bench_res);
    return 1;
  }


  static int default_bench1 = 0;   // first benchmark to test
  static int default_bench2 = 5;   // last benchmark to test
  static int default_n = 1000000; // maximum number
  //
  // if you call it with java...
  //
  public static void main(String args[]) {
    String argStr = "";
    if (args.length > 0) {
      default_bench1 = Integer.parseInt(args[0]);
      default_bench2 = default_bench1;
    }
    if (args.length > 1) {
      default_bench2 = Integer.parseInt(args[1]);
    }
    if (args.length > 2) {
      default_n = Integer.parseInt(args[2]);
    }
    if (args.length > 3) {
      g_cali_ms = Integer.parseInt(args[3]);
    }

    for (String s: args) {
      argStr += " " +  s;
    }

    int rc = start_bench(default_bench1, default_bench2, default_n, argStr);
    rc = rc; //avoid warning
    System.out.println("Total elapsed time: " + (int)(conv_ms(get_ts())) + " ms");
  }
}

public class Main {  
    public static void main(String args[]) {
        bmbench.main(args);
    }
}

/*
// https://rextester.com/l/java_online_compiler
class Rextester {  
    public static void main(String args[]) {
        bmbench.main(args);
    }
}
*/

// end
