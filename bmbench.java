//
// BM Bench - bmbench.java (Java)
// (c) Marco Vieth, 2002-2006
// http://www.benchmarko.de
//
// 06.05.2002 0.01
// 11.05.2002 0.02  bench01 = (sum 1..n) mod 65536 (integer)
// 22.05.2002 0.03  bench02 = (sum 1..n) mod 65536 (floating point), bench03 = Sieve of Eratosthenes
// 20.07.2002 0.04  some errors corrected
// 24.01.2003 0.05  output format changed
// 23.05.2006 0.06  based on version 0.05
// 05.05.2019 0.07  changed bench 00-03; time interval estimation
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
  static String prg_version = "0.07";
  static String prg_language = "Java";

  private static long gState_startTs = 0;
  private static double gState_tsPrecMs = 0; // measured time stamp precision
  private static int gState_tsPrecCnt = 0; // time stamp count (calls) per precision interval (until time change)
  private static int gState_tsMeasCnt = 0; // last measured count

  
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
  // out:    x = <output decription>
  //
  // loops may be increased to produce a longer runtime without changing the result.
  //


  //
  // bench00 (Integer 16 bit)
  // (sum of 1..n) mod 65536
  // can be computed with short
  //
  private static int bench00(int loops, int n, int check) {
    short x = 0; // short is -32767..32768
    short n_div_65536 = (short)(n >> 16);
    short n_mod_65536 = (short)(n & 0xffff);
    //System.out.println("DEBUG: sum1="+ sum1 +", ndiv="+ n_div_65536 +", nmod="+ n_mod_65536);
    while (loops-- > 0 && x == 0) {
      for (int i = n_div_65536; i > 0; i--) {
        for (short j = 32767; j > 0; j--) {
          x += j;
        }
        for (short j = -32768; j < 0; j++) {
          x += j;
        }
      }
      for (short j = n_mod_65536; j > 0; j--) {
        x += j;
      }
      //x &= 0xffff; // not needed because x is short
      x -= check;
    }
    return x & 0xffff;
  }


  //
  // bench01 (Integer 16/32 bit)
  // (arithmetic mean of 1..n)
  //
  private static int bench01(int loops, int n, int check) {
    int x = 0;
    while (loops-- > 0 && x == 0) {
      int sum = 0;
      for (int i = 1; i <= n; i++) {
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


  //
  // bench02 (Floating Point, normally 64 bit)
  // (arithmetic mean of 1..n)
  //
  private static int bench02(int loops, int n, int check) {
    int x = 0;
    while (loops-- > 0 && x == 0) {
      double sum = 0.0;
      for (int i = 1; i <= n; i++) {
        sum += i;
        if (sum >= n) {
          sum -= n;
          x++;
        }
      }
      x -= check;
    }
    return x;
  }


  //
  // bench03 (Integer)
  // number of primes below n (Sieve of Eratosthenes)
  // Example: n=500000 => x=41538 (expected), n=1000000 => x=78498
  //
  private static int bench03(int loops, int n, int check) {
    n /= 2; // compute only up to n/2
    int x = 0; // number of primes below n
    int nHalf = n >> 1;
    boolean sieve1[] = new boolean[nHalf + 1];

    while (loops-- > 0 && x == 0) {
      // initialize sieve
      for (int i = 0; i <= nHalf; i++) {
        sieve1[i] = false;
      }
      // compute primes
      int i = 0;
      int m = 3;
      while (m * m < n) {
        if (!sieve1[i]) {
          x++;
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
      while (i <= nHalf) {
        if (!sieve1[i]) {
          x++;
        }
        i++;
      }
      x -= check;
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
  private static int bench04(int loops, int n, int check) {
    final int m = 2147483647; // modulus, do not change!
    final int a = 16807;      // multiplier
    final int q = 127773;     // m div a
    final int r = 2836;       // m mod a
    int x = 0;                // last random value
    while (loops-- > 0 && x == 0) {
      x++; // start with 1
      for (int i = n; i > 0; i--) {
        x = a * (x % q) - r * (x / q); // x div q
        if (x <= 0) {
          x += m; // x is new random number
        }
      }
      x -= check;
    }
    return x;
  }


  //
  // bench05 (Integer 32 bit)
  // n over n/2 mod 65536 (Pascal's triangle)
  //
  private static int bench05(int loops, int n_p, int check) {
    int x = 0;
    int n = n_p / 500;
    int k = n / 2;

    if ((n - k) < k) {
      k = n - k; // keep k minimal with  n over k  =  n over n-k
    }

    // allocate memory...
    int pas1[][] = new int[2][k + 1];
    pas1[0][0] = 1; pas1[1][0] = 1; // set first column

    while (loops-- > 0 && x == 0) {
      for (int i = 2; i <= n; i++) {
        int i_mod_2 = i % 2;
        int min1 = (i - 1) / 2;
        if (k < min1) {
          min1 = k;
        }
        pas1[i_mod_2][1] = i; // second column is i
        for (int j = 2; j <= min1; j++) { // up to min((i-1)/2, k)
          pas1[i_mod_2][j] = (pas1[i_mod_2 ^ 1][j - 1] + pas1[i_mod_2 ^ 1][j]);
        }
        if ((min1 < k) && (i_mod_2 == 0)) { // new element
          pas1[i_mod_2][min1 + 1] = 2 * pas1[i_mod_2 ^ 1][min1];
        }
      }
      x += pas1[n % 2][k] & 0xffff;
      x -= check;
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
  public static int run_bench(int bench, int loops, int n) {
    int x = 0;
    int check;
    switch (bench) {
      case 0:
        check = (short)((n / 2) * (n + 1)) & 0xffff;
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
        System.out.println("Error: Unknown benchmark " + bench);
        check = -1;
        break;
    }

    x += check;
    if (x != check) {
      System.out.println("Error(bench" + bench + "): x=" + x);
      x = -1; // exit
    }
    return x;
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

  private static String getdate1() {
    long date_ms = System.currentTimeMillis();
    SimpleDateFormat format1 = new SimpleDateFormat("dd.MM.yyyy HH:mm:ss");
    return format1.format(new java.util.Date(date_ms));
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
    double displ_prec_after = Math.pow(10, 2);  // display precision after decimal point
    str.append(Math.round(val * displ_prec_after) / (displ_prec_after * 1.0));

    if (str.toString().indexOf('.') < 0) { // should not occur
      System.out.println("WARNING: str does not contain a dot: " + str);
      str.append('.');
    }

    // format to prec digits after comma
    while ((str.length() <= prec) || (str.charAt(str.length() - (prec + 1)) != '.')) {
      //int len1 = str.length();
      //int pos1 = str.length() - (prec - 1);
      str.append("0");
    }

    for (int i = str.length(); i < digits; i++) {
      str.insert(0, ' ');
    }
    return (str.toString());
  }


  private static void print_info() {
    System.out.print("BM Bench v" + prg_version + " (" + prg_language + ") -- (short:" + checkbits_short1() + " int:" + checkbits_int1()
      + " long:" + checkbits_long1() + " float:" + checkbits_float1() + " double:" + checkbits_double1() + " tsMs:" + gState_tsPrecMs + " tsCnt:" + gState_tsPrecCnt + ") ");
    System.out.print("java.version=" + System.getProperty("java.version") + ", java.vendor=" + System.getProperty("java.vendor"));
    System.out.println("os.name=" + System.getProperty("os.name") + ", os.arch=" + System.getProperty("os.arch")
      + ", os.version=" + System.getProperty("os.version"));
    System.out.println("(c) Marco Vieth, 2002-1019");
    System.out.println("Date: " + getdate1());
    //System.out.println("properties="+ System.getProperties());
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

  private static double measureBench(int bench, int n) {
    final int cali_ms = 1001;
    final int delta_ms = 100;
    final int max_ms = 10000;
    int loops = 1; // number of loops
    int x;     // result from benchmark
    double tMeas = 0;   // measured time
    double tEsti = 0;   // estimated time
    double throughput = 0;

    System.out.println("Calibrating benchmark " + bench + " with n=" + n);
    while (throughput == 0) {
      tMeas = getPrecMs(false);
      x = run_bench(bench, loops, n);
      tMeas = getPrecMs(true) - tMeas;

      double t_delta = (tEsti > tMeas) ? (tEsti - tMeas) : (tMeas - tEsti); // compute difference abs(measures-estimated)
      double loops_p_sec = (tMeas > 0) ? (loops * 1000.0 / tMeas) : 0;
      System.out.println(mynumformat1_d(loops_p_sec, 10, 3) + "/s (time=" + mynumformat1_d(tMeas, 9, 3) + " ms, loops=" + mynumformat1_i(loops, 7) + ", delta=" + mynumformat1_d(t_delta, 9, 3) + " ms, x=" + x);

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

  private static int start_bench(int bench1, int bench2, int n) {
    print_info();

    double bench_res[] = new double[bench2 + 1];

    for (int bench = bench1; bench <= bench2; bench++) {
      double throughput = measureBench(bench, n);
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
    determineTsPrecision();
    int rc = start_bench(default_bench1, default_bench2, default_n);
    rc = rc; //avoid warning
    System.out.println("Total elapsed time: " + (int)(conv_ms(get_ts())) + " ms");
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
