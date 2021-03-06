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
//
// Usage:
// java bmbench [bench] [n]
// or as an applet in a HTML page
//

//
// Compile:
// javac -O bmbench.java
// (/usr/lib/IBMJava2-1.3.0/bin or /usr/lib/SunJava1-1.1.8/bin/)
// Or:
// guavac, gcj, ...
//

import java.applet.Applet;
//import java.awt.Graphics;  // required for applet
import java.text.SimpleDateFormat; // just to print date

// import java.text.NumberFormat;


public class bmbench extends java.applet.Applet {

  static String prg_version = "0.06";
  static String prg_language = "Java";

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
  private static int bench00(int loops, int n) {
    short x = 0; // short is -32767..32768
    short sum1 = (short)((n / 2) * (n + 1)); // assuming n even!
    // (sum1..1000000 depends on type: 500000500000 (floating point), 1784293664 (32bit), 10528 (16 bit)
    short n_div_65536 = (short)(n >> 16);
    short n_mod_65536 = (short)(n & 0xffff);
    //System.out.println("DEBUG: sum1="+ sum1 +", ndiv="+ n_div_65536 +", nmod="+ n_mod_65536);
    while (loops-- > 0) {
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
      if (loops > 0) { // some more loops left?
        x -= sum1;     // yes, set x back to 0 (assuming n even)
        if (x != 0) {  // now x must be 0 again
          x++;         // force error for many wrong computations
          break;       // error
        }
      }
    }
    return (int)(x & 0xffff);
  }


  //
  // bench01 (Integer 16/32 bit)
  // (sum of 1..n) mod 65536
  //
  private static int bench01(int loops, int n) {
    int x = 0;
    int sum1 = (n / 2) * (n + 1); // assuming n even! (32 bit sum should be 1784293664)
    while (loops-- > 0) {
      for (int i = n; i > 0; i--) {
        x += i;
      }
      if (loops > 0) {  // some more loops left?
        x -= sum1;      // yes, set x back to 0
        if (x != 0) {   // now x must be 0 again
          x++;          // force error for many wrong computations
          return x;     // Error
        }
      }
    }
    return (x & 0xffff); // % 65536
  }


  //
  // bench02 (Floating Point, normally 64 bit)
  // (sum of 1..n) mod 65536
  //
  private static int bench02(int loops, int n) {
    double x = 0.0;
    double sum1 = ((double)n / 2.0) * (n + 1.0); // assuming n even! (sum should be 5.000005E11)
    while (loops-- > 0) {
      for (int i = n; i > 0; i--) {
        x += (double)i;
      }
      if (loops > 0) {    // some more loops left?
        x -= sum1;        // yes, set x back to 0
        if (x != 0.0) {   // now x must be 0 again
          x++;
          break;          // Error
        }
      }
    }
    return (int)(x % 65536);
  }


  //
  // bench03 (Integer)
  // number of primes below n (Sieve of Eratosthenes)
  // Example: n=500000 => x=41538 (expected), n=1000000 => x=78498
  //
  private static int bench03(int loops, int n) {
    n /= 2; // compute only up to n/2
    int x = 0; // number of primes below n
    boolean sieve1[] = new boolean[n+1];
    sieve1[0] = false;
    sieve1[1] = false;
    while (loops-- > 0) {
      // initialize sieve
      for (int i = 2; i <= n; i++) {
        sieve1[i] = true;
      }
      // compute primes
      for (int i = 2; (i * i) <= n; i++) {
        if (sieve1[i]) {
          for (int j = i * i; j <= n; j += i) {
            sieve1[j] = false;
          }
        }
      }
      // count primes
      for (int i = 0; i <= n; i++) {
        if (sieve1[i]) {
          x++;
        }
      }
      // check prime count
      if (loops > 0) {  // some more loops left?
        x -= 41538;     // yes, set x back to 0 (number of primes below 1000000)
        if (x != 0) {   // now x must be 0 again
          x++;
          break;        // Error
        }
      }
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
  public static int bench04(int loops, int n) {
    final int m = 2147483647; // modulus, do not change!
    final int a = 16807;      // multiplier
    final int q = 127773;     // m div a
    final int r = 2836;       // m mod a
    int x = 1;                // last random value
    while (loops-- > 0) {
      for (int i = n; i > 0; i--) {
        int x_div_q = x / q;
        int x_mod_q = x - q * x_div_q;
        x = a * x_mod_q - r * x_div_q;
        if (x <= 0) {
          x += m; // x is new random number
        }
      }
      if (loops > 0) {
        x -= 1227283347;
        if (x != 0) {   // now x must be 0 again
          x++;
          break;        // Error
        }
        x++; // start with 1 again
      }
    }
    return x;
  }


  //
  // bench05 (Integer 32 bit)
  // n over n/2 mod 65536 (Pascal's triangle)
  //
  private static int bench05(int loops, int n_p) {
    int x = 0;
    int n = n_p / 500;
    int k = n / 2;

    if ((n - k) < k) {
      k = n - k; // keep k minimal with  n over k  =  n over n-k
    }

    // allocate memory...
    int pas1[][] = new int[2][k + 1];
    pas1[0][0] = 1; pas1[1][0] = 1; // set first column

    while (loops-- > 0) {
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
      x += pas1[n % 2][k] & 0xffff; // % 65536
      if (loops > 0) {
        x -= 27200;
        if (x != 0) {   // now x must be 0 again
          x++;
          break;        // Error
        }
      }
    }

    /*
     for (int i = 0; i < 2; i++) {
     System.out.print(i +": ");
     for (int j = 0; j < k; j++) {
     System.out.print(pas1[i % 2][j] +" ");
     }
     System.out.println();
     }
     */
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
    int check1 = 0;
    switch (bench) {
      case 0:
        x = bench00(loops, n);
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
        check1 = 41538;
        break;

      case 4:
        x = bench04(loops, n);
        check1 = 1227283347;
        break;

      case 5:
        x = bench05(loops, n);
        check1 = 27200;
        break;

      default:
        System.out.println("Error: Unknown benchmark " + bench);
        check1 = x + 1; // force error
        break;
    }

    if (check1 != x) {
      System.out.println("Error(bench" + bench + "): x=" + x);
      x = -1; //exit;
    }
    return (x);
  }

  //
  // get timestamp in milliseconds
  // out: x = time in ms
  //
  // Even if the function is intended for short measurements we should
  // return a long to avoid overflows...
  //
  private static long get_ms() {
    return (System.currentTimeMillis());
  }


  private static String getdate1() {
    long date1 = get_ms();
    SimpleDateFormat format1 = new SimpleDateFormat("dd.MM.yyyy HH:mm:ss");
    return format1.format(new java.util.Date(date1));
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
      + " long:" + checkbits_long1() + " float:" + checkbits_float1() + " double:" + checkbits_double1() + ") ");
    System.out.print("java.version=" + System.getProperty("java.version") + ", java.vendor=" + System.getProperty("java.vendor"));
    System.out.println("os.name=" + System.getProperty("os.name") + ", os.arch=" + System.getProperty("os.arch")
      + ", os.version=" + System.getProperty("os.version"));
    System.out.println("(c) Marco Vieth, 2006");
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
      str += mynumformat1_d(bench_res1[bench], 9, 2) + ' ';
    }
    System.out.println(str);
    System.out.println("");
  }


  private static int start_bench(int bench1, int bench2, int n) {
    final int cali_ms = 1001;
    final int delta_ms = 100;
    final int max_ms = 10000;

    print_info();

    double bench_res1[] = new double[bench2+1];

    for (int bench = bench1; bench <= bench2; bench++) {
      int loops = 1; // number of loops
      int x = 0;     // result from benchmark
      long t1 = 0;   // measured time
      long t2 = 0;   // estimated time
      System.out.println("Calibrating benchmark " + bench + " with n=" + n);
      while(true) {
        t1 = get_ms();
        x = run_bench(bench, loops, n);
        t1 = get_ms() - t1;

        long t_delta = (t2 > t1) ? (t2 - t1) : (t1 - t2); // compute difference abs(measures-estimated)
        double loops_p_sec = (t1 > 0) ? (loops * 1000.0 / t1) : 0;
        System.out.println(mynumformat1_d(loops_p_sec, 10, 3) + "/s (time=" + mynumformat1_i((int)t1, 5) + " ms, loops=" + mynumformat1_i(loops, 7) + ", delta=" + mynumformat1_i((int)t_delta, 5) + " ms, x=" + x);
        if (x == -1) { // some error?
          bench_res1[bench] = -1;
          break;
        }
        if (t2 > 0) { // do we have some estimated/expected time?
          if (t_delta < delta_ms) { // smaller than delta_ms=100?
            bench_res1[bench] = loops_p_sec; // set loops per sec
            System.out.println("Benchmark " + bench + " (" + prg_language + "): " + mynumformat1_d(bench_res1[bench], 0, 3) + "/s (time=" + t1 + " ms, loops=" + loops + ", delta=" + t_delta + " ms)");
            break;
          }

        }
        if (t1 > max_ms) {
          System.out.println("Benchmark " + bench + " (" + prg_language + "): Time already > " + max_ms + " ms. No measurement possible.");
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
    return 1;
  }


  static int default_bench1 = 0;   // first benchmark to test
  static int default_bench2 = 5;   // last benchmark to test
  static int default_n = 1000000; // maximum number
  //
  // if you call it with java...
  //
  public static void main(String args[]) {
    long start_t = get_ms();  // memorize start time        
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
    int rc = start_bench(default_bench1, default_bench2, default_n);
    rc = rc; //avoid warning
    System.out.println("Total elapsed time: " + (get_ms() - start_t) + " ms");
  }



  //
  // Applet code...
  //

  //
  // Initialize the applet.
  //
  public void init() {
    String bench_str = getParameter("bench1");
    if (bench_str != null) {
      default_bench1 = Integer.parseInt(bench_str);
      default_bench2 = default_bench1;
    }
    bench_str = getParameter("bench2");
    if (bench_str != null) {
      default_bench2 = Integer.parseInt(bench_str);
    }
    String n_str = getParameter("n");
    if (n_str != null) {
      default_n = Integer.parseInt(n_str);
    }
  }

  public void simbench() {
    showStatus("Applet running...");
    long start_t = get_ms();  // memorize start time     
    int rc = start_bench(default_bench1, default_bench2, default_n);
    System.out.println("Total elapsed time: " + (get_ms() - start_t) + " ms");    
    showStatus("Applet ready. See log (rc=" + rc +")...");
  }

  public String getAppletInfo() {
    return "Title: bmbench\nAuthor: Marco Vieth, 2006 (http://www.benchmarko.de)\n";
  }

  public String[][] getParameterInfo() {
    String[][] info = {
      { "bench1", "int", "First Benchmark" },
      { "bench2", "int", "Last Benchmark" },
      {"n", "int", "Workload size n"}
    };
    return info;
  }
}
// end
