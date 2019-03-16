// bmbench.java
// (c) Benchmarko, 2002
//
// 06.05.2002  0.01
// 11.05.2002  0.02  bench01 = (sum 1..n) mod 65536 (integer)
// 22.05.2002  0.03  bench02 = (sum 1..n) mod 65536 (floating point), bench03 = Sieve of Eratosthenes
// 20.07.2002  0.04  some errors corrected
//
//
// Usage:
// java bmbench [bench] [n]
// or as an applet in a HTML page
//

//
// Compile:
// javac bmbench.java
// (/usr/lib/IBMJava2-1.3.0/bin or /usr/lib/SunJava1-1.1.8/bin/)
// Or:
// guavac, gcj, ...
//

import java.applet.Applet;
//import java.awt.Graphics;  // required for applet



public class bmbench extends java.applet.Applet {


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
    System.out.println("Benchmark 0 not available.");
    return 0;
  }

/*
  private static int bench00_notready1(int loops, int n) {
    short x = 0; // short is -32767..32768
    short sum1 = (short)((n / 2) * (n + 1)); // assuming n even!
    // (sum1..1000000 depends on type: 500000500000 (floating point), 1784293664 (32bit), 10528 (16 bit)
    short n_div_65536 = (short)(n >> 16);
    short n_mod_65536 = (short)(n & 0xffff);
    while (loops-- > 0) {
      for (int i = n_div_65536; i > 0; i--) {
        for (short j = 32767; j != -32768; j--) {
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
*/


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
    boolean sieve1[] = new boolean[n + 1];
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
    switch(bench) {
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
        System.out.println("Error: Unknown benchmark "+ bench);
        check1 = x + 1; // force error
      break;
    }

    if (check1 != x) {
      System.out.println("Error(bench"+ bench +"): x="+ x);
      x = -1; //exit;
    }
    return(x);
  }

  //
  // get timestamp in milliseconds
  // out: x = time in ms
  //
  // This function is intended for short measurements only so we
  // can return it as an integer.
  //
  private static int get_ms() {
    return((int)System.currentTimeMillis());
  }


  private static void start_bench(int bench1, int bench2, int n) {
    int start_t = get_ms();  // memorize start time
    int min_ms = 10000;      // minimum runtime for measurement in ms
    int bench_res1[] = new int[bench2 + 1];

    System.out.println("BM Bench v0.4 (Java)");
    System.out.println("java.version="+ System.getProperty("java.version") +", java.vendor="+ System.getProperty("java.vendor"));
    System.out.println("os.name="+ System.getProperty("os.name") +", os.arch="+ System.getProperty("os.arch")
      +", os.version="+ System.getProperty("os.version"));
    //System.out.println("properties="+ System.getProperties());

    for (int bench = bench1; bench <= bench2; bench++) {
      int loops = 1; // number of loops
      int x = 0;     // result from benchmark
      int t1 = 0;    // timestamp
      // calibration
      while (t1 < 1000) { // we want at least 1 sec calibration time
        System.out.println("Calibrating benchmark "+ bench +" with loops="+ loops +", n="+ n);
        t1 = get_ms();
        x = run_bench(bench, loops, n);
        t1 = get_ms() - t1;
        System.out.println("x="+ x +" (time: "+ t1 +" ms)");
        loops *= 2;
        if (x == -1) {
          break;
        }
      }
      if (x != -1) {
        loops /= 2;
        loops *= (min_ms / t1) + 1; // integer division!
        System.out.println("Calibration done. Starting measurement with "+ loops +" loops to get >="+ min_ms +" ms");

        // measurement
        t1 = get_ms();
        x = run_bench(bench, loops, n);
        t1 = get_ms() - t1;
        System.out.println("x="+ x +" (time: "+ t1 +" ms)");
        System.out.println("Elapsed time for "+ loops +" loops: "+ t1 +" ms; estimation for 10 loops: "+ (t1 * 10 / loops) +" ms");
        bench_res1[bench] = (t1 * 10 / loops);
      } else {
        bench_res1[bench] = -1;
      }
    }
    System.out.println("Summary for 10 Loops:");
    for (int bench = bench1; bench <= bench2; bench++) {
      System.out.println("Benchmark "+ bench +": "+ bench_res1[bench] +" ms");
    }
    System.out.println("Total elapsed time: "+ (get_ms() - start_t) +" ms");
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
    start_bench(default_bench1, default_bench2, default_n);
  }

  //
  // Applet code...
  //
  //int applet_status = 0;

/*
  public void paint(Graphics g) {
    //g.drawString((applet_status == 1) ? "Applet running..." : "Applet ready. Click ?", 0, getSize().height / 2);  // getSize().width / 2
  }

  public synchronized void mousePressed(MouseEvent e) {
    if (applet_status == 0) {
      applet_status = 1;
    } else if (applet_status == 1) {
      applet_status = 0;
    }
    showStatus((applet_status == 1) ? "Applet running..." : "Applet ready. Click?");
  }

*/

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
    start_bench(default_bench1, default_bench2, default_n);
    showStatus("Applet ready. See log...");
  }

  public String getAppletInfo() {
    return "Title: bmbench\nAuthor: Benchmarko, 2002\n";
  }

  public String[][] getParameterInfo() {
    String[][] info = {
      {"bench1", "int", "First Benchmark"},
      {"bench2", "int", "Last Benchmark"},
      {"n", "int", "Workload size n"}
    };
    return info;
  }
}
// end
