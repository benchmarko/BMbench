//
// BM Bench - bmbench.cs (C-Sharp, C#)
// (c) Marco Vieth, 2002-2006
// http://www.benchmarko.de
//
// 23.05.2006 0.06  based on version 0.05
//
// Compile:
// - with Visual Studio .NET:
//   "C:\Program Files\Microsoft Visual Studio .NET 2003\Common7\Tools\vsvars32.bat"
//   csc /optimize bmbench.cs
// - or with Mono (1.1.5):
//   mcs bmbench.cs    (mcs -out:bmbench.exe bmbench.cs)
//   mono bmbench.exe  (on Windows; also without mono -> use MS .NET)
//
// Usage:
// bmbench [bench1] [bench2] [n]
//

using System;

class Bmbench {

  static String prg_version = "0.06";
  static String prg_language = "C#";

  //
  // bench00 (Integer 16 bit)
  // (sum of 1..n) mod 65536
  //
  static int bench00(int loops, int n) {
    int x = 0;
    int sum1 = (int)(((n / 2) * (n + 1)) % 65536); // assuming n even! (sum should be ...)
    int n_div_65536 = (n >> 16);
    int n_mod_65536 = (n & 0xffff);
    while (loops-- > 0) {
      for (int i = n_div_65536; i > 0; i--) {
        for (int j = 32767; j > 0; j--) {
          x += j;
        }
        for (int j = -32768; j < 0; j++) {
          x += j;
        }
      }
      for (int j = n_mod_65536; j > 0; j--) {
        x += j;
      }
      if (loops > 0) {   // some more loops left?
        x %= 65536;      // (do not use &= 0xffff)
        x -= sum1;       // yes, set x back to 0
        if (x != 0) {    // now x must be 0 again
          x++;           // force error for many wrong computations
          break;         // Error   //alert("Error(bench01): x="+ x);
        }
      }
    }
    return x % 65536;
  }

  //
  // bench01 (Integer 16/32 bit)
  // (sum of 1..n) mod 65536
  static int bench01(int loops, int n) {
    int x = 0;
    int sum1 = (int)(((n / 2) * (n + 1)) % 65536); // assuming n even! (sum should be ...)
    while (loops-- > 0) {
      for (int i = n; i > 0; i--) {
        x += i;
      }
      if (loops > 0) {   // some more loops left?
        x %= 65536;      // (do not use &= 0xffff)
        x -= sum1;       // yes, set x back to 0
        if (x != 0) {    // now x must be 0 again
          x++;           // force error for many wrong computations
          break;         // Error   //alert("Error(bench01): x="+ x);
        }
      }
    }
    return x % 65536;
  }


  //
  // bench02 (Floating Point, normally 64 bit)
  // (sum of 1..n) mod 65536
  //
  static int bench02(int loops, int n) {
    double x = 0.0;
    double sum1 = (n / 2.0) * (n + 1.0); // assuming n even! (sum should be 5.000005E11)
    while (loops-- > 0) {
      for (int i = n; i > 0; i--) {
        x += i;
      }
      if (loops > 0) {   // some more loops left?
        x -= sum1;       // yes, set x back to 0
        if (x != 0.0) {    // now x must be 0 again
          x++;
          break;         // Error   //alert("Error(bench01): x="+ x);
        }
      }
    }
    return (int)(x % 65536);
  }

  //
  // bench03 (Integer)
  // number of primes below n (Sieve of Eratosthenes)
  // Example: n=500000 => x=41538 (expected), n=1000000 => x=78498
  // (No bit array available, just bool...)
  static int bench03(int loops, int n) {
    n >>= 1; // compute only up to n/2

    int x = 0; // number of primes below n
    //BitArray sieve1 = new BitArray(n + 1); // slower than bool
    //BitArray from http://www.csharpfriends.com/Spec/index.aspx?specID=17.8.htm 
    bool[] sieve1 = new bool[n + 1];
    sieve1[0] = false;
    sieve1[1] = false;
    while (loops-- > 0) {
      // initialize sieve
      for (int i = 2; i <= n; i++) {
        sieve1[i] = true;
      }
      // compute primes
      for (int i = 2; (i * i) <= n; i++) {
        if (sieve1[i] != false) {
          for (int j = i * i; j <= n; j += i) {
            sieve1[j] = false;
          }
        }
      }
      // count primes
      for (int i = 0; i <= n; i++) {
        if (sieve1[i] != false) {
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
  static int bench04(int loops, int n) {
    const int m = 2147483647; // modulus, do not change!
    const int a = 16807;      // multiplier
    const int q = 127773;     // m div a
    const int r = 2836;       // m mod a
    int x = 1;                // last random value
    while (loops-- > 0) {
      for (int i = n; i > 0; i--) {
        int x_div_q = x / q; // (int)
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
  static int bench05(int loops, int n) {
    int x = 0;
    n = (n / 500);
    int k = n >> 1; // div 2

    if ((n - k) < k) {
      k = n - k; // keep k minimal with  n over k  =  n over n-k
    }

    // allocate memory...
    int[][] pas1 = new int[2][];
    pas1[0] = new int[k + 1];
    pas1[1] = new int[k + 1];
    pas1[0][0] = 1; pas1[1][0] = 1; // set first column

    while (loops-- > 0) {
      for (int i = 3; i <= n; i++) {
        int i_mod_2 = i & 1;
        //int i1_mod_2 = i_mod_2 ^ 1;
        int min1 = (i_mod_2 == 0) ? ((i - 2) >> 1) : ((i - 1) >> 1); // Math.floor((i - 1) / 2);
        if (k < min1) {
          min1 = k;
        }
        pas1[i_mod_2][1] = i; // second column is i
        for (int j = 2; j <= min1; j++) { // up to min((i-1)/2, k)
          pas1[i_mod_2][j] = (pas1[i_mod_2 ^ 1][j - 1] + pas1[i_mod_2 ^ 1][j]) & 0xffff; // % 65536 -- we need mod here to avoid overflow
        }
        if ((min1 < k) && (i_mod_2 == 0)) { // new element
          //pas1[i_mod_2][Math.floor(i / 2)] = 2 * pas1[i_mod_2 ^ 1][Math.floor((i - 1) / 2)];
          pas1[i_mod_2][min1 + 1] = 2 * pas1[i_mod_2 ^ 1][min1];
        }
      }
      x += pas1[n & 1][k] & 0xffff; // % 65536
      if (loops > 0) {
        x -= 27200;
        if (x != 0) {   // now x must be 0 again
          x++;
          break;        // Error
        }
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
  static int run_bench(int bench, int loops, int n) {
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
        Console.Error.WriteLine("Error: Unknown benchmark " + bench);
        check1 = x + 1;
        break;
    }
    if (check1 != x) {
      Console.Error.WriteLine("Error(bench" + bench + "): x=" + x);
      x = -1; //exit;
    }
    return (x);
  }

  //
  // get timestamp in milliseconds
  // out: x = time in ms
  //
  // This function is intended for short measurements only.
  //
  static long get_ms() {
    long ticks = System.DateTime.Now.Ticks;
    return ticks / 10000; // convert 100ns -> 1 ms 
    //return (ticks / 10000000) * 1000; // TTT: simulate sec resolution
  }

  static string getdate1() {
    // Creates and initializes a DateTimeFormatInfo associated with the en-US culture.
    System.Globalization.DateTimeFormatInfo dtfi = new System.Globalization.CultureInfo("de-DE", false).DateTimeFormat;
    return System.DateTime.Now.ToString(dtfi); // always German format
  }

  static string getruntime1() {
    string runtimeName = typeof(object).GetType().FullName;
    string runtimeVersion = System.Environment.Version.ToString();
    // 'System.Environment.Version' shows more info than 'System.Reflection.Assembly.GetExecutingAssembly().ImageRuntimeVersion'

    switch (runtimeName) {
      case "System.RuntimeType":
        runtimeName = "Microsoft .NET Framework";
        break;
      case "System.MonoType":
        //runtimeName = "Mono";
        //call Mono.Runtime.GetDisplayName()...
        string runtimeNameVersion = (string)typeof(object).Assembly.GetType("Mono.Runtime").InvokeMember("GetDisplayName", System.Reflection.BindingFlags.InvokeMethod | System.Reflection.BindingFlags.NonPublic | System.Reflection.BindingFlags.Static | System.Reflection.BindingFlags.DeclaredOnly | System.Reflection.BindingFlags.ExactBinding, null, null, null);
        string[] parts = runtimeNameVersion.Split(" ".ToCharArray(), 2);
        runtimeName = parts[0];
        runtimeVersion = parts[1];
        break;
      case "System.Reflection.ClrType":
        runtimeName = "DotGNU Portable.NET";
        break;
      default:
        runtimeName = "<" + runtimeName + ">";
        break;
    }
    return runtimeName + " " + runtimeVersion;
  }


  // Here we compute the number of "significant" bits for positive numbers (which means 53 for double)
  static int checkbits_int1() {
    int num = 1;
    int last_num = 0;
    int bits = 0;
    do {
      last_num = num;
      num *= 2;
      num++;
      bits++;
    } while ((((num - 1) / 2) == last_num) && (bits < 101));
    return bits;
  }

  static int checkbits_double1() {
    double num = 1.0;
    double last_num = 0.0;
    int bits = 0;
    do {
      last_num = num;
      num *= 2.0;
      num++;
      bits++;
    } while ((((num - 1.0) / 2.0) == last_num) && (bits < 101));
    return bits;
  }


  static void print_info() {
    string cs_version = "Runtime: " + getruntime1() + ", " + System.Reflection.Assembly.GetExecutingAssembly().GetName().Version + ", " + Environment.OSVersion.ToString();

    Console.WriteLine("BM Bench v" + prg_version + " (" + prg_language + ") -- (int:" + checkbits_int1() + " double:" + checkbits_double1() + ") " + cs_version);
    Console.WriteLine("(c) Marco Vieth, 2006");
    Console.WriteLine(getdate1());
  }


  static void print_results(int bench1, int bench2, double[] bench_res1, System.Globalization.NumberFormatInfo nfi)
  {
    const int max_language_len1 = 10;
    Console.WriteLine("\nThroughput for all benchmarks (loops per sec):");
    string str = "BMR (" + prg_language + ")";
    for (int i = prg_language.Length; i < max_language_len1; i++) {
      str += " ";
    }
    str += ": ";
    for (int bench = bench1; bench <= bench2; bench++) {
      str += String.Format(nfi, "{0,9:F2} ", bench_res1[bench]);
    }
    Console.WriteLine(str);
    Console.WriteLine("");
  }


  static int start_bench(int bench1, int bench2, int n) {
    const int cali_ms = 1001;
    const int delta_ms = 100;
    const int max_ms = 10000;

    print_info();

    System.Globalization.NumberFormatInfo nfi = new System.Globalization.CultureInfo("en-US", false).NumberFormat;
    // we use this format to get a decimal point for every culture

    double[] bench_res1 = new double[bench2 + 1]; //reserve for up to last benchmark

    for (int bench = bench1; bench <= bench2; bench++) {
      int loops = 1; // number of loops
      int x = 0;     // result from benchmark
      long t1 = 0;   // measured time
      long t2 = 0;   // estimated time
      Console.WriteLine("Calibrating benchmark {0} with n={1}", bench, n);
      while (true) {
        t1 = get_ms();
        x = run_bench(bench, loops, n);
        t1 = get_ms() - t1;

        long t_delta = (t2 > t1) ? (t2 - t1) : (t1 - t2); // compute difference abs(measures-estimated)
        double loops_p_sec = (t1 > 0) ? (loops * 1000.0 / t1) : 0;
        Console.WriteLine("{0,10}/s (time={1,5} ms, loops={2,7}, delta={3,5} ms, x={4})", loops_p_sec.ToString("F3", nfi), t1, loops, t_delta, x);
        if (x == -1) // some error?
        {
          bench_res1[bench] = -1;
          break;
        }
        if (t2 > 0) // do we have some estimated/expected time?
        {
          if (t_delta < delta_ms) // smaller than delta_ms=100?
          {
            bench_res1[bench] = loops_p_sec; // set loops per sec
            Console.WriteLine("Benchmark {0} ({1}): {2}/s (time={3} ms, loops={4}, delta={5} ms)", bench, prg_language, bench_res1[bench].ToString("F3", nfi), t1, loops, t_delta);
            break;
          }

        }
        if (t1 > max_ms) {
          Console.WriteLine("Benchmark {0} ({1}): Time already > {2} ms. No measurement possible.", bench, prg_language, max_ms);
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

    print_results(bench1, bench2, bench_res1, nfi);

    return 0;
  }
  
  
  public static int Main(string[] args) {
    long start_t = get_ms(); // memorize start time
    int bench1 = 0;          // first benchmark to test
    int bench2 = 5;          // last benchmark to test
    int n = 1000000;         // maximum number

    if (args.Length > 0) {
      bench1 = int.Parse(args[0]);
      bench2 = bench1;
    }
    if (args.Length > 1) {
      bench2 = int.Parse(args[1]);
    }
    if (args.Length > 2) {
      n = int.Parse(args[2]);
    }

    int rc = start_bench(bench1, bench2, n);
    
    Console.WriteLine("Total elapsed time: " + (get_ms() - start_t) + " ms");
    //System.Console.ReadLine();
    return rc;
  }

}
// end
