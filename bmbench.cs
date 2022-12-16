//
// BM Bench - bmbench.cs (C-Sharp, C#)
// (c) Marco Vieth, 2002-2006
// http://www.benchmarko.de
//
// 23.05.2006 0.06  based on version 0.05
// 18.05.2019 0.07  changed bench 01-03; time interval estimation
// 03.12.2022 0.072 bench03 corrected, bench05 improved
//
//
// Compile & Run:
// - .Net Core (https://dotnet.microsoft.com/download)
//   dotnet run -p bmbench.csproj -c Release [bench1] [bench2] [n]
//   dotnet build bmbench.csproj -c Release  (build only)
//   dotnet bin/Release/netcoreapp2.2/bmbench_cs.dll [bench1] [bench2] [n]  (run only)
//
// - Visual Studio .NET (C:\Program Files\Microsoft Visual Studio .NET 2003\Common7\Tools\vsvars32.bat)
//   csc /optimize bmbench.cs
//   bmbench.exe [bench1] [bench2] [n]
//
// - Mono (since 1.1.5):
//   mcs -optimize -out:bmbench_cs_mono.exe bmbench.cs
//   mono bmbench_cs.exe  (on Windows also without mono -> use MS .NET)
//  [Disassemble: monodis.exe --output=bmbench_mono.txt bmbench_mono.exe]
//

using System;

class Bmbench {

  static String g_prg_version = "0.072";
  static String g_prg_language = "C#";
  private static long g_startTs = 0;
  private static double g_tsPrecMs = 0; // measured time stamp precision
  private static int g_tsPrecCnt = 0; // time stamp count (calls) per precision interval (until time change)
  private static int g_tsMeasCnt = 0; // last measured count

  //private static System.Globalization.NumberFormatInfo nfi = new System.Globalization.CultureInfo("en-US", false).NumberFormat;
  private static IFormatProvider nfi = System.Globalization.CultureInfo.InvariantCulture; //new System.Globalization.CultureInfo("en-US", false).NumberFormat;
  // we use this format to get a decimal point for every culture

  //
  // bench00 (Integer 16 bit)
  // (sum of 1..n) mod 65536
  //
  static int bench00(int loops, int n, int check) {
    int x = 0;
    //int sum1 = (int)(((n / 2) * (n + 1)) % 65536); // assuming n even! (sum should be ...)
    int n_div_65536 = (n >> 16);
    int n_mod_65536 = (n & 0xffff);
    while (loops-- > 0 && x == 0) {
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
      x &= 0xffff;
      x -= check;
    }
    return x & 0xffff;
  }

  //
  // bench01 (Integer 16/32 bit)
  // (arithmetic mean of 1..n)
//
  static int bench01(int loops, int n, int check) {
    int x = 0;
    //int sum1 = (int)(((n / 2) * (n + 1)) % 65536); // assuming n even! (sum should be ...)
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
  static int bench02(int loops, int n, int check) {
    int x = 0;
    //double sum1 = (n / 2.0) * (n + 1.0); // assuming n even! (sum should be 5.000005E11)
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
  // number of primes less than or equal to n (prime-counting function)
  // Example: n=500000 => x=41538 (expected), n=1000000 => x=78498
  // (Sieve of Eratosthenes, no multiples of 2's are stored)
  // (BitArray sieve1 = new BitArray(n + 1); // slower than bool)
  // (BitArray from http://www.csharpfriends.com/Spec/index.aspx?specID=17.8.htm)
  static int bench03(int loops, int n, int check) {
    n >>= 1; // compute only up to n/2

    int x = 0; // number of primes below n
    int nHalf = n >> 1;
    bool[] sieve1 = new bool[nHalf + 1];
    int i;    

    while (loops-- > 0 && x == 0) {
      // initialize sieve
      for (i = 0; i <= nHalf; i++) {
        sieve1[i] = false;
      }
      // compute primes
      i = 0;
      int m = 3;
      x++; // 2 is prime
      while (m * m < n) {
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

      // count primes
      for (; m <= n; i++, m += 2) {
        if (!sieve1[i]) {
          x++; // m is prime
        }
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
  static int bench04(int loops, int n, int check) {
    const int m = 2147483647; // modulus, do not change!
    const int a = 16807;      // multiplier
    const int q = 127773;     // m div a
    const int r = 2836;       // m mod a
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
  static int bench05(int loops, int n, int check) {
    int x = 0;
    n = (n / 500);
    int k = n >> 1; // div 2

    if ((n - k) < k) {
      k = n - k; // keep k minimal with  n over k  =  n over n-k
    }

    // allocate memory...
    int[] line = new int[k + 1];
    int[] lastLine = new int[k + 1];
    line[0] = 1;
    lastLine[0] = 1; // set first column

    while (loops-- > 0 && x == 0) {
      // initialize
      for (int j = 1; j <= k; j++) {
        line[j] = 0;
        lastLine[j] = 0;
      }

      // compute
      for (int i = 3; i <= n; i++) {
        int min1 = (i - 1) / 2;
        if (k < min1) {
          min1 = k;
        }
        line[1] = i; // second column is i
        for (int j = 2; j <= min1; j++) {
          line[j] = (lastLine[j - 1] + lastLine[j]) & 0xffff; // we need mod here to avoid overflow
        }
        if ((min1 < k) && ((i & 1) == 0)) { // new element
          line[min1 + 1] = 2 * lastLine[min1];
        }
        int[] tempLine = lastLine;
        lastLine = line;
        line = tempLine;
      }
      x += lastLine[k] & 0xffff;
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
  static int run_bench(int bench, int loops, int n) {
    int x = 0;
    int check = 0;
    switch (bench) {
      case 0:
        check = ((n / 2) * (n + 1)) & 0xffff;
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
        Console.Error.WriteLine("Error: Unknown benchmark " + bench);
        check = -1;
        break;
    }

    x += check;
    if (x != check) {
      Console.Error.WriteLine("Error(bench" + bench + "): x=" + x);
      x = -1; //exit;
    }
    return x;
  }


  // get timestamp with full precision
  static long get_raw_ts() {
      //return System.DateTime.Now.Ticks; //100ns ticks
      //return (System.DateTime.Now.Ticks / 100000) * 1000000; // Test: simulate 10 ms resolution
      return System.DateTime.Now.Ticks;
  }

  // get timestamp since program start
  // int should be enough
  static int get_ts() {
      return (int)(get_raw_ts() - g_startTs);
  }

  // convert timestamp to ms
  static double conv_ms(int ts) {
    return ts / 10000.0;
  }

  static double correctTime(double tMeas, double tMeas2, int measCount) {
    int tsPrecCnt = g_tsPrecCnt;

    if (measCount < tsPrecCnt) {
      tMeas += g_tsPrecMs * ((tsPrecCnt - measCount) / tsPrecCnt); // ts + correction
      if (tMeas > tMeas2) {
        tMeas = tMeas2; // cannot correct
      }
    }
    return tMeas;
  }

   static double getPrecMs(bool stopFlg) {
    int measCount = 0;

    int tMeas0 = get_ts();
    int tMeas = tMeas0;
    while (tMeas <= tMeas0) {
      tMeas = get_ts();
      measCount++;
    }
    g_tsMeasCnt = measCount; // memorize count
    //Console.WriteLine("DEBUG: getPrecMs: measCount=" + measCount + " ts=" + tMeas);

    // for stop: use first ts + correction
    double tMeasD = (!stopFlg) ? conv_ms(tMeas) : correctTime(conv_ms(tMeas0), conv_ms(tMeas), measCount);
    return tMeasD;
  }

  // usually only needed if time precision is low, e.g. one second
  static void determineTsPrecision() {
    g_startTs = get_raw_ts(); // memorize start time

    double tMeas0 = getPrecMs(false);
    double tMeas1 = getPrecMs(false);
    g_tsPrecMs = tMeas1 - tMeas0;
    g_tsPrecCnt = g_tsMeasCnt;

    // do it again
    tMeas0 = tMeas1;
    tMeas1 = getPrecMs(false);
    if (g_tsMeasCnt > g_tsPrecCnt) { // taker maximum count
      g_tsPrecCnt = g_tsMeasCnt;
      g_tsPrecMs = tMeas1 - tMeas0;
    }
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

  static void print_info() {
    string version1 = "";

    try {
      version1 = System.Reflection.Assembly.GetExecutingAssembly().GetName().Version.ToString();
    } catch (System.Security.SecurityException) {
    }

    string cs_version = "Runtime: " + getruntime1() + ", " + version1 + ", " + Environment.OSVersion.ToString();

    Console.WriteLine("BM Bench v" + g_prg_version + " (" + g_prg_language + ") -- (int:" + checkbits_int1() + " double:" + checkbits_double1() + " tsMs:" + g_tsPrecMs.ToString("", nfi) + " tsCnt:" + g_tsPrecCnt + ") " + cs_version);
    Console.WriteLine("(c) Marco Vieth, 2006-2022");
    Console.WriteLine(getdate1());
  }


  static void print_results(int bench1, int bench2, double[] bench_res1) {
    const int max_language_len1 = 10;
    Console.WriteLine("\nThroughput for all benchmarks (loops per sec):");
    string str = "BMR (" + g_prg_language + ")";
    for (int i = g_prg_language.Length; i < max_language_len1; i++) {
      str += " ";
    }
    str += ": ";
    for (int bench = bench1; bench <= bench2; bench++) {
      str += String.Format(nfi, "{0,9:F3} ", bench_res1[bench]);
    }
    Console.WriteLine(str);
    Console.WriteLine("");
  }


  static double measureBench(int bench, int n) {
    const int cali_ms = 1001;
    const int delta_ms = 100;
    const int max_ms = 10000;
    int loops = 1; // number of loops
    int x = 0;     // result from benchmark
    double tMeas = 0;   // measured time
    double tEsti = 0;   // estimated time
    double throughput = 0;

    Console.WriteLine("Calibrating benchmark {0} with n={1}", bench, n);
    while (throughput == 0) {
      tMeas = getPrecMs(false); //conv_ms(get_ts()); //getPrecMs(false);
      x = run_bench(bench, loops, n);
      tMeas = getPrecMs(true) - tMeas; //conv_ms(get_ts()) - t1; //getPrecMs(true) - t1;

      double t_delta = (tEsti > tMeas) ? (tEsti - tMeas) : (tMeas - tEsti); // compute difference abs(measures-estimated)
      double loops_p_sec = (tMeas > 0) ? (loops * 1000.0 / tMeas) : 0;
      Console.WriteLine("{0,10}/s (time={1,9} ms, loops={2,7}, delta={3,9} ms, x={4})", loops_p_sec.ToString("F3", nfi), tMeas.ToString("F3", nfi), loops, t_delta.ToString("F3", nfi), x);

      if (x == -1) { // some error?
        throughput = -1;
      } else if ((tEsti > 0) && (t_delta < delta_ms)) { // do we have some estimated/expected time smaller than delta_ms=100?
          throughput = loops_p_sec; // yeah, set measured loops per sec
          Console.WriteLine("Benchmark {0} ({1}): {2}/s (time={3} ms, loops={4}, delta={5} ms)", bench, g_prg_language, loops_p_sec.ToString("F3", nfi), tMeas.ToString("F3", nfi), loops, t_delta.ToString("F3", nfi));
      } else if (tMeas > max_ms) {
        Console.WriteLine("Benchmark {0} ({1}): Time already > {2} ms. No measurement possible.", bench, g_prg_language, max_ms);
        throughput = (loops_p_sec > 0) ? -loops_p_sec : -1; // cannot rely on measurement, so set to negative
      } else {
        //int scale_fact = ((tMeas < cali_ms) && (tMeas > 0)) ? (int)((cali_ms + 100) / tMeas) + 1 : 2;
        //double scale_fact = (tMeas == 0) ? 50 : (tMeas < cali_ms) ? ((cali_ms + 100) / tMeas) : 2;
        int scale_fact;
        if (tMeas == 0) {
				  scale_fact = 50;
			  } else if (tMeas < cali_ms) {
  				scale_fact = (int)((cali_ms + 100) / tMeas) + 1; // scale a bit up to 1100 ms (cali_ms+100)
        } else {
          scale_fact = 2;
        }
        loops *= scale_fact;
        tEsti = tMeas * scale_fact;
      }
    }
    return throughput;
  }


 static int start_bench(int bench1, int bench2, int n) {

    print_info();

    double[] bench_res1 = new double[bench2 + 1]; //reserve for up to last benchmark

    for (int bench = bench1; bench <= bench2; bench++) {
      double throughput = measureBench(bench, n);
      bench_res1[bench] = throughput;
    }

    print_results(bench1, bench2, bench_res1);

    return 0;
  }

  
  public static int Main(string[] args) {
    int bench1 = 0;          // first benchmark to test
    int bench2 = 5;          // last benchmark to test
    int n = 1000000;         // maximum number

    //Console.WriteLine("Stdout is tty: {0}", Console.IsInputRedirected);

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
    determineTsPrecision();
    int rc = start_bench(bench1, bench2, n);
    
    Console.WriteLine("Total elapsed time: " + (int)(conv_ms(get_ts())) + " ms");
    //System.Console.ReadLine();
    return rc;
  }

}


//Can be run in https://rextester.com/l/csharp_online_compiler
/*
namespace Rextester {
    public class Program {
        public static void Main(string[] args) {
            string[] dummyArgs = {};
            Bmbench.Main(dummyArgs);
        }
    }
}
*/

// end
