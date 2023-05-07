//
// BM Bench - bmbench.cs (C-Sharp, C#)
// (c) Marco Vieth, 2002-2006
// http://www.benchmarko.de
//
// 23.05.2006 0.06  based on version 0.05
// 18.05.2019 0.07  changed bench 01-03; time interval estimation
// 03.12.2022 0.072 bench03 corrected, bench05 improved
// 19.02.2023 0.08  bench05 optimized
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

public class Bmbench {

  static String g_prg_version = "0.08";
  static String g_prg_language = "C#";
  private static long g_startTs = 0;
  private static double g_tsPrecMs = 0; // measured time stamp precision
  private static int g_tsPrecCnt = 0; // time stamp count (calls) per precision interval (until time change)
  private static int g_tsMeasCnt = 0; // last measured count
  private static int g_cali_ms = 1001; //
  private static int maxBench = 6;

  //private static System.Globalization.NumberFormatInfo nfi = new System.Globalization.CultureInfo("en-US", false).NumberFormat;
  private static IFormatProvider nfi = System.Globalization.CultureInfo.InvariantCulture; //new System.Globalization.CultureInfo("en-US", false).NumberFormat;
  // we use this format to get a decimal point for every culture

  //
  // bench00 (Integer 16 bit)
  // (sum of 1..n) mod 65536
  //
  private static int bench00(int n) {
    int x = 0;
    int n_div_65536 = n >> 16;
    int n_mod_65536 = n & 0xffff;
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


  private static bool[] bench03Sieve1;

  //
  // bench03 (Integer)
  // number of primes less than or equal to n (prime-counting function)
  // Example: n=500000 => x=41538 (expected), n=1000000 => x=78498
  // (Sieve of Eratosthenes, no multiples of 2's are stored)
  // (BitArray sieve1 = new BitArray(n + 1); // slower than bool)
  // (BitArray from http://www.csharpfriends.com/Spec/index.aspx?specID=17.8.htm)
  private static int bench03(int n) {
    int nHalf = n >> 1;

    // allocate memory...
    if (bench03Sieve1 == null) {
      bench03Sieve1 = new bool[nHalf + 1];
    }
    bool[] sieve1 = bench03Sieve1;

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

    // count primes
    for (; m <= n; i++, m += 2) {
      if (!sieve1[i]) {
        x++; // m is prime
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
  private static int bench04(int n) {
    const int m = 2147483647; // modulus, do not change!
    const int a = 16807;      // multiplier
    const int q = 127773;     // m div a
    const int r = 2836;       // m mod a
    int x = 1;                // last random value
    for (int i = n; i > 0; i--) {
      x = a * (x % q) - r * (x / q); // x div q
      if (x <= 0) {
        x += m; // x is new random number
      }
    }
    return x;
  }

  private static int[] bench05Line1;

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
    int[] line = bench05Line1;

    // initialize (not needed)
    for (int j = 0; j <= k; j++) {
      line[j] = 0;
    }

    line[0] = 1;
    if (line.Length > 1) {
      line[1] = 2; // for line 2, second column is 2
    }

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
      sum += flip / (2*i - 1);       
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
  private static int run_bench(int bench, int loops, int n, int check) {
    if (bench > maxBench) {
      Console.Error.WriteLine("Error: Unknown benchmark " + bench);
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
          Console.Error.WriteLine("Error: Unknown benchmark " + bench);
          check = -1;
          break;
      }
      x -= check;
    }

    x += check;
    if (x != check) {
      Console.Error.WriteLine("Error(bench" + bench + "): x=" + x);
      x = -1; //exit
    }
    return x;
  }


  private static int bench03Check(int n) {
		int x;

    if (n == 500000) {
	  	x = 41538;
	  } else {
		  x = 1; // 2 is prime
      for (int j = 3; j <= n; j += 2) {
        bool isPrime = true;
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

  private static int getCheck(int bench, int n) {
    int check;

    switch (bench) {
      case 0: // (n / 2) * (n + 1)
        //check = ((n / 2) * (n + 1)) & 0xffff;
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
        Console.Error.WriteLine("Error: Unknown benchmark " + bench);
        check = -1;
        break;
    }
    return check;
  }

  // get timestamp with full precision
  private static long get_raw_ts() {
      //return System.DateTime.Now.Ticks; //100ns ticks
      //return (System.DateTime.Now.Ticks / 100000) * 1000000; // Test: simulate 10 ms resolution
      return System.DateTime.Now.Ticks;
  }

  // get timestamp since program start
  // int should be enough
  private static int get_ts() {
      return (int)(get_raw_ts() - g_startTs);
  }

  // convert timestamp to ms
  private static double conv_ms(int ts) {
    return ts / 10000.0;
  }

  private static double correctTime(double tMeas, double tMeas2, int measCount) {
    int tsPrecCnt = g_tsPrecCnt;

    if (measCount < tsPrecCnt) {
      tMeas += g_tsPrecMs * ((tsPrecCnt - measCount) / tsPrecCnt); // ts + correction
      if (tMeas > tMeas2) {
        tMeas = tMeas2; // cannot correct
      }
    }
    return tMeas;
  }

  private static double getPrecMs(bool stopFlg) {
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
  private static void determineTsPrecision() {
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
  private static int checkbits_int1() {
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

  private static int checkbits_double1() {
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

  private static string getruntime1() {
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

  private static string get_info() {
    string version1 = "";

    try {
      version1 = System.Reflection.Assembly.GetExecutingAssembly().GetName().Version.ToString();
    } catch (System.Security.SecurityException) {
    }

    string cs_version = "Runtime: " + getruntime1() + ", " + version1 + ", " + Environment.OSVersion.ToString();

    string str = "BM Bench v" + g_prg_version + " (" + g_prg_language + ") -- (int:" + checkbits_int1() + " double:" + checkbits_double1() + " tsMs:" + g_tsPrecMs.ToString("", nfi) + " tsCnt:" + g_tsPrecCnt + ") " + cs_version + Environment.NewLine
      + "(c) Marco Vieth, 2006-2023" + Environment.NewLine
      + "Date: " + System.DateTime.Now.ToString("yyyy-MM-dd HH:mm:ssZ", System.Globalization.CultureInfo.InvariantCulture);
    return str;
  }


  private static void print_results(int bench1, int bench2, double[] bench_res1) {
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


  private static double measureBench(int bench, int n, int check) {
    const int delta_ms = 100;
    const int max_ms = 10000;
    int cali_ms = g_cali_ms;
    int loops = 1; // number of loops
    int x = 0;     // result from benchmark
    double tMeas = 0;   // measured time
    double tEsti = 0;   // estimated time
    double throughput = 0;

    Console.WriteLine("Calibrating benchmark {0} with n={1}, check={2}", bench, n, check);
    while (throughput == 0) {
      tMeas = getPrecMs(false);
      x = run_bench(bench, loops, n, check);
      tMeas = getPrecMs(true) - tMeas;

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


  private static int start_bench(int bench1, int bench2, int n) {
    Console.WriteLine(get_info());

    double[] bench_res = new double[bench2 + 1]; //reserve for up to last benchmark

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
    return 0;
  }

  
  public static int Main(string[] args) {
    int bench1 = 0;          // first benchmark to test
    int bench2 = 5;          // last benchmark to test
    int n = 1000000;         // maximum number

    //Console.WriteLine("Stdout is tty: {0}", Console.IsInputRedirected);

    if (args != null) { // not for e.g. dotnetfiddle.net
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
      if (args.Length > 3) {
        g_cali_ms = int.Parse(args[3]);
      }
    }

    determineTsPrecision();
    int rc = start_bench(bench1, bench2, n);
    
    Console.WriteLine("Total elapsed time: " + (int)(conv_ms(get_ts())) + " ms");
    //System.Console.ReadLine();
    return rc;
  }

}

// https://dotnetfiddle.net/
//
// https://www.programiz.com/csharp-programming/online-compiler/ (several languages, runs slow but full benchmark)
//
// https://onecompiler.com/csharp  (several languages, without login no URLs allowed)
//
// https://rextester.com/l/csharp_online_compiler
//
// https://replit.com/languages/
//
// https://wandbox.org/

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
