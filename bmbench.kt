//
// BM Bench - bmbench.kt (Kotlin)
// (c) Marco Vieth, 2017
// http://www.benchmarko.de
//
// 06.06.2017 0.06  first tests
//
// Compile:
// kotlinc bmbench.kt -include-runtime -d bmbenchkt.jar
//
// Run:
// java -jar bmbenchkt.jar [bench1] [bench2] [n]
//


//package runs
//test in: https://try.kotlinlang.org/#/Examples/Problems/Runs/Runs.kt
//BM 0: loops/s: 894.01 total: 1528 ms
//BM 1: loops/s: 2527.40 total: 1123 ms (sometimes no response)
//BM 2: loops/s: 839.79 total: 1574 ms
//BM 3: loops/s: 478.63 total: 3017 ms
//BM 4: loops/s: 147.99 total: 3768 ms
//BM 5: loops/s: ? total: ? ms (Server didn't respond for 10 seconds.)
//
import java.text.SimpleDateFormat // just to print date

//fun runs(a: IntArray):Int {
//    if (a.size == 0) {
//      val bm1 = "5"
//      val bm2 = bm1
//      val args:Array<String> = arrayOf(bm1, bm2)
//      main(args)
//      return 0
//  }
//  var runs = 1
//  var last = a[0]
//  for (x in a) {
//    if (x != last) {
//      last = x
//      runs++
//    }
//  }
//  return runs
//}
//


//class bmbench {
    val prg_version = "0.06"
    val prg_language = "Kotlin"
    //
    // General description for benchmark test functions
    // benchxx - benchmark
    // <description>
    // in: loops = number of loops
    // n = maximum number (assumed even, normally n=1000000)
    // out: x = <output decription>
    //
    // loops may be increased to produce a longer runtime without changing the result.
    //
    //
    // bench00 (Integer 16 bit)
    // (sum of 1..n) mod 65536
    //
    fun bench00(loops_par:Int, n:Int):Int {
      var loops = loops_par
      var x:Short = 0 // short is -32767..32768
      val sum1 = ((n / 2) * (n + 1)).toShort() // assuming n even!
      // (sum1..1000000 depends on type: 500000500000 (floating point), 1784293664 (32bit), 10528 (16 bit)
      val n_div_65536 = (n shr 16).toShort()
      val n_mod_65536 = (n and 0xffff).toShort()
      //System.out.println("DEBUG: sum1="+ sum1 +", ndiv="+ n_div_65536 +", nmod="+ n_mod_65536);
      while (loops-- > 0)
      {
        for (i in n_div_65536 downTo 1)
        {
          for (j in 32767 downTo 1)
          {
            x = (x + j).toShort()
          }
          for (j in -32768..-1)
          {
            x = (x + j).toShort()
          }
        }
        for (j in n_mod_65536 downTo 1)
        {
          x = (x + j).toShort()
        }
        if (loops > 0)
        { // some more loops left?
          x = (x - sum1).toShort() // yes, set x back to 0 (assuming n even)
          if (x.toInt() != 0)
          { // now x must be 0 again
            x++ // force error for many wrong computations
            break // error
          }
        }
      }
      return x.toInt() and 0xffff
    }
    //
    // bench01 (Integer 16/32 bit)
    // (sum of 1..n) mod 65536
    //
    private fun bench01(loops_par:Int, n:Int):Int {
      var loops = loops_par
      var x = 0
      val sum1 = (n / 2) * (n + 1) // assuming n even! (32 bit sum should be 1784293664)
      while (loops-- > 0)
      {
        for (i in n downTo 1)
        {
          x += i
        }
        if (loops > 0)
        { // some more loops left?
          x -= sum1 // yes, set x back to 0
          if (x != 0)
          { // now x must be 0 again
            x++ // force error for many wrong computations
            return x // Error
          }
        }
      }
      return (x and 0xffff) // % 65536
    }
    //
    // bench02 (Floating Point, normally 64 bit)
    // (sum of 1..n) mod 65536
    //
    private fun bench02(loops_par:Int, n:Int):Int {
      var loops = loops_par
      var x:Double = 0.0
      val sum1:Double = (n.toDouble() / 2.0) * (n + 1.0) // assuming n even! (sum should be 5.000005E11)
      while (loops-- > 0)
      {
        for (i in n downTo 1)
        {
          x += i.toDouble()
        }
        if (loops > 0)
        { // some more loops left?
          x -= sum1 // yes, set x back to 0
          if (x != 0.0)
          { // now x must be 0 again
            x++
            break // Error
          }
        }
      }
      return (x % 65536).toInt()
    }
    //
    // bench03 (Integer)
    // number of primes below n (Sieve of Eratosthenes)
    // Example: n=500000 => x=41538 (expected), n=1000000 => x=78498
    //
    private fun bench03(loops_par:Int, n_par:Int):Int {
      var loops = loops_par
      var n = n_par / 2 // compute only up to n/2
      var x = 0 // number of primes below n
      val sieve1 = BooleanArray(n + 1)
      sieve1[0] = false
      sieve1[1] = false
      while (loops-- > 0)
      {
        // initialize sieve
        for (i in 2..n)
        {
          sieve1[i] = true
        }
        // compute primes
        run({ var i = 2;
             while ((i * i) <= n)
             {
               if (sieve1[i])
               {
                 var j = i * i;
                 while (j <= n)
                 {
                   sieve1[j] = false
                   j += i
                 }
               }
               i++
             } })
        // count primes
        for (i in 0..n)
        {
          if (sieve1[i])
          {
            x++
          }
        }
        // check prime count
        if (loops > 0)
        { // some more loops left?
          x -= 41538 // yes, set x back to 0 (number of primes below 1000000)
          if (x != 0)
          { // now x must be 0 again
            x++
            break // Error
          }
        }
      }
      return x
    }
    //
    // bench04 (Integer 32 bit)
    // nth random number number
    // Random number generator taken from
    // Raj Jain: The Art of Computer Systems Performance Analysis, John Wiley & Sons, 1991, page 442-444.
    // It needs longs with at least 32 bit.
    // Starting with x0=1, x10000 should be 1043618065, x1000000 = 1227283347.
    //
    fun bench04(loops_par:Int, n:Int):Int {
      var loops = loops_par
      val m = 2147483647 // modulus, do not change!
      val a = 16807 // multiplier
      val q = 127773 // m div a
      val r = 2836 // m mod a
      var x = 1 // last random value
      while (loops-- > 0)
      {
        for (i in n downTo 1)
        {
          val x_div_q = x / q
          val x_mod_q = x - q * x_div_q
          x = a * x_mod_q - r * x_div_q
          if (x <= 0)
          {
            x += m // x is new random number
          }
        }
        if (loops > 0)
        {
          x -= 1227283347
          if (x != 0)
          { // now x must be 0 again
            x++
            break // Error
          }
          x++ // start with 1 again
        }
      }
      return x
    }
    //
    // bench05 (Integer 32 bit)
    // n over n/2 mod 65536 (Pascal's triangle)
    //
    private fun bench05(loops_par:Int, n_p:Int):Int {
      var loops = loops_par
      var x = 0
      val n = n_p / 500
      var k = n / 2
      if ((n - k) < k)
      {
        k = n - k // keep k minimal with n over k = n over n-k
      }
      // allocate memory...
      val pas1 = Array<IntArray>(2, {IntArray(k + 1)})
      pas1[0][0] = 1
      pas1[1][0] = 1 // set first column
      while (loops-- > 0)
      {
        for (i in 2..n)
        {
          val i_mod_2 = i % 2
          var min1 = (i - 1) / 2
          if (k < min1)
          {
            min1 = k
          }
          pas1[i_mod_2][1] = i // second column is i
          for (j in 2..min1)
          { // up to min((i-1)/2, k)
            pas1[i_mod_2][j] = (pas1[i_mod_2 xor 1][j - 1] + pas1[i_mod_2 xor 1][j])
          }
          if ((min1 < k) && (i_mod_2 == 0))
          { // new element
            pas1[i_mod_2][min1 + 1] = 2 * pas1[i_mod_2 xor 1][min1]
          }
        }
        x += pas1[n % 2][k] and 0xffff // % 65536
        if (loops > 0)
        {
          x -= 27200
          if (x != 0)
          { // now x must be 0 again
            x++
            break // Error
          }
        }
      }
      return x
    }
    //
    // run a benchmark
    // in: bench = benchmark to use
    // loops = number of loops
    // n = maximum number (used in some benchmarks to define size of workload)
    // out: x = result
    //
    fun run_bench(bench:Int, loops:Int, n:Int):Int {
      var x = 0
      var check1 = 0
      when (bench) {
        0 -> {
          x = bench00(loops, n)
          check1 = 10528
        }
        1 -> {
          x = bench01(loops, n)
          check1 = 10528
        }
        2 -> {
          x = bench02(loops, n)
          check1 = 10528
        }
        3 -> {
          x = bench03(loops, n)
          check1 = 41538
        }
        4 -> {
          x = bench04(loops, n)
          check1 = 1227283347
        }
        5 -> {
          x = bench05(loops, n)
          check1 = 27200
        }
        else -> {
          println("Error: Unknown benchmark " + bench)
          check1 = -1 // force error
        }
      }
      if (check1 != x)
      {
        println("Error(bench" + bench + "): x=" + x)
        x = -1 //exit;
      }
      return (x)
    }
    //
    // get timestamp in milliseconds
    // out: x = time in ms
    //
    // Even if the function is intended for short measurements we should
    // return a long to avoid overflows...
    //
    private val _ms:Long
    get() {
      return (System.currentTimeMillis())
    }
    private fun getdate1():String {
      val date1 = _ms
      val format1 = SimpleDateFormat("dd.MM.yyyy HH:mm:ss")
      return format1.format(java.util.Date(date1))
    }
    // --------------------------------------------------------
    // in Java the sizes of the types is specified (int: 32, double: 64)
    // Here we compute the number of "significant" bits for positive numbers (which means 53 for double)
    private fun checkbits_short1():Int {
      var num:Short = 1
      var last_num:Short = 0
      var bits = 0
      do
      {
        last_num = num
        num = (num * 2).toShort()
        num++
        bits++
        //System.out.println("DEBUG: bits="+ bits +", num="+ num);
      }
      while ((((num - 1) / 2) == last_num.toInt()) && (bits < 101))
      return bits
    }
    private fun checkbits_int1():Int {
      var num = 1
      var last_num = 0
      var bits = 0
      do
      {
        last_num = num
        num *= 2
        num++
        bits++
        //System.out.println("DEBUG: bits="+ bits +", num="+ num);
      }
      while ((((num - 1) / 2) == last_num) && (bits < 101))
      return bits
    }
    private fun checkbits_long1():Int {
      var num:Long = 1
      var last_num:Long = 0
      var bits = 0
      do
      {
        last_num = num
        num *= 2
        num++
        bits++
        //System.out.println("DEBUG: bits="+ bits +", num="+ num);
      }
      while ((((num - 1) / 2) == last_num) && (bits < 101))
      return bits
    }
    private fun checkbits_float1():Int {
      var num = 1.0f
      var last_num = 0.0f
      var bits = 0
      do
      {
        last_num = num
        num *= 2.0f
        num++
        bits++
        //System.out.println("DEBUG: bits="+ bits +", num="+ num);
      }
      while ((((num - 1.0f) / 2.0f) == last_num) && (bits < 101))
      return bits
    }
    private fun checkbits_double1():Int {
      var num = 1.0
      var last_num = 0.0
      var bits = 0
      do
      {
        last_num = num
        num *= 2.0
        num++
        bits++
        //System.out.println("DEBUG: bits="+ bits +", num="+ num);
      }
      while ((((num - 1.0) / 2.0) == last_num) && (bits < 101))
      return bits
    }
    // --------------------------------------------------------
    // or use NumberFormat?
    private fun mynumformat1_i(`val`:Int, digits:Int):String {
      val str = StringBuffer() // buffer for one formatted value
      str.append(`val`)
      for (i in str.length..digits - 1)
      {
        str.insert(0, ' ')
      }
      return (str.toString())
    }
    private fun mynumformat1_d(`val`:Double, digits:Int, prec:Int):String {
      val str = StringBuffer() // buffer for one formatted value
      val displ_prec_after = Math.pow(10.0, 2.0) // display precision after decimal point
      str.append(Math.round(`val` * displ_prec_after) / (displ_prec_after * 1.0))
      if (str.toString().indexOf('.') < 0)
      { // should not occur
        println("WARNING: str does not contain a dot: " + str)
        str.append('.')
      }
      // format to prec digits after comma
      while ((str.length <= prec) || (str.get(str.length - (prec + 1)) != '.'))
      {
        //int len1 = str.length();
        //int pos1 = str.length() - (prec - 1);
        str.append("0")
      }
      for (i in str.length..digits - 1)
      {
        str.insert(0, ' ')
      }
      return (str.toString())
    }
    private fun print_info() {
      print("BM Bench v" + prg_version + " (" + prg_language + ") -- (short:" + checkbits_short1() + " int:" + checkbits_int1()
            + " long:" + checkbits_long1() + " float:" + checkbits_float1() + " double:" + checkbits_double1() + ") ")
      print("java.version=" + System.getProperty("java.version") + ", java.vendor=" + System.getProperty("java.vendor"))
      println("os.name=" + System.getProperty("os.name") + ", os.arch=" + System.getProperty("os.arch")
              + ", os.version=" + System.getProperty("os.version"))
      println("(c) Marco Vieth, 2006")
      println("Date: " + getdate1())
      //System.out.println("properties="+ System.getProperties());
    }
    private fun print_results(bench1:Int, bench2:Int, bench_res1:DoubleArray) {
      val max_language_len1 = 10
      println("\nThroughput for all benchmarks (loops per sec):")
      val str = "BMR (" + prg_language + ")"
      println(str)
      for (i in prg_language.length..max_language_len1 - 1)
      {
        print(" "); //str += " "
      }
      print(": "); //str += ": "
      for (bench in bench1..bench2)
      {
        print("${mynumformat1_d(bench_res1[bench], 9, 2)}" + " ") //str += mynumformat1_d(bench_res1[bench], 9, 2) + ' '
      }
      //println(str)
      println("")
    }
    private fun start_bench(bench1:Int, bench2:Int, n:Int):Int {
      val cali_ms = 1001
      val delta_ms = 100
      val max_ms = 10000
      print_info()
      val bench_res1 = DoubleArray(bench2 + 1)
      for (bench in bench1..bench2)
      {
        var loops = 1 // number of loops
        var x = 0 // result from benchmark
        var t1:Long = 0 // measured time
        var t2:Long = 0 // estimated time
        println("Calibrating benchmark " + bench + " with n=" + n)
        while (true)
        {
          t1 = _ms
          x = run_bench(bench, loops, n)
          t1 = _ms - t1
          val t_delta = if ((t2 > t1)) (t2 - t1) else (t1 - t2) // compute difference abs(measures-estimated)
          val loops_p_sec:Double = if ((t1 > 0)) (loops * 1000.0 / t1) else 0.0
          println(mynumformat1_d(loops_p_sec, 10, 3) + "/s (time=" + mynumformat1_i(t1.toInt(), 5) + " ms, loops=" + mynumformat1_i(loops, 7) + ", delta=" + mynumformat1_i(t_delta.toInt(), 5) + " ms, x=" + x)
          if (x == -1)
          { // some error?
            bench_res1[bench] = -1.0
            break
          }
          if (t2 > 0)
          { // do we have some estimated/expected time?
            if (t_delta < delta_ms)
            { // smaller than delta_ms=100?
              bench_res1[bench] = loops_p_sec // set loops per sec
              println("Benchmark " + bench + " (" + prg_language + "): " + mynumformat1_d(bench_res1[bench], 0, 3) + "/s (time=" + t1 + " ms, loops=" + loops + ", delta=" + t_delta + " ms)")
              break
            }
          }
          if (t1 > max_ms)
          {
            println("Benchmark " + bench + " (" + prg_language + "): Time already > " + max_ms + " ms. No measurement possible.")
            bench_res1[bench] = -1.0
            break
          }
          run({ val scale_fact = if (((t1 < cali_ms) && (t1 > 0))) ((cali_ms + 100) / t1).toInt() + 1 else 2
               // scale a bit up to 1100 ms (cali_ms+100)
               loops *= scale_fact
               t2 = t1 * scale_fact })
        }
      }
      print_results(bench1, bench2, bench_res1)
      return 1
    }
    internal var default_bench1 = 0 // first benchmark to test
    internal var default_bench2 = 5 // last benchmark to test
    internal var default_n = 1000000 // maximum number
    //
    // if you call it with java...
    //
    fun main(args:Array<String>) {
      val start_t = _ms // memorize start time 
      if (args.size > 0)
      {
        default_bench1 = Integer.parseInt(args[0])
        default_bench2 = default_bench1
      }
      if (args.size > 1)
      {
        default_bench2 = Integer.parseInt(args[1])
      }
      if (args.size > 2)
      {
        default_n = Integer.parseInt(args[2])
      }
      /* val rc = */ start_bench(default_bench1, default_bench2, default_n)
      //rc = rc //avoid warning
      println("Total elapsed time: " + (_ms - start_t) + " ms")
    }
// end
