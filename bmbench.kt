//
// BM Bench - bmbench.kt (Kotlin)
// (c) Marco Vieth, 2017
// http://www.benchmarko.de
//
// 06.06.2017 0.06  first tests
// 04.05.2023 0.08  adapted for new version (translated Java->Kotlin with Android Studio Flamingo 2022.2.1)
//
// Compile:
// kotlinc bmbench.kt -include-runtime -d bmbenchkt.jar
//
// Run:
// java -jar bmbenchkt.jar [bench1] [bench2] [n]
//

import java.text.SimpleDateFormat
import java.util.Date

    var prg_version = "0.08"
    var prg_language = "Kotlin"
    private var gState_startTs: Long = 0
    private var gState_tsPrecMs = 0.0 // measured time stamp precision
    private var gState_tsPrecCnt =
        0 // time stamp count (calls) per precision interval (until time change)
    private var gState_tsMeasCnt = 0 // last measured count
    private var g_cali_ms = 1001 //
    private const val maxBench = 6

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
    private fun bench00(n: Int): Int {
        var x: Short = 0 // short is -32767..32768
        val n_div_65536 = (n shr 16).toShort()
        val n_mod_65536 = (n and 0xffff).toShort()
        // System.out.println("DEBUG: ndiv="+ n_div_65536 +", nmod="+ n_mod_65536);
        for (i in n_div_65536 downTo 1) {
            for (j in 32767 downTo 1) {
                x = (x + j).toShort()
            }
            for (j in -32768..-1) {
                x = (x + j).toShort()
            }
            // System.out.println("DEBUG: x="+ x);
        }
        for (j in n_mod_65536 downTo 1) {
            x = (x + j).toShort()
        }
        return x.toInt() and 0xffff
    }

    //
    // bench01 (Integer 16/32 bit)
    // (arithmetic mean of 1..n)
    //
    private fun bench01(n: Int): Int {
        var x = 0
        var sum = 0
        for (i in 1..n) {
            sum += i
            if (sum >= n) { // to avoid numbers above 2*n, divide by n using subtraction
                sum -= n
                x++
            }
        }
        return x
    }

    //
    // bench02 (Floating Point, normally 64 bit)
    // (arithmetic mean of 1..n)
    //
    private fun bench02(n: Int): Int {
        var x = 0
        var sum = 0.0
        for (i in 1..n) {
            sum += i.toDouble()
            if (sum >= n) {
                sum -= n.toDouble()
                x++
            }
        }
        return x
    }

    private var bench03Sieve1: BooleanArray? = null

    //
    // bench03 (Integer)
    // number of primes less than or equal to n (prime-counting function)
    // Example: n=500000 => x=41538 (expected), n=1000000 => x=78498
    // (Sieve of Eratosthenes, no multiples of 2 are stored)
    //
    private fun bench03(n: Int): Int {
        val nHalf = n shr 1

        // allocate memory...
        if (bench03Sieve1 == null) {
            bench03Sieve1 = BooleanArray(nHalf + 1)
        }
        val sieve1 = bench03Sieve1
        var i: Int
        // initialize sieve
        i = 0
        while (i <= nHalf) {
            sieve1!![i] = false
            i++
        }

        // compute primes
        i = 0
        var m = 3
        var x = 1 // number of primes below n (2 is prime)
        while (m * m <= n) {
            if (!sieve1!![i]) {
                x++ // m is prime
                var j = m * m - 3 shr 1 // div 2
                while (j < nHalf) {
                    sieve1[j] = true
                    j += m
                }
            }
            i++
            m += 2
        }

        // count remaining primes
        while (m <= n) {
            if (!sieve1!![i]) {
                x++ // m is prime
            }
            i++
            m += 2
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
    private fun bench04(n: Int): Int {
        val m = 2147483647 // modulus, do not change!
        val a = 16807 // multiplier
        val q = 127773 // m div a
        val r = 2836 // m mod a
        var x = 1 // 1=last random value
        for (i in n downTo 1) {
            val xDivQ = x / q
            val xModQ = x - q * xDivQ
            x = a * xModQ - r * xDivQ
            //x = a * (x % q) - r * (x / q); // x div q
            if (x <= 0) {
                x += m // x is new random number
            }
        }
        return x
    }

    private var bench05Line1: IntArray? = null

    //
    // bench05 (Integer 32 bit)
    // (n choose n/2) mod 65536 (Central Binomial Coefficient mod 65536)
    // Using dynamic programming and Pascal's triangle, storing only one line
    // Instead of nCk mod 65536 with k=n/2, we compute the product of (n/2)Ck mod 65536 with k=0..n/4 (Vandermonde folding)
    // Example: (2000 choose 1000) mod 65536 = 27200
    //
    private fun bench05(n: Int): Int {
        // Instead of nCk with k=n/2, we compute the product of (n/2)Ck with k=0..n/4
        var n = n
        n /= 2
        var k = n / 2
        if (n - k < k) {
            k = n - k // keep k minimal with  n over k  =  n over n-k
        }

        // allocate memory...
        if (bench05Line1 == null) {
            bench05Line1 = IntArray(k + 1)
        }
        val line = bench05Line1

        // initialize (not needed)
        for (j in 0..k) {
            line!![j] = 0
        }
        line!![0] = 1
        line[1] = 2 // for line 2, second column is 2

        // compute lines of Pascal's triangle
        for (i in 3..n) {
            val min1 = (i - 1) / 2
            if (i and 1 == 0) { // new element?
                line[min1 + 1] = 2 * line[min1]
            }
            var prev = line[1]
            for (j in 2..min1) {
                val num = line[j]
                line[j] += prev
                prev = num
            }
            line[1] = i // second column is i
        }

        // compute sum of ((n/2)Ck)^2 mod 65536 for k=0..n/2
        var x = 0
        for (j in 0 until k) {
            x += 2 * line[j] * line[j] /* add nCk and nC(n-k) */
        }
        x += line[k] * line[k] /* we assume that k is even, so we need to take the middle element */
        return x and 0xffff
    }

    private fun bench06(n: Int): Int {
        var sum = 0.0
        var flip = -1.0
        for (i in 1..n) {
            flip *= -1.0
            sum += flip / (2 * i - 1)
        }
        return (sum * 4.0 * 100000000).toInt()
    }

    //
    // run a benchmark
    // in: bench = benchmark to use
    //     loops = number of loops
    //         n = maximum number (used in some benchmarks to define size of workload)
    // out:    x = result
    //
    fun run_bench(bench: Int, loops: Int, n: Int, check: Int): Int {
        var loops = loops
        if (bench > maxBench) {
            println("Error: Unknown benchmark $bench")
        }
        var x = 0
        while (loops-- > 0 && x == 0) {
            when (bench) {
                0 -> x = bench00(n)
                1 -> x = bench01(n)
                2 -> x = bench02(n)
                3 -> x = bench03(n)
                4 -> x = bench04(n)
                5 -> x = bench05(n)
                6 -> x = bench06(n)
                else -> println("Error: Unknown benchmark $bench")
            }
            x -= check
        }
        x += check
        if (x != check) {
            println("Error(bench$bench): x=$x")
            x = -1 // exit
        }
        return x
    }

    fun bench03Check(n: Int): Int {
        var x: Int
        if (n == 500000) {
            x = 41538
        } else {
            x = 1 // 2 is prime
            var j = 3
            while (j <= n) {
                var isPrime = true
                var i = 3
                while (i * i <= j) {
                    if (j % i == 0) {
                        isPrime = false
                        break
                    }
                    i += 2
                }
                if (isPrime) {
                    x++
                }
                j += 2
            }
        }
        return x
    }

    fun getCheck(bench: Int, n: Int): Int {
        val check: Int
        check = when (bench) {
            0 -> (n + (n and 1) shr 1) * (n + 1 - (n and 1)) and 0xffff // 10528 for n=1000000
            1 -> (n + 1) / 2
            2 -> (n + 1) / 2
            3 -> bench03Check(n)
            4 -> if (n == 1000000) 1227283347 else bench04(n) // bench04 not a real check
            5 -> if (n == 5000) 17376 else bench05(n) // bench05 not a real check
            6 -> if (n == 1000000) 314159165 else bench06(n) // bench06 not a real check
            else -> {
                println("Error: Unknown benchmark $bench")
                -1
            }
        }
        return check
    }

    //
    // get timestamp in milliseconds
    // out: x = time in ms
    //
    // Even if the function is intended for short measurements we should
    // return a long to avoid overflows...
    //
    // get timestamp with full precision
    private fun get_raw_ts(): Long {
        return System.currentTimeMillis()
        //return (System.DateTime.Now.Ticks / 10) * 10; // Test: simulate 10 ms resolution
    }

    private fun get_ts(): Int {
        return (get_raw_ts() - gState_startTs).toInt()
    }

    fun conv_ms(ts: Long): Double {
        return ts.toDouble()
    }

    private fun correctTime(tMeas: Double, tMeas2: Double, measCount: Int): Double {
        var tMeas = tMeas
        val tsPrecCnt = gState_tsPrecCnt

        //System.out.println("DEBUG: correctTime1: tMeas=" + tMeas + ", tMeas2=" + tMeas2 + ", measCount=" + measCount + ", tsPrecCnt=" + tsPrecCnt);
        if (measCount < tsPrecCnt) {
            tMeas += gState_tsPrecMs * ((tsPrecCnt - measCount) / tsPrecCnt.toDouble()) // ts + correction
            //System.out.println("DEBUG: correctTime2: tMeas=" + tMeas + ", tMeas2=" + tMeas2 +", measCount=" + measCount);
            if (tMeas > tMeas2) {
                tMeas = tMeas2 // cannot correct
                //System.out.println("DEBUG: correctTime3: cannot correct, using tMeas=" + tMeas);
            }
        }
        return tMeas
    }

    private fun getPrecMs(stopFlg: Boolean): Double {
        var measCount = 0
        val tMeas0 = get_ts()
        var tMeas = tMeas0
        while (tMeas <= tMeas0) {
            tMeas = get_ts()
            measCount++
        }
        gState_tsMeasCnt = measCount // memorize count
        return if (!stopFlg) conv_ms(tMeas.toLong()) else correctTime(
            conv_ms(tMeas0.toLong()),
            conv_ms(tMeas.toLong()),
            measCount
        )
    }

    // usually only needed if time precision is low, e.g. one second
    private fun determineTsPrecision() {
        gState_startTs = get_raw_ts() // memorize start time
        var tMeas0 = getPrecMs(false)
        var tMeas1 = getPrecMs(false)
        gState_tsPrecMs = tMeas1 - tMeas0
        gState_tsPrecCnt = gState_tsMeasCnt

        // do it again
        tMeas0 = tMeas1
        tMeas1 = getPrecMs(false)
        if (gState_tsMeasCnt > gState_tsPrecCnt) { // taker maximum count
            gState_tsPrecCnt = gState_tsMeasCnt
            gState_tsPrecMs = tMeas1 - tMeas0
        }
    }

    // --------------------------------------------------------
    // in Java the sizes of the types is specified (int: 32, double: 64)
    // Here we compute the number of "significant" bits for positive numbers (which means 53 for double)
    private fun checkbits_short1(): Int {
        var num: Short = 1
        var last_num: Short = 0
        var bits = 0
        do {
            last_num = num
            num = (num * 2).toShort()
            num++
            bits++
            //System.out.println("DEBUG: bits="+ bits +", num="+ num);
        } while ((num - 1) / 2 == last_num.toInt() && bits < 101)
        return bits
    }

    private fun checkbits_int1(): Int {
        var num = 1
        var last_num = 0
        var bits = 0
        do {
            last_num = num
            num *= 2
            num++
            bits++
            //System.out.println("DEBUG: bits="+ bits +", num="+ num);
        } while ((num - 1) / 2 == last_num && bits < 101)
        return bits
    }

    private fun checkbits_long1(): Int {
        var num: Long = 1
        var last_num: Long = 0
        var bits = 0
        do {
            last_num = num
            num *= 2
            num++
            bits++
            //System.out.println("DEBUG: bits="+ bits +", num="+ num);
        } while ((num - 1) / 2 == last_num && bits < 101)
        return bits
    }

    private fun checkbits_float1(): Int {
        var num = 1.0f
        var last_num = 0.0f
        var bits = 0
        do {
            last_num = num
            num *= 2.0f
            num++
            bits++
            //System.out.println("DEBUG: bits="+ bits +", num="+ num);
        } while ((num - 1.0f) / 2.0f == last_num && bits < 101)
        return bits
    }

    private fun checkbits_double1(): Int {
        var num = 1.0
        var last_num = 0.0
        var bits = 0
        do {
            last_num = num
            num *= 2.0
            num++
            bits++
            //System.out.println("DEBUG: bits="+ bits +", num="+ num);
        } while ((num - 1.0) / 2.0 == last_num && bits < 101)
        return bits
    }

    // --------------------------------------------------------
    // or use NumberFormat?
    private fun mynumformat1_i(`val`: Int, digits: Int): String {
        val str = StringBuffer() // buffer for one formatted value
        str.append(`val`)
        for (i in str.length until digits) {
            str.insert(0, ' ')
        }
        return str.toString()
    }

    private fun mynumformat1_d(`val`: Double, digits: Int, prec: Int): String {
        val str = StringBuffer() // buffer for one formatted value
        val displ_prec_after = Math.pow(10.0, 3.0) // display precision after decimal point
        str.append(Math.round(`val` * displ_prec_after) / (displ_prec_after * 1.0))
        if (str.toString().indexOf('.') < 0) { // should not occur
            println("WARNING: str does not contain a dot: $str")
            str.append('.')
        }

        // format to prec digits after comma
        while (str.length <= prec || str[str.length - (prec + 1)] != '.') {
            str.append("0")
        }
        for (i in str.length until digits) {
            str.insert(0, ' ')
        }
        return str.toString()
    }

    private fun get_info(): String {

        //System.out.println("properties="+ System.getProperties());
        return ("BM Bench v" + prg_version + " (" + prg_language + ") -- (short:" + checkbits_short1() + " int:" + checkbits_int1()
                + " long:" + checkbits_long1() + " float:" + checkbits_float1() + " double:" + checkbits_double1() + " tsMs:" + gState_tsPrecMs + " tsCnt:" + gState_tsPrecCnt + ")"
                + " java.version=" + System.getProperty("java.version") + ", java.vendor=" + System.getProperty(
            "java.vendor"
        ) + " "
                + ", os.name=" + System.getProperty("os.name") + ", os.arch=" + System.getProperty("os.arch")
                + ", os.version=" + System.getProperty("os.version") + "\n"
                + "(c) Marco Vieth, 2002-2022\n"
                + SimpleDateFormat("yyyy-MM-dd HH:mm:ss").format(Date(System.currentTimeMillis())))
    }

    private fun print_results(bench1: Int, bench2: Int, bench_res1: DoubleArray) {
        val max_language_len1 = 10
        println("\nThroughput for all benchmarks (loops per sec):")
        var str = "BMR (" + prg_language + ")"
        for (i in prg_language.length until max_language_len1) {
            str += " "
        }
        str += ": "
        for (bench in bench1..bench2) {
            str += mynumformat1_d(bench_res1[bench], 9, 3) + ' '
        }
        println(str)
        println("")
    }

    private fun measureBench(bench: Int, n: Int, check: Int): Double {
        val delta_ms = 100
        val max_ms = 10000
        val cali_ms = g_cali_ms
        var loops = 1 // number of loops
        var x: Int // result from benchmark
        var tMeas = 0.0 // measured time
        var tEsti = 0.0 // estimated time
        var throughput = 0.0
        println("Calibrating benchmark $bench with n=$n, check=$check")
        while (throughput == 0.0) {
            tMeas = getPrecMs(false)
            x = run_bench(bench, loops, n, check)
            tMeas = getPrecMs(true) - tMeas
            val t_delta =
                if (tEsti > tMeas) tEsti - tMeas else tMeas - tEsti // compute difference abs(measures-estimated)
            val loops_p_sec: Double = if (tMeas > 0.0) loops * 1000.0 / tMeas else 0.0
            println(
                mynumformat1_d(loops_p_sec, 10, 3) + "/s (time=" + mynumformat1_d(
                    tMeas,
                    9,
                    3
                ) + " ms, loops=" + mynumformat1_i(loops, 7) + ", delta=" + mynumformat1_d(
                    t_delta,
                    9,
                    3
                ) + " ms)"
            )
            if (x == -1) { // some error?
                throughput = -1.0
            } else if (tEsti > 0 && t_delta < delta_ms) { // do we have some estimated/expected time smaller than delta_ms=100?
                throughput = loops_p_sec // yeah, set measured loops per sec
                println(
                    "Benchmark " + bench + " (" + prg_language + "): " + mynumformat1_d(
                        loops_p_sec,
                        0,
                        3
                    ) + "/s (time=" + mynumformat1_d(
                        tMeas,
                        9,
                        3
                    ) + " ms, loops=" + loops + ", delta=" + mynumformat1_d(t_delta, 9, 3) + " ms)"
                )
            } else if (tMeas > max_ms) {
                println("Benchmark " + bench + " (" + prg_language + "): Time already > " + max_ms + " ms. No measurement possible.")
                throughput =
                    if (loops_p_sec > 0) -loops_p_sec else -1.0 // cannot rely on measurement, so set to negative
            } else {
                var scale_fact: Int
                scale_fact = if (tMeas == 0.0) {
                    50
                } else if (tMeas < cali_ms) {
                    ((cali_ms + 100) / tMeas).toInt() + 1 // scale a bit up to 1100 ms (cali_ms+100)
                } else {
                    2
                }
                // scale a bit up to 1100 ms (cali_ms+100)
                loops *= scale_fact
                tEsti = tMeas * scale_fact
            }
        }
        return throughput
    }

    private fun start_bench(bench1: Int, bench2: Int, n: Int, argStr: String): Int {
        determineTsPrecision()
        println(get_info())
        if (argStr.length > 0) {
            println("Args:$argStr")
        }
        val bench_res = DoubleArray(bench2 + 1)
        for (bench in bench1..bench2) {
            var n2 = n
            if (bench == 3) {
                n2 = n2 / 2
            } else if (bench == 5) {
                n2 = n2 / 200
            }
            val check = getCheck(bench, n2)
            val throughput: Double = if (check > 0) measureBench(bench, n2, check) else -1.0
            bench_res[bench] = throughput
        }
        print_results(bench1, bench2, bench_res)
        return 1
    }

    var default_bench1 = 0 // first benchmark to test
    var default_bench2 = 5 // last benchmark to test
    var default_n = 1000000 // maximum number

    //
    // if you call it with java...
    //
    fun main(args: Array<String>) {
        var argStr = ""
        if (args.size > 0) {
            default_bench1 = args[0].toInt()
            default_bench2 = default_bench1
        }
        if (args.size > 1) {
            default_bench2 = args[1].toInt()
        }
        if (args.size > 2) {
            default_n = args[2].toInt()
        }
        if (args.size > 3) {
            g_cali_ms = args[3].toInt()
        }
        for (s in args) {
            argStr += " $s"
        }
        var rc = start_bench(default_bench1, default_bench2, default_n, argStr)
        rc = rc //avoid warning
        println("Total elapsed time: " + conv_ms(get_ts().toLong()).toInt() + " ms")
    }

/*
    class bmbench {
        @Test
        fun mytest() {
            main(arrayOf("0", "5", "1000000", "200"))
        }
    }
*/
// end
