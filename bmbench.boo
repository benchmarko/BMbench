//
// BM Bench - bmbench.boo (Boo)
// (c) Marco Vieth, 2002-2007
// http://www.benchmarko.de
//
// 21.10.2007 0.06  first tests
//
// Compile:
//   booc bmbench.boo
// - or with Mono (1.2.5):
//   booc -checked- bmbench.boo    (booc -checked- -out:bmbench.exe bmbench.boo)
//   mono bmbench.exe  (on Windows; also without mono -> use MS .NET)
//
// Usage:
// bmbench [bench1] [bench2] [n]
//
//
// Converted from the C# version 0.06 by online Codeconverter C# -> Boo
// from http://codeconverter.sharpdevelop.net/Convert.aspx
//
//Boo errors after using Codeconverter:
//from http://codeconverter.sharpdevelop.net/Convert.aspx
//Error: Sorry but the 'get_' prefix is reserved by the compiler. Friends?
// Fix: get_ms => get1_ms
//Error: No entry point found.
// Fix: [STAThread] before Main?


import System



//
// bench00 (Integer 16 bit)
// (sum of 1..n) mod 65536
//
// assuming n even! (sum should be ...)
// some more loops left?
// (do not use &= 0xffff)
// yes, set x back to 0
// now x must be 0 again
// force error for many wrong computations
// Error   //alert("Error(bench01): x="+ x);

//
// bench01 (Integer 16/32 bit)
// (sum of 1..n) mod 65536
// assuming n even! (sum should be ...)
// some more loops left?
// (do not use &= 0xffff)
// yes, set x back to 0
// now x must be 0 again
// force error for many wrong computations
// Error   //alert("Error(bench01): x="+ x);


//
// bench02 (Floating Point, normally 64 bit)
// (sum of 1..n) mod 65536
//
// assuming n even! (sum should be 5.000005E11)
// some more loops left?
// yes, set x back to 0
// now x must be 0 again
// Error   //alert("Error(bench01): x="+ x);

//
// bench03 (Integer)
// number of primes below n (Sieve of Eratosthenes)
// Example: n=500000 => x=41538 (expected), n=1000000 => x=78498
// (No bit array available, just bool...)
// compute only up to n/2
// number of primes below n
//BitArray sieve1 = new BitArray(n + 1); // slower than bool
//BitArray from http://www.csharpfriends.com/Spec/index.aspx?specID=17.8.htm 
// initialize sieve
// compute primes
// count primes
// check prime count
// some more loops left?
// yes, set x back to 0 (number of primes below 1000000)
// now x must be 0 again
// Error


//
// bench04 (Integer 32 bit)
// nth random number number
// Random number generator taken from
// Raj Jain: The Art of Computer Systems Performance Analysis, John Wiley & Sons, 1991, page 442-444.
// It needs longs with at least 32 bit.
// Starting with x0=1, x10000 should be 1043618065, x1000000 = 1227283347.
//
// modulus, do not change!
// multiplier
// m div a
// m mod a
// last random value
// (int)
// x is new random number
// now x must be 0 again
// Error
// start with 1 again


//
// bench05 (Integer 32 bit)
// n over n/2 mod 65536 (Pascal's triangle)
//
// div 2
// keep k minimal with  n over k  =  n over n-k

// allocate memory...
// set first column
//int i1_mod_2 = i_mod_2 ^ 1;
// Math.floor((i - 1) / 2);
// second column is i
// up to min((i-1)/2, k)
// % 65536 -- we need mod here to avoid overflow
// new element
//pas1[i_mod_2][Math.floor(i / 2)] = 2 * pas1[i_mod_2 ^ 1][Math.floor((i - 1) / 2)];
// % 65536
// now x must be 0 again
// Error


//
// run a benchmark
// in: bench = benchmark to use
//     loops = number of loops
//         n = maximum number (used in some benchmarks to define size of workload)
// out:    x = result
//






//exit;

//
// get timestamp in milliseconds
// out: x = time in ms
//
// This function is intended for short measurements only.
//
// convert 100ns -> 1 ms 
//return (ticks / 10000000) * 1000; // TTT: simulate sec resolution

// Creates and initializes a DateTimeFormatInfo associated with the en-US culture.
// always German format

// 'System.Environment.Version' shows more info than 'System.Reflection.Assembly.GetExecutingAssembly().ImageRuntimeVersion'

//runtimeName = "Mono";
//call Mono.Runtime.GetDisplayName()...


// Here we compute the number of "significant" bits for positive numbers (which means 53 for double)










// we use this format to get a decimal point for every culture

//reserve for up to last benchmark
// number of loops
// result from benchmark
// measured time
// estimated time

// compute difference abs(measures-estimated)
// some error?
// do we have some estimated/expected time?
// smaller than delta_ms=100?
// set loops per sec

// scale a bit up to 1100 ms (cali_ms+100)





// memorize start time
// first benchmark to test
// last benchmark to test
// maximum number


//System.Console.ReadLine();
[module]
class Bmbench:

	private static prg_version = '0.06'

	private static prg_language = 'Boo'

	private static def bench00(loops as int, n as int) as int:
		x = 0
		sum1 = cast(int, (((n / 2) * (n + 1)) % 65536))
		n_div_65536 as int = (n >> 16)
		n_mod_65536 as int = (n & 65535)
		while (loops--) > 0:
			for i in range(n_div_65536, 0, -1):
				for j in range(32767, 0, -1):
					x += j
				for j in range(-32768, 0):
					x += j
			for j in range(n_mod_65536, 0, -1):
				x += j
			if loops > 0:
				x = (x % 65536)
				x -= sum1
				if x != 0:
					x += 1
					break 
		return (x % 65536)

	private static def bench01(loops as int, n as int) as int:
		x = 0
		sum1 = cast(int, (((n / 2) * (n + 1)) % 65536))
		while (loops--) > 0:
			for i in range(n, 0, -1):
				x += i
			if loops > 0:
				x = (x % 65536)
				x -= sum1
				if x != 0:
					x += 1
					break 
		return (x % 65536)

	private static def bench02(loops as int, n as int) as int:
		x = 0.0
		sum1 as double = ((n / 2.0) * (n + 1.0))
		while (loops--) > 0:
			for i in range(n, 0, -1):
				x += i
			if loops > 0:
				x -= sum1
				if x != 0.0:
					x += 1
					break 
		return cast(int, (x % 65536))

	private static def bench03(loops as int, n as int) as int:
		n >>= 1
		x = 0
		sieve1 as (bool) = array(bool, (n + 1))
		sieve1[0] = false
		sieve1[1] = false
		while (loops--) > 0:
			for i in range(2, (n + 1)):
				sieve1[i] = true
			i = 2
			goto converterGeneratedName1
			while true:
				i += 1
				:converterGeneratedName1
				break  unless ((i * i) <= n)
				if sieve1[i] != false:
					j as int = (i * i)
					goto converterGeneratedName2
					while true:
						j += i
						:converterGeneratedName2
						break  unless (j <= n)
						sieve1[j] = false
			for i in range(0, (n + 1)):
				if sieve1[i] != false:
					x += 1
			if loops > 0:
				x -= 41538
				if x != 0:
					x += 1
					break 
		return x

	private static def bench04(loops as int, n as int) as int:
		m = 2147483647
		a = 16807
		q = 127773
		r = 2836
		x = 1
		while (loops--) > 0:
			for i in range(n, 0, -1):
				x_div_q as int = (x / q)
				x_mod_q as int = (x - (q * x_div_q))
				x = ((a * x_mod_q) - (r * x_div_q))
				if x <= 0:
					x += m
			if loops > 0:
				x -= 1227283347
				if x != 0:
					x += 1
					break 
				x += 1
		return x

	private static def bench05(loops as int, n as int) as int:
		x = 0
		n = (n / 500)
		k as int = (n >> 1)
		if (n - k) < k:
			k = (n - k)
		pas1 as ((int)) = array(typeof((int)), 2)
		pas1[0] = array(int, (k + 1))
		pas1[1] = array(int, (k + 1))
		pas1[0][0] = 1
		pas1[1][0] = 1
		while (loops--) > 0:
			for i in range(3, (n + 1)):
				i_mod_2 as int = (i & 1)
				min1 as int = (((i - 2) >> 1) if (i_mod_2 == 0) else ((i - 1) >> 1))
				if k < min1:
					min1 = k
				pas1[i_mod_2][1] = i
				for j in range(2, (min1 + 1)):
					pas1[i_mod_2][j] = ((pas1[(i_mod_2 ^ 1)][(j - 1)] + pas1[(i_mod_2 ^ 1)][j]) & 65535)
				if (min1 < k) and (i_mod_2 == 0):
					pas1[i_mod_2][(min1 + 1)] = (2 * pas1[(i_mod_2 ^ 1)][min1])
			x += (pas1[(n & 1)][k] & 65535)
			if loops > 0:
				x -= 27200
				if x != 0:
					x += 1
					break 
		return x

	private static def run_bench(bench as int, loops as int, n as int) as int:
		x = 0
		check1 = 0
		converterGeneratedName3 = bench
		if converterGeneratedName3 == 0:
			x = bench00(loops, n)
			check1 = 10528
		else:
			if converterGeneratedName3 == 1:
				x = bench01(loops, n)
				check1 = 10528
			else:
				if converterGeneratedName3 == 2:
					x = bench02(loops, n)
					check1 = 10528
				else:
					if converterGeneratedName3 == 3:
						x = bench03(loops, n)
						check1 = 41538
					else:
						if converterGeneratedName3 == 4:
							x = bench04(loops, n)
							check1 = 1227283347
						else:
							if converterGeneratedName3 == 5:
								x = bench05(loops, n)
								check1 = 27200
							else:
								Console.Error.WriteLine(('Error: Unknown benchmark ' + bench))
								check1 = (x + 1)
		if check1 != x:
			Console.Error.WriteLine(((('Error(bench' + bench) + '): x=') + x))
			x = (-1)
		return x

	private static def get1_ms() as long:
		ticks as long = System.DateTime.Now.Ticks
		return (ticks / 10000)

	private static def getdate1() as string:
		dtfi as System.Globalization.DateTimeFormatInfo = System.Globalization.CultureInfo('de-DE', false).DateTimeFormat
		return System.DateTime.Now.ToString(dtfi)

	private static def getruntime1() as string:
		runtimeName as string = typeof(object).GetType().FullName
		runtimeVersion as string = System.Environment.Version.ToString()
		converterGeneratedName4 = runtimeName
		if converterGeneratedName4 == 'System.RuntimeType':
			runtimeName = 'Microsoft .NET Framework'
		else:
			if converterGeneratedName4 == 'System.MonoType':
				runtimeNameVersion = cast(string, typeof(object).Assembly.GetType('Mono.Runtime').InvokeMember('GetDisplayName', ((((System.Reflection.BindingFlags.InvokeMethod | System.Reflection.BindingFlags.NonPublic) | System.Reflection.BindingFlags.Static) | System.Reflection.BindingFlags.DeclaredOnly) | System.Reflection.BindingFlags.ExactBinding), null, null, null))
				parts as (string) = runtimeNameVersion.Split(' '.ToCharArray(), 2)
				runtimeName = parts[0]
				runtimeVersion = parts[1]
			else:
				if converterGeneratedName4 == 'System.Reflection.ClrType':
					runtimeName = 'DotGNU Portable.NET'
				else:
					runtimeName = (('<' + runtimeName) + '>')
		return ((runtimeName + ' ') + runtimeVersion)

	private static def checkbits_int1() as int:
		num = 1
		last_num = 0
		bits = 0
		while true:
			last_num = num
			unchecked:
			  num *= 2
			  num += 1
			bits += 1
			break  unless ((((num - 1) / 2) == last_num) and (bits < 101))
		return bits

	private static def checkbits_double1() as int:
		num = 1.0
		last_num = 0.0
		bits = 0
		while true:
			last_num = num
			num *= 2.0
			num += 1
			bits += 1
			break  unless ((((num - 1.0) / 2.0) == last_num) and (bits < 101))
		return bits

	private static def print_info():
		cs_version as string = ((((('Runtime: ' + getruntime1()) + ', ') + System.Reflection.Assembly.GetExecutingAssembly().GetName().Version) + ', ') + Environment.OSVersion.ToString())
		Console.WriteLine(((((((((('BM Bench v' + prg_version) + ' (') + prg_language) + ') -- (int:') + checkbits_int1()) + ' double:') + checkbits_double1()) + ') ') + cs_version))
		Console.WriteLine('(c) Marco Vieth, 2006')
		Console.WriteLine(getdate1())

	private static def print_results(bench1 as int, bench2 as int, bench_res1 as (double), nfi as System.Globalization.NumberFormatInfo):
		max_language_len1 = 10
		Console.WriteLine('\nThroughput for all benchmarks (loops per sec):')
		str as string = (('BMR (' + prg_language) + ')')
		for i in range(prg_language.Length, max_language_len1):
			str += ' '
		str += ': '
		for bench in range(bench1, (bench2 + 1)):
			str += String.Format(nfi, '{0,9:F2} ', bench_res1[bench])
		Console.WriteLine(str)
		Console.WriteLine('')

	private static def start_bench(bench1 as int, bench2 as int, n as int) as int:
		cali_ms = 1001
		delta_ms = 100
		max_ms = 10000
		print_info()
		nfi as System.Globalization.NumberFormatInfo = System.Globalization.CultureInfo('en-US', false).NumberFormat
		bench_res1 as (double) = array(double, (bench2 + 1))
		for bench in range(bench1, (bench2 + 1)):
			loops = 1
			x = 0
			t1 as long = 0
			t2 as long = 0
			Console.WriteLine('Calibrating benchmark {0} with n={1}', bench, n)
			while true:
				t1 = get1_ms()
				x = run_bench(bench, loops, n)
				t1 = (get1_ms() - t1)
				t_delta as long = ((t2 - t1) if (t2 > t1) else (t1 - t2))
				loops_p_sec as double = (((loops * 1000.0) / t1) if (t1 > 0) else 0)
				Console.WriteLine('{0,10}/s (time={1,5} ms, loops={2,7}, delta={3,5} ms, x={4})', loops_p_sec.ToString('F3', nfi), t1, loops, t_delta, x)
				if x == (-1):
					bench_res1[bench] = (-1)
					break 
				if t2 > 0:
					if t_delta < delta_ms:
						bench_res1[bench] = loops_p_sec
						Console.WriteLine('Benchmark {0} ({1}): {2}/s (time={3} ms, loops={4}, delta={5} ms)', bench, prg_language, bench_res1[bench].ToString('F3', nfi), t1, loops, t_delta)
						break 
				if t1 > max_ms:
					Console.WriteLine('Benchmark {0} ({1}): Time already > {2} ms. No measurement possible.', bench, prg_language, max_ms)
					bench_res1[bench] = (-1)
					break 
				scale_fact as int = ((cast(int, ((cali_ms + 100) / t1)) + 1) if ((t1 < cali_ms) and (t1 > 0)) else 2)
				loops *= scale_fact
				t2 = (t1 * scale_fact)
		print_results(bench1, bench2, bench_res1, nfi)
		return 0

	public static def Main(args as (string)) as int:
	//def Main(args as (string)):
		start_t as long = get1_ms()
		bench1 = 0
		bench2 = 5
		n = 1000000
		if args.Length > 0:
			bench1 = int.Parse(args[0])
			bench2 = bench1
		if args.Length > 1:
			bench2 = int.Parse(args[1])
		if args.Length > 2:
			n = int.Parse(args[2])
		rc as int = start_bench(bench1, bench2, n)
		Console.WriteLine((('Total elapsed time: ' + (get1_ms() - start_t)) + ' ms'))
		return rc
	

[STAThread]
def Main(argv as (string)):
  return Bmbench.Main(argv)

// end
