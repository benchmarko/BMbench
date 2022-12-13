//
// BM Bench - bmbench.js (JavaScript)
// (c) Marco Vieth, 2002-2019
// http://www.benchmarko.de
//
// 06.05.2002 0.01
// 11.05.2002 0.02  bench01 = (sum 1..n) mod 65536 (integer)
// 30.05.2002 0.03
// 18.06.2002 0.031 adapted for standalone JS engines (NSG JS Engine, Rhino, ...)
// 20.07.2002 0.04  some errors corrected
// 24.01.2003 0.05  output format changed
// 04.04.2003       navigator platform corrected; use args[0] as first element (as in Java)
// 18.07.2004       added support for JScript (Windows Scripting Host, cscript.exe)
// 17.01.2006       added support for DMDscript
// 30.11.2006 0.06  based on version 0.05
// 16.03.2019       adapted for Node.js
// 05.05.2019 0.07  changed bench 01-03; time interval estimation
// 15.11.2022 0.071 bench03 corrected
// 02.12.2022 0.072 bench05 improved
//
// Usage:
// bmbench([bench1], [bench2], [n])  (from HTML)
//

// Notes:
// This JavaScript can be included in an HTML page to start in a browser or executed
// by a stand-alone JavaScript engine.
//
// Tested with the following JS engines:
// 1. Stand alone NGS JS Engine:

// Usage: js bmbench.js [bench1] [bench2] [n]  (or use compiler: js -c -O2 bmbench1.js, js bmbench1.jsc)
// - Has no windows, navigator object, but System, System.stdout.writeln, ...
// - Allows integer arithmetic (int(), isInt()), but Math.floor(), division results float
// - Does not allow object function syntax  f1 = function() { blabla; };
// - allows byte code generation, optimization
// Important note, if you use NGS Javascript engine:
// js-0.2.5 has some serious bugs in file b_math.c
// - Math.pow(a, b) computes a^a if b is an Integer (Math.atan2() also affected!)
//   Correct line 111:  d2 = (double) args[1].u.vinteger;  =>  d2 = (double) cvt.u.vinteger;
// - Math.min(), Math.Max() return always the first argument (which may be sometimes correct)
//   Correct line 259: js_vm_to_number (vm, &args[1], &cvt);  =>  js_vm_to_number (vm, &args[i+1], &cvt);
//
// 2. Rhino (JS Engine written in JAVA; http://www.mozilla.org/js)
// https://developer.mozilla.org/en-US/docs/Mozilla/Projects/Rhino/Download_Rhino
// java -jar rhino-1.7.7.1.jar bmbench2.js
// Usage: java -jar js.jar bmbench.js (or: java -classpath js.jar org.mozilla.javascript.tools.shell.Main bmbench1.js)
// Compiler: [export CLASSPATH=js.jar]
//           java -classpath js.jar org.mozilla.javascript.tools.jsc.Main -opt 0 bmbench.js
//           java -classpath js.jar bmbench
//
// 3. SpiderMonkey (JS Engine written in C; http://www.mozilla.org/js/spidermonkey/)
// Compile SpiderMonkey
// - gmake BUILD_OPT=1 -f Makefile.ref  (see README.html)
//   (On my Linux system I get a js shell in Linux_All_OPT.OBJ/js)
// Usage: js bmbench.js [bench1] [bench2] [n]
// (Help: echo "help()" | js_sp;  JavaScript-C 1.5 pre-release 5 2003-01-10)
// (Debugging: js -s ...  for strict)
//
// 4. Windows Scripting host (cscript.exe) (Documentation for Jscript: Help in Microsoft Script Editor)
// Usage: cscript.exe bmbench.js [bench1] [bench2] [n]
//
// 5. DMDScript (ds.exe) (http://www.digitalmars.com/dscript/index.html)
// Usage: ds.exe bmbench.js  (DMDScript version 1.06 cannot parse arguments)
//

// IE 4.01: not ok
// IE 5.01, 5.51: ok
// Chrome: BMR (JavaScript):   1060.45    804.07    536.04    464.48     69.80    317.58

"use strict";

/* jslint plusplus: true */
/* globals ARGS, Graal, java, print, println, System, VM, window, ScriptEngine, ScriptEngineBuildVersion, ScriptEngineMajorVersion, ScriptEngineMinorVersion, System, Uint8Array, Uint16Array, WScript */ // make ESLint, JSlint happy

var gState = {
		fnGetMs: null, // get ms, set below
		fnGetPrecMs: null, // get precise ms, set below
		startMs: null, // set later
		bench1: 0, // bench1: first benchmark
		bench2: 5, // bench2: last benchmark
		n: 1000000, // n: max number
		caliMs: 1001, // calibration ms (minimum time needed for calibration)
		deltaMs: 100, // delta ms (maximum delta between estimation and measurement to use result)
		maxMs: 10000, // max ms (maximum time of a single benchmark run)
		benchRes: [], // benchmark results
		bench: 0, // bench: current benchmark
		tsType: "", // type of time stamp source
		tsPrecMs: 0, // measured time stamp precision
		tsPrecCnt: 0, // time stamp count (calls) per precision interval (until time change)
		tsMeasCnt: 0, // last measured count
		bWantStop: false, // set to break benchmark execution
		fnLog: null, // log function, set below
		fnDone: null, // callback when done
		hasTimeout: typeof setTimeout !== "undefined",
		hasUint8Array: typeof Uint8Array !== "undefined",
		hasUint16Array: typeof Uint16Array !== "undefined"
	},
	myint = null,
	gPrgVersion = "0.072",
	gPrgLanguage = "JavaScript";


//
// General description for benchmark test functions
// benchxx - benchmark
// <description>
// in: loops = number of loops
//         n = maximum number (assumed even, normally n=1000000)
// out:    x = <output description>
//
// loops may be increased to produce a longer runtime without changing the result.
//


//
// bench00 (Integer 16 bit)
// (sum of 1..n) % 65536
//
function bench00(loops, n, check) {
	var x = 0,
		nDiv65536 = (n >> 16),
		nMod65536 = (n & 0xffff),
		i, j;

	while (loops-- > 0 && x === 0) {
		for (i = nDiv65536; i > 0; i--) {
			for (j = 32767; j > 0; j--) {
				x += j;
			}
			for (j = -32768; j < 0; j++) {
				x += j;
			}
		}
		for (j = nMod65536; j > 0; j--) {
			x += j;
		}
		x &= 0xffff;
		x -= check;
	}
	return x & 0xffff;
}


//
// bench01 (Integer 32 bit)
// (arithmetic mean of 1..n)
// (to avoid numbers above 2*n, divide by n using subtraction)
// bench01(loops, 1000000, (1000000 + 1) / 2) | 0)
//
function bench01(loops, n, check) {
	var x = 0,
		sum = 0,
		i;

	while (loops-- > 0 && x === 0) {
		sum = 0;
		for (i = 1; i <= n; i++) {
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
// bench02 (Floating Point, normally 64 bit)
// (arithmetic mean of 1..n)
function bench02(loops, n, check) {
	var x = 0,
		sum = 0.0,
		i;

	while (loops-- > 0 && x === 0) {
		sum = 0.0;
		for (i = 1; i <= n; i++) {
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
function bench03(loops, n, check) {
	var x = 0, // number of primes below n
		sieve1, i, j, nHalf, m;

	n = (n / 2) | 0; // compute only up to n/2
	nHalf = n >> 1; // div 2

	sieve1 = gState.hasUint8Array ? new Uint8Array(nHalf + 1) : new Array(nHalf + 1); // set the size we need, only for odd numbers
	// gState.fnLog("DEBUG: nHalf=" + (nHalf) + ", sieve1.length=" + sieve1.length);
	while (loops-- > 0 && x === 0) {
		// initialize sieve
		for (i = 0; i <= nHalf; i++) {
			sieve1[i] = 0; // odd numbers are possible primes
		}
		// compute primes
		i = 0;
		m = 3;
		x++; // 2 is prime
		while (m * m <= n) {
			if (!sieve1[i]) {
				x++; // m is prime
				j = (m * m - 3) >> 1; // div 2
				while (j < nHalf) {
					sieve1[j] = 1;
					j += m;
				}
			}
			i++;
			m += 2; // or: =2 * i + 3;
		}

		// count remaining primes
		while (m <= n) {
			if (!sieve1[i]) {
				x++; // m is prime
			}
			i++;
			m += 2;
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
// (or: https://www.cse.wustl.edu/~jain/cse567-08/ftp/k_26rng.pdf 26.21, Example 26.3; q=127773; 26.45)
// It needs longs with at least 32 bit.
// Starting with x0=1, x10000 should be 1043618065, x1000000 = 1227283347
function bench04(loops, n, check) {
	var m = 2147483647, // prime number 2^31-1; modulus, do not change!
		a = 16807, // 7^5, one primitive root; multiplier
		q = 127773, // m div a
		r = 2836, // m mod a
		x = 0, // random value
		i;

	while (loops-- > 0 && x === 0) {
		x++; // start with 1=last random value
		for (i = 0; i < n; i++) {
			x = a * (x % q) - r * ((x / q) | 0); // x div q
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
function bench05(loops, n, check) {
	var x = 0,
		k, i, j, min1, line, lastLine, tempLine;

	n = myint(n / 500); // compute only up to n/500

	k = myint(n / 2); // div 2
	if ((n - k) < k) {
		k = n - k; // keep k minimal with  n over k  =  n over n-k
	}

	line = gState.hasUint16Array ? new Uint16Array(k + 1) : new Array(k + 1);
	lastLine = gState.hasUint16Array ? new Uint16Array(k + 1) : new Array(k + 1);
	line[0] = 1;
	lastLine[0] = 1;

	while (loops-- > 0 && x === 0) {
		// initialize
		for (j = 1; j <= k; j++) {
			line[j] = 0;
			lastLine[j] = 0;
		}

		// compute
		for (i = 3; i <= n; i++) {
			min1 = (i - 1) >> 1;
			if (k < min1) {
				min1 = k;
			}
			line[1] = i; // second column is i
			for (j = 2; j <= min1; j++) {
				line[j] = (lastLine[j - 1] + lastLine[j]) & 0xffff; // for Uint16Array we could skip the & 0xffff but not for Array
			}
			if ((min1 < k) && ((i & 1) === 0)) { // new element
				line[min1 + 1] = 2 * lastLine[min1];
			}

			tempLine = lastLine;
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
function runBench(bench, loops, n, check) {
	var x = 0;

	switch (bench) {
	case 0:
		x = bench00(loops, n, check);
		break;

	case 1:
		x = bench01(loops, n, check);
		break;

	case 2:
		x = bench02(loops, n, check);
		break;

	case 3:
		x = bench03(loops, n, check);
		break;

	case 4:
		x = bench04(loops, n, check);
		break;

	case 5:
		x = bench05(loops, n, check);
		break;

	default:
		gState.fnLog("Error: Unknown benchmark " + bench);
		break;
	}

	x += check;
	if (x !== check) {
		gState.fnLog("Error(bench" + bench + "): x=" + x);
		//x = (x > 0) ? -x : -1;
		x = -1;
	}
	return x;
}


// bench03 check function (used for n !== 1000000)
function bench03Check(n) {
	var i, j, isPrime,
		x = 0;

	n = (n / 2) | 0; // compute only up to n/2

	for (j = 2; j <= n; j++) {
		isPrime = true;
		for (i = 2; i * i <= j; i++) {
			if (j % i === 0) {
				isPrime = false;
				break;
			}
		}
		if (isPrime) {
			x++;
		}
	}
	return x;
}


/*
function bench05Check(n) {
	n = myint(n / 500); // compute only up to n/500

	var p = 65536, // TODO: must be prime!
		r = n >> 1,
		greater = (r >= (n - r)) ? r : (n - r),
		lesser = n - greater,
		numerator = 1,
		denominator = 1,
		numCount = 0,
		denCount = 0;

	for (; n > greater; n--) {
		numerator *= n;
		while (numerator % 10 === 0)	{
			numerator /= 10;
			numCount++;
		}
	}
	for (; lesser > 1; lesser--) {
		denominator *= lesser;
		while (denominator % 10 === 0) {
			denominator /= 10;
			denCount++;
		}
	}
	return (numerator / denominator * Math.pow(10, numCount - denCount)) % p;
}
*/


/*
// https://www.quora.com/How-do-I-find-the-modulus-of-large-combinations-like-nCr-mod-p-where-p-is-a-prime-number
public static long nCr(long n,long r,long p)
{
	long greater=(r>=(n-r))?r:(n-r);
	long lesser=n-greater;
	long numerator=denominator=1,numCount=denCount=0;
	for(;n>greater;n--)
	{
		numerator*=n;
		while(numerator%10==0)
		{
			numerator/=10;
			numCount++;
		}
	}
	for(;lesser>1;lesser--)
	{
		denominator*=lesser;
		while(denominator%10==0)
		{
			denominator/=10;
			denCount++;
		}
	}
	return (numerator/denominator*Math.pow(10,numCount-denCount))%p;
}
*/


function getCheck(bench, n) {
	var check;

	switch (bench) {
	case 0:
		check = ((n / 2) * (n + 1)) % 65536; // NGS: cannot use "& 0xffff" here because it would use integer then
		break;

	case 1:
		check = myint((n + 1) / 2);
		break;

	case 2:
		check = myint((n + 1) / 2);
		break;

	case 3:
		check = (n === 1000000) ? 41538 : bench03Check(n);
		break;

	case 4:
		check = (n === 1000000) ? 1227283347 : bench04(1, n, 0); // bench04 not a real check
		break;

	case 5:
		check = (n === 1000000) ? 27200 : bench05(1, n, 0); // bench05 not a real check
		break;

	default:
		gState.fnLog("Error: Unknown benchmark " + bench);
		check = -1;
		break;
	}
	return check;
}


function strNumFormat(s, iLen, sFillChar) {
	var i;

	s = String(s);
	for (i = s.length; i < iLen; i++) {
		s = sFillChar + s;
	}
	return s;
}

function strZeroFormat(s, iLen) {
	return strNumFormat(s, iLen, "0");
}


function strIntFormat(val, digits) {
	var str = strNumFormat(val, digits, " ");

	return str;
}

function strDoubleFormat(val, digits, prec) {
	var str = "", // buffer for one formatted value
		displPrecAfter = Math.pow(10, prec), // display precision after decimal point
		dotPos, count;

	str += String(Math.round(val * displPrecAfter) / displPrecAfter);

	dotPos = str.indexOf(".");
	// integers do not have a dot yet...
	if (dotPos < 0) {
		dotPos = str.length;
		str += ".";
	}

	// format to prec digits after comma, beware of exponential numbers, e.g. 1e+23!
	count = prec + 1 - (str.length - dotPos);
	while (count > 0) {
		str += "0";
		count--;
	}

	str = strNumFormat(str, digits, " ");
	return str;
}

function fnGetDate() {
	var dt = new Date(),
		str = strZeroFormat(String(dt.getDate()), 2) + "." + strZeroFormat(String(dt.getMonth() + 1), 2)
			+ "." + ((dt.getFullYear) ? dt.getFullYear() : dt.getYear())
			// NSG JS engine has no dt.getFullYear()
			+ " " + strZeroFormat(String(dt.getHours()), 2) + ":" + strZeroFormat(String(dt.getMinutes()), 2)
			+ ":" + strZeroFormat(String(dt.getSeconds()), 2);

	return str;
}


// Here we compute the number of "significant" bits for positive numbers (which means 53 for double)
function checkbitsInt() {
	var num = 1,
		lastNum = 0,
		bits = 0;

	do {
		lastNum = num;
		num *= 2;
		num++;
		bits++;
	} while ((((num - 1) / 2) === lastNum) && (bits < 101));
	return bits;
}

function checkbitsDouble() {
	var num = 1.0,
		lastNum = 0.0,
		bits = 0;

	do {
		lastNum = num;
		num *= 2.0;
		num++;
		bits++;
	} while ((((num - 1.0) / 2.0) === lastNum) && (bits < 101));
	return bits;
}


function getInfo() {
	var str, jls;

	str = "BM Bench v" + gPrgVersion + " (" + gPrgLanguage + ") -- (int:" + checkbitsInt() + " double:" + checkbitsDouble() + " tsType:" + gState.tsType + " tsMs:" + gState.tsPrecMs + " tsCnt:" + gState.tsPrecCnt + ")";

	if (typeof navigator !== "undefined") { // only in browsers...
		str += " appCodeName=" + navigator.appCodeName + ", appName=" + navigator.appName
			+ ", appVersion=" + navigator.appVersion + ", platform=" + navigator.platform + ", userAgent=" + navigator.userAgent;
	}
	if (typeof process !== "undefined") { // node.js
		str += " name=" + process.release.name + ", version=" + process.version + ", v8=" + process.versions.v8 + ", arch=" + process.arch + ", platform=" + process.platform;
		// console.log(process);
	}
	if (typeof System !== "undefined") { // NGS JS Engine
		str += " Interpreter: " + System.canonicalHost + ", VM.version=" + VM.version;
	}
	if (typeof ScriptEngine !== "undefined") { // JScript (Windows Scripting Host) or DMDScript
		str += " " + ScriptEngine() + " Version " + ScriptEngineMajorVersion() + "." + ScriptEngineMinorVersion() + "." + ScriptEngineBuildVersion(); // eslint-disable-line new-cap
	}
	if (typeof java !== "undefined") { // Rhino or Java active?
		if (java.lang.System) {
			jls = java.lang.System;
			if (typeof jls.getProperty === "function") { // not for GraalVM
				str += " java.version=" + jls.getProperty("java.version") + ", java.vendor=" + jls.getProperty("java.vendor")
					+ "os.name=" + jls.getProperty("os.name") + ", os.arch=" + jls.getProperty("os.arch")
					+ ", os.version=" + jls.getProperty("os.version");
			}
		}
	}

	if (typeof Graal !== "undefined") {
		str += " graal.versionJS=" + (Graal.versionECMAScript || Graal.versionJS) + ", graal.version=" + Graal.versionGraalVM + ", isGraal=" + Graal.isGraalRuntime();
	}

	/*
	if (typeof Java !== "undefined") { //?
		//str += " " + Java.type("java.io.File");
		//str += " " + Java.type('org.graalvm').create();
		//str += " " + Java.type('org.graalvm.home.Version').create();
		//str += " " + Java.type('org.graalvm.polyglot.Context').create(); //System.getProperty("org.graalvm.home");
		//str += " XXX: " + org.graalvm.home.Version.getCurrent().isRelease();
	}
	*/

	str += "\n(c) Marco Vieth, 2002-2022\n" + fnGetDate();
	return str;
}


function printResults(bench1, bench2, benchRes) {
	var maxLanguageLen = 10,
		str,
		i,
		bench;

	str = "\nThroughput for all benchmarks (loops per sec):\nBMR (" + gPrgLanguage + ")";
	for (i = gPrgLanguage.length; i < maxLanguageLen; i++) {
		str += " ";
	}
	str += ": ";

	for (bench = bench1; bench <= bench2; bench++) {
		str += strDoubleFormat(benchRes[bench], 9, 3) + " ";
	}
	gState.fnLog(str + "\n");
}


function measureBench(bench, n, check) {
	var fnGetPrecMs = gState.fnGetPrecMs,
		caliMs = gState.caliMs, // 1001
		deltaMs = gState.deltaMs, // 100
		maxMs = gState.maxMs, // 10000

		loops = 1, // number of loops
		tMeas, // measured time
		tEsti = 0, // estimated time
		throughput = 0,
		x, // result from benchmark
		tDelta, loopsPerSec, scaleFact;

	gState.fnLog("Calibrating benchmark " + bench + " with n=" + n + ", check=" + check);

	/*
	//TTT
	do {
		tDelta = tMeas || 100000;
		tMeas = fnGetPrecMs(); // start measurement when time changes
		x = runBench(bench, loops, n, check);
		tMeas = fnGetPrecMs(1) - tMeas; // stop measurement and count until time changes
		gState.fnLog("TTT: tMeas=", tMeas);
	} while (tMeas < tDelta);
	//TTT
	*/

	while (!throughput && !gState.bWantStop) {
		tMeas = fnGetPrecMs(); // start measurement when time changes
		x = runBench(bench, loops, n, check);
		tMeas = fnGetPrecMs(1) - tMeas; // stop measurement and count until time changes

		tDelta = (tEsti > tMeas) ? (tEsti - tMeas) : (tMeas - tEsti); // compute difference abs(measures-estimated)
		loopsPerSec = (tMeas > 0) ? (loops * 1000.0 / tMeas) : 0;
		gState.fnLog(strDoubleFormat(loopsPerSec, 10, 3) + "/s (time=" + strDoubleFormat(tMeas, 9, 3) + " ms, loops=" + strIntFormat(loops, 7) + ", delta=" + strDoubleFormat(tDelta, 9, 3) + " ms)");
		//TTT gState.fnLog(strDoubleFormat(loopsPerSec, 10, 3) + " loops/s (" + strIntFormat(loops, 7) + " /" + strDoubleFormat(tMeas, 9, 3) + " ms) (delta=" + strDoubleFormat(tDelta, 9, 3) + " ms, x=" + x + ")");

		if (x === -1) { // some error?
			throughput = -1;
		} else if ((tEsti > 0) && (tDelta < deltaMs)) { // estimated time already? smaller than delta_ms=100?
			throughput = loopsPerSec; // yeah, set measured loops per sec
			gState.fnLog("Benchmark " + bench + " (" + gPrgLanguage + "): " + strDoubleFormat(loopsPerSec, 0, 3) + "/s (time=" + strDoubleFormat(tMeas, 0, 3) + " ms, loops=" + loops + ", delta=" + strDoubleFormat(tDelta, 0, 3) + " ms)");
		} else if (tMeas > maxMs) {
			gState.fnLog("Benchmark " + bench + " (" + gPrgLanguage + "): Time already > " + maxMs + " ms. No measurement possible.");
			throughput = (loopsPerSec) ? -loopsPerSec : -1; // cannot rely on measurement, so set to negative
		} else {
			if (tMeas === 0) {
				scaleFact = 50;
			} else if (tMeas < caliMs) {
				scaleFact = myint((caliMs + 100) / tMeas) + 1; // scale a bit up to 1100 ms (cali_ms+100)
			} else {
				scaleFact = 2;
			}
			loops *= scaleFact;
			tEsti = tMeas * scaleFact;
		}
	}
	return throughput;
}


/*
// only useful for linear problem size:
function determineProblemSizeN(bench, n) {
	var fnGetPrecMs = gState.fnGetPrecMs,
		n1 = 100,
		check, tMeas, x;

	while (n1 <= n) {
		check = getCheck(bench, n1);
		tMeas = fnGetPrecMs(); // start measurement when time changes
		x = runBench(bench, 1, n1, check);
		tMeas = fnGetPrecMs(1) - tMeas; // stop measurement and count until time changes
		gState.fnLog("TEST: n=" + n1 + ", tMeas=" + tMeas + ", x=" + x);
		if (tMeas > 10) {
			n = n1;
			break;
		}
		if ((tMeas * 10) < 10) {
			n1 *= 10;
		} else {
			n1 *= 2;
		}
	}
	return n;
}
*/

function determineProblemSizeN(_bench, n) {
	return n;
}

function endBench(startMs) {
	printResults(gState.bench1, gState.bench2, gState.benchRes);
	gState.fnLog("Total elapsed time: " + (gState.fnGetMs() - startMs) + " ms");
	if (gState.fnDone) {
		gState.fnDone();
	}
}


function fnSetTimeout(func, time) {
	if (gState.hasTimeout) {
		setTimeout(func, time);
	} else {
		func(); // call it directly
	}
}


function doBench() {
	var bench = gState.bench,
		n, scaleN, check, rc;

	n = determineProblemSizeN(bench, gState.n);
	scaleN = gState.n / n;
	check = getCheck(bench, n);
	if (check > 0) {
		rc = measureBench(bench, n, check);
		rc /= scaleN;
		if (scaleN !== 1) {
			gState.fnLog("Note: Result scaled by factor " + scaleN + " to " + rc);
		}
	} else {
		rc = -1;
	}
	gState.benchRes[bench] = rc;
	bench++;
	if (bench <= gState.bench2 && !gState.bWantStop) {
		gState.bench = bench;
		fnSetTimeout(doBench, 1); //TTT
	} else {
		endBench(gState.startMs);
	}
}


function getMsPerformanceNow() {
	// return ((performance.now() / 1000) | 0) * 1000; // TEST: only second resolution
	return performance.now(); // ms
}

function getMsDateNow() {
	return Date.now();
}

function getMsNewDate() {
	return new Date().getTime();
}

function initMsGetter() {
	if (typeof performance !== "undefined") {
		gState.fnGetMs = getMsPerformanceNow;
		gState.tsType = "PerfNow";
	} else if (Date.now) {
		gState.fnGetMs = getMsDateNow;
		gState.tsType = "DateNow";
	} else {
		gState.fnGetMs = getMsNewDate;
		gState.tsType = "NewDate";
	}
}


function correctTime(tMeas, measCount) {
	var tsPrecCnt = gState.tsPrecCnt;

	if (measCount < tsPrecCnt) {
		tMeas += gState.tsPrecMs * ((tsPrecCnt - measCount) / tsPrecCnt); // ts + correction
	}
	return tMeas;
}

function getPrecMs(stopFlg) {
	var fnGetMs = gState.fnGetMs,
		measCount = 0,
		tMeas0, tMeas;

	tMeas0 = fnGetMs();
	tMeas = tMeas0;
	while (tMeas <= tMeas0) {
		tMeas = fnGetMs();
		measCount++;
	}

	if (stopFlg) {
		tMeas = correctTime(tMeas0, measCount); // for stop: use first ts + correction
	}
	gState.tsMeasCnt = measCount; // memorize count
	return tMeas;
}

// usually only neede if time precision is low, e.g. one second
function determineTsPrecision() {
	var tMeas0, tMeas1;

	initMsGetter();
	gState.startMs = gState.fnGetMs(); // memorize start time

	tMeas0 = getPrecMs();
	tMeas1 = getPrecMs();
	gState.tsPrecMs = tMeas1 - tMeas0;
	gState.tsPrecCnt = gState.tsMeasCnt;

	// do it again
	tMeas0 = tMeas1;
	tMeas1 = getPrecMs();
	if (gState.tsMeasCnt > gState.tsPrecCnt) { // take maximum count
		gState.tsPrecCnt = gState.tsMeasCnt;
		gState.tsPrecMs = tMeas1 - tMeas0;
	}
	gState.fnGetPrecMs = getPrecMs;
}


function startBench(oArgs) {
	var sKey;

	if (!oArgs) {
		gState.fnLog("DEBUG: startBench: No args.");
		return;
	}

	for (sKey in oArgs) {
		if (!oArgs.hasOwnProperty || oArgs.hasOwnProperty(sKey)) {
			gState[sKey] = oArgs[sKey];
		}
	}

	determineTsPrecision();
	gState.fnLog(getInfo());

	gState.bench = gState.bench1;
	gState.benchRes = [];

	fnSetTimeout(doBench, 1);
}


function main(args) {
	var oArgs = {},
		parNames = [
			"bench1",
			"bench2",
			"n"
		],
		index = 0,
		parName;

	if (typeof bmBenchNoAutoStart !== "undefined") {
		gState.fnLog("DEBUG: bmBenchNoAutoStart set.");
		return;
	}

	if (args) {
		while (index < args.length) {
			parName = parNames[index];
			if (parName) {
				oArgs[parName] = parseInt(args[index], 10);
			}
			index++;
		}
	}

	startBench(oArgs);
}

// ---------------------------------------

// DMDScript does not like functions inside "if" (and NGS js no anonymous functions), so define them outside...

function fnLogNGS(s) {
	System.print(s + "\n"); // or: System.stdout.writeln(str);
}

function fnLogDMD(s) {
	println(s);
}

function fnLogRhino(s) {
	print(s);
}

function fnLogJScript(s) {
	WScript.Echo(s); // eslint-disable-line new-cap
}


function fnGetArguments(args, startIndex) {
	var aArgs = [], // copy arguments into array
		i;

	for (i = startIndex; i < args.length; i++) {
		aArgs[i - startIndex] = args[i];
	}
	return aArgs;
}

function fnGetWscriptArguments(args) {
	var aArgs = [], // copy arguments into array
		i;

	for (i = 0; i < args.length; i++) {
		aArgs[i] = args(i); // function call
	}
	return aArgs;
}

function fnGetNodeStdinArgs() {
	var args = [];

	try {
		args = require("fs").readFileSync(process.stdin.fd).toString().split(/ +/);
	} catch (e) {
		gState.fnLog(e.message);
	}
	return args;
}


function fnMyInt(x) {
	return x | 0;
}

myint = fnMyInt;


if (typeof window === "undefined") { // are we outside of a browser in a standalone JS engine?
	if ((typeof process === "object") && (typeof process.versions === "object") && (typeof process.versions.node !== "undefined")) { // Node.js
		gState.fnLog = console.log; // eslint-disable-line no-console
		global.performance = require("perf_hooks").performance; // eslint-disable-line global-require
		// console.log(performance);
		main(process.argv.length > 2 || process.stdin.isTTY ? fnGetArguments(process.argv, 2) : fnGetNodeStdinArgs());
	} else if (typeof System !== "undefined") { // System object is available with NGS JS Engine
		gState.fnLog = fnLogNGS;
		// convert to integer with NGS JS Engine engine, use Math.floor for others...
		eval("myint = int"); // set integer cast; avoid warning 'int' is reserved identifier in browsers
		if (typeof ARGS !== "undefined") {
			if ((Math.max(5, 8) !== 8) || (Math.pow(0.5, 2) !== 0.25)) {
				gState.fnLog("ERROR: Buggy NGS Javascript Engine! Correct b_math.c and try again...");
			} else {
				main(fnGetArguments(ARGS, 1)); // program name in ARGS[0]
			}
		}
	} else if (typeof arguments !== "undefined") { // Rhino, SpiderMonkey, DMDScript...
		if (typeof println !== "undefined") { // DMDScript
			gState.fnLog = fnLogDMD;
			// Note: arguments for DMDScript do not work.
		} else { // Rhino, SpiderMonkey
			gState.fnLog = fnLogRhino;
		}
		main(fnGetArguments(arguments, 0)); // start script
	} else if (typeof WScript !== "undefined") { // JScript (cscript)...
		gState.fnLog = fnLogJScript;
		//gState.fnLog(WScript.stdin.AtEndOfStream); //TTT
		main(fnGetWscriptArguments(WScript.Arguments)); // copy arguments into array
	} else {
		main(); // unknown engine, call without arguments
	}
} else {
	gState.fnLog = console.log; // eslint-disable-line no-console
}


if (typeof module !== "undefined" && module.exports) {
	module.exports = startBench;
}
// end
