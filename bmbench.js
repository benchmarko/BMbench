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
// 05.02.2023 0.073 use Array.fill, if available; removed myint
// 19.02.2023 0.08  bench05 optimized
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
// 6. GraalVM
// Usage: js bmbench.js  or:  js --jvm bmbench.js

// IE 4.01: not ok
// IE 5.01, 5.51: ok
// Chrome: BMR (JavaScript):   1060.45    804.07    536.04    464.48     69.80    317.58

"use strict";

/* jslint plusplus: true */
/* globals ARGS, Graal, java, print, println, System, VM, window, ScriptEngine, ScriptEngineBuildVersion, ScriptEngineMajorVersion, ScriptEngineMinorVersion, System, Uint8Array, Uint16Array, WScript */ // make ESLint, JSlint happy

var gState = {
	prgVersion: "0.08",
	prgLanguage: "JavaScript",
	mode: 0, // runmode: 1=test (todo), 2=node BenchmarkJs

	fnGetMs: undefined, // get ms, set below
	fnGetPrecMs: undefined, // get precise ms, set below
	startMs: undefined, // set later
	bench1: 0, // bench1: first benchmark
	bench2: 5, // bench2: last benchmark
	n: 1000000, // n: max number

	benchList: undefined,
	checkList: undefined,
	nPerBench: [],
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
	fnLog: undefined, // log function, set below
	fnDone: undefined, // callback when done
	bench03Sieve: undefined,
	bench05Line: undefined,
	bench05LastLine: undefined
};


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
function bench00(n) {
	var nDiv65536 = n >> 16,
		nMod65536 = n & 0xffff,
		x = 0,
		i, j;

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
	return x & 0xffff;
}

//
// bench01 (Integer 32 bit)
// (arithmetic mean of 1..n)
// (to avoid numbers above 2*n, divide by n using subtraction)
// bench01(loops, 1000000, (1000000 + 1) / 2) | 0)
//
function bench01(n) {
	var x = 0,
		sum = 0,
		i;

	for (i = 1; i <= n; i++) {
		sum += i;
		if (sum >= n) {
			sum -= n;
			x++;
		}
	}
	return x;
}


//
// bench02 (Floating Point, normally 64 bit)
// (arithmetic mean of 1..n)
function bench02(n) {
	var x = 0,
		sum = 0.0,
		i;

	for (i = 1; i <= n; i++) {
		sum += i;
		if (sum >= n) {
			sum -= n;
			x++;
		}
	}
	return x;
}


//
// bench03 (Integer)
// number of primes less than or equal to n (prime-counting function)
// Example: n=500000 => x=41538 (expected), n=1000000 => x=78498
// (Sieve of Eratosthenes, no multiples of 2's are stored)
function bench03(n, state) {
	var nHalf = n >> 1, // div 2
		x, sieve1, i, j, m;

	if (!state.bench03Sieve) {
		state.bench03Sieve = typeof Uint8Array !== "undefined" ? new Uint8Array(nHalf + 1) : new Array(nHalf + 1); // set the size we need, only for odd numbers
	}
	sieve1 = state.bench03Sieve;

	// initialize sieve
	if (sieve1.fill) {
		sieve1.fill(0);
	} else {
		for (i = 0; i <= nHalf; i++) {
			sieve1[i] = 0; // odd numbers are possible primes
		}
	}

	// compute primes
	i = 0;
	m = 3;
	x = 1; // number of primes below n (2 is prime)
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
function bench04(n) {
	var m = 2147483647, // prime number 2^31-1; modulus, do not change!
		a = 16807, // 7^5, one primitive root; multiplier
		q = 127773, // m div a
		r = 2836, // m mod a
		x = 1, // 1=Last random value
		i, xDivQ, xModQ;

	for (i = 0; i < n; i++) {
		xDivQ = (x / q) | 0;
		xModQ = x - q * xDivQ;
		x = a * xModQ - r * xDivQ;
		// faster than: x = a * (x % q) - r * ((x / q) | 0);
		if (x <= 0) {
			x += m; // x is new random number
		}
	}
	return x;
}


//
// bench05 (Integer 32 bit)
// (n choose n/2) mod 65536 (Central Binomial Coefficient mod 65536)
// Using dynamic programming and Pascal's triangle, storing only one line
// Instead of nCk mod 65536 with k=n/2, we compute the product of (n/2)Ck mod 65536 with k=0..n/4 (Vandermonde folding)
// Example: (2000 choose 1000) mod 65536 = 27200
//
function bench05(n, state) {
	var	x, k, i, j, min1, line, num, prev;

	// Instead of nCk with k=n/2, we compute the product of (n/2)Ck with k=0..n/4
	n = (n / 2) | 0; // div 2

	k = (n / 2) | 0; // div 2
	if ((n - k) < k) {
		k = n - k; // keep k minimal with  n over k  =  n over n-k
	}

	if (!state.bench05Line || state.bench05Line.length !== k + 1) {
		state.bench05Line = typeof Uint16Array !== "undefined" ? new Uint16Array(k + 1) : new Array(k + 1);
	}
	line = state.bench05Line;

	// initialize (not needed)
	if (line.fill) {
		line.fill(0);
	} else {
		for (j = 0; j <= k; j++) {
			line[j] = 0;
		}
	}

	line[0] = 1;
	line[1] = 2; // for line 2, second column is 2

	// compute lines of Pascal's triangle
	for (i = 3; i <= n; i++) {
		min1 = (i - 1) >> 1; // (i - 1) div 2
		if ((i & 1) === 0) { // new element?
			line[min1 + 1] = 2 * line[min1];
		}

		prev = line[1];
		for (j = 2; j <= min1; j++) {
			num = line[j];
			line[j] = (prev + line[j]) & 0xffff; // for Uint16Array we could skip the & 0xffff but not for Array
			prev = num;
		}
		line[1] = i; // second column is i
		// gState.fnLog("DEBUG: " + i + ": " + line.join(","));
	}

	// compute sum of ((n/2)Ck)^2 mod 65536 for k=0..n/2
	x = 0;
	for (j = 0; j < k; j++) {
		x = (x + 2 * line[j] * line[j]) & 0xffff; // add nCk and nC(n-k)
	}
	x = (x + line[k] * line[k]) & 0xffff; // we assume that k is even, so we need to take the middle element

	return x;
}

// https://benchmarksgame-team.pages.debian.net/benchmarksgame/description/toosimple.html
// (Gregory series formula in slow convergence towards Pi) 
function bench06(n) {
	var sum = 0.0,
		flip = 1.0,
		i;

	for (i = 1; i <= n; i++) {
		sum += flip / (2 * i - 1);
		flip *= -1.0;
	}
	return ((sum * 4.0) * 100000000) | 0;
}

gState.benchList = [
	bench00,
	bench01,
	bench02,
	bench03,
	bench04,
	bench05,
	bench06
];

// checks

function bench00Check(n) {
	// (n / 2) * (n + 1)
	//check = ((n / 2) * (n + 1)) % 65536; // NGS: cannot use "& 0xffff" here because it would use integer then
	return (((n + (n & 1)) >> 1) * (n + 1 - (n & 1))) & 0xffff; // 10528 for n=1000000
	//gState.fnLog("DEBUG: check " + check);
}

function bench01Check(n) {
	return ((n + 1) / 2) | 0;
}

function bench02Check(n) {
	return ((n + 1) / 2) | 0;
}

function bench03Check(n) {
	var x, i, j, isPrime;

	if (n === 500000) {
		x = 41538;
	} else {
		x = 1; // 2 is prime
		for (j = 3; j <= n; j += 2) {
			isPrime = true;
			for (i = 3; i * i <= j; i += 2) {
				if (j % i === 0) {
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

function bench04Check(n) {
	return (n === 1000000) ? 1227283347 : bench04(n); // bench04 not a real check
}

function bench05Check(n) {
	return (n === 5000) ? 17376 : bench05(n, gState); // bench05 not a real check
}

function bench06Check(n) {
	return (n === 1000000) ? 314159165 : bench06(n);
}

gState.checkList = [
	bench00Check,
	bench01Check,
	bench02Check,
	bench03Check,
	bench04Check,
	bench05Check,
	bench06Check
];

//
// run a benchmark
// in: bench = benchmark to use
//     loops = number of loops
//         n = maximum number (used in some benchmarks to define size of workload)
// out:    x = result
//
function runBench(bench, loops, n, check) {
	var fnBench = gState.benchList[bench],
		x;

	x = 0;
	while (loops-- > 0 && x === 0) {
		x = fnBench(n, gState);
		x -= check;
	}

	x += check;
	if (x !== check) {
		gState.fnLog("Error(bench" + bench + "): x=" + x);
		x = -1;
	}
	return x;
}

/*
function bench05Check(n) {
	var p = 65521, // TODO: 65536; must be prime, e.g. 65521
		numerator = 1,
		denominator = 1,
		numCount = 0,
		denCount = 0,
		r, greater, lesser;

	n = (n / 500) | 0; // compute only up to n/500

	r = n >> 1;
	greater = (r >= (n - r)) ? r : (n - r);
	lesser = n - greater;


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

function strNumFormat(s, iLen, sFillChar) {
	var i;

	s = String(s);
	for (i = s.length; i < iLen; i++) {
		s = sFillChar + s;
	}
	return s;
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


function strZeroFormat(s, iLen) {
	return strNumFormat(s, iLen, "0"); // NSG JS engine has no slice()
}


function getInfo() {
	var d = new Date(),
		str, jls;

	str = "BM Bench v" + gState.prgVersion + " (" + gState.prgLanguage + ") -- (int:" + checkbitsInt() + " double:" + checkbitsDouble() + " tsType:" + gState.tsType + " tsMs:" + gState.tsPrecMs + " tsCnt:" + gState.tsPrecCnt + ")";

	if (typeof navigator !== "undefined") { // only in browsers...
		str += " appCodeName=" + navigator.appCodeName + ", appName=" + navigator.appName
			+ ", appVersion=" + navigator.appVersion + ", platform=" + navigator.platform + ", userAgent=" + navigator.userAgent;
	}
	if (typeof global !== "undefined" && typeof global.process !== "undefined") { // node.js
		str += " name=" + global.process.release.name + ", version=" + global.process.version + ", v8=" + global.process.versions.v8 + ", arch=" + global.process.arch + ", platform=" + global.process.platform;
		// gState.fnLog(process);
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

	str += "\n(c) Marco Vieth, 2002-2023\n"
		+ "Date: " + ((d.getFullYear) ? d.getFullYear() : strZeroFormat(String(d.getYear()), 2)) + "-"
		+ strZeroFormat(String(d.getMonth()), 2) + "-" + strZeroFormat(String(d.getDate()), 2) + " "
		+ strZeroFormat(String(d.getHours()), 2) + ":" + strZeroFormat(String(d.getMinutes()), 2) + ":" + strZeroFormat(String(d.getSeconds()), 2);
	// NSG JS engine has no Date().getFullYear(), no slice()

	return str;
}


function printResults(bench1, bench2, benchRes) {
	var prgLanguage = gState.prgLanguage,
		maxLanguageLen = 10,
		str,
		i,
		bench;

	str = "\nThroughput for all benchmarks (loops per sec):\nBMR (" + prgLanguage + ")";
	for (i = prgLanguage.length; i < maxLanguageLen; i++) {
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

	while (!throughput && !gState.bWantStop) {
		tMeas = fnGetPrecMs(); // start measurement when time changes
		x = runBench(bench, loops, n, check);
		tMeas = fnGetPrecMs(1) - tMeas; // stop measurement and count until time changes

		tDelta = (tEsti > tMeas) ? (tEsti - tMeas) : (tMeas - tEsti); // compute difference abs(measures-estimated)
		loopsPerSec = (tMeas > 0) ? (loops * 1000.0 / tMeas) : 0;
		gState.fnLog(strDoubleFormat(loopsPerSec, 10, 3) + "/s (time=" + strDoubleFormat(tMeas, 9, 3) + " ms, loops=" + strIntFormat(loops, 7) + ", delta=" + strDoubleFormat(tDelta, 9, 3) + " ms)");

		if (x === -1) { // some error?
			throughput = -1;
		} else if ((tEsti > 0) && (tDelta < deltaMs)) { // estimated time already? smaller than delta_ms=100?
			throughput = loopsPerSec; // yeah, set measured loops per sec
			gState.fnLog("Benchmark " + bench + " (" + gState.prgLanguage + "): " + strDoubleFormat(loopsPerSec, 0, 3) + "/s (time=" + strDoubleFormat(tMeas, 0, 3) + " ms, loops=" + loops + ", delta=" + strDoubleFormat(tDelta, 0, 3) + " ms)");
		} else if (tMeas > maxMs) {
			gState.fnLog("Benchmark " + bench + " (" + gState.prgLanguage + "): Time already > " + maxMs + " ms. No measurement possible.");
			throughput = (loopsPerSec) ? -loopsPerSec : -1; // cannot rely on measurement, so set to negative
		} else {
			if (tMeas === 0) {
				scaleFact = 50;
			} else if (tMeas < caliMs) {
				scaleFact = (((caliMs + 100) / tMeas) | 0) + 1; // scale a bit up to 1100 ms (cali_ms+100)
			} else {
				scaleFact = 2;
			}
			loops *= scaleFact;
			tEsti = tMeas * scaleFact;
		}
	}
	return throughput;
}


function endBench(startMs) {
	printResults(gState.bench1, gState.bench2, gState.benchRes);
	gState.fnLog("Total elapsed time: " + (gState.fnGetMs() - startMs) + " ms");
	if (gState.fnDone) {
		gState.fnDone();
	}
}


function fnSetTimeout(func, time) {
	if (typeof setTimeout !== "undefined") {
		setTimeout(func, time);
	} else {
		func(); // call it directly
	}
}


function doBench() {
	var bench = gState.bench,
		n, check, rc;

	if (bench >= 0 && bench < gState.benchList.length) {
		n = gState.nPerBench[bench];
		check = gState.checkList[bench](n);
		rc = measureBench(bench, n, check);
	} else {
		gState.fnLog("Error: Unknown benchmark " + bench);
		rc = -1;
	}
	gState.benchRes[bench] = rc;

	bench++;
	if (bench <= gState.bench2 && !gState.bWantStop) {
		gState.bench = bench;
		fnSetTimeout(doBench, 1);
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


function onBenchmarkJsEvent(event) {
	//gState.fnLog(event);
	if (event.type === "cycle") {
		gState.fnLog(String(event.target));
	} else if (event.type === "complete") {
		gState.fnLog("\n");
		gState.fnLog("Fastest is " + JSON.stringify(event.currentTarget));
	} else {
		gState.fnLog("Error: unknown event type: " + event.type);
		//gState.fnLog("xxx: " + JSON.stringify(event));
	}
}

function runOneBenchmarkJs() {
	//console.log("TTT: ", this); //TTT
	//console.log("TTT: ", arguments); //TTT
	return this._benchFn(this._n, gState); // eslint-disable-line no-underscore-dangle, no-invalid-this
}

function benchmarkJsCreateFunction(i) {
	return {
		_n: gState.nPerBench[i],
		_benchFn: gState.benchList[i],

		fn: runOneBenchmarkJs
		//onStart: onBenchmarkJsEvent,
		//onComplete: onBenchmarkJsEvent
	};
}

function doBenchmarkJs() {
	// https://benchmarkjs.com/
	// https://github.com/bramstein/hypher/blob/110febf9abadc6f91aeb118e475d0b98ec0a9e19/test/benchmark.js#L3

	var Benchmark = require("benchmark"); // eslint-disable-line global-require

	var suite = new Benchmark.Suite("BMBench BenchmarkJs"), // eslint-disable-line one-var
		i;

	for (i = gState.bench1; i <= gState.bench2; i++) {
		suite.add("bench0" + i, benchmarkJsCreateFunction(i));
	}

	suite.on("cycle", onBenchmarkJsEvent).on("complete", onBenchmarkJsEvent).run({
		async: true
	});
}


function startBench(oArgs, sArgStr) {
	var sKey, bench, n;

	if (!oArgs) {
		gState.fnLog("DEBUG: startBench: No args.");
		return;
	}

	for (sKey in oArgs) {
		if (!oArgs.hasOwnProperty || oArgs.hasOwnProperty(sKey)) {
			gState[sKey] = oArgs[sKey];
		}
	}

	for (bench = 0; bench < gState.benchList.length; bench++) {
		n = gState.n;

		// reduce problem size
		if (bench === 3) {
			n = (n / 2) | 0;
		} else if (bench === 5) {
			n = (n / 200) | 0;
		}
		gState.nPerBench[bench] = n;
	}

	determineTsPrecision();
	gState.fnLog(getInfo());
	if (sArgStr) {
		gState.fnLog("Args:" + sArgStr);
	}

	gState.bench = gState.bench1;
	gState.benchRes = [];

	// benchmark
	if (gState.mode === 2) {
		doBenchmarkJs();
	} else {
		fnSetTimeout(doBench, 1);
	}
}


function main(args) {
	var	oArgs = {},
		parNames = [
			"bench1",
			"bench2",
			"n",
			"caliMs",
			"mode" // run mode
		],
		index = 0,
		argStr = "",
		parName;

	if (typeof bmBenchNoAutoStart !== "undefined") {
		gState.fnLog("DEBUG: bmBenchNoAutoStart set.");
		return;
	}

	if (args) {
		argStr = args.join(" ");
		while (index < args.length) {
			parName = parNames[index];
			if (parName) {
				oArgs[parName] = parseInt(args[index], 10);
			}
			index++;
		}
	}

	startBench(oArgs, argStr);
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
		args = require("fs").readFileSync(global.process.stdin.fd).toString().split(/ +/); // eslint-disable-line global-require
	} catch (e) {
		gState.fnLog(e.message);
	}
	return args;
}


if (typeof window === "undefined") { // are we outside of a browser in a standalone JS engine?
	if (typeof global !== "undefined" && typeof global.process === "object" && typeof global.process.versions === "object" && typeof global.process.versions.node !== "undefined") { // Node.js
		gState.fnLog = console.log; // eslint-disable-line no-console
		global.performance = require("perf_hooks").performance; // eslint-disable-line global-require
		// gState.fnLog(performance);
		main(global.process.argv.length > 2 || global.process.stdin.isTTY ? fnGetArguments(global.process.argv, 2) : fnGetNodeStdinArgs());
	} else if (typeof System !== "undefined") { // System object is available with NGS JS Engine
		gState.fnLog = fnLogNGS;
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
