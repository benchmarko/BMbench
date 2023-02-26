//
// BM Bench - bmbench.ts (TypeScript)
// (c) Marco Vieth, 2002-2023
// http://www.benchmarko.de
//
// 05.02.2023 0.074 based on bmbench.js 0.074
// 19.02.2023 0.08  bench05 optimized
//
// Usage:
// bmbench([bench1], [bench2], [n])  (from HTML)
//
// Compile:
// tsc bmbench.ts
// or using tsconfig.json: tsc --build
//
// Notes:
// This JavaScript can be included in an HTML page to start in a browser or executed
// by a stand-alone JavaScript engine.
//
// Tested with the following JS engines:
// 1. nodeJS
// TODO: 2. Rhino (JS Engine written in JAVA; http://www.mozilla.org/js)
// https://developer.mozilla.org/en-US/docs/Mozilla/Projects/Rhino/Download_Rhino
// java -jar rhino-1.7.7.1.jar bmbench2.js
// Usage: java -jar js.jar bmbench.js (or: java -classpath js.jar org.mozilla.javascript.tools.shell.Main bmbench1.js)
// Compiler: [export CLASSPATH=js.jar]
//           java -classpath js.jar org.mozilla.javascript.tools.jsc.Main -opt 0 bmbench.js
//           java -classpath js.jar bmbench
//
// TODO: 3. SpiderMonkey (JS Engine written in C; http://www.mozilla.org/js/spidermonkey/)
// Compile SpiderMonkey
// - gmake BUILD_OPT=1 -f Makefile.ref  (see README.html)
//   (On my Linux system I get a js shell in Linux_All_OPT.OBJ/js)
// Usage: js bmbench.js [bench1] [bench2] [n]
// (Help: echo "help()" | js_sp;  JavaScript-C 1.5 pre-release 5 2003-01-10)
// (Debugging: js -s ...  for strict)
//
// (Does not work any more:) 4. Windows Scripting host (cscript.exe) (Documentation for Jscript: Help in Microsoft Script Editor)
// Usage: cscript.exe bmbench.js [bench1] [bench2] [n]
//
// 5. DMDScript (ds.exe) (http://www.digitalmars.com/dscript/index.html)
// Usage: ds.exe bmbench.js  (DMDScript version 1.06 cannot parse arguments)
//
// 6. GraalVM
// Usage: js bmbench.js  or:  js --jvm bmbench.js

// TODO: IE 5.01, 5.51: ok

"use strict";

/* jslint plusplus: true */
/* globalsXXX ARGS, Graal, java, print, println, System, VM, window, ScriptEngine, ScriptEngineBuildVersion, ScriptEngineMajorVersion, ScriptEngineMinorVersion, System, Uint8Array, Uint16Array, WScript */ // make ESLint, JSlint happy


// NGS (not supported any more since it does only support global functions)
declare var System: any;
declare var VM: any;

// JScript (Windows Scripting Host) or DMDScript 
declare var ScriptEngine: () => string;
declare var ScriptEngineMajorVersion: () => string;
declare var ScriptEngineMinorVersion: () => string;
declare var ScriptEngineBuildVersion: () => string;

// Rhino or Java
declare var java: any;

// Graal
declare var Graal: any;

// DMDScript
//declare var println;

declare var WScript: any;

//var arguments;

declare var bmBenchNoAutoStart: boolean | undefined; //TTT


/*
if (typeof globalThis === "undefined") {
	(globalThis as any) = {};
}
*/

const gState: Record <string, any> = {
	prgVersion: "0.08",
	prgLanguage: "TypeScript",
	mode: 0, // runmode: 1=test (todo), 2=node BenchmarkJs

	fnGetMs: Date.now, // get ms, set below
	fnGetPrecMs: undefined as ((stopFlg: boolean) => number) | undefined, // get precise ms, set below
	startMs: 0, // set later
	bench1: 0, // bench1: first benchmark
	bench2: 5, // bench2: last benchmark
	n: 1000000, // n: max number
	caliMs: 1001, // calibration ms (minimum time needed for calibration)
	deltaMs: 100, // delta ms (maximum delta between estimation and measurement to use result)
	maxMs: 10000, // max ms (maximum time of a single benchmark run)
	benchRes:[] as  number[], // benchmark results
	bench: 0, // bench: current benchmark
	tsType: "", // type of time stamp source
	tsPrecMs: 0, // measured time stamp precision
	tsPrecCnt: 0, // time stamp count (calls) per precision interval (until time change)
	tsMeasCnt: 0, // last measured count
	bWantStop: false, // set to break benchmark execution
	fnLog: typeof console !== "undefined" ? console.log : (_s: string) => {}, // XXXeslint-disable-line no-console
	fnDone: undefined as (() => void) | undefined, // callback when done
	benchList: [] as ((n: number) => number)[],
	bench03Sieve: undefined as Uint8Array | number[] | undefined, // e.g. cscript has no Uint8Array
	bench05Line: undefined as Uint16Array | number[] | undefined,
	bench05LastLine: undefined as Uint16Array | number[] | undefined,
};


//
// General description for benchmark test functions
// benchxx - benchmark
// <description>
// in:   n = maximum number (assumed even, default n=1000000)
// out:  x = <output description>
//
// loops may be increased to produce a longer runtime without changing the result.
//


//
// bench00 (Integer 16 bit)
// (sum of 1..n) % 65536
//
function bench00(n: number) {
	const nDiv65536 = n >> 16,
		nMod65536 = n & 0xffff;
	let x = 0;

	for (let i = nDiv65536; i > 0; i--) {
		for (let j = 32767; j > 0; j--) {
			x += j;
		}
		for (let j = -32768; j < 0; j++) {
			x += j;
		}
	}
	for (let j = nMod65536; j > 0; j--) {
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
function bench01(n: number) {
	let x = 0,
		sum = 0;

	for (let i = 1; i <= n; i++) {
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
function bench02(n: number) {
	let x = 0,
		sum = 0.0;

	for (let i = 1; i <= n; i++) {
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
function bench03(n: number) {
	//n = (n / 2) | 0; // compute only up to n/2
	const nHalf = n >> 1; // div 2

	if (!gState.bench03Sieve || gState.bench03Sieve.length !== nHalf + 1) {
		gState.bench03Sieve = typeof Uint8Array !== "undefined" ? new Uint8Array(nHalf + 1) : new Array(nHalf + 1);
	}
	const sieve1 = gState.bench03Sieve;

	// initialize sieve
	if (sieve1.fill) {
		sieve1.fill(0);
	} else {
		for (let i = 0; i <= nHalf; i++) {
			sieve1[i] = 0;
		}
	}

	// compute primes
	let i = 0;
	let m = 3;
	let x = 1; // number of primes below n (2 is prime)
	while (m * m <= n) {
		if (!sieve1[i]) {
			x++; // m is prime
			let j = (m * m - 3) >> 1; // div 2
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
function bench04(n: number) {
	const m = 2147483647, // prime number 2^31-1; modulus, do not change!
		a = 16807, // 7^5, one primitive root; multiplier
		q = 127773, // m div a
		r = 2836; // m mod a
	let	x = 1; // 1=Last random value

	for (let i = 0; i < n; i++) {
		const xDivQ = (x / q) | 0;
		const xModQ = x - q * xDivQ;
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
function bench05(n: number) {
	//n = (n / 200) | 0; // compute only up to n/200

	// Instead of nCk with k=n/2, we compute the product of (n/2)Ck with k=0..n/4
	n = (n / 2) | 0; // div 2

	let k = (n / 2) | 0; // div 2
	if ((n - k) < k) {
		k = n - k; // keep k minimal with  n over k  =  n over n-k
	}

	if (!gState.bench05Line || gState.bench05Line.length !== k + 1) {
		gState.bench05Line = typeof Uint16Array !== "undefined" ? new Uint16Array(k + 1) : new Array(k + 1);
	}
	let line = gState.bench05Line;

	// initialize
	if (line.fill) {
		line.fill(0);
	} else {
		for (let j = 0; j <= k; j++) {
			line[j] = 0;
		}
	}
	line[0] = 1;
	line[1] = 2; // for line 2, second column is 2

	// compute lines of Pascal's triangle
	let x = 0;
	for (let i = 3; i <= n; i++) {
		let min1 = (i - 1) >> 1;
		if ((i & 1) === 0) { // new element?
			line[min1 + 1] = 2 * line[min1];
		}

		let prevElem = line[1];
		for (let j = 2; j <= min1; j++) {
			const elem = line[j];
			line[j] = (prevElem + line[j]) & 0xffff; // for Uint16Array we could skip the & 0xffff but not for Array
			prevElem = elem;
		}
		line[1] = i; // second column is i
		// console.log("DEBUG: " + i + ": " + line.join(","));
	}

	// compute sum of ((n/2)Ck)^2 mod 65536 for k=0..n/2
	x = 0;
	for (let j = 0; j < k; j++) {
		x = (x + 2 * line[j] * line[j]) & 0xffff; // add nCk and nC(n-k)
	}
	x = (x + line[k] * line[k]) & 0xffff; // we assume that k is even, so we need to take the middle element

	return x;
}

//
// run a benchmark
// in: bench = benchmark to use
//     loops = number of loops
//         n = maximum number (used in some benchmarks to define size of workload)
// out:    x = result
//
function runBench(bench: number, loops: number, n: number, check: number) {
	const fnBench = gState.benchList[bench];

	if (!fnBench) {
		gState.fnLog("Error: Unknown benchmark " + bench);
	}

	let x = 0;
	while (loops-- > 0 && x === 0) {
		x = fnBench(n);
		x -= check;
	}

	x += check;
	if (x !== check) {
		gState.fnLog("Error(bench" + bench + "): x=" + x);
		x = -1;
	}
	return x;
}


// bench03 check function (used for n !== 1000000)
function bench03Check(n: number) {
	//n = (n / 2) | 0; // compute only up to n/2

	let x = 0;
	for (let j = 2; j <= n; j++) {
		let isPrime = true;
		for (let i = 2; i * i <= j; i++) {
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
	n = (n / 500) | 0; // compute only up to n/500

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


function getCheck(bench: number, n: number) {
	let check: number;

	switch (bench) {
	case 0: // (n / 2) * (n + 1)
		//check = ((n / 2) * (n + 1)) % 65536;
		check = (((n + (n & 1)) >> 1) * (n + 1 - (n & 1))) & 0xffff; // 10528 for n=1000000
		break;

	case 1:
		check = ((n + 1) / 2) | 0;
		break;

	case 2:
		check = ((n + 1) / 2) | 0;
		break;

	case 3:
		check = (n === 500000) ? 41538 : bench03Check(n);
		break;

	case 4:
		check = (n === 1000000) ? 1227283347 : bench04(n); // bench04 not a real check
		break;

	case 5:
		check = (n === 5000) ? 17376 : bench05(n); // bench05 not a real check
		break;

	default:
		gState.fnLog("Error: Unknown benchmark " + bench);
		check = -1;
		break;
	}
	return check;
}


function strNumFormat(s: string | number, iLen: number, sFillChar: string) {
	s = String(s);
	for (let i = s.length; i < iLen; i++) {
		s = sFillChar + s;
	}
	return s;
}

function strZeroFormat(s: string, iLen: number) {
	return strNumFormat(s, iLen, "0");
}

function strIntFormat(val: number, digits: number) {
	return strNumFormat(val, digits, " ");
}

function strDoubleFormat(val: number, digits: number, prec: number) {
	const displPrecAfter = Math.pow(10, prec); // display precision after decimal point

	let str = String(Math.round(val * displPrecAfter) / displPrecAfter); // buffer for one formatted value

	let dotPos = str.indexOf(".");
	// integers do not have a dot yet...
	if (dotPos < 0) {
		dotPos = str.length;
		str += ".";
	}

	// format to prec digits after comma, beware of exponential numbers, e.g. 1e+23!
	let count = prec + 1 - (str.length - dotPos);
	while (count > 0) {
		str += "0";
		count--;
	}

	str = strNumFormat(str, digits, " ");
	return str;
}

function fnGetDate() {
	const dt = new Date(),
		str = strZeroFormat(String(dt.getDate()), 2) + "." + strZeroFormat(String(dt.getMonth() + 1), 2)
			+ "." + ((dt.getFullYear) ? dt.getFullYear() : dt.getFullYear())
			// NSG JS engine has no dt.getFullYear()
			+ " " + strZeroFormat(String(dt.getHours()), 2) + ":" + strZeroFormat(String(dt.getMinutes()), 2)
			+ ":" + strZeroFormat(String(dt.getSeconds()), 2);

	return str;
}


// Here we compute the number of "significant" bits for positive numbers (which means 53 for double)
function checkbitsInt() {
	let num = 1,
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
	let num = 1.0,
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
	let str = "BM Bench v" + gState.prgVersion + " (" + gState.prgLanguage + ") -- (int:" + checkbitsInt() + " double:" + checkbitsDouble() + " tsType:" + gState.tsType + " tsMs:" + gState.tsPrecMs + " tsCnt:" + gState.tsPrecCnt + ")";

	if (typeof navigator !== "undefined") { // only in browsers...
		str += " appCodeName=" + navigator.appCodeName + ", appName=" + navigator.appName
			+ ", appVersion=" + navigator.appVersion + ", platform=" + navigator.platform + ", userAgent=" + navigator.userAgent;
	}
	if (typeof process !== "undefined") { // node.js
		str += " name=" + process.release.name + ", version=" + process.version + ", v8=" + process.versions.v8 + ", arch=" + process.arch + ", platform=" + process.platform;
	}
	if (typeof System !== "undefined") { // NGS JS Engine
		str += " Interpreter: " + System.canonicalHost + ", VM.version=" + VM.version;
	}
	if (typeof ScriptEngine !== "undefined") { // JScript (Windows Scripting Host) or DMDScript
		str += " " + ScriptEngine() + " Version " + ScriptEngineMajorVersion() + "." + ScriptEngineMinorVersion() + "." + ScriptEngineBuildVersion(); // eslint-disable-line new-cap
	}
	if (typeof java !== "undefined") { // Rhino or Java active?
		if (java.lang.System) {
			const jls = java.lang.System;
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

	const d = new Date();

	str += "\n(c) Marco Vieth, 2002-2023\n"
	+ "Date: " + d.getFullYear() + "-" + ("0" + (d.getMonth() + 1)).slice(-2) + "-" + ("0" + d.getDate()).slice(-2) + " "
	+ ("0" + d.getHours()).slice(-2) + ":" + ("0" + d.getMinutes()).slice(-2) + ":" + ("0" + d.getSeconds()).slice(-2);
	//+ "." + ("0" + d.getMilliseconds()).slice(-3);

	return str;
}


function printResults(bench1: number, bench2: number, benchRes: number[]) {
	const prgLanguage = gState.prgLanguage,
		maxLanguageLen = 10;

	let str = "\nThroughput for all benchmarks (loops per sec):\nBMR (" + prgLanguage + ")";
	for (let i = prgLanguage.length; i < maxLanguageLen; i++) {
		str += " ";
	}
	str += ": ";

	for (let bench = bench1; bench <= bench2; bench++) {
		str += strDoubleFormat(benchRes[bench], 9, 3) + " ";
	}
	gState.fnLog(str + "\n");
}


function measureBench(bench: number, n: number, check: number) {
	const fnGetPrecMs = gState.fnGetPrecMs,
		caliMs = gState.caliMs, // 1001
		deltaMs = gState.deltaMs, // 100
		maxMs = gState.maxMs; // 10000

	let	loops = 1, // number of loops
		tEsti = 0, // estimated time
		throughput = 0;

	if (!fnGetPrecMs) {
		gState.fnLog("Error: fnGetPrecMs undefined");
		return 0;
	}
	gState.fnLog("Calibrating benchmark " + bench + " with n=" + n + ", check=" + check);

	while (!throughput && !gState.bWantStop) {
		let tMeas = fnGetPrecMs(false); // start measurement when time changes
		const x = runBench(bench, loops, n, check);
		tMeas = fnGetPrecMs(true) - tMeas; // stop measurement and count until time changes

		const tDelta = (tEsti > tMeas) ? (tEsti - tMeas) : (tMeas - tEsti); // compute difference abs(measures-estimated)
		const loopsPerSec = (tMeas > 0) ? (loops * 1000.0 / tMeas) : 0;
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
			let scaleFact: number;
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


/*
// only useful for linear problem size:
function determineProblemSizeN(bench, n) {
	let fnGetPrecMs = gState.fnGetPrecMs,
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

function determineProblemSizeN(_bench, n) {
	return n;
}
*/

function endBench(startMs: number) {
	printResults(gState.bench1, gState.bench2, gState.benchRes);
	gState.fnLog("Total elapsed time: " + (gState.fnGetMs() - startMs) + " ms");
	if (gState.fnDone) {
		gState.fnDone();
	}
}


function fnSetTimeout(func: () => void, time: number) {
	if (typeof setTimeout !== "undefined") { // not for cscript
		setTimeout(func, time);
	} else {
		func(); // call it directly
	}
}


function doBench() {
	let n = gState.n; //determineProblemSizeN(bench, gState.n);
	const scaleN = gState.n / n;
	let bench = gState.bench;

	// reduce problem size
	if (bench === 3) {
		n = (n / 2) | 0;
	} else if (bench === 5) {
		n = (n / 200) | 0;
	}

	let rc = 0;
	const check = getCheck(bench, n);
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
		fnSetTimeout(doBench, 1);
	} else {
		endBench(gState.startMs);
	}
}


function getMsPerformanceNow() {
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
	} else if ((Date as any).now) {
		gState.fnGetMs = getMsDateNow;
		gState.tsType = "DateNow";
	} else {
		gState.fnGetMs = getMsNewDate;
		gState.tsType = "NewDate";
	}
}



function correctTime(tMeas: number, measCount: number) {
	const tsPrecCnt = gState.tsPrecCnt;

	if (measCount < tsPrecCnt) {
		tMeas += gState.tsPrecMs * ((tsPrecCnt - measCount) / tsPrecCnt); // ts + correction
	}
	return tMeas;
}

function getPrecMs(stopFlg: boolean) {
	let fnGetMs = gState.fnGetMs,
		measCount = 0;

	const tMeas0 = fnGetMs();
	let tMeas = tMeas0;
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

// usually only needed if time precision is low, e.g. one second
function determineTsPrecision() {
	initMsGetter();
	gState.startMs = gState.fnGetMs(); // memorize start time

	let tMeas0 = getPrecMs(false);
	let tMeas1 = getPrecMs(false);
	gState.tsPrecMs = tMeas1 - tMeas0;
	gState.tsPrecCnt = gState.tsMeasCnt;

	// do it again
	tMeas0 = tMeas1;
	tMeas1 = getPrecMs(false);
	if (gState.tsMeasCnt > gState.tsPrecCnt) { // take maximum count
		gState.tsPrecCnt = gState.tsMeasCnt;
		gState.tsPrecMs = tMeas1 - tMeas0;
	}
	gState.fnGetPrecMs = getPrecMs;
}


function doBenchmarkJs() {
	// https://benchmarkjs.com/
	// https://github.com/bramstein/hypher/blob/110febf9abadc6f91aeb118e475d0b98ec0a9e19/test/benchmark.js#L3

	// eslint-disable-next-line global-require
	const Benchmark = require("benchmark");
	const suite = new Benchmark.Suite("BMBench BenchmarkJs"),
		createFunc = function (i: number) {
			const benchFn = gState.benchList[i];
			return function () {
				return benchFn(gState.n);
			};
		};

	for (let i = gState.bench1; i <= gState.bench2; i++) {
		suite.add("bench0" + i, createFunc(i));
	}

	suite.on("cycle", function (event: Event) {
		console.log(String(event.target));
	}).on("complete", function (event: Event) {
		console.log("Fastest is " + JSON.stringify(event.currentTarget));
	}).run({
		async: true
	});
}


function startBench(oArgs: any) {
	if (!oArgs) {
		gState.fnLog("DEBUG: startBench: No args.");
		return;
	}

	for (let sKey in oArgs) {
		if (!oArgs.hasOwnProperty || oArgs.hasOwnProperty(sKey)) {
			gState[sKey] = oArgs[sKey];
		}
	}

	gState.benchList = [
		bench00,
		bench01,
		bench02,
		bench03,
		bench04,
		bench05
	];

	determineTsPrecision();
	gState.fnLog(getInfo());

	gState.bench = gState.bench1;
	gState.benchRes = [];

	// benchmark
	if (gState.mode === 2) {
		doBenchmarkJs();
	} else {
		fnSetTimeout(doBench, 1);
	}
}


function main(args: any[]) {
	const oArgs: Record<string, any> = {},
		parNames = [
			"bench1",
			"bench2",
			"n",
			"mode" // run mode
		];

	if (typeof bmBenchNoAutoStart !== "undefined") {
		gState.fnLog("DEBUG: bmBenchNoAutoStart set.");
		return;
	}

	if (args) {
		let index = 0;
		while (index < args.length) {
			const parName = parNames[index];
			if (parName) {
				oArgs[parName] = parseInt(args[index], 10);
			}
			index++;
		}
	}

	startBench(oArgs);
}

// ---------------------------------------

/*
// DMDScript does not like functions inside "if" (and NGS js no anonymous functions), so define them outside...
function fnLogNGS(s: string) {
	System.print(s + "\n"); // or: System.stdout.writeln(str);
}
*/

/*
function fnLogDMD(s: string) {
	println(s);
}
*/

function fnLogRhino(s: string) {
	(print as any)(s); //TTT
}

function fnLogJScript(s: string) {
	WScript.Echo(s); // eslint-disable-line new-cap
}


function fnGetArguments(args: any[], startIndex: number) {
	const aArgs: any[] = []; // copy arguments into array

	for (let i = startIndex; i < args.length; i++) {
		aArgs[i - startIndex] = args[i];
	}
	return aArgs;
}

function fnGetWscriptArguments(args: typeof WScript.Arguments) { //args: (i: number) => any) {
	const aArgs: any[] = []; // copy arguments into array

	for (let i = 0; i < args.length; i++) {
		aArgs[i] = args.Item(i); // function call //TTT
	}
	return aArgs;
}

function fnGetNodeStdinArgs() {
	let args = [];

	try {
		args = require("fs").readFileSync(process.stdin.fd).toString().split(/ +/);
	} catch (e) {
		gState.fnLog(e.message);
	}
	return args;
}


if (typeof window === "undefined") { // are we outside of a browser in a standalone JS engine?
	if ((typeof process === "object") && (typeof process.versions === "object") && (typeof process.versions.node !== "undefined")) { // Node.js
		gState.fnLog = console.log; // eslint-disable-line no-console
		global.performance = require("perf_hooks").performance; // eslint-disable-line global-require
		// console.log(performance);
		main(process.argv.length > 2 || process.stdin.isTTY ? fnGetArguments(process.argv, 2) : fnGetNodeStdinArgs());
	/* TODO
	} else if (typeof arguments !== "undefined") { // Rhino, SpiderMonkey, DMDScript...
		if (typeof println !== "undefined") { // DMDScript
			gState.fnLog = (s: string) => {	println(s);	}
			// Note: arguments for DMDScript do not work.
		} else { // Rhino, SpiderMonkey
			gState.fnLog = fnLogRhino;
		}
		main(fnGetArguments(arguments, 0)); // start script
	*/
	} else if (typeof WScript !== "undefined") { // JScript (cscript)...
		gState.fnLog = fnLogJScript;
		//gState.fnLog(WScript.stdin.AtEndOfStream); //TTT
		main(fnGetWscriptArguments(WScript.Arguments)); // copy arguments into array
	} else {
		main([]); // unknown engine, call without arguments
	}
} else {
	gState.fnLog = console.log; // eslint-disable-line no-console
}


if (typeof module !== "undefined" && module.exports) {
	module.exports = startBench;
}
// end
