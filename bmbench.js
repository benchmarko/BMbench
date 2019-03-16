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
// xx.xx.2019 0.07  bench03, bench04 optimized
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

"use strict";

/* jslint plusplus: true */
/* globals ARGS, java, print, println, System, VM, window, document, ScriptEngine, ScriptEngineBuildVersion, ScriptEngineMajorVersion, ScriptEngineMinorVersion, System, WScript */ // make ESLint, JSlint happy

var gState = {
		startMs: null, // set later
		bench1: 0, // bench1: first benchmark
		bench2: 5, // bench2: last benchmark
		n: 1000000, // n: max number
		caliMs: 1001, // calibration ms (minimum time needed for calibration)
		deltaMs: 100, // delta ms (maximum delta between estimation and measurement to use result)
		maxMs: 10000, // max ms (maximum time of a single benchmark run)
		benchRes: [],
		bench: 0, // bench: current benchmark
		bWantStop: false,
		fnLog: null,
		fnDone: null
	},
	myint = null,
	prgVersion = "0.062",
	prgLanguage = "JavaScript";


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
// bench00 (Integer 16 bit) TTT TODO: define other BM
// (alternating sum of 1..n) mod 65536
//
function bench00(loops, n, check) {
	var x = 0,
		i;

	while (loops-- > 0 && x === 0) {
		for (i = 0; i < n; i++) {
			x = (i - x) & 0xffff; // much faster than % 65536
		}
		x += check;
	}
	return x % 65536;
}


//
// bench01 (Integer 16/32 bit)
// (alternating sum of 1..n) mod 65536
function bench01(loops, n, check) {
	var x = 0,
		i;

	while (loops-- > 0 && x === 0) {
		for (i = 0; i < n; i++) {
			x = i - x;
		}
		x += check;
	}
	return x % 65536;
}


//
// bench02 (Floating Point, normally 64 bit)
// (sum of 1..n) mod 65536
//
function bench02(loops, n, checkDouble) {
	var x = 0.0,
		i;

	while (loops-- > 0 && x === 0) {
		for (i = n; i > 0; i--) {
		//for (i = 0; i < n; i++) {
			x += i;
		}
		x -= checkDouble;
	}
	return x % 65536;
}


//
// bench03 (Integer)
// number of primes below n (Sieve of Eratosthenes)
// Example: n=500000 => x=41538 (expected), n=1000000 => x=78498
// (no multiples of 2 stored)
function bench03(loops, n, check) {
	var x = 0, // number of primes below n
		sieve1 = [],
		i, j;

	n /= 2; // compute only up to n/2
	sieve1[0] = 0;
	sieve1[1] = 0;
	while (loops-- > 0 && x === 0) {
		// initialize sieve
		for (i = 2; i <= n; i++) {
			sieve1[i] = 0;
		}
		// compute primes
		x++; // 2 is prime
		for (i = 3; (i * i) <= n; i += 2) {
			if (!sieve1[i]) {
				x++;
				for (j = i * i; j <= n; j += i * 2) {
					sieve1[j] = 1;
				}
			}
		}
		// count remaining primes
		while (i <= n) {
			if (!sieve1[i]) {
				x++;
			}
			i += 2;
		}
		x -= check;
		/*
		// check prime count
		if (loops > 0) {
			x -= 41538; // restore number of primes below 1000000
			if (x !== 0) {
				x++;
				break;
			}
		}
		*/
	}
	return x;
}


//
// bench04 (Integer 32 bit)
// nth random number number
// Random number generator taken from
// Raj Jain: The Art of Computer Systems Performance Analysis, John Wiley & Sons, 1991, page 442-444.
// It needs longs with at least 32 bit.
// Starting with x0=1, x10000 should be 1043618065, x1000000 = 1227283347
function bench04(loops, n, check) {
	var m = 2147483647, // modulus, do not change!
		a = 16807, // multiplier
		q = 127773, // m div a
		r = 2836, // m mod a
		x = 0, // random value
		i;

	while (loops-- > 0 && x === 0) {
		x++; // start with 1=last random value
		for (i = 0; i < n; i++) {
			x = a * (x % q) - r * myint(x / q);
			if (x <= 0) {
				x += m; // x is new random number
			}
		}
		x -= check;
		/*
		if (loops > 0) {
			x -= 1227283347;
			if (x !== 0) {
				x++;
				break;
			}
			x++;
		}
		*/
	}
	return x;
}


//
// bench05 (Integer 32 bit)
// n over n/2 mod 65536 (Pascal's triangle)
//
function bench05(loops, n, check) {
	var x = 0,
		pas1 = [
			[1],
			[1]
		], // arrays: 2* k+1; first elements set to 1
		k, i, j, iMod2,	min1;

	n = myint(n / 500);
	k = myint(n / 2); // div 2

	if ((n - k) < k) {
		k = n - k; // keep k minimal with  n over k  =  n over n-k
	}

	/*
	// allocate memory...
	var pas1 = new Array(2);
	pas1[0] = new Array(k + 1);
	pas1[1] = new Array(k + 1);
	pas1[0][0] = 1; pas1[1][0] = 1; // set first column
	*/

	while (loops-- > 0 && x === 0) {
		for (i = 3; i <= n; i++) {
			iMod2 = i & 1;
			min1 = (iMod2 === 0) ? ((i - 2) >> 1) : ((i - 1) >> 1); // Math.floor((i - 1) / 2);
			if (k < min1) {
				min1 = k;
			}
			pas1[iMod2][1] = i; // second column is i
			for (j = 2; j <= min1; j++) { // up to min((i-1)/2, k)
				pas1[iMod2][j] = (pas1[iMod2 ^ 1][j - 1] + pas1[iMod2 ^ 1][j]) & 0xffff; // we need mod here to avoid overflow
			}
			if ((min1 < k) && (iMod2 === 0)) { // new element
				pas1[iMod2][min1 + 1] = 2 * pas1[iMod2 ^ 1][min1];
			}
		}
		x += pas1[n & 1][k] & 0xffff;
		x -= check;
		/*
		if (loops > 0) {
			x -= 27200;
			if (x !== 0) {
				x++;
				break;
			}
		}
		*/
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
function runBench(bench, loops, n) {
	var x = 0,
		check, checkDouble;

	switch (bench) {
	case 0:
		check = myint(n / 2 * ((n % 2 === 0) ? -1 : 1)) % 65536; // 41248, assuming n even!
		x = bench00(loops, n, check);
		break;

	case 1:
		check = myint(n / 2 * ((n % 2 === 0) ? -1 : 1)); // assuming n even! (sum should be ...)
		x = bench01(loops, n, check);
		break;

	case 2:
		//check = n / 2.0 * ((n % 2 === 0) ? -1.0 : 1.0); // assuming n even! (sum should be 5.000005E11)
		checkDouble = (n / 2.0) * (n + 1.0); // assuming n even! (sum should be 5.000005E11)
		check = checkDouble % 65536;
		x = bench02(loops, n, checkDouble);
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
		gState.fnLog("Error: Unknown benchmark " + bench);
		check = -1;
		break;
	}

	x += check;
	if (x !== check) {
		gState.fnLog("Error(bench" + bench + "): x=" + x);
		x = -1; // exit
	}
	return x;
}


//
// get timestamp in milliseconds
// out: x = time in ms
//
// This function is intended for short measurements only
//
function getMs() {
	var d1 = new Date();

	return d1.getTime();
}


function strNumFormat(s, iLen, sFillChar) {
	var i;

	s = String(s);
	for (i = s.length; i < iLen; i += 1) {
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
		displPrecAfter = Math.pow(10, 2); // display precision after decimal point

	str += String(Math.round(val * displPrecAfter) / displPrecAfter);

	// integers do not have a dot yet...
	if (str.indexOf(".") < 0) {
		str += ".";
	}

	// format to prec digits after comma
	while ((str.length <= prec) || (str.charAt(str.length - (prec + 1)) !== ".")) {
		str += "0";
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

	str = "BM Bench v" + prgVersion + " (" + prgLanguage + ") -- (int:" + checkbitsInt() + " double:" + checkbitsDouble() + ")";

	if (typeof navigator !== "undefined") { // only in browsers...
		str += " appCodeName=" + navigator.appCodeName + ", appName=" + navigator.appName
			+ ", appVersion=" + navigator.appVersion + ", platform=" + navigator.platform + ", userAgent=" + navigator.userAgent;
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
			str += " java.version=" + jls.getProperty("java.version") + ", java.vendor=" + jls.getProperty("java.vendor")
				+ "os.name=" + jls.getProperty("os.name") + ", os.arch=" + jls.getProperty("os.arch")
				+ ", os.version=" + jls.getProperty("os.version");
		}
	}
	str += "\n(c) Marco Vieth, 2002-2019\n" + fnGetDate();
	return str;
}


function printResults(bench1, bench2, benchRes) {
	var maxLanguageLen = 10,
		str,
		i,
		bench;

	str = "\nThroughput for all benchmarks (loops per sec):\nBMR (" + prgLanguage + ")";
	for (i = prgLanguage.length; i < maxLanguageLen; i++) {
		str += " ";
	}
	str += ": ";

	for (bench = bench1; bench <= bench2; bench++) {
		str += strDoubleFormat(benchRes[bench], 9, 2) + " ";
	}
	gState.fnLog(str + "\n");
}


function measureBench(bench, n) {
	var caliMs = gState.caliMs, // 1001
		deltaMs = gState.deltaMs, // 100
		maxMs = gState.maxMs, // 10000
		loops = 1, // number of loops
		tMeas, // measured time
		tEsti = 0, // estimated time
		rc = 0,
		x, // result from benchmark
		tDelta, loopsPerSec, scaleFact;

	gState.fnLog("Calibrating benchmark " + bench + " with n=" + n);

	while (!rc && !gState.bWantStop) {
		tMeas = getMs();
		x = runBench(bench, loops, n);
		tMeas = getMs() - tMeas;

		tDelta = (tEsti > tMeas) ? (tEsti - tMeas) : (tMeas - tEsti); // compute difference abs(measures-estimated)
		loopsPerSec = (tMeas > 0) ? (loops * 1000.0 / tMeas) : 0;
		gState.fnLog(strDoubleFormat(loopsPerSec, 10, 3) + "/s (time=" + strIntFormat(myint(tMeas), 5) + " ms, loops=" + strIntFormat(loops, 7) + ", delta=" + strIntFormat(myint(tDelta), 5) + " ms, x=" + x);
		if (x === -1) { // some error?
			rc = -1;
		} else if ((tEsti > 0) && (tDelta < deltaMs)) { // estimated time already? smaller than delta_ms=100?
			rc = loopsPerSec; // yeah, set measured loops per sec
			gState.fnLog("Benchmark " + bench + " (" + prgLanguage + "): " + strDoubleFormat(loopsPerSec, 0, 3) + "/s (time=" + tMeas + " ms, loops=" + loops + ", delta=" + tDelta + " ms)");
		} else if (tMeas > maxMs) {
			gState.fnLog("Benchmark " + bench + " (" + prgLanguage + "): Time already > " + maxMs + " ms. No measurement possible.");
			rc = -1;
		} else {
			scaleFact = ((tMeas < caliMs) && (tMeas > 0)) ? myint((caliMs + 100) / tMeas) + 1 : 2;
			// scale a bit up to 1100 ms (cali_ms+100)
			loops *= scaleFact;
			tEsti = tMeas * scaleFact;
		}
	}
	return rc;
}


function endBench(startMs) {
	printResults(gState.bench1, gState.bench2, gState.benchRes);
	gState.fnLog("Total elapsed time: " + (getMs() - startMs) + " ms");
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
		n = gState.n,
		rc;

	rc = measureBench(bench, n);
	gState.benchRes[bench] = rc;
	bench += 1;
	if (bench <= gState.bench2 && !gState.bWantStop) {
		gState.bench = bench;
		fnSetTimeout(doBench, 100);
	} else {
		endBench(gState.startMs);
	}
}


function startBench(oArgs) {
	var sKey;

	if (!oArgs) {
		gState.fnLog("DEBUG: startBench: No args.");
		return;
	}
	//gState.fnLog("DEBUG: startBench: called with " + oArgs);

	for (sKey in oArgs) {
		if (oArgs.hasOwnProperty(sKey)) {
			gState[sKey] = oArgs[sKey];
		}
	}

	gState.startMs = getMs(); // memorize start time

	gState.fnLog(getInfo());

	gState.bench = gState.bench1;
	gState.benchRes.length = 0;

	fnSetTimeout(doBench, 100);
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

	if (typeof global.bmBenchNoAutoStart !== "undefined") {
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


function fnMyInt(x) {
	return x | 0;
}

myint = fnMyInt;

if (typeof window === "undefined") { // are we outside of a browser in a standalone JS engine?
	if ((typeof process === "object") && (typeof process.versions === "object") && (typeof process.versions.node !== "undefined")) { // Node.js
		gState.fnLog = console.log; // eslint-disable-line no-console
		main(fnGetArguments(process.argv, 2));
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
