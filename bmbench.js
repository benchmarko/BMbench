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
// Chrome: BMR (JavaScript):   1060.45    804.07    536.04    464.48     69.80    317.58

"use strict";

/* jslint plusplus: true */
/* globals ARGS, java, print, println, System, VM, window, document, ScriptEngine, ScriptEngineBuildVersion, ScriptEngineMajorVersion, ScriptEngineMinorVersion, System, WScript */ // make ESLint, JSlint happy

var gState = {
		fnGetMs: null, // get ms, set below
		startMs: null, // set later
		bench1: 0, // bench1: first benchmark
		bench2: 5, // bench2: last benchmark
		n: 1000000, // n: max number
		caliMs: 1001, // calibration ms (minimum time needed for calibration)
		deltaMs: 100, // delta ms (maximum delta between estimation and measurement to use result)
		maxMs: 10000, // max ms (maximum time of a single benchmark run)
		benchRes: [], // benchmark results
		bench: 0, // bench: current benchmark
		bWantStop: false, // set to break benchmark execution
		fnLog: null, // log function, set below
		fnDone: null // callback when done
	},
	myint = null,
	prgVersion = "0.07",
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
// (alternating sum of 1..n)
//
function bench00(loops, n, check) {
	var x = 0,
		isOdd = n & 1,
		i;

	while (loops-- > 0 && x === 0) {
		for (i = 1; i <= n; i++) {
			x = (i - x) & 0xffff; // much faster than % 65536
		}
		if (isOdd) {
			x = -x;
		}
		x -= check;
		x &= 0xffff;
	}
	return x;
}


//
// bench01 (Integer 32 bit)
// (alternating sum of 1..n)
function bench01(loops, n, check) {
	var x = 0,
		isOdd = n & 1,
		i;

	while (loops-- > 0 && x === 0) {
		for (i = 1; i <= n; i++) {
			x = (i - x) | 0; //myint(i - x); // x = i - x; //hmm, OR always positive: myint(i - x)
		}
		if (isOdd) {
			x = -x;
		}
		x -= check;
	}
	return x; //% 65536;
}


//
// bench02 (Floating Point, normally 64 bit)
// (alternating sum of 1..n)
function bench02(loops, n, check) {
	var x = 0.0,
		isOdd = n % 2,
		i;

	while (loops-- > 0 && x === 0) {
		for (i = 1; i <= n; i++) {
			x = i - x;
		}
		if (isOdd) {
			x = -x;
		}
		x -= check;
	}
	return x;
}


/*
//
// bench02 (Floating Point, normally 64 bit)
// (sum of 1..n) mod 65536
//
function bench02(loops, n, checkDouble) {
	var x = 0.0,
		i;

	while (loops-- > 0 && x === 0) {
		for (i = n; i > 0; i--) { // or increasing?
			x += i;
		}
		x -= checkDouble;
	}
	return x % 65536;
}
*/


/*
// https://en.wikipedia.org/wiki/Methods_of_computing_square_roots#Binary_numeral_system_.28base_2.29
function iSqrt(num) {
	var res = 0,
		bit = 1 << 30; // The second-to-top bit is set: 1 << 14 (16 bit), 1 << 30 for 32 bits

	// "bit" starts at the highest power of four <= the argument.
	while (bit > num) {
		bit >>= 2;
	}

	while (bit !== 0) {
		if (num >= res + bit) {
			num -= res + bit;
			res = (res >> 1) + bit;
		} else {
			res >>= 1;
		}
		bit >>= 2;
	}
	return res;
}
*/


/*
function primeListDebug(n) {
	var aList = [],
		sieve1, i, j;

	sieve1 = new Array(n + 1); // set the size we need
	sieve1[0] = 0;
	sieve1[1] = 0;

	// initialize sieve
	for (i = 2; i <= n; i++) {
		sieve1[i] = 0; // all numbers are possible primes
	}
	// compute primes
	if (n >= 2) {
		aList.push(2); // 2 is prime
	}
	for (i = 3; (i * i) <= n; i += 2) {
		if (!sieve1[i]) { // not marked -> prime found
			aList.push(i);
			for (j = i * i; j <= n; j += i * 2) { // mark all multiples of prime as non-prime
				sieve1[j] = 1;
			}
		}
	}
	// count remaining primes
	while (i <= n) {
		if (!sieve1[i]) {
			aList.push(i);
		}
		i += 2;
	}
	return aList;
}
*/


/*
// https://primes.utm.edu/glossary/page.php?sort=SieveOfEratosthenes
Eratosthenes(n) {
	a[1] := 0
	for i := 2 to n do a[i] := 1
	p := 2
	while p2  <  n do {
	  j := p2
	  while (j  <  n) do {
		a[j] := 0
		j := j+p
	  }
	  repeat p := p+1 until a[p] = 1
	}
	return(a)
}
*/

/*
https://en.wikipedia.org/wiki/Wheel_factorization
inc = [4, 2, 4, 2, 4, 6, 2, 6],

test:=false
if div(n, 2) = true then return 2;
if div(n, 3) = true then return 3;
if div(n, 5) = true then return 5;
k := 7; i= 1
while test = false and k * k ≤ n do
   test := div(n, k)
   if test = true, then return k
   k := k + inc[i]
   if i < 8 then i := i + 1 else i:= 1
return n.
*/

/*
//https://stackoverflow.com/questions/17394838/python-prime-numbers-generator-yield-vs-return?noredirect=1&lq=1
def primes(n):
    if n==2: return [2]
    elif n<2: return []
    s=range(3,n+1,2)
    mroot = n ** 0.5
    half=(n+1)/2-1
    i=0
    m=3
    while m <= mroot:
        if s[i]:
            j=(m*m-3)/2
            s[j]=0
            while j<half:
                s[j]=0
                j+=m
        i=i+1
        m=2*i+3
    return [2]+[x for x in s if x]
*/


// also ok; 477/sec, 483/sec
function bench03(loops, n, check) {
	var x = 0, // number of primes below n
		sieve1, i, j, nHalf, m;

	if (n & 1) { // isOdd
		n += 1; // even
	}
	n /= 2; // compute only up to n/2

	if (n & 1) { // isOdd
		n += 1; // even
	}
	nHalf = n / 2;

	sieve1 = new Array(nHalf + 1); // set the size we need, only for odd numbers
	while (loops-- > 0 && x === 0) {
		// initialize sieve
		for (i = 0; i <= nHalf; i += 1) {
			sieve1[i] = 0; // odd numbers are possible primes
		}
		// compute primes
		i = 0;
		m = 3;
		while (m * m < n) {
			if (!sieve1[i]) {
				x++;
				j = (m * m - 3) / 2;
				while (j < nHalf) {
					sieve1[j] = 1;
					j += m;
				}
			}
			i += 1;
			m += 2; //= 2 * i + 3;
		}

		// count remaining primes
		while (i <= nHalf) {
			if (!sieve1[i]) {
				x++;
			}
			i += 1;
		}
		x -= check;
	}
	return x;
}


/*
// store only odd numbers in array, array half size: also fast: 464/sec
//function bench03(loops, n, check) {
function bench03_ok2(loops, n, check) {
	var x = 0, // number of primes below n
		sieve1, i, j, nHalf;

	n /= 2; // compute only up to n/2
	nHalf = n >> 1;
	sieve1 = new Array(nHalf + 1); // set the size we need
	sieve1[0] = 0;
	sieve1[1] = 0;

	//var aList = primeListDebug(n);

	while (loops-- > 0 && x === 0) {
		// initialize sieve
		for (i = 0; i < nHalf; i += 1) {
			sieve1[i] = 0; // all odd numbers are possible primes
		}
		// compute primes
		x++; // 2 is prime
		for (i = 3; (i * i) <= n; i += 2) {
			if (!sieve1[i >> 1]) { // not marked -> prime found
				x++;
				//if (aList.indexOf(i) < 0) {
				//	console.log("DEBUG: Not a prime: " + i);
				//}
				for (j = i * i; j <= n; j += i * 2) { // mark all multiples of prime as non-prime
					sieve1[j >> 1] = 1;
				}
			}
		}
		// count remaining primes
		i >>= 1;
		while (i < nHalf) {
			if (!sieve1[i]) {
				x++;
				//if (aList.indexOf(i * 2 + 1) < 0) {
				//	console.log("DEBUG2: Not a prime: " + (i * 2 + 1));
				//}
			}
			i += 1;
		}
		x -= check;
	}
	return x;
}
*/

//
// bench03 (Integer)
// number of primes below n (Sieve of Eratosthenes)
// Example: n=500000 => x=41538 (expected), n=1000000 => x=78498
// (no multiples of 2 stored)
// ok: 403.13, now with init odd numbers: 439
/*
//function bench03(loops, n, check) {
function bench03_ok1(loops, n, check) {
	var x = 0, // number of primes below n
		sieve1, i, j;

	n /= 2; // compute only up to n/2
	sieve1 = new Array(n + 1); // set the size we need
	sieve1[0] = 0;
	sieve1[1] = 0;
	while (loops-- > 0 && x === 0) {
		// initialize sieve
		for (i = 3; i <= n; i += 2) {
			sieve1[i] = 0; // all odd numbers are possible primes
		}
		// compute primes
		x++; // 2 is prime
		for (i = 3; (i * i) <= n; i += 2) {
			if (!sieve1[i]) { // not marked -> prime found
				x++;
				for (j = i * i; j <= n; j += i * 2) { // mark all multiples of prime as non-prime
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
	}
	return x;
}
*/


/*
int count_primes(int n) {
    const int S = 10000;

    vector<int> primes;
    int nsqrt = sqrt(n);
    vector<char> is_prime(nsqrt + 1, true);
    for (int i = 2; i <= nsqrt; i++) {
        if (is_prime[i]) {
            primes.push_back(i);
            for (int j = i * i; j <= nsqrt; j += i)
                is_prime[j] = false;
        }
    }

    int result = 0;
    vector<char> block(S);
    for (int k = 0; k * S <= n; k++) {
        fill(block.begin(), block.end(), true);
        int start = k * S;
        for (int p : primes) {
            int start_idx = (start + p - 1) / p;
            int j = max(start_idx, p) * p - start;
            for (; j < S; j += p)
                block[j] = false;
        }
        if (k == 0)
            block[0] = block[1] = false;
        for (int i = 0; i < S && start + i <= n; i++) {
            if (block[i])
                result++;
        }
    }
    return result;
}
*/

/*
// does not work...
function bench03Test1(loops, n, check) {
	var x = 0, // number of primes below n
		blocksize,
		sieve1, i, j, k, p, start, nsqrt, primes, block, startIdx;

	n /= 2; // compute only up to n/2
	nsqrt = Math.floor(Math.sqrt(n));
	sieve1 = new Array(nsqrt + 1);
	primes = new Array();
	blocksize = 700; //TTT
	block = new Array(blocksize);
	sieve1[0] = 0;
	sieve1[1] = 0;
	while (loops-- > 0 && x === 0) {
		// initialize sieve
		for (i = 2; i <= n; i++) {
			sieve1[i] = 0; // all numbers are possible primes
		}
		// compute primes
		//x++; // 2 is prime
		for (i = 2; i <= nsqrt; i += 1) {
			if (!sieve1[i]) { // not marked -> prime found
				primes.push(i);
				//x++;
				for (j = i * i; j <= nsqrt; j += i) { // mark all multiples of prime as non-prime
					sieve1[j] = 1;
				}
			}
		}

		for (k = 0; k * blocksize <= n; k++) {
			//fill(block.begin(), block.end(), true);
			for (i = 0; i < blocksize; i++) {
				block[i] = 0;
			}

			start = k * blocksize;
			for (i = 0; i < primes.length; i++) {
				p = primes[i];
				startIdx = (start + p - 1) / p;
				j = Math.max(startIdx, p) * p - start;
				for (; j < blocksize; j += p) {
					block[j] = 1;
				}
			}
			if (k === 0) {
				block[0] = 1;
				block[1] = 1;
			}
			for (i = 0; i < blocksize && start + i <= n; i++) {
				if (!block[i]) {
					x++;
				}
			}
		}
		x -= check;
	}
	return x;
}
*/


/*
//still only roughly half the speed of optimized sieve: 264.57

//jump over multiples of 2
//https://cp-algorithms.com/algebra/prime-sieve-linear.html
function bench03(loops, n, check) {
	var x = 0, // number of primes below n
		lp, pr, i, j;

	n /= 2; // compute only up to n/2
	lp = new Array(n + 1); // set the size we need
	pr = new Array();

	while (loops-- > 0 && x === 0) {
		// initialize sieve
		for (i = 2; i <= n; i++) {
			lp[i] = 0; // all numbers are possible primes
		}
		// compute primes
		for (i = 3; i <= n; i += 2) {
			if (!lp[i]) { // not marked -> prime found
				lp[i] = i;
				pr.push(i);
				x++;
			}
			for (j = 0; j < pr.length && pr[j] <= lp[i] && i * pr[j] <= n; j++) {
				lp[i * pr[j]] = pr[j];
			}
		}
		x++; //2 is prime
		x -= check;
	}
	return x;
}
*/


/*
//https://cp-algorithms.com/algebra/prime-sieve-linear.html
function bench03(loops, n, check) {
	var x = 0, // number of primes below n
		lp, pr, i, j;

	n /= 2; // compute only up to n/2
	lp = new Array(n + 1); // set the size we need
	pr = new Array();

	while (loops-- > 0 && x === 0) {
		// initialize sieve
		for (i = 2; i <= n; i++) {
			lp[i] = 0; // all numbers are possible primes
		}
		// compute primes
		for (i = 2; i <= n; i++) {
			if (!lp[i]) { // not marked -> prime found
				lp[i] = i;
				pr.push(i);
				x++;
			}
			for (j = 0; j < pr.length && pr[j] <= lp[i] && i * pr[j] <= n; j++) {
				lp[i * pr[j]] = pr[j];
			}
		}
		x -= check;
	}
	return x;
}
*/


/*
//Can we store only odd numbers? Does not work.
function bench03_odd_experiment(loops, n, check) {
	var x = 0, // number of primes below n
		sieve1, i, j;

	n /= 2; // compute only up to n/2
	sieve1 = new Array((n / 2) + 1); // set the size we need
	sieve1[0] = 0;
	//sieve1[1] = 0;
	while (loops-- > 0 && x === 0) {
		// initialize sieve
		for (i = 2 / 2; i <= n / 2; i++) {
			sieve1[i] = 0; // all odd numbers are possible primes
		}
		// compute primes
		x++; // 2 is prime
		for (i = 1; (i * i) <= n / 2; i += 1) {
			if (!sieve1[i]) { // not marked -> prime found
				x++;
				for (j = (i * i) / 2; j <= n / 2; j += i) { // mark all multiples of prime as non-prime
					sieve1[j] = 1;
				}
			}
		}
		// count remaining primes
		while (i <= n / 2) {
			if (!sieve1[i]) {
				x++;
			}
			i += 1; //2;
		}
		x -= check;
	}
	return x;
}
*/


/*
// blocksize_test: can we cound primes already in the first part?  Does not work?
// https://math.stackexchange.com/questions/111623/is-it-a-bad-idea-to-use-a-sieve-of-eratosthenes-to-find-all-primes-up-to-very-la
function bench03_blocksize_experiment(loops, n, check) {
	var x = 0, // number of primes below n
		BLOCKSIZE, //= 8, //1 << 16,
		numPrimes = 0,
		sieve1, // bool C[BLOCKSIZE];
		primes, // primes to use in the sieve
		first, // next integer divisible by p (delta form block start)
		i, j, B, p, size2, start1,
		aList, s1, f1;

	n = myint(n / 2); // compute only up to n/2

	//BLOCKSIZE = 700; //512;
	BLOCKSIZE = 10;

	aList = primeListDebug(n);
	s1 = Math.floor(Math.sqrt(n)); //TTT
	//var b1 = BLOCKSIZE * myint(s1 / BLOCKSIZE);

	//primes = Array(); // primes to use in the sieve (up to sqrt(n))
	//first = Array(); // next integer divisible by p (delta form block start)

	while (loops-- > 0 && x === 0) {
		numPrimes = 0;
		sieve1 = Array(); //(BLOCKSIZE); // bool C[BLOCKSIZE];
		primes = Array(); // primes to use in the sieve (up to sqrt(n))
		first = Array(); // next integer divisible by p (delta form block start)
		for (i = 3; (i * i) <= n; i += 2) {
			if (!sieve1[i]) {
				primes[numPrimes] = i;
				x++;
				if (aList.indexOf(i) < 0) {
					console.log("DEBUG: Not a prime: " + i);
				}
				//	first[numPrimes] = i * i;
				for (j = i * i; j * j <= n; j += i * 2) { // multiples of prime up to sqrt(n) are not prime
					sieve1[j] = 1;
				}
				f1 = j;
				f1 -= s1;
				first[numPrimes] = f1; //j - s1; //j % BLOCKSIZE; //TTT
				numPrimes++;
			}
		}

		size2 = BLOCKSIZE;
		if (size2 > n) {
			size2 = n + 1;
		}
		//for (B = b1; B < n; B += size2) {
		for (B = s1; B < n; B += size2) {
			if (n - B < size2) {
				size2 = n - B + 1; // remaining
			}

			for (i = 0; i < size2; i++) {
				sieve1[i] = 0;
			}

			for (i = 0; i < numPrimes; i++) {
				p = primes[i];
				j = first[i];
				for (; j < size2; j += p) {
					sieve1[j] = 1;
				}
				f1 = j;
				f1 -= size2;
				first[i] = f1; //j - size2;
			}
			// Block is finished, primes are the integers B+m with C[m]==0; process the primes here

			start1 = 0;
			if (B % 2 === 0) { // even?
				start1 += 1;
			}
			//start1 = 1; //TTT rem1; // we are assuming BLOCKSIZE even!
			for (i = start1; i < size2; i += 2) {
				if (!sieve1[i]) {
					//p = B + i;
					j = B + i;
					if (aList.indexOf(j) < 0) {
						console.log("DEBUG2: Not a prime: " + j);
					}
					x++;
				}
			}
		}
		primes.unshift(2); // 2 is prime
		first.unshift(0); // not used
		x++; // one more prime

		x -= check;
	}
	return x;
}
*/


/*
//ok:
// 318/sec; 356/sec with init += 2; also for first init: 376/sec
function bench03_blocksize2_ok(loops, n, check) {
//function bench03(loops, n, check) {
	var x = 0, // number of primes below n
		blockSize, //= 8, //1 << 16,
		numPrimes = 0,
		sieve1, // bool C[BLOCKSIZE];
		primes, // primes to use in the sieve
		first, // next integer divisible by p (delta form block start)
		i, j, B, p, size2;

	n = myint(n / 2); // compute only up to n/2

	blockSize = iSqrt(n); // must be even!
	if (blockSize >> 1 !== 0) {
		blockSize += 1; // make blocksize even!
	}

	sieve1 = Array(blockSize + 1); //(blockSize); //bool would we enough

	//var aList = primeListDebug(n);

	while (loops-- > 0 && x === 0) {
		numPrimes = 0;
		//sieve1 = Array(blockSize); //(blockSize); //bool would we enough
		for (i = 1; i <= blockSize; i += 2) {
			sieve1[i] = 0;
		}

		primes = Array(); // primes to use in the sieve (up to sqrt(n))
		first = Array(); // next integer divisible by p (delta form block start)
		for (i = 3; (i * i) <= n; i += 2) {
			if (!sieve1[i]) {
				primes[numPrimes] = i;
				first[numPrimes] = i * i;
				//console.log("DDD: prime[" + numPrimes + "]=" + i + " (first=" + i * i + ")");
				numPrimes++;
				for (j = i * i; j * j <= n; j += i * 2) { // multiples of prime up to sqrt(n) are not prime
					sieve1[j] = 1;
					//console.log("DDD: NonPrime: " + j + " (factor " + i + ")");
				}
			}
		}

		size2 = blockSize;
		if (size2 > n) {
			size2 = n;
		}
		//console.log("DDD: phase2: before: size2=" + size2);
		for (B = 0; B < n; B += size2) {
			if (n - B < size2) {
				size2 = n - B; // remaining
			}
			//console.log("DDD: phase2: B=" + B + ", size2=" + size2);

			//start1 = 0;
			//if (B % 2 === 0) { // even?
			//	start1 += 1;
			//}
			//start1 = 1; // we are assuming blockSize even!

			for (i = 1; i <= size2; i += 2) {
				sieve1[i] = 0;
			}
			if (B === 0) {
				//sieve1[0] = 1;
				sieve1[1] = 1; // 0,1 are not prime
				x++; // 2 is prime
				//console.log("DDD: NonPrimes: 0, 1; prime: 2");
			}

			for (i = 0; i < numPrimes; i++) {
				p = primes[i];
				j = first[i];
				for (; j <= size2; j += p) {
					sieve1[j] = 1;
					//console.log("DDD: NonPrime: " + (B + j) + " (factor " + p + ")");
				}
				first[i] = j - size2;
			}
			// Block is finished, primes are the integers B+m with C[m]==0; process the primes here

			for (i = 1; i <= size2; i += 2) {
				if (!sieve1[i]) {
					//p = B + i;
					//console.log("DDD: prime: " + (B + i));

					//if (aList.indexOf(B + i) === -1) {
					//	console.log("DDD: WARNING: not a prime: " + (B + i));
					//}
					x++;
				}
			}
		}
		x -= check;
	}
	return x;
}
*/


/*
// https://math.stackexchange.com/questions/111623/is-it-a-bad-idea-to-use-a-sieve-of-eratosthenes-to-find-all-primes-up-to-very-la
function bench03_blocksize1_ok(loops, n, check) {
	var x = 0, // number of primes below n
		BLOCKSIZE, //= 8, //1 << 16,
		NP = 0,
		C, // bool C[BLOCKSIZE];
		Primes, // primes to use in the sieve
		First, // next integer divisible by p (delta form block start)
		i, j, B, k, p, t, size2;

	n = myint(n / 2); // compute only up to n/2

	//aList = primeListDebug(n);

	BLOCKSIZE = 512;

	while (loops-- > 0 && x === 0) {
		NP = 0;
		C = Array(); //(BLOCKSIZE); // bool C[BLOCKSIZE];
		Primes = Array(); // primes to use in the sieve (up to sqrt(n))
		First = Array(); // next integer divisible by p (delta form block start)
		for (i = 2; (i * i) <= n; i += 1) {
			if (!C[i]) {
				Primes[NP] = i;
				First[NP] = i * i;
				NP++;
				for (j = i * i; j * j <= n; j += i) {
					C[j] = 1;
				}
			}
		}

		//for (B = 0; B < n; B += BLOCKSIZE) {
		B = 0;
		size2 = BLOCKSIZE;
		if (size2 > n) {
			size2 = n + 1; //TTT
		}
		while (B < n) {
			if (n - B < BLOCKSIZE) {
				size2 = n - B + 1; //TTT
				//console.log("DDD: last loop: remaining: " + (size2 - 1));
			}

			for (j = 0; j < size2; j++) {
				C[j] = 0;
			}
			if (B === 0) {
				C[0] = 1;
				C[1] = 1; // 0,1 are not prime
			}

			for (k = 0; k < NP; k++) {
				p = Primes[k];
				t = First[k];
				for (; t < size2; t += p) {
					C[t] = 1;
				}
				First[k] = t - size2;
			}
			// Block is finished, primes are the integers B+m with C[m]==0 //???
			// process the primes here
			for (j = 0; j < size2; j++) {
				if (!C[j]) {
					//console.log("DDD: " + (B + j));
					p = B + j;
					//if (aList.indexOf(p) === -1) {
					//	console.log("DDD: WARNING: not a prime: " + p);
					//}
					x++;
				}
			}
			B += size2;
		}
		x -= check;
	}
	return x;
}
*/


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
			x = a * (x % q) - r * myint(x / q);
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
		pas1, k, i, j, iMod2, min1;

	n = myint(n / 500); // compute only up to n/500

	k = myint(n / 2); // div 2
	if ((n - k) < k) {
		k = n - k; // keep k minimal with  n over k  =  n over n-k
	}

	// allocate memory...
	pas1 = new Array(2);
	pas1[0] = new Array(k + 1);
	pas1[1] = new Array(k + 1);
	pas1[0][0] = 1;
	pas1[1][0] = 1; // now we have two arrays: 2* k+1; first elements set to 1

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
		check;

	switch (bench) {
	case 0:
		//check = myint(n / 2 * ((n % 2) ? -1 : 1)) % 65536; // 41248, assuming n even!
		check = ((n % 2) ? -(n + 1) / 2 : (n / 2)) & 0xffff;
		x = bench00(loops, n, check);
		break;

	case 1:
		//check = myint(n / 2 * ((n % 2) ? -1 : 1)); // assuming n even! (sum should be ...)
		check = (n % 2) ? -(n + 1) / 2 : (n / 2);
		x = bench01(loops, n, check);
		break;

	case 2:
		//check = n / 2.0 * ((n % 2 === 0) ? -1.0 : 1.0); // assuming n even! (sum should be 5.000005E11)
		//checkDouble = (n / 2.0) * (n + 1.0); // assuming n even! (sum should be 5.000005E11)
		//check = checkDouble % 65536;
		//x = bench02(loops, n, checkDouble);
		//check = (n / 2 * ((n % 2) ? -1 : 1)); // assuming n even! (sum should be ...)
		check = (n % 2) ? -(n + 1) / 2 : (n / 2);
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

/*
function strDoubleFormat_ok1(val, digits, prec) {
	var str = "", // buffer for one formatted value
		displPrecAfter = Math.pow(10, 2); // display precision after decimal point

	str += String(Math.round(val * displPrecAfter) / displPrecAfter);

	// integers do not have a dot yet...
	if (str.indexOf(".") < 0) {
		str += ".";
	}

	// format to prec digits after comma, beware of exponential numbers, e.g. 1e+23!
	while ((str.length <= prec) || (str.charAt(str.length - (prec + 1)) !== ".")) {
		str += "0";
	}
	str = strNumFormat(str, digits, " ");
	return str;
}
*/

function strDoubleFormat(val, digits, prec) {
	var str = "", // buffer for one formatted value
		displPrecAfter = Math.pow(10, 2), // display precision after decimal point
		dotPos, count;

	str += String(Math.round(val * displPrecAfter) / displPrecAfter);

	dotPos = str.indexOf(".");
	// integers do not have a dot yet...
	if (dotPos < 0) {
		dotPos = str.length;
		str += ".";
	}

	/*
	// format to prec digits after comma, beware of exponential numbers, e.g. 1e+23!
	while ((str.length <= prec) || (str.charAt(str.length - (prec + 1)) !== ".")) {
		str += "0";
	}
	*/

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
	var fnGetMs = gState.fnGetMs,
		caliMs = gState.caliMs, // 1001
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
		tMeas = fnGetMs();
		x = runBench(bench, loops, n);
		tMeas = fnGetMs() - tMeas;

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
		if (!oArgs.hasOwnProperty || oArgs.hasOwnProperty(sKey)) {
			gState[sKey] = oArgs[sKey];
		}
	}

	gState.startMs = gState.fnGetMs(); // memorize start time

	gState.fnLog(getInfo());

	gState.bench = gState.bench1;
	gState.benchRes = [];

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

function getMsPerformanceNow() {
	return performance.now();
}

function getMsDateNow() {
	return Date.now();
}

function getMsNewDate() {
	return new Date().getTime();
}

if (typeof performance !== "undefined") {
	gState.fnGetMs = getMsPerformanceNow;
} else if (Date.now) {
	gState.fnGetMs = getMsDateNow;
} else {
	gState.fnGetMs = getMsNewDate;
}

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
