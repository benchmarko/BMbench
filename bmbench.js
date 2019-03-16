// bmbench.js
// (c) Benchmarko, 2002
//
// 06.05.2002  0.01
// 11.05.2002  0.02  bench01 = (sum 1..n) mod 65536 (integer)
// 30.05.2002  0.03
// 18.06.2002  0.031 adapted for standalone JS engines (NSG JS Engine, Rhino, ...)
// 20.07.2002  0.04  some errors corrected

//
// usage:
// bmbench([bench1], [bench2], [n])  (from HTML)
//

// Notes:
// Stand alone NGS JS Engine:
// Usage: js bmbench.js [bench1] [bench2] [n]  (or use compiler: js -c -O2 bmbench.js, js bmbench.jsc)
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
// Rhino (JS Engine from Mozilla)
// Usage: java -jar js.jar bmbench.js (or: java -cp js.jar org.mozilla.javascript.tools.shell.Main bmbench.js)
// Compiler: [export CLASSPATH=js.jar]
//           java -cp js.jar org.mozilla.javascript.tools.jsc.Main -opt 0 bmbench.js
//           java -cp js.jar bmbench
//
//

var myint = Math.floor; // cast to integer


  //
  // General description for benchmark test functions
  // benchxx - benchmark
  // <description>
  // in: loops = number of loops
  //         n = maximum number (assumed even, normally n=1000000)
  // out:    x = <output decription>
  //
  // loops may be increased to produce a longer runtime without changing the result.
  //


  //
  // bench00 (Integer 16 bit)
  // (sum of 1..n) mod 65536
  //
  function bench00(loops, n) {
    //System.out.println("Benchmark 0 not available.");
    return 0;
  }

  //
  // bench01 (Integer 16/32 bit)
  // (sum of 1..n) mod 65536
  // (Javascript seems to have no Integer arithmetic, so same as bench02...)
  function bench01(loops, n) {
    var x = 0;
    var sum1 = myint(((n / 2) * (n + 1)) % 65536); // assuming n even! (sum should be ...)
    // do not use '& 0xffff' since this converts sum1 to 0 for stand alonw engine
    while (loops-- > 0) {
      for (var i = n; i > 0; i--) {
        x += i;
      }
      if (loops > 0) {   // some more loops left?
        x %= 65536;      // (do not use &= 0xffff)
        x -= sum1;       // yes, set x back to 0
        if (x != 0) {    // now x must be 0 again
          x++;           // force error for many wrong computations
          break;         // Error   //alert("Error(bench01): x="+ x);
        }
      }
    }
    //if (isInt(x)) { System.stdout.writeln("Yes, it is integer!"); }
    return x % 65536;
  }


  //
  // bench02 (Floating Point, normally 64 bit)
  // (sum of 1..n) mod 65536
  //
  function bench02(loops, n) {
    var x = 0.0;
    var sum1 = (n / 2.0) * (n + 1.0); // assuming n even! (sum should be 5.000005E11)
    while (loops-- > 0) {
      for (var i = n; i > 0; i--) {
        x += i;
      }
      if (loops > 0) {   // some more loops left?
        x -= sum1;       // yes, set x back to 0
        if (x != 0.0) {    // now x must be 0 again
          x++;
          break;         // Error   //alert("Error(bench01): x="+ x);
        }
      }
    }
    return x % 65536;
  }


  //
  // bench03 (Integer)
  // number of primes below n (Sieve of Eratosthenes)
  // Example: n=500000 => x=41538 (expected), n=1000000 => x=78498
  // (No bit array in Javascript available, String is also not cheaper, so...)
  function bench03(loops, n) {
    n >>= 1; // compute only up to n/2

    var x = 0; // number of primes below n
    var sieve1 = new Array(n + 1); // boolean[n + 1];??
    sieve1[0] = 0;
    sieve1[1] = 0;
    while (loops-- > 0) {
      // initialize sieve
      for (var i = 2; i <= n; i++) {
        sieve1[i] = 1;
      }
      // compute primes
      for (var i = 2; (i * i) <= n; i++) {
        if (sieve1[i]) {
          for (var j = i * i; j <= n; j += i) {
            sieve1[j] = 0;
          }
        }
      }
      // count primes
      for (var i = 0; i <= n; i++) {
        if (sieve1[i]) {
          x++;
        }
      }
      // check prime count
      if (loops > 0) {  // some more loops left?
        x -= 41538;     // yes, set x back to 0 (number of primes below 1000000)
        if (x != 0) {   // now x must be 0 again
          x++;
          break;        // Error
        }
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
  function bench04(loops, n) {
    var m = 2147483647; // modulus, do not change!
    var a = 16807;      // multiplier
    var q = 127773;     // m div a
    var r = 2836;       // m mod a
    var x = 1;                // last random value
    while (loops-- > 0) {
      for (var i = n; i > 0; i--) {
        var x_div_q = myint(x / q); // Math.floor(x / q);
        //if (isInt(x_div_q)) { System.stdout.writeln("Yes, it is integer!"); }
        var x_mod_q = x - q * x_div_q;
        x = a * x_mod_q - r * x_div_q;
        if (x <= 0) {
          x += m; // x is new random number
        }
      }
      if (loops > 0) {
        x -= 1227283347;
        if (x != 0) {   // now x must be 0 again
          x++;
          break;        // Error
        }
        x++; // start with 1 again
      }
    }
    return x;
  }


  //
  // bench05 (Integer 32 bit)
  // n over n/2 mod 65536 (Pascal's triangle)
  //
  function bench05(loops, n) {
    var x = 0;
    n = myint(n / 500);
    var k = n >> 1; // div 2

    if ((n - k) < k) {
      k = n - k; // keep k minimal with  n over k  =  n over n-k
    }

    // allocate memory...
    var pas1 = new Array(2);
    pas1[0] = new Array(k + 1);
    pas1[1] = new Array(k + 1);
    pas1[0][0] = 1; pas1[1][0] = 1; // set first column

    while (loops-- > 0) {
      for (var i = 3; i <= n; i++) {
        var i_mod_2 = i & 1;
        var i1_mod_2 = i_mod_2 ^ 1;
        var min1 = (i_mod_2 == 0) ? ((i - 2) >> 1) : ((i - 1) >> 1); // Math.floor((i - 1) / 2);
        if (k < min1) {
          min1 = k;
        }
        pas1[i_mod_2][1] = i; // second column is i
        for (var j = 2; j <= min1; j++) { // up to min((i-1)/2, k)
          pas1[i_mod_2][j] = (pas1[i_mod_2 ^ 1][j - 1] + pas1[i_mod_2 ^ 1][j]) & 0xffff; // % 65536 -- we need mod here to avoid overflow
        }
        if ((min1 < k) && (i_mod_2 == 0)) { // new element
          //pas1[i_mod_2][Math.floor(i / 2)] = 2 * pas1[i_mod_2 ^ 1][Math.floor((i - 1) / 2)];
          pas1[i_mod_2][min1 + 1] = 2 * pas1[i_mod_2 ^ 1][min1];
        }
      }
      x += pas1[n & 1][k] & 0xffff; // % 65536
      if (loops > 0) {
        x -= 27200;
        if (x != 0) {   // now x must be 0 again
          x++;
          break;        // Error
        }
      }
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
  function run_bench(bench, loops, n) {
    var x = 0;
    var check1 = 0;
    switch(bench) {
      case 0:
        x = bench00(loops, n);
        check1 = 10528;
      break;

      case 1:
        x = bench01(loops, n);
        check1 = 10528;
      break;

      case 2:
        x = bench02(loops, n);
        check1 = 10528;
      break;

      case 3:
        x = bench03(loops, n);
        check1 = 41538;
      break;

      case 4:
        x = bench04(loops, n);
        check1 = 1227283347;
      break;

      case 5:
        x = bench05(loops, n);
        check1 = 27200;
      break;

      default:
        window.alert("Error: Unknown benchmark "+ bench);
        check1 = x + 1;
      break;
    }
    if (check1 != x) {
      window.alert("Error(bench"+ bench +"): x="+ x);
      x = -1; //exit;
    }
    return(x);
  }


//
// get timestamp in milliseconds
// out: x = time in ms
//
// This function is intended for short measurements only so we
// can return it as an integer.
//
function get_ms() {
  var d1 = new Date();
  return(d1.getTime());
}


// main
function bmbench(args) {
  var start_t = get_ms();  // memorize start time
  var bench1 = 1;          // first benchmark to test
  var bench2 = 5;          // last benchmark to test
  var n = 1000000;         // maximum number
  var min_ms = 10000;      // minimum runtime for measurement in ms

  if (args) {
    if (args.length > 1) {
      bench1 = parseInt(args[1]);
      bench2 = bench1;
    }
    if (args.length > 2) {
      bench2 = parseInt(args[2]);
    }
    if (args.length > 3) {
      n = parseInt(args[3]);
    }
  }

  var bench_res1 = new Array(bench2 - bench1 + 1);

  var win = window.open("","Result","width=640,height=300,resizable=yes,scrollbars=yes,dependent=yes");
  if (win.focus) { win.focus(); }
  win.document.open("text/html", "reuse");
  win.document.writeln("<pre>\n");

  win.document.writeln("BM Bench v0.4 (JavaScript)");
  if (typeof navigator != "undefined") { // only in browsers...
    win.document.writeln("appCodeName="+ navigator.appCodeName, "+ appName="+ navigator.appName, "+ appVersion="+ navigator.appVersion +", platform="+ navigator.plattform);
    win.document.writeln("userAgent="+ navigator.userAgent);
  }
  if (typeof System != "undefined") { // NGS JS Engine
    win.document.writeln("Interpreter: "+ System.canonicalHost +", VM.version="+ VM.version);
  }
  if (typeof java != "undefined") { // Rhino or Java active?
    if (java.lang.System) {
      var jls = java.lang.System;
      win.document.writeln("java.version="+ jls.getProperty("java.version") +", java.vendor="+ jls.getProperty("java.vendor"));
      win.document.writeln("os.name="+ jls.getProperty("os.name") +", os.arch="+ jls.getProperty("os.arch")
      +", os.version="+ jls.getProperty("os.version"));
    }
  }

  for (var bench = bench1; bench <= bench2; bench++) {
    var loops = 1; // number of loops
    var x = 0;     // result from benchmark
    var t1 = 0;    // timestamp
    // calibration
    while (t1 < 1001) { // we want at least 1 sec calibration time
      win.document.writeln("Calibrating benchmark "+ bench +" with loops="+ loops +", n="+ n);
      t1 = get_ms();
      x = run_bench(bench, loops, n);
      t1 = get_ms() - t1;
      win.document.writeln("x="+ x +" (time: "+ t1 +" ms)");
      loops *= 2;
      if (x == -1) {
        break;
      }
    }
    if (x != -1) {
      loops >>= 1; // div 2
      loops *= myint(min_ms / t1) + 1; // integer division!
      win.document.writeln("Calibration done. Starting measurement with "+ loops +" loops to get >="+ min_ms +" ms");

      // measurement
      t1 = get_ms();
      x = run_bench(bench, loops, n);
      t1 = get_ms() - t1;
      win.document.writeln("x="+ x +" (time: "+ t1 +" ms)");
      win.document.writeln("Elapsed time for "+ loops +" loops: "+ t1 +" ms; estimation for 10 loops: "+ myint(t1 * 10 / loops) +" ms\n");
      bench_res1[bench] = myint(t1 * 10 / loops);
    } else {
      bench_res1[bench] = -1;
    }
  }
  win.document.writeln("Summary for 10 Loops:");
  for (var bench = bench1; bench <= bench2; bench++) {
    win.document.writeln("Benchmark "+ bench +": "+ bench_res1[bench] +" ms");
  }
  win.document.writeln("Total elapsed time: "+ (get_ms() - start_t) +" ms");
  win.document.writeln("</pre>\n");
  win.document.close();

  return true;
}


function main(args) {
  return bmbench(args);
}

// ---------------------------------------

// simulate window object for stand alone JS engines...
if (typeof window == "undefined") { // are we outside of a browser in a standalone JS engine?
  function window_open1() {
    return window;
  }

  function window_dummy1() {
  }

  function window_writeln_system1() { // for NSG JS Engine
    var str = ""; // does not work: arguments.join(" ");
    for (var i = 0; i < arguments.length; i++) {
      str += arguments[i]; // copy arguments
    }
    //System.stdout.writeln(str);
    System.print(str +"\n");
  }

  function window_writeln_print1() { // for Rhino
    var str = ""; // does not work: arguments.join(" ");
    for (var i = 0; i < arguments.length; i++) {
      str += arguments[i]; // copy arguments
    }
    print(str);
  }

  window = new Object(); // how to avoid warining about undefined global 'window'?
  window.open = window_open1;
  //window.focus = window_dummy1;
  window.document = new Object();
  window.document.open = window_dummy1;
  window.document.close = window_dummy1;
  if (typeof System != "undefined") { // System object is available with NGS JS Engine
    window.document.writeln = window_writeln_system1;
    // convert to integer with standalone engine, use Math.floor for others...
    eval("myint = int"); // set integer cast; avoid warning 'int' is reserved identifier in browsers
    window.alert = window_writeln_system1; //or: System.error(str +"\n"), System.stderr.writeln();
    if (typeof ARGS != "undefined") {
      //if ((Math.max(5, 8) != 8) || (Math.pow(0.5, 2) != 0.25)) {
      //  window.alert("ERROR: Buggy NGS Javascript Engine! Correct b_math.c and try again...");
      //} else {
        main(ARGS); // start script
      //}
    }
  } else { // Rhino, ...
    window.document.writeln = window_writeln_print1;
    if (typeof java != "undefined") {
      window.alert = java.lang.System.out.println;
    }
    if (typeof arguments != "undefined") {
      arguments.unshift("0"); // unshift dummy argument
      main(arguments); // start script
    }
  }
}

// end
