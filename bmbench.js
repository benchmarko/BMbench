//
// BM Bench - bmbench.js (JavaScript)
// (c) Marco Vieth, 2002
// http://www.benchmarko.de
//
// 06.05.2002  0.01
// 11.05.2002  0.02  bench01 = (sum 1..n) mod 65536 (integer)
// 30.05.2002  0.03
// 18.06.2002  0.031 adapted for standalone JS engines (NSG JS Engine, Rhino, ...)
// 20.07.2002  0.04  some errors corrected
// 24.01.2003  0.05  output format changed
// 04.04.2003        navigator platform corrected; use args[0] as first element (as in Java)
// 18.07.2004        added support for JScript (Windows Scripting Host, cscript.exe)
// 17.01.2006        added support for DMDscript
//
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
// 2. Rhino (JS Engine written in JAVA; http://www.mozilla.org/js)
// Usage: java -jar js.jar bmbench.js (or: java -classpath js.jar org.mozilla.javascript.tools.shell.Main bmbench.js)
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
    var x = 0;
    var sum1 = myint(((n / 2) * (n + 1)) % 65536); // assuming n even! (sum should be ...)
    // do not use '& 0xffff' since this converts sum1 to 0 for stand alone engine
    var n_div_65536 = (n >> 16);
    var n_mod_65536 = (n & 0xffff);
    //window.document.writeln("DEBUG: sum1="+ sum1 +", ndiv="+ n_div_65536 +", nmod="+ n_mod_65536);
    while (loops-- > 0) {
      for (var i = n_div_65536; i > 0; i--) {
        for (var j = 32767; j > 0; j--) {
          x += j;
        }
        //var j; // already declared
        for (j = -32768; j < 0; j++) {
          x += j;
        }
      }
      //var j; // already declared
      for (j = n_mod_65536; j > 0; j--) {
        x += j;
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
  // bench01 (Integer 16/32 bit)
  // (sum of 1..n) mod 65536
  // (Javascript seems to have no Integer arithmetic, so same as bench02...)
  function bench01(loops, n) {
    var x = 0;
    var sum1 = myint(((n / 2) * (n + 1)) % 65536); // assuming n even! (sum should be ...)
    // do not use '& 0xffff' since this converts sum1 to 0 for stand alone engine
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
      //var i; // already declared
      for (i = 2; (i * i) <= n; i++) {
        if (sieve1[i]) {
          for (var j = i * i; j <= n; j += i) {
            sieve1[j] = 0;
          }
        }
      }
      // count primes
      //var i; // already declared
      for (i = 0; i <= n; i++) {
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


function getdate1() {
  var dt = new Date();
  return(dt.toLocaleString());
}


  // Here we compute the number of "significant" bits for positive numbers (which means 53 for double)
  function checkbits_int1() {
    var num = 1;
    var last_num = 0;
    var bits = 0;
    do {
      last_num = num;
      num *= 2;
      num++;
      bits++;
    } while ( (((num - 1) / 2) == last_num) && (bits < 101) );
    return bits;
  }

  function checkbits_double1() {
    var num = 1.0;
    var last_num = 0.0;
    var bits = 0;
    do {
      last_num = num;
      num *= 2.0;
      num++;
      bits++;
      //window.document.writeln("DEBUG: bits="+ bits +", num="+ num);
    } while ( (((num - 1.0) / 2.0) == last_num) && (bits < 101) );
    return bits;
  }


function mynumformat1(val, digits) {
  var str = String(val);
  var spaces = "";
  for (var i = str.length; i < digits; i++) {
    spaces += " ";
  }
  return(spaces + str);
}


// main
function bmbench(args) {
  var start_t = get_ms();  // memorize start time
  var bench1 = 0;          // first benchmark to test
  var bench2 = 5;          // last benchmark to test
  var n = 1000000;         // maximum number
  var min_ms = 10000;      // minimum runtime for measurement in ms

  if (args) {
    if (args.length > 0) {
      bench1 = parseInt(args[0]);
      bench2 = bench1;
    }
    if (args.length > 1) {
      bench2 = parseInt(args[1]);
    }
    if (args.length > 2) {
      n = parseInt(args[2]);
    }
  }

  var bench_res1 = new Array(bench2 - bench1 + 1);

  var win = window.open("","Result","width=640,height=300,resizable=yes,scrollbars=yes,dependent=yes");
  if (win.focus) { win.focus(); }
  win.document.open("text/html", "reuse");

  var js_version = "";
  if (typeof navigator != "undefined") { // only in browsers...
    win.document.writeln("<pre>\n");
    js_version += " appCodeName="+ navigator.appCodeName +", appName="+ navigator.appName
      +", appVersion="+ navigator.appVersion +", platform="+ navigator.platform +", userAgent="+ navigator.userAgent;
  }
  if (typeof System != "undefined") { // NGS JS Engine
    js_version += " Interpreter: "+ System.canonicalHost +", VM.version="+ VM.version;
  }
  if (typeof ScriptEngine != "undefined") { // JScript (Windows Scripting Host) or DMDScript
    js_version += " "+ ScriptEngine() +" Version "+ ScriptEngineMajorVersion() +"."+ ScriptEngineMinorVersion() +"."+ ScriptEngineBuildVersion();
  }
  if (typeof java != "undefined") { // Rhino or Java active?
    if (java.lang.System) {
      var jls = java.lang.System;
      js_version += " java.version="+ jls.getProperty("java.version") +", java.vendor="+ jls.getProperty("java.vendor")
        +"os.name="+ jls.getProperty("os.name") +", os.arch="+ jls.getProperty("os.arch")
        +", os.version="+ jls.getProperty("os.version");
    }
  }

  win.document.writeln("BM Bench v0.5 (JavaScript) -- (int:"+ checkbits_int1() +" double:"+checkbits_double1() +")"+ js_version);
  win.document.writeln("(c) Marco Vieth, 2002");
  win.document.writeln(getdate1());

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
      bench_res1[bench] = myint(t1 * 10 / loops);
      win.document.writeln("Elapsed time for "+ loops +" loops: "+ t1 +" ms; estimation for 10 loops: "+ bench_res1[bench] +" ms\n");
    } else {
      bench_res1[bench] = -1;
    }
  }
  win.document.writeln("Times for all benchmarks (10 loops, ms):");
  var str = "BM Results (JavaScript): ";
  //var bench; // already declared
  for (bench = bench1; bench <= bench2; bench++) {
    str += mynumformat1(bench_res1[bench], 7) +" ";
    // normally we also have win.document.write() but for NGS we need to define it.
  }
  win.document.writeln(str);
  win.document.writeln("Total elapsed time: "+ (get_ms() - start_t) +" ms");
  if (typeof navigator != "undefined") { // only in browsers...
    win.document.writeln("</pre>\n");
  }
  win.document.close();

  return true;
}


function main(args) {
  return bmbench(args);
}

// ---------------------------------------

// simulate window object for stand alone JS engines...

// DMDScript does not like functions inside "if", so define them outside...
  function window_open1() {
    return this;
  }

  function window_writeln1() {
    var str = ""; // does not work: arguments.join(" ");
    for (var i = 0; i < arguments.length; i++) {
      str += arguments[i]; // copy arguments
    }
    switch(window.js_engine1) {
      case 1: // NGS JS Engine
        System.print(str +"\n"); //or: System.stdout.writeln(str);
      break;
      case 2: // Rhino, SpiderMonkey
        print(str);
      break;
      case 3: // DMDScript
        println(str);
      break;
      case 4: // Windows JScript (cscript)
        WScript.Echo(str);
      break;
    }
  }

if (typeof window == "undefined") { // are we outside of a browser in a standalone JS engine?
  window = new Object(); // NGS: how to avoid warning about undefined global 'window'?
  window.open = window_open1;
  window.focus = window_open1; // dummy
  window.document = new Object();
  window.document.open = window_open1; // dummy
  window.document.close = window_open1; // dummy
  window.js_engine1 = 0; // js engine for writeln
  window.document.writeln = window_writeln1;
  window.alert = window.document.writeln; // same as writeln
  if (typeof System != "undefined") { // System object is available with NGS JS Engine
    window.js_engine1 = 1;
    // convert to integer with NGS JS Engine engine, use Math.floor for others...
    eval("myint = int"); // set integer cast; avoid warning 'int' is reserved identifier in browsers
    if (typeof ARGS != "undefined") {
      if ((Math.max(5, 8) != 8) || (Math.pow(0.5, 2) != 0.25)) {
        window.alert("ERROR: Buggy NGS Javascript Engine! Correct b_math.c and try again...");
      } else {
        ARGS.shift(); // remove program name in ARGV[0]
        main(ARGS); // start script
      }
    }
  } else if (typeof arguments != "undefined") { // Rhino, SpiderMonkey, DMDScript...
    if (typeof println != "undefined") { // DMDScript
      window.js_engine1 = 3;
      // Note: arguments for DMDScript do not work.
    } else { // Rhino, SpiderMonkey
      window.js_engine1 = 2;
    }
    main(arguments); // start script
  } else if (typeof WScript != "undefined") { // JScript (cscript)...
    window.js_engine1 = 4;
    var args = new Array(); // copy arguments into array
    for (var i = 0; i < WScript.Arguments.length; i++) {
      args[i] = WScript.Arguments(i);
    }
    main(args);
  } else {
    main(); // unknown engine, call without arguments
  }
}

// end
