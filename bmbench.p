(*
 * BM Bench - bmbench.p (Pascal)
 * (c) Marco Vieth, 2002-2022
 * http://www.benchmarko.de
 *
 * 06.05.2002 0.01
 * 11.05.2002 0.02  bench1 = (sum 1..n) mod 65536
 * 20.07.2002 0.04  more benchmarks
 * 24.01.2003 0.05  output format changed
 * 13.07.2019 0.07  changed bench 01-03; time interval estimation
 * 03.12.2022 0.072 bench03 corrected, bench05 improved
 * 25.02.2023 0.08  bench05 optimized
 *
 * Usage:
 * bmbench [bench1] [bench2] [n]
 *
 *)

(*
 * Usage (fpc):
 * fpc.exe -obmbench_fpc.exe bmbench.p
 *
 * Usage (gpc):
 * gpc bmbench.p -O2 -o bmbench
 * (See: /usr/share/doc/packages/gpc, /usr/lib/gcc-lib/i486-suse-linux/2.95.3/units ...)
 *
 * Test with --classic-pascal --extended-pascal --object-pascal or --borland-pascal; -pascal-sc
 * --no-delphi-comments
 *
 * Usage (p2c): (to do!)
 * p2c -LHP -a -o bmbench_p2c.c bmbench.p
 * gcc -O2 -Wall -Wtraditional bmbench_p2c.c -lp2c -o bmbench_p2c
 *
 * Usage (Borland Delphi): (for Delphi 2.0 and maybe others, line separator must be Windows CRLF!)
 * dcc32.exe -obmbench_fpc.exe bmbench.p  (use -CC, if no  {$APPTYPE CONSOLE} )
 *
 *
 * unit Dos;   procedure GetTime (var Hour, Minute, Second, Sec100: Word);
 *
 *)


PROGRAM bmbench (Input, Output);
{$APPTYPE CONSOLE}

(* uses Gpc, CRT; *)

{IntegerWidth=1 ## set printf for p2c}

{$ifdef __GPC__}
  {GPC specific compiler options}
  {$define WORD SHORTCARD}
  (* define PASCAL_VERSION_XXX CONCAT('gpc ', 'to do: __GPC_RELEASE__') *) (* gpc: set macro PASCAL_VERSION *)
  CONST PASCAL_VERSION = CONCAT('gpc ', __GPC_RELEASE__);
{$else}
  {$ifdef FPC}
    uses SysUtils; (* Now *)
    CONST PASCAL_VERSION = CONCAT('FPC ', {$I %FPCVERSION%});
  {$else}
    (* uses SysUtils; runtime error when using Now *) (* Now *)
    {$define _BP}  (* we assume Borland Pascal/Delphi *)
    {$ifdef VER90} (* example, see: http://docwiki.embarcadero.com/RADStudio/XE6/en/Compiler_Versions *)
      CONST PASCAL_VERSION = 'Borland Delphi 2.0';
	{$else}
	  CONST PASCAL_VERSION = 'Unknown';
    {$endif}
  {$endif}
{$endif}

  CONST g_prg_version = '0.08';
  CONST g_prg_language = 'Pascal';
  CONST MAX_BENCH = 6;

  VAR argc: INTEGER;
  VAR argv: POINTER;

  VAR g_startTs, g_tsPrecMs : DOUBLE;
  VAR g_tsPrecCnt, g_tsMeasCnt, g_cali_ms : LONGINT;

  CONST BENCH05_MAX_N = 1000000 DIV (200 DIV 2 * 2); (* highest number for bench05 array *)
  TYPE bench05LineType = ARRAY[0..BENCH05_MAX_N] OF INTEGER;
  VAR bench05Line: bench05LineType;


  (*
   * General description for benchmark test functions
   * benchxx - benchmark
   * <description>
   * in: loops = number of loops
   *         n = maximum number (assumed even, normally n=1000000)
   *     check = expected value for x
   * out:    x = <output value>
   *
   * loops may be increased to produce a longer runtime without changing the result.
   *)


  (*
   * bench00 (Integer 16 bit)
   * (sum of 1..n) mod 65536
   *)
  FUNCTION bench00(n: LONGINT): INTEGER;
  VAR x : INTEGER; (* usually 16 bit *)
      n_div_65536, n_mod_65536 : INTEGER;
	    i, j : INTEGER;

  BEGIN
    x := 0;
    n_div_65536 := n DIV 65536;
    n_mod_65536 := n MOD 65536;
    (* fprintf(stderr, "Test(bench%d): x=%f, %ld, %ld\n", 1, (double)sum, (long)fmod(sum, 2147483648L), (long)fmod(sum, 65536L)); *)
    FOR i := n_div_65536 DOWNTO 1 DO BEGIN
      FOR j := 32767 DOWNTO 1 DO BEGIN
        x := x + j;
      END;
    FOR j := -32768 TO -1 DO BEGIN
        x := x + j;
      END;
    END;
    FOR j := n_mod_65536 DOWNTO 1 DO BEGIN
      x := x + j;
    END;
	  x := x MOD 65536;
    bench00 := x;
  END; (* bench00 *)


  (*
   * bench01 (Integer 16/32 bit)
   * (arithmetic mean of 1..n) mod 65536
   *)
  FUNCTION bench01(n: LONGINT): INTEGER;
  VAR x : INTEGER;
      i, sum : LONGINT;

  BEGIN
    x := 0;
	  sum := 0;
    FOR i := 1 TO n DO BEGIN
      sum := sum + i;
      IF (sum >= n) THEN BEGIN (* to avoid numbers above 2*n, divide by n using subtraction *)
        sum := sum - n;
        x := x + 1;
      END;
    END;
    bench01 := x MOD 65536;
  END; (* bench01 *)


  (*
   * bench02 (Floating Point, normally 64 bit)
   * (arithmetic mean of 1..n) mod 65536
   *)
  FUNCTION bench02(n: LONGINT): INTEGER;
  VAR x : INTEGER;
      sum : DOUBLE;
      i : LONGINT;

  BEGIN
    x := 0;
	  sum := 0.0;
    FOR i := 1 TO n DO BEGIN
      sum := sum + i;
      IF (sum >= n) THEN BEGIN (* to avoid numbers above 2*n, divide by n using subtraction *)
        sum := sum - n;
        x := x + 1;
      END;
    END;
    bench02 := x MOD 65536; (* ?? TRUNC(x - (TRUNC(x / 65536.0) * 65536.0)); *)
  END; (* bench02 *)


  (*
   * bench03 (Integer)
   * number of primes below n (Sieve of Eratosthenes)
   * Example: n=500000 => x=41538 (expected), n=1000000 => x=78498
   *)
  FUNCTION bench03(n: LONGINT): INTEGER;
  CONST MAX_N = 500000; (* highest number for half sieve div 2 *)
  VAR x : INTEGER;
      nHalf, i, m, j : LONGINT;
      sieve1 : PACKED ARRAY[0..MAX_N+1] OF BOOLEAN;
  LABEL bench03_exit;
  BEGIN
    (* n := n DIV 2; compute only up to n/2 *)
	  nHalf := n DIV 2;
    IF (nHalf + 1 > MAX_N) THEN BEGIN
      Writeln('Error: n too large: ', n);
      x := -1; (* error *)
      GOTO bench03_exit;
    END;

    (* initialize sieve *)
    FOR i := 0 TO nHalf DO BEGIN
      sieve1[i] := FALSE;
    END;

    (* compute primes *)
    i := 0;
    m := 3;
    x := 1; (* number of primes below n, 2 is prime *)
    WHILE (m * m <= n) DO BEGIN
      IF (NOT sieve1[i]) THEN BEGIN
        x := x + 1; (* m is prime *)
        j := (m * m - 3) DIV 2;
        WHILE (j < nHalf) DO BEGIN
          sieve1[j] := TRUE;
          j := j + m;
        END;
      END;
      i := i + 1; (* INC(i) *)
      m := m + 2;
    END;

    (* count remaining primes *)
    WHILE (m <= n) DO BEGIN
      IF (NOT sieve1[i]) THEN BEGIN
        x := x + 1; (* m is prime *)
      END;
      i := i + 1;
      m := m + 2;
    END;

    bench03_exit:
    bench03 := x;
  END; (* bench03 *)


  (*
   * bench04 (Integer 32 bit)
   * nth random number number
   * Random number generator taken from
   * Raj Jain: The Art of Computer Systems Performance Analysis, John Wiley & Sons, 1991, page 442-444.
   * It needs longs with at least 32 bit.
   * Starting with x0=1, x10000 should be 1043618065, x1000000 = 1227283347.
   *)
  FUNCTION bench04(n: LONGINT): INTEGER;
  CONST m = 2147483647; (* modulus, do not change! *)
        a = 16807;      (* multiplier *)
        q = 127773;     (* m div a *)
        r = 2836;       (* m mod a *)
  VAR x, i : LONGINT;
  BEGIN
    x := 1; (* last random value *)
    FOR i := n DOWNTO 1 DO BEGIN
      (* x_div_q := x DIV q; *)
      (* x_mod_q := x - q * x_div_q; *)
      (* x := a * x_mod_q - r * x_div_q; *)
      x := a * (x MOD q) - r * (x DIV q);
      IF (x <= 0) THEN BEGIN
        x := x + m; (* x is new random number *)
      END;
      (* Writeln('DEBUG: i=', i, ' x=', x); *)
    END;
	  x := x AND $ffff;
    bench04 := x;
  END; (* bench04 *)


  (*
   * bench05 (Integer 32 bit)
   * n over n/2 mod 65536 (Pascal's triangle)
   *)
FUNCTION bench05(n_p: LONGINT): INTEGER;
  TYPE linePtrType = ^bench05LineType;
  VAR x : INTEGER;
      n, k, i, min1, prev, num, j : INTEGER;
      linePtr : linePtrType;
  LABEL bench05_exit;

  BEGIN
    (* Instead of nCk with k=n/2, we compute the product of (n/2)Ck with k=0..n/4 *)
    n := n_p DIV 2;

    k := n DIV 2;
    IF ((n - k) < k) THEN BEGIN
      k := n - k; (* keep k minimal with  n over k  =  n over n-k *)
    END;

    IF (k > BENCH05_MAX_N) THEN BEGIN
      Writeln('Error: k too large: ', k);
      x:= -1; (* error *)
      GOTO bench05_exit;
    END;

    linePtr := @bench05Line;

    (* initialize *)
    FOR j := 1 TO k DO BEGIN
      linePtr^[j] := 0;
    END;

    linePtr^[0] := 1;
    IF (k >= 1) THEN BEGIN
      linePtr^[1] := 2; (* for line 2, second column is 2 *)
    END;

    (* compute *)
    FOR i := 3 TO n DO BEGIN
      min1 := (i - 1) DIV 2;
      IF ((i AND 1) = 0) THEN BEGIN (* new element *)
        linePtr^[min1 + 1] := 2 * linePtr^[min1];
      END;

      prev := linePtr^[1];
      FOR j := 2 TO min1 DO BEGIN
        num := linePtr^[j];
        linePtr^[j] := (linePtr^[j] + prev); (* And $ffff; *)
        prev := num;
      END;
      linePtr^[1] := i; (* second column is i *)
    END;

    x := 0;
    FOR j := 0 TO k - 1 DO BEGIN
      x := (x + 2 * linePtr^[j] * linePtr^[j]) And $ffff;
    END;
    x := (x + linePtr^[k] * linePtr^[k]) And $ffff;
    bench05_exit:
    bench05 := x;
  END; (* bench05 *)


  FUNCTION bench06(n: LONGINT): INTEGER;
  VAR sum : DOUBLE;
      flip: DOUBLE;
      i : LONGINT;

  BEGIN
	  sum := 0.0;
    flip := -1.0;
    FOR i := 1 TO n DO BEGIN
      flip := flip * -1.0;
      sum := sum + flip / (2*i - 1);
    END;
    bench06 := Trunc((sum * 4.0) * 100000000) MOD 65536;
  END; (* bench06 *)


 (*
  * bench03Check
  * check function (used for n <> 1000000)
  *)
  FUNCTION bench03Check(n: LONGINT) : INTEGER;
  VAR x, j, i : INTEGER;
  VAR isPrime: BOOLEAN;

  BEGIN
    IF n = 500000 THEN x := INTEGER(41538)
    ELSE BEGIN
      x := 0;
      j := 3;
      WHILE j <= n DO BEGIN
        isPrime := TRUE;
        i := 3;
        WHILE (i * i <= j) And (isPrime = true) DO BEGIN
          IF (j And i) = 0 THEN isPrime := FALSE;
          i := i + 2;
        END;
        if isPrime = TRUE then x := x + 1;
        j := j + 2;
      END;
    END;
    bench03Check := x;
  END; (* bench03Check *)



 (*
  * run a benchmark
  * in: bench = benchmark to use
  *     loops = number of loops
  *         n = maximum number (used in some benchmarks to define size of workload)
  * out:    x = result
  *)
  FUNCTION run_bench(bench: INTEGER; loops: LONGINT; n: LONGINT; check: INTEGER) : INTEGER;
  VAR x : INTEGER;

  BEGIN
    x := 0;

    WHILE ((loops > 0) AND (x = 0)) DO BEGIN
      CASE bench OF
        0:  BEGIN
              x := bench00(n);
            END;
        1:  BEGIN
              x := bench01(n);
            END;
        2:  BEGIN
              x := bench02(n);
            END;
        3:  BEGIN
              x := bench03(n);
            END;
        4:  BEGIN
              x := bench04(n);
            END;
        5:  BEGIN
              x := bench05(n);
            END;
        6:  BEGIN
              x := bench06(n);
            END;
        ELSE
          WriteLn('Error: Unknown benchmark: ', bench);  (* StdErr is gpc extension, so don't use it *)
          x := -1;
      END;
      loops := loops - 1;
	    x := x - check;
    END;

	  x := x + check;
    IF (x <> check) THEN BEGIN
      WriteLn('Error(bench', bench, '): x=', x);
      x := -1; (* exit *)
    END;
    run_bench := x;
  END; (* run_bench *)


  FUNCTION get_check(bench, n: LONGINT) : INTEGER;
  VAR check : INTEGER;

  BEGIN
    CASE bench OF
      0:  BEGIN
            check := ((n DIV 2) * (n + 1)) MOD 65536; (* SHORTINT(...) or: AND $ffff *)
          END;
      1:  BEGIN
            check := ((n + 1) DIV 2) AND $ffff;
            (*
            IF (check < 0) THEN BEGIN
              check := check + 32768;
            END
            *)
          END;
      2:  BEGIN
            check := INTEGER(((n + 1) DIV 2) AND $ffff);
            (*
            IF (check < 0) THEN BEGIN
              check := check + 32768;
            END
            *)
          END;
      3:  BEGIN
            check := bench03Check(n);
          END;
      4:  BEGIN
            IF n = 1000000 THEN check := INTEGER(1227283347 AND $ffff)
            ELSE check := bench04(n);
          END;
      5:  BEGIN
            IF n = 5000 THEN check := 17376
            ELSE check := bench05(n);
          END;
      6:  BEGIN
            IF n = 1000000 THEN check := INTEGER(314159165 AND $ffff)
            ELSE check := bench06(n);
          END;
      ELSE
        WriteLn('Error: Unknown benchmark: ', bench);  (* StdErr is gpc extension, so don't use it *)
        check := -1;
    END;
    (* WriteLn('DDD: check=', check); *)
    get_check := check;
  END;


  PROCEDURE init_ms;
  BEGIN
    (* ResetClock(); *)
  END; (* init_ms *)


{$ifdef __GPC__}
  {$ifdef _COMMENT}
  TYPE TIMESTAMP = PACKED RECORD
      DateValid,
      TimeValid  : Boolean;
      Year       : Integer;
      Month      : 1 .. 12;
      Day        : 1 .. 31;
      DayOfWeek  : 0 .. 6;  (* 0 means Sunday *)
      Hour       : 0 .. 23;
      Minute     : 0 .. 59;
      Second     : 0 .. 61; (* to allow for leap seconds *)
      MicroSecond: 0 .. 999999;
      TimeZone   : Integer; (* in seconds *)
      DST        : Boolean;
      TZName1, TZName2 : String(32)  (* String(32)?? *)
    END;
    (* The fields `DateValid', `TimeValid', `Year', `Month', `Day', `Hour', `Minute', `Second'
    * are required by Extended Pascal, the other ones are extensions. *)
  {$endif}


  FUNCTION get_raw_ts: DOUBLE;
  VAR t : TIMESTAMP;
      t2 : LONGINT;
  BEGIN
    gettimestamp(t); (* see procs.pas *) (* ISO-10206 Extended Pascal extension *)
    (* writeln(time(t)); -- time(t) is string "hh:mm:ss" *)
    t2 := (t.Second + t.Minute * 60 + t.Hour * 3600);
    (* t2 := GetMicroSecondTime; -- where to find ?? *)
    get_raw_ts := t2 * 1000 + (t.MicroSecond / 1000);  (* MicroSecond is gpc extension *)
    (* WriteLn('DEBUG: get_raw_ts=', t2 * 1000 + (t.MicroSecond DIV 1000)); *)
  END; (* get_raw_ts *)
  (* Also available in gpc.pas: GetCPUTime(var MicroSecond: Integer): Integer *)


  PROCEDURE getdate1;
  VAR t : TIMESTAMP;
  BEGIN
    gettimestamp(t);
    (* we could use Date(t), Time(t) to print the date now, but... *)
    (* Write(t.Day:2, ':', t.Month:2, ':', t.Year:4, ' ', t.Hour:2, ':', t.Minute:2, ':', t.Second:2); *)
    Write(t.Day, ':', t.Month, ':', t.Year, ' ', t.Hour, ':', t.Minute, ':', t.Second); (* not formatted right *)
  END;

{$endif}


{$ifdef FPC}
  FUNCTION get_raw_ts: DOUBLE;
  BEGIN
    get_raw_ts := Now * SecsPerDay * 1000.0; (* Now: from sysutils; todayInMilliseconds; https://stackoverflow.com/questions/29790895/get-current-time-milliseconds *)
  END; (* get_raw_ts *)

  PROCEDURE getdate1;
  BEGIN
    (* Write(t.Day, ':', t.Month, ':', t.Year, ' ', t.Hour, ':', t.Minute, ':', t.Second); * not formatted right *)
	Write(DateTimeToStr(Now));
  END;
{$endif}


{$ifdef _COMMENT_BP}
  FUNCTION get_raw_ts: DOUBLE;
  VAR t1 : DOUBLE;
  BEGIN
    (* get_raw_ts := Now * SecsPerDay * 1000.0; *) (* Now: from sysutils; todayInMilliseconds; https://stackoverflow.com/questions/29790895/get-current-time-milliseconds *)
	get_raw_ts := 100 * 1000.0; (* TTT *)
  END; (* get_raw_ts *)

  PROCEDURE getdate1;
  BEGIN
    (* Write(t.Day, ':', t.Month, ':', t.Year, ' ', t.Hour, ':', t.Minute, ':', t.Second); * not formatted right *)
  END;
{$endif}


{$ifdef _BP}
  VAR last_time : DOUBLE;
  FUNCTION get_raw_ts: DOUBLE;
  BEGIN
    (* t1 := Now * SecsPerDay * 1000.0; *) (* Now: from sysutils; todayInMilliseconds; https://stackoverflow.com/questions/29790895/get-current-time-milliseconds *)
	last_time := last_time + 20000; (* simulate 20 seconds more *)
	get_raw_ts := last_time;
  END; (* get_raw_ts *)

  PROCEDURE getdate1;
  BEGIN
    (* Write(t.Day, ':', t.Month, ':', t.Year, ' ', t.Hour, ':', t.Minute, ':', t.Second); * not formatted right *)
  END;
{$endif}


  (* get timestamp since program start *)
  FUNCTION get_ts : DOUBLE;
  BEGIN
    get_ts := get_raw_ts() - g_startTs;
  END;

  FUNCTION conv_ms(ts: DOUBLE) : DOUBLE;
  BEGIN
    conv_ms := ts;
  END;


  FUNCTION correctTime(tMeas, tMeas2: DOUBLE; measCount: LONGINT) : DOUBLE;
  VAR tsPrecCnt : LONGINT;
  BEGIN
    tsPrecCnt := g_tsPrecCnt;
    IF (measCount < tsPrecCnt) THEN BEGIN
      tMeas := tMeas + g_tsPrecMs * ((tsPrecCnt - measCount) / tsPrecCnt); (* ts + correction *)
      IF (tMeas > tMeas2) THEN BEGIN
        tMeas := tMeas2; (* cannot correct *)
	  END;
    END;
    correctTime := tMeas;
  END;

  FUNCTION getPrecMs(stopFlg: BOOLEAN) : DOUBLE;
  VAR measCount : LONGINT;
      tMeas0, tMeas : DOUBLE;
	  tMeasD : DOUBLE;
  BEGIN
    measCount := 0;
    tMeas0 := get_ts();
    tMeas := tMeas0;
    WHILE (tMeas <= tMeas0) DO BEGIN
      tMeas := get_ts();
      measCount := measCount + 1;
    END;
    g_tsMeasCnt := measCount; (* memorize count *)
    (* Console.WriteLine("DEBUG: getPrecMs: measCount=" + measCount + " ts=" + tMeas); *)

    (* for stop: use first ts + correction *)
	IF stopFlg THEN BEGIN
      tMeasD := correctTime(conv_ms(tMeas0), conv_ms(tMeas), measCount);
    END ELSE BEGIN
      tMeasD := conv_ms(tMeas);
    END;
    getPrecMs := tMeasD;
  END;

  (* usually only needed if time precision is low, e.g. one second *)
  PROCEDURE determineTsPrecision;
  VAR tMeas0, tMeas1 : DOUBLE;
  BEGIN
    g_startTs := get_raw_ts(); (* memorize start time *)

    tMeas0 := getPrecMs(FALSE);
    tMeas1 := getPrecMs(FALSE);
    g_tsPrecMs := tMeas1 - tMeas0;
    g_tsPrecCnt := g_tsMeasCnt;

    (* do it again *)
    tMeas0 := tMeas1;
    tMeas1 := getPrecMs(FALSE);
    IF (g_tsMeasCnt > g_tsPrecCnt) THEN BEGIN  (* take maximum count *)
      g_tsPrecCnt := g_tsMeasCnt;
      g_tsPrecMs := tMeas1 - tMeas0;
    END;
  END;


  (* Here we compute the number of "significant" bits for positive numbers (which means 53 for double) *)
  FUNCTION checkbits_short1 : INTEGER;
  VAR num, last_num : SHORTINT;
  VAR bits : INTEGER;
  BEGIN
    {test range check off: $R-} {TEST}
    num := 1;
    last_num := 0;
    bits := 0;

    REPEAT
      last_num := num;
      num := num * 2;
      num := num + 1;
      bits := bits + 1;
    UNTIL ( (((num - 1) DIV 2) <> last_num) OR (bits >= 101) );
    checkbits_short1 := bits;
  END; (* checkbits_short1 *)


  FUNCTION checkbits_int1 : INTEGER;
  VAR num, last_num : INTEGER;
  VAR bits : INTEGER;
  BEGIN
    num := 1;
    last_num := 0;
    bits := 0;
    REPEAT
      last_num := num;
      num := num * 2;
      num := num + 1;
      bits := bits + 1;
    UNTIL ( (((num - 1) DIV 2) <> last_num) OR (bits >= 101) );
    checkbits_int1 := bits;
  END; (* checkbits_int1 *)

  FUNCTION checkbits_longint1 : INTEGER;
  VAR num, last_num : LONGINT;
  VAR bits : INTEGER;
  BEGIN
    num := 1;
    last_num := 0;
    bits := 0;
    REPEAT
      last_num := num;
      num := num * 2;
      num := num + 1;
      bits := bits + 1;
    UNTIL ( (((num - 1) DIV 2) <> last_num) OR (bits >= 101) );
    checkbits_longint1 := bits;
  END; (* checkbits_longint1 *)

  FUNCTION checkbits_float1 : INTEGER;
  VAR num, last_num : SINGLE;
  VAR bits : INTEGER;
  BEGIN
    num := 1.0;
    last_num := 0.0;
    bits := 0;
    REPEAT
      last_num := num;
      num := num * 2.0;
      num := num + 1.0;
      bits := bits + 1;
    UNTIL ( (((num - 1.0) / 2.0) <> last_num) OR (bits >= 101) );
    checkbits_float1 := bits;
  END; (* checkbits_float1 *)


  FUNCTION checkbits_double1 : INTEGER;
  VAR num, last_num : DOUBLE;
  VAR bits : INTEGER;
  BEGIN
    num := 1.0;
    last_num := 0.0;
    bits := 0;
    REPEAT
      last_num := num;
      num := num * 2.0;
      num := num + 1.0;
      bits := bits + 1;
    UNTIL ( (((num - 1.0) / 2.0) <> last_num) OR (bits >= 101) );
    checkbits_double1 := bits;
  END; (* checkbits_double1 *)


  PROCEDURE print_info;
  BEGIN
    WriteLn('BM Bench v', g_prg_version, ' (', g_prg_language, ') -- (short:', checkbits_short1, ' int:', checkbits_int1,
      ' longint:', checkbits_longint1, ' float:', checkbits_float1, ' double:', checkbits_double1,
      ' tsMs:', g_tsPrecMs, ' tsCnt:', g_tsPrecCnt, ') version ', PASCAL_VERSION);
    WriteLn('(c) Marco Vieth, 2002-2023');
    Write('Date: '); getdate1; WriteLn;
  END;

  PROCEDURE print_results(bench1, bench2: INTEGER; bench_res1: ARRAY OF DOUBLE);
  (* Dim max_language_len1 As Integer = 10 *)
  VAR bench : INTEGER;
  BEGIN
    WriteLn;
    WriteLn('Throughput for all benchmarks (loops per sec):');
	  Write('BMR (', g_prg_language:-10, ')');

    FOR bench := bench1 TO bench2 DO BEGIN
      (* str += String.Format(nfi, "{0,9:F3} ", bench_res1(bench)) *)
	  Write(bench_res1[bench]:9:3, ' ');
    END;
	WriteLn;
    (* System.Console.WriteLine(str) *)
	(*
		old:
		WriteLn('Times for all benchmarks (10 loops, ms):');
    Write('BM Results (Pascal)    : ');
    FOR bench := bench1 TO bench2 DO BEGIN
      Write(bench_res1[bench]:7, ' ');
    END;
    WriteLn;
	*)
  END;


  FUNCTION measureBench(bench: INTEGER; n: LONGINT; check: INTEGER) : DOUBLE;
  VAR cali_ms : INTEGER;
      delta_ms : INTEGER;
      max_ms : INTEGER;
      loops : LONGINT; (* number of loops *)
      x : INTEGER; (* result from benchmark *)
      tMeas : DOUBLE; (* measured time *)
      tEsti : DOUBLE; (* estimated time *)
      throughput : DOUBLE;
	    t_delta : DOUBLE;
	    loops_p_sec : DOUBLE;
	    scale_fact : LONGINT;

  BEGIN
    cali_ms := g_cali_ms; (* 1001 *)
    delta_ms := 100;
    max_ms := 10000;
    loops := 1;
    tMeas := 0;
    tEsti := 0;
    throughput := 0;

	  WriteLn('Calibrating benchmark ', bench, ' with n=', n, ', check=', check);
	  WHILE (throughput = 0) DO BEGIN
	    tMeas := getPrecMs(false);
      x := run_bench(bench, loops, n,check);
      tMeas := getPrecMs(true) - tMeas;

      IF (tEsti > tMeas) THEN BEGIN (* compute difference abs(measures-estimated) *)
        t_Delta := tEsti - tMeas;
      END ELSE BEGIN
        t_Delta := tMeas - tEsti;
      END;

      IF (tMeas > 0) THEN BEGIN
        loops_p_sec := loops * 1000.0 / tMeas;
      END ELSE BEGIN
        loops_p_sec := 0;
      END;

      WriteLn(loops_p_sec:10:3, '/s (time=', tMeas:9:3, ' ms, loops=', loops:7, ', delta=', t_delta:9:3, ' ms, x=', x);

      If (x = -1) THEN BEGIN (* some error? *)
          throughput := -1;
        END ELSE IF (tEsti > 0) And (t_delta < delta_ms) THEN BEGIN (* do we have some estimated/expected time smaller than delta_ms=100? *)
          throughput := loops_p_sec; (* yeah, set measured loops per sec *)
          WriteLn('Benchmark ', bench, ' (', g_prg_language, '): ', loops_p_sec:10:3, '/s (time=', tMeas:9:3, ' ms, loops=', loops, ' , delta=', t_delta:9:3, ' ms)');
        END ELSE IF (tMeas > max_ms) THEN BEGIN
          WriteLn('Benchmark ', bench, ' (', g_prg_language, '): Time already > ', max_ms, ' ms. No measurement possible.');
          IF (loops_p_sec > 0) THEN BEGIN
            throughput := -loops_p_sec; (* cannot rely on measurement, so set to negative *)
          END ELSE BEGIN
            throughput := -1;
          END
        END ELSE BEGIN
          IF (tMeas = 0) THEN BEGIN
            scale_fact := 50;
          END ELSE IF (tMeas < cali_ms) THEN BEGIN
            scale_fact := TRUNC(((cali_ms + 100) / tMeas) + 1); (* for Integer *)
              (* scale a bit up to 1100 ms (cali_ms+100) *)
          END ELSE BEGIN
            scale_fact := 2;
        END;
        (* WriteLn('DEBUG: loops=', loops, ' scale_fact=', scale_fact); *)
        loops := loops * scale_fact;
        tEsti := tMeas * scale_fact;
      END;
	  END;
	  measureBench := throughput;
  END;


  PROCEDURE start_bench(bench1, bench2: INTEGER; n: LONGINT);
  VAR bench : INTEGER;
      bench_res : ARRAY[0..MAX_BENCH] OF DOUBLE;
      check: INTEGER;
	    throughput : DOUBLE;
      n2 : LONGINT;
  BEGIN
    print_info();
    FOR bench := bench1 TO bench2 DO BEGIN
      n2 := n;

      IF (bench = 3) THEN BEGIN
        n2 := n2 DIV 2
      END ELSE IF (bench = 5) THEN BEGIN
        n2 := n2 DIV 200;
      END;

      check := get_check(bench, n2);
      IF check <> 0 THEN BEGIN
	      throughput := measureBench(bench, n2, check);
      END ELSE BEGIN
        throughput := -1;
      END;
      bench_res[bench] := throughput;
    END;
    print_results(bench1, bench2, bench_res);
  END;


  PROCEDURE main(argc: INTEGER; VAR argv: POINTER);
  VAR bench1, bench2 : INTEGER;
      n : LONGINT;
	    err_code : INTEGER;
  BEGIN
    bench1 := 0;
	  bench2 := 5;
    n := 1000000;

    g_cali_ms := 1001;

	  err_code := 0;
	  IF (argc > 0) THEN BEGIN
      Val(ParamStr(1), bench1, err_code);  (* ReadStr(ParamStr(1), bench1); *)
      bench2 := bench1; (* set also last benchmark *)
    END;
    IF (argc > 1) THEN BEGIN
      Val(ParamStr(2), bench2, err_code); (* ReadStr(ParamStr(2), bench2); *)
    END;
    IF (argc > 2) THEN BEGIN
      Val(ParamStr(3), n, err_code); (* ReadStr(ParamStr(3), n); *)
    END;
    IF (argc > 3) THEN BEGIN
      Val(ParamStr(4), g_cali_ms, err_code); (* ReadStr(ParamStr(4), n); *)
    END;

	  IF (err_code <> 0) THEN BEGIN
      WriteLn('Error: parameter parsing error: ', err_code);
    END;

    IF (bench1 > MAX_BENCH) OR (bench2 > MAX_BENCH) THEN BEGIN
      WriteLn('Error: benchmark out of range!');
      HALT; (* error *)
    END;

	  determineTsPrecision();
    start_bench(bench1, bench2, n);
    WriteLn('Total elapsed time: ', TRUNC(conv_ms(get_ts())), ' ms');
  END;


BEGIN (* program bmbench *)
  argc := ParamCount;
  main(argc, argv); (* argv not used!! *)
END. (* bmbench *)
