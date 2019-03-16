(* bmbench.p (Pascal)
 * (c) Benchmarko, 2002
 *
 * 06.05.2002  0.01
 * 11.05.2002  0.02  bench1 = (sum 1..n) mod 65536
 * 20.07.2002  0.04  more benchmarks
 *
 * Usage:
 * bmbench [bench1] [bench2] [n]
 *
 *)

(*
 * Compile:
 * gpc bmbench.p -O2 -o bmbench
 * (See: /usr/share/doc/packages/gpc ...)
 *)
 

{$ifdef __GPC__}
{GPC specific compiler options}
{$else}
{BP specific compiler options}
{$endif}


PROGRAM bmbench;

(* uses Gpc, CRT; *)

  VAR argc: SHORTCARD; VAR argv: POINTER;


  (*
   * General description for benchmark test functions
   * benchxx - benchmark
   * <description>
   * in: loops = number of loops
   *         n = maximum number (assumed even, normally n=1000000)
   * out:    x = <output decription>
   *
   * loops may be increased to produce a longer runtime without changing the result.
   *)


  (*
   * bench00 (Integer 16 bit)
   * (sum of 1..n) mod 65536
   *)
  FUNCTION bench00(loops, n: INTEGER): INTEGER;
  VAR x : SHORTCARD;
      sum1 : SHORTCARD;
      n_div_65536, n_mod_65536, i, j : CARDINAL;

  BEGIN
    x := 0;
    sum1 := (n DIV 2) * (n + 1);
    (* (sum1..1000000 depends on type: 500000500000 (floating point), 1784293664 (32bit), 10528 (16 bit) *)
    n_div_65536 := n DIV 65536;
    n_mod_65536 := n MOD 65536;
    (* fprintf(stderr, "Test(bench%d): x=%f, %ld, %ld\n", 1, (double)sum, (long)fmod(sum, 2147483648L), (long)fmod(sum, 65536L)); *)
    WHILE (loops > 0) DO BEGIN
      DEC(loops);
      FOR i := n_div_65536 DOWNTO 1 DO BEGIN
        FOR j := 65536 DOWNTO 1 DO BEGIN
          x := x + j;
        END;
      END;
      FOR j := n_mod_65536 DOWNTO 1 DO BEGIN
        x := x + j;
      END;
      IF (loops > 0) THEN BEGIN (* some more loops left? *)
        x := x - sum1;     (* yes, set x back to 0 (assuming n even) *)
        IF (x <> 0) THEN BEGIN (* now x must be 0 again *)
          INC(x);          (* force error for many wrong computations *)
          RETURN x; (* HALT; *)
        END;
      END;
    END;
    RETURN x;
  END; (* bench00 *)


  (*
   * bench01 (Integer 16/32 bit)
   * (sum of 1..n) mod 65536
   *)
  FUNCTION bench01(loops, n: INTEGER): INTEGER;
  VAR x : INTEGER;
      sum1 : INTEGER;
      i : INTEGER;

  BEGIN
    x := 0;
    sum1 := (n DIV 2) * (n + 1);
    WHILE (loops > 0) DO BEGIN
      DEC(loops);
      FOR i := n DOWNTO 1 DO BEGIN
        x := x + i;
      END;
      IF (loops > 0) THEN BEGIN  (* some more loops left? *)
        x := x - sum1;     (* yes, set x back to 0 (assuming n even) *)
        IF (x <> 0) THEN BEGIN  (* now x must be 0 again *)
          INC(x);          (* force error for many wrong computations *)
          RETURN x MOD 65536; (* HALT; *)
        END;
      END;
    END;
    RETURN x MOD 65536;
  END; (* bench01 *)


   (*
   * bench02 (Floating Point, normally 64 bit)
   * (sum of 1..n) mod 65536
   *)
  FUNCTION bench02(loops, n: INTEGER): INTEGER;
  VAR x : LONGREAL;
      sum1 : LONGREAL;
      i : INTEGER;

  BEGIN
    x := 0.0;
    sum1 := (n / 2.0) * (n + 1.0);
    WHILE (loops > 0) DO BEGIN
      DEC(loops);
      FOR i := n DOWNTO 1 DO BEGIN
        x := x + i;
      END;
      IF (loops > 0) THEN BEGIN (* some more loops left? *)
        x := x - sum1;     (* yes, set x back to 0 (assuming n even) *)
        IF (x <> 0.0) THEN BEGIN (* now x must be 0 again *)
          x := x + 1.0;    (* force error for many wrong computations *)
          RETURN TRUNC(x); (* HALT; *)
        END;
      END;
    END;
    RETURN TRUNC(x - (TRUNC(x / 65536.0) * 65536.0));
  END; (* bench02 *)


  (*
   * bench03 (Integer)
   * number of primes below n (Sieve of Eratosthenes)
   * Example: n=500000 => x=41538 (expected), n=1000000 => x=78498
   *)
  FUNCTION bench03(loops, n_p: INTEGER): INTEGER;
  CONST MAX_N = 500000; (* highest number for sieve div 2 *)
  VAR x : INTEGER;
      n, i, i_mul_i, j : INTEGER;
      sieve1 : PACKED ARRAY[0..MAX_N] OF BOOLEAN;
  BEGIN
    n := n_p DIV 2; (* compute only up to n/2 *)
    IF (n > MAX_N) THEN BEGIN
      Writeln('Error: n too large: ', n);
      RETURN -1; (* error *)
    END;
    x := 0; (* number of primes below n *)
    sieve1[0] := FALSE;
    sieve1[1] := FALSE;
    WHILE (loops > 0) DO BEGIN
      DEC(loops);
      (* initialize sieve *)
      FOR i := 2 TO n DO BEGIN
        sieve1[i] := TRUE;
      END;
      (* compute primes *)
      i := 2;
      i_mul_i := i * i;
      WHILE (i_mul_i <= n) DO BEGIN
        IF (sieve1[i]) THEN BEGIN
          (* FOR j := i_mul_i TO n BY i DO *)
          j := i_mul_i;
          WHILE (j <= n) DO BEGIN
            sieve1[j] := FALSE;
            j := j + i;
          END;
        END;
        INC(i);
        i_mul_i := i * i;
      END;
      (* count primes *)
      FOR i := 0 TO n DO BEGIN
        IF (sieve1[i]) THEN BEGIN
          INC(x);
        END;
      END;
      (* check prime count *)
      IF (loops > 0) THEN BEGIN (* some more loops left? *)
        x := x - 41538;    (* yes, set x back to 0 (assuming n even) *)
        IF (x <> 0) THEN BEGIN  (* now x must be 0 again *)
          INC(x);          (* force error for many wrong computations *)
          RETURN x;
        END;
      END;
    END;
    RETURN x;
  END; (* bench03 *)


  (*
   * bench04 (Integer 32 bit)
   * nth random number number
   * Random number generator taken from
   * Raj Jain: The Art of Computer Systems Performance Analysis, John Wiley & Sons, 1991, page 442-444.
   * It needs longs with at least 32 bit.
   * Starting with x0=1, x10000 should be 1043618065, x1000000 = 1227283347.
   *)
  FUNCTION bench04(loops, n: INTEGER): INTEGER;
    CONST m = 2147483647; (* modulus, do not change! *)
          a = 16807;      (* multiplier *)
          q = 127773;     (* m div a *)
          r = 2836;       (* m mod a *)
    VAR x, i, x_div_q, x_mod_q : INTEGER;
    BEGIN
    x := 1; (* last random value *)
    WHILE (loops > 0) DO BEGIN
      DEC(loops);
      FOR i := n DOWNTO 1 DO BEGIN
        x_div_q := x DIV q;
        x_mod_q := x - q * x_div_q;
        x := a * x_mod_q - r * x_div_q;
        IF (x <= 0) THEN BEGIN
          x := x + m; (* x is new random number *)
        END;
      END;
      IF (loops > 0) THEN BEGIN (* some more loops left? *)
        x := x - 1227283347; (* yes, set x back to 0 (assuming n even) *)
        IF (x <> 0) THEN BEGIN  (* now x must be 0 again *)
          INC(x);          (* force error for many wrong computations *)
          RETURN x; (* HALT; *)
        END;
        INC(x); (* start with 1 again *)
      END;
    END;
    RETURN x;
  END; (* bench04 *)


  (*
   * bench05 (Integer 32 bit)
   * n over n/2 mod 65536 (Pascal's triangle)
   *)
  FUNCTION bench05(loops, n_p: INTEGER): INTEGER;  (* to do!!! *)
  CONST MAX_N = 1000000 DIV (500 * 2); (* highest number for array *)
  VAR x : INTEGER;
      n, k, i, i_mod_2, min1, i_mod_2_1, j : INTEGER;
      pas1 : ARRAY[0..1, 0..MAX_N] OF INTEGER; (* same as ARRAY[0..1] OF ARRAY[0..MAX_N] OF INTEGER; *)

  BEGIN
    x := 0;
    n := n_p DIV 500;
    k := n DIV 2;
    IF ((n - k) < k) THEN BEGIN
      k := n - k; (* keep k minimal with  n over k  =  n over n-k *)
    END;

    IF (k > MAX_N) THEN BEGIN
      Writeln('Error: k too large: ', k);
      RETURN -1; (* error *)
    END;

    pas1[0][0] := 1; pas1[1][0] := 1; (* set first column *)

    WHILE (loops > 0) DO BEGIN
      DEC(loops);
      FOR i := 2 TO n DO BEGIN
        i_mod_2 := i MOD 2;
        min1 := (i - 1) DIV 2;
        IF (k < min1) THEN BEGIN
          min1 := k;
        END;
        i_mod_2_1 := (i + 1) MOD 2;
        pas1[i_mod_2][1] := i; (* second column is i *)
        FOR j := 2 TO min1 DO BEGIN (* up to min((i-1)/2, k) *)
          pas1[i_mod_2][j] := (pas1[i_mod_2_1][j - 1] + pas1[i_mod_2_1][j]);
        END;
        IF ((min1 < k) AND (i_mod_2 = 0)) THEN BEGIN (* new element *)
          pas1[i_mod_2][min1 + 1] := 2 * pas1[i_mod_2_1][min1];
        END;
      END;

      x := x + pas1[n MOD 2][k] MOD 65536; (* % 65536 *)

      IF (loops > 0) THEN BEGIN (* some more loops left? *)
        x := x - 27200;    (* yes, set x back to 0 (assuming n even) *)
        IF (x <> 0) THEN BEGIN  (* now x must be 0 again *)
          INC(x);          (* force error for many wrong computations *)
          RETURN x;
        END;
      END;
    END;
    RETURN x;
  END; (* bench05 *)


 (*
  * run a benchmark
  * in: bench = benchmark to use
  *     loops = number of loops
  *         n = maximum number (used in some benchmarks to define size of workload)
  * out:    x = result
  *)
  FUNCTION run_bench(bench, loops, n: INTEGER) : INTEGER;
  VAR x : INTEGER;
      check1 : INTEGER;

  BEGIN
    x := 0;
    check1 := 0;
    CASE bench OF
      0:  BEGIN
            x := bench00(loops, n);  check1 := 10528;
          END;
      1:  BEGIN
            x := bench01(loops, n);  check1 := 10528;
          END;
      2:  BEGIN
            x := bench02(loops, n);  check1 := 10528;
          END;
      3:  BEGIN
            x := bench03(loops, n);  check1 := 41538;
          END;
      4:  BEGIN
            x := bench04(loops, n);  check1 := 1227283347;
          END;
      5:  BEGIN
            x := bench05(loops, n);  check1 := 27200;
          END;
      ELSE
        WriteLn(StdErr, 'Error: Unknown benchmark: ', bench);
        check1 := x + 1; (* force error *) (* HALT; *)
    END;
    IF (check1 <> x) THEN BEGIN
      WriteLn(StdErr, 'Error(bench', bench, '): x=', x);
      x := -1; (* exit *)
    END;
    RETURN(x);
  END; (* run_bench *)



  PROCEDURE init_ms;
  BEGIN
    (* ResetClock(); *)
  END; (* init_ms *)

 (*
  * get timestamp in milliseconds
  * out: x = time in ms
  *
  * This function is intended for short measurements only so we
  * can return it as an integer.
  *)
  FUNCTION get_ms: INTEGER;
  VAR t : timestamp;
  VAR t2 : INTEGER;
  BEGIN
    gettimestamp(t); (* see procs.pas *)
    (* writeln(time(t)); -- time(t) is string "hh:mm:ss" *)
    t2 := (t.Second + t.Minute * 60 + t.Hour * 3600);
    (* t2 := GetMicroSecondTime; -- where to find ?? *)
    RETURN(t2 * 1000);
  END; (* get_ms *)



  PROCEDURE main(argc: SHORTCARD; VAR argv: POINTER);
    CONST min_ms = 10000;  (* minimum runtime for measurement in ms *)
          MAX_BENCH = 5;

    VAR start_t : INTEGER; (* memorize start time *)
        bench1, bench2 : INTEGER;
        bench : INTEGER;   (* benchmark to test *)
        n : INTEGER;       (* maximum number *)

        loops : INTEGER;   (* number of loops *)
        x : INTEGER;       (* result from benchmark *)
        t1 : INTEGER;      (* timestamp *)
        bench_res1 : ARRAY[0..MAX_BENCH] OF INTEGER;

  BEGIN
    init_ms;
    start_t := get_ms;
    bench1 := 0;
    bench2 := 5;
    n := 1000000;

    IF (argc > 0) THEN BEGIN
      ReadStr(ParamStr(1), bench1);
      bench2 := bench1; (* set also last benchmark *)
    END;
    IF (argc > 1) THEN BEGIN
      ReadStr(ParamStr(2), bench2);
    END;
    IF (argc > 2) THEN BEGIN
      ReadStr(ParamStr(3), n);
    END;

    IF (bench1 > MAX_BENCH) OR (bench2 > MAX_BENCH) THEN BEGIN
      WriteLn('Error: benchmark out of range!');
      HALT; (* error *)
    END;

    WriteLn('BM Bench v0.4 (Pascal)');
    
    FOR bench := bench1 TO bench2 DO BEGIN
      loops := 1;
      x := 0;
      t1 := 0;
      (* calibration *)
      WHILE (t1 < 1001) AND (x <> -1) DO BEGIN (* we want at least 1001 ms calibration time *)
        WriteLn('Calibrating benchmark ', bench, ' with loops=', loops, ', n=', n);
        t1 := get_ms;
        x := run_bench(bench, loops, n);
        t1 := get_ms - t1;
        WriteLn('x=', x, ' (time: ', t1, ' ms)');
        loops := loops * 2;
      END;
      IF (x <> -1) THEN BEGIN
        loops := loops DIV 2;
        loops := loops * (min_ms DIV t1) + 1; (* integer division! *)
        Writeln('Calibration done. Starting measurement with ', loops, ' loops to get >=', min_ms, ' ms');

        (* measurement *)
        t1 := get_ms;
        x := run_bench(bench, loops, n);
        t1 := get_ms - t1;
        WriteLn('x=', x, ' (time: ', t1, ' ms)');

        WriteLn('Elapsed time for ', loops, ' loops: ', t1, ' ms; estimation for 10 loops: ', (t1 * 10 DIV loops), ' ms');
        bench_res1[bench] := t1 * 10 DIV loops;
      END ELSE BEGIN
        bench_res1[bench] := -1;
      END;
    END;

    WriteLn('Summary for 10 Loops:');
    FOR bench := bench1 TO bench2 DO BEGIN
      WriteLn('Benchmark ', bench, ': ', bench_res1[bench], ' ms');
    END;
    WriteLn('Total elapsed time: ', (get_ms - start_t), ' ms');
  END; (* main *)


BEGIN (* program bmbench *)
  main(ParamCount, argv); (* argv not used!! *)
END. (* bmbench *)
