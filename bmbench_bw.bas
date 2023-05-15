     REM BM Bench - bmbench.bas (BASIC)
     REM (c) Marco Vieth, 2002-2023
     REM http://www.benchmarko.de
     REM
     REM 06.05.2002  0.01
     REM 18.05.2002  0.02
     REM 24.01.2003  0.05  output format changed
     REM 01.04.2023  0.08  adapted for new version
     REM
     REM Usage (bwbasic):
     REM bwbasic bmbench_bw.bas
     REM
     REM Documentation for bwbasic (Bywater BASIC Interpreter):
     REM https://github.com/nerun/bwbasic/blob/main/DOCS/BYWATER.txt
     REM
     REM Notes
     REM - commands are not case sensitive, variables are
     REM - bwbasic does not require line numbers but we use them here
     REM - bwbasic would also support subroutines
     REM
     REM Notes (bwbasic problems)
     REM - Precision is irrelevant to bwbasic (all computations in double?)
     REM - FOR and NEXT statements must be on single lines?
     REM - RETURN should also on single line
     REM - No comments starting with apostrophe available
     REM - Function definitions: DEF NF<name> cannot be redefined (so do it before Locomotive Basic)
     REM - No comment in line with SYSTEM
     REM - integer division with "\" does rounding
     REM
     REM
     CLEAR
     DEFINT a-z
     prgLanguage$ = "Basic"
     prgVersion$ = "0.08"
     startTs = 0
     maxBench = 6
     bwbasic = 0: REM 1 for bwbasic, 0 for Locomotive Basic
     IF TIMER > 0 THEN bwbasic = 1: GOTO 1000: REM TIMER variable set -> assume bwbasic
     REM Settings for Locomotive Basic
     basicver$ = "Locomotive Basic 1.1"
     DEF FNgetTs(fac!) = TIME - startTs
     DEF FNconvMs!(ts) = ts * 10.0 / 3.0: REM time conversion factor for ms, usually 300 Hz
     startTs = FNgetTs
     GOTO 6000: REM main
     REM Settings for bwbasic
1000 basicver$ = "bwbasic ?"
     DEF FNgetTs = TIMER - startTs
     DEF FNconvMs!(ts) = ts * 1000.0
     startTs = FNgetTs
     GOTO 6000: REM main
     REM
     REM
     REM bench00(n): x
1570 x = 0
     ndiv = INT(n / 65536)
     nmod = (n - ndiv * 65536)
     FOR i = ndiv TO 1 STEP -1
       FOR j = 32767 TO 1 STEP -1
         x = x + j
       NEXT j
       FOR j = -32768 TO -1
         x = x + j
       NEXT j
     NEXT i
     FOR j = nmod TO 1 STEP -1
       x = x + j
     NEXT j
     x = x MOD 65536
     RETURN
     REM
     REM
     REM bench01(n): x
1840 x = 0
     sum = 0
     FOR i = 1 TO n
       sum = sum + i
       IF sum >= n THEN sum = sum - n: x = x + 1
     NEXT i
     RETURN
     REM
     REM
     REM bench02(n): x (Floating Point)
2040 x = 0
     sum! = 0
     FOR i = 1 TO n
       sum! = sum! + i
       IF sum! >= n THEN sum! = sum! - n: x = x + 1
     NEXT i
     RETURN
     REM
     REM
     REM bench03(n): x
2060 nHalf = INT(n / 2)
     REM initialize sieve
     FOR i = 0 TO nHalf: sieve1(i) = 0: NEXT i
     REM compute primes
     i = 0
     m = 3
     x = 1
     WHILE m * m <= n
       IF sieve1(i) = 1 THEN 2070
       x = x + 1
       j = INT((m * m - 3) / 2)
       WHILE j < nHalf
         sieve1(j) = 1
         j = j + m
       WEND
2100   i = i + 1
       m = m + 2
     WEND
     REM count remaining primes
     WHILE m <= n
       IF sieve1(i) = 0 THEN x = x + 1
       i = i + 1
       m = m + 2
     WEND
     RETURN
     REM
     REM
     REM bench04(n): x
2150 m = 2147483647
     a = 16807
     q = 127773
     r = 2836
     x = 1
     FOR i = n TO 1 STEP - 1
       xDivQ = INT(x / q)
       xModQ = x - q * xDivQ
       x = a * xModQ - r * xDivQ
       IF x <= 0 THEN x = x + m
     NEXT i
     RETURN
     REM
     REM
     REM bench05(n): x
2200 b05nSave = n
     n = INT(n / 2)
     k = INT(n / 2)
     IF (n - k) < k THEN k = n - k
     REM initialize (not needed)
     FOR j = 0 TO k: line1(j) = 0: NEXT j
     line1(0) = 1
     IF k >= 1 THEN line1(1) = 2
     REM compute lines of Pascal's triangle
     FOR i = 3 TO n
       min1 = INT((i - 1) / 2)
       IF (i AND 1) = 0 THEN line1(min1 + 1) = 2 * line1(min1)
       prev1 = line1(1)
       FOR j = 2 TO min1
         num1 = line1(j)
         line1(j) = (line1(j) + prev1) AND 65535
         prev1 = num1
       NEXT j
       line1(1) = i
     NEXT i
     REM compute sum of ((n/2)Ck)^2 mod 65536 for k=0..n/2
     x = 0
     FOR j = 0 TO k - 1
       REM x = (x + 2 * line1(j) * line1(j)) MOD 65536
       xHelp = line1(j)
       xHelp = (2.0 * (xHelp * xHelp)) MOD 65536
       x = (x + xHelp) AND 65535
     NEXT j
     REM x = (x + line1(k) * line1(k)) AND 65535
     xHelp = line1(k)
     xHelp = (xHelp * xHelp) MOD 65536
     x = (x + xHelp) AND 65535
     n = b05nSave
     RETURN
     REM
     REM
     REM
     REM bench06(n): x
2240 sum! = 0.0
     flip! = -1.0
     FOR i = 1 TO n
       flip! = flip! * -1.0
       sum! = sum! + flip! / (2 * i - 1)
     NEXT i
     x = ((sum! * 4.0) * 100000000)
     RETURN
     REM
     REM
     REM run_bench(bench, loops, n, check): x
2260 x = 0
     IF bench > maxBench THEN PRINT "Error: Unknown benchmark:"; bench: RETURN
     l = loops
     WHILE l > 0 AND x = 0
       ON bench + 1 GOSUB 1570, 1840, 2040, 2060, 2150, 2200, 2240
       x = x - check
       l = l - 1
     WEND
     x = x + check
     IF x <> check THEN PRINT "Error(bench"; bench ;"): x=";x : x = -1
     RETURN
     REM
     REM
     REM bench03Check(n): x
2280 x = 1
     FOR j = 3 TO n STEP 2
       isPrime = 1
       i = 3
       WHILE (i * i <= j) AND (isPrime = 1)
         IF j MOD i = 0 THEN isPrime = 0
         i = i + 2
       WEND
       if isPrime = 1 Then x = x + 1
     Next j
     RETURN
     REM
     REM
     REM getCheck(bench, n): check
2300 check = -1
     IF bench = 0 THEN check = ((n / 2) * (n + 1)) MOD 65536
     IF bench = 1 OR bench = 2 THEN check = INT((n + 1) / 2)
     IF bench = 3 THEN IF n = 500000 THEN check = 41538 ELSE GOSUB 2280: check = x
     IF bench = 4 THEN IF n = 1000000 THEN check = 1227283347 ELSE GOSUB 2150: check = x
     IF bench = 5 THEN IF n = 5000 THEN check = 17376 ELSE GOSUB 2200: check = x
     IF bench = 6 THEN IF n = 1000000 THEN check = 314159165 ELSE GOSUB 2240: check = x
     IF check = -1 THEN PRINT "Error: Unknown benchmark:"; bench
     RETURN
     REM
     REM
     REM getPrecMs: t0, t1
2320 gtsMeasCnt = 0
     t1 = FNgetTs
     t0 = t1
     WHILE t1 = t0
       t1 = FNgetTs
       gtsMeasCnt = gtsMeasCnt + 1
     WEND
     RETURN
     REM
     REM
     REM correctTime(t0, t1, gtsMeasCnt, gtsPrecCnt): t1Ms!
2380 t1Ms! = FNconvMs!(t1)
     IF gtsMeasCnt < gtsPrecCnt THEN t0MsTmp! = FNconvMs!(t0) + gtsPrecMs! * ((gtsPrecCnt - gtsMeasCnt) * 1.0 / gtsPrecCnt) : IF t0MsTmp! < t1Ms! THEN t1Ms! = t0MsTmp!
     RETURN
     REM
     REM
     REM determineTsPrecision(): {global gtsPrecCnt, gtsPrecMs!}
2400 GOSUB 2320: REM getPrecMs
     t0tmp = t1
     GOSUB 2320: REM getPrecMs
     t1tmp = t1
     gtsPrecMs! = FNconvMs!(t1tmp) - FNconvMs!(t0tmp)
     gtsPrecCnt = gtsMeasCnt
     REM do it again
     t0tmp = t1tmp
     GOSUB 2320: REM getPrecMs
     t1tmp = t1
     IF gtsMeasCnt > gtsPrecCnt THEN gtsPrecCnt = gtsMeasCnt: gtsPrecMs! = FNconvMs!(t1tmp) - FNconvMs!(t0tmp)
     RETURN
     REM
     REM
     REM printInfo(): void
3400 PRINT "BM Bench v"; prgVersion$; " ("; prgLanguage$; ") -- (tsMs:"; gtsPrecMs!; "tsCnt:"; gtsPrecCnt; ") -- "; basicver$
     PRINT "(c) Marco Vieth, 2002-2023"
     RETURN
     REM
     REM
     REM printResult(): void
3500 PRINT: PRINT "Throughput for all benchmarks (loops per sec):"
     PRINT "BMR ("; prgLanguage$; ") :";
     FOR bench = bench1 TO bench2
       PRINT USING "#######.### "; benchres!(bench);
     NEXT bench
     PRINT
     RETURN
     REM
     REM
     REM printLine1(): void
3800 PRINT "Benchmark"; bench; "("; prgLanguage$; "):";
     PRINT ROUND(loopsPsec!, 3); : REM loops per sec
     PRINT "/s (time="; ROUND(tMeas!, 3); "ms, loops="; loops;
     PRINT ", delta="; ROUND(tDelta!, 3); "ms)"
     RETURN
     REM
     REM
     REM measureBench(bench1, bench2, n, check, caliMs, deltaMs): throughput!
4000 maxMs = 10000
     loops = 1
     tEsti! = 0
     throughput! = 0
     PRINT "Calibrating benchmark"; bench; "with loops ="; loops; ", n ="; n; ", check ="; check
     WHILE throughput! = 0
       GOSUB 2320: REM getPrecMs
       t0m = t1
       GOSUB 2260: REM run_bench
       GOSUB 2320: REM getPrecMs
       GOSUB 2380: REM correctTime
       tMeas! = t1Ms! - FNconvMs!(t0m)
       tDelta! = tEsti! - tMeas!
       if tDelta! < 0 THEN tDelta! = -tDelta!
       REM  xx IF tEsti! > tMeas! THEN tDelta! = tEsti! - tMeas! ELSE tDelta! = tMeas! - tEsti!
       loopsPsec! = 0
       IF tMeas! > 0 THEN loopsPsec! = (loops * 1000) / tMeas!
       PRINT USING "######.###";loopsPsec!;
       PRINT "/s (time="; USING "#####.###"; tMeas!;
       PRINT " ms, loops="; USING "#######"; loops;
       PRINT ", delta="; USING "#####.###"; tDelta!;: PRINT " ms)"
       IF x = -1 then throughput! = -1:goto 4100
       IF tMeas! > maxMs THEN PRINT "Benchmark"; bench; "("; prgLanguage$; "): Time already >"; maxMs; " ms. No measurement possible.": throughput! = -loopsPsec!: IF throughput! = 0 THEN throughput! = -1: goto 4100 ELSE 4100
       IF tEsti! > 0 AND tDelta! < deltaMs THEN throughput! = loopsPsec!: GOSUB 3800: GOTO 4100
       IF tMeas! = 0 THEN scaleFact = 50 ELSE if tMeas! < caliMs THEN scaleFact = int((caliMs + 100) / tMeas!) + 1 ELSE scaleFact = 2
       loops = loops * scaleFact
       tEsti! = tMeas! * scaleFact
4100 WEND
     RETURN
     REM
     REM
     REM startBench(bench1, bench2, n): void
5480 GOSUB 2400: REM determineTsPrecision
     GOSUB 3400: REM printInfo
     DIM benchres!(maxBench): REM benchmark timing results
     nSave = n
     FOR bench = bench1 TO bench2
       n = nSave
       IF bench = 3 THEN n = INT(n / 2): DIM sieve1(n / 2 + 1) ELSE IF bench = 5 THEN n = INT(n / 200): DIM line1(n / 4)
       GOSUB 2300: REM getCheck
       throughput! = -1
       IF check > 0 THEN GOSUB 4000: REM measureBench
       benchres!(bench) = throughput!
     NEXT bench
     GOSUB 3500: REM printResult
     RETURN
     REM
     REM
     REM main()
6000 bench1 = 0: REM first benchmark to test
     bench2 = 5: REM last benchmark to test
     n = 1000000: REM maximum number
     IF bwbasic = 0 THEN n = 10000: REM reduce n for Locomotive BASIC
     caliMs = 1001
     deltaMs = 100
     IF COMMAND$(1) <> "" THEN bench1 = VAL(COMMAND$(1))
     IF COMMAND$(2) <> "" THEN bench2 = VAL(COMMAND$(2))
     IF COMMAND$(3) <> "" THEN n = VAL(COMMAND$(3))
     IF COMMAND$(4) <> "" THEN caliMs = VAL(COMMAND$(4))
     IF COMMAND$(5) <> "" THEN deltaMs = VAL(COMMAND$(5))
     GOSUB 5480
     tMeas! = FNconvMs!(FNgetTs)
     PRINT "Total elapsed time:"; tMeas!; "ms"
     REM VARS
     REM SYSTEM
     REM system or quit to exit bwbasic
     END
     REM end
