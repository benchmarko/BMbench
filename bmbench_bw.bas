100 REM bmbench_bw.bas (bwbasic)
110 REM (c) Benchmarko, 2002
120 REM 06.05.2002  0.01
122 REM 18.05.2002  0.02
124 REM 26.05.2002  0.03
130 REM
131 REM Usage:
132 REM bwbasic bmbench_bw.bas
133 REM
134 REM Documentation for bwbasic (Bywater BASIC Interpreter):
135 REM /usr/share/doc/packages/bwbasic/bwbasic.doc
136 REM
137 REM Notes (bwbasic problems)
138 REM - Precision is irrelavalt to bwbasic (all computations in double?)
139 REM - FOR and NEXT statements must be on single lines?
140 REM - RETURN should also on single line
141 REM - No comments starting with ' available
142 REM
143 REM MODE 2
144 DEFINT a-z: REM irrelevant to bwbasic
145 tfac! = 1000: REM time conversion factor for ms (adapt?)
150 GOTO 600: REM main
180 REM
190 REM bench01(loops, n)
200 x = 0
205 sum1 = (n / 2) * (n + 1)
208 l = loops
210 WHILE l > 0: l = l - 1
212 FOR i = n TO 1 STEP -1
213 x = x + i
214 NEXT i
215 REM (overflow for 32640+256?)
216 IF l > 0 THEN x = x - sum1: IF x <> 0 THEN RETURN: REM Error
218 WEND
220 x = x MOD 65536
230 RETURN
231 REM returns x
235 REM
240 REM bench02(loops, n) (Floating Point)
250 x! = 0
255 sum1! = (n / 2) * (n + 1)
258 l = loops
260 WHILE l > 0: l = l - 1
262 FOR i = n TO 1 STEP -1
263 x! = x! + i
264 NEXT i
265 IF l > 0 THEN x! = x! - sum1!: IF x! <> 0 THEN x = x!: RETURN: REM Error
268 WEND
270 x = (x! - INT(x! / 65536) * 65536): REM x mod 65536
275 RETURN
276 REM returns x
280 REM
290 REM run_bench(bench, loops, n)
300 x = 0: check1 = 0
301 IF bench > 2 THEN GOTO 320
302 ON bench GOSUB 200,250
303 check1 = x + 1
304 IF bench = 1 THEN check1 = 10528
305 IF bench = 2 THEN check1 = 10528
310 GOTO 330
320 PRINT "Error: Unknown benchmark:"; bench
330 IF x <> check1 THEN PRINT "Error(bench"; bench ;"): x=";x: STOP
350 RETURN
351 REM returns x
380 REM
390 REM get_ms
400 t! = TIMER * tfac!
410 RETURN
580 REM
590 REM main()
600 GOSUB 400: startt1! = t!: REM get_ms()
605 bench = 2: n = 1000000: minms = 10000
610 PRINT "BM Bench v0.3 (bwBasic)"
620 REM calibrate
630 loops = 1: x = 0: t1! = 0
640 WHILE t1! < 1000: REM we want at least 1 sec calibration time
650 PRINT "Calibrating benchmark"; bench; " with loops ="; loops; "; n ="; n
660 GOSUB 400: t1! = t!: REM get_ms()
670 GOSUB 300: REM x = run_bench(bench, loops, n)
680 GOSUB 400: t1! = t! - t1!: REM get_ms()
690 PRINT "x ="; x; " (time="; t1!; " ms)"
700 loops = loops * 2
710 WEND
720 loops = loops / 2
730 loops = loops * INT(minms / t1!) + 1: REM integer division!
740 PRINT "Calibration done. Starting measurement with"; loops; " loops to get >="; minms; " ms"
750 REM measurement
760 GOSUB 400: t1! = t!: REM get_ms()
770 GOSUB 300: REM x = run_bench(bench, loops, n)
780 GOSUB 400: t1! = t! - t1!: REM get_ms()
790 PRINT "x ="; x; " (time="; t1!; " ms)"
800 PRINT "Elapsed time for"; loops; " loops:"; t1!; " ms; estimation for 10 loops:"; INT(t1! * 10 / loops); " ms"
810 GOSUB 400: t1! = t! - startt1!: REM get_ms()
840 PRINT "Total elapsed time:"; t1!; " ms"
850 SYSTEM
851 REM system or quit exit bwbasic
856 END
860 REM end