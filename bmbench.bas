1 x = (x! - INT(x! / 65536) * 65536):' x mod 65536
100 REM BM Bench - bench001
110 REM (c) Benchmarko, 2002
120 REM 06.05.2002  0.01
122 REM 18.05.2002  0.02
130 REM
135 MODE 2
140 CLEAR:DEFINT a-z
145 tfac! = 10 / 3:' time conversion factor for ms (adapt for Emulators!)
150 GOTO 600:'main
180 '
190 REM bench01(loops, n)
200 x = 0
210 FOR l = 1 TO loops STEP 1
212 FOR i = 1 TO n STEP 1: x = x + i: PRINT x: NEXT :'overflow for 32640+256
214 IF l < loops THEN x = x - (n / 2) * (n + 1) : IF x <> 0 THEN PRINT "Error: bench01: x="; x
218 NEXT
220 'x = x MOD 65536
230 RETURN:'x
235 '
240 REM bench02(loops, n) (Floating Point)
250 x! = 0
260 FOR l = 1 TO loops STEP 1
262 FOR i = 1 TO n STEP 1: x! = x! + i: NEXT
264 IF l < loops THEN x! = x! - (n / 2) * (n + 1) : IF x! <> 0 THEN PRINT "Error: bench01: x="; x!
268 NEXT
270 x = (x! - INT(x! / 65536) * 65536):' x mod 65536
275 RETURN:'x
280 '
290 REM run_bench(bench, loops, n)
300 ON bench GOSUB 200,250
310 RETURN:'x
380 '
390 REM get_ms
400 t! = TIME * tfac!
410 RETURN
580 '
590 REM main()
600 GOSUB 400: startt1! = t!:'get_ms()
605 bench = 2: n = 10000: minms = 10000
610 PRINT "BM Bench v0.1 (Locomotive Basic 1.x)"
620 REM calibrate
630 loops = 1: x = 0: t1! = 0
640 WHILE t1! < 1000:'we want at least 1 sec calibration time
650 PRINT "Calibrating benchmark"; bench; "with loops ="; loops; "; n ="; n
660 GOSUB 400: t1! = t!:'get_ms()
670 GOSUB 300:'x = run_bench(bench, loops, n)
680 GOSUB 400: t1! = t! - t1!:'get_ms()
690 PRINT "x ="; x; " (time="; t1!; ")"
700 loops = loops * 2
710 WEND
720 loops = loops / 2
730 loops = loops * INT(minms / t1!) + 1 :'integer division!
740 PRINT "Calibration done. Starting measurement with "; loops; "loops to get >="; minms; "ms"
750 REM measurement
760 GOSUB 400: t1! = t!:'get_ms()
770 GOSUB 300:'x = run_bench(bench, loops, n)
780 GOSUB 400: t1! = t! - t1!:'get_ms()
790 PRINT "x ="; x; " (time="; t1!; ")"
800 PRINT "Elapsed time for"; loops; "loops:"; t1!; "ms; estimation for 10 loops:"; (t1! * 10 / loops); "ms"
810 GOSUB 400: t1! = t! - startt1!:'get_ms()
840 PRINT "Total elapsed time:"; t1!; "ms"
850 END
860 REM end
