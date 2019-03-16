option explicit 'force variable declarations
' bmbench.c (StarOffice Basic)
' (c) Benchmarko, 2002
'
' 06.05.2002  0.01
' 11.05.2002  0.02  bench1 = (sum1 1..n) mod 65536
'
' usage:
' bmbench [bench] [n]
'

'Notes:
'- Help: Runtime functions
'- Recursive subs, functions not possible
'

'
' General description for benchmark test functions
' benchxx - benchmark
' <description>
' in: loops = number of loops
'         n = maximum number (assumed even, normally n=1000000)
' out:    x = <output decription>
'
' loops may be increased to produce a longer runtime without changing the result.
'
'

'StarBasic starts with first sub...
sub bmbench()
  Dim bench1, bench2 As Integer
  Dim n As Long 'maximum number
  bench1 = 2
  bench2 = 2
  n = 1000000
  main(bench1, bench2, n)
end sub



'
' bench00 (Integer 16 bit) - DOES NOT WORK YET!
' (sum1 of 1..n) mod 65536
'
Function bench00(ByVal loops, ByVal n) As Long
  Dim i, j As Long
  Dim x As Long
  Dim sum1 as long
  Dim n_div_65536 as Long
  Dim n_mod_65536 as Long
  Dim sum1_d as Double
  x = 0
  sum1_d = ((n / 2) * (n + 1))
  sum1 = (sum1_d - (int(sum1_d / 65536.0) * 65536.0))
  ' (sum11..1000000 depends on type: 500000500000 (floating point), 1784293664 (32bit), 10528 (16 bit)
  While (loops > 0)
    loops = loops - 1
    For i = n_div_65536 To 1 step -1
      For j = 65535 To 0 step -1
        x = x + i 'overflow???
      Next J
    Next i
    For j = n_mod_65536 To 0 step -1
      x = x + j
    Next j
    If (loops > 0) Then
      x = x - sum1
      If (x <> 0) Then
        bench00 = x Mod 65536
        Exit Function
      End If
    End If
  Wend
  bench00 = x Mod 65536
End Function


'
' bench01 (Integer 16/32 bit) - DOES NOT WORK YET!
' (sum1 of 1..n) mod 65536
'
Function bench01(ByVal loops, ByVal n) As Long
  Dim i As Long
  Dim x As Long
  Dim sum1 as long

  On Local Error Goto ErrorHandler
  
  x = 0

  bench01 = 0
  Exit function ' test
  
  sum1 = (n / 2) * (n + 1) ' overflow!
  While (loops > 0)
    loops = loops - 1
    For i = n To 1 step -1
      x = x + i 'overflow???
    Next i
    If (loops > 0) Then
      x = x - sum1
      If (x <> 0) Then
        bench01 = x Mod 65536
        Exit Function
      End If
    End If
  Wend
  bench01 = x Mod 65536
  Exit Function
  
ErrorHandler:
  Msgbox "Error " & err & "(" & error & ") in line" & erl
  stop
  
End Function


'
' bench02 (Floating Point, normally 64 bit)
' (sum1 of 1..n) mod 65536
'
Function bench02(ByVal loops, ByVal n) As Long
  Dim i As Long
  Dim x As Double
  Dim sum1 as Double
  
  x = 0
  sum1 = (n / 2) * (n + 1)
  While (loops > 0)
    loops = loops - 1
    For i = n To 1 step -1
      x = x + i
    Next i
    If (loops > 0) Then
      x = x - sum1
      If (x <> 0) Then
        bench02 = (x - (int(x / 65536.0) * 65536.0)) 'x Mod 65536
        Exit Function
      End If
    End If
  Wend
  bench02 = (x - (int(x / 65536.0) * 65536.0)) 'x Mod 65536
End Function


'
' bench03 (Integer) - DOES NOT WORK YET!
' number of primes below n (Sieve of Eratosthenes)
' Example: n=500000 => x=41538 (expected), n=1000000 => x=78498
'
Function bench03(ByVal loops, ByVal n) As Long
  Dim i, j As Long
  Dim x As Long
  Dim max_n As Long
  'Const chunk1 = 8191 '8192 - 1
  'Const chunk1_2 = 8192
  Dim chunk1 as integer
  chunk1 = 7
  Dim chunk1_2 as integer
  chunk1_2 = 8
  
  n = n / 2 'compute only up to n/2
  x = 0 'number of primes below n
  'Dim sieve1(n + 1) as boolean 'max elements only 32767! (index only up to 16363?!)
  'also this limit for 2 dimensions??
  n = n / 8 ' test!
  MsgBox "bench03: limited! n=" & n
  Dim sieve1(chunk1, int(n / chunk1_2) + 1) as boolean 'boolean occupies 2 bytes...
  'MsgBox "DEBUG: x=" & chunk1 & ", y=" & int((n + 1) / chunk1_2) + 1

  'sieve1(0) = false
  sieve1(0 and chunk1, int(0 / chunk1_2)) = false
  'sieve1(1) = false
  sieve1(1 and chunk1, int(1 / chunk1_2)) = false
  while (loops > 0)
    loops = loops - 1
    'initialize sieve
    For i = 2 to n step 1
      'sieve1(i) = true
      'if ((i and chunk1) >= Ubound(sieve1(), 1)) then
      ' MsgBox "DEBUG: Ubound 1=" & Ubound(sieve1())
      'endif
      'if (int(i / chunk1_2) >= Ubound(sieve1(), 2)) then
      '  MsgBox "DEBUG: Ubound 2=" & Ubound(sieve1(), 2)
      'endif
      sieve1(i and chunk1, int(i / chunk1_2)) = true
    next i
    'compute primes
    max_n = sqr(n)
    For i = 2 to max_n step 1
      'if (sieve1(i) = true) then
      if (sieve1(i and chunk1, int(i / chunk1_2)) = true) then
        For j = i * i to n step i
          'sieve1(j) = false
          sieve1(j and chunk1, int(j / chunk1_2)) = false
        next j
      endif
    next i
    'count primes
    For i = 0 to n step 1
      'if (sieve1(i) = true) then
      if (sieve1(i and chunk1, int(i / chunk1_2)) = true) then
        x = x + 1
      endif
    next i
    'check prime count
    if (loops > 0) then 'some more loops left?
      x = x - 41538 'yes, set x back to 0 (number of primes below 1000000)
      if (x <> 0) then 'now x must be 0 again
        Exit Function 'Error
      Endif
    EndIf
  Wend
  bench03 = x
End Function
  

'
' run a benchmark
' in: bench = benchmark to use
'     loops = number of loops
'         n = maximum number (used in some benchmarks to define size of workload)
' out:    x = result
'
Function run_bench(bench, loops, n) As Long
  Dim check1 as Long
  run_bench = 0
  check1 = 0
  Select Case bench
    Case "0"
      run_bench = bench00(loops, n)
      check1 = 10528  
    Case "1"
      run_bench = bench01(loops, n)
      check1 = 10528
    Case "2"
      run_bench = bench02(loops, n)
      check1 = 10528
    Case "3"
      run_bench = bench03(loops, n)
      check1 = 41538
    Case "4"
      'run_bench = bench04(loops, n)
      check1 = 1227283347     
    Case "5"
      'run_bench = bench05(loops, n)
      check1 = 27200 'or 58336 or 43584
    Case Else
      'MsgBox "Error: Unknown benchmark: " & bench
      print "Error: Unknown benchmark: " & bench
      check1 = run_bench + 1
  End Select
  if run_bench <> check1 then
    'MsgBox "Error(bench" & bench & "): x=" & run_bench 
    print "Error(bench" & bench & "): x=" & run_bench 
    run_bench = -1
  endif
End Function

'
' get timestamp in milliseconds
' out: x = time in ms
'
' if (g_calibrate = 0) then
'
Function get_ms() As Long
  get_ms = Timer * 1000 'return ms since midnight
End Function

'does not work this way...
Function get_ms_t1() As Long
  Static g_calibrate As Long
  if (g_calibrate = 0) then
    g_calibrate = 1
    'try calibration...
    'wait 1000 'wait for next second
    'g_calibrate = GetSystemTicks() 'depends on system, normally ms
    'wait 1000 'wait 1 second
    'g_calibrate = GetSystemTicks() - g_calibrate 'now we have ticks for 1 second
    'g_calibrate = g_calibrate / 1000 'ticks for 1 ms
  endif
  get_ms_t1 = GetSystemTicks() / g_calibrate
End Function



Sub Main(bench1, bench2, n)
  Dim start_t As Long
  start_t = get_ms()
  Dim bench As Integer 'benchmark to test
  'Dim n As Long 'maximum number
  Dim min_ms As Integer 'minimum runtime for measurement in ms
  Dim bench_res1(bench2 + 1)
  Dim msg1$
  Dim out_file As String
  Dim iNumber As Integer
  'Dim GuiTypes$(4) = "Windows", "Presentation Manager", "Macintosh", "Motif", "Open Window"

  out_file = "bm1_out1.txt"
  iNumber = Freefile
  Open out_file For Output As #iNumber

  'n = 100000 'test
  min_ms = 10000
  msg1$ = ""
  
  'MsgBox "DEBUG: start_t = " & start_t
  msg1$ = msg1$ + "BM Bench v0.3 (StarOffice Basic)"+ Chr(13)
  msg1$ = msg1$ + "GUIType="+ GetGUIType + " (" + choose(GetGUIType, "Windows", "Presentation Manager", "Macintosh", "Motif", "Open Window") +"), GUIVersion="+ GetGUIVersion + ", SystemType="+ GetSystemType + Chr(13)

  for bench = bench1 to bench2 step 1
    Dim loops As Integer 'number of loops
    Dim x As Long 'result from benchmark
    Dim t1 As Long 'timestamp
    loops = 1
    x = 0
    t1 = 0

    'Calibration
    While (t1 < 1001) and (x <> -1) 'we want at least 1 sec calibration time
      msg1$ = msg1$ + "Calibrating benchmark "+ bench +" with loops="+ loops +", n="+ n + Chr(13)
      t1 = get_ms()
      x = run_bench(bench, loops, n)
      t1 = get_ms() - t1
      msg1$ = msg1$ + "x="+ x +" (time: "+ t1 +" ms)"+ Chr(13)
      loops = loops * 2
    Wend
    if (x <> -1) then
      loops = loops / 2
      loops = loops * (min_ms / t1) + 1 'integer division!
      msg1$ = msg1$ + "Calibration done. Starting measurement with " & loops & " loops to get >=" & min_ms & " ms" + Chr(13)

      'Measurement
      t1 = get_ms()
      x = run_bench(bench, loops, n)
      t1 = get_ms() - t1
      msg1$ = msg1$ + "x=" & x & " (time: " & t1 & " ms)" + Chr(13)
      msg1$ = msg1$ + "Elapsed time for " & loops & " loops: " & t1 & " ms; estimation for 10 loops: " & (t1 * 10 / loops) & " ms" + Chr(13)
    endif
    bench_res1(bench) = (t1 * 10 / loops)
  next bench

  msg1$ = msg1$ + "Summary for 10 Loops:" + Chr(13)
  for bench = bench1 to bench2 step 1
    msg1$ = msg1$ + "Benchmark "& bench &": "& bench_res1(bench) &" ms" + Chr(13)
  next bench
  msg1$ = msg1$ + "Total elapsed time: " & (get_ms() - start_t) & " ms"
  
  print #iNumber, msg1$
  Close #iNumber
  MsgBox "Output written to file ($HOME/)" & out_file
  MsgBox msg1$
End Sub

