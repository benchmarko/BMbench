Sub Modul1
' bmbench.c
' (c) Benchmarko, 2002
'
' 06.05.2002  0.01
' 11.05.2002  0.02  bench1 = (sum 1..n) mod 65536
'
' usage:
' bmbench [bench] [n]
'


'
' bench01
' compute (sum of 1..n) mod 65536
' in: loops = number of loops
'         n = maximum number (assumed even, if integer arithmetic, normally n=1000000)
' out:    x = (sum 1..n) mod 65536
'
' Loops may be increased to produce a longer runtime without
' changing the result.
'
Function bmbench01(loops, n) As Integer
  Dim l As Integer
  Dim i As Long
  Dim x As Long
  x = 0
  For l = 1 To loops
    For i = 1 To n
      x = x + i 'overflow???
    Next i
    If (l < loops) Then
      x = x - (n / 2) * (n + 1)
      If (x <> 0) Then
        MsgBox "Error: bench01: x=" & x
      End If
    End If
  Next l
  bmbench01 = x Mod 65536
End Function

'
' run a benchmark
' in: bench = benchmark to use
'     loops = number of loops
'         n = maximum number (used in some benchmarks to define size of workload)
' out:    x = result
'
Function run_bench(bench, loops, n) As Integer
  Select Case bench
    Case "1"
      run_bench = bmbench01(loops, n)
    Case Else
      MsgBox "Error: Unknown benchmark: " & bench
      run_bench = -1
  End Select
End Function

'
' get timestamp in milliseconds
' out: x = time in ms
'
Function get_ms() As Long
  get_ms = Timer * 1000
End Function

Sub Main()
  Dim start_t As Long
  start_t = get_ms()
  Dim bench As Integer 'benchmark to test
  Dim n As Long 'maximum number
  Dim min_ms As Integer 'minimum runtime for measurement in ms
  bench = 1
  n = 100000
  min_ms = 10000

  'MsgBox "DEBUG: start_t = " & start_t
  MsgBox "BM Bench v0.2 (Excel/VBA)"

  
  Dim loops As Integer 'number of loops
  Dim x As Integer 'result from benchmark
  Dim t1 As Long 'timestamp
  loops = 1
  x = 0
  t1 = 0
  'Calibration
  While (t1 < 1000)  'we want at least 1 sec calibration time
    MsgBox "Calibrating benchmark " & bench & " with loops=" & loops & ", n=" & n
    t1 = get_ms()
    x = run_bench(bench, loops, n)
    t1 = get_ms() - t1
    MsgBox "x=" & x & " (time: " & t1 & " ms)"
    loops = loops * 2
  Wend
  loops = loops / 2
  loops = loops * (min_ms / t1) + 1 'integer division!
  MsgBox "Calibration done. Starting measurement with " & loops & " loops to get >=" & min_ms & " ms"

  'Measurement
  t1 = get_ms()
  x = run_bench(bench, loops, n)
  t1 = get_ms() - t1
  MsgBox "x=" & x & " (time: " & t1 & " ms)"
  MsgBox "Elapsed time for " & loops & " loops: " & t1 & " ms; estimation for 10 loops: " & (t1 * 10 / loops) & " ms"
  MsgBox "Total elapsed time: " & (get_ms() - start_t) & " ms"
  
End Sub

End Sub