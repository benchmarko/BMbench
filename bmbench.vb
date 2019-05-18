'
' BM Bench - bmbench.vb (Visual Basic .NET)
' (c) Marco Vieth, 2006
' http://www.benchmarko.de
'
' 06.05.2002 0.01
' 11.05.2002 0.02 bench1 = (sum 1..n) mod 65536
' 03.05.2006 0.05 created .vb based on .vba
' 17.05.2019 0.07
'
'
' Compile & Run:
' - .Net Core (https://dotnet.microsoft.com/download)
'   dotnet run -p bmbench.vbproj -c Release [bench1] [bench2] [n]
'   dotnet build bmbench.vbproj -c Release  (build only)
'   dotnet bin/Release/netcoreapp2.2/bmbench_vb.dll [bench1] [bench2] [n]  (run only)
'
' - Visual Basic .NET 2003 (C:\Program Files\Microsoft Visual Studio .NET 2003\Common7\Tools\vsvars32.bat)
'   ??? /optimize bmbench.vb
'   bmbench.exe [bench1] [bench2] [n]
'
'
' Compatability:
' VB 6.0 -> VB .NET: Long -> Integer, Integer -> Short
'
'
' bench01
' compute (sum of 1..n) mod 65536
' in: loops = number of loops
'         n = maximum number (assumed even, if integer arithmetic, normally n=1000000)
' out:    x = (sum 1..n) mod 65536
'
' Loops may be increased to produce a longer runtime without changing the result.
'

Imports System

Public Module Module1

    Dim prg_version as String = "0.07"
    Dim prg_language as String = "VB"

    Dim gState_tsPrecCnt = 0
    Dim gState_tsPrecMs = 0
    Dim gState_tsMeasCnt = 0

'    Dim nfi As System.Globalization.NumberFormatInfo = new System.Globalization.CultureInfo("en-US", false).NumberFormat
    Dim nfi As IFormatProvider = System.Globalization.CultureInfo.InvariantCulture
  

    '
    ' bench00 (Integer 16 bit)
    ' (sum of 1..n) mod 65536
    Function bench00(ByVal loops As Integer, ByVal n As Integer, ByVal check As Integer) As Integer
        Dim i As Integer
        Dim j As Short
        Dim x As Integer = 0
        'Dim sum1 As Integer = ((n / 2) * (n + 1)) Mod 65536 'assuming n even! (sum should be ...)
        Dim n_div_65536 As Integer = n >> 16 'use short?
        Dim n_mod_65536 As Integer = n And &HFFFF
        While loops > 0 And x = 0
            For i = n_div_65536 To 1 Step -1
                For j = 32767 To 1 Step -1
                    x += j
                Next j
                For j = -32768 To -1
                    x += j
                Next j
            Next i
            For j = n_mod_65536 To 1 Step -1
                x += j
            Next j

            loops -= 1
            x = x Mod 65536
            x -= check
        End While
        Return x Mod 65536
    End Function


    '
    ' bench01 (Integer 16/32 bit)
    ' (arithmetic mean of 1..n)
    Function bench01(ByVal loops As Integer, ByVal n As Integer, ByVal check As Integer) As Integer
        Dim x As Integer = 0
        'Dim sum1 As Integer = ((n / 2) * (n + 1)) Mod 65536 'assuming n even! (sum should be ...)
        While loops > 0 And x = 0
            Dim sum as Integer = 0
            For i As Integer = 1 To n Step 1
                sum += i
                If sum >= n Then 'to avoid numbers above 2*n, divide by n using subtraction
                    sum -= n
                    x += 1
                End If
            Next i
            loops -= 1
            x -= check
        End While
        Return x
    End Function


    '
    ' bench02 (Floating Point, normally 64 bit)
    ' (arithmetic mean of 1..n)
    Function bench02(ByVal loops As Integer, ByVal n As Integer, ByVal check As Integer) As Integer
        Dim x As Integer = 0
        'Dim sum1 As Double = (n / 2.0) * (n + 1.0) 'assuming n even! (sum should be ...)
        While loops > 0 And x = 0
            Dim sum as Double = 0.0
            For i As Integer = 1 To n Step 1
                sum += i
                If sum >= n Then
                    sum -= n
                    x += 1
                End If
            Next i

            loops -= 1
            x -= check
        End While
        Return x
    End Function


    '
    ' bench03 (Integer)
    ' number of primes below n (Sieve of Eratosthenes)
    ' Example: n=500000 => x=41538 (expected), n=1000000 => x=78498
    ' (Byte consumes 8 bits, Boolean 16, so use Byte; BitArray is slow)
    ' (For loop is much faster than while here)
    Function bench03(ByVal loops As Integer, ByVal n As Integer, ByVal check As Integer) As Integer
        n >>= 1 'compute only up to n/2
        Dim i, m, j As Integer
        Dim x As Integer = 0 'number of primes below n

        Dim nHalf As Integer = n >> 1
        Dim sieve1(nHalf + 1) As Byte 'or: new BitArray(n + 1)

        While loops > 0 And x = 0
            'initialize sieve
            For i = 0 To nHalf
                sieve1(i) = 0
            Next i

            'compute primes
            i = 0
            m = 3
            While (m * m < n)
                If (sieve1(i) = 0) Then
                    x += 1
                    j = (m * m - 3) >> 1 'div 2
                    For j = j to nHalf - 1 Step m
                        sieve1(j) = 1
                    Next j
                End If
                i += 1
                m += 2
            End While

            'count remaining primes
            For i = i To nHalf
                If (sieve1(i) = 0) Then
                    x += 1
                End If
            Next i

            'check prime count
            loops -= 1
            x -= check
        End While
        Return x
    End Function


    Function bench03_ok1_was_fast(ByVal loops As Integer, ByVal n As Integer, ByVal check As Integer) As Integer
        n >>= 1 'compute only up to n/2
        Dim i, j As Integer
        Dim sieve1(n + 1) As Byte 'or: new BitArray(n + 1)
        Dim x As Integer = 0 'number of primes below n
        sieve1(0) = 0
        sieve1(1) = 0

        While loops > 0 And x = 0
            'initialize sieve
            For i = 2 To n
                sieve1(i) = 1
            Next i

            ' compute primes
            i = 2
            While (i * i) <= n
                If (sieve1(i) <> 0) Then
                    For j = i * i To n Step i
                        sieve1(j) = 0
                    Next j
                End If
                i += 1
            End While

            ' count primes
            For i = 0 To n
                If (sieve1(i) <> 0) Then
                    x += 1
                End If
            Next i

            'check prime count
            loops -= 1
            x -= check
        End While
        Return x
    End Function


    '
    ' bench04 (Integer 32 bit)
    ' nth random number number
    ' Random number generator taken from
    ' Raj Jain: The Art of Computer Systems Performance Analysis, John Wiley & Sons, 1991, page 442-444.
    ' It needs longs with at least 32 bit.
    ' Starting with x0=1, x10000 should be 1043618065, x1000000 = 1227283347.
    Function bench04(ByVal loops As Integer, ByVal n As Integer, ByVal check As Integer) As Integer
        Dim m As Integer = 2147483647 'modulus, do not change!
        Dim a As Integer = 16807      'multiplier
        Dim q As Integer = 127773     'm div a
        Dim r As Integer = 2836       'm mod a
        Dim x As Integer = 0          'last random value

        While loops > 0 And x = 0
            x += 1
            For i As Integer = n To 1 Step -1
                'Dim x_div_q As Integer = x / q '(int)
                'Dim x_mod_q As Integer = x - q * x_div_q
                'x = a * x_mod_q - r * x_div_q
                x = a * (x Mod q) - r * (x \ q) 'x div q !
                If (x <= 0) Then
                    x += m 'x is new random number
                End If
            Next i

            loops -= 1
            x -= check
        End While
        Return x
    End Function


    '
    ' bench05 (Integer 32 bit)
    ' n over n/2 mod 65536 (Pascal's triangle)
    '
    Function bench05(ByVal loops As Integer, ByVal n As Integer, ByVal check As Integer) As Integer
        Dim x As Integer = 0
        n = n / 500
        Dim k As Integer = n >> 1 'div 2

        If ((n - k) < k) Then
            k = n - k 'keep k minimal with  n over k  =  n over n-k
        End If

        'allocate memory...
        Dim pas1(2, k + 1) As Integer
        pas1(0, 0) = 1
        pas1(1, 0) = 1 'set first column

        While loops > 0 And x = 0
            For i As Integer = 3 To n
                Dim i_mod_2 As Integer = i And 1
                Dim i1_mod_2 As Integer = i_mod_2 Xor 1
                Dim min1 As Integer
                If (i_mod_2 = 0) Then 'Math.floor((i - 1) / 2)
                    min1 = (i - 2) >> 1
                Else
                    min1 = (i - 1) >> 1
                End If

                If (k < min1) Then
                    min1 = k
                End If
                pas1(i_mod_2, 1) = i 'second column is i
                For j As Integer = 2 To min1 'up to min((i-1)/2, k)
                    pas1(i_mod_2, j) = (pas1(i_mod_2 Xor 1, j - 1) + pas1(i_mod_2 Xor 1, j)) And &HFFFF
                    '% 65536 -- we need mod here to avoid overflow
                Next j
                If ((min1 < k) And (i_mod_2 = 0)) Then 'new element
                    'pas1[i_mod_2][Math.floor(i / 2)] = 2 * pas1[i_mod_2 ^ 1][Math.floor((i - 1) / 2)];
                    pas1(i_mod_2, min1 + 1) = 2 * pas1(i_mod_2 Xor 1, min1)
                End If
            Next i
            x += pas1(n And 1, k) And &HFFFF '% 65536

            loops -= 1
            x -= check
        End While
        Return x
    End Function


    '
    ' run a benchmark
    ' in: bench = benchmark to use
    '     loops = number of loops
    '         n = maximum number (used in some benchmarks to define size of workload)
    ' out:    x = result
    '
    Function run_bench(ByVal bench As Integer, ByVal loops As Integer, ByVal n As Integer) As Integer
        Dim x As Integer = 0
        Dim check As Integer = 0

        Select Case bench
            Case 0
                check = ((n / 2) * (n + 1)) And &HFFFF
                x = bench00(loops, n, check)
            Case 1
                check = (n + 1) / 2
                x = bench01(loops, n, check)
            Case 2
                check = (n + 1) / 2
                x = bench02(loops, n, check)
            Case 3
                check = 41538
                x = bench03(loops, n, check)
            Case 4
                check = 1227283347
                x = bench04(loops, n, check)
            Case 5
                check = 27200
                x = bench05(loops, n, check)
            Case Else
                System.Console.WriteLine("Error: Unknown benchmark: " & bench)
                check = -1
        End Select

        x += check
        If x <> check Then
            System.Console.Error.WriteLine("Error(bench" & bench & "): x=" & x)
            x = -1 'exit
        End If
        Return x
    End Function

    '
    ' get timestamp in milliseconds
    ' out: x = time in ms
    '
    Function get_ms() As Long
        'get_ms = Microsoft.VisualBasic.DateAndTime.Timer * 1000
        ' or: System.DateTime.Now.Ticks / 10000 'returns 100ns ticks
        Return System.DateTime.Now.Ticks / 10000 'returns 100ns ticks
    End Function


    Function getdate1() As String
        Dim dtfi as System.Globalization.DateTimeFormatInfo = new System.Globalization.CultureInfo("de-DE", false).DateTimeFormat
        Return System.DateTime.Now.ToString(dtfi) 'always German format
    End Function


    Function correctTime(ByVal tMeas As Long, ByVal measCount As Integer) As Long
        Dim tsPrecCnt = gState_tsPrecCnt

        If measCount < tsPrecCnt Then
        tMeas += gState_tsPrecMs * ((tsPrecCnt - measCount) / tsPrecCnt) ' ts + correction
        End If
        return tMeas
    End Function

    Function getPrecMs(ByVal stopFlg as Boolean) As Long
        Dim measCount As Integer = 0

        Dim tMeas0 As Long = get_ms()
        Dim tMeas = tMeas0
        While tMeas <= tMeas0
            tMeas = get_ms()
            measCount += 1
        End While

        If stopFlg Then
            tMeas = correctTime(tMeas0, measCount) 'for stop: use first ts + correction
        End If
        gState_tsMeasCnt = measCount ' memorize count
        return tMeas
    End Function

    'usually only needed if time precision is low, e.g. one second
    Sub determineTsPrecision()
        Dim tMeas0 As Long = getPrecMs(false)
        Dim tMeas1 As Long = getPrecMs(false)
        gState_tsPrecMs = (tMeas1 - tMeas0)
        gState_tsPrecCnt = gState_tsMeasCnt

        ' do it again
        tMeas0 = tMeas1
        tMeas1 = getPrecMs(false)
        If gState_tsMeasCnt > gState_tsPrecCnt Then 'taker maximum count
            gState_tsPrecCnt = gState_tsMeasCnt
            gState_tsPrecMs = (tMeas1 - tMeas0)
        End If
    End Sub


    ' Here we compute the number of "significant" bits for positive numbers (which means 53 for double)
    Function checkbits_int1() As Integer
        Dim num As Integer = 1
        Dim last_num As Integer = 0
        Dim bits As Integer = 0
        Try
            Do While ((((num - 1) / 2) = last_num) And (bits < 101))
                last_num = num
                num *= 2 'will overflow
                num += 1
                bits += 1
            Loop
        Catch ex As System.Exception
            bits += 1
        End Try
        Return bits
    End Function

    Function checkbits_double1() As Integer
        Dim num As Double = 1.0
        Dim last_num As Double = 0.0
        Dim bits As Integer = 0
        Do While ((((num - 1.0) / 2.0) = last_num) And (bits < 101))
            last_num = num
            num *= 2.0
            num += 1
            bits += 1
        Loop
        Return bits
    End Function


    Sub print_info()
        Dim version1 As String = System.Environment.OSVersion.ToString()
        Try
          version1 = System.Reflection.Assembly.GetExecutingAssembly().GetName().Version().ToString() & ", " & version1
        Catch ex As System.Security.SecurityException
        End Try
        System.Console.WriteLine("BM Bench v" & prg_version & " (" & prg_language & ") -- (int:" & checkbits_int1() & " double:" & checkbits_double1() & " tsMs:" & gState_tsPrecMs & " tsCnt:" & gState_tsPrecCnt & ") " & version1)
        System.Console.WriteLine("(c) Marco Vieth, 2006-2019")
        System.Console.WriteLine(getdate1())
    End Sub


    Sub print_results(ByVal bench1 as Integer, ByVal bench2 As Integer, bench_res1() as Double)
        Dim max_language_len1 As Integer = 10
        System.Console.WriteLine()
        System.Console.WriteLine("Throughput for all benchmarks (loops per sec):")
        Dim str As String = "BMR (" + prg_language + ")"
        Dim i As Integer
        For i = prg_language.Length To max_language_len1 
            str += " "
        Next i

        Dim bench As Integer
        For bench = bench1 To bench2
            str += String.Format(nfi, "{0,9:F3} ", bench_res1(bench))
        Next bench
        System.Console.WriteLine(str)
    End Sub


    Function measureBench(ByVal bench As Integer, ByVal n As Integer)
        Dim cali_ms As Integer = 1001 '401 '1001 TTT
        Dim delta_ms As Integer = 100 '150 '100 TTT
        Dim max_ms As Integer = 10000
        Dim loops as Integer = 1 'number of loops
        Dim x as Integer 'result from benchmark
        Dim tMeas as Long = 0 'measured time
        Dim tEsti as Long = 0  'estimated time
        Dim throughput As Double = 0

        System.Console.WriteLine("Calibrating benchmark " & bench & " with n=" & n)
        While (throughput = 0)
            tMeas = getPrecMs(false)
            x = run_bench(bench, loops, n)
            tMeas = getPrecMs(true) - tMeas

            Dim t_delta As Long
            If tEsti > tMeas Then 'compute difference abs(measures-estimated)
                t_Delta = tEsti - tMeas
            Else
                t_Delta = tMeas - tEsti
            End If

            Dim loops_p_sec As Double
            If tMeas > 0 Then
                loops_p_sec = loops * 1000.0 / tMeas
            Else
                loops_p_sec = 0
            End If
           
            System.Console.WriteLine("{0,10}/s (time={1,5} ms, loops={2,7}, delta={3,5} ms, x={4})", loops_p_sec.ToString("F3", nfi), tMeas, loops, t_delta, x)

            If (x = -1) Then 'some error?
                throughput = -1
            Else If ((tEsti > 0) And (t_delta < delta_ms)) Then 'do we have some estimated/expected time smaller than delta_ms=100?
                throughput = loops_p_sec 'yeah, set measured loops per sec
                System.Console.WriteLine("Benchmark {0} ({1}): {2}/s (time={3} ms, loops={4}, delta={5} ms)", bench, prg_language, loops_p_sec.ToString("F3", nfi), tMeas, loops, t_delta)
            Else If (tMeas > max_ms) Then
                System.Console.WriteLine("Benchmark {0} ({1}): Time already > {2} ms. No measurement possible.", bench, prg_language, max_ms)
                If loops_p_sec > 0 Then
                    throughput = -loops_p_sec 'cannot rely on measurement, so set to negative
                Else
                    throughput = 0
                End If
            Else
                Dim scale_fact As Integer = 2
                If (tMeas < cali_ms) And (tMeas > 0) Then
                    scale_fact = ((cali_ms + 100) / tMeas) + 1
                    ' scale a bit up to 1100 ms (cali_ms+100)
                End If
                loops *= scale_fact
                tEsti = tMeas * scale_fact
            End If
        End While
        Return throughput
    End Function


    Function start_bench(ByVal bench1 as Integer, ByVal bench2 As Integer, ByVal n as Integer) As Integer
        print_info()
        Dim bench_res(bench2 + 1) As Double
        Dim bench As Integer 'benchmark to test
        For bench = bench1 To bench2
            Dim throughput As Double = measureBench(bench, n)
            bench_res(bench) = throughput
        Next bench
        print_results(bench1, bench2, bench_res)
        Return 1
    End Function


'    Function GetCommandLineArgs_old() As String()
'        Dim separators As String = " "
'        Dim commands As String = ""
'        Try
'          commands = Microsoft.VisualBasic.Interaction.Command()
'        Catch ex As System.Security.SecurityException
'        End Try
'        Dim args() As String = commands.Split(separators.ToCharArray)
'        Return args
'    End Function


    Function GetCommandLineArgs As String()
        Dim args() As String = {}
        Try
            args = Environment.GetCommandLineArgs()
        Catch ex As System.Security.SecurityException
        End Try
        Return args
    End Function


    Public Sub Main()
        Dim start_t As Long = get_ms() 'memorize start time
        Dim bench1 As Integer = 0 '0 first benchmark to test
        Dim bench2 As Integer = 5 '5 'last benchmark to test
        Dim n As Integer = 1000000 'maximum number

        Dim args = GetCommandLineArgs()

        If (args.Length >= 2) Then
            If (args(1) <> "") Then
                bench1 = Integer.Parse(args(1))
                bench2 = bench1
            End If
        End If
        If (args.Length >= 3) Then
            bench2 = Integer.Parse(args(2))
        End If
        If (args.Length >= 4) Then
            n = Integer.Parse(args(3))
        End If

        determineTsPrecision()
        Dim rc =  start_bench(bench1, bench2, n)
        System.Console.WriteLine("Total elapsed time: " & (get_ms() - start_t) & " ms")
    End Sub

End Module


'Can be run in https://rextester.com/l/visual_basic_online_compiler
#if False then
Namespace Rextester
    Public Module Program
        Public Sub Main(args() As string)
            Module1.Main()
        End Sub
    End Module
End Namespace
#End if

'end
