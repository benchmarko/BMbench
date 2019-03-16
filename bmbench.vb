'
' BM Bench - bmbench.vb (Visual Basic .NET)
' (c) Marco Vieth, 2006
' http://www.benchmarko.de
'
' 06.05.2002 0.01
' 11.05.2002 0.02 bench1 = (sum 1..n) mod 65536
' 03.05.2006 0.05 created .vb based on .vba
'
'
' Compile:
' "C:\Program Files\Microsoft Visual Studio .NET 2003\Common7\Tools\vsvars32.bat"
' ??? /optimize bmbench.vb
'
' Usage:
' bmbench [bench1] [bench2] [n]
'

'
' This code is designed for Visual Basic .NET 2003.
' Some changes compared to Visual Basic 6.0:
' VB 6.0      VB .NET
' Long        Integer
' Integer     Short
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

Module Module1

    '
    ' bench00 (Integer 16 bit)
    ' (sum of 1..n) mod 65536
    Function bmbench00(ByVal loops As Integer, ByVal n As Integer) As Integer
        Dim i As Integer
        Dim j As Short
        Dim x As Integer = 0
        Dim sum1 As Integer = ((n / 2) * (n + 1)) Mod 65536 'assuming n even! (sum should be ...)
        Dim n_div_65536 As Integer = n >> 16 'use short?
        Dim n_mod_65536 As Integer = n And &HFFFF
        While loops > 0
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
            If (loops > 0) Then 'some more loops left?
                x = x Mod 65536  'do not use &= 0xffff
                x -= sum1    'yes, set x back to 0
                If (x <> 0) Then 'now x must be 0 again
                    x += 1  'force error for many wrong computations
                    Exit While 'break
                End If
            End If
        End While
        Return x Mod 65536
    End Function


    '
    ' bench01 (Integer 16/32 bit)
    ' (sum of 1..n) mod 65536
    Function bmbench01(ByVal loops As Integer, ByVal n As Integer) As Integer
        Dim x As Long = 0 'use Long here to avoid overflow
        Dim sum1 As Integer = ((n / 2) * (n + 1)) Mod 65536 'assuming n even! (sum should be ...)
        While loops > 0
            For i As Integer = n To 1 Step -1
                x += i
            Next i

            loops -= 1
            If (loops > 0) Then 'some more loops left?
                x = x Mod 65536  'do not use &= 0xffff
                x -= sum1    'yes, set x back to 0
                If (x <> 0) Then 'now x must be 0 again
                    x += 1  'force error for many wrong computations
                    Exit While 'break
                End If
            End If
        End While
        Return x Mod 65536
    End Function



    '
    ' bench02 (Floating Point, normally 64 bit)
    ' (sum of 1..n) mod 65536
    Function bmbench02(ByVal loops As Integer, ByVal n As Integer) As Integer
        Dim x As Double = 0.0
        Dim sum1 As Double = (n / 2.0) * (n + 1.0) 'assuming n even! (sum should be ...)
        While loops > 0
            For i As Integer = n To 1 Step -1
                x += i
            Next i

            loops -= 1
            If (loops > 0) Then 'some more loops left?
                x -= sum1    'yes, set x back to 0
                If (x <> 0.0) Then 'now x must be 0 again
                    x += 1  'force error for many wrong computations
                    Exit While 'Error
                End If
            End If
        End While
        Return x Mod 65536
    End Function


    '
    ' bench03 (Integer)
    ' number of primes below n (Sieve of Eratosthenes)
    ' Example: n=500000 => x=41538 (expected), n=1000000 => x=78498
    ' (Byte consumes 8 bits, Boolean 16, so use Byte; BitArray is slow)
    Function bmbench03(ByVal loops As Integer, ByVal n As Integer) As Integer
        n >>= 1 'compute only up to n/2
        Dim i, j As Integer
        Dim sieve1(n + 1) As Byte 'or: new BitArray(n + 1)
        Dim x As Integer = 0 'number of primes below n
        sieve1(0) = 0
        sieve1(1) = 0

        While loops > 0
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
            If (loops > 0) Then 'some more loops left?
                x -= 41538 'yes, set x back to 0 (number of primes below 1000000)
                If (x <> 0) Then 'now x must be 0 again
                    x += 1
                    Exit While 'Error
                End If
            End If
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
    Function bmbench04(ByVal loops As Integer, ByVal n As Integer) As Integer
        Dim m As Integer = 2147483647 'modulus, do not change!
        Dim a As Integer = 16807      'multiplier
        Dim q As Integer = 127773     'm div a
        Dim r As Integer = 2836       'm mod a
        Dim x As Integer = 1          'last random value

        While loops > 0
            For i As Integer = n To 1 Step -1
                Dim x_div_q As Integer = x / q '(int)
                Dim x_mod_q As Integer = x - q * x_div_q
                x = a * x_mod_q - r * x_div_q
                If (x <= 0) Then
                    x += m 'x is new random number
                End If
            Next i

            loops -= 1
            If (loops > 0) Then 'some more loops left?
                x -= 1227283347 'yes, set x back to 0
                If (x <> 0) Then 'now x must be 0 again
                    x += 1
                    Exit While 'Error
                End If
                x += 1 'start with 1 again
            End If
        End While
        Return x
    End Function


    '
    ' bench05 (Integer 32 bit)
    ' n over n/2 mod 65536 (Pascal's triangle)
    '
    Function bmbench05(ByVal loops As Integer, ByVal n As Integer) As Integer
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

        While loops > 0
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
            If (loops > 0) Then 'some more loops left?
                x -= 27200 'yes, set x back to 0
                If (x <> 0) Then 'now x must be 0 again
                    x += 1
                    Exit While 'Error
                End If
            End If

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
        Dim check1 As Integer = 0

        Select Case bench
            Case 0
                x = bmbench00(loops, n)
                check1 = 10528
            Case 1
                x = bmbench01(loops, n)
                check1 = 10528
            Case 2
                x = bmbench02(loops, n)
                check1 = 10528
            Case 3
                x = bmbench03(loops, n)
                check1 = 41538
            Case 4
                x = bmbench04(loops, n)
                check1 = 1227283347
            Case 5
                x = bmbench05(loops, n)
                check1 = 27200
            Case Else
                System.Console.WriteLine("Error: Unknown benchmark: " & bench)
                check1 = x + 1
        End Select
        If check1 <> x Then
            Console.Error.WriteLine("Error(bench" & bench & "): x=" & x)
            x = -1 'exit
        End If
        Return x
    End Function

    '
    ' get timestamp in milliseconds
    ' out: x = time in ms
    '
    Function get_ms() As Long
        get_ms = Timer * 1000
    End Function


    Function getdate1() As String
        Return "ddd"
    End Function


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
        Catch ex As Exception
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


    Function GetCommandLineArgs() As String()
        Dim separators As String = " "
        Dim commands As String = Microsoft.VisualBasic.Command()
        Dim args() As String = commands.Split(separators.ToCharArray)
        Return args
    End Function


    Sub Main()
        Dim start_t As Long = get_ms() 'memorize start time
        Dim bench1 As Integer = 0 'first benchmark to test
        Dim bench2 As Integer = 0 '5 'last benchmark to test
        Dim n As Integer = 1000000 'maximum number
        Dim min_ms As Integer = 10000 'minimum runtime for measurement in ms

        Dim args = GetCommandLineArgs()
        If (args.Length >= 1) Then
            If (args(0) <> "") Then
                bench1 = Integer.Parse(args(0))
                bench2 = bench1
            End If
        End If
        If (args.Length >= 2) Then
            bench2 = Integer.Parse(args(1))
        End If
        If (args.Length >= 3) Then
            n = Integer.Parse(args(2))
        End If

        Dim bench_res1(bench2 + 1) As Integer

        Dim version1 As String = System.Reflection.Assembly.GetExecutingAssembly().GetName().Version().ToString() & ", " & Environment.OSVersion.ToString()

        System.Console.WriteLine("BM Bench v0.5 (VB) (int:" & checkbits_int1() & " double:" & checkbits_double1() & ") " & version1)
        System.Console.WriteLine("(c) Marco Vieth, 2006")
        System.Console.WriteLine(getdate1())

        Dim bench As Integer 'benchmark to test
        For bench = bench1 To bench2
            Dim loops As Integer = 1 'number of loops
            Dim x As Integer = 0 'result from benchmark
            Dim t1 As Long = 0 'timestamp
            'Calibration
            While (t1 < 1000)  'we want at least 1 sec calibration time
                System.Console.WriteLine("Calibrating benchmark " & bench & " with loops=" & loops & ", n=" & n)
                t1 = get_ms()
                x = run_bench(bench, loops, n)
                t1 = get_ms() - t1
                System.Console.WriteLine("x=" & x & " (time: " & t1 & " ms)")
                loops = loops * 2
                If (x = -1) Then
                    Exit While
                End If
            End While
            If (x <> -1) Then
                loops >>= 1 'div 2
                loops *= (min_ms / t1) + 1 'integer division!
                System.Console.WriteLine("Calibration done. Starting measurement with " & loops & " loops to get >=" & min_ms & " ms")

                'Measurement
                t1 = get_ms()
                x = run_bench(bench, loops, n)
                t1 = get_ms() - t1
                System.Console.WriteLine("x=" & x & " (time: " & t1 & " ms)")
                bench_res1(bench) = t1 * 10 / loops
                System.Console.WriteLine("Elapsed time for " & loops & " loops: " & t1 & " ms; estimation for 10 loops: " & bench_res1(bench) & " ms")
            Else
                bench_res1(bench) = -1
            End If
        Next bench

        Console.WriteLine("Times for all benchmarks (10 loops, ms):")
        Dim str As String = "BM Results (VB)        : "
        For bench = bench1 To bench2
            str += String.Format("{0, 7} ", bench_res1(bench))
        Next bench
        Console.WriteLine(str)
        System.Console.WriteLine("Total elapsed time: " & (get_ms() - start_t) & " ms")

    End Sub

End Module
