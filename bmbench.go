//
// BM Bench - bmbench.go (Go)
// (c) Marco Vieth, 2002-2023
// http://www.benchmarko.de
//
// 29.04.2023 0.08 based on bmbench.java 0.08, (https://www.javainuse.com/java2go)
//
//
// https://www.onlinegdb.com/online_go_compiler
// https://www.w3schools.com/go/trygo.php?filename=demo_helloworld
// https://rextester.com/l/go_online_compiler
// https://go.dev/learn/
// https://go.dev/doc/
// https://gobyexample.com/
// https://www.golangprograms.com/basic-programs.html
// [https://play.golang.com/]
// https://www.javainuse.com/java2go

package main

import (
	"fmt"
	"os"
	"runtime"
	"time"
)

var prg_version = "0.08"
var prg_language = "Go"
var gState_startTs = int64(0)
var gState_tsPrecMs = 0.0
var gState_tsPrecCnt = 0
var gState_tsMeasCnt = 0
var g_cali_ms = 1001
var bench03Sieve1 []bool
var bench05Line1 []int
var default_bench1 = 0
var default_bench2 = 5
var default_n = 1000000
var maxBench = 6

// bench00 (Integer 16 bit)
// (sum of 1..n) mod 65536
func bench00(n int) int {
	x := int16(0)
	n_div_65536 := int16(n >> 16)
	n_mod_65536 := int16(n & 0xffff)
	var j int16
	for i := n_div_65536; i > 0; i-- {
		for j = 32767; j > 0; j-- {
			x += j
		}
		for j = -32768; j < 0; j++ {
			x += j
		}
	}
	for j = n_mod_65536; j > 0; j-- {
		x += j
	}
	return int(x) & 0xffff
}

// bench01 (Integer 32/64 bit)
// (arithmetic mean of 1..n)
func bench01(n int) int {
	x := 0
	sum := 0
	for i := 1; i <= n; i++ {
		sum += i
		if sum >= n {
			sum -= n
			x++
		}
	}
	return x
}

// bench02 (Floating Point 64 bit)
// (arithmetic mean of 1..n)
func bench02(n int) int {
	x := 0
	sum := 0.0
	nf := float64(n)
	for i := 1; i <= n; i++ {
		sum += float64(i)
		if sum >= nf {
			sum -= nf
			x++
		}
	}
	return x
}

/*
func bench02(n int) int {
	x := 0
	sum := 0.0
	nf := float64(n)
	for i := 1.0; i <= nf; i++ {
		sum += i
		if sum >= nf {
			sum -= nf
			x++
		}
	}
	return x
}
*/

// bench03 (Integer)
// number of primes less than or equal to n (prime-counting function)
// (Sieve of Eratosthenes, no multiples of 2 are stored)
func bench03(n int) int {
	nHalf := n >> 1
	if bench03Sieve1 == nil {
		bench03Sieve1 = make([]bool, nHalf+1)
	}
	sieve1 := bench03Sieve1
	var i int
	for i = 0; i <= nHalf; i++ {
		sieve1[i] = false
	}
	i = 0
	m := 3
	x := 1
	for m*m <= n {
		if !sieve1[i] {
			x++
			j := (m*m - 3) >> 1
			for j < nHalf {
				sieve1[j] = true
				j += m
			}
		}
		i++
		m += 2
	}
	for m <= n {
		if !sieve1[i] {
			x++
		}
		i++
		m += 2
	}
	return x
}

// bench04 (Integer 32 bit)
// nth random number number
// Raj Jain: The Art of Computer Systems Performance Analysis, John Wiley & Sons, 1991, page 442-444.
func bench04(n int) int {
	m := 2147483647
	a := 16807
	q := 127773
	r := 2836
	x := 1
	for i := n; i > 0; i-- {
		xDivQ := x / q
		xModQ := x - q*xDivQ
		x = a*xModQ - r*xDivQ
		if x <= 0 {
			x += m
		}
	}
	return x
}

// bench05 (Integer 32 bit)
// (n choose n/2) mod 65536 (Central Binomial Coefficient mod 65536)
// Using dynamic programming and Pascal's triangle, storing only one line
// Instead of nCk mod 65536 with k=n/2, we compute the product of (n/2)Ck mod 65536 with k=0..n/4 (Vandermonde folding)
func bench05(n int) int {
	n /= 2
	k := n / 2
	if n-k < k {
		k = n - k
	}
	if bench05Line1 == nil {
		bench05Line1 = make([]int, k+1)
	}
	line := bench05Line1
	for j := 0; j <= k; j++ {
		line[j] = 0
	}
	line[0] = 1
	if (len(line) > 1) {
	    line[1] = 2
	}
	for i := 3; i <= n; i++ {
		min1 := (i - 1) / 2
		if i&1 == 0 {
			line[min1+1] = 2 * line[min1]
		}
		prev := line[1]
		for j := 2; j <= min1; j++ {
			num := line[j]
			line[j] += prev
			prev = num
		}
		line[1] = i
	}
	x := 0
	for j := 0; j < k; j++ {
		x += 2 * line[j] * line[j]
	}
	x += line[k] * line[k]
	return x & 0xffff
}

func bench06(n int) int {
	sum := 0.0
	flip := -1.0
	for i := 1; i <= n; i++ {
		flip *= -1.0
		sum += flip / float64(2*i - 1)
	}
	return int(sum * 4.0 * 100000000)
}

func run_bench(bench int, loops int, n int, check int) int {
	if bench > maxBench {
		fmt.Println("Error: Unknown benchmark", bench)
	}
	x := 0
	for loops > 0 && x == 0 {
		switch bench {
		case 0:
			x = bench00(n)
		case 1:
			x = bench01(n)
		case 2:
			x = bench02(n)
		case 3:
			x = bench03(n)
		case 4:
			x = bench04(n)
		case 5:
			x = bench05(n)
		case 6:
			x = bench06(n)
		default:
			fmt.Println("Error: Unknown benchmark", bench)
		}
		x -= check
		loops--
	}
	x += check
	if x != check {
		fmt.Printf("Error(bench %d): x=%d\n", bench, x)
		x = -1
	}
	return x
}

func bench03Check(n int) int {
	var x int
	if n == 500000 {
		x = 41538
	} else {
		x = 1
		for j := 3; j <= n; j += 2 {
			isPrime := true
			for i := 3; i*i <= j; i += 2 {
				if j%i == 0 {
					isPrime = false
					break
				}
			}
			if isPrime {
				x++
			}
		}
	}
	return x
}

func getCheck(bench int, n int) int {
	var check int
	switch bench {
	case 0:
		check = (((n + n&1) >> 1) * (n + 1 - n&1)) & 0xffff
	case 1:
		check = (n + 1) / 2
	case 2:
		check = (n + 1) / 2
	case 3:
		check = bench03Check(n)
	case 4:
		if n == 1000000 {
			check = 1227283347
		} else {
			check = bench04(n)
		}
	case 5:
		if n == 5000 {
			check = 17376
		} else {
			check = bench05(n)
		}
	case 6:
		if n == 1000000 {
			check = 314159165
		} else {
			check = bench06(n)
		}
	default:
		fmt.Println("Error: Unknown benchmark", bench)
		check = -1
	}
	return check
}

func get_raw_ts() int64 {
	return time.Now().UnixNano()
}
func get_ts() int {
	return int(get_raw_ts() - gState_startTs)
}
func conv_ms(ts int) float64 {
	return float64(ts) / 1000000
}
func correctTime(tMeas float64, tMeas2 float64, measCount int) float64 {
	tsPrecCnt := gState_tsPrecCnt
	if measCount < tsPrecCnt {
		tMeas += gState_tsPrecMs * (float64(tsPrecCnt-measCount) / float64(tsPrecCnt))
		if tMeas > tMeas2 {
			tMeas = tMeas2
		}
	}
	return tMeas
}

func getPrecMs(stopFlg bool) float64 {
	measCount := 0
	tMeas0 := get_ts()
	tMeas := tMeas0
	for tMeas <= tMeas0 {
		tMeas = get_ts()
		measCount++
	}
	gState_tsMeasCnt = measCount
	var tMeasD float64
	if !stopFlg {
		tMeasD = conv_ms(tMeas)
	} else {
		tMeasD = correctTime(conv_ms(tMeas0), conv_ms(tMeas), measCount)
	}
	return tMeasD
}

func determineTsPrecision() {
	gState_startTs = get_raw_ts()
	tMeas0 := getPrecMs(false)
	tMeas1 := getPrecMs(false)
	gState_tsPrecMs = tMeas1 - tMeas0
	gState_tsPrecCnt = gState_tsMeasCnt
	tMeas0 = tMeas1
	tMeas1 = getPrecMs(false)
	if gState_tsMeasCnt > gState_tsPrecCnt {
		gState_tsPrecCnt = gState_tsMeasCnt
		gState_tsPrecMs = tMeas1 - tMeas0
	}
}

func checkbits_short1() int {
	num := int16(1)
	last_num := int16(0)
	bits := 0
	for {
		last_num = num
		num *= 2
		num++
		bits++
		if !((num-1)/2 == last_num && bits < 101) {
			break
		}
	}
	return bits
}

func checkbits_int32() int {
	num := int32(1)
	last_num := int32(0)
	bits := 0
	for {
		last_num = num
		num *= 2
		num++
		bits++
		if !((num-1)/2 == last_num && bits < 101) {
			break
		}
	}
	return bits
}
func checkbits_int1() int {
	num := 1
	last_num := 0
	bits := 0
	for {
		last_num = num
		num *= 2
		num++
		bits++
		if !((num-1)/2 == last_num && bits < 101) {
			break
		}
	}
	return bits
}

func checkbits_float1() int {
	num := float32(1.0)
	last_num := float32(0.0)
	bits := 0
	for {
		last_num = num
		num *= 2.0
		num++
		bits++
		if !((num-1.0)/2.0 == last_num && bits < 101) {
			break
		}
	}
	return bits
}

func checkbits_double1() int {
	num := 1.0
	last_num := 0.0
	bits := 0
	for {
		last_num = num
		num *= 2.0
		num++
		bits++
		if !((num-1.0)/2.0 == last_num && bits < 101) {
			break
		}
	}
	return bits
}

func get_info() string {
	currentTime := time.Now()

	str := fmt.Sprintf("BM Bench v%s (%s) -- (short:%d int32:%d int:%d float:%d double:%d tsMs:%f tsCnt:%d) -- ", prg_version, prg_language, checkbits_short1(), checkbits_int32(), checkbits_int1(), checkbits_float1(), checkbits_double1(), gState_tsPrecMs, gState_tsPrecCnt)
	str += runtime.Version() + " " + runtime.GOOS + " " + runtime.GOARCH + " " + runtime.Compiler + "\n"
	str += "(c) Marco Vieth, 2002-2023\n"
	str += currentTime.Format("2006-01-02 15:04:05") + "\n"
	return str
}

func measureBench(bench int, n int, check int) float64 {
	delta_ms := 100
	max_ms := 10000
	cali_ms := g_cali_ms
	loops := 1
	var x int
	tMeas := 0.0
	tEsti := 0.0
	throughput := 0.0
	fmt.Printf("Calibrating benchmark %d with n=%d, check=%d\n", bench, n, check)
	for throughput == 0 {
		tMeas = getPrecMs(false)
		x = run_bench(bench, loops, n, check)
		tMeas = getPrecMs(true) - tMeas
		var t_delta float64
		if tEsti > tMeas {
			t_delta = tEsti - tMeas
		} else {
			t_delta = tMeas - tEsti
		}
		var loops_p_sec float64
		if tMeas > 0 {
			loops_p_sec = float64(loops) * 1000.0 / tMeas
		} else {
			loops_p_sec = 0.0
		}
		fmt.Printf("%10.3f/s (time=%9.3f ms, loops=%7d, delta=%9.3f ms)\n", loops_p_sec, tMeas, loops, t_delta)

		if x == -1 {
			throughput = -1
		} else if tEsti > 0 && t_delta < float64(delta_ms) {
			throughput = loops_p_sec
			fmt.Printf("Benchmark %d (%s): %.3f/s (time=%.3f ms, loops=%d, delta=%.3f ms)\n", bench, prg_language, loops_p_sec, tMeas, loops, t_delta)
		} else if tMeas > float64(max_ms) {
			fmt.Printf("Benchmark %d (%s): Time already > %d ms. No measurement possible.\n", bench, prg_language, max_ms)
			if loops_p_sec > 0 {
				throughput = -loops_p_sec
			} else {
				throughput = -1
			}
		} else {
			var scale_fact int
			if tMeas == 0 {
				scale_fact = 50
			} else if tMeas < float64(cali_ms) {
				scale_fact = int(float64(cali_ms+100)/tMeas) + 1
			} else {
				scale_fact = 2
			}
			loops *= scale_fact
			tEsti = tMeas * float64(scale_fact)
		}
	}
	return throughput
}

func print_results(bench1 int, bench2 int, bench_res1 []float64) {
	max_language_len1 := 10
	fmt.Println("\nThroughput for all benchmarks (loops per sec):")
	str := fmt.Sprintf("BMR (%s)", prg_language)
	for i := len([]rune(prg_language)); i < max_language_len1; i++ {
		str += " "
	}
	str += ": "
	for bench := bench1; bench <= bench2; bench++ {
		str += fmt.Sprintf("%9.3f ", bench_res1[bench])
	}
	fmt.Println(str)
	fmt.Println("")
}

func start_bench(bench1 int, bench2 int, n int, argStr string) int {
	determineTsPrecision()
	fmt.Println(get_info())
	if argStr != "" {
		fmt.Println("Args:", argStr)
	}
	bench_res := make([]float64, bench2+1)
	for bench := bench1; bench <= bench2; bench++ {
		n2 := n
		if bench == 3 {
			n2 = n2 / 2
		} else if bench == 5 {
			n2 = n2 / 200
		}
		check := getCheck(bench, n2)
		var throughput float64
		if check > 0 {
			throughput = measureBench(bench, n2, check)
		} else {
			throughput = -1
		}
		bench_res[bench] = throughput
	}
	print_results(bench1, bench2, bench_res)
	return 1
}

func main() {
	if len(os.Args) > 1 {
		fmt.Sscanf(os.Args[1], "%d", &default_bench1)
		default_bench2 = default_bench1
	}
	if len(os.Args) > 2 {
		fmt.Sscanf(os.Args[2], "%d", &default_bench2)
	}
	if len(os.Args) > 3 {
		fmt.Sscanf(os.Args[3], "%d", &default_n)
	}
	if len(os.Args) > 4 {
		fmt.Sscanf(os.Args[4], "%d", &g_cali_ms)
	}

	argStr := ""
	for i := 1; i < len(os.Args); i++ {
		argStr += " " + os.Args[i]
	}

	start_bench(default_bench1, default_bench2, default_n, argStr)
	fmt.Printf("Total elapsed time: %f ms\n", conv_ms(get_ts()))
}
