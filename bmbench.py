#! /usr/bin/env python
# BM Bench - bmbench.py (Python)
# (c) Marco Vieth, 2002-2022
# http://www.benchmarko.de
#
# 06.05.2002 0.01
# 11.05.2002 0.02  bench01 = (sum 1..n) mod 65536
# 20.07.2002 0.04  some errors corrected
# 24.01.2003 0.05  output format changed
# 03.12.2006 0.06  based on version 0.05
# 05.05.2019 0.07  changed bench 01-03; time interval estimation
# 03.12.2022 0.072 bench03 corrected, bench05 improved
# 19.02.2023 0.08  bench05 optimized
#
# Python version 2 or 3
#
# Usage:
# python -O bmbench1.py [bench1] [bench2] [n]
# (example in: /usr/lib/python2.2/urllib.py)
#
#
# Note:
# This is my first program in Python, so I tried my best...
# A good starting point is the python-doc package:
# file:/usr/share/doc/packages/python/html/index.html
# (including a very readable Tutorial).
# Other references:
# pydoc -p 8080 to start a Python documentation server
#
# Or: http://www.angelfire.com/tx4/cus/notes/python.html
#
# Data types are nearly implicit:
# - integer (32 bit), if it gets too long -> long integer (any number of bits, marked with L)
# - floating point (marked with dot)
# - print(type(x))
#

#
# Explicit while loop is slower than for i in range()
#

from __future__ import print_function #python3 style
import time

import sys # for flush()

#check this: https://www.numpy.org/  used at: http://zwmiller.com/blogs/python_data_structure_speed.html
#import numpy as np

G_PRG_VERSION = "0.08"
G_PRG_LANGUAGE = "Python"

g_startTs = 0
g_tsPrecMs = 0 # measured time stamp precision
g_tsPrecCnt = 0 # time stamp count (calls) per precision interval (until time change)
g_tsMeasCnt = 0 # last measured count
g_cali_ms = 1001
g_delta_ms = 100
g_sieve1 = []

#
# General description for benchmark test functions
# benchxx - benchmark
# <description>
# in: loops = number of loops
#         n = maximum number (assumed even, normally n=1000000)
# out:    x = <output decription>
#
# loops may be increased to produce a longer runtime without changing the result.
#

#
# bench00 (Integer 16 bit)
# (sum of 1..n) mod 65536
#
def bench00(n):
  x = 0
  n_div_65536 = (n >> 16) & 0xffff
  n_mod_65536 = n & 0xffff;

  # simulate summation with 16 bit borders...
  for i in range(n_div_65536, 0, -1):
    for j in range(65535, 0, -1):
      x += j
  for j in range(n_mod_65536, 0, -1):
    x += j

  return int(x & 0xffff)


# bench01 (Integer 32 bit)
# (arithmetic mean of 1..n) mod 65536
#
def bench01(n):
  x = 0
  sum = 0
  for i in range(1, n + 1):
    sum += i
    if (sum >= n): # to avoid numbers above 2*n, divide by n using subtraction
      sum -= n
      x += 1

  return x


#
# bench02 (Floating Point, normally 64 bit)
# (arithmetic mean of 1..n) mod 65536
#
def bench02(n):
  x = 0
  sum = 0.0
  for i in range(1, n + 1):
    sum += i
    if (sum >= n):
      sum -= n
      x += 1

  return x


#
# bench03 (Integer)
# number of primes less than or equal to n (prime-counting function)
# Example: n=500000 => x=41538 (expected), n=1000000 => x=78498
# (Sieve of Eratosthenes, no multiples of 2's are stored)
#
# (It would be possible to put all in a long integer but this is extremely slow)
#
def bench03(n):
  global g_sieve1
  nHalf = n >> 1

  if (len(g_sieve1) != nHalf + 1):
    g_sieve1 = [0] * (nHalf + 1) #[0 for i in range(nHalf + 1)]
    #g_sieve1 = np.zeros((nHalf + 1,), dtype=int)
    #print('DEBUG:'+ str(nHalf + 1) + ', ' + str(type(g_sieve1)))

  sieve1 = g_sieve1

  # initialize sieve
  #sieve1.clear()
  #sieve1 = [0 for i in range(nHalf + 1)]
  for i in range(0, nHalf + 1):
    sieve1[i] = 0

  # compute primes
  i = 0
  m = 3
  x = 1 # number of primes below n (2 is prime)
  while (m * m) <= n:
    if (sieve1[i] == 0):
      x += 1 # m is prime
      j = (m * m - 3) >> 1 # div 2
      while (j < nHalf):
        sieve1[j] = 1
        j += m
    i += 1
    m += 2

  # count remaining primes
  while (m <= n):
    if (sieve1[i] == 0):
      x += 1 # m is prime

    i += 1
    m += 2

  return x


#
# bench04 (Integer 32 bit)
# nth random number number
# Random number generator taken from
# Raj Jain: The Art of Computer Systems Performance Analysis, John Wiley & Sons, 1991, page 442-444.
# It needs longs with at least 32 bit.
# Starting with x0=1, x10000 should be 1043618065, x1000000 = 1227283347.
#
def bench04(n):
  m = 2147483647  # modulus, do not change!
  a = 16807       # multiplier
  q = 127773      # m div a
  r = 2836        # m mod a
  x = 0 # last random value

  x = x + 1 # start with 1=last random value
  for i in range(1, n + 1):
    #x_div_q = x / q;
    #x_mod_q = x % q  # faster than  x_mod_q = x - q * x_div_q
    x = (a * (x % q) - r * (x // q))  # int(a * (x % q) - r * int(x / q))
    #print('DEBUG(bench'+ str(type(x)))
    if (x <= 0):
      x += m  # x is new random number

  return x


# bench05 (Integer 32 bit)
# (n choose n/2) mod 65536 (Central Binomial Coefficient mod 65536)
# Using dynamic programming and Pascal's triangle, storing only one line
# Instead of nCk mod 65536 with k=n/2, we compute the product of (n/2)Ck mod 65536 with k=0..n/4 (Vandermonde folding)
# Example: (2000 choose 1000) mod 65536 = 27200
#
def bench05(n):
  # Instead of nCk with k=n/2, we compute the product of (n/2)Ck with k=0..n/4
  n = int(n / 2)

  k = int(n / 2)
  if ((n - k) < k):
    k = n - k # keep k minimal with  n over k  =  n over n-k

  line = [0 for i in range(k + 1)]

  # initialize (already done)
  #for j in range(0, k + 1):
  #  line[j] = 0

  line[0] = 1
  if (k >= 1):
    line[1] = 2 # for line 2, second column is 2

  x = 0
  # compute lines of Pascal's triangle
  for i in range(3, n + 1):
    min1 = (i - 1) >> 1 #int((i - 1) / 2)
    if ((i & 1) == 0): # new element
      line[min1 + 1] = 2 * line[min1]

    prevElem = line[1]
    for j in range(2, min1 + 1):
      elem = line[j]
      line[j] = (prevElem + line[j]) & 0xffff # we use & 0xffff to avoid (slow) long int
      prevElem = elem

    line[1] = i # second column is i

  # compute sum of ((n/2)Ck)^2 mod 65536 for k=0..n/2
  x = 0
  for j in range(0, k):
    x = (x + 2 * line[j] * line[j]) & 0xffff # add nCk and nC(n-k)

  x = (x + line[k] * line[k]) & 0xffff # we assume that k is even, so we need to take the middle element
  return x

def bench06(n):
  sum = 0.0
  flip = -1.0
  for i in range(1, n + 1):
    flip *= -1.0
    sum += flip / (2*i - 1)
  return int((sum * 4.0) * 100000000)


benchList = [bench00, bench01, bench02, bench03, bench04, bench05, bench06]

#
# run a benchmark
# in: bench = benchmark to use
#     loops = number of loops
#         n = maximum number (used in some benchmarks to define size of workload)
# out:    x = result
#
def run_bench(bench, loops, n, check):
  x = 0

  benchPtr = benchList[bench]

  while loops > 0 and x == 0:
    x = benchPtr(n)
    loops -= 1
    x -= check

  x += check
  if (x != check):
    print('Error(bench'+ str(bench) +'): x='+ str(x))
    x = -1;
  return x


def bench03Check(n):
  if n == 500000:
      x = 41538
  else:
    x = 1
    for j in range(3, n + 1, 2):
      isPrime = True
      i = 3
      while (i * i) <= j:
        if (j % i == 0):
          isPrime = False
          break
        i += 2
      if (isPrime):
        x += 1
  return x


def getCheck(bench, n):
  check = 0
  if bench == 0: # (n / 2) * (n + 1)
    check = (((n + (n & 1)) >> 1) * (n + 1 - (n & 1))) & 0xffff # 10528 for n=1000000
  elif bench == 1:
    check = (n + 1) // 2
  elif bench == 2:
    check = (n + 1) // 2
  elif bench == 3:
    check = bench03Check(n)
  elif bench == 4:
    if n == 1000000:
      check = 1227283347
    else:
      check = bench04(n); # bench04 not a real check
  elif bench == 5:
    if n == 5000:
      check = 17376
    else:
      check = bench05(n); # bench05 not a real check
  elif bench == 6:
    if n == 1000000:
      check = 314159165
    else:
      check = bench06(n); # bench06 not a real check
  else:
    print('Error: Unknown benchmark: '+ str(bench))
    check = -1 # force error
  return check


def get_raw_ts():
  return time.time()

def get_ts():
  return get_raw_ts() - g_startTs

def conv_ms(ts):
    return ts * 1000


def correctTime(tMeas, tMeas2,  measCount):
  tsPrecCnt = g_tsPrecCnt
  #print('DEBUG: tsPrecCnt='+ str(tsPrecCnt))

  if (measCount < tsPrecCnt):
    tMeas += g_tsPrecMs * ((tsPrecCnt - measCount) / tsPrecCnt) # ts + correction
    if (tMeas > tMeas2):
      tMeas = tMeas2 # cannot correct

  return tMeas


def getPrecMs(stopFlg):
  global g_tsMeasCnt
  measCount = 0
  tMeas0 = get_ts()
  tMeas = tMeas0
  while (tMeas == tMeas0):
    tMeas = get_ts()
    measCount += 1

  g_tsMeasCnt = measCount # memorize last count

  if (not stopFlg):
    tMeasD = conv_ms(tMeas)
  else:
    tMeasD = correctTime(conv_ms(tMeas0), conv_ms(tMeas), measCount) # for stop: use first ts + correction
  return tMeasD


def determineTsPrecision():
  global g_tsPrecMs, g_tsPrecCnt, g_startTs
  g_startTs = get_raw_ts() # memorize start time

  tMeas0 = getPrecMs(False)
  tMeas1 = getPrecMs(False)
  g_tsPrecMs = tMeas1 - tMeas0
  g_tsPrecCnt = g_tsMeasCnt

  #do it again
  tMeas0 = tMeas1
  tMeas1 = getPrecMs(False)
  if (g_tsMeasCnt > g_tsPrecCnt): # taker maximum count
    g_tsPrecCnt = g_tsMeasCnt
    g_tsPrecMs = tMeas1 - tMeas0

#
#

def checkbits_short1():
  num = 1 # will get a long integer later...
  last_num = 0
  bits = 0
  while (bits < 101):
    last_num = num
    num <<= 1 # force (short) integer operation (num *= 2)
    num += 1
    bits += 1
    if (((num - 1) / 2) != last_num):
      break
  return bits


def checkbits_int1():
  num = 1 # get's long integer later...
  last_num = 0
  bits = 0
  while (bits < 101):
    last_num = num
    num *= 2
    num += 1
    bits += 1
    if (((num - 1) / 2) != last_num):
      break
  return bits


def checkbits_double1():
  num = 1.0
  last_num = 0.0
  bits = 0
  while (bits < 101):
    last_num = num
    num *= 2.0
    num += 1.0
    bits += 1
    if (((num - 1.0) / 2.0) != last_num):
      break
  return bits


def get_info():
  python_version = sys.version.replace('\n', '')
  str1 = 'BM Bench v%s (%s) -- (short:%d int:%d double:%d' %(G_PRG_VERSION, G_PRG_LANGUAGE, checkbits_short1(), checkbits_int1(), checkbits_double1())
  str1 += ", tsMs:" + str(g_tsPrecMs) + ", tsCnt:" + str(g_tsPrecCnt) + ")"
  str1 += ', version: '+ python_version +'; platform: ' + sys.platform + "\n"
  str1 += '(c) Marco Vieth, 2002-2023' + "\n"
  str1 += 'Date: ' + time.strftime("%Y-%m-%d %H:%M:%S", time.localtime())
    #print('Date:', time.ctime(time.time()))
    # https://docs.python.org/3/library/time.html#module-time
  return str1

def print_results(bench_res1):
  max_language_len1 = 10
  print('\nThroughput for all benchmarks (loops per sec):')
  print('BMR ('+ G_PRG_LANGUAGE +')'+ (' ' * (max_language_len1 - len(G_PRG_LANGUAGE))) + ': ', end=' ')
  #(' ' x ($max_language_len1 - length($G_PRG_LANGUAGE))), ": ";

  for br in bench_res1:
    print("%9.3f " % (br), end=' ')
  print()
  print()


def measureBench(bench, n, check):
  delta_ms = g_delta_ms
  max_ms = 10000 # const
  cali_ms = g_cali_ms
  #print("DEBUG: g_delta_ms=%d delta_ms=%d g_cali_ms=%d cali_ms=%d" % (g_delta_ms, delta_ms, g_cali_ms, cali_ms))

  loops = 1  # number of loops
  x = 0      # result from benchmark
  tMeas = 0     # measured time
  tEsti = 0     # estimated time
  throughput = 0

  print("Calibrating benchmark %d with n=%d, check=%d" % (bench, n, check))
  while (throughput == 0):
    tMeas = getPrecMs(False)
    x = run_bench(bench, loops, n, check)
    tMeas = getPrecMs(True) - tMeas

    if (tEsti > tMeas):
      t_delta = tEsti - tMeas
    else:
      t_delta = tMeas - tEsti # compute difference abs(measures-estimated)

    loops_p_sec = 0
    if (tMeas > 0):
      loops_p_sec = loops * 1000.0 / tMeas

    print("%10.3f/s (time=%9.3f ms, loops=%7d, delta=%9.3f ms)" % (loops_p_sec, tMeas, loops, t_delta))
    if (x == -1): # some error?
      throughput = -1

    elif (tEsti > 0) and (t_delta < delta_ms): # do we have some estimated/expected time smaller than delta_ms=100?
      throughput = loops_p_sec # yeah, set measured loops per sec
      print("Benchmark %d (%s): %.3f/s (time=%9.3f ms, loops=%d, delta=%9.3f ms)" % (bench, G_PRG_LANGUAGE, loops_p_sec, tMeas, loops, t_delta))

    elif (tMeas > max_ms):
      print("Benchmark %d (%s): Time already > %d ms. No measurement possible." % (bench, G_PRG_LANGUAGE, max_ms))
      if (loops_p_sec > 0):
        throughput = -loops_p_sec # cannot rely on measurement, so set to negative
      else:
        throughput = -1

    else:
      scale_fact = 2
      if (tMeas == 0):
        scale_fact = 50
      elif (tMeas < cali_ms):
        scale_fact = int(((cali_ms + 100) / tMeas) + 1) # scale a bit up to 1100 ms (cali_ms+100) (stay with int)
      else:
        scale_fact = 2

      loops *= scale_fact
      tEsti = tMeas * scale_fact

    sys.stdout.flush()
  return throughput


def start_bench(bench1, bench2, n, argStr):
  bench_res = []

  print(get_info())
  if (argStr):
    print("Args: " + argStr)

  for bench in range(bench1, bench2 + 1):
    n2 = n
    # reduce problem size
    if (bench == 3):
      n2 = int(n2 / 2)
    elif (bench == 5):
      n2 = int(n2 / 200)

    check = getCheck(bench, n2)
    if (check > 0):
      throughput = measureBench(bench, n2, check)
    else:
      throughput = -1
    bench_res.append(throughput)

  print_results(bench_res)


def main(argv=[]):
  global g_cali_ms, g_delta_ms
  bench1 = 0          # first benchmark to test
  bench2 = 5          # last benchmark to test
  n = 1000000         # maximum number

  if argv:  # also possible: len(sys.argv) > 0
    if argv[1:]:
      bench1 = int(argv[1]);
      bench2 = bench1

    if argv[2:]:
      bench2 = int(argv[2]);

    if argv[3:]:
      n = int(argv[3]);

    if argv[4:]:
      g_cali_ms = int(argv[4]);
    
    if argv[5:]:
      g_delta_ms = int(argv[5]);
      print("DEBUG: g_delta_ms=%d" % (g_delta_ms));

  determineTsPrecision()
  argStr = " ".join(argv[1:])
  start_bench(bench1, bench2, n, argStr)
  print("Total elapsed time: %d ms" % conv_ms(get_ts()))


# Run test program when run as a script
if __name__ == '__main__':
  import sys
  if len(sys.argv) > 1 or sys.stdin.isatty():
    main(sys.argv)
  else:
    main(("argv0 " + sys.stdin.readline().rstrip()).rstrip().split(' '))

#
# https://replit.com/languages/python3
#

# end
