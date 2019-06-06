#! /usr/bin/env python
# BM Bench - bmbench.py (Python)  version 3
# (c) Marco Vieth, 2002-2006
# http://www.benchmarko.de
#
# 06.05.2002 0.01
# 11.05.2002 0.02  bench01 = (sum 1..n) mod 65536
# 20.07.2002 0.04  some errors corrected
# 24.01.2003 0.05  output format changed
# 03.12.2006 0.06  based on version 0.05
# 05.05.2019 0.07  changed bench 01-03; time interval estimation
#
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

G_PRG_VERSION = "0.07"
G_PRG_LANGUAGE = "Python"


g_startTs = 0
g_tsPrecMs = 0 # measured time stamp precision
g_tsPrecCnt = 0 # time stamp count (calls) per precision interval (until time change)
g_tsMeasCnt = 0 # last measured count

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
# bench00 (Integer 16 bit) -- adapt!! TODO
# (sum of 1..n) mod 65536
#
def bench00(loops, n, check1):
  x = 0
  #sum1 = int((n / 2) * (n + 1)) & 0xffff # this produces an integer
  # sum1..1000000 depends on type: 500000500000 (floating point), 1784293664 (32bit), 10528 (16 bit)
  n_div_65536 = (n >> 16) & 0xffff
  n_mod_65536 = n & 0xffff;
  #print 'DEBUG: sum1='+ str(sum1) +', n_div='+ str(n_div_65536) +', n_mod='+ str(n_mod_65536)
  while loops > 0 and x == 0:
    loops = loops - 1

    # simulate summation with 16 bit borders...
    for i in range(n_div_65536, 0, -1):
      for j in range(65535, 0, -1):
        x += j
    for j in range(n_mod_65536, 0, -1):
      x += j

    x &= 0xffff
    x -= check1
  return int(x & 0xffff)


# bench01 (Integer 32 bit)
# (average of 1..n)
#
def bench01(loops, n, check):
  x = 0
  while loops > 0 and x == 0:
    loops = loops - 1
    sum = 0
    for i in range(1, n + 1):
      sum += i
      if (sum >= n): # to avoid numbers above 2*n, divide by n using subtraction
        sum -= n
        x += 1

    x -= check
  return x


#
# bench02 (Floating Point, normally 64 bit)
# (average of 1..n)
#
def bench02(loops, n, check):
  x = 0
  while loops > 0 and x == 0:
    loops = loops - 1
    sum = 0.0
    for i in range(1, n + 1):
      sum += i
      if (sum >= n):
        sum -= n
        x += 1

    x -= check
  return x


#
# bench03 (Integer)
# number of primes below n (Sieve of Eratosthenes)
# Example: n=500000 => x=41538 (expected), n=1000000 => x=78498
#
# (It would be possible to put all in a long integer but this is extremely slow.)
#
def bench03(loops, n, check):
  n = int(n / 2)  # compute only up to n/2
  x = 0      # number of primes below n
  nHalf = n >> 1
  #sieve1 = []  #nHalf + 1 elements

  while loops > 0 and x == 0:
    loops = loops - 1
    # initialize sieve
    #sieve1.clear()
    #print(len(sieve1))
    sieve1 = [0 for i in range(nHalf + 1)]
    #for i in range(0, nHalf + 1):
    #  sieve1.append(0)

    # compute primes
    i = 0
    m = 3
    while (m * m) < n:
      if (sieve1[i] == 0):
        x += 1
        j = (m * m - 3) >> 1 # div 2
        while (j < nHalf):
          sieve1[j] = 1
          j += m
      i += 1
      m += 2

    # count remaining primes
    while (i <= nHalf):
      if (sieve1[i] == 0):
        x += 1
      i += 1

    x -= check
  return x


#
# bench04 (Integer 32 bit)
# nth random number number
# Random number generator taken from
# Raj Jain: The Art of Computer Systems Performance Analysis, John Wiley & Sons, 1991, page 442-444.
# It needs longs with at least 32 bit.
# Starting with x0=1, x10000 should be 1043618065, x1000000 = 1227283347.
#
def bench04(loops, n, check):
  m = 2147483647  # modulus, do not change!
  a = 16807       # multiplier
  q = 127773      # m div a
  r = 2836        # m mod a
  x = 0 # last random value

  while loops > 0 and x == 0:
    loops = loops - 1
    x = x + 1 # start with 1=last random value
    for i in range(1, n + 1):
      #x_div_q = x / q;
      #x_mod_q = x % q  # faster than  x_mod_q = x - q * x_div_q
      x = (a * (x % q) - r * (x // q))  # int(a * (x % q) - r * int(x / q))
      #print('DEBUG(bench'+ str(type(x)))
      if (x <= 0):
        x += m  # x is new random number

    #print('DEBUG(bench'+ str(4) +'): x='+ str(x*2))
    x -= check

  return x


#
# bench05 (Integer 32 bit)
# n over n/2 mod 65536 (Pascal's triangle)
# We start with an empty list and append elements.
#
def bench05(loops, n, check):
  x = 0
  n = int(n / 500)
  k = int(n / 2)

  if ((n - k) < k):
    k = n - k # keep k minimal with  n over k  =  n over n-k

  while loops > 0 and x == 0:
    loops = loops - 1
    pas1 = [1]
    for i in range(2, n + 1):
      pas2 = pas1 # get last line to pas2
      pas1 = [1] # and restart with new list

      min1 = (i - 1) >> 1 #int((i - 1) / 2)
      if (k < min1):
        min1 = k

      pas1.append(i)

      for j in range(2, min1 + 1):
        pas1.append((pas2[j - 1] + pas2[j]) & 0xffff) # we use & 0xffff to avoid (slow) long int

      if (min1 < k) and (i % 2 == 0): # new element
        pas1.append(2 * pas2[min1])

    x += pas1[k] & 0xffff;

    x -= check

  return x


#
# run a benchmark
# in: bench = benchmark to use
#     loops = number of loops
#         n = maximum number (used in some benchmarks to define size of workload)
# out:    x = result
#
def run_bench(bench, loops, n):
  x = 0
  #check = 0
  if bench == 0:
    check = int((n // 2) * (n + 1)) & 0xffff #10528
    x = bench00(loops, n, check)
  elif bench == 1:
    check = (n + 1) // 2
    x = bench01(loops, n, check)
  elif bench == 2:
    check = (n + 1) // 2
    x = bench02(loops, n, check)
  elif bench == 3:
    check = 41538
    x = bench03(loops, n, check)
  elif bench == 4:
    check = 1227283347
    x = bench04(loops, n, check)
  elif bench == 5:
    check = 27200
    x = bench05(loops, n, check)
  else:
    print('Error: Unknown benchmark: '+ str(bench))
    check = -1 # force error

  x += check
  if (check != x):
    print('Error(bench'+ str(bench) +'): x='+ str(x))
    x = -1; # exit
  return x



def get_raw_ts():
  return time.time()

def get_ts():
  #global g_startTs
  return get_raw_ts() - g_startTs

def conv_ms(ts):
    return ts * 1000

#
# get timestamp in milliseconds
# out: x = time in ms
#
#def get_ms_xxx():
#  return time.time() * 1000


#
#

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
  global g_tsPrecMs
  global g_tsPrecCnt
  global g_startTs
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


def print_info():
  global g_tsPrecMs
  python_version = sys.version.replace('\n', '')
  print('BM Bench v%s (%s) -- (short:%d int:%d double:%d' %(G_PRG_VERSION, G_PRG_LANGUAGE, checkbits_short1(), checkbits_int1(), checkbits_double1()), end=' ')
  print("tsMs:" + str(g_tsPrecMs), "tsCnt:" + str(g_tsPrecCnt) + ")", end=' ')
  print('version: '+ python_version +'; platform:', sys.platform)
  print('(c) Marco Vieth, 2006-2019')
  print('Date:', time.ctime(time.time()))


def print_results(bench_res1):
  max_language_len1 = 10
  print('\nThroughput for all benchmarks (loops per sec):')
  print('BMR ('+ G_PRG_LANGUAGE +')'+ (' ' * (max_language_len1 - len(G_PRG_LANGUAGE))) + ': ', end=' ')
  #(' ' x ($max_language_len1 - length($G_PRG_LANGUAGE))), ": ";

  for br in bench_res1:
    print("%9.3f " % (br), end=' ')
  print()
  print()


def measureBench(bench, n):
  cali_ms = 1001 # const
  delta_ms = 100 # const
  max_ms = 10000 # const
  loops = 1  # number of loops

  x = 0      # result from benchmark
  tMeas = 0     # measured time
  tEsti = 0     # estimated time
  throughput = 0

  print("Calibrating benchmark %d with n=%d" % (bench, n))
  while (throughput == 0):
    tMeas = getPrecMs(False)
    x = run_bench(bench, loops, n)
    tMeas = getPrecMs(True) - tMeas

    if (tEsti > tMeas):
      t_delta = tEsti - tMeas
    else:
      t_delta = tMeas - tEsti # compute difference abs(measures-estimated)

    loops_p_sec = 0
    if (tMeas > 0):
      loops_p_sec = loops * 1000.0 / tMeas

    print("%10.3f/s (time=%9.3f ms, loops=%7d, delta=%9.3f ms, x=%d)" % (loops_p_sec, tMeas, loops, t_delta, x))
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
      # scale_fact = 2
      # if ((tMeas < cali_ms) and (tMeas > 0)):
      #   scale_fact = int(((cali_ms + 100) / tMeas) + 1) # scale a bit up to 1100 ms (cali_ms+100)
      scale_fact = 2
      if (tMeas == 0):
        scale_fact = 50
      elif (tMeas < cali_ms):
        #scale_fact = ((cali_ms + 100) / tMeas) # scale a bit up to 1100 ms (cali_ms+100)
        scale_fact = int(((cali_ms + 100) / tMeas) + 1) # scale a bit up to 1100 ms (cali_ms+100) (stay with int)
      else:
        scale_fact = 2

      loops *= scale_fact
      tEsti = tMeas * scale_fact

    sys.stdout.flush()
  return throughput


def start_bench(bench1, bench2, n):
  bench_res = []

  print_info()

  for bench in range(bench1, bench2 + 1):
    throughput = measureBench(bench, n)
    bench_res.append(throughput)

  print_results(bench_res)


def main(argv=[]):
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

  determineTsPrecision()
  start_bench(bench1, bench2, n)
  print("Total elapsed time: %d ms" % conv_ms(get_ts()))



# Run test program when run as a script
if __name__ == '__main__':
  import sys
  main(sys.argv)

# end
