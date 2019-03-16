#! /usr/bin/env python
# BM Bench - bmbench.py (Python)
# (c) Marco Vieth, 2002
# http://www.benchmarko.de
#
# 06.05.2002  0.01
# 11.05.2002  0.02  bench01 = (sum 1..n) mod 65536
# 20.07.2002  0.04  some errors corrected
# 24.01.2003  0.05  output format changed
#
#
# Usage:
# python -O bmbench.py [bench1] [bench2] [n]
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
# Data types are nearly implicit:
# - integer (32 bit), if it gets too long -> long integer (any number of bits, marked with L)
# - floating point (marked with dot)
#

#
# Explicit while loop is slower than for i in range()
#

import time


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
# bench00 (Integer 16 bit) -- adapt!!
# (sum of 1..n) mod 65536
#
def bench00(loops, n):
  x = 0
  sum1 = ((n / 2) * (n + 1)) & 0xffff # this produces an integer
  # sum1..1000000 depends on type: 500000500000 (floating point), 1784293664 (32bit), 10528 (16 bit)
  n_div_65536 = (n >> 16) & 0xffff
  n_mod_65536 = n & 0xffff;
  #print 'DEBUG: sum1='+ str(sum1) +', n_div='+ str(n_div_65536) +', n_mod='+ str(n_mod_65536)
  while loops > 0:
    loops = loops - 1

    # simulate summation with 16 bit borders...
    for i in range(n_div_65536, 0, -1):
      for j in range(65535, 0, -1):
        x += j
    for j in range(n_mod_65536, 0, -1):
      x += j

    x &= 0xffff

    if (loops > 0):   # some more loops left
      x -= sum1       # yes, set x back to 0 (assuming n even)
      if x != 0:      # now x must be 0 again
        x = x + 1     # force error for many wrong computations
        break         # raise ValueError, "bench01: x=" + str(x)
  return int(x & 0xffff)


#
# bench01 (Integer 16/32 bit)
# (sum of 1..n) mod 65536
#
def bench01(loops, n):
  x = 0
  sum1 = ((n / 2) * (n + 1))  # this produces a (slow) long integer which has arbitrary length
  #print 'sum1='+ str(sum1)
  while loops > 0:
    loops = loops - 1
    for i in range(1, n + 1):
      x += i;
    if (loops > 0):   # some more loops left
      x -= sum1       # yes, set x back to 0 (assuming n even)
      if x != 0:      # now x must be 0 again
        x = x + 1     # force error for many wrong computations
        break         # raise ValueError, "bench01: x=" + str(x)
  return int(x & 0xffff)


#
# bench02 (Floating Point, normally 64 bit)
# (sum of 1..n) mod 65536
#
def bench02(loops, n):
  x = 0.0
  sum1 = (n / 2.0) * (n + 1.0)
  while loops > 0:
    loops = loops - 1
    for i in range(1, n + 1):
      x += i;
    if (loops > 0):   # some more loops left
      x -= sum1       # yes, set x back to 0 (assuming n even)
      if x != 0.0:      # now x must be 0 again
        x = x + 1     # force error for many wrong computations
        break         # raise ValueError, "bench01: x=" + str(x)
  return int(x % 65536)


#
# bench03 (Integer)
# number of primes below n (Sieve of Eratosthenes)
# Example: n=500000 => x=41538 (expected), n=1000000 => x=78498
#
# (It would be possible to put all in a long integer but this is extremely slow.)
#
def bench03(loops, n):
  n = n / 2  # compute only up to n/2
  x = 0      # number of primes below n
  sieve1 = []  # [] = new boolean[n + 1];
  sieve1.append(0)
  sieve1.append(0)
  while loops > 0:
    loops = loops - 1
    # initialize sieve
    for i in range(2, n + 1):
      sieve1.append(1)

    # compute primes
    i = 2
    while (i * i) <= n:
      if (sieve1[i]):
        for j in range(i * i, n + 1, i):
          sieve1[j] = 0
      i = i + 1

    # count primes
    for i in range(0, n + 1):
      if (sieve1[i]):
        x = x + 1

    # check prime count
    if (loops > 0):   # some more loops left
      x -= 41538      # yes, set x back to 0 (assuming n even)
      if x != 0:      # now x must be 0 again
        x = x + 1     # force error for many wrong computations
        break         # raise ValueError, "bench01: x=" + str(x)

  return x


#
# bench04 (Integer 32 bit)
# nth random number number
# Random number generator taken from
# Raj Jain: The Art of Computer Systems Performance Analysis, John Wiley & Sons, 1991, page 442-444.
# It needs longs with at least 32 bit.
# Starting with x0=1, x10000 should be 1043618065, x1000000 = 1227283347.
#
def bench04(loops, n):
  m = 2147483647  # modulus, do not change!
  a = 16807       # multiplier
  q = 127773      # m div a
  r = 2836        # m mod a
  x = 1 # last random value
  while loops > 0:
    loops = loops - 1
    for i in range(1, n + 1):
      #x_div_q = x / q;
      #x_mod_q = x % q  # faster than  x_mod_q = x - q * x_div_q
      x = a * (x % q) - r * (x / q)
      if (x <= 0):
        x += m  # x is new random number

    if (loops > 0):   # some more loops left
      x -= 1227283347 # yes, set x back to 0 (assuming n even)
      if x != 0:      # now x must be 0 again
        x = x + 1     # force error for many wrong computations
        break         # raise ValueError, "bench01: x=" + str(x)
      x = x + 1  # start with 1 again

  return x


#
# bench05 (Integer 32 bit)
# n over n/2 mod 65536 (Pascal's triangle)
# We start with an empty list and append elements.
#
def bench05(loops, n):
  x = 0
  n = n / 500
  k = n / 2

  if ((n - k) < k):
    k = n - k # keep k minimal with  n over k  =  n over n-k

  while loops > 0:
    loops = loops - 1
    pas1 = [1]
    for i in range(2, n + 1):
      pas2 = pas1 # get last line to pas2
      pas1 = [1] # and restart with new list

      min1 = (i - 1) / 2
      if (k < min1):
        min1 = k

      pas1.append(i)

      for j in range(2, min1 + 1):
        pas1.append((pas2[j - 1] + pas2[j]) & 0xffff) # we use & 0xffff to avoid (slow) long int

      if (min1 < k) and (i % 2 == 0): # new element
        pas1.append(2 * pas2[min1])

    x += pas1[k] & 0xffff;

    if (loops > 0):   # some more loops left
      x -= 27200      # yes, set x back to 0 (assuming n even)
      if x != 0:      # now x must be 0 again
        x = x + 1     # force error for many wrong computations
        break         # raise ValueError, "bench01: x=" + str(x)

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
  check1 = 0
  if bench == 0:
    x = bench00(loops, n)
    check1 = 10528
  elif bench == 1:
    x = bench01(loops, n)
    check1 = 10528
  elif bench == 2:
    x = bench02(loops, n)
    check1 = 10528
  elif bench == 3:
    x = bench03(loops, n)
    check1 = 41538
  elif bench == 4:
    x = bench04(loops, n)
    check1 = 1227283347
  elif bench == 5:
    x = bench05(loops, n)
    check1 = 27200
  else:
    print 'Error: Unknown benchmark: '+ str(bench)
    check1 = x + 1  # force error

  if (check1 != x):
    print 'Error(bench'+ str(bench) +'): x='+ str(x)
    x = -1; # exit
  return(x)


#
# get timestamp in milliseconds
# out: x = time in ms
#
def get_ms():
  return time.time() * 1000


def checkbits_short1():
  num = 1 # get's long integer later...
  last_num = 0
  bits = 0
  while (bits < 101):
    last_num = num
    num <<= 1 # force (short) intrger operation (num *= 2)
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



def main(argv=[]):
  start_t = get_ms()  # memorize start time
  bench1 = 0          # first benchmark to test
  bench2 = 5          # last benchmark to test
  n = 1000000         # maximum number
  min_ms = 10000      # minimum runtime for measurement in ms
  bench_res1 = []

  if argv:  # also possible: len(sys.argv) > 0
    if argv[1:]:
      bench1 = int(argv[1]);
      bench2 = bench1

    if argv[2:]:
      bench2 = int(argv[2]);

    if argv[3:]:
      n = int(argv[3]);

  python_version = sys.version.replace('\n', '')
  print 'BM Bench v0.5 (Python) -- (short:%d int:%d double:%d)' %(checkbits_short1(), checkbits_int1(), checkbits_double1()),
  print 'version: '+ python_version +'; platform:', sys.platform
  print '(c) Marco Vieth, 2002'
  print 'Date:', time.ctime(time.time())

  for bench in range(bench1, bench2 + 1):
    loops = 1  # number of loops
    x = 0      # result from benchmark
    t1 = 0     # timestamp
    # calibration
    while (t1 < 1001) and (x != -1):  # we want at least 1001 ms calibration time
      print "Calibrating benchmark %d with loops=%d, n=%d" % (bench, loops, n)
      t1 = get_ms()
      x = run_bench(bench, loops, n)
      t1 = get_ms() - t1
      print "x=%d (time: %d)" % (x, t1)
      loops *= 2

    if (x != -1):
      loops /= 2
      loops *= int(min_ms / t1) + 1  # integer division!
      print "Calibration done. Starting measurement with %d loops to get >=%d ms" % (loops, min_ms)

      # measurement
      t1 = get_ms()
      x = run_bench(bench, loops, n)
      t1 = get_ms() - t1
      print "x=%d (time: %d)" % (x, t1)
      bench_res1.append(int(t1 * 10 / loops))
      print "Elapsed time for %d loops: %d ms; estimation for 10 loops: %d ms" % (loops, t1, bench_res1[bench - bench1])
    else:
      bench_res1.append(-1)

  print 'Times for all benchmarks (10 loops, ms):'
  print 'BM Results (Python)    :',
  for br in bench_res1:
    print "%7d" % (br),
  print

  print "Total elapsed time: %d ms" % (get_ms() - start_t)


# Run test program when run as a script
if __name__ == '__main__':
  import sys
  main(sys.argv)

# end
