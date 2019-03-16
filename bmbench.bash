#!/usr/bin/bash -f
# BM Bench - bmbench.bash (bash or zsh)
# (c) Marco Vieth, 2002
# http://www.benchmarko.de
#
# 05.09.2003  0.01
# 06.09.2003  0.05 all benchmark tests 0..5 implemented but arrays are very slow...
#
#
# Usage:
# bash bmbench.bash [bench1] [bench2] [n]
#   or
# zsh bmbench.bash [bench1] [bench2] [n]
#
#

#
# Bash performance
#- For loop is faster than while
#  'for (( i = n ; i ; )); do ((x += i--)); done;'  is faster than
#  'for (( i = n ; i ; i-- )); do ((x += i)); done;'  is faster than
#  '((i = n)); while [ $i -gt 0 ]; do ((x += i--)); done;'
# - expressions: implicit value on integers 'i' is faster than '$i'
#
#

# Note: bash does only integer, zsh can both integer and fp!
# zsh has very slow arrays (seems to be exponential time to access high indices)


#
# zsh:
#zrecompile -p -U -z $zwc $files
#

# Bash:
#
# - Arithmetic
# '((expression))' or 'let "expression"'
# - Conditions
# '[[ expression ]]'
# - Arrays
# Bash provides one-dimensional array variables. Any variable may be used as an array; the declare builtin will explicitly declare an array.
# 'declare -a name' ;   readonly builtins
# set: 'name[subscript]=value' ; get: '${name[subscript]}' ; length: '${#name[subscript]}'
# - Types, variables
# 'declare [-afFirtx] [-p] [name[=value]]' or 'typeset [-afFirtx] [-p] [name[=value]]'
# 'local [option] [name[=value] ...]'
# 'readonly [-apf] [name ...]'
# - Flow control
# if  list;  then list; [ elif list; then list; ] ... [ else list; ] fi
# while list; do list; done
# for (( expr1 ; expr2 ; expr3 )) ; do list ; done
# - Functions
# [ function ] name () { list; }
#
#


####################################


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
bench00() {
  local loops=$1 n=$2;
  typeset -i sum1 n_div_65536 n_mod_65536 i j;
  local sum1=0 n_div_65536=0 n_mod_65536=0 i=0 j=0;
  x=0; # global!
  ((sum1=((n / 2) * (n + 1)) % 65536)); # assuming n even!
  # (sum1..1000000 depends on type: 500000500000 (floating point), 1784293664 (32bit), 10528 (16 bit)
  ((n_div_65536=(n / 65536)));
  ((n_mod_65536=(n % 65536)));

  for (( ; loops-- ; )); do
    for (( i = n_div_65536 ; i ; i-- )); do
      for (( j = 65535 ; j ; )); do
       ((x += j--));
      done;
    done;
    for (( j = n_mod_65536 ; j ; )); do
     ((x += j--));
    done;
    ((x %= 65536));
    if [ "$loops" -gt 0 ]; then # some more loops left?
      ((x -= sum1));     # yes, set x back to 0 (assuming n even)
      if [ "$x" -ne 0 ]; then # now x must be 0 again
        ((x++));
        break;       # error
      fi;
    fi;
  done;
  ((x %= 65536));
  # returns x
}


#
# bench01 (Integer 16/32 bit)
# (sum of 1..n) mod 65536
#
bench01() {
#in: loops, n
  local loops=$1 n=$2;
  typeset -i sum1 i;
  local sum1=0 i=0;
  ((sum1=(n / 2) * (n + 1))); # assuming n even!
  # (sum1..1000000 depends on type: 500000500000 (floating point), 1784293664 (32bit), 10528 (16 bit)
  x=0;
  for (( ; loops-- ; )); do
    #echo "DEBUG: bench01: loops=$loops, sum1=$sum1"
    for (( i = n ; i ; )); do ((x += i--)); done;
    #echo "DEBUG: bench01: x=$x"
    if [ "$loops" -gt 0 ]; then # some more loops left?
      ((x -= sum1));     # yes, set x back to 0 (assuming n even)
      if [ "$x" -ne 0 ]; then # now x must be 0 again
        ((x++));
        break;       # error
      fi;
    fi;
  done;
  ((x %= 65536));
  # returns x
}


#
# bench02 (Floating Point, normally 64 bit)
# (sum of 1..n) mod 65536
#
bench02_zsh() {
#in: loops, n
  typeset -i i;
  local loops=$1 n=$2;
  local sum1_f x_f i;
  ((sum1_f=(n / 2.0) * (n + 1.0))); # assuming n even!
  # (sum1..1000000 depends on type: 500000500000 (floating point), 1784293664 (32bit), 10528 (16 bit)
  x_f=0.0;
  for (( ; loops-- ; )); do
    for (( i = n ; i ; )); do ((x_f += i--)); done;
    if [ "$loops" -gt 0 ]; then # some more loops left?
      ((x_f -= sum1_f));     # yes, set x back to 0 (assuming n even)
      if [ "$x_f" -ne 0.0 ]; then # now x must be 0 again
        ((x_f++));
        break;       # error
      fi;
    fi;
  done;
  ((x_f %= 65536.0));
  x=x_f;
  # returns x
}


bench02_bash() {
#in: loops, n
  typeset -i i;
  local loops=$1 n=$2;
  local sum1_f x_f i;
  ((sum1_f=(n / 2) * (n + 1))); # assuming n even!
  # (sum1..1000000 depends on type: 500000500000 (floating point), 1784293664 (32bit), 10528 (16 bit)
  x_f=0;
  for (( ; loops-- ; )); do
    for (( i = n ; i ; )); do ((x_f += i--)); done;
    if [ "$loops" -gt 0 ]; then # some more loops left?
      ((x_f -= sum1_f));     # yes, set x back to 0 (assuming n even)
      if [ "$x_f" -ne 0 ]; then # now x must be 0 again
        ((x_f++));
        break;       # error
      fi;
    fi;
  done;
  ((x_f %= 65536));
  x=x_f;
  # returns x
}


g_fact_bench03=20 # benchmark reduction factor
g_result_bench03=2762; #result for factor 20 (for factor 1 it would be 41538)

# bench03 (Integer)
# number of primes below n (Sieve of Eratosthenes)
# Example: n=500000 => x=41538 (expected), n=1000000 => x=78498
# (No bit array available but zsh as max array index of 262144 (and gets very slow), so simulate bit array...)
bench03() {
#in: loops, n
  local loops=$1 n=$2;
  typeset -i i j test1;
  local i=0 j=0 test1=0;

  ((n /= g_fact_bench03)); # reduce by a factor
  echo "Note: $g_sh has a very slow array implementation, so reduce n by factor $g_fact_bench03 to $n."

  ((n /= 2)); # compute only up to n/2
  x=0; # number of primes below n
  typeset -a sieve1;
  #local sieve1; # do we need local??
  i=0; ((sieve1[i>>3] &= ~(1 << (i & 7))));
  i=1; ((sieve1[i>>3] &= ~(1 << (i & 7))));

  for (( ; loops-- ; )); do
    # initialize sieve
    for (( i = n; i >= 2; i--)); do
      ((sieve1[i>>3] |= (1 << (i & 7))));
      #echo "DEBUG: init i=$i, i>>3=$((i>>3)), sieve1=$((sieve1[i>>3]))";
    done;

    # compute primes
    for ((i = 2; (i * i) <= n; i++)); do
      ((test1=(sieve1[i>>3] & (1 << (i & 7))) ));
      #echo "DEBUG: i=$i, i>>3=$((i>>3)), sieve1=$((sieve1[i>>3])), test1=$test1";
      if [ "$test1" -gt 0 ]; then
        #echo "DEBUG: $i is prime.";
        for ((j = i * i; j <= n; j += i)); do
          ((sieve1[j>>3] &= ~(1 << (j & 7))));
        #echo "DEBUG: j=$j, j>>3=$((j>>3)), sieve1=$((sieve1[j>>3])): $j no prime (multiple of $i)";
        done;
      fi;
    done;

    # count primes
    for ((i = n; i > 0; i--)); do
      ((test1=(sieve1[i>>3] & (1 << (i & 7))) ));
      if [ "$test1" -gt 0 ]; then
        ((x++));
        #echo "DEBUG: $i is prime.";
      fi;
    done;

    # check prime count
    if [ "$loops" -gt 0 ]; then # some more loops left?
      ((x -= g_result_bench03));     # yes, set x back to 0 (number of primes below 1000000)
      if [ "$x" -ne 0 ]; then # now x must be 0 again
        ((x++));
        break;       # error
      fi;
    fi;
  done;
  # returns x
}


g_fact_bench04=4; # benchmark reduction factor
g_result_bench04=838931758; #result for factor 4 (for factor 1 it would be 1227283347)

#
# bench04 (Integer 32 bit)
# nth random number number
# Random number generator taken from
# Raj Jain: The Art of Computer Systems Performance Analysis, John Wiley & Sons, 1991, page 442-444.
# It needs longs with at least 32 bit.
# Starting with x0=1, x10000 should be 1043618065, x1000000 = 1227283347.
#
bench04() {
#in: loops, n
  local loops=$1 n=$2;

  ((n /= g_fact_bench04)); # reduce by a factor
  echo "Note: $g_sh has a very slow computations, so reduce n by factor $g_fact_bench04 to $n."

  typeset -i bench04_M bench04_A bench04_Q bench04_R;
  local bench04_M=2147483647; # modulus, do not change!
  local bench04_A=16807;      # multiplier
  local bench04_Q=127773;     # m div a
  local bench04_R=2836;       # m mod a

  typeset -i i x_div_q x_mod_q;
  local i=0 x_div_q=0 x_mod_q=0;

  x=1;                # last random value
  for (( ; loops-- ; )); do
    for (( i = 1 ; i <= n ; i++ )); do
      ((x_div_q = (x / bench04_Q))); # int
      ((x_mod_q = x - bench04_Q * x_div_q));
      ((x = bench04_A * x_mod_q - bench04_R * x_div_q));
      if [ "$x" -le 0 ]; then
        ((x += bench04_M)); # x is new random number
      fi;
    done;
    if [ "$loops" -gt 0 ]; then # some more loops left?
      ((x -= g_result_bench04));     # now x must be 0
      if [ "$x" -ne 0 ]; then # now x must be 0 again
        ((x++));
        break;       # error
      fi;
      ((x++)); # start with 1 again
    fi;
  done;
  # returns x
}


g_fact_bench05=2; # benchmark reduction factor
g_result_bench05=43584; #result for factor 2 (for factor 1 it would be 27200)

#
# bench05 (Integer 32 bit) (list implementation)
# n over n/2 mod 65536 (Pascal's triangle)
# (we just need to store the last 2 lines of computation)
#
bench05() {
#in: loops, n
  local loops=$1 n=$2;

  ((n /= g_fact_bench05)); # reduce by a factor
  echo "Note: $g_sh has a very slow array implementation, so reduce n by factor $g_fact_bench05 to $n."

  typeset -i k pas1 pas2 i min1 j test1;
  local k=0 pas1=0 pas2=0 i=0 min1=0 j=0 test1=0;

  x=0;

  ((n = (n / 500))); # int
  ((k = (n / 2))); # int

  if [ $((n - k)) -lt $k ]; then
    ((k = n - k)); # keep k minimal with  n over k  =  n over n-k
  fi;

  typeset -a pas1;
  typeset -a pas2;
  #pas1 = ();
  #pas2 = ();

  for (( ; loops-- ; )); do
    pas1[0]=1;
    #min1=1;
    for (( i = 2 ; i <= n ; i++)); do

      #echo "DEBUG: i=$i, pas1=$#pas1";
      # get last line to pas2 (pas2 = pas1) ...
      #delete pas2; # don't need this because we just grow and delete pas1...
      ((min1++)); # set to maximum used index in pas1 (one too high if no new element)
      for ((j = 0; j <= min1; j++)); do
        #echo "DEBUG: copy element $j: ${pas1[$j]}";
        ((pas2[j]=pas1[j])); # copy elements from pas2 into pas1
      done;

      # (We do not need to delete the array since we overwrite all elements...)
      pas1[0]=1; # and restart with new list
      ((min1 = ((i - 1) / 2))); # int(...)
      if [ "$k" -lt "$min1" ]; then
        min1=$k;
      fi;
      pas1[1]=$i; # second column is i
      for ((j = 2; j <= min1; j++)); do # up to min((i-1)/2, k)
        ((pas1[j] = (pas2[j - 1] + pas2[j]) % 65536)); # modulus to avoid nan
      done;
      ((test1=((min1 < k) && ((i % 2) == 0)))); # new element?
      if [ "$test1" -gt 0 ]; then # new element
        ((pas1[j] = 2 * pas2[min1]));
      fi;
    done;
    ((x += pas1[k] % 65536));
    #echo "DEBUG: bench05: x=$x"
    if [ "$loops" -gt 0 ]; then # some more loops left?
      ((x -= g_result_bench05)); # normally 27200
      if [ "$x" -ne 0 ]; then # now x must be 0 again
        ((x++));
        break;       # error
      fi;
    fi;
  done;
  # returns x
}


#
# run a benchmark
# in: bench = benchmark to use
#     loops = number of loops
#         n = maximum number (used in some benchmarks to define size of workload)
# out:    x = result
#
run_bench() {
#(bench, loops, n,    x, check1) {
  local bench=$1 loops=$2 n=$3;
  typeset -i check1;
  local check1=0;
  x=0;
  #echo "DEBUG: run_bench";
  case "$bench" in
    0)
      bench00 $loops $n; # special version optimized for 16 bit
      check1=10528;
    ;;

    1)
      bench01 $loops $n;
      check1=10528;
    ;;

    2)
      bench02_$g_sh $loops $n; # for bash or zsh
      check1=10528;
    ;;

    3)
      bench03 $loops $n;
      check1=$g_result_bench03;
    ;;

    4)
      bench04 $loops $n;
      check1=g_result_bench04; # normally 1227283347
    ;;

    5)
      bench05 $loops $n;
      check1=g_result_bench05; # normally 27200; # 58336; 43584;
    ;;

    *)
      echo "Error: Unknown benchmark: $bench";
      ((check1=x+1)); # force error
    ;;
  esac;
  if [ "$check1" -ne "$x" ]; then
    echo "Error(bench$bench): x=$x";
    x=-1; # exit
  fi;
}


get_ms() {
  get_ms=$SECONDS;
  ((get_ms *= 1000));
}


# Here we compute the number of "significant" bits for positive numbers (which means 53 for double)
checkbits_int1() {
# num, last_num, bits
  typeset -i num last_num num2 bits;
  local num=1 last_num=0 num2=0 bits=0;
  #while [ ((($num - 1) / 2) -eq $last_num) && ($bits < 101) ]; do
  while [ 1 ]; do
    last_num=$num;
    ((num *= 2));
    ((num++));
    ((bits++));
    ((num2=(num-1)/2));
    if [ "$num2" -ne "$last_num" ] || [ "$bits" -ge 101 ]; then
      break;
    fi;
  done;
  checkbits_int1=$bits;
}


checkbits_double1_bash() {
# num, last_num, bits
  typeset -i bits;
  local num_f=1 last_num_f=0 num2_f=0;
  local bits=0;
  #while [ ((($num - 1) / 2) -eq $last_num) && ($bits < 101) ]; do
  while [ 1 ]; do
    last_num_f=$num_f;
    ((num_f *= 2));
    ((num_f++));
    ((bits++));
    ((num2_f=(num_f - 1) / 2));
    if [ "$num2_f" -ne "$last_num_f" ] || [ "$bits" -ge 101 ]; then
      break;
    fi;
  done;
  checkbits_double1=$bits;
}


# for zsh we have real floating point...
checkbits_double1_zsh() {
# num, last_num, bits
  typeset -i bits;
  local num_f=1.0 last_num_f=0.0 num2_f=0.0;
  local bits=0;
  #while [ ((($num - 1) / 2) -eq $last_num) && ($bits < 101) ]; do
  while [ 1 ]; do
    last_num_f=$num_f;
    ((num_f *= 2.0));
    ((num_f++));
    ((bits++));
    ((num2_f=(num_f - 1.0) / 2.0));
    if [ "$num2_f" -ne "$last_num_f" ] || [ "$bits" -ge 101 ]; then
      break;
    fi;
  done;
  checkbits_double1=$bits;
}



main() {
  typeset -i argc n min_ms;
  typeset -i start_t bench1 bench2 bench loops x t1;
  local argc=$#;
  #local argv2=$@;

  local start_t=0;      # memorize start time
  local bench1=0;       # first benchmark to test
  local bench2=5;       # last benchmark to test
  local n=1000000;      # maximum number
  local min_ms=10000;   # minimum runtime for measurement in ms
  local bench=0;
  local loops=0 x=0 t1=0;
  typeset -a bench_res1;

  #typeset -a g_bench_facts; # benchmark simplification factors
  #typeset -a g_bench_results; # benchmark simplificatiopn factors


  get_ms; start_t=$get_ms;

  if [ "$argc" -ge 1 ]; then
    bench1=$1; #$argv2[0];
    bench2=$bench1; # set also last benchmark
  fi;
  if [ "$argc" -ge 2 ]; then
    bench2=$2; #${argv2[1]};
  fi;
  if [ "$argc" -ge 3 ]; then
    n=$3; #${argv2[2]};
  fi;

  #echo "DEBUG: bench1=$bench1, bench2=$bench2, n=$n";

  if [ "$ZSH_VERSION" ]; then
    g_sh='zsh';
    g_sh_version="$ZSH_NAME $ZSH_VERSION $MACHTYPE";
  elif [ "$BASH_VERSION" ]; then
    g_sh='bash';
    g_sh_version="$BASH $BASH_VERSION $MACHTYPE";
  else
    g_sh='bash'; # assume bash
    g_sh_version="???";
  fi;

  checkbits_int1;
  checkbits_double1_$g_sh; # bash or zsh

  echo "BM Bench v0.5 ($g_sh) -- (int:$checkbits_int1 double:$checkbits_double1) $g_sh_version";
  echo "(c) Marco Vieth, 2002";
  echo "Date:" `date`; # call external date

  for (( bench = bench1; bench2 + 1 - bench; bench++ )); do
    loops=1;   # number of loops
    x=0;     # result from benchmark
    t1=0; # timestamp
    # calibration
    while [ "$t1" -lt 1001 ]; do # we want at least 1 sec calibration time
      echo "Calibrating benchmark $bench with loops=$loops, n=$n";
      get_ms; t1=$get_ms;
      run_bench $bench $loops $n;
      get_ms; ((t1=get_ms-t1));
      echo "x=$x (time: $t1 ms)";
      ((loops *= 2));
      if [ "$x" -eq -1 ]; then
        break;
      fi;
    done;
    if [ "$x" -ne -1 ]; then
      ((loops /= 2)); # div 2
      ((loops *= ((min_ms / t1) + 1))); # integer division!
      echo "Calibration done. Starting measurement with $loops loops to get >=$min_ms ms";
      # measurement
      get_ms; t1=$get_ms;
      run_bench $bench $loops $n;
      get_ms; ((t1=get_ms-t1));
      echo "x=$x (time: $t1 ms)";
      ((t1=t1 * 10 / loops)); # int  #bench_res1[bench] = int(t1 * 10 / loops); # int
      bench_res1[$bench]=$t1;
      echo "Elapsed time for $loops loops: $t1 ms; estimation for 10 loops: ${bench_res1[$bench]} ms";
    else
      bench_res1[$bench]=-1;
    fi;
  done;

  for (( bench = bench1; bench <= bench2; bench++ )); do
    if [ "$bench" -ge 3 ]; then # starting with benchmark 3
      ((i=g_fact_bench0${bench})); # get simplification factor
        echo "bench=$bench, i=$i";
      if [ "$i" -gt 1 ]; then
        ((bench_res1[bench] *= i));
        echo "Note: Estimated runtime for benchmark $bench corrected by factor $i: ${bench_res1[bench]}";
      fi;
    fi;
  done;

  echo "Times for all benchmarks (10 loops, ms):";
  echo -n "BM Results (bash)       : ";
  for (( bench = bench1; bench <= bench2; bench++ )); do
    echo -n "${bench_res1[bench]} ";
    #printf("%7d ", bench_res1[bench]);
  done;
  echo "";
  get_ms; ((start_t=get_ms - start_t));
  echo "Total elapsed time: $start_t ms";
  return 0;
}

#######################################


if [ "$ZSH_VERSION" ]; then
  #echo "DEBUG: zsh: ksh arrays...";
  set -o KSH_ARRAYS # for zsh: array indices start with 0
fi

main $*;

exit;

#end

