#!/usr/bin/bash -f
# BM Bench - bmbench.bash (bash or zsh) # TTT not ok for bash since it canno do math?
# (c) Marco Vieth, 2002-2006
# http://www.benchmarko.de
#
# 05.09.2003 0.01
# 06.09.2003 0.05  all benchmark tests 0..5 implemented but arrays are very slow...
# 30.04.2008 0.06  based on version 0.05
# 10.05.2019 0.07
#
#
# Usage:
# bash bmbench.bash [bench1] [bench2] [n]
#   or
# zsh bmbench.bash [bench1] [bench2] [n]
#
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
#
# Note: bash does only integer, zsh can both integer and fp
# zsh has very slow arrays (seems to be exponential time to access high indices)
# zsh has max array index of 262144 (and gets very slow)
#
# needs commands "printf", "bc", "date"
#
#
# zsh:
#zrecompile -p -U -z $zwc $files
#
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
#
# Checked with SpellCheck: https://www.shellcheck.net/
#
#
set -euo pipefail
IFS=$'\n\t'
LC_ALL=C

set +e # can we use "set -e" with arithmetic expressions?

set +u
if [ "$ZSH_VERSION" ]; then
  set -o KSH_ARRAYS # for zsh: array indices start with 0
fi
set -u

###

PRG_VERSION="0.07";
PRG_LANGUAGE="bash";

####################################

typeset -a gState_fact=(20 20 20 20 20 4); # benchmark simplification factors for n
typeset -a gState_result=("" "" "" 2762 872041851 12864); # benchmark simplification results

typeset -i gState_tsMeasCnt gState_tsPrecCnt gState_tsPrecMs; # will be set later


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
  local loops=$1 n=$2 check=$3;
  typeset -i n_div_65536=0 n_mod_65536=0 i=0 j=0;
  local x=0;
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
    ((x -= check));
    if [ "$x" -ne 0 ]; then
      break;
    fi;
  done;
  ((x %= 65536));
  rc=$x; # return x
}


#
# bench01 (Integer 32 bit)
# (arithmetic mean of 1..n)
#
bench01() {
  local loops=$1 n=$2 check=$3;
  typeset -i sum i=0;

  for (( ; loops-- ; )); do
    sum=0;
    for (( i = 1 ; i <= n ; i++ )); do
      (( sum+=i, sum>n ? sum -= n, x++ : 0 ));
      ##if [ "$sum" -gt "$n" ]; then
      ##  ((sum -= n, ix++));
      ##fi;
    done;
    ((x -= check));
    if [ "$x" -ne 0 ]; then
      break;
    fi;
  done;
  rc=$x # return x
}


#
# bench02 (Floating Point, normally 64 bit)
# (arithmetic mean of 1..n)
# only zsh has floating point
#
bench02() {
#in: loops, n, check; out: x
  local loops=$1 n=$2 check=$3;
  typeset -i i x=0;
  local sum sumInit;

  if [ "$g_sh" = "zsh" ]; then # zsh has floating point
    sumInit=0.0;
  else
    sumInit=0;
  fi

  for (( ; loops-- ; )); do
    sum=$sumInit;
    for (( i = 1 ; i <= n ; i++ )); do
      (( sum+=i, sum>n ? sum -= n, x++ : 0 ));
      ##if [ "$sum" -gt "$n" ]; then
      ##  ((sum -= n, x++));
      ##fi;
    done;
    ((x -= check));
    if [ "$x" -ne 0 ]; then
      break;
    fi;
  done;
  rc=$x # return x
}


#
# bench03 (Integer)
# number of primes below n (Sieve of Eratosthenes)
# Example: n=500000 => x=41538 (expected), n=1000000 => x=78498
# (no multiples of 2 stored)
bench03() {
  local loops=$1 n=$2 check=$3;
  typeset -i i=0 j=0 nHalf=0 m=0 x=0;
  typeset -a sieve1=();

  ((n /= 2)); # compute only up to n/2
  ((nHalf = n >> 1)); # div 2

  for (( ; loops-- ; )); do
    # initialize sieve
    for (( i = 0; i <= nHalf; i++)); do
      sieve1[i]=0; # odd numbers are possible primes
    done;

    # compute primes
    i=0;
    m=3;
    for (( ; m * m < n ; i++, m +=2 )); do
      if [ "${sieve1[i]}" -eq 0 ]; then
        ((x++, j = (m * m - 3) >> 1));
        for (( ; j < nHalf; j += m )); do
            sieve1[j]=1;
        done;
      fi;
    done;

    # count remaining primes
    for (( ; i <= nHalf; i++ )); do
      if [ "${sieve1[i]}" -eq 0 ]; then
        ((x++));
      fi;
    done;

    # check prime count
    ((x -= check));
    if [ "$x" -ne 0 ]; then
      break; # error
    fi;
  done;
  rc=$x; # return x
}


#
# bench04 (Integer 32 bit)
# nth random number number
# Random number generator taken from
# Raj Jain: The Art of Computer Systems Performance Analysis, John Wiley & Sons, 1991, page 442-444.
# It needs longs with at least 32 bit.
# Starting with x0=1, x10000 should be 1043618065, x1000000 = 1227283347
bench04() {
  local loops=$1 n=$2 check=$3;

  # We inserted the constants below...
  ## typeset -ir bench04_M=2147483647 bench04_A=16807 bench04_Q=127773 bench04_R=2836;
  ## M=modulus, A=multiplier, Q=M div A, R=M mod A
  ##
  ## long version: computation loop:
  ##((x_mod_q = x - bench04_Q * x_div_q));
  ##((x = bench04_A * x_mod_q - bench04_R * x_div_q));
  ##if [ "$x" -le 0 ]; then
  ##  ((x += bench04_M)); # x is new random number
  ##fi;

  typeset -i i=0 x=0;

  for (( ; loops-- ; )); do
    ((x++)) # start with 1=last random value
    for (( i = 1 ; i <= n ; i++ )); do
      (( x = 16807 * (x % 127773) - 2836 * ((x / 127773) | 0) ))
      (( x<0 ? x += 2147483647 : 0 ));
    done;
    ((x -= check));
    if [ "$x" -ne 0 ]; then
      break; # error
    fi;
  done;
  rc=$x; # return x
}


#
# bench05 (Integer 32 bit) (list implementation)
# n over n/2 mod 65536 (Pascal's triangle)
# (we just need to store the last 2 lines of computation)
#
bench05() {
  local loops=$1 n=$2 check=$3;

  typeset -i k=0 i=0 min1=0 j=0 test1=0 x=0;
  typeset -a pas1=() pas2=();

  ((n = (n / 500))); # int
  ((k = (n / 2))); # int

  if [ $((n - k)) -lt $k ]; then
    ((k = n - k)); # keep k minimal with  n over k  =  n over n-k
  fi;

  for (( ; loops-- ; )); do
    pas1[0]=1;
    for (( i = 2 ; i <= n ; i++)); do
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
    ((x -= check));
    if [ "$x" -ne 0 ]; then
      break; # error
    fi;
  done;
  rc=$x; # return x
}


#
# run a benchmark
# in: bench = benchmark to use
#     loops = number of loops
#         n = maximum number (used in some benchmarks to define size of workload)
# out:    x = result
#
run_bench() {
  local bench=$1 loops=$2 n=$3;
  typeset -i x=0 check=${gState_result[$bench]}; # if available
  rc=0;
  case "$bench" in
    0)
      #check=10528;
      ((check=((n / 2) * (n + 1)) % 65536)); # assuming n even!
      bench00 "$loops" "$n" "$check"; # special version optimized for 16 bit
    ;;

    1)
      ((check=(n + 1) / 2));
      bench01 "$loops" "$n" "$check";
    ;;

    2)
      ((check=(n + 1) / 2));
      bench02 "$loops" "$n" "$check"; # for bash or zsh
    ;;

    3)
      bench03 "$loops" "$n" "$check";
    ;;

    4)
      bench04 "$loops" "$n" "$check";
    ;;

    5)
      bench05 "$loops" "$n" "$check";
    ;;

    *)
      echo "Error: Unknown benchmark: $bench";
      check=-1
    ;;
  esac;

  ((x = rc + check));
  if [ "$x" -ne "$check" ]; then
    echo "Error(bench$bench): x=$x";
    x=-1; # exit
  fi;
  rc=$x; # return x
}


# get time, $SECONDS contains the senconds since start of script
get_ms() {
  ((rc=SECONDS * 1000));
}


correctTime() {
  local tMeas=$1 measCount=$2;
  typeset -i tsPrecMs=$gState_tsPrecMs tsPrecCnt=$gState_tsPrecCnt tMeas;

  if [ "$measCount" -le "$tsPrecCnt" ]; then
    (( tMeas += (tsPrecMs * (tsPrecCnt - measCount) / tsPrecCnt) )); # ts + correction
  fi;
  rc=$tMeas; # return
}

getPrecMs() {
  local stopFlg=$1;
  typeset -i measCount=0 tMeas0 tMeas;

  get_ms;
  tMeas0=$rc;
  tMeas=$tMeas0;
  while [ "$tMeas" -le "$tMeas0" ]; do
    get_ms;
    tMeas=$rc;
    ((measCount++));
  done;

  if [ "$stopFlg" -eq "1" ]; then
    correctTime $tMeas0 $measCount; # for stop: use first ts + correction
    tMeas=$rc;
  fi;
  gState_tsMeasCnt=$measCount; # memorize last count
  rc=$tMeas; # return
}

# usually only needed if time precision is low, e.g. one second
determineTsPrecision() {
  typeset -i tMeas0 tMeas1;
  getPrecMs 0;
  tMeas0=$rc;
  getPrecMs 0;
  tMeas1=$rc;
  ((gState_tsPrecMs = tMeas1 - tMeas0));
  gState_tsPrecCnt=$gState_tsMeasCnt;

  # do it again
  tMeas0=$tMeas1;
  getPrecMs 0;
  tMeas1=$rc;
  if [ "$gState_tsMeasCnt" -gt "$gState_tsPrecCnt" ]; then # taker maximum count
    gState_tsPrecCnt=$gState_tsMeasCnt;
    ((gState_tsPrecMs = tMeas1 - tMeas0));
  fi;
}


# Here we compute the number of "significant" bits for positive numbers
checkbits_int1() {
  typeset -i bits=0 num=1 last_num=0 num2=0;
  while true; do
    last_num=$num;
    (( num *= 2, num++, bits++, num2=(num-1)/2 ));
    if [ "$num2" -ne "$last_num" ] || [ "$bits" -ge 101 ]; then
      break;
    fi;
  done;
  rc=$bits;
}


checkbits_double1_bash() {
# num, last_num, bits
  typeset -i bits=0;
  local num_f=1 last_num_f=0 num2_f=0;
  while true; do
    last_num_f=$num_f;
    (( num_f *= 2, num_f++, bits++, num2_f=(num_f - 1) / 2 ));
    if [ "$num2_f" -ne "$last_num_f" ] || [ "$bits" -ge 101 ]; then
      break;
    fi;
  done;
  rc=$bits;
}


# for zsh we have real floating point...
checkbits_double1_zsh() {
# num, last_num, bits
  typeset -i bits=0;
  local num_f=1.0 last_num_f=0.0 num2_f=0.0;
  while true; do
    last_num_f=$num_f;
    (( num_f *= 2.0, num_f++, bits++, num2_f=(num_f - 1.0) / 2.0 ));
    if [ "$num2_f" -ne "$last_num_f" ] || [ "$bits" -ge 101 ]; then
      break;
    fi;
  done;
  rc=$bits;
}


print_info() {
  typeset -i checkbits_i1 checkbits_d1;
  local sh_version;

  set +u
  if [ "$ZSH_VERSION" ]; then
    g_sh='zsh';
    sh_version="$ZSH_NAME $ZSH_VERSION $MACHTYPE";
  elif [ "$BASH_VERSION" ]; then
    g_sh='bash';
    sh_version="$BASH $BASH_VERSION $MACHTYPE";
  else
    g_sh='bash'; # assume bash
    sh_version="???";
  fi;
  set -u

  checkbits_int1;
  checkbits_i1=$rc

  checkbits_double1_$g_sh; # bash or zsh
  checkbits_d1=$rc

  echo "BM Bench v$PRG_VERSION ($PRG_LANGUAGE) -- (int:$checkbits_i1 double:$checkbits_d1 tsMs:$gState_tsPrecMs tsCnt:$gState_tsPrecCnt) $sh_version";
  echo "(c) Marco Vieth, 2006-2019";
  echo "Date:" "$(date)"; # call external date
}


print_results() {
  local bench1=$1 bench2=$2;
  typeset -i bench;

  echo "";
  echo "Throughput for all benchmarks (loops per sec):";
  echo -n "BMR ($PRG_LANGUAGE) : ";
  for (( bench = bench1; bench <= bench2; bench++ )); do
    printf "%9.3f " $((${bench_res1[$bench]}))e-3;
  done;
  echo "";
  echo "";
}


measure_bench() {
  local bench=$1 n=$2;

  typeset -ir cali_ms=1001 delta_ms=100 max_ms=10000; # const
  typeset -i loops=1 x=0 t1=0 t2=0 t_delta=0 result=0;
  # loops=number of loops x=result from benchmark result t1=measured time t2=estimated time t_delta=time delta resultc=result
  local loops_p_tsec scale_fact;
  # loops_p_tsec=loops per thousand seconds scale_fact=scaling factor

  if [ "${gState_fact[$bench]}" ]; then
    ((n /= gState_fact[bench])); # reduce by a factor
    echo "Note: $g_sh is rather slow, so reduce n by factor ${gState_fact[$bench]} to $n.";
  fi;

  echo "Calibrating benchmark $bench with n=$n";
  while [ "$result" -eq 0 ]; do
    getPrecMs 0;
    t1=$rc;
    run_bench "$bench" "$loops" "$n";
    x=$rc;
    getPrecMs 1;
    ((t1=rc-t1));

    t_delta=0;
    # compute difference abs(measures-estimated)
    if [ "$t2" -gt "$t1" ]; then
      ((t_delta = t2 - t1));
    else
      ((t_delta = t1 - t2));
    fi;

    loops_p_tsec=0;
    if [ "$t1" -gt 0 ]; then
      ((loops_p_tsec = (loops * 1000000) / t1));
    fi;

    printf "%10.3f/s (time=%5d ms, loops=%7d, delta=%5d ms, x=$x)\n" "${loops_p_tsec}"e-3 $t1 $loops $t_delta;
    if [ "$x" -eq -1 ]; then # some error?
      result=-1;
    elif [ "$t2" -gt 0 ] && [ "$t_delta" -lt "$delta_ms" ]; then # do we have some estimated/expected time smaller than delta_ms=100?
      result=$loops_p_tsec;
      echo -n "Benchmark $bench ($PRG_LANGUAGE): ";
      printf "%.3f" $((loops_p_tsec))e-3;
      echo "/s (time=$t1 ms, loops=$loops, delta=$t_delta ms)";
    elif [ "$t1" -gt $max_ms ]; then
      echo "Benchmark $bench ($PRG_LANGUAGE): Time already > $max_ms ms. No measurement possible.";
      result=-1;
    else
      if [ "$t1" -lt "$cali_ms" ] && [ "$t1" -gt 0 ]; then
        ((scale_fact = ((cali_ms + 100) / t1) + 1)); # '/' is integer division!
        # scale a bit up to 1100 ms (cali_ms+100)
      else
        scale_fact=2;
      fi;
      ((loops *= scale_fact));
      ((t2 = t1 * scale_fact));
    fi;
  done;

  # correction factor
  if [ "$result" -ge 0 ] && [ "${gState_fact[$bench]}" ]; then
    ((result /= gState_fact[bench]));
    echo -n "Note: Estimated runtime for benchmark $bench corrected by factor ${gState_fact[$bench]}: ";
    printf "%.3f\n" "$result"e-3;
  fi;
  rc=$result; # return
}


start_bench() {
  local bench1=$1 bench2=$2 n=$3;
  typeset -i bench;
  typeset -a bench_res1;

  print_info;

  for (( bench = bench1; bench2 + 1 - bench; bench++ )); do
    measure_bench "$bench" "$n";
    bench_res1[$bench]=$rc;
  done;

  print_results "$bench1" "$bench2";
  rc=0; # return
}

###

main() {
  typeset -i argc=$#;
  typeset -i start_t=0 bench1=0 bench2=5 n=1000000 exitCode=0;
  # start_t=start time bench1=first benchmark to test bench2=last benchmark to test n=maximum number rc=return code

  get_ms;
  start_t=$rc;

  if [ "$argc" -ge 1 ]; then
    bench1=$1;
    bench2=$bench1; # set also last benchmark
  fi;
  if [ "$argc" -ge 2 ]; then
    bench2=$2;
  fi;
  if [ "$argc" -ge 3 ]; then
    n=$3;
  fi;

  determineTsPrecision;
  start_bench "$bench1" "$bench2" "$n";
  exitCode=$rc;

  get_ms;
  ((start_t=rc - start_t));
  echo "Total elapsed time: $start_t ms";

  return $exitCode;
}

#######################################

main "$@";

exit;

#end

