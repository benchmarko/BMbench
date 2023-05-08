#!/usr/bin/bash -f
# BM Bench - bmbench.bash (bash or zsh) # TTT not ok for bash since it canno do math?
# (c) Marco Vieth, 2002-2006
# http://www.benchmarko.de
#
# 05.09.2003 0.01
# 06.09.2003 0.05  all benchmark tests 0..5 implemented but arrays are very slow...
# 30.04.2008 0.06  based on version 0.05
# 10.05.2019 0.07
# 04.04.2023 0.08  adapted for new version; bench05 optimized
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

PRG_VERSION="0.08";
PRG_LANGUAGE="bash";

####################################

typeset -a gState_fact=(20 20 20 20 20 4); # benchmark simplification factors for n
#typeset -a gState_result=("" "" "" 2762 872041851 12864); # benchmark simplification results

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
  local n=$1;
  typeset -i n_div_65536=0 n_mod_65536=0 i=0 j=0;
  local x=0;
  ((n_div_65536=(n / 65536)));
  ((n_mod_65536=(n % 65536)));

  for (( i = n_div_65536 ; i ; i-- )); do
    for (( j = 65535 ; j ; )); do
      ((x += j--));
    done;
  done;
  for (( j = n_mod_65536 ; j ; )); do
    ((x += j--));
  done;
  ((x %= 65536));
  rc=$x; # return x
}


#
# bench01 (Integer 32 bit)
# (arithmetic mean of 1..n)
#
bench01() {
  local n=$1;
  typeset -i sum i x=0;
  local sum;

  sum=0;
  for (( i = 1 ; i <= n ; i++ )); do
    (( sum+=i, sum>n ? sum -= n, x++ : 0 ));
    ##if [ "$sum" -gt "$n" ]; then
    ##  ((sum -= n, x++));
    ##fi;
  done;
  rc=$x # return x
}


#
# bench02 (Floating Point, normally 64 bit)
# (arithmetic mean of 1..n)
# only zsh has floating point
#
bench02() {
  local n=$1;
  typeset -i i x=0;
  local sum sumInit;

  if [ "$g_sh" = "zsh" ]; then # zsh has floating point
    sumInit=0.0;
  else
    sumInit=0;
  fi

  sum=$sumInit;
  for (( i = 1 ; i <= n ; i++ )); do
    (( sum+=i, sum>n ? sum -= n, x++ : 0 ));
  done;
  rc=$x # return x
}


#
# bench03 (Integer)
# number of primes below n (Sieve of Eratosthenes)
# Example: n=500000 => x=41538 (expected), n=1000000 => x=78498
# (no multiples of 2 stored)
bench03() {
  local n=$1;
  typeset -i i=0 j=0 nHalf=0 m=0 x;
  typeset -a sieve1=();

  ((n /= 2)); # compute only up to n/2
  ((nHalf = n >> 1)); # div 2

  # initialize sieve
  for (( i = 0; i <= nHalf; i++)); do
    sieve1[i]=0; # odd numbers are possible primes
  done;

  # compute primes
  i=0;
  m=3;
  x=1; # 2 is prime
  for (( ; m * m <= n ; i++, m +=2 )); do
    if [ "${sieve1[i]}" -eq 0 ]; then
      ((x++, j = (m * m - 3) >> 1));
      for (( ; j < nHalf; j += m )); do
          sieve1[j]=1;
      done;
    fi;
  done;

  # count remaining primes
  for (( ; m <= n; i++, m +=2 )); do
    if [ "${sieve1[i]}" -eq 0 ]; then
      ((x++));
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
  local n=$1;

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

  typeset -i i=0 x=1;

  for (( i = 1 ; i <= n ; i++ )); do
    (( x = 16807 * (x % 127773) - 2836 * ((x / 127773) | 0) ))
    (( x<0 ? x += 2147483647 : 0 ));
  done;
  rc=$x; # return x
}


#
# bench05 (Integer 32 bit) (list implementation)
# n over n/2 mod 65536 (Pascal's triangle)
# (we just need to store the last 2 lines of computation)
#
bench05() {
  local n=$1;

  typeset -i k=0 i=0 min1=0 j=0 test1=0 x=0 prev=0 num=0;
  typeset -a pas1=();

  ((n = (n / 2))); # int

  ((k = (n / 2))); # int
  if [ $((n - k)) -lt $k ]; then
    ((k = n - k)); # keep k minimal with  n over k  =  n over n-k
  fi;

  pas1[0]=1;
  if [ $k -ge 1 ]; then
    pas1[1]=2;
  fi;
  for (( i = 3 ; i <= n ; i++ )); do
    ((min1 = ((i - 1) / 2))); # int(...)
    ((test1=(i % 2))); # new element?
    if [ "$test1" -eq 0 ]; then # new element
      ((pas1[min1 + 1] = 2 * pas1[min1]));
    fi;

    #pas1[1]=$i; # second column is i
    prev=pas1[1];
    for ((j = 2; j <= min1; j++)); do # up to min((i-1)/2, k)
      num=${pas1[$j]};
      ((pas1[j] = (prev + pas1[j]) % 65536)); # modulus to avoid NaN
      ((prev=num));
    done;
    pas1[1]=$i;
  done;

  # compute sum of ((n/2)Ck)^2 mod 65536 for k=0..n/2
  x=0;
  for ((j = 0; j < k; j++)); do
    ((x = (x + 2 * pas1[j] * pas1[j]) % 65536));
  done;
  ((x = (x + pas1[k] * pas1[k]) % 65536));
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
  local bench=$1 loops=$2 n=$3 check=$4;

  x=0;
  for (( ; loops-- ; )); do
    case "$bench" in
      0)
        bench00 "$n"; # special version optimized for 16 bit
      ;;

      1)
        bench01 "$n";
      ;;

      2)
        bench02 "$n"; # for bash or zsh
      ;;

      3)
        bench03 "$n";
      ;;

      4)
        bench04 "$n";
      ;;

      5)
        bench05 "$n";
      ;;

      *)
        echo "Error: Unknown benchmark: $bench";
        rc=0;
      ;;
    esac;
    x=$rc;
    ((x -= check));
    if [ "$x" -ne 0 ]; then
        break; # error
      fi;
  done;
  ((x += check));
  if [ "$x" -ne "$check" ]; then
    echo "Error(bench$bench): x=$x";
    x=-1; # exit
  fi;
  rc=$x; # return x
}


bench03Check() {
  local n=$1;

  typeset -i i=0 j=0 isPrime=0 x=0;

  ((n = (n / 2))); # int

  for (( j = 2; j <= n; j++)); do
    isPrime=1;
    for ((i = 2; i * i <= j; i++)); do
      (( j % i == 0 ? isPrime=0 : 0));
      if [ "$isPrime" -eq 0 ]; then
        break; # error
      fi;
    done;
    if [ "$isPrime" -ne 0 ]; then
      ((x++));
    fi;
  done;
  rc=$x; # return x
}

getCheck() {
  local bench=$1 n=$2;
  #typeset -i x=0 check=${gState_result[$bench]}; # if available

  typeset -i check=0;

  local len;

  len=${#gState_fact[@]};

  if [ "$bench" -lt "$len" ]; then   #-a "${gState_fact[$bench]}" ]; then
    ((n /= gState_fact[bench])); # reduce by a factor
    echo "Note: $g_sh is rather slow, so reduce n by factor ${gState_fact[$bench]} to $n.";
  fi;

  rc=0;
  case "$bench" in
    0)
      ((check=((n / 2) * (n + 1)) % 65536)); # assuming n even!
    ;;

    1)
      ((check=(n + 1) / 2));
    ;;

    2)
      ((check=(n + 1) / 2));
    ;;

    3)
      if [ "$n" -eq 1000000 ]; then
        ((check=41538));
      else
        bench03Check "$n"
        check=$rc;
      fi;
    ;;

    4)
      if [ "$n" -eq 1000000 ]; then
        ((check=1227283347));
      else
        bench04 "$n" # bench04 not a real check
        check=$rc;
      fi;
    ;;

    5)
      if [ "$n" -eq 1000000 ]; then
        ((check=27200));
      else
        bench05 "$n" # bench05 not a real check
        check=$rc;
      fi;
    ;;

    *)
      echo "Error: Unknown benchmark: $bench";
      check=-1
    ;;
  esac;

  rc=$check; # return check
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
  local bench=$1 n=$2 check=$3;

  typeset -ir cali_ms=1001 delta_ms=100 max_ms=10000; # const
  typeset -i loops=1 x=0 t1=0 t2=0 t_delta=0 result=0;
  # loops=number of loops x=result from benchmark result t1=measured time t2=estimated time t_delta=time delta resultc=result
  local loops_p_tsec scale_fact;
  # loops_p_tsec=loops per thousand seconds scale_fact=scaling factor

  if [ "${gState_fact[$bench]}" ]; then
    ((n /= gState_fact[bench])); # reduce by a factor
    #echo "Note: $g_sh is rather slow, so reduce n by factor ${gState_fact[$bench]} to $n.";
  fi;

  echo "Calibrating benchmark $bench with n=$n, check=$check";
  while [ "$result" -eq 0 ]; do
    getPrecMs 0;
    t1=$rc;
    run_bench "$bench" "$loops" "$n" "$check";
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

    printf "%10.3f/s (time=%9.3f ms, loops=%7d, delta=%9.3f ms)\n" "${loops_p_tsec}"e-3 $t1 $loops $t_delta;
    if [ "$x" -eq -1 ]; then # some error?
      result=-1;
    elif [ "$t2" -gt 0 ] && [ "$t_delta" -lt "$delta_ms" ]; then # do we have some estimated/expected time smaller than delta_ms=100?
      result=$loops_p_tsec;
      echo -n "Benchmark $bench ($PRG_LANGUAGE): ";
      printf "%.3f/s (time=%.3f ms," $((loops_p_tsec))e-3 "$t1";
      echo -n " loops=$loops,";
      printf " delta=%.3f ms)\n" "$t_delta";
    elif [ "$t1" -gt $max_ms ]; then
      echo "Benchmark $bench ($PRG_LANGUAGE): Time already > $max_ms ms. No measurement possible.";
      result=-1;
    else
      if [ "$t1" -eq 0 ]; then
        scale_fact=50;
      elif [ "$t1" -lt "$cali_ms" ]; then
        ((scale_fact = ((cali_ms + 100) / t1) + 1)); # '/' is integer division!
        # scale a bit up to 1100 ms (cali_ms+100)
      else
        scale_fact=2;
      fi;
      ((loops *= scale_fact));
      ((t2 = t1 * scale_fact));
    fi;
  done;

  # correction factor (only useful for linear problems)
  if [ "$result" -ge 0 ] && [ "${gState_fact[$bench]}" ]; then
    ((result /= gState_fact[bench]));
    echo -n "Note: Estimated runtime for benchmark $bench corrected by factor ${gState_fact[$bench]}: ";
    printf "%.3f\n" "$result"e-3;
  fi;
  rc=$result; # return
}


start_bench() {
  local bench1=$1 bench2=$2 n=$3;
  typeset -i bench nsave;
  typeset -a bench_res1;

  print_info;

  nsave=n;
  for (( bench = bench1; bench2 + 1 - bench; bench++ )); do
    n=nsave;
    # reduce problem size
    if [ "$bench" -eq 3 ]; then
			((n = n / 2));
		elif [ "$bench" -eq 5 ]; then
			((n = n / 200));
		fi;
    getCheck "$bench" "$n";
    check=$rc;
    if [ "$check" -gt 0 ]; then
      measure_bench "$bench" "$n" "$check";
    else
     rc=-1;
    fi;
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
