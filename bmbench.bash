#!/usr/bin/bash -f
# BM Bench - bmbench.bash (bash or zsh) # TTT not ok for bash since it canno do math?
# (c) Marco Vieth, 2002-2006
# http://www.benchmarko.de
#
# 05.09.2003 0.01
# 06.09.2003 0.05  all benchmark tests 0..5 implemented but arrays are very slow...
# 30.04.2008 0.06  based on version 0.05
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
# needs commands "printf", "bc", "date"

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
# Checked with SpellCheck: https://www.shellcheck.net/
#

set -euo pipefail
IFS=$'\n\t'

set +e # can we use "set -e" with arithmetic expressions?

PRG_VERSION="0.07";
PRG_LANGUAGE="bash";

####################################

typeset -a gState_fact=(20 20 20 20 20 4); # benchmark simplification factors for n
typeset -a gState_result=("" "" "" 2762 872041851 12864); # benchmark simplification results

#typeset -a gState_fact=("" "" "" 20 4 2); # benchmark simplification factors for n
#typeset -a gState_result=("" "" "" 2762 838931758 43584); # benchmark simplification results

#gState_fact[3]=20;     # bench03 reduction factor for n
#gState_result[3]=2762; #result for factor 20 (for factor 1 it would be 41538)
#gState_fact[4]=4;     # bench04 reduction factor for n
#gState_result[4]=838931758; #result for factor 4 (for factor 1 it would be 1227283347)
#gState_fact[5]=2;     # bench05 reduction factor for n
#gState_result[5]=43584; #result for factor 2 (factor 1 it would be 27200)


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
#in: loops, n, check; out: x
  local loops=$1 n=$2 check=$3;
  typeset -i n_div_65536 n_mod_65536 i j;
  local n_div_65536=0 n_mod_65536=0 i=0 j=0;
  x=0; # global!
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
  # returns x
}


#
# bench01 (Integer 32 bit)
# (arithmetic mean of 1..n)
#
bench01_ok_unused() {
#in: loops, n, check; out: x
  local loops=$1 n=$2 check=$3;
  typeset -i isum i ix;
  local isum i ix=0;
  for (( ; loops-- ; )); do
    isum=0
    for (( i = 1 ; i <= n ; i++, isum += i )); do
      if [ "$isum" -gt "$n" ]; then
        ((isum -= n, ix++));
      fi;
    done;
    ((ix -= check));
    if [ "$ix" -ne 0 ]; then
      break;
    fi;
  done;
  x=ix # return x (global)
}


bench01() {
#in: loops, n, check; out: x
  local loops=$1 n=$2 check=$3;
  typeset -i sum i ix=0;
  for (( ; loops-- ; )); do
    sum=0;
    for (( i = 1 ; i <= n ; i++ )); do
      (( sum+=i, sum>n ? sum -= n, ix++ : 0 ));
    done;
    ((ix -= check));
    if [ "$ix" -ne 0 ]; then
      break;
    fi;
  done;
  x=ix # return x (global)
}



#
# bench02 (Floating Point, normally 64 bit)
# (arithmetic mean of 1..n)
#
bench02_zsh() {
#in: loops, n, check; out: x
  local loops=$1 n=$2 check=$3;
  typeset -i i ix;
  local sum i ix=0;
  for (( ; loops-- ; )); do
    sum=0.0 # zsh has floating point
    for (( i = 1 ; i <= n ; i++ )); do
      ((sum += i));
      if [ "$sum" -gt "$n" ]; then
        ((sum -= n));
        ((ix++));
      fi;
    done;
    ((ix -= check));
    if [ "$ix" -ne 0 ]; then
      break;
    fi;
  done;
  x=ix # return x (global)
}


bench02_bash() {
#in: loops, n, check; out: x
  local loops=$1 n=$2 check=$3;
  typeset -i i ix;
  local sum i ix=0;
  for (( ; loops-- ; )); do
    sum=0
    for (( i = 1 ; i <= n ; i++ )); do
      ((sum += i));
      if [ "$sum" -gt "$n" ]; then
        ((sum -= n));
        ((ix++));
      fi;
    done;
    ((ix -= check));
    if [ "$ix" -ne 0 ]; then
      break;
    fi;
  done;
  x=ix # return x (global)
}


# bench03 (Integer)
# number of primes below n (Sieve of Eratosthenes)
# Example: n=500000 => x=41538 (expected), n=1000000 => x=78498
# (No bit array available but zsh as max array index of 262144 (and gets very slow), so simulate bit array...)
bench03() {
#in: loops, n, check; out: x
  local loops=$1 n=$2 check=$3;
  typeset -i i=0 j=0 test1=0;
  typeset -a sieve1=();

  #((n /= g_fact_bench03)); # reduce by a factor
  #echo "Note: $g_sh has a very slow array implementation, so reduce n by factor $g_fact_bench03 to $n."

  ((n /= 2)); # compute only up to n/2
  x=0; # number of primes below n
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
    ((x -= check));
    if [ "$x" -ne 0 ]; then # now x must be 0 again
      break;       # error
    fi;
  done;
  # returns x
}


#
# bench04 (Integer 32 bit)
# nth random number number
# Random number generator taken from
# Raj Jain: The Art of Computer Systems Performance Analysis, John Wiley & Sons, 1991, page 442-444.
# It needs longs with at least 32 bit.
# Starting with x0=1, x10000 should be 1043618065, x1000000 = 1227283347.
#
bench04_ok1_unused() {
#in: loops, n, check; out: x
  local loops=$1 n=$2 check=$3;

  #((n /= g_fact_bench04)); # reduce by a factor
  #echo "Note: $g_sh has a very slow computations, so reduce n by factor $g_fact_bench04 to $n."

  #typeset -i bench04_M bench04_A bench04_Q bench04_R;
  #local bench04_M=2147483647; # modulus, do not change!
  #local bench04_A=16807;      # multiplier
  #local bench04_Q=127773;     # m div a
  #local bench04_R=2836;       # m mod a
  typeset -ir bench04_M=2147483647 bench04_A=16807 bench04_Q=127773 bench04_R=2836;
  # M=modulus, A=multiplier, Q=M div A, R=M mod A
  typeset -i i=0 x_div_q=0 x_mod_q=0;

  x=0;                # last random value
  for (( ; loops-- ; )); do
    ((x++)) # start with 1=last random value
    for (( i = 1 ; i <= n ; i++ )); do
      ((x_div_q = (x / bench04_Q))); # int!
      ((x_mod_q = x - bench04_Q * x_div_q));
      ((x = bench04_A * x_mod_q - bench04_R * x_div_q));
      if [ "$x" -le 0 ]; then
        ((x += bench04_M)); # x is new random number
      fi;
    done;
    ((x -= check));
    if [ "$x" -ne 0 ]; then # now x must be 0 again
      break;       # error
    fi;
  done;
  # returns x
}


bench04() {
#in: loops, n, check; out: x
  local loops=$1 n=$2 check=$3;

  #((n /= g_fact_bench04)); # reduce by a factor
  #echo "Note: $g_sh has a very slow computations, so reduce n by factor $g_fact_bench04 to $n."

  #typeset -i bench04_M bench04_A bench04_Q bench04_R;
  #local bench04_M=2147483647; # modulus, do not change!
  #local bench04_A=16807;      # multiplier
  #local bench04_Q=127773;     # m div a
  #local bench04_R=2836;       # m mod a
  #typeset -ir bench04_M=2147483647 bench04_A=16807 bench04_Q=127773 bench04_R=2836;
  # M=modulus, A=multiplier, Q=M div A, R=M mod A
  #typeset -i i=0 x_div_q=0 x_mod_q=0;
  typeset -i i=0;

  x=0;                # last random value
  for (( ; loops-- ; )); do
    ((x++)) # start with 1=last random value
    for (( i = 1 ; i <= n ; i++ )); do
      #(( x = 16807 * (x % 127773) - 2836 * ((x / 127773) | 0), x<0 ? x += 2147483647 : 0 ));
      (( x = 16807 * (x % 127773) - 2836 * ((x / 127773) | 0) ))
      (( x<0 ? x += 2147483647 : 0 ));

      #"x =  A     * (x % Q     ) - R    * (x div Q)"

      #((x_mod_q = x - bench04_Q * x_div_q));
      #((x = bench04_A * x_mod_q - bench04_R * x_div_q));
      #if [ "$x" -le 0 ]; then
      #  ((x += bench04_M)); # x is new random number
      #fi;
    done;
    ((x -= check));
    if [ "$x" -ne 0 ]; then # now x must be 0 again
      break;       # error
    fi;
  done;
  # returns x
}


#
# bench05 (Integer 32 bit) (list implementation)
# n over n/2 mod 65536 (Pascal's triangle)
# (we just need to store the last 2 lines of computation)
#
bench05() {
#in: loops, n, check; out: x
  local loops=$1 n=$2 check=$3;

  #((n /= g_fact_bench05)); # reduce by a factor
  #echo "Note: $g_sh has a very slow array implementation, so reduce n by factor $g_fact_bench05 to $n."

  typeset -i k=0 i=0 min1=0 j=0 test1=0;
  typeset -a pas1=() pas2=();

  x=0;

  ((n = (n / 500))); # int
  ((k = (n / 2))); # int

  if [ $((n - k)) -lt $k ]; then
    ((k = n - k)); # keep k minimal with  n over k  =  n over n-k
  fi;

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
    ((x -= check));
    if [ "$x" -ne 0 ]; then # now x must be 0 again
      break;       # error
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
  typeset -i check;
  local check=${gState_result[$bench]}; # if available
  x=0;
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
      bench02_"$g_sh" "$loops" "$n" "$check"; # for bash or zsh
    ;;

    3)
      #check=${gState_result[$bench]};
      bench03 "$loops" "$n" "$check";
    ;;

    4)
      #check=g_result_bench04; # normally 1227283347
      bench04 "$loops" "$n" "$check";
    ;;

    5)
      #check=g_result_bench05; # normally 27200; # 58336; 43584;
      bench05 "$loops" "$n" "$check";
    ;;

    *)
      echo "Error: Unknown benchmark: $bench";
      check=-1
    ;;
  esac;

  ((x += check));
  if [ "$x" -ne "$check" ]; then
    echo "Error(bench$bench): x=$x";
    x=-1; # exit
  fi;
}


#for bash: integer div with floating point result
#https://stackoverflow.com/questions/12147040/division-in-script-and-floating-point/24431665#24431665
divf_unused() {
#dividend and divisor
  if [ "$2" -eq 0 ]; then
    echo division by 0;
    exit;
  fi
  local p=12                            # precision
  local c=${c:-0}                       # precision counter
  local d=.                             # decimal separator
  local r=$(($1/$2)); echo -n $r        # result of division
  local m=$((r*$2))
  [ "$c" -eq 0 ] && [ "$m" -ne "$1" ] && echo -n "$d"
  [ "$1" -eq "$m" ] || [ "$c" -eq "$p" ] && return
  local e=$(($1-m))
  ((c=c+1))
  divf_unused $((e*10)) "$2" # recursive
} 


get_ms() {
  get_ms=$SECONDS;
  ((get_ms *= 1000));
}


correctTime() {
  local tMeas0=$1 measCount=$2;
  typeset -i tsPrecMs tsPrecCnt tMeas;
  local tsPrecMs=$gState_tsPrecMs;
  local tsPrecCnt=$gState_tsPrecCnt;
  local tMeas;

  if [ "$measCount" -gt "$tsPrecCnt" ]; then
    measCount=$tsPrecCnt; # fix
  fi;
  #echo "DDD0: correctTime: $tMeas0 + $tsPrecMs * ( $tsPrecCnt - $measCount) / $tsPrecCnt";
  (( tMeas = tMeas0 + (tsPrecMs * (tsPrecCnt - measCount) / tsPrecCnt) )); # use start ts + correction
  correctTime=$tMeas; # return
  #echo "DDD1: correctTime=tMeas=$tMeas";
}

getPrecMs() {
  local stopFlg=$1;
  typeset -i measCount tMeas tMeas0;
  local measCount=0;
  local tMeas;

  get_ms;
  local tMeas0=$get_ms;

  tMeas=tMeas0;
  while [ "$tMeas" -eq "$tMeas0" ]; do
    get_ms; 
    tMeas=$get_ms;
    ((measCount++));
  done;
  if [ "$stopFlg" -eq "1" ]; then
    correctTime $tMeas0 $measCount;
    tMeas=$correctTime;
  fi;
  gState_tsMeasCnt=$measCount; # memorize last count
  getPrecMs=$tMeas; # return
  #echo "DDD: getPrecMs=$getPrecMs measCount=$measCount"
}

# usually only needed if time precision is low, e.g. one second
determineTsPrecision() {
  getPrecMs 0;
  local tMeas0=$getPrecMs;
  getPrecMs 0;
  local tMeas1=$getPrecMs;
  ((gState_tsPrecMs = tMeas1 - tMeas0));
  gState_tsPrecCnt=$gState_tsMeasCnt;

  #echo "DDD0: determineTsPrecision: tsPrecMs=$gState_tsPrecMs tsPrecCnt=$gState_tsPrecCnt";

  # do it again
  tMeas0=$tMeas1;
  #echo "DDD1h1: $tMeas0 $tMeas1";
  getPrecMs 0;
  tMeas1=$getPrecMs;
  #echo "DDD1h1: $tMeas0 $tMeas1";
  if [ "$gState_tsMeasCnt" -gt "$gState_tsPrecCnt" ]; then # taker maximum count
    gState_tsPrecCnt=$gState_tsMeasCnt;
    ((gState_tsPrecMs = tMeas1 - tMeas0));
  fi;
  #echo "DDD1: determineTsPrecision: tsPrecMs=$gState_tsPrecMs tsPrecCnt=$gState_tsPrecCnt";
}


# Here we compute the number of "significant" bits for positive numbers (which means 53 for double)
checkbits_int1() {
# num, last_num, bits
  typeset -i num last_num num2 bits;
  local num=1 last_num=0 num2=0 bits=0;
  while true; do
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
  while true; do
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
  while true; do
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


print_info() {
  set +u
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
  set -u

  checkbits_int1;
  checkbits_double1_$g_sh; # bash or zsh

  echo "BM Bench v$PRG_VERSION ($PRG_LANGUAGE) -- (int:$checkbits_int1 double:$checkbits_double1 tsMs:$gState_tsPrecMs tsCnt:$gState_tsPrecCnt) $g_sh_version";
  echo "(c) Marco Vieth, 2006-2019";
  echo "Date:" "$(date)"; # call external date
}


print_results() {
  #local bench1=$1 bench2=$2 bench_res1=$3;
  local bench1=$1 bench2=$2;
  echo "";
  echo "Throughput for all benchmarks (loops per sec):";
#TTT
  echo -n "BMR ($PRG_LANGUAGE) : ";
  for (( bench = bench1; bench <= bench2; bench++ )); do
    #echo -n "${bench_res1[bench]} ";
    #echo "DEBUG: print_results: ${bench_res1[$bench]}";
    
    #LC_ALL=C printf "%9.2f " $((${bench_res1[$bench]}))e-3;
    LC_ALL=C printf "%9.3f " $((${bench_res1[$bench]}))e-3;

    ##printf "%9.2f " $((${bench_res1[$bench]}/10)); #TTT
  done;
  echo "";
  echo "";
}


measure_bench() {
#(bench, n,    cali_ms, delta_ms, max_ms, bench, loops, x, t1, t2, t_delta, loops_p_sec, scale_fact)
  local bench=$1 n=$2;

  typeset -i cali_ms delta_ms max_ms;
  typeset -i loops x t1 t2 t_delta;

  local cali_ms=1001; # const
  local delta_ms=100; # const
  local max_ms=10000; # const

  local loops=1; # number of loops
  local x=0;     # result from benchmark
  local t1=0;    # measured time
  local t2=0;    # estimated time
  local scale_fact;

  #echo "DDD: ${gState_fact[$bench]}";
  if [ "${gState_fact[$bench]}" ]; then
    ((n /= gState_fact[bench])); # reduce by a factor
    echo "Note: $g_sh is rather slow, so reduce n by factor ${gState_fact[$bench]} to $n.";
  fi;

  rc=0;
  echo "Calibrating benchmark $bench with n=$n";
  while [ "$rc" -eq 0 ]; do
    getPrecMs 0;
    t1=$getPrecMs;
    run_bench "$bench" "$loops" "$n";
    getPrecMs 1;
    ((t1=getPrecMs-t1));

    local t_delta=0;
    # compute difference abs(measures-estimated)
    if [ "$t2" -gt "$t1" ]; then
      ((t_delta = t2 - t1));
    else
      ((t_delta = t1 - t2));
    fi;

    local loops_p_sec=0;
    if [ "$t1" -gt 0 ]; then
      if [ "$g_sh" = 'zsh' ]; then
        ((loops_p_sec = (loops * 1000000.0) / t1)); #use factor 1e6 instead of 1e3 here
      else
        #loops_p_sec=$(echo "scale=0; ($loops * 1000000) / $t1" | bc); # we use factor 1e6, so we can use scale=0
        ((loops_p_sec = (loops * 1000000) / t1)); #use factor 1e6 instead of 1e3 here
      fi
    fi;

    LC_ALL=C printf "%10.3f/s (time=%5d ms, loops=%7d, delta=%5d ms, x=$x)\n" "${loops_p_sec}"e-3 $t1 $loops $t_delta;
    if [ "$x" -eq -1 ]; then # some error?
      rc=-1;
    elif [ "$t2" -gt 0 ] && [ "$t_delta" -lt "$delta_ms" ]; then # do we have some estimated/expected time smaller than delta_ms=100?
      rc=$loops_p_sec;
      ##printf -v formatted_bench_res "%.3f" bench_res1[$bench];
      echo -n "Benchmark $bench ($PRG_LANGUAGE): ";
      LC_ALL=C printf "%.3f" $((loops_p_sec))e-3;
      echo "/s (time=$t1 ms, loops=$loops, delta=$t_delta ms)";
    elif [ "$t1" -gt $max_ms ]; then 
      echo "Benchmark $bench ($PRG_LANGUAGE): Time already > $max_ms ms. No measurement possible.";
      rc=-1;
    else
      if [ "$t1" -lt "$cali_ms" ] && [ "$t1" -gt 0 ]; then
        ((scale_fact = ((cali_ms + 100) / t1) + 1)); # '/' is integer division!
      else
        scale_fact=2;
      fi;
      # scale a bit up to 1100 ms (cali_ms+100)
      ((loops *= scale_fact));
      ((t2 = t1 * scale_fact));
    fi;
  done;

  # correct...
  if [ "$rc" -ge 0 ] && [ "${gState_fact[$bench]}" ]; then
    ((rc /= gState_fact[bench]));
    echo -n "Note: Estimated runtime for benchmark $bench corrected by factor ${gState_fact[$bench]}: ";
    LC_ALL=C printf "%.3f\n" "$rc"e-3;
  fi;

  # return rc
}



start_bench() {
#(bench1, bench2, n,    cali_ms, delta_ms, max_ms, bench, loops, x, t1, t2, t_delta, loops_p_sec, scale_fact)
  local bench1=$1 bench2=$2 n=$3;

  print_info;
  
  typeset -a bench_res1;

  #typeset -a g_bench_facts; # benchmark simplification factors
  #typeset -a g_bench_results; # benchmark simplificatiopn factors

  #echo "DEBUG: bench1=$bench1, bench2=$bench2, n=$n";

  for (( bench = bench1; bench2 + 1 - bench; bench++ )); do
    measure_bench "$bench" "$n";
    #echo "DEBUG1: $rc";
    bench_res1[$bench]=$rc;
  done;
 
  #print_results $bench1 $bench2 $bench_res1;
  print_results "$bench1" "$bench2"; # $bench_res1;
  start_bench=0; #return
}

###


g1_unused() {
  echo "g1"
  local g1=$SECONDS;
  echo "g2"
  #local area=$(($1 * $2))
  #set +e;
  ((g1*=1000));
  #set -e;
  echo "g3"
  return 0;
}

main() {
  typeset -i argc;
  typeset -i start_t bench1 bench2 n rc;
  local argc=$#;
  #local argv2=$@;

  local start_t=0;      # memorize start time
  local bench1=0;       # first benchmark to test
  local bench2=5;       # last benchmark to test
  local n=1000000;      # maximum number
  
  get_ms;
  #echo "DEBUG: main";
  start_t=$get_ms;
  
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

  determineTsPrecision;
  start_bench "$bench1" "$bench2" "$n";
  local rc=$start_bench;

  get_ms;
  ((start_t=get_ms - start_t));
  echo "Total elapsed time: $start_t ms";
  return $rc;
}

#######################################

set +u
if [ "$ZSH_VERSION" ]; then
  #echo "DEBUG: zsh: ksh arrays...";
  set -o KSH_ARRAYS # for zsh: array indices start with 0
fi
set -u

main "$@"; # or: "$*";

exit;

#end

