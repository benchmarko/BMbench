C
C BM Bench - bmbench.f (Fortran)
C (c) Marco Vieth, 2002
C http://www.benchmarko.de
C
C 06.05.2002  0.01
C 14.05.2002  0.02  bench1 = (sum 1..n) mod 65536
C 20.07.2002  0.04  extended version
C 24.01.2003  0.05  output format changed
C
C Usage (g77):
C g77 -O2 -Wall -Wsurprising -Wunused -fpedantic bmbench.f -o bmbench
C .\bmbench [bench] [n]
C
C Testing (g77):
C -Wno-globals -Wimplicit -fbounds-check
C (with -fcase-strict-upper we cannot link GETARG)
C
C
C Usage (f2c):
C - f2c -A -a bmbench.f
C - gcc -O2 -Wall -Wtraditional -lf2c -lm bmbench_f2clib.c bmbench.c -o bmbench
C - (or for both:  f77-f2c bmbench.f)
C - Especially option '-a' to use automatic variables (locals) is useful.
C
C
C Info:
C - man g77, info g77
C
C Notes:
C - f2c does not like 'END <blabla>', so skip <blabla>
C - f2c does not know IARGC(), what to do?
C - Modulo of negative integers, how to compute?
C   MOD(N, 65526) ?
C   N - INT(N / 65536) * 65536) ?
C   (We expect (-531600832 % 65536) = 27200 and not -38336!)
C
C      X = X + PAS1(MOD(N, 2), K)
C how to do this more elegant?
C      IF (X .LT. 0) THEN
C        X = X + X'7fffffff'
C        X = X + 1
C      ENDIF
C      X = MOD(X, 65536)
C
C Maybe we can use AND()...
C
C
C
C
C Performance:
C - Use PARAMETERS(...) to define constants!
C
C
      PROGRAM bmbench
      IMPLICIT NONE
C IARGC(), CALL GETARG are not very portable but supported on most UNIX plattforms...
      EXTERNAL GETARG
C pass function GETARG to main...
C Normally we would use "CALL MAIN(IARGC(), GETARG)" nut f2c has no IARGC()
      CALL MAIN(0, GETARG)
      END
C
C
C
C General description for benchmark test functions
C benchxx - benchmark
C <description>
C in: loops = number of loops
C         n = maximum number (assumed even, normally n=1000000)
C out:    x = <output decription>
C
C loops may be increased to produce a longer runtime without changing the result.
C
C
C bench00 (Integer 16 bit)
C (sum of 1..n) mod 65536
C
      SUBROUTINE BENCH00(LOOPS, N, XRET)
      INTEGER LOOPS, N, XRET
      INTEGER L, I
C A short integer data type, INTEGER*2, holds a signed integer (not standard but f77).
      INTEGER*2 X, SUM1, NDIV, NMOD, J
      X = 0
      SUM1 = (N / 2) * (N + 1)
      NDIV = (N / 65536)
C      NMOD = (N .AND. X'ffff')
      NMOD = MOD(N, 65536)
C      PRINT *, 'DEBUG: SUM1=', SUM1, ' NDIV=', NDIV, ' NMOD=', NMOD

      DO 20 L = 1, LOOPS
        DO 15 I = 1, NDIV
          DO 10 J = 32767, 1, -1
            X = X + J
   10     CONTINUE
          DO 12 J = -32767, -1
            X = X + J
   12     CONTINUE
C The compiler does not like -32768, so add it separately...
          X = X + (-32768)
   15   CONTINUE
        DO 17 J = 1, NMOD
          X = X + J
   17   CONTINUE

C      PRINT *, 'DEBUG: sum1: I=', I, ' L=', L, ' X=', X
C     Some more loops left? -> set X back to 0
      IF (L .LT. LOOPS) THEN
        X = X - SUM1
        IF (X .NE. 0) THEN
          XRET = X + 1
          RETURN
        ENDIF
      ENDIF
   20 CONTINUE
      XRET = MOD(X, 65536)
C      PRINT *, 'DEBUG: ret2: I=', I, ' L=', L, ' X=', X
      RETURN
      END
C
C
C bench01 (Integer 16/32 bit)
C (sum of 1..n) mod 65536
C
      SUBROUTINE BENCH01(LOOPS, N, X)
      INTEGER LOOPS, N, X, SUM1
      INTEGER L, I
      SUM1 = (N / 2) * (N + 1)
      X = 0

      DO 20 L = 1, LOOPS
        DO 10 I = 1, N
          X = X + I
C          PRINT *, 'add: I=', I, ' L=', L, ' X=', X
   10   CONTINUE
C      PRINT *, 'DEBUG: sum1: I=', I, ' L=', L, ' X=', X, ' sum1=', SUM1
C     Some more loops left? -> set X back to 0
      IF (L .LT. LOOPS) THEN
        X = X - SUM1
        IF (X .NE. 0) THEN
          X = X + 1
          RETURN
        ENDIF
      ENDIF
   20 CONTINUE
      X = MOD(X, 65536)
C      PRINT *, 'DEBUG: ret2: I=', I, ' L=', L, ' X=', X
      RETURN
      END
C
C
C bench02 (Floating Point, normally 64 bit)
C (sum of 1..n) mod 65536
C
      SUBROUTINE BENCH02(LOOPS, N, I_X)
      DOUBLE PRECISION X, SUM1
      INTEGER LOOPS, N, I_X
      INTEGER L, I
      SUM1 = (N / 2.0) * (N + 1.0)
      X = 0.0
      DO 40 L = 1, LOOPS
        DO 30 I = 1, N
          X = X + I
C          PRINT *, 'DEBUG: add: I=', I, ' L=', L, ' X=', X
   30   CONTINUE
C      PRINT *, 'DEBUG: sum1: I=', I, ' L=', L, ' X=', X, ' sum1=', SUM1
C     Some more loops left? -> set X back to 0
      IF (L .LT. LOOPS) THEN
        X = X - SUM1
        IF (X .NE. 0.0) THEN
          X = X + 1
          I_X = INT(X)
          RETURN
        ENDIF
      ENDIF
   40 CONTINUE
C      X = MOD(X, 65536.0)  "does not work
C      PRINT *, 'DDD: ttt =', INT(X / 65536.0) * 65536.0
      X = X - (INT(X / 65536.0) * 65536.0)
      I_X = INT(X)
C      PRINT *, 'DEBUG: ret2: I=', I, ' L=', L, ' X=', X, ' I_X=', I_X
      RETURN
      END
C

C
C bench03 (Integer)
C number of primes below n (Sieve of Eratosthenes)
C Example: n=500000 => x=41538 (expected), n=1000000 => x=78498
C
      SUBROUTINE BENCH03(LOOPS, N_P, X)
      INTEGER LOOPS, N_P, X
      INTEGER MAX_N
      PARAMETER (MAX_N = 500000)
      INTEGER N, L, I, J
C array size must be known at compile time, so use a constant
      LOGICAL SIEVE1(0:MAX_N)
C Compute only up to n/2
      N = N_P / 2

C x=Number of primes below n
      X = 0

      IF (N .GT. MAX_N) THEN
        PRINT *, 'Error: n too large: ', N, ' > ', MAX_N
        X  = -1
        RETURN
      ENDIF

C Numbers 0 and 1 are not primes
      SIEVE1(0) = .FALSE.
      SIEVE1(1) = .FALSE.

      DO 40 L = 1, LOOPS

C Initialize sieve...
        DO 10 I = 2, N
        SIEVE1(I) = .TRUE.
   10   CONTINUE

C Compute primes
       I = 2
       DO 20 WHILE ((I * I) .LE. N)
         IF (SIEVE1(I)) THEN
           DO 15 J = I * I, N, I
             SIEVE1(J) = .FALSE.
   15      CONTINUE

         ENDIF
         I = I + 1
   20  CONTINUE

C Count primes...
        DO 30 I = 0, N
          IF (SIEVE1(I)) THEN
            X = X + 1
          ENDIF
   30   CONTINUE

C Check prime count
C     Some more loops left? -> set X back to 0
      IF (L .LT. LOOPS) THEN
        X = X - 41538
        IF (X .NE. 0) THEN
          X = X + 1
          RETURN
        ENDIF
      ENDIF
   40 CONTINUE
C      PRINT *, 'DEBUG: ret2: I=', I, ' L=', L, ' X=', X, ' I_X=', I_X
      RETURN
      END
C
C
C
C bench04 (Integer 32 bit)
C nth random number number
C Random number generator taken from
C Raj Jain: The Art of Computer Systems Performance Analysis, John Wiley & Sons, 1991, page 442-444.
C It needs longs with at least 32 bit.
C Starting with x0=1, x10000 should be 1043618065, x1000000 = 1227283347.
C
      SUBROUTINE BENCH04(LOOPS, N, X)
      INTEGER LOOPS, N, X
      INTEGER M, A, Q, R
C Define some constants...
      PARAMETER (M = 2147483647, A = 16807, Q = 127773, R = 2836)
      INTEGER L, I
      INTEGER X_DIV_Q, X_MOD_Q
      X = 1
      DO 80 L = 1, LOOPS
        DO 70 I = 1, N
          X_DIV_Q = X / Q
          X_MOD_Q = X - Q * X_DIV_Q
          X = A * X_MOD_Q - R * X_DIV_Q
          IF (X .LE. 0) THEN
            X = X + M
          ENDIF
   70   CONTINUE
C      PRINT *, 'DEBUG: I=', I, ' L=', L, ' X=', X
C     Some more loops left? -> set X back to 0
        IF (L .LT. LOOPS) THEN
          X = X - 1227283347
          IF (X .NE. 0) THEN
            X = X + 1
            RETURN
          ENDIF
          X = X + 1
        ENDIF
   80 CONTINUE
      RETURN
      END
C
C
C
C bench05 (Integer 32 bit)
C n over n/2 mod 65536 (Pascal's triangle)
C
      SUBROUTINE BENCH05(LOOPS, N_P, X)
      INTEGER LOOPS, N_P, X
      INTEGER MAX_N
      PARAMETER (MAX_N = 1000000 / (500 * 2))
      INTEGER N, K
      INTEGER L, I, I_MOD_2, I1_MOD_2, MIN1, J
      INTEGER PAS1(0:1,0:MAX_N)

      X = 0
      N = N_P / 500
      K = N / 2

C keep k minimal with  n over k  =  n over n-k
      IF ((N - K) < K) THEN
        K = N - K
      ENDIF

      IF (K .GT. MAX_N) THEN
        PRINT *, 'Error: k too large: ', K, ' > ', MAX_N
        X  = -1
        RETURN
      ENDIF

C Set first column
      PAS1(0, 0) = 1
      PAS1(1, 0) = 1
      
C      PRINT *, 'DEBUG: LOOPS=', LOOPS, ', N=', N, ', K=', K
      DO 80 L = 1, LOOPS
        DO 70 I = 2, N
          I_MOD_2 = MOD(I, 2)
          I1_MOD_2 = MOD(I + 1, 2)
          MIN1 = (I - 1) / 2
          IF (K .LT. MIN1) THEN
            MIN1 = K
          ENDIF
C Second column is i
          PAS1(I_MOD_2, 1) = I
C          PRINT *, 'DEBUG: L=', L, ', I=', I, ', MIN1=', MIN1,
C     *  ', P1=', PAS1(I_MOD_2, 10)
C up to min((i-1)/2, k)
          DO 60 J = 2, MIN1
            PAS1(I_MOD_2, J) = PAS1(I1_MOD_2, J - 1) + PAS1(I1_MOD_2, J)
   60     CONTINUE
C new element?
          IF ((MIN1 .LE. K) .AND. (I_MOD_2 .EQ. 0)) THEN
            PAS1(I_MOD_2, MIN1 + 1) = 2 * PAS1(I1_MOD_2, MIN1)
C          PRINT *, 'DEBUG: PAS1(', I_MOD_2, ',', MIN1 + 1, ')=',
C     * PAS1(I_MOD_2, MIN1 + 1)
          ENDIF
   70   CONTINUE
C      PRINT *, 'DEBUG: H=', PAS1(MOD(N, 2), K)

C We expect (-531600832 % 65536) = 27200 and not -38336!
      X = AND(X + PAS1(MOD(N, 2), K), X'ffff')

C     Some more loops left? -> set X back to 0
        IF (L .LT. LOOPS) THEN
          X = X - 27200
          IF (X .NE. 0) THEN
            X = X + 1
            RETURN
          ENDIF
        ENDIF
   80 CONTINUE
      RETURN
      END
C
C
C
C run a benchmark
C in: bench = benchmark to use
C     loops = number of loops
C         n = maximum number (used in some benchmarks to define size of workload)
C out:    x = result
C
      SUBROUTINE RUN_BENCH(BENCH, LOOPS, N, X)
      INTEGER BENCH, LOOPS, N, X
      INTEGER CHECK1
      X = 0
      CHECK1 = 0
      IF (BENCH .EQ. 0) THEN
        CALL BENCH00(LOOPS, N, X)
        CHECK1 = 10528
      ELSE IF (BENCH .EQ. 1) THEN
        CALL BENCH01(LOOPS, N, X)
        CHECK1 = 10528
C        PRINT *, 'ret3: I=', I, ' L=', L, ' X=', X
      ELSE IF (BENCH .EQ. 2) THEN
        CALL BENCH02(LOOPS, N, X)
        CHECK1 = 10528
      ELSE IF (BENCH .EQ. 3) THEN
        CALL BENCH03(LOOPS, N, X)
        CHECK1 = 41538
      ELSE IF (BENCH .EQ. 4) THEN
        CALL BENCH04(LOOPS, N, X)
        CHECK1 = 1227283347
      ELSE IF (BENCH .EQ. 5) THEN
        CALL BENCH05(LOOPS, N, X)
        CHECK1 = 27200
      ELSE
        PRINT *, 'Error: Unknown benchmark: ', BENCH
        CHECK1 = X + 1
      ENDIF
      IF (CHECK1 .NE. X) THEN
        PRINT *, 'Error(bench', BENCH, '): x=', X
        X = -1
      ENDIF
      RETURN
      END
C
C
C
C get_numarg - get numerical arguments
C
      INTEGER FUNCTION GET_NUMARG(IDX, ARGV)
      INTEGER IDX
      EXTERNAL ARGV
      INTEGER NUM
      CHARACTER*25 CHBUF
C call the parameter function "ARGV" which is normally GETARG...
      CALL ARGV(IDX, CHBUF)
C      PRINT *, 'DEBUG: get_numarg: argv =--', CHBUF(:1), '--'
C     Check if first character is a number between 0 and 9...
      IF ((CHBUF(:1) .GE. "0") .AND. (CHBUF(:1) .LE. "9")) THEN
        READ(CHBUF, *) NUM
      ELSE
C Undefined value
        NUM = -1
      ENDIF
      GET_NUMARG = NUM
      RETURN
      END
C
C
C
C get timestamp in milliseconds
C out: x = time in ms
C
C This function is intended for short measurements only so we
C can return it as an integer.
C
      INTEGER FUNCTION GET_MS()
C      FUNCTION GET_MS()
      REAL R_MS
      CALL CPU_TIME(R_MS)
      GET_MS = INT(R_MS * 1000)
      RETURN
      END
C
C
C
C
      SUBROUTINE CHECKBITS_SHORT1(BITS)
      INTEGER BITS
      INTEGER*2 NUM, LAST_NUM
      NUM = 1
      LAST_NUM = 0
      BITS = 0
      DO 150 WHILE ((((NUM - 1) / 2) .EQ. LAST_NUM) .AND.
     * (BITS .LT. 101))
        LAST_NUM = NUM
        NUM = NUM * 2
        NUM = NUM + 1
        BITS = BITS + 1
  150 CONTINUE
  160 RETURN
      END
C
C
C
C
      SUBROUTINE CHECKBITS_INT1(BITS)
      INTEGER BITS
      INTEGER NUM, LAST_NUM
      NUM = 1
      LAST_NUM = 0
      BITS = 0
      DO 150 WHILE ((((NUM - 1) / 2) .EQ. LAST_NUM) .AND.
     * (BITS .LT. 101))
        LAST_NUM = NUM
        NUM = NUM * 2
        NUM = NUM + 1
        BITS = BITS + 1
  150 CONTINUE
  160 RETURN
      END
C
C
C
C
      SUBROUTINE CHECKBITS_FLOAT1(BITS)
      INTEGER BITS
      REAL NUM, LAST_NUM
      NUM = 1.0
      LAST_NUM = 0.0
      BITS = 0
      DO 150 WHILE ((((NUM - 1.0) / 2.0) .EQ. LAST_NUM) .AND.
     * (BITS .LT. 101))
        LAST_NUM = NUM
        NUM = NUM * 2.0
        NUM = NUM + 1.0
        BITS = BITS + 1
  150 CONTINUE
  160 RETURN
      END
C
C
C
      SUBROUTINE CHECKBITS_DOUBLE1(BITS)
      INTEGER BITS
      DOUBLE PRECISION NUM, LAST_NUM
      NUM = 1.0
      LAST_NUM = 0.0
      BITS = 0
      DO 150 WHILE ((((NUM - 1.0) / 2.0) .EQ. LAST_NUM) .AND.
     * (BITS .LT. 101))
        LAST_NUM = NUM
        NUM = NUM * 2.0
        NUM = NUM + 1.0
        BITS = BITS + 1
  150 CONTINUE
  160 RETURN
      END

C


C
C   main
C
      SUBROUTINE MAIN(ARGC, ARGV)
      INTEGER ARGC
      EXTERNAL ARGV
C      IMPLICIT INTEGER (A-Z)
      INTEGER MAX_BENCH
      PARAMETER (MAX_BENCH = 5)
      INTEGER START_T, BENCH1, BENCH2, BENCH, N, MIN_MS
      INTEGER LOOPS, T1, T2
      INTEGER X
      INTEGER BENCH_RES1(0:MAX_BENCH)
C benchmark results 0..5
      INTEGER SBITS, IBITS, FBITS, DBITS
C declare functions...
      INTEGER GET_MS, GET_NUMARG

      START_T = GET_MS()
      BENCH1 = 0
      BENCH2 = 5
      N = 1000000
      MIN_MS = 10000
C
C We don't use ARGC but scan numbers from ARGV until -1...
      IF (GET_NUMARG(1, ARGV) .NE. -1) THEN
        BENCH1 = GET_NUMARG(1, ARGV)
        IF (GET_NUMARG(2, ARGV) .NE. -1) THEN
          BENCH2 = GET_NUMARG(2, ARGV)
          IF (GET_NUMARG(3, ARGV) .NE. -1) THEN
            N = GET_NUMARG(3, ARGV)
          ENDIF
        ENDIF
      ENDIF
C
      IF ((BENCH1 .GT. MAX_BENCH) .OR. (BENCH2 .GT. MAX_BENCH)) THEN
        PRINT *, 'Error: Benchmark out of range! ', BENCH1, ' or ',
     *    BENCH2, ' > ', MAX_BENCH
        RETURN
      ENDIF
C
      CALL CHECKBITS_SHORT1(SBITS)
      CALL CHECKBITS_INT1(IBITS)
      CALL CHECKBITS_FLOAT1(FBITS)
      CALL CHECKBITS_DOUBLE1(DBITS)
  10  FORMAT('BM Bench v0.5 (Fortran) -- (short:', I2, ' int:', I2,
     * ' float:', I2, ' double:', I2, ') version: ?')
      PRINT 10, SBITS, IBITS, FBITS, DBITS
      PRINT *, '(c) Marco Vieth, 2002'
C
C
C
      DO 400 BENCH = BENCH1, BENCH2
C      PRINT *, 'DEBUG: bench=', bench
        LOOPS = 1
        X = 0
        T1 = 0
        T2 = 0
C
C Calibration
C
        DO 300 WHILE ((T2 .LE. 1001) .AND. (X .NE. -1))
          PRINT *, 'Calibrating benchmark ', BENCH, ' with loops=',
     * LOOPS, ', n=', N
          T1 = GET_MS()
          CALL RUN_BENCH(BENCH, LOOPS, N, X)
          T2 = GET_MS() - T1
          PRINT *, 'x=', X, ' (time: ', T2, ' ms)'
          LOOPS = LOOPS * 2
C          IF ((T2 .GT. 1001) .OR. (X .EQ. -1)) THEN
C            GOTO 310
C          ENDIF
  300   CONTINUE
C        PRINT *, 'DEBUG: x=', X
        IF (X .NE. -1) THEN
          LOOPS = LOOPS / 2
          LOOPS = LOOPS * (MIN_MS / T2) + 1
          PRINT *, 'Calibration done. Starting measurement with ',
     * LOOPS, ' loops to get >=', MIN_MS, ' ms'
C
C Measurement
C
          T1 = GET_MS()
          CALL RUN_BENCH(BENCH, LOOPS, N, X)
          T2 = GET_MS() - T1
          PRINT *, 'x=', X, ' (time: ', T2, ' ms)'

          BENCH_RES1(BENCH) = (T2 * 10 / LOOPS)
          PRINT *, 'Elapsed time for ', LOOPS, ' loops: ', T2,
     * ' ms; estimation for 10 loops: ', BENCH_RES1(BENCH), ' ms'
        ELSE
          BENCH_RES1(BENCH) = -1
        ENDIF
C        PRINT *, 'DEBUG: b=', BENCH, ', br1=', BENCH_RES1(BENCH)
  400 CONTINUE
      PRINT *, 'Times for all benchmarks (10 loops, ms):'
  20  FORMAT('BM Results (Fortran)   : ',$)
      PRINT 20
  30  FORMAT(I7,' ',$)
      DO 500 BENCH = BENCH1, BENCH2
C        PRINT *, 'DEBUG: b=', BENCH, ', br1=', BENCH_RES1(BENCH)
C        PRINT *, BENCH_RES1(BENCH), ' '
        PRINT 30, BENCH_RES1(BENCH)
  500 CONTINUE
      PRINT *
      T2 = GET_MS()
      PRINT *, 'Total elapsed time: ', (T2 - START_T), ' ms'
      RETURN
      END
C
C
C
C End
