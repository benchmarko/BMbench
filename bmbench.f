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
C
C
C Usage (gfortran):
C gfortran.exe -O2 -Wall -Wsurprising -Wunused -o bmbench_f bmbench.f
C bmbench [bench1] [bench2] [n]
C
C Usage (g77):
C g77 -O2 -Wall -Wsurprising -Wunused -fpedantic bmbench.f -o bmbench
C bmbench [bench] [n]
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
C Maybe we can use AND().
C
C Performance:
C - Use PARAMETERS(...) to define constants!
C
C
C
      PROGRAM bmbench
      IMPLICIT NONE
      REAL G_START_TS, G_TSPRECMS
      INTEGER G_TSPRECCNT, G_TSMEASCNT
      COMMON /G1/ G_START_TS
      COMMON /GSTATE/ G_TSPRECMS, G_TSPRECCNT, G_TSMEASCNT

C IARGC(), CALL GETARG are not very portable but supported on most UNIX plattforms.
C GETARG is intrinsic for GCU fortran (gfortran), external for f2c?
C (Do not know, how to pass intrinsic function GETARG to main, so we call in in xxx directly)
C (f2c has no IARGC())
      CALL MAIN()
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
      SUBROUTINE BENCH00(LOOPS, N, CHECK1, XRET)
      INTEGER LOOPS, N, CHECK1, XRET
      INTEGER L, I
C A short integer data type, INTEGER*2, holds a signed integer (not standard but f77).
      INTEGER*2 X, NDIV, NMOD, J
      X = 0
C      PRINT *, 'DEBUG: bench00: X=', X, ' CHECK1=', CHECK1

C      SUM1 = (N / 2) * (N + 1)
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

C     X = X .AND. X'ffff'
      X = MOD(X, 65536)
      X = X - CHECK1
C      PRINT *, 'DEBUG: sum1_2: I=', I, ' L=', L, ' X=', X, ' CHECK1=', CHECK1
      IF (X .NE. 0) THEN
        GOTO 25
      ENDIF
   20 CONTINUE
   25 XRET = MOD(X, 65536)
C      PRINT *, 'DEBUG: ret2: I=', I, ' L=', L, ' X=', X
      RETURN
      END
C
C
C bench01 (Integer 16/32 bit)
C (sum of 1..n) mod 65536
C
      SUBROUTINE BENCH01(LOOPS, N, CHECK1, X)
      INTEGER LOOPS, N, CHECK1, X, SUM1
      INTEGER L, I
C      SUM1 = (N / 2) * (N + 1)
      X = 0

      DO 20 L = 1, LOOPS
        SUM1 = 0
        DO 10 I = 1, N
          SUM1 = SUM1 + I
C          PRINT *, 'add: I=', I, ' L=', L, ' X=', X
          IF (SUM1 .GE. N) THEN
            SUM1 = SUM1 - N
            X = X + 1
          ENDIF
   10   CONTINUE
C      PRINT *, 'DEBUG: sum1: I=', I, ' L=', L, ' X=', X, ' sum1=', SUM1
        X = X - CHECK1
        IF (X .NE. 0) THEN
          RETURN
        ENDIF
   20 CONTINUE
C      PRINT *, 'DEBUG: ret2: I=', I, ' L=', L, ' X=', X
      RETURN
      END
C
C
C bench02 (Floating Point, normally 64 bit)
C (sum of 1..n) mod 65536
C
      SUBROUTINE BENCH02(LOOPS, N, CHECK1, X)
      DOUBLE PRECISION SUM1
      INTEGER LOOPS, N, CHECK1, X
      INTEGER L, I
C      SUM1 = (N / 2.0) * (N + 1.0)
      X = 0
      DO 40 L = 1, LOOPS
        SUM1 = 0.0
        DO 30 I = 1, N
          SUM1 = SUM1 + I
          IF (SUM1 .GE. N) THEN
            SUM1 = SUM1 - N
            X = X + 1
          ENDIF
C          PRINT *, 'DEBUG: add: I=', I, ' L=', L, ' X=', X
   30   CONTINUE
C      PRINT *, 'DEBUG: sum1: I=', I, ' L=', L, ' X=', X, ' sum1=', SUM1
C     Some more loops left? -> set X back to 0
      X = X - CHECK1
      IF (X .NE. 0) THEN
        GOTO 45
      ENDIF
   40 CONTINUE
C      PRINT *, 'DEBUG: ret2: I=', I, ' L=', L, ' X=', X
   45 RETURN
      END
C

C
C bench03 (Integer)
C number of primes below n (Sieve of Eratosthenes)
C Example: n=500000 => x=41538 (expected), n=1000000 => x=78498
C
      SUBROUTINE BENCH03(LOOPS, N_P, CHECK1, X)
      INTEGER LOOPS, N_P, CHECK1, X
      INTEGER MAX_HALFN
C      PARAMETER (MAX_N = 500000)
      PARAMETER (MAX_HALFN = 250000)
      INTEGER N, NHALF, L, I, M, J
C array size must be known at compile time, so use a constant
      LOGICAL SIEVE1(0:MAX_HALFN)
C Compute only up to n/2
      N = N_P / 2
      NHALF = N / 2

C x=Number of primes below n
      X = 0

      IF (NHALF .GT. MAX_HALFN) THEN
        PRINT *, 'Error: n too large: ', N, ' > ', (MAX_HALFN * 2)
        X  = -1
        RETURN
      ENDIF

      DO 40 L = 1, LOOPS
C Initialize sieve...
        DO 10 I = 0, NHALF
        SIEVE1(I) = .FALSE.
   10   CONTINUE

C Compute primes
       I = 0
       M = 3
       DO 20 WHILE ((M * M) .LT. N)
         IF (.NOT. SIEVE1(I)) THEN
           X = X + 1
           DO 15 J = (M * M - 3) / 2, NHALF - 1, M
             SIEVE1(J) = .TRUE.
   15      CONTINUE
         ENDIF
         I = I + 1
         M = M + 2
   20  CONTINUE

C Count primes...
        DO 30 I = I, NHALF
          IF (.NOT. SIEVE1(I)) THEN
            X = X + 1
          ENDIF
   30   CONTINUE

C Check prime count
        X = X - CHECK1
        IF (X .NE. 0) THEN
          GOTO 45
        ENDIF
   40 CONTINUE
C      PRINT *, 'DEBUG: ret2: I=', I, ' L=', L, ' X=', X, ' I_X=', I_X
   45 RETURN
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
      SUBROUTINE BENCH04(LOOPS, N, CHECK1, X)
      INTEGER LOOPS, N, CHECK1, X
C Define some constants...
      INTEGER M, A, Q, R
      PARAMETER (M = 2147483647, A = 16807, Q = 127773, R = 2836)
      INTEGER L, I
      INTEGER X_DIV_Q, X_MOD_Q
      X = 0
      DO 80 L = 1, LOOPS
        X = X + 1
        DO 70 I = 1, N
          X_DIV_Q = X / Q
          X_MOD_Q = X - Q * X_DIV_Q
          X = A * X_MOD_Q - R * X_DIV_Q
C not faster:  X = A * MOD(X, Q) - R * INT(X / Q)
          IF (X .LE. 0) THEN
            X = X + M
          ENDIF
   70   CONTINUE
C      PRINT *, 'DEBUG: I=', I, ' L=', L, ' X=', X
        X = X - CHECK1
        IF (X .NE. 0) THEN
          GOTO 85
        ENDIF        
   80 CONTINUE
   85 RETURN
      END
C
C
C
C bench05 (Integer 32 bit)
C n over n/2 mod 65536 (Pascal's triangle)
C
      SUBROUTINE BENCH05(LOOPS, N_P, CHECK1, X)
      INTEGER LOOPS, N_P, CHECK1, X
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

        X = X - CHECK1
        IF (X .NE. 0) THEN
          GOTO 85
        ENDIF
   80 CONTINUE
   85 RETURN
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
        CHECK1 = MOD((N / 2) * (N + 1), 65536)
        CALL BENCH00(LOOPS, N, CHECK1, X)
      ELSE IF (BENCH .EQ. 1) THEN
        CHECK1 = (N + 1) / 2
        CALL BENCH01(LOOPS, N, CHECK1, X)
C        PRINT *, 'ret3: I=', I, ' L=', L, ' X=', X
      ELSE IF (BENCH .EQ. 2) THEN
        CHECK1 = (N + 1) / 2
        CALL BENCH02(LOOPS, N, CHECK1, X)
      ELSE IF (BENCH .EQ. 3) THEN
        CHECK1 = 41538
        CALL BENCH03(LOOPS, N, CHECK1, X)
      ELSE IF (BENCH .EQ. 4) THEN
        CHECK1 = 1227283347
        CALL BENCH04(LOOPS, N, CHECK1, X)
      ELSE IF (BENCH .EQ. 5) THEN
        CHECK1 = 27200
        CALL BENCH05(LOOPS, N, CHECK1, X)
      ELSE
        PRINT *, 'Error: Unknown benchmark: ', BENCH
        CHECK1 = -1
      ENDIF
      X = X + CHECK1
      IF (X .NE. CHECK1) THEN
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
      INTEGER FUNCTION GET_NUMARG(IDX)
      INTEGER IDX
C      EXTERNAL ARGV
C     For GNU fortran GETARG is intrinsic and not external
      INTRINSIC GETARG 
      INTEGER NUM
      CHARACTER*25 CHBUF
C call the parameter function "ARGV" which is normally GETARG...
      CALL GETARG(IDX, CHBUF)
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
      REAL FUNCTION GET_RAW_TS()
      REAL R_MS
      CALL CPU_TIME(R_MS)
      GET_RAW_TS = R_MS
      RETURN
      END
C
C
      REAL FUNCTION GET_TS()
      REAL G_START_TS
      COMMON /G1/ G_START_TS
      GET_TS = GET_RAW_TS() - G_START_TS
      RETURN
      END
C
C
      REAL FUNCTION CONV_MS(TS)
      REAL TS
      CONV_MS = TS * 1000.0
      RETURN
      END
C
C get timestamp in milliseconds
C out: x = time in ms
C
C This function is intended for short measurements only
C
      REAL FUNCTION GET_MS_XXX()
C      FUNCTION GET_MS()
      REAL R_MS
      CALL CPU_TIME(R_MS)
      GET_MS_XXX = R_MS * 1000.0
      RETURN
      END
C
C
      REAL FUNCTION CORRECT_TIME(TMEAS, TMEAS2, MEASCOUNT)
      REAL TMEAS
      INTEGER MEASCOUNT, TSPRECCNT

      REAL G_TSPRECMS
      INTEGER G_TSPRECCNT, G_TSMEASCNT
      COMMON /GSTATE/ G_TSPRECMS, G_TSPRECCNT, G_TSMEASCNT

      TSPRECCNT = G_TSPRECCNT
      IF (MEASCOUNT < TSPRECCNT) THEN
        TMEAS = TMEAS + G_TSPRECMS *
     * ((TSPRECCNT - MEASCOUNT) / TSPRECCNT)
        IF (TMEAS > TMEAS2) THEN
          TMEAS = TMEAS2
        ENDIF
      ENDIF
        CORRECT_TIME = TMEAS
      RETURN
      END
C
C
      REAL FUNCTION GETPRECMS(STOPFLG)
      LOGICAL STOPFLG
      INTEGER MEASCOUNT
      REAL TMEAS0, TMEAS, TMEASD

      REAL G_TSPRECMS
      INTEGER G_TSPRECCNT, G_TSMEASCNT
      COMMON /GSTATE/ G_TSPRECMS, G_TSPRECCNT, G_TSMEASCNT

      MEASCOUNT = 0
      TMEAS0 = GET_TS()
      TMEAS = TMEAS0
      DO 150 WHILE (TMEAS .LE. TMEAS0)
        TMEAS = GET_TS()
        MEASCOUNT = MEASCOUNT + 1
  150 CONTINUE

      G_TSMEASCNT = MEASCOUNT

      IF (STOPFLG .EQV. .TRUE.) THEN
        TMEASD = CORRECT_TIME(CONV_MS(TMEAS0), CONV_MS(TMEAS),
     *    MEASCOUNT)
      ELSE
        TMEASD = CONV_MS(TMEAS)
      ENDIF
      GETPRECMS = TMEASD
      RETURN
      END
C
C
      SUBROUTINE DETERMINETSPRECISION()
      REAL TMEAS0, TMEAS1

      REAL G_START_TS
      COMMON /G1/ G_START_TS
      REAL G_TSPRECMS
      INTEGER G_TSPRECCNT, G_TSMEASCNT
      COMMON /GSTATE/ G_TSPRECCNT, G_TSPRECMS, G_TSMEASCNT
      
      G_START_TS = GET_RAW_TS()

      TMEAS0 = GETPRECMS(.FALSE.)
      TMEAS1 = GETPRECMS(.FALSE.)
      G_TSPRECMS = TMEAS1 - TMEAS0
      G_TSPRECCNT = G_TSMEASCNT
C     DO IT AGAIN
      TMEAS0 = TMEAS1
      TMEAS1 = GETPRECMS(.FALSE.)
      IF (G_TSMEASCNT .GT. G_TSPRECCNT) THEN
        G_TSPRECCNT = G_TSMEASCNT
        G_TSPRECMS = TMEAS1 - TMEAS0
      ENDIF
      RETURN
      END
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
      RETURN
      END
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
      RETURN
      END
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
      RETURN
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
      RETURN
      END
C
C
C
      SUBROUTINE PRINT_INFO()
      INTEGER SBITS, IBITS, FBITS, DBITS

      REAL G_TSPRECMS
      INTEGER G_TSPRECCNT, G_TSMEASCNT
      COMMON /GSTATE/ G_TSPRECCNT, G_TSPRECMS, G_TSMEASCNT

      CALL CHECKBITS_SHORT1(SBITS)
      CALL CHECKBITS_INT1(IBITS)
      CALL CHECKBITS_FLOAT1(FBITS)
      CALL CHECKBITS_DOUBLE1(DBITS)
  10  FORMAT('BM Bench v0.7 (Fortran) -- (short:', I2, ' int:', I2,
     * ' float:', I2, ' double:', I2, ' tsMs:', F9.6,
     * ' tsCnt:', I6, ') version: ?')
      PRINT 10, SBITS, IBITS, FBITS, DBITS, G_TSPRECMS, G_TSPRECCNT
C TTT why is G_TSPRECCNT real??
      PRINT *, '(c) Marco Vieth, 2002-2019'
      RETURN
      END
C
C
      SUBROUTINE PRINT_RESULTS(BENCH1, BENCH2, BENCH_RES1, MAX_BENCH)
      INTEGER BENCH1, BENCH2, MAX_BENCH
      REAL BENCH_RES1(0:MAX_BENCH)
      INTEGER BENCH

      PRINT *
      PRINT *, 'Throughput for all benchmarks (loops per sec):'
  20  FORMAT('BMR (Fortran)   : ',$)
      PRINT 20
  30  FORMAT(F9.3,' ',$)
      DO 500 BENCH = BENCH1, BENCH2
C        PRINT *, 'DEBUG: b=', BENCH, ', br1=', BENCH_RES1(BENCH)
C        PRINT *, BENCH_RES1(BENCH), ' '
        PRINT 30, BENCH_RES1(BENCH)
  500 CONTINUE
      RETURN
      END
C
C
      REAL FUNCTION MEASURE_BENCH(BENCH, N)
      INTEGER BENCH, N

      INTEGER CALI_MS, DELTA_MS, MAX_MS, LOOPS, X
      REAL T1, T2, THROUGHPUT, T_DELTA, LOOPS_P_SEC
      CHARACTER*7 PRG_LANGUAGE
      
      PRG_LANGUAGE = 'Fortran'

      CALI_MS = 1001
      DELTA_MS = 100
      MAX_MS = 10000
      LOOPS = 1
      X = 0
      T1 = 0
      T2 = 0
      THROUGHPUT = 0

      PRINT *, 'Calibrating benchmark ', BENCH, ' with n=', N
      DO 220 WHILE (THROUGHPUT .eq. 0)
        T1 = GETPRECMS(.FALSE.)
C        X = RUN_BENCH(BENCH, LOOPS, N)
        CALL RUN_BENCH(BENCH, LOOPS, N, X)
        T1 = GETPRECMS(.TRUE.) - T1
  
        IF (T2 .GT. T1) THEN
          T_DELTA = T2 - T1
        ELSE
          T_DELTA = T1 - T2
        ENDIF     

        IF (T1 .GT. 0) THEN
          LOOPS_P_SEC = LOOPS * 1000.0 / T1
        ELSE
          LOOPS_P_SEC = 0
        ENDIF

   80   FORMAT(F10.3, '/s (time=', F9.3, ' ms, loops=', I7,
     *    ' delta=', F9.3, ' ms, x=', I10)
        PRINT 80, LOOPS_P_SEC, T1, LOOPS, T_DELTA, X

        IF (X .EQ. -1) THEN
          THROUGHPUT = -1
        ELSEIF ((T2 .GT. 0) .AND. (T_DELTA .LT. DELTA_MS)) THEN
          THROUGHPUT = LOOPS_P_SEC
  120     FORMAT('Benchmark ', I1, ' (', A, '): ',
     *      F9.3, '/s (time=', F9.3, ' ms, loops=', I7,
     *      ', delta=', F9.3, ' ms)')
          PRINT 120, BENCH, PRG_LANGUAGE, LOOPS_P_SEC, T1, LOOPS,
     *      T_DELTA
        ELSEIF (T1 > MAX_MS) THEN
          PRINT *, 'Benchmark ', BENCH, ' (', PRG_LANGUAGE,
     *     '): Time already > ', MAX_MS, ' ms. No measurement possible.'
          IF (LOOPS_P_SEC .GT. 0) THEN
            THROUGHPUT = -LOOPS_P_SEC
          ELSE
            THROUGHPUT = -1
          ENDIF
        ELSE
          IF (T1 .EQ. 0) THEN
            SCALE_FACT = 50
          ELSEIF (T1 .LT. CALI_MS) THEN
            SCALE_FACT = INT(((CALI_MS + 100) / T1) + 1)
          ELSE
            SCALE_FACT = 2
          ENDIF
          LOOPS = INT(LOOPS * SCALE_FACT)
          T2 = T1 * SCALE_FACT
        ENDIF
  220 CONTINUE
      MEASURE_BENCH = THROUGHPUT
      RETURN
      END
C
C
      SUBROUTINE START_BENCH(BENCH1, BENCH2, N, MAX_BENCH)
      INTEGER BENCH1, BENCH2, N, MAX_BENCH
      REAL THROUGHPUT
      REAL MEASURE_BENCH
      REAL BENCH_RES1(0:MAX_BENCH)
      INTEGER BENCH

      CALL PRINT_INFO()
      DO 70 BENCH = BENCH1, BENCH2
        THROUGHPUT = MEASURE_BENCH(BENCH, N)
        BENCH_RES1(BENCH) = THROUGHPUT
  70  CONTINUE
      CALL PRINT_RESULTS(BENCH1, BENCH2, BENCH_RES1, MAX_BENCH)
      RETURN
      END
C
C   main
C
      SUBROUTINE MAIN()
      INTEGER MAX_BENCH
      PARAMETER (MAX_BENCH = 5)
      INTEGER BENCH1, BENCH2, N
C declare functions...
      INTEGER GET_NUMARG
      REAL GET_TS

      BENCH1 = 0
      BENCH2 = 5
      N = 1000000

      CALL DETERMINETSPRECISION()
C
C We don't use ARGC but scan numbers from GETARG until -1...
      IF (GET_NUMARG(1) .NE. -1) THEN
        BENCH1 = GET_NUMARG(1)
        IF (GET_NUMARG(2) .NE. -1) THEN
          BENCH2 = GET_NUMARG(2)
          IF (GET_NUMARG(3) .NE. -1) THEN
            N = GET_NUMARG(3)
          ENDIF
        ENDIF
      ENDIF
C
      IF ((BENCH1 .GT. MAX_BENCH) .OR. (BENCH2 .GT. MAX_BENCH)) THEN
        PRINT *, 'Error: Benchmark out of range! ', BENCH1, ' or ',
     *    BENCH2, ' > ', MAX_BENCH
        RETURN
      ENDIF

      CALL START_BENCH(BENCH1, BENCH2, N, MAX_BENCH)

      PRINT *
      PRINT *, 'Total elapsed time: ', INT(CONV_MS(GET_TS())), ' ms'
      RETURN
      END
C
C End
