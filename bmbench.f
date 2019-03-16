c BM Bench - bmbench.f (Fortran)
c (c) Benchmarko, 2002
c
c 06.05.2002  0.01
c 14.05.2002  0.02  bench1 = (sum 1..n) mod 65536
c 20.07.2002  0.04  extended version
c
c Usage:
c g77 -Wall -Wsurprising -Wunused -fpedantic bmbench.f -o bmbench
c .\bmbench [bench] [n]
c
c
c (How to get command line options?)
c
c
      PROGRAM bmbench
      CALL MAIN
      END
c
c
c
c bench01
c compute (sum of 1..n) mod 65536
c in: loops = number of loops
c         n = maximum number (assumed even, if integer arithmetic, normally n=1000000)
c out:    x = (sum 1..n) mod 65536
c
c Loops may be increased to produce a longer runtime without
c changing the result.
c
c
      SUBROUTINE BENCH01(LOOPS, N, X)
      INTEGER LOOPS, N, X, SUM1
      INTEGER L, I
      SUM1 = (N / 2) * (N + 1)
      X = 0
      DO 20 L = 1, LOOPS
        DO 10 I = 1, N
          X = X + I
c          PRINT *, 'add: I=', I, ' L=', L, ' X=', X
   10   CONTINUE
c      PRINT *, 'sum1: I=', I, ' L=', L, ' X=', X, ' sum1=', SUM1
c     Some more loops left? -> set X back to 0
      IF (L .LT. LOOPS) THEN
        X = X - SUM1
        IF (X .NE. 0) THEN
          X = X + 1
          RETURN
        ENDIF
      ENDIF
   20 CONTINUE
      X = MOD(X, 65536)
c      PRINT *, 'ret2: I=', I, ' L=', L, ' X=', X
      RETURN
      END
c
c
c Floating Point
c
      SUBROUTINE BENCH02(LOOPS, N, I_X)
      DOUBLE PRECISION X, SUM1
      INTEGER LOOPS, N
      INTEGER L, I
      SUM1 = (N / 2.0) * (N + 1.0)
      X = 0.0
      DO 40 L = 1, LOOPS
        DO 30 I = 1, N
          X = X + I
c          PRINT *, 'add: I=', I, ' L=', L, ' X=', X
   30   CONTINUE
c      PRINT *, 'sum1: I=', I, ' L=', L, ' X=', X, ' sum1=', SUM1
c     Some more loops left? -> set X back to 0
      IF (L .LT. LOOPS) THEN
        X = X - SUM1
        IF (X .NE. 0.0) THEN
          X = X + 1
          I_X = INT(X)
          RETURN
        ENDIF
      ENDIF
   40 CONTINUE
c      X = MOD(X, 65536.0)  "does not work
c      PRINT *, 'DDD: ttt =', INT(X / 65536.0) * 65536.0
      X = X - (INT(X / 65536.0) * 65536.0)
      I_X = INT(X)
c      PRINT *, 'ret2: I=', I, ' L=', L, ' X=', X, ' I_X=', I_X
      RETURN
      END
c
c
c bench04 (Integer 32 bit) (still some problems...)
c nth random number number
c Random number generator taken from
c Raj Jain: The Art of Computer Systems Performance Analysis, John Wiley & Sons, 1991, page 442-444.
c It needs longs with at least 32 bit.
c Starting with x0=1, x10000 should be 1043618065, x1000000 = 1227283347.
c
      SUBROUTINE BENCH04(LOOPS, N, X)
      INTEGER LOOPS, N, X
      INTEGER M, A, Q, R
      INTEGER L, I
      INTEGER X_DIV_Q, X_MOD_Q
      M = 2147483647
      A = 16807
      Q = 127773
      R = 2836
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
c      PRINT *, 'ddd: I=', I, ' L=', L, ' X=', X
c     Some more loops left? -> set X back to 0
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
c
c
c
c run a benchmark
c in: bench = benchmark to use
c     loops = number of loops
c         n = maximum number (used in some benchmarks to define size of workload)
c out:    x = result
c
      SUBROUTINE RUN_BENCH(BENCH, LOOPS, N, X)
      INTEGER BENCH, LOOPS, N, X
      INTEGER CHECK1
      X = 0
      CHECK1 = 0
      IF (BENCH .EQ. 0) THEN
c        CALL BENCH00(LOOPS, N, X)
        CHECK1 = 10528
      ELSE IF (BENCH .EQ. 1) THEN
        CALL BENCH01(LOOPS, N, X)
        CHECK1 = 10528
c        PRINT *, 'ret3: I=', I, ' L=', L, ' X=', X
      ELSE IF (BENCH .EQ. 2) THEN
        CALL BENCH02(LOOPS, N, X)
        CHECK1 = 10528
      ELSE IF (BENCH .EQ. 3) THEN
c        CALL BENCH03(LOOPS, N, X)
        CHECK1 = 41538
      ELSE IF (BENCH .EQ. 4) THEN
        CALL BENCH04(LOOPS, N, X)
        CHECK1 = 1227283347
      ELSE IF (BENCH .EQ. 5) THEN
c        CALL BENCH05(LOOPS, N, X)
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
c
c
c
c
c get timestamp in milliseconds
c out: x = time in ms
c
c This function is intended for short measurements only so we
c can return it as an integer.
c
      SUBROUTINE GET_MS(I_MS)
      REAL R_MS
      CALL CPU_Time(R_MS)
      I_MS = INT(R_MS * 1000)
      RETURN
      END
c ??? SECONDS: `REAL'; scalar; INTENT(OUT).
c
c
c   main
c
      SUBROUTINE MAIN
c      IMPLICIT INTEGER (A-Z)
      INTEGER START_T, BENCH1, BENCH2, BENCH, N, MIN_MS
      INTEGER LOOPS, T1, T2
      INTEGER X
      INTEGER BENCH_RES1(5)

      CALL GET_MS(START_T)
      BENCH1 = 1
      BENCH2 = 5
      N = 1000000
      MIN_MS = 10000
      PRINT *, 'BM Bench v0.4 (Fortran)'
c
c
c
      DO 400 BENCH = BENCH1, BENCH2
        LOOPS = 1
        X = 0
        T1 = 0
        T2 = 0
c
c Calibration
c
        DO 300
          PRINT *, 'Calibrating benchmark ', BENCH, ' with loops=',
     * LOOPS, ', n=', N
          CALL GET_MS(T1)
          CALL RUN_BENCH(BENCH, LOOPS, N, X)
          CALL GET_MS(T2)
          T2 = T2 - T1
          PRINT *, 'x=', X, ' (time: ', T2, ' ms)'
          LOOPS = LOOPS * 2
          IF ((T2 .GT. 1001) .OR. (X .EQ. -1)) THEN
            GOTO 310
          ENDIF
  300   CONTINUE
  310   IF (X .NE. -1) THEN
          LOOPS = LOOPS / 2
          LOOPS = LOOPS * (MIN_MS / T2) + 1
          PRINT *, 'Calibration done. Starting measurement with ',
     * LOOPS, ' loops to get >=', MIN_MS, ' ms'
c
c Measurement
c
          CALL GET_MS(T1)
          CALL RUN_BENCH(BENCH, LOOPS, N, X)
          CALL GET_MS(T2)
          T2 = T2 - T1
          PRINT *, 'x=', X, ' (time: ', T2, ' ms)'

          PRINT *, 'Elapsed time for ', LOOPS, ' loops: ', T2,
     * ' ms; estimation for 10 loops: ', (T2 * 10 / loops), ' ms'
          BENCH_RES1(BENCH) = (T2 * 10 / loops)
        ELSE
          BENCH_RES1(BENCH) = -1
        ENDIF
  400 CONTINUE
      PRINT *, 'Summary for 10 Loops:'
      DO 500 BENCH = BENCH1, BENCH2
      PRINT *, 'Benchmark ', bench, ': ', bench_res1(bench), ' ms'
  500 CONTINUE
      CALL GET_MS(T2)
      PRINT *, 'Total elapsed time: ', (T2 - START_T), ' ms'
      RETURN
      END
c End
