*&---------------------------------------------------------------------*
*& Report  ZMVTEST5                                                    *
*&                                                                     *
*&---------------------------------------------------------------------*
*&                                                                     *
*& 21.08.2002  extended system info                                    *
*& 04.09.2002  calibration corrected                                   *
*& 11.02.2003  output format changed                                   *
*& 19.02.2003  ...                                                     *
*&                                                                     *
*&                                                                     *
*&---------------------------------------------------------------------*

REPORT ZMVTEST5 LINE-SIZE 120.

PARAMETERS: BENCH1 TYPE I DEFAULT 1,
            BENCH2 TYPE I DEFAULT 5,
            N TYPE I DEFAULT 1000000.

PERFORM MAIN USING BENCH1 BENCH2 N.
EXIT.

* -------------------
* Notes (4.6C):
* - '/' does rounding, 'DIV' not.
* - WHILE ... ENDWHILE is slower than DO n TIMES...ENDDO
* - WHILE ... ENDWHILE is also slower that LOOP AT <itab> into wa
* - using field symbols seems to be slow, so avoid them
*   (e.g. read table <ts> assigning <fs> -> read table p1 into wa)
*   (For small tables prefer append+table copy instead of field symbols)
*

* with integer arithmetic we get an overflow...
*
* Notes:
* - Data types:
*   - I (integer, 4 byte): -2147483648 bis 2147483647
*   - P (packed): -2147483648 bis 2147483647
*   - F (floating point): 2,225073E-308 bis 1,769313E+308
*
*
*
*---------------------------------------------------------------------*
*       FORM BENCH01                                                  *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*  -->  VALUE(LOOPS)                                                  *
*  -->  VALUE(N)                                                      *
*  -->  X                                                             *
*---------------------------------------------------------------------*
FORM BENCH01 USING VALUE(LOOPS) TYPE I
                   VALUE(N) TYPE I
             CHANGING X TYPE I.
  CONSTANTS: PART1 TYPE I VALUE 1000. " n must be divisible by part1!
  DATA: SUM1 TYPE I VALUE 0,
          F1 TYPE F VALUE '0.0'.

  DATA: N_DIV_PART1 TYPE I VALUE 0,
                 J TYPE I VALUE 0.
  N_DIV_PART1 = N DIV PART1.

*  CATCH SYSTEM-EXCEPTIONS ARITHMETIC_ERRORS = 4.
  F1 = ( ( N DIV 2 ) * ( N + 1 ) ).
  SUM1 = F1 MOD 65536. " MOD 2147483648.
* MOD 2147483648 is much more expensive than MOD 65536. Why?

  DO LOOPS TIMES.
* mod in loop is expensive, so try a workaround for n=1000000...
*      DO N TIMES.
*        X = ( X + SY-INDEX ) MOD 65536. " MOD 2147483648
*      ENDDO.
    DO N_DIV_PART1 TIMES.
      J = N_DIV_PART1 * ( SY-INDEX - 1 ).
      DO N_DIV_PART1 TIMES.
        X = ( X + J + SY-INDEX ).
      ENDDO.
      X = X MOD 65536.
    ENDDO.
    IF SY-INDEX < LOOPS.
      X = X - SUM1.
      IF X <> 0.
        X = X + 1. "force error for many wrong computations
        EXIT. "return.
      ENDIF.
    ENDIF.
  ENDDO.
*  ENDCATCH.
*  IF SY-SUBRC = 4.
*    WRITE: / 'Overflow! x=',X, ', sy-index=', SY-INDEX.
*  ENDIF.

  X = X MOD 65536.
ENDFORM.


*---------------------------------------------------------------------*
*       FORM BENCH02                                                  *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*  -->  VALUE(LOOPS)                                                  *
*  -->  VALUE(N)                                                      *
*  -->  XP                                                            *
*---------------------------------------------------------------------*
FORM BENCH02 USING VALUE(LOOPS) TYPE I
                   VALUE(N) TYPE I
             CHANGING XP TYPE I.
  DATA:    X TYPE F VALUE '0.0',
        SUM1 TYPE F VALUE '0.0'.
  SUM1 = ( N / 2 ) * ( N + 1 ).

  DO LOOPS TIMES.
    DO N TIMES.
      X = X + SY-INDEX.
    ENDDO.
    IF SY-INDEX < LOOPS.
      X = X - SUM1.
      IF X <> 0.
        X = X + 1. "force error for many wrong computations
        EXIT. "return.
      ENDIF.
    ENDIF.
  ENDDO.
  XP = X MOD 65536.
ENDFORM.



*---------------------------------------------------------------------*
*       FORM BENCH02_SLOW_WHILE                                       *
*---------------------------------------------------------------------*
*      (unused)                                                       *
*---------------------------------------------------------------------*
*  -->  VALUE(LOOPS)                                                  *
*  -->  VALUE(N)                                                      *
*  -->  XP                                                            *
*---------------------------------------------------------------------*
FORM BENCH02_SLOW_WHILE USING VALUE(LOOPS) TYPE I
                   VALUE(N) TYPE I
             CHANGING XP TYPE I.
  DATA X TYPE F VALUE '0.0'.
  DATA SUM1 TYPE F VALUE '0.0'.
  DATA I TYPE I VALUE 0.
  SUM1 = ( N / 2 ) * ( N + 1 ).

  WHILE LOOPS > 0.
    LOOPS = LOOPS - 1.
    I = N.
    WHILE I > 0.
      X = X + I.
      I = I - 1.
    ENDWHILE.
    IF LOOPS > 0.
      X = X - SUM1.
      IF X <> 0.
        X = X + 1. "force error for many wrong computations
        EXIT. "return.
      ENDIF.
    ENDIF.
  ENDWHILE.
  XP = X MOD 65536. "TRUNC?
ENDFORM.



*---------------------------------------------------------------------*
*       FORM BENCH03                                                  *
*---------------------------------------------------------------------*
* Sieve of Eratosthenes
* More than 1 loops only possible for n=1000000
* n=500000  : x=41538
* n=1000000 : x=78498
*---------------------------------------------------------------------*
*  -->  VALUE(LOOPS)                                                  *
*  -->  VALUE(N)                                                      *
*  -->  X                                                             *
*---------------------------------------------------------------------*
FORM BENCH03 USING VALUE(LOOPS) TYPE I
                   VALUE(N) TYPE I
             CHANGING X TYPE I.
  DATA:       I TYPE I VALUE 0,
        I_MUL_I TYPE I VALUE 0,
              J TYPE I VALUE 0,
           BIT1 TYPE I VALUE 0.

  X = 0. "number of primes below n
  N = N DIV 2. " only up to 500.000

*  N_DIV_8 = ( N DIV 8 ) + 1.
  CONSTANTS: MAX1 TYPE I VALUE 62501.
  DATA: SIEVE1(MAX1) TYPE X. "only constant here, only up to 65535

  SET BIT N OF SIEVE1 TO 0. " check if we've enough bits...
*  CHECK SY-SUBRC = 0.
  IF SY-SUBRC <> 0.
    WRITE: / 'Error: Not enough bits in array!'.
    EXIT.
  ENDIF.

  SET BIT 1 OF SIEVE1 TO 0. "we're starting with i=1
  DO LOOPS TIMES.
*   initialize sieve
    I = 2.
    WHILE I <= N.
      SET BIT I OF SIEVE1 TO 1.
      I = I + 1.
    ENDWHILE.

*   compute primes
    I = 2.
    I_MUL_I = I * I.
    WHILE I_MUL_I <= N.
      GET BIT I OF SIEVE1 INTO BIT1.
      IF BIT1 > 0.
        J = I_MUL_I.
        WHILE J <= N.
          SET BIT J OF SIEVE1 TO 0.
          J = J + I.
        ENDWHILE.
      ENDIF.
      I = I + 1.
      I_MUL_I = I * I.
    ENDWHILE.

*   count primes (starting from i=1)
    I = 1.
    WHILE I <= N.
      GET BIT I OF SIEVE1 INTO BIT1.
      IF BIT1 > 0.
        X = X + 1.
      ENDIF.
      I = I + 1.
    ENDWHILE.

*   check prime count
    IF SY-INDEX < LOOPS. "some more loops left?
      X = X - 41538. "yes, set x back to 0
      IF X <> 0. "now x must be 0 again
        X = X + 1. "force error for many wrong computations
        EXIT.  "Error
      ENDIF.
    ENDIF.
  ENDDO.
ENDFORM.


*---------------------------------------------------------------------*
*       FORM BENCH04                                                  *
*---------------------------------------------------------------------*
* bench04 (Integer 32 bit)
* nth random number number
* Random number generator taken from
* Raj Jain: The Art of Computer Systems Performance Analysis,
* John Wiley & Sons, 1991, page 442-444.
* It needs longs with at least 32 bit.
* Starting with x0=1, x10000 should be 1043618065, x1000000 = 1227283347
*---------------------------------------------------------------------*
*  -->  VALUE(LOOPS)                                                  *
*  -->  VALUE(N)                                                      *
*  -->  X                                                             *
*---------------------------------------------------------------------*
FORM BENCH04 USING VALUE(LOOPS) TYPE I
                   VALUE(N) TYPE I
             CHANGING X TYPE I.
  CONSTANTS: M TYPE I VALUE 2147483647, "modulus, do not change!
             A TYPE I VALUE 16807, "multiplier
             Q TYPE I VALUE 127773, "m div a
             R TYPE I VALUE 2836. "m mod a

  DATA:
       X_DIV_Q TYPE I,
       X_MOD_Q TYPE I,
          SUM1 TYPE I VALUE 1227283347.

  X = 1. "last random value
  DO LOOPS TIMES.
    DO N TIMES.
      X_DIV_Q = X DIV Q.
      X_MOD_Q = X - Q * X_DIV_Q.
      X = A * X_MOD_Q - R * X_DIV_Q.
      IF X <= 0.
        X = X + M. "x is new random number
      ENDIF.
    ENDDO.
    IF SY-INDEX < LOOPS.
      X = X - SUM1.
      IF X <> 0.
        X = X + 1. "force error for many wrong computations
        EXIT. "return.
      ENDIF.
      X = X + 1. " restart with 1
    ENDIF.
  ENDDO.
ENDFORM.




*---------------------------------------------------------------------*
*       FORM BENCH05                                                  *
*---------------------------------------------------------------------*
* bench05 (Integer 32 bit)
* n over n/2 mod 65536 (Pascal's triangle)
* (we just need to store the last 2 lines of computation)
*---------------------------------------------------------------------*
*  -->  VALUE(LOOPS)                                                  *
*  -->  VALUE(N)                                                      *
*  -->  X                                                             *
*---------------------------------------------------------------------*
FORM BENCH05 USING VALUE(LOOPS) TYPE I
                   VALUE(N) TYPE I
             CHANGING X TYPE I.

  DATA:   K TYPE I,
          I TYPE I,
          J TYPE I,
    I_MOD_2 TYPE I,
       DIV2 TYPE I,
        WA1 TYPE I,
        WA2 TYPE I,
       PAS1 TYPE STANDARD TABLE OF I INITIAL SIZE 100,
       PAS2 TYPE STANDARD TABLE OF I INITIAL SIZE 100.

  X = 0.
  N = N DIV 500.
  K = N DIV 2.

  J = N - K.
  IF J < K. "keep k minimal with  n over k  =  n over n-k
    K = J.
  ENDIF.

  DO LOOPS TIMES.
    REFRESH PAS1.
    APPEND 1 TO PAS1.
    I = 3.
    WHILE I <= N.
      PAS2 = PAS1.
      REFRESH PAS1.
      APPEND 1 TO PAS1. "set first column to 1
      APPEND I TO PAS1. "second column is i

      WA1 = I - 1. "last second column
      LOOP AT PAS2 INTO WA2.
        CHECK SY-TABIX >= 3. "starting with index 3
        WA1 = ( WA1 + WA2 ) MOD 65536. "sum: use mod to avoid overflow
        APPEND WA1 TO PAS1.
        WA1 = WA2. "save element for next round
      ENDLOOP.

      DIV2 = ( I - 1 ) DIV 2.

      I_MOD_2 = I MOD 2.
      IF DIV2 < K AND I_MOD_2 = 0. "new element
        WA2 = 2 * WA1. "wa1 contains already pas2[min1+1]
        APPEND WA2 TO PAS1.
      ENDIF.
      I = I + 1.
    ENDWHILE.

    J = K + 1. "get pas1[n mod 2][k]
    READ TABLE PAS1 INDEX J INTO WA1.
    X = ( X + WA1 ) MOD 65536.
    IF SY-INDEX < LOOPS.
      X = X - 27200.
      IF X <> 0.
        X = X + 1. "force error for many wrong computations
        EXIT. "return.
      ENDIF.
    ENDIF.

  ENDDO.

* debug output
*  IF 1 = 0.
*    WRITE: / 'DEBUG: '.
*    LOOP AT PAS1 INTO WA1.
*      WRITE: WA1, ' '.
*    ENDLOOP.
*    WRITE: /.
*  ENDIF.

  FREE PAS1.
  FREE PAS2.
ENDFORM.



*
* bench05 (Integer 32 bit) UNUSED
* n over n/2 mod 65536 (Pascal's triangle)
* (we just need to store the last 2 lines of computation)
*
FORM BENCH05_OK3 USING VALUE(LOOPS) TYPE I
                   VALUE(N_P) TYPE I
             CHANGING X TYPE I.

  DATA:   N TYPE I,
          K TYPE I,
          I TYPE I,
          J TYPE I,
    I_MOD_2 TYPE I,
       MIN1 TYPE I,
       SUM1 TYPE I,
        WA1 TYPE I,
        WA2 TYPE I,
       PAS1 TYPE STANDARD TABLE OF I INITIAL SIZE 100,
       PAS2 TYPE STANDARD TABLE OF I INITIAL SIZE 100.

  X = 0.
  N = N_P DIV 500.
  K = N DIV 2.

  J = N - K.
  IF J  < K. "keep k minimal with  n over k  =  n over
    K = J.
  ENDIF.

  DO LOOPS TIMES.
    REFRESH PAS1.
    APPEND 1 TO PAS1.
    I = 2.
    WHILE I <= N.
      PAS2 = PAS1.
      REFRESH PAS1.
      APPEND 1 TO PAS1. "set first column to 1
      APPEND I TO PAS1. "second column is i

      MIN1 = ( I - 1 ) DIV 2.
      IF K < MIN1.
        MIN1 = K.
      ENDIF.

      J = 2.
      READ TABLE PAS2 INDEX J INTO WA2.
      WHILE J <= MIN1. "up to min(i, k)
        WA1 = WA2.
        J = J + 1.
        READ TABLE PAS2 INDEX J INTO WA2.
        SUM1 = ( WA1 + WA2 ) MOD 65536. "use mod to avoid overflow
        APPEND SUM1 TO PAS1.
      ENDWHILE.
      I_MOD_2 = I MOD 2.
      IF MIN1 < K AND I_MOD_2 = 0. "new element
        J = MIN1 + 1.
        READ TABLE PAS2 INDEX J INTO WA1.
        SUM1 = 2 * WA1.
        APPEND SUM1 TO PAS1.
      ENDIF.
      I = I + 1.
    ENDWHILE.

    J = K + 1. "get pas1[n mod 2][k]
    READ TABLE PAS1 INDEX J INTO WA1.
    X = ( X + WA1 ) MOD 65536.
    IF SY-INDEX < LOOPS.
      X = X - 27200.
      IF X <> 0.
        X = X + 1. "force error for many wrong computations
        EXIT. "return.
      ENDIF.
    ENDIF.

  ENDDO.

* debug output
*  DATA: WA1 TYPE I.
*  WRITE: / 'DEBUG: '.
*  LOOP AT <PS1> INTO WA1.
*    WRITE: WA1, ' '.
*  ENDLOOP.
*  WRITE: /.

  FREE PAS1.
  FREE PAS2.
ENDFORM.



*
* bench05 (Integer 32 bit) UNUSED
* n over n/2 mod 65536 (Pascal's triangle)
* (we just need to store the last 2 lines of computation)
*
FORM BENCH05_OK2 USING VALUE(LOOPS) TYPE I
                   VALUE(N_P) TYPE I
             CHANGING X TYPE I.

  DATA:   N TYPE I,
          K TYPE I,
          I TYPE I,
          J TYPE I,
    I_MOD_2 TYPE I,
       MIN1 TYPE I,
       SUM1 TYPE I,
       WA TYPE I,
       PAS1 TYPE STANDARD TABLE OF I INITIAL SIZE 100,
       PAS2 TYPE STANDARD TABLE OF I INITIAL SIZE 100.

  X = 0.
  N = N_P DIV 500.
  K = N DIV 2.

  J = N - K.
  IF J  < K. "keep k minimal with  n over k  =  n over
    K = J.
  ENDIF.

  DO LOOPS TIMES.
    REFRESH PAS1.
    APPEND 1 TO PAS1.
    I = 2.
    WHILE I <= N.
      PAS2 = PAS1.
      REFRESH PAS1.
      APPEND 1 TO PAS1. "set first column to 1
      APPEND I TO PAS1. "second column is i

      MIN1 = ( I - 1 ) DIV 2.
      IF K < MIN1.
        MIN1 = K.
      ENDIF.

      J = 2.
      WHILE J <= MIN1. "up to min(i, k)
        READ TABLE PAS2 INDEX J INTO SUM1.
        J = J + 1.
        READ TABLE PAS2 INDEX J INTO WA.
        SUM1 = ( SUM1 + WA ) MOD 65536. "use mod to avoid overflow
        APPEND SUM1 TO PAS1.
      ENDWHILE.
      I_MOD_2 = I MOD 2.
      IF MIN1 < K AND I_MOD_2 = 0. "new element
        J = MIN1 + 1.
        READ TABLE PAS2 INDEX J INTO WA.
        SUM1 = 2 * WA.
        APPEND SUM1 TO PAS1.
      ENDIF.
      I = I + 1.
    ENDWHILE.

    J = K + 1. "get pas1[n mod 2][k]
    READ TABLE PAS1 INDEX J INTO WA.
    X = ( X + WA ) MOD 65536.
    IF SY-INDEX < LOOPS.
      X = X - 27200.
      IF X <> 0.
        X = X + 1. "force error for many wrong computations
        EXIT. "return.
      ENDIF.
    ENDIF.

  ENDDO.

* debug output
*  DATA: WA TYPE I.
*  WRITE: / 'DEBUG: '.
*  LOOP AT <PS1> INTO WA.
*    WRITE: WA, ' '.
*  ENDLOOP.
*  WRITE: /.

  FREE PAS1.
  FREE PAS2.
ENDFORM.





*---------------------------------------------------------------------*
*       FORM run_bench                                                *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*  -->  VALUE(BENCH)                                                  *
*  -->  VALUE(LOOPS)                                                  *
*  -->  VALUE(N)                                                 *
*  -->  X                                                             *
*---------------------------------------------------------------------*
FORM RUN_BENCH USING VALUE(BENCH) TYPE I
                     VALUE(LOOPS) TYPE I
                     VALUE(N) TYPE I
               CHANGING X TYPE I.
  DATA CHECK1 TYPE I VALUE 0.
  X = 0.
* or use: PERFORM bench OF bench01 bench02 bench03 bench04 bench05.
  CASE BENCH.
    WHEN 1.
      PERFORM BENCH01 USING LOOPS N CHANGING X.
      CHECK1 = 10528.

    WHEN 2.
      PERFORM BENCH02 USING LOOPS N CHANGING X.
      CHECK1 = 10528.

    WHEN 3.
      PERFORM BENCH03 USING LOOPS N CHANGING X.
      CHECK1 = 41538.

    WHEN 4.
      PERFORM BENCH04 USING LOOPS N CHANGING X.
      CHECK1 = 1227283347.

    WHEN 5.
      PERFORM BENCH05 USING LOOPS N CHANGING X.
      CHECK1 = 27200.

    WHEN OTHERS.
      WRITE: / 'Error: Unknown benchmark: ', BENCH.
      CHECK1 = X + 1.

  ENDCASE.

  IF X <> CHECK1.
    WRITE: / 'Error(bench', BENCH, '): x=', X.
    X = -1.
*    STOP.
  ENDIF.
ENDFORM.


*---------------------------------------------------------------------*
*       FORM get_ms                                                   *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*  -->  MS                                                            *
*---------------------------------------------------------------------*
FORM GET_MS CHANGING MS TYPE I.
  DATA: T1 TYPE I.
  GET RUN TIME FIELD T1.
  MS = T1 / 1000.
ENDFORM.


*FROM GET_MS_TEST1 CHANGING MS TYPE I.
*DATA: P1 TYPE timestampl.
*  GET TIME STAMP FIELD P1.
* *TODO: MS = P1 / 1000.
*ENDFORM.


*---------------------------------------------------------------------*
*       FORM CHECKBITS_INT1                                           *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*  -->  BITS                                                          *
*---------------------------------------------------------------------*
FORM CHECKBITS_INT1 CHANGING BITS TYPE I.
  DATA: NUM TYPE I VALUE 1,
        TMP_NUM TYPE I,
        LAST_NUM TYPE I VALUE 0.

  BITS = 0.
  CATCH SYSTEM-EXCEPTIONS ARITHMETIC_ERRORS = 4.
    WHILE BITS < 101.
      BITS = BITS + 1. "increment before overflow
      LAST_NUM = NUM.
      NUM = NUM * 2.
      NUM = NUM + 1.
      TMP_NUM = ( NUM - 1 ) DIV 2.
      IF TMP_NUM <> LAST_NUM.
        EXIT.
      ENDIF.
    ENDWHILE.

  ENDCATCH.
  IF SY-SUBRC = 4.
*    WRITE: / 'DEBUG: Overflow! bits=',bits.
  ENDIF.
ENDFORM.


*---------------------------------------------------------------------*
*       FORM CHECKBITS_DOUBLE1                                        *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*  -->  BITS                                                          *
*---------------------------------------------------------------------*
FORM CHECKBITS_DOUBLE1 CHANGING BITS TYPE I.
  DATA: NUM TYPE F VALUE '1.0',
        TMP_NUM TYPE F,
        LAST_NUM TYPE F VALUE '0.0'.

  BITS = 0.
  WHILE BITS < 101.
    BITS = BITS + 1.
    LAST_NUM = NUM.
    NUM = NUM * 2.
    NUM = NUM + 1.
    TMP_NUM = ( NUM - 1 ) / 2.
    IF TMP_NUM <> LAST_NUM.
      EXIT.
    ENDIF.
  ENDWHILE.
ENDFORM.


* Get and write the kernel information (optional)
* (taken from RSUVM002...)
FORM GET_KINFO CHANGING KERNEL_RELEASE TYPE C
                        KERNEL_PATCH TYPE C.
  DATA: BEGIN OF KERNEL_VERSION_INFO OCCURS 0,
          KEY(21)  TYPE C,
          DATA(59) TYPE C,
        END OF KERNEL_VERSION_INFO.
*       KERNEL_RELEASE(10)     TYPE C,
*       KERNEL_PATCH_LEVEL(10) TYPE C.

*  CLEAR KERNEL_RELEASE.
*  CLEAR KERNEL_PATCH_LEVEL.

  CALL 'SAPCORE' ID 'ID'    FIELD 'VERSION'
                 ID 'TABLE' FIELD KERNEL_VERSION_INFO-*SYS*.
  IF SY-SUBRC = 0.
    READ TABLE KERNEL_VERSION_INFO INDEX 12.
    IF SY-SUBRC = 0.
      IF KERNEL_VERSION_INFO-KEY = 'kernel release'.
        KERNEL_RELEASE = KERNEL_VERSION_INFO-DATA.
      ENDIF.
    ENDIF.

    READ TABLE KERNEL_VERSION_INFO INDEX 15.
    IF SY-SUBRC = 0.
      IF KERNEL_VERSION_INFO-KEY = 'kernel patch level'.
        KERNEL_PATCH = KERNEL_VERSION_INFO-DATA.
      ENDIF.
    ENDIF.
  ENDIF.

*  WRITE: / 'Kernel rel/patch:', KERNEL_RELEASE.
*  WRITE: / 'Kernel patch level:', KERNEL_PATCH_LEVEL.
ENDFORM.  "get_kinfo


*---------------------------------------------------------------------*
*       FORM main                                                     *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
FORM MAIN USING VALUE(BENCH1) TYPE I
                VALUE(BENCH2) TYPE I
                VALUE(N) TYPE I.

  CONSTANTS: MIN_MS TYPE I VALUE 10000.

  DATA: BENCH TYPE I VALUE 0,
        LOOPS TYPE I VALUE 1,
            X TYPE I VALUE 0,
      T_START TYPE I VALUE 0,
           T1 TYPE I VALUE 0,
           T2 TYPE I VALUE 0,
      NS1(10) TYPE C,
      NS2(10) TYPE C,
      NS3(10) TYPE C,
         LEN1 TYPE I,
         LEN2 TYPE I,
         LEN3 TYPE I,
         B_INT1 TYPE I,
         B_DOUBLE1 TYPE I,
         KERNEL_REL(10) TYPE C,
         KERNEL_PATCH(10) TYPE C,
      BENCH_RES1 TYPE STANDARD TABLE OF I.

*  SET RUN TIME CLOCK RESOLUTION HIGH. "is default
  PERFORM GET_MS CHANGING T_START.

  PERFORM CHECKBITS_INT1 CHANGING B_INT1.
  WRITE B_INT1 TO NS1 LEFT-JUSTIFIED. LEN1 = STRLEN( NS1 ).
  PERFORM CHECKBITS_DOUBLE1 CHANGING B_DOUBLE1.
  WRITE B_DOUBLE1 TO NS2 LEFT-JUSTIFIED. LEN2 = STRLEN( NS2 ).

  WRITE: / 'BM Bench v0.5 (ABAP) -- (int:', AT (LEN1) NS1,
    ' double:', AT (LEN2) NS2, ')'.

  WRITE SY-SYSID TO NS1 LEFT-JUSTIFIED. LEN1 = STRLEN( NS1 ).
  WRITE SY-SAPRL TO NS2 LEFT-JUSTIFIED. LEN2 = STRLEN( NS2 ).
  WRITE: / 'sysid=', AT (LEN1) NS1, ', saprel=', AT (LEN2) NS2,
   ','.

  WRITE SY-HOST TO NS1 LEFT-JUSTIFIED. LEN1 = STRLEN( NS1 ).
  WRITE SY-OPSYS TO NS2 LEFT-JUSTIFIED. LEN2 = STRLEN( NS2 ).
  WRITE SY-DBSYS TO NS3 LEFT-JUSTIFIED. LEN3 = STRLEN( NS3 ).
  WRITE: 'host=', AT (LEN1) NS1, ', opsys=', AT (LEN2) NS2,
    ', dbsys=', AT (LEN3) NS3.

  PERFORM GET_KINFO CHANGING KERNEL_REL KERNEL_PATCH.
  WRITE KERNEL_REL TO NS1 LEFT-JUSTIFIED. LEN1 = STRLEN( NS1 ).
  WRITE KERNEL_PATCH TO NS2 LEFT-JUSTIFIED. LEN2 = STRLEN( NS2 ).
  WRITE: ', kernel_rel/patch=', AT (LEN1) NS1, '/', AT (LEN2) NS2.

  WRITE: / '(c) Marco Vieth, 2002'.
  WRITE: / 'Date:', SY-DATUM, SY-UZEIT.


  BENCH2 = BENCH2 + 1 - BENCH1.
  DO BENCH2 TIMES.
    BENCH = BENCH1 + SY-INDEX - 1.

    T2 = 0.
    X = 0.
* We want at least 1001 ms calibration time
    WHILE T2 < 1001 AND X <> -1.
      WRITE BENCH TO NS1 LEFT-JUSTIFIED. LEN1 = STRLEN( NS1 ).
      WRITE LOOPS TO NS2 LEFT-JUSTIFIED. LEN2 = STRLEN( NS2 ).
      WRITE N TO NS3 LEFT-JUSTIFIED. LEN3 = STRLEN( NS3 ).
      WRITE: / 'Calibrating benchmark', AT (LEN1) NS1, 'with loops=',
        AT (LEN2) NS2, ', n=', AT (LEN3) NS3.
      PERFORM GET_MS CHANGING T1.
      PERFORM RUN_BENCH USING BENCH LOOPS N CHANGING X.

      PERFORM GET_MS CHANGING T2.
      T2 = T2 - T1.
      WRITE X TO NS1 LEFT-JUSTIFIED. LEN1 = STRLEN( NS1 ).
      WRITE T2 TO NS2 LEFT-JUSTIFIED. LEN2 = STRLEN( NS2 ).
      WRITE: / 'x=', AT (LEN1) NS1, '(time:', AT (LEN2) NS2, 'ms)'.
      LOOPS = LOOPS * 2.
    ENDWHILE.
    IF X <> -1.
      LOOPS = LOOPS DIV 2.
      LOOPS = LOOPS * ( MIN_MS DIV T2 ) + 1. "ceil?
      WRITE LOOPS TO NS1  LEFT-JUSTIFIED. LEN1 = STRLEN( NS1 ).
      WRITE MIN_MS TO NS2  LEFT-JUSTIFIED. LEN2 = STRLEN( NS2 ).
      WRITE: / 'Calibration done. Starting measurement with',
        AT (LEN1) NS1, 'loops to get >=', AT (LEN2) NS2, 'ms'.

* Measurement
      PERFORM GET_MS CHANGING T1.
      PERFORM RUN_BENCH USING BENCH LOOPS N CHANGING X.
      PERFORM GET_MS CHANGING T2.
      T2 = T2 - T1.
      WRITE X TO NS1 LEFT-JUSTIFIED. LEN1 = STRLEN( NS1 ).
      WRITE T2 TO NS2 LEFT-JUSTIFIED. LEN2 = STRLEN( NS2 ).
      WRITE: / 'x=', AT (LEN1) NS1, '(time:', AT (LEN2) NS2, 'ms)'.

      DATA: T_ESTI10 TYPE I.
      T_ESTI10 = ( T2 * 10 / LOOPS ).
      WRITE LOOPS TO NS1 LEFT-JUSTIFIED. LEN1 = STRLEN( NS1 ).
      WRITE T2 TO NS2 LEFT-JUSTIFIED. LEN2 = STRLEN( NS2 ).
      WRITE T_ESTI10 TO NS3 LEFT-JUSTIFIED. LEN3 = STRLEN( NS3 ).
      WRITE: / 'Elapsed time for', AT (LEN1) NS1, 'loops:',
        AT (LEN2) NS2, 'ms; estimation for 10 loops:',
        AT (LEN3) NS3, 'ms'.
      APPEND T_ESTI10 TO BENCH_RES1.
    ELSE.
      APPEND -1 TO BENCH_RES1.
    ENDIF.
  ENDDO.

  WRITE: / 'Times for all benchmarks (10 loops, ms):'.
  WRITE: / 'BM Results (ABAP)      : '.
  LOOP AT BENCH_RES1 INTO T_ESTI10.
*    WRITE T_ESTI10 TO NS2 LEFT-JUSTIFIED. LEN2 = STRLEN( NS2 ).
*    WRITE: AT (LEN2) NS2.
    WRITE: T_ESTI10.
  ENDLOOP.

  PERFORM GET_MS CHANGING T2.
  T2 = T2 - T_START.
  WRITE T2 TO NS1 LEFT-JUSTIFIED NO-GROUPING. LEN1 = STRLEN( NS1 ).
  WRITE: / 'Total elapsed time:', AT (LEN1) NS1, 'ms'.
ENDFORM.
*
* end
*