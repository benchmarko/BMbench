*&---------------------------------------------------------------------*
*& Report  ZMVTEST06                                                   *
*&                                                                     *
*&---------------------------------------------------------------------*
*&
*& 21.08.2002       extended system info
*& 04.09.2002 0.04  calibration corrected
*& 11.02.2003 0.041 output format changed
*& 19.02.2003 0.05  kernel info
*& 25.02.2003       benchmark bench00 uses type I, bench01 uses P
*& 23.10.2006 0.06  based on version 0.05
*&
*&---------------------------------------------------------------------*

*
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
*   - P (packed): depends on number of digits
*   - F (floating point): 2,225073E-308 bis 1,769313E+308
*
*
*


REPORT ZMVTEST06 NO STANDARD PAGE HEADING LINE-SIZE 200.

CONSTANTS:
  PRG_VERSION(5) TYPE C VALUE '0.06'.

PARAMETERS: BENCH1(1) TYPE C DEFAULT '0',
            BENCH2(1) TYPE C DEFAULT '5',
            N(9) TYPE C DEFAULT '1000000'.

PERFORM MAIN USING BENCH1 BENCH2 N.
EXIT.


*---------------------------------------------------------------------*
*       FORM BENCH00                                                  *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*  -->  VALUE(LOOPS)                                                  *
*  -->  VALUE(N)                                                      *
*  -->  X                                                             *
*---------------------------------------------------------------------*
FORM BENCH00 USING VALUE(LOOPS) TYPE I
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
ENDFORM.                                                    "BENCH01


*&--------------------------------------------------------------------*
*&      Form  BENCH01
*&--------------------------------------------------------------------*
*       we are using the (slow) packed type 'P' with 7*2-1 digits here
*---------------------------------------------------------------------*
*      -->VALUE(LOOPStext
*      -->VALUE(N)   text
*      -->X          text
*---------------------------------------------------------------------*
FORM BENCH01 USING VALUE(LOOPS) TYPE I
                   VALUE(N) TYPE I
             CHANGING X_P TYPE I.
  DATA: SUM1(7) TYPE P VALUE 0,
          X(7) TYPE P VALUE 0.

*  CATCH SYSTEM-EXCEPTIONS ARITHMETIC_ERRORS = 4.
  SUM1 = ( ( N DIV 2 ) * ( N + 1 ) ).

  DO LOOPS TIMES.
    DO N TIMES.
      X = X + SY-INDEX.
    ENDDO.
    IF SY-INDEX < LOOPS.
      X = X - SUM1.
      IF X <> 0.
        X = X + 1. "force error for many wrong computations
        X_P = X MOD 65536.
        EXIT. "return.
      ENDIF.
    ENDIF.
  ENDDO.
*  ENDCATCH.
*  IF SY-SUBRC = 4.
*    WRITE: / 'Overflow! x=',X, ', sy-index=', SY-INDEX.
*  ENDIF.
  X_P = X MOD 65536.
ENDFORM.                                                    "BENCH01


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
ENDFORM.                                                    "BENCH02



*---------------------------------------------------------------------*
*       FORM BENCH02_SLOW_WHILE                                       *
*---------------------------------------------------------------------*
*      This is the same as BENCH02 but with a while loop which seems
*      to be slower than a DO loop.
*---------------------------------------------------------------------*
*  -->  VALUE(LOOPS)                                                  *
*  -->  VALUE(N)                                                      *
*  -->  XP                                                            *
*---------------------------------------------------------------------*
*FORM BENCH02_SLOW_WHILE USING VALUE(LOOPS) TYPE I
*                   VALUE(N) TYPE I
*             CHANGING XP TYPE I.
*  DATA X TYPE F VALUE '0.0'.
*  DATA SUM1 TYPE F VALUE '0.0'.
*  DATA I TYPE I VALUE 0.
*  SUM1 = ( N / 2 ) * ( N + 1 ).
*
*  WHILE LOOPS > 0.
*    LOOPS = LOOPS - 1.
*    I = N.
*    WHILE I > 0.
*      X = X + I.
*      I = I - 1.
*    ENDWHILE.
*    IF LOOPS > 0.
*      X = X - SUM1.
*      IF X <> 0.
*        X = X + 1. "force error for many wrong computations
*        EXIT. "return.
*      ENDIF.
*    ENDIF.
*  ENDWHILE.
*  XP = X MOD 65536. "TRUNC?
*ENDFORM.                    "BENCH02_SLOW_WHILE



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

  CONSTANTS: MAX1 TYPE I VALUE 62501.
  DATA: SIEVE1(MAX1) TYPE X. "only constant here, only up to 65535

  SET BIT N OF SIEVE1 TO 0. " check if we've enough bits...

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
ENDFORM.                                                    "BENCH03


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
ENDFORM.                                                    "BENCH04




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
ENDFORM.                                                    "BENCH05


*&--------------------------------------------------------------------*
*&      Form  BENCH05_OK3
*&--------------------------------------------------------------------*
*       This version of bench05 is unused.
*---------------------------------------------------------------------*
*      -->VALUE(LOOPS)
*      -->VALUE(N_P)
*      -->X
*---------------------------------------------------------------------*
*FORM BENCH05_OK3 USING VALUE(LOOPS) TYPE I
*                   VALUE(N_P) TYPE I
*             CHANGING X TYPE I.
*
*  DATA:   N TYPE I,
*          K TYPE I,
*          I TYPE I,
*          J TYPE I,
*    I_MOD_2 TYPE I,
*       MIN1 TYPE I,
*       SUM1 TYPE I,
*        WA1 TYPE I,
*        WA2 TYPE I,
*       PAS1 TYPE STANDARD TABLE OF I INITIAL SIZE 100,
*       PAS2 TYPE STANDARD TABLE OF I INITIAL SIZE 100.
*
*  X = 0.
*  N = N_P DIV 500.
*  K = N DIV 2.
*
*  J = N - K.
*  IF J  < K. "keep k minimal with  n over k  =  n over
*    K = J.
*  ENDIF.
*
*  DO LOOPS TIMES.
*    REFRESH PAS1.
*    APPEND 1 TO PAS1.
*    I = 2.
*    WHILE I <= N.
*      PAS2 = PAS1.
*      REFRESH PAS1.
*      APPEND 1 TO PAS1. "set first column to 1
*      APPEND I TO PAS1. "second column is i
*
*      MIN1 = ( I - 1 ) DIV 2.
*      IF K < MIN1.
*        MIN1 = K.
*      ENDIF.
*
*      J = 2.
*      READ TABLE PAS2 INDEX J INTO WA2.
*      WHILE J <= MIN1. "up to min(i, k)
*        WA1 = WA2.
*        J = J + 1.
*        READ TABLE PAS2 INDEX J INTO WA2.
*        SUM1 = ( WA1 + WA2 ) MOD 65536. "use mod to avoid overflow
*        APPEND SUM1 TO PAS1.
*      ENDWHILE.
*      I_MOD_2 = I MOD 2.
*      IF MIN1 < K AND I_MOD_2 = 0. "new element
*        J = MIN1 + 1.
*        READ TABLE PAS2 INDEX J INTO WA1.
*        SUM1 = 2 * WA1.
*        APPEND SUM1 TO PAS1.
*      ENDIF.
*      I = I + 1.
*    ENDWHILE.
*
*    J = K + 1. "get pas1[n mod 2][k]
*    READ TABLE PAS1 INDEX J INTO WA1.
*    X = ( X + WA1 ) MOD 65536.
*    IF SY-INDEX < LOOPS.
*      X = X - 27200.
*      IF X <> 0.
*        X = X + 1. "force error for many wrong computations
*        EXIT. "return.
*      ENDIF.
*    ENDIF.
*
*  ENDDO.
*
*  FREE PAS1.
*  FREE PAS2.
*ENDFORM.                    "BENCH05_OK3


*&--------------------------------------------------------------------*
*&      Form  BENCH05_OK2
*&--------------------------------------------------------------------*
*       This version of bench05 is unused.
*---------------------------------------------------------------------*
*      -->VALUE(LOOPS)
*      -->VALUE(N_P)
*      -->X
*---------------------------------------------------------------------*
*FORM BENCH05_OK2 USING VALUE(LOOPS) TYPE I
*                   VALUE(N_P) TYPE I
*             CHANGING X TYPE I.
*
*  DATA:   N TYPE I,
*          K TYPE I,
*          I TYPE I,
*          J TYPE I,
*    I_MOD_2 TYPE I,
*       MIN1 TYPE I,
*       SUM1 TYPE I,
*       WA TYPE I,
*       PAS1 TYPE STANDARD TABLE OF I INITIAL SIZE 100,
*       PAS2 TYPE STANDARD TABLE OF I INITIAL SIZE 100.
*
*  X = 0.
*  N = N_P DIV 500.
*  K = N DIV 2.
*
*  J = N - K.
*  IF J  < K. "keep k minimal with  n over k  =  n over
*    K = J.
*  ENDIF.
*
*  DO LOOPS TIMES.
*    REFRESH PAS1.
*    APPEND 1 TO PAS1.
*    I = 2.
*    WHILE I <= N.
*      PAS2 = PAS1.
*      REFRESH PAS1.
*      APPEND 1 TO PAS1. "set first column to 1
*      APPEND I TO PAS1. "second column is i
*
*      MIN1 = ( I - 1 ) DIV 2.
*      IF K < MIN1.
*        MIN1 = K.
*      ENDIF.
*
*      J = 2.
*      WHILE J <= MIN1. "up to min(i, k)
*        READ TABLE PAS2 INDEX J INTO SUM1.
*        J = J + 1.
*        READ TABLE PAS2 INDEX J INTO WA.
*        SUM1 = ( SUM1 + WA ) MOD 65536. "use mod to avoid overflow
*        APPEND SUM1 TO PAS1.
*      ENDWHILE.
*      I_MOD_2 = I MOD 2.
*      IF MIN1 < K AND I_MOD_2 = 0. "new element
*        J = MIN1 + 1.
*        READ TABLE PAS2 INDEX J INTO WA.
*        SUM1 = 2 * WA.
*        APPEND SUM1 TO PAS1.
*      ENDIF.
*      I = I + 1.
*    ENDWHILE.
*
*    J = K + 1. "get pas1[n mod 2][k]
*    READ TABLE PAS1 INDEX J INTO WA.
*    X = ( X + WA ) MOD 65536.
*    IF SY-INDEX < LOOPS.
*      X = X - 27200.
*      IF X <> 0.
*        X = X + 1. "force error for many wrong computations
*        EXIT. "return.
*      ENDIF.
*    ENDIF.
*
*  ENDDO.
*
** debug output
**  DATA: WA TYPE I.
**  WRITE: / 'DEBUG: '.
**  LOOP AT <PS1> INTO WA.
**    WRITE: WA, ' '.
**  ENDLOOP.
**  WRITE: /.
*
*  FREE PAS1.
*  FREE PAS2.
*ENDFORM.                    "BENCH05_OK2




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
    WHEN 0.
      PERFORM BENCH00 USING LOOPS N CHANGING X.
      CHECK1 = 10528.

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
      WRITE: / 'Error: Unknown benchmark:', (1) BENCH.
      CHECK1 = X + 1.

  ENDCASE.

  IF X <> CHECK1.
    WRITE: / 'Error(bench' NO-GAP, (1) BENCH NO-GAP,
      '): x=' NO-GAP, X USING EDIT MASK 'V__________'.
    X = -1.
*    STOP.
  ENDIF.
ENDFORM.                    "RUN_BENCH



*&--------------------------------------------------------------------*
*&      Form  GET_MS
*&--------------------------------------------------------------------*
*       text
*---------------------------------------------------------------------*
*      -->MS         text
*---------------------------------------------------------------------*
FORM GET_MS CHANGING MS TYPE I.
  DATA: T1 TYPE I.
  GET RUN TIME FIELD T1.
  MS = T1 / 1000.
ENDFORM.                    "GET_MS


*FROM GET_MS_TEST1 CHANGING MS TYPE I.
*DATA: P1 TYPE timestampl.
*  GET TIME STAMP FIELD P1.
* *TODO: MS = P1 / 1000.
"ENDFORM.



*&--------------------------------------------------------------------*
*&      Form  CHECKBITS_INT1
*&--------------------------------------------------------------------*
*       text
*---------------------------------------------------------------------*
*      -->BITS       text
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
*  IF SY-SUBRC = 4.
**    WRITE: / 'DEBUG: Overflow! bits=',bits.
*  ENDIF.
ENDFORM.                    "CHECKBITS_INT1



*&--------------------------------------------------------------------*
*&      Form  CHECKBITS_NUM1
*&--------------------------------------------------------------------*
*       check number type, currently unused
*---------------------------------------------------------------------*
*      -->BITS       text
*---------------------------------------------------------------------*
*FORM CHECKBITS_NUM1_UNUSED CHANGING BITS TYPE I.
*  DATA: NUM TYPE N VALUE 1,
*        TMP_NUM TYPE N,
*        LAST_NUM TYPE N VALUE 0.
*
*  BITS = 0.
*  CATCH SYSTEM-EXCEPTIONS ARITHMETIC_ERRORS = 4.
*    WHILE BITS < 101.
*      BITS = BITS + 1. "increment before overflow
*      LAST_NUM = NUM.
*      NUM = NUM * 2.
*      NUM = NUM + 1.
*      TMP_NUM = ( NUM - 1 ) DIV 2.
*      IF TMP_NUM <> LAST_NUM.
*        EXIT.
*      ENDIF.
*    ENDWHILE.
*
*  ENDCATCH.
**  IF SY-SUBRC = 4.
***    WRITE: / 'DEBUG: Overflow! bits=',bits.
**  ENDIF.
*ENDFORM.                    "CHECKBITS_NUM1


*&--------------------------------------------------------------------*
*&      Form  CHECKBITS_PACKED1
*&--------------------------------------------------------------------*
*       text
*---------------------------------------------------------------------*
*      -->BITS       text
*---------------------------------------------------------------------*
FORM CHECKBITS_PACKED1 CHANGING BITS TYPE I.
  DATA: NUM(7) TYPE P VALUE 1,
        TMP_NUM(7) TYPE P,
        LAST_NUM(7) TYPE P VALUE 0.

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
*  IF SY-SUBRC = 4.
**    WRITE: / 'DEBUG: Overflow! bits=',bits.
*  ENDIF.
ENDFORM.                    "CHECKBITS_PACKED1



*&--------------------------------------------------------------------*
*&      Form  CHECKBITS_DOUBLE1
*&--------------------------------------------------------------------*
*       text
*---------------------------------------------------------------------*
*      -->BITS       text
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
ENDFORM.                    "CHECKBITS_DOUBLE1


*&--------------------------------------------------------------------*
*&      Form  GET_KINFO
*&--------------------------------------------------------------------*
*       Get and write the kernel information (optional)
*       (see also RSUVM002)
*---------------------------------------------------------------------*
*      -->KERNEL_RELEtext
*      -->KERNEL_PATCtext
*---------------------------------------------------------------------*
FORM GET_KINFO CHANGING KERNEL_RELEASE TYPE C
                        KERNEL_PATCH TYPE C.

  TYPES:
    BEGIN OF KERNEL_VERSION_INFO,
      KEY(21)  TYPE C,
      DATA(59) TYPE C,
    END OF KERNEL_VERSION_INFO.
  DATA:
    T_KERNEL_VERSION_INFO TYPE STANDARD TABLE OF KERNEL_VERSION_INFO,
    WA_KERNEL_VERSION_INFO LIKE LINE OF T_KERNEL_VERSION_INFO.

  CALL 'SAPCORE' ID 'ID'    FIELD 'VERSION'
                 ID 'TABLE' FIELD T_KERNEL_VERSION_INFO.

  IF SY-SUBRC = 0.
    READ TABLE T_KERNEL_VERSION_INFO INDEX 12 INTO
      WA_KERNEL_VERSION_INFO.
    IF SY-SUBRC = 0 AND WA_KERNEL_VERSION_INFO-KEY = 'kernel release'.
      KERNEL_RELEASE = WA_KERNEL_VERSION_INFO-DATA.
    ENDIF.

    READ TABLE T_KERNEL_VERSION_INFO INDEX 15 INTO
      WA_KERNEL_VERSION_INFO.
    IF SY-SUBRC = 0 AND WA_KERNEL_VERSION_INFO-KEY =
        'kernel patch level'.
      KERNEL_PATCH = WA_KERNEL_VERSION_INFO-DATA.
    ENDIF.
  ENDIF.
ENDFORM.  "get_kinfo



*&---------------------------------------------------------------------*
*&      Form  FORMAT_I2STR
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->I1         text
*      -->STR        text
*----------------------------------------------------------------------*
FORM FORMAT_I2STR USING I1 TYPE I
               CHANGING STR TYPE C.

*  DATA:
*    NS1(11) TYPE C.

*  WRITE I1 TO NS1 USING EDIT MASK 'V__________'.
*  WRITE I1 TO STR USING EDIT MASK 'V__________'.
  WRITE I1 TO STR USING EDIT MASK '__________'.



*  WRITE I1 TO NS1 LEFT-JUSTIFIED.

ENDFORM.                    "FORMAT_I2STR


*&---------------------------------------------------------------------*
*&      Form  WRITE_I2STR
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->I1         text
*      -->STR        text
*----------------------------------------------------------------------*
FORM WRITE_I2STR USING I1 TYPE I
              CHANGING STR TYPE C.

  DATA:
    STR2(11) TYPE C.
*    LEN TYPE I.

  PERFORM FORMAT_I2STR USING I1 CHANGING STR2.
* LEN = STRLEN( STR ).
* WRITE: AT (LEN) STR NO-GAP.
  CONCATENATE STR STR2 INTO STR.

ENDFORM.                    "WRITE_I2STR



*&---------------------------------------------------------------------*
*&      Form  PRINT_SYSTEM_INFO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM PRINT_SYSTEM_INFO.
  DATA:
    NS1(10) TYPE C,
    NS2(10) TYPE C,
    NS3(10) TYPE C,
       LEN1 TYPE I,
       LEN2 TYPE I,
       LEN3 TYPE I,
    KERNEL_REL(10) TYPE C,
    KERNEL_PATCH(10) TYPE C.

  WRITE SY-SYSID TO NS1 LEFT-JUSTIFIED. LEN1 = STRLEN( NS1 ).
  WRITE SY-SAPRL TO NS2 LEFT-JUSTIFIED. LEN2 = STRLEN( NS2 ).
  WRITE: 'sysid=' NO-GAP, AT (LEN1) NS1 NO-GAP,
    ', saprel=' NO-GAP, AT (LEN2) NS2 NO-GAP, ','.

  DATA: SYSNO(2) TYPE N.
  CALL FUNCTION 'GET_SYSTEM_NUMBER'
    IMPORTING
      INSTANCENUMBER = SYSNO.

  WRITE SY-HOST TO NS1 LEFT-JUSTIFIED. LEN1 = STRLEN( NS1 ).
  WRITE SY-OPSYS TO NS2 LEFT-JUSTIFIED. LEN2 = STRLEN( NS2 ).
  WRITE SY-DBSYS TO NS3 LEFT-JUSTIFIED. LEN3 = STRLEN( NS3 ).
  WRITE: 'host=' NO-GAP, AT (LEN1) NS1 NO-GAP,
    ', sysno=' NO-GAP, SYSNO NO-GAP,
    ', opsys=' NO-GAP, AT (LEN2) NS2 NO-GAP,
    ', dbsys=' NO-GAP, AT (LEN3) NS3 NO-GAP.

  PERFORM GET_KINFO CHANGING KERNEL_REL KERNEL_PATCH.
  WRITE KERNEL_REL TO NS1 LEFT-JUSTIFIED. LEN1 = STRLEN( NS1 ).
  WRITE KERNEL_PATCH TO NS2 LEFT-JUSTIFIED. LEN2 = STRLEN( NS2 ).
  WRITE: ', kernel_rel=' NO-GAP, AT (LEN1) NS1 NO-GAP,
         ', kernel_patch=' NO-GAP, AT (LEN2) NS2.
ENDFORM.                    "PRINT_SYSTEM_INFO



*&--------------------------------------------------------------------*
*&      Form  MAIN
*&--------------------------------------------------------------------*
*       text
*---------------------------------------------------------------------*
*      -->VALUE(BENCH)
*      -->VALUE(BENCH)
*      -->VALUE(N)
*---------------------------------------------------------------------*
FORM MAIN USING VALUE(P_BENCH1) TYPE C
                VALUE(P_BENCH2) TYPE C
                VALUE(P_N) TYPE C.

  CONSTANTS:
    CALI_MS TYPE I VALUE 1001,
    DELTA_MS TYPE I VALUE 100,
    MAX_MS TYPE I VALUE 10000.

  DATA:
    T_START TYPE I,
    BENCH1 TYPE I,
    BENCH2 TYPE I,
    N TYPE I,
    BENCH_RES1 TYPE STANDARD TABLE OF F.

  BENCH1 = P_BENCH1.
  BENCH2 = P_BENCH2.
  N = P_N.


*  SET RUN TIME CLOCK RESOLUTION HIGH. "is default
  PERFORM GET_MS CHANGING T_START.

*  PERFORM CHECKBITS_INT1 CHANGING B_INT1.
*  WRITE B_INT1 TO NS1 LEFT-JUSTIFIED. LEN1 = STRLEN( NS1 ).
*  PERFORM CHECKBITS_PACKED1 CHANGING B_PACK1.
*  WRITE B_PACK1 TO NS2 LEFT-JUSTIFIED. LEN2 = STRLEN( NS2 ).
*  PERFORM CHECKBITS_DOUBLE1 CHANGING B_DOUBLE1.
*  WRITE B_DOUBLE1 TO NS3 LEFT-JUSTIFIED. LEN3 = STRLEN( NS3 ).

*  WRITE: /  'BM Bench v0.6 (ABAP) -- (int:' NO-GAP, AT (LEN1) NS1,
*    'packed:' NO-GAP, AT (LEN2) NS2, 'double:' NO-GAP,
*    AT (LEN3) NS3 NO-GAP, ')'.

  DATA:
    STR(150) TYPE C,
    STR_LEN TYPE I,
    B_INT1 TYPE I.

  STR = 'BM Bench v0.6 (ABAP) -- (int:'.
  PERFORM CHECKBITS_INT1 CHANGING B_INT1.
  PERFORM WRITE_I2STR USING B_INT1 CHANGING STR.

  CONCATENATE STR ' packed:' INTO STR.
  PERFORM CHECKBITS_PACKED1 CHANGING B_INT1.
  PERFORM WRITE_I2STR USING B_INT1 CHANGING STR.

  CONCATENATE STR ' double:' INTO STR.
  PERFORM CHECKBITS_DOUBLE1 CHANGING B_INT1.
  PERFORM WRITE_I2STR USING B_INT1 CHANGING STR.
  CONCATENATE STR ')' INTO STR.

  STR_LEN = STRLEN( STR ).
  WRITE: AT (STR_LEN) STR.

  PERFORM PRINT_SYSTEM_INFO.

  WRITE: / '(c) Marco Vieth, 2006'.
  WRITE: / 'Date:', SY-DATUM, SY-UZEIT.

  SET COUNTRY 'EN'. "English decimal number format

  BENCH2 = BENCH2 + 1 - BENCH1.

  DO BENCH2 TIMES.

    DATA:
      BENCH TYPE I,
      LOOPS TYPE I,
          X TYPE I,
         T1 TYPE I,
         T2 TYPE I.

    BENCH = BENCH1 + SY-INDEX - 1.

    LOOPS = 1.
    X = 0.
    T1 = 0.
    T2 = 0.

    DATA:
      BENCH_STR(10) TYPE C,
      BENCH_STR_LEN TYPE I,
      N_STR(10) TYPE C,
      N_STR_LEN TYPE I.

    WRITE BENCH TO BENCH_STR LEFT-JUSTIFIED NO-SIGN NO-GROUPING.
    BENCH_STR_LEN = STRLEN( BENCH_STR ).
    WRITE N TO N_STR LEFT-JUSTIFIED NO-SIGN NO-GROUPING.
    N_STR_LEN = STRLEN( N_STR ).
    WRITE: / 'Calibrating benchmark', AT (BENCH_STR_LEN) BENCH_STR,
      'with n=' NO-GAP, AT (N_STR_LEN) N_STR.

    WHILE 1 >= 1.
      DATA:
        T1_START TYPE I,
        T_DELTA TYPE I,
        LOOPS_P_SEC TYPE F.

      PERFORM GET_MS CHANGING T1_START.
      PERFORM RUN_BENCH USING BENCH LOOPS N CHANGING X.
      PERFORM GET_MS CHANGING T1.
      T1 = T1 - T1_START.

* compute difference abs(measures-estimated)
      IF T2 > T1.
        T_DELTA = T2 - T1.
      ELSE.
        T_DELTA = T1 - T2.
      ENDIF.

      IF T1 > 0.
        LOOPS_P_SEC = LOOPS * 1000 / T1.
      ELSE.
        LOOPS_P_SEC = 0.
      ENDIF.


      DATA:
        X_STR(10) TYPE C,
        X_STR_LEN TYPE I.


      IF X >= 0.
        WRITE X TO X_STR LEFT-JUSTIFIED NO-SIGN NO-GROUPING.
      ELSE.
        WRITE X TO X_STR LEFT-JUSTIFIED NO-GROUPING.
      ENDIF.
      X_STR_LEN = STRLEN( X_STR ).

      WRITE: /(10) LOOPS_P_SEC EXPONENT 0 DECIMALS 3 NO-GAP,
        '/s (time=' NO-GAP,
       (7) T1 NO-SIGN NO-GROUPING, 'ms, loops=',
       (8) LOOPS NO-SIGN NO-GROUPING NO-GAP, ', delta=',
       (6) T_DELTA NO-SIGN NO-GROUPING, 'ms, x=' NO-GAP,
       AT (X_STR_LEN) X_STR NO-GAP, ')'.

* some error?
      IF X = -1.
        APPEND -1 TO BENCH_RES1.
        EXIT.
      ENDIF.

* do we have some estimated/expected time?
      IF T2 > 0.
* smaller than delta_ms=100?
        IF T_DELTA < DELTA_MS.
          APPEND LOOPS_P_SEC TO BENCH_RES1.

          DATA:
            T1_STR(10) TYPE C,
            T1_STR_LEN TYPE I,
            LOOPS_STR(10) TYPE C,
            LOOPS_STR_LEN TYPE I,
            T_DELTA_STR(10) TYPE C,
            T_DELTA_STR_LEN TYPE I,
            LPS_STR(20) TYPE C,
            LPS_STR_LEN TYPE I.

          WRITE T1 TO T1_STR LEFT-JUSTIFIED NO-SIGN NO-GROUPING.
          T1_STR_LEN = STRLEN( T1_STR ).

          WRITE LOOPS TO LOOPS_STR LEFT-JUSTIFIED NO-SIGN NO-GROUPING.
          LOOPS_STR_LEN = STRLEN( LOOPS_STR ).

          WRITE T_DELTA TO T_DELTA_STR LEFT-JUSTIFIED NO-SIGN
NO-GROUPING.
          T_DELTA_STR_LEN = STRLEN( T_DELTA_STR ).

          WRITE LOOPS_P_SEC TO LPS_STR LEFT-JUSTIFIED NO-SIGN
NO-GROUPING EXPONENT 0 DECIMALS 3.
          LPS_STR_LEN = STRLEN( LPS_STR ).

          WRITE: / 'Benchmark', AT (BENCH_STR_LEN) BENCH_STR,
            '(ABAP):', AT (LPS_STR_LEN) LPS_STR NO-GAP,
            '/s (time=' NO-GAP, AT (T1_STR_LEN) T1_STR NO-SIGN,
            'ms, loops=' NO-GAP, AT (LOOPS_STR_LEN) LOOPS_STR NO-GAP,
            ', delta=' NO-GAP, AT (T_DELTA_STR_LEN) T_DELTA_STR NO-GAP,
            ' ms)'.
          EXIT.
        ENDIF.
      ENDIF.

      IF T1 > MAX_MS.
        DATA:
          MAX_MS_STR(10) TYPE C,
          MAX_MS_STR_LEN TYPE I.

        WRITE T1 TO MAX_MS_STR LEFT-JUSTIFIED NO-SIGN NO-GROUPING.
        MAX_MS_STR_LEN = STRLEN( MAX_MS_STR ).

        WRITE: / 'Benchmark', AT (BENCH_STR_LEN) BENCH_STR,
          '(ABAP): Time already >', AT (MAX_MS_STR_LEN) MAX_MS_STR,
          'ms. No measurement possible.'.
        APPEND -1 TO BENCH_RES1. "TTT??
        EXIT.
      ENDIF.

      DATA:
        SCALE_FACT TYPE I.
* scale a bit up to 1100 ms (cali_ms+100)
      IF ( T1 < CALI_MS ) AND ( T1 > 0 ).
        SCALE_FACT = ( ( CALI_MS + 100 ) DIV T1 ) + 1.
      ELSE.
        SCALE_FACT = 2.
      ENDIF.

      LOOPS = LOOPS * SCALE_FACT.
      T2 = T1 * SCALE_FACT.

    ENDWHILE.

  ENDDO.

  WRITE: / ''.
  WRITE: / 'Throughput for all benchmarks (loops per sec):'.
  WRITE: / 'BM Results (ABAP)      :'.

  DATA:
    TMP_F TYPE F.

  LOOP AT BENCH_RES1 INTO TMP_F.
    WRITE: AT (9) TMP_F EXPONENT 0 DECIMALS 2 NO-SIGN.
  ENDLOOP.

  PERFORM GET_MS CHANGING T2.
  T2 = T2 - T_START.

  DATA:
    T2_STR(10) TYPE C,
    T2_STR_LEN TYPE I.

  WRITE T2 TO T2_STR LEFT-JUSTIFIED NO-SIGN NO-GROUPING.
  T2_STR_LEN = STRLEN( T2_STR ).
  WRITE: / 'Total elapsed time:', AT (T2_STR_LEN) T2_STR, 'ms'.

ENDFORM.                    "MAIN
*
* end
*