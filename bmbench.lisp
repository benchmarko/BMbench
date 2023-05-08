;#!/usr/bin/clisp
;Cannot compile with clisp 2.41 when starting with shebang line.
;
;; BM Bench - bmbench1.lisp (Lisp)
;; (c) Marco Vieth, 2002-2006
;; http://www.benchmarko.de
;;
;; 24.01.2003 0.05  output format changed
;; 01.05.2008 0.06  based on version 0.05
;; 06.05.2023 0.08  adapted for new version
;;
;; Usage:
;; clisp bmbench.lisp [bench1] [bench2] [n]
;;
;; Compile (clisp):
;; - clisp -c bmbench.lisp -o bmbench.fas
;; - clisp bmbench.fas
;;
;; cmucl: ...
;;
;;
;; Info:
;; man clisp
;; info elisp
;;
;; /usr/share/doc/packages/elisp-manual
;; /usr/share/doc/packages/clisp/doc ??
;;
;; David B. Lamkins: Lisp...  (http://www.psg.com/~dlamkins/sl/contents.html)
;;

; command: (emacs-version)  ; variable: emacs-version
; *FEATURES*
; Check type of n: (type-of n)
;
; FIXNUM (Integer): normally 28 bit
; (string-to-number "num")
; (make-bool-vector <length> <initial>)  (aref <array> <index)   (aset <array> <index> <value)
; Timing:  (time (...))     (SYSTEM::%%TIME)
; (apropos 'concat 'user)
; (inspect ...)
; (describe  ...)
; (DISASSEMBLE #'ARGLIST)
;
; defconst not available?

;
; Loops:
; (loop for i fixnum from 1 upto n do
; (dotimes (i n)   count 0..n-1 !

;
; format: output strings: ~S with surrounding quotes, ~A without
;


; https://www.jdoodle.com/execute-clisp-online/

(declaim (optimize (speed 3) (debug 0) (safety 0) (space 0) (compilation-speed 0)))


(defconstant PRG_VERSION "0.08")
(defconstant PRG_LANGUAGE "Lisp")

(defvar gState_startTs 0)
(defvar gState_tsPrecMs 0.0d0)
(defvar gState_tsPrecCnt 0)
(defvar gState_tsMeasCnt 0)

;;
;; bench00 (Integer 16/32 bit)
;; (sum of 1..n) mod 65536
;;
(defun bench00 (n)
  (declare (fixnum n))
  (let (
      (x 0)
       )
       (declare (fixnum x) (fixnum n) (fixnum i) (fixnum sum1))
      (dotimes (i (1+ n)) ;we need n+1 because dotimes counts up to n-1
        ;(incf x i)
        (setq x (logand (+ x i) 65535))
      )
    (logand x 65535)
  ))

;;
;;
;; bench01 (Integer 16/32 bit)
;; (sum of 1..n) mod 65536
;;
(defun bench01 (n)
  (declare (fixnum n))
  (let (
      (x 0)
      (sum1 0)
       )
       (declare (fixnum x) (fixnum sum1) (fixnum n) (fixnum i))
      (dotimes (i (1+ n))
        (incf sum1 i)
        (if (>= sum1 n)
          (setq sum1 (- sum1 n))
          (incf x)
        )
      )
      (decf x) ;TTT
    x
  ))

;;
;;
;; bench02 (Floating Point, normally 64 bit)
;; (sum of 1..n) mod 65536
;;
(defun bench02 (n)
  (declare (fixnum n))
  (let (
      (x 0)
      (sum1 0.0d0)
       )
       (declare (fixnum x) (double-float sum1))
      (dotimes (i (1+ n))
        (setq sum1 (+ sum1 i))
        (if (>= sum1 n)
          (setq sum1 (- sum1 n))
          (incf x)
        )
      )
    (decf x) ;TTT
    x
  ))

;;
;; bench03 (Integer)
;; number of primes below n (Sieve of Eratosthenes)
;; Example: n=500000 => x=41538 (expected), n=1000000 => x=78498
;;
(defun bench03 (n)
  (declare (fixnum n))
  (setq nHalf (floor (/ n 2)))
  (let (
      (x 1) ;number of primes below n (2 is prime)
      (m 3)
      (i0 0)
      (sieve1 (make-array (1+ nHalf) :element-type 'fixnum :initial-element 1))
       )
       (declare (fixnum nHalf x m n i))

    ;(setf (aref sieve1 0) 0)
    ;(setf (aref sieve1 1) 0)
      ;initialize sieve
      (loop for i fixnum from 0 to nHalf do
        (setf (aref sieve1 i) 0)
      )

      ;compute primes
      (setq i0 (do ((i 0 (1+ i)) ;general do loop: (<var> <initial> <increment>) (<end condition>) (<body>); 1+ is incf
          )
          ((> (* m m) n) i)
          (if (zerop (aref sieve1 i))
           (progn
            (incf x) ;m is prime
            (loop for j fixnum from (/ (- (* m m) 3) 2) upto nHalf by m do
                  (setf (aref sieve1 j) 1))
           )
          )
          (incf m 2)
          ;(setq i0 i)
       ))

      ;count remaining primes
      ;(print i0)
      ;(setq i i0)
      (loop for j fixnum from m to n by 2 do
        (if (zerop (aref sieve1 i0))
          (incf x)
        )
        (incf i0)
      )
    x
  ))


;not much faster with constants...
(defconstant bench04_m 2147483647) ;modulus, do not change!
(defconstant bench04_a 16807)      ;multiplier
(defconstant bench04_q 127773)     ;m div a
(defconstant bench04_r 2836)       ;m mod a

;;
;; bench04 (Integer 32 bit)
;; nth random number number
;; Random number generator taken from
;; Raj Jain: The Art of Computer Systems Performance Analysis, John Wiley & Sons, 1991, page 442-444.
;; It needs longs with at least 32 bit.
;; Starting with x0=1, x10000 should be 1043618065, x1000000 = 1227283347.
;;
(defun bench04 (n)
  (declare (fixnum n))
  (let (
      (x 1)          ;last random value
      ;(bench04_m 2147483647) ;modulus, do not change!
      ;(bench04_a 16807)      ;multiplier
      ;(bench04_q 127773)     ;m div a
      ;(bench04_r 2836)       ;m mod a
      (x_div_q 0)
      (x_mod_q 0)
       )
    (declare (fixnum x n i d_div_q x_mod_q))

      (dotimes (i n)
        (setq x_div_q (floor x bench04_q)) ;seems to be slower: (setq x_div_q (truncate (/ x q)))
        ;(print x_div_q)
        (setq x_mod_q (- x (* bench04_q x_div_q)))
        (setq x (- (* bench04_a x_mod_q) (* bench04_r x_div_q)))
        (if (<= x 0)
          (incf x  bench04_m) ;x is new random number
        )
      )
    x
  ))

;; bench05 (Integer 32 bit)
;; n over n/2 mod 65536 (Pascal's triangle)
;; (we just need to store the last 2 lines of computation)
;;
(defun bench05 (n)
  (declare (fixnum n))
  (setq nHalf (floor (/ n 2))) ;Instead of nCk with k=n/2, we compute the product of (n/2)Ck with k=0..n/4
  (let (
      (x 0)
      (k 0)
      (line1 0) ;array
      (min1 0)
      (prev1 0)
      (num1 0)
      )

    (declare (fixnum k min1 prev1 num1))

    (setq k (floor (/ n 2)))

    (if (< (- n k) k)
      (setq k (- n k)) ;keep k minimal with  n over k  =  n over n-k
    )

    (setq line1 (make-array (1+ k) :element-type 'fixnum :initial-element 0))

    (setf (aref line1 0) 1)
    (setf (aref line1 1) 2) ;for line 2, second column is 2

      (loop for i fixnum from 3 to n do
        (setq min1 (floor (/ (1- i) 2)))
        (if (= (mod i 2) 0) ;new element
          (setf (aref line1 (+ min1 1)) (* 2 (aref line1 min1)))
        )

        (setq prev1 (aref line1 1))
        (loop for j fixnum from 2 to min1 do  ;up to min((i-1)/2, k)
          (setq num1 (aref line1 j))
          ;(setf (aref line1 (+ min1 1)))
          (setf (aref line1 j) (logand (+ (aref line1 j) prev1) 65535)) ;use and to get small numbers
          (setq prev1 num1)
        )
        (setf (aref line1 1) i)
      )

      ;compute sum of ((n/2)Ck)^2 mod 65536 for k=0..n/2
      (loop for j fixnum from 0 to (1- k) do
          (incf x (* (aref line1 j) (aref line1 j) 2)) ;add nCk and nC(n-k)
      )
      (incf x (* (aref line1 k) (aref line1 k)))

      ;(format t "DEBUG5: ~A ~A ~A ~A~%" n k pas1 pas2 )
      (setq x (logand x 65535))

    x
  ))


(defun bench06 (n)
  (declare (fixnum n))
  (let (
      (sum1 0.0d0)
      (flip1 -1.0d0)
      )
    (declare (double-float sum1 flip1))
    (loop for i fixnum from 1 to n do
      (setq flip1 (* flip1 -1.0d0))
      (incf sum1 (/ flip1 (- (* 2 i) 1)))
    )
    (setq x (floor (* sum1 4.0d0 100000000d0)))
    x
  ))

;;
;;
;;
;; (defun current-date-string ()
;;   "Returns current date as a string."
;;   (multiple-value-bind (sec min hr day mon yr dow dst-p tz)
;;                        (get-decoded-time)
;;     (declare (ignore sec min hr dow dst-p tz))
;;     (format nil "~A-~A-~A" yr mon day)))


;; run a benchmark
;; in: bench = benchmark to use
;;   loops = number of loops
;;   n = maximum number (used in some benchmarks to define size of workload)
;; out:    x = result
;;
(defun run_bench (bench loops n check1)
  (declare (fixnum bench loops n check1))
  (let ((x 0)
    )

   (dotimes (loop loops)
       (cond ((eql bench 0)
           (setq x (bench00 n)))

         ((eql bench 1)
           (setq x (bench01 n)))

         ((eql bench 2)
           (setq x (bench02 n)))

         ((eql bench 3)
           (setq x (bench03 n)))

         ((eql bench 4)
           (setq x (bench04 n)))

         ((eql bench 5)
           (setq x (bench05 n)))

         ((eql bench 6)
           (setq x (bench06 n)))

         (t (format t "Error: unknown benchmark: ~D~%" bench) (incf x)))

       (if (/= x check1)
         (progn
           (format t "Error(bench~D): x=~D~%" bench x)
           (setq x -1) ;force error
           (return)
         )
       )
   )
  x
  ))


;;
;;
(defun bench03Check (n)
  (declare (fixnum n))
  (let (
    (x 1)
    (isPrime1 0)
    ) ;2 is prime

   (declare (fixnum x isPrime1))

    (loop for j fixnum from 3 to n by 2 do
      (setq isPrime1 1)
      ;(loop for i fixnum from 3 to j by 2 do
      (do ((i 3 (+ i 2)) ;general do loop: (<var> <initial> <increment>) (<end condition>) (<body>); 1+ is incf
          )
          ;((> (* i i) j))
          ((or (> (* i i) j) (zerop isPrime1)))
          (if (zerop (mod j i))
            (setq isPrime1 0)
            ;(return)
          )
      )
      (unless (zerop isPrime1)
        (incf x)
      )
    )

   x
  ))


;;
;;
(defun getCheck (bench n)
  (declare (fixnum bench n))
  (let (
    (check1 0))

   (cond ((eql bench 0)
       (setq check1 (logand (* (/ n 2) (1+ n)) 65535)))

     ((eql bench 1)
       (setq check1 (floor (1+ n) 2)))

     ((eql bench 2)
       (setq check1 (floor (1+ n) 2)))

     ((eql bench 3)
       (setq check1 (if (= n 1000000) 41538 (bench03Check n))))

     ((eql bench 4)
       (setq check1 (if (= n 1000000) 1227283347 (bench04 n))))

     ((eql bench 5)
       (setq check1 (if (= n 5000) 27200 (bench05 n))))

     ((eql bench 6)
       (setq check1 (if (= n 1000000) 314159165 (bench06 n))))

     (t (format t "Error: unknown benchmark: ~D~%" bench) (setq check1 -1)))
  check1
  ))

;;
;;
;;
(defun get_raw_ts ()
  (GET-INTERNAL-REAL-TIME)
  )

;;
(defun get_ts ()
  (- (get_raw_ts) gState_startTs)
  )

(defun conv_ms (ts)
  (/ ts (/ INTERNAL-TIME-UNITS-PER-SECOND 1000)) ;convert to msec
  )

(defun correctTime (tMeas tMeas2 measCount)
  (declare (double-float tMeas tMeas2) (fixnum measCount))

  (if (< measCount gState_tsPrecCnt)
    (setq tMeas (* gState_tsPrecMs (/ (- tsPrecCnt measCount) gState_tsPrecCnt)))
    (if (> tMeas tMeas2) ;cannot correct
      (setq tMeas tMeas2)
    )
  )
   tMeas
  )


(defun getPrecMs (stopFlg)
  (declare (fixnum stopFlg))
  (setq gState_tsMeasCnt 0)
  (let ((tMeas0 0)
      (tMeas 0)
      (tMeasD 0.0d0))
    (declare (fixnum tMeas0 tMeas) (double-float tMeasD))
    (setq tMeas0 (get_ts))
    (setq tMeas tMeas0)
    (loop
      (if (> tMeas tMeas0)
        (return)
      )
      (setq tMeas (get_ts))
      (incf gState_tsMeasCnt)
    )

   (setq tMeasD (if (= stopFlg 1)
     (conv_ms tMeas)
     (correctTime (conv_ms tMeas0) (conv_ms tMeas) gState_tsMeasCnt)
   ))
   ;(format t "TTT2: ~F ~F~%" tMeas tMeasD)
   tMeasD
  ))


(defun determineTsPrecision ()
  (setq gState_startTs (get_raw_ts))
  (let ((tMeas0 0.0d0)
      (tMeas1 0.0d0))
    (declare (double-float tMeas0 tMeas1))

    (setq tMeas0 (getPrecMs 0))
    (setq tMeas1 (getPrecMs 0))
    (setq gState_tsPrecMs (- tMeas1 tMeas0))
    (setq gState_tsPrecCnt gState_tsMeasCnt)

    ;do it again
    (setq tMeas0 tMeas1)
    (setq tMeas1 (getPrecMs 0))

    ;(format t "DEBUG1: ~S~%" (type-of tMeas0) )
    ;(format t "DEBUG2: ~S~%" (type-of gState_tsPrecMs) )

    (if (> gState_tsMeasCnt gState_tsPrecCnt) ;// taker maximum count
      (progn
      (setq gState_tsPrecMs (- tMeas1 tMeas0))
      (setq gState_tsPrecCnt gState_tsMeasCnt)
      )
    )
  ))


;;(defun get_ms ()
;;  (let ((t1 (get_ts)))
;;  (setq t1 (floor t1 (/ INTERNAL-TIME-UNITS-PER-SECOND 1000))) ;convert to msec
;;  ))


;;(defun get_ms ()
;;  (conv_ms (get_ts))
;;  )


;;
;;
;;
(defun checkbits_short1 ()
  (let ((num 2) ; start with 2 because 1 is type BIT
    (last_num 1)
    (bits 1)
    (last_type 0))

    (setq last_type (type-of num))
    ;(format t "DEBUG: ~S~%" (type-of num) )

    (loop
      (setq last_num num)
      (setq num (* num 2))
      (incf num)
      (incf bits)
      ;(format t "DEBUG: ~S~%" (type-of num) )
      ;(if (string/= last_type (type-of num)) ; comparison does not work for type tupels
      ;  (return bits)
      ;)
      (if (not (eq last_type (type-of num))) ; type changed?
        (return bits)
      )
      (if (not (and (= (/ (- num 1) 2) last_num) (< bits 101) ))
        (return bits)
      )
    )
    bits
  ))

;;
;;
;;
(defun checkbits_int1 ()
  (let ((num 1)
    (last_num 0)
    (bits 0))

    (loop
      (setq last_num num)
      (setq num (* num 2))
      (incf num)
      (incf bits)
      (if (not (and (= (/ (- num 1) 2) last_num) (< bits 101)))
        (return bits)
      )
    )
    bits
  ))

;;
;;
;;
(defun checkbits_float1 ()
  (let ((num 1.0)
    (last_num 0.0)
    (bits 0))

    (loop
      (setq last_num num)
      (setq num (* num 2.0))
      (incf num)
      (incf bits)
      (if (not (and (= (/ (- num 1.0) 2.0) last_num) (< bits 101)))
        (return bits)
      )
    )
    bits
  ))

;;
;;
;;
(defun checkbits_double1 ()
  (let ((num 1.0d0)
    (last_num 0.0d0)
    (bits 0))

    (loop
      (setq last_num num)
      (setq num (* num 2.0d0))
      (incf num)
      (incf bits)
      (if (not (and (= (/ (- num 1.0d0) 2.0d0) last_num) (< bits 101)))
        (return bits)
      )
    )
    bits
  ))



;rfc 1945, 1123?
(defun get_current_time ()
  (defvar wkdays '("Mon" "Tue" "Wed" "Thu" "Fri" "Sat" "Sun"))
  (defvar months '("Jan" "Feb" "Mar" "Apr" "May" "Jun" "Jul" "Aug" "Sep" "Oct" "Nov" "Dec"))
  (multiple-value-bind
      (second minute hour day month year wkday)
      (decode-universal-time (get-universal-time))
    (format nil "~a, ~2,'0d ~a ~a ~2,'0d:~2,'0d:~2,'0d GMT"
      (nth wkday wkdays) day (nth (1- month) months)
      year hour minute second)))

;;
;;
;;
(defun print_info ()
  (format t "BM Bench v~A (~A) -- (short:~D int:~D float:~D double:~D tsMs:~F tsCnt:~D) -- ~A~%" PRG_VERSION PRG_LANGUAGE (checkbits_short1)
    (checkbits_int1) (checkbits_float1) (checkbits_double1) gState_tsPrecMs gState_tsPrecCnt (lisp-implementation-version))
  (princ "(c) Marco Vieth, 2006-2023") (terpri)
  (format t "Date: ~A~%" (get_current_time))
  )



(defun print_results (bench1 bench2 bench_res1)
  (declare (fixnum bench1 bench2))
  (let (
    (max_language_len1 10))
  ;(format t "DEBUG: ~S~%" (type-of PRG_LANGUAGE) )
  (terpri)
  (princ "Throughput for all benchmarks (loops per sec):") (terpri)

  (format t "BMR (~A)~VA: " PRG_LANGUAGE (- max_language_len1 (length PRG_LANGUAGE)) "")
  (loop for bench fixnum from bench1 to bench2 do
    (format t "~9,3F" (aref bench_res1 bench)))
  (format t "~%")
  (format t "~%")
  ))


;;
;;
;;
(defun measureBench (bench n check1 cali_ms)
  (declare (fixnum bench n check1 cali_ms))
  (let (
    (delta_ms 100)
    (max_ms 10000)
    (loops 1) ;number of loops
    (x 0)         ;result from benchmark
    (t1 0.0d0)        ;measured time
    (t2 0.0d0)       ;estimated time
    (throughput 0.0d0))

;    (declare (fixnum n)
;             (fixnum x)
;             (optimize (speed 3) (debug 0) (safety 0)))

    (declare (fixnum delta_ms max_ms x scale_fact startN))

    (format t "Calibrating benchmark ~D with n=~D, check=~D~%" bench n check1)

     (setq throughput (loop
        (let (
          (t_delta 0.0d0)
          (loops_p_sec 0.0d0)
          (scale_fact 0)
        )

        (progn
          (setq t1 (getPrecMs 0))
          (setq x (run_bench bench loops n check1))
          (setq t1 (- (getPrecMs 1) t1))
          ;(format t "DEBUG: x=~D (time: ~D ms)~%" x t1)
        )

        (progn
          (setq t_delta
            (if (> t2 t1)
              (- t2 t1)
              (- t1 t2)))

          (setq loops_p_sec
            (if (> t1 0)
              (/ (* loops 1000.0) t1)
              0))

          (format t "~10,3F/s (time=~9,3F ms, loops=~7D, delta=~9,3F ms, x=~D)~%" loops_p_sec t1 loops t_delta x)
        )

        (if (= x -1)
            (return -1) ;last
        )

        (if (> t2 0) ;do we have some estimated/expected time?
          (if (< t_delta delta_ms) (progn ;smaller than delta_ms=100?
            (format t "Benchmark ~D (~A): ~,3F/s (time=~,3F ms, loops=~D, delta=~,3F ms)~%" bench PRG_LANGUAGE loops_p_sec t1 loops t_delta)
            (return loops_p_sec) ;last
          ))
        )

        (if (> t1 max_ms)
          (progn
            (format t "Benchmark ~D (~A): Time already > ~D ms. No measurement possible.~%" bench PRG_LANGUAGE max_ms)
            (return -1) ;last
          )
        )

        ; scale a bit up to 1100 ms (cali_ms+100)
        (setq scale_fact
          (if (and (< t1 cali_ms) (> t1 0))
            (floor (+ (/ (+ cali_ms 100) t1) 1))
            2
          )
        )

        (setq loops (* loops scale_fact))
        (setq t2 (* t1 scale_fact))

        )

      ))
    throughput
  ))

;;
;;
;;
(defun start_bench (bench1 bench2 n cali_ms)
  (declare (fixnum bench1 bench2 n cali_ms))
  (let (
    (startN n)
    (bench_res1))

    (declare (fixnum startN))

    (determineTsPrecision)
    (print_info)

    (setq bench_res1 (make-array (+ bench2 1) :element-type 'double-float))

    (loop for bench fixnum from bench1 to bench2 do
      (let ((loops 1) ;number of loops
        (x 0)         ;result from benchmark
        (t1 0.0d0)        ;measured time
        (t2 0.0d0)       ;estimated time
        (check1 0))


      (setq n startN)
      (cond ((eql bench 3)
        (setq n (/ n 2)))

        ((eql bench 5)
         (setq n (floor n 500)))
      )

      (setq check1 (getCheck bench n))


      (setf (aref bench_res1 bench) (if (> check1 0)
        (measureBench bench n check1 cali_ms)
        -1
        ))

    ))

    (print_results bench1 bench2 bench_res1)
    0
  ))

;;
;;
;;
(defun main (argc argv)
    (let ((bench1 0)      ;first benchmark to test
    (bench2 5)      ;last benchmark to test
    (n 1000000)
    (cali_ms 1001)
    (rc 0))    ;maximum number

    (if (> argc 0)
      (setq bench1 (parse-integer (nth 0 argv)))
    )
    (if (> argc 1)
      (setq bench2 (parse-integer (nth 1 argv)))
    )
    (if (> argc 2)
      (setq n (parse-integer (nth 2 argv)))
    )
    (if (> argc 3)
      (setq cali_ms (parse-integer (nth 3 argv)))
    )

    (setq rc (start_bench bench1 bench2 n cali_ms))

    (format t "~&Total elapsed time: ~D ms~%" (conv_ms (get_ts)))
    rc ;(return-from main rc)
  ))


(main (length *ARGS*) *ARGS*)
;(quit 0)
;end
