;#!/usr/bin/clisp
;Cannot compile with clisp 2.41 when starting with shebang line.
;
;; BM Bench - bmbench1.lisp (Lisp)
;; (c) Marco Vieth, 2002-2006
;; http://www.benchmarko.de
;;
;; 24.01.2003 0.05  output format changed
;; 01.05.2008 0.06  based on version 0.05
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


(declaim (optimize (speed 3) (debug 0) (safety 0) (space 0) (compilation-speed 0)))


(defconstant PRG_VERSION "0.06")
(defconstant PRG_LANGUAGE "Lisp")

;;
;; bench00 (Integer 16/32 bit)
;; (sum of 1..n) mod 65536
;;
(defun bench00 (loops n)
  (declare (fixnum loops n))
  (let (
      (x 0)
      (sum1 (logand (* (/ n 2) (+ n 1)) 65535))
       )
       (declare (fixnum x) (fixnum n) (fixnum i) (fixnum sum1))
    ;sum1..1000000 depends on type: 500000500000 (floating point), 1784293664 (32bit), 10528 (16 bit)
    (dotimes (loop loops)
      ;(format t "DEBUG: bench00: loop=~D, x=~D~%" loop x)
      (dotimes (i (+ n 1)) ;we need n+1 because dotimes counts up to n-1
        ;(incf x i)
        (setq x (logand (+ x i) 65535))
      )
      (if (< loop (- loops 1)) ;some more loops left?
        (progn (setq x (- x sum1)) ;yes, set x back to 0 (assuming n even)
          (if (/= x 0)
            (progn (incf x) ;force error
              (return x))
          )
        )
      )
    )
    (logand x 65535)
  ))

;;
;;
;; bench01 (Integer 16/32 bit)
;; (sum of 1..n) mod 65536
;;
(defun bench01 (loops n)
  (declare (fixnum loops n))
  (let (
      (x 0)
      (sum1 (* (/ n 2) (+ n 1)))
       )
       (declare (fixnum x) (fixnum n) (fixnum i))
    ;sum1..1000000 depends on type: 500000500000 (floating point), 1784293664 (32bit), 10528 (16 bit)
    (dotimes (loop loops)
      ;(format t "DEBUG: bench01: loop=~D, x=~D~%" loop x)
      (dotimes (i (+ n 1)) ;we need n+1 because dotimes counts up to n-1
        (incf x i)
        ;(setq x (+ x i))
        ;(setq x (logand (+ x i) 65535))
      )
      (if (< loop (- loops 1)) ;some more loops left?
        (progn (setq x (- x sum1)) ;yes, set x back to 0 (assuming n even)
          (if (/= x 0)
            (progn (incf x) ;force error
              (return x))
          )
        )
      )
    )
    ;(print (type-of x))

    (logand x 65535)
  ))

;;
;;
;; bench02 (Floating Point, normally 64 bit)
;; (sum of 1..n) mod 65536
;;
(defun bench02 (loops n)
  (declare (fixnum loops n))
  (let (
      (x 0.0d0)
      (sum1 (* (/ n 2.0d0) (+ n 1.0d0)))
       )
       (declare (double-float x sum1))
    ;sum1..1000000 depends on type: 500000500000 (floating point), 1784293664 (32bit), 10528 (16 bit)
    ;(print (type-of x))
    ;(print sum1)
    (dotimes (loop loops)
      (dotimes (i (+ n 1)) ;we need n+1 because dotimes counts up to n-1
        ;(incf x i)
        (setq x (+ x i))
      )
      ;(print (type-of x))
      ;(print x)
      (if (< loop (- loops 1)) ;some more loops left?
        (progn (setq x (- x sum1)) ;yes, set x back to 0 (assuming n even)
          (if (/= x 0.0d0)
            (progn (incf x) ;force error
              (floor (setq x (mod x 65536.0d0)))
              (return x))
          )
        )
      )
    )
    ;(print x)
    (floor (mod x 65536.0d0))
  ))

;;
;; bench03 (Integer)
;; number of primes below n (Sieve of Eratosthenes)
;; Example: n=500000 => x=41538 (expected), n=1000000 => x=78498
;;
(defun bench03 (loops n)
  (declare (fixnum loops n))
  (setf n (/ n 2)) ;compute only up to n/2
  (let (
      (x 0) ;number of primes below n
      (sieve1 (make-array (+ n 1) :element-type 'fixnum :initial-element 1))
       )
       (declare (fixnum x) (fixnum n) (fixnum i))

    (setf (aref sieve1 0) 0)
    (setf (aref sieve1 1) 0)
    (dotimes (loop loops)
      ;initialize sieve
      (loop for i fixnum from 2 to n do
        (setf (aref sieve1 i) 1)
      )

      ;compute primes
      (do ((i 2 (1+ i)) ;general do loop: (<var> <initial> <increment>) (<end condition>) (<body>); 1+ is incf
          )
          ((> (* i i) n))
          (unless (zerop (aref sieve1 i))
            ;(print i)
            (loop for j fixnum from (* i i) upto n by i do
                  (setf (aref sieve1 j) 0))
          )
       )

      ;count primes
      (loop for i fixnum from 0 to n do
        (unless (zerop(aref sieve1 i))
          (incf x)
        )
      )

      ;(print x)

      ;check prime count
      (if (< loop (- loops 1)) ;some more loops left?
        (progn (setq x (- x 41538)) ;yes, set x back to 0 (assuming n even)
          (if (/= x 0)
            (progn (incf x) ;force error
              (return x))
          )
        )
      )
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
(defun bench04 (loops n)
  (declare (fixnum loops n))
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

    (dotimes (loop loops)
      (dotimes (i n)
        (setq x_div_q (floor x bench04_q)) ;seems to be slower: (setq x_div_q (truncate (/ x q)))
        ;(print x_div_q)
        (setq x_mod_q (- x (* bench04_q x_div_q)))
        (setq x (- (* bench04_a x_mod_q) (* bench04_r x_div_q)))
        (if (<= x 0)
          (incf x  bench04_m) ;x is new random number
        )
      )
      (if (< loop (- loops 1)) ;some more loops left?
        (progn (setq x (- x 1227283347)) ;yes, set x back to 0 (assuming n even)
          (if (/= x 0)
            (progn (incf x) ;force error
              (return x))
          )
          (incf x) ;start with 1 again
        )
      )
    )
    x
  ))

;; (maybe to improve...)
;; bench05 (Integer 32 bit)
;; n over n/2 mod 65536 (Pascal's triangle)
;; (we just need to store the last 2 lines of computation)
;;
(defun bench05 (loops n)
  (declare (fixnum loops n))
  (setq n (floor (/ n 500)))
  (let (
      (x 0)
      (k 0)
      ;(pas1 (make-array (+ n 1) :element-type 'fixnum :initial-element 1))
      (pas1_a 0) ;array
      (pas2_a 0)
      (pas1 0)   ;array pointer
      (pas2 0)
      (min1 0)
      )

    (declare (fixnum k min1 pas1 pas2))

    (setq k (floor (/ n 2)))

    (if (< (- n k) k)
      (setq k (- n k)) ;keep k minimal with  n over k  =  n over n-k
    )

    (setq pas1_a (make-array (+ k 1) :element-type 'fixnum :initial-element 0))
    (setq pas2_a (make-array (+ k 1) :element-type 'fixnum :initial-element 0))
    (setf (aref pas1_a 0) 1)
    (setf (aref pas2_a 0) 1)

    ;(setq pas1 nil)
    ;(setq pas2 nil)

    ;(format t "DEBUG0: ~A ~A ~A ~A~%" n k pas1 (type-of pas1) )

    (dotimes (loop loops)
      ;(setq pas1 (list 1))
      (loop for i fixnum from 2 to n do
        ;(format t "DEBUG1: ~A ~A ~A ~A i=~A~%" n k pas1 pas2 i )
        ;(setq pas2 pas1) ;get last line to pas2 (for array point to same array, do not use!

        ;set array pointer...
        (if (= (mod i 2) 0)
          (progn
            (setq pas1 pas1_a)
            (setq pas2 pas2_a)
          )
          (progn
            (setq pas1 pas2_a)
            (setq pas2 pas1_a)
          )
        )

        ;(setq pas1 (list 1)) ;and restart with new list

        (setq min1 (floor (/ (- i 1) 2)))
        (if (< k min1)
          (setq min1 k)
        )

        ;(push i pas1) ;second column is i
        (setf (aref pas1 1) i)

        ;(format t "DEBUGx: n=~A k=~A min1=~A pas1=~A pas2=~A~%" n k min1 pas1 pas2 )

        (loop for j fixnum from 2 to min1 do  ;up to min((i-1)/2, k)
          ;(format t "DEBUG3: n=~A k=~A min1=~A pas1=~A pas2=~A j=~A~%" n k min1 pas1 pas2 j )
          ;(push (+ (nth (- j 1) pas2) (nth j pas2)) pas1)
          ;(setf (aref pas1 j) (+ (aref pas2 (- j 1)) (aref pas2 j)))
          (setf (aref pas1 j) (logand (+ (aref pas2 (- j 1)) (aref pas2 j)) 65535)) ; use and to get small numbers
          ;(format t "DEBUGy: n=~A k=~A min1=~A pas1=~A pas2=~A j=~A~%" n k min1 pas1 pas2 j )
        )

        (if (and (< min1 k) (= (mod i 2) 0)) ;new element
          ;(push (* 2 (nth min1 pas2)) pas1)
          (setf (aref pas1 (+ min1 1)) (* 2 (aref pas2 min1)))
          ;(format t "DEBUG4: n=~A k=~A min1=~A pas1=~A pas2=~A new~%" n k min1 pas1 pas2 )
        )
      )

      ;(format t "DEBUG5: ~A ~A ~A ~A~%" n k pas1 pas2 )
      ;(setq x (logand (+ x (nth k pas1)) 65535))
      (setq x (logand (+ x (aref pas1 k)) 65535))

      (if (< loop (- loops 1)) ;some more loops left?
        (progn (setq x (- x 27200)) ;yes, set x back to 0 (assuming n even)
          (if (/= x 0)
            (progn (incf x) ;force error
              (return x))
          )
        )
      )
    )
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
(defun run_bench (bench loops n)
  (declare (fixnum bench loops n))
  (let ((x 0)
    (check1 0))

   (cond ((eql bench 0)
       (setq x (bench00 loops n))
       (setq check1 10528))

     ((eql bench 1)
       (setq x (bench01 loops n))
       (setq check1 10528))

     ((eql bench 2)
       (setq x (bench02 loops n))
       (setq check1 10528))

     ((eql bench 3)
       (setq x (bench03 loops n))
       (setq check1 41538))

     ((eql bench 4)
       (setq x (bench04 loops n))
       (setq check1 1227283347))

     ((eql bench 5)
       (setq x (bench05 loops n))
       (setq check1 27200))

     (t (format t "Error: unknown benchmark: ~D~%" bench) (incf x)))
   (if (/= x check1)
     (progn
       (format t "Error(bench~D): x=~D~%" bench x)
       (setq x -1) ;force error
     )
   )
  x
  ))

  
;;
;;
;;
(defun get_ms ()
  (let ((t1 (GET-INTERNAL-REAL-TIME)))
  (setq t1 (floor t1 (/ INTERNAL-TIME-UNITS-PER-SECOND 1000))) ;convert to msec
  ))


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
  (format t "BM Bench v~A (~A) -- (short:~D int:~D float:~D double:~D) -- ~A~%" PRG_VERSION PRG_LANGUAGE (checkbits_short1)
    (checkbits_int1) (checkbits_float1) (checkbits_double1) (lisp-implementation-version))
  (princ "(c) Marco Vieth, 2006") (terpri)
  (format t "Date: ~A~%" (get_current_time))
  ;(format t "Date: ~A~%" (current-time) ) ;TTT
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
    (format t "~9,2F" (aref bench_res1 bench)))
  (format t "~%")
  (format t "~%")
  ))

;;
;;
;;
(defun start_bench (bench1 bench2 n)
  (declare (fixnum bench1 bench2 n))
  (let (
    (cali_ms 1001)
    (delta_ms 100)
    (max_ms 10000)
    (bench_res1))

;    (declare (fixnum n)
;             (fixnum x)
;             (optimize (speed 3) (debug 0) (safety 0)))

    (declare (fixnum cali_ms delta_ms max_ms x t1 t2 t_delta scale_fact))

    (print_info)

    ;(setq bench_res1 (make-array (+ bench2 1) :element-type 'fixnum))
    (setq bench_res1 (make-array (+ bench2 1) :element-type 'single-float))

    (loop for bench fixnum from bench1 to bench2 do
      (let ((loops 1) ;number of loops
        (x 0)         ;result from benchmark
        (t1 0)        ;measured time
        (t2 0))       ;estimated time

      (format t "Calibrating benchmark ~D with n=~D~%" bench n)

      (loop
        (let (
          (t_delta 0)
          (loops_p_sec 0)
          (scale_fact 0)
        )

        (progn
          (setq t1 (get_ms))
          (setq x (run_bench bench loops n))
          (setq t1 (- (get_ms) t1))
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

          (format t "~10,3F/s (time=~5D ms, loops=~7D, delta=~5D ms, x=~D)~%" loops_p_sec t1 loops t_delta x)
        )

        (if (= x -1)
          (progn
            (setf (aref bench_res1 bench) -1)
            (return) ;last
          )
        )

        (if (> t2 0) ;do we have some estimated/expected time?
          (if (< t_delta delta_ms) (progn ;smaller than delta_ms=100?
            (setf (aref bench_res1 bench) loops_p_sec)
            (format t "Benchmark ~D (~A): ~,3F/s (time=~D ms, loops=~D, delta=~D ms)~%" bench PRG_LANGUAGE (aref bench_res1 bench) t1 loops t_delta)
            (return) ;last
          ))
        )

        (if (> t1 max_ms)
          (progn
            (format t "Benchmark ~D (~A): Time already > ~D ms. No measurement possible.~%" bench PRG_LANGUAGE max_ms)
            (setf (aref bench_res1 bench) -1)
            (return) ;last
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

      )
    ))

    (print_results bench1 bench2 bench_res1)
    0
  ))

;;
;;
;;
(defun main (argc argv)
  (let ((start_t (get_ms)) ;memorize start time
    (bench1 0)      ;first benchmark to test
    (bench2 5)      ;last benchmark to test
    (n 1000000)
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

    (setq rc (start_bench bench1 bench2 n))

    (format t "~&Total elapsed time: ~D ms~%" (- (get_ms) start_t))
    rc ;(return-from main rc)
  ))


(main (length *ARGS*) *ARGS*)
;(quit 0)
;end
