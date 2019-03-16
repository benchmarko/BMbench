#!/usr/bin/clisp
;; BM Bench - bmbench.c (Lisp)
;; (c) Marco Vieth, 2002
;; http://www.benchmarko.de
;;
;; 24.01.2003  0.05  output format changed
;;
;; Usage:
;; clisp bmbench.lisp [bench1] [bench2] [n]
;;
;; Compile (clisp):
;; - clisp -c bmbench.lisp   => bmbench.fas
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


(declaim (optimize (speed 3) (debug 0) (safety 0) (space 0) (compilation-speed 0)))


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


(defun bench05 (loops n)
  (let (
      (x 0))
  x
  ))


;;
;;
;;
(defun current-date-string ()
  "Returns current date as a string."
  (multiple-value-bind (sec min hr day mon yr dow dst-p tz)
                       (get-decoded-time)
    (declare (ignore sec min hr dow dst-p tz))
    (format nil "~A-~A-~A" yr mon day)))

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
  (let ((num 1)
    (last_num 0)
    (bits 0)
    (last_type 0))

    (setq last_type (type-of num))
    (loop
      (setq last_num num)
      (setq num (* num 2))
      (incf num)
      (incf bits)
      (if (string/= last_type (type-of num))
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
;;
;;
;;
(defun main (argc argv)
  (let ((start_t (get_ms)) ;memorize start time
    (bench1 0)      ;first benchmark to test
    (bench2 5)      ;last benchmark to test
    (n 1000000)     ;maximum number
    (min_ms 10000)  ;minimum runtime for measurement in ms
    (x 0)
    (bench_res1))

;    (declare (fixnum n)
;             (fixnum x)
;             (optimize (speed 3) (debug 0) (safety 0)))

    (if (> argc 0)
      (setq bench1 (parse-integer (nth 0 argv)))
    )
    (if (> argc 1)
      (setq bench2 (parse-integer (nth 1 argv)))
    )
    (if (> argc 2)
      (setq n (parse-integer (nth 2 argv)))
    )

    (setq bench_res1 (make-array (+ bench2 1) :element-type 'fixnum))

    (format t "BM Bench v0.5 (Lisp) -- (short:~D int:~D float:~D double:~D) -- ~S~%" (checkbits_short1)
      (checkbits_int1) (checkbits_float1) (checkbits_double1) (lisp-implementation-version))
    (princ "(c) Marco Vieth, 2002") (terpri)
    (format t "~A~%" (current-time) )


    (loop for bench fixnum from bench1 to bench2 do
      (let ((loops 1) ;number of loops
        (x 0) ;result from benchmark
        (t1 0)) ;timestamp

      ;calibration
      ;This is: (loop while (and (< t1 1001) (/= x -1)) ;we want at least 1001 ms calibration time
      (do ((i 2 (1+ i))
          )
          ((or (>= t1 1001) (= x -1)))
      (progn
        (format t "Calibrating benchmark ~D with loops=~D, n=~D~%" bench loops n)
        (setq t1 (get_ms))
        (setq x (run_bench bench loops n))
        (setq t1 (- (get_ms) t1))
        (format t "x=~D (time: ~D ms)~%" x t1)
        (setq loops (* loops 2))
      ))
      (if (/= x -1) (progn
        (setq loops (floor loops 2))  ;>>= 1; /* div 2 */
        (setq loops (* loops (+ (floor min_ms t1) 1))) ;integer division!
        (format t "Calibration done. Starting measurement with ~D loops to get >=~D ms~%" loops min_ms)

        ;measurement
        (setq t1 (get_ms))
        (setq x (run_bench bench loops n))
        (setq t1 (- (get_ms) t1))
        (format t "x=~D (time: ~D ms)~%" x t1)

        (setf (aref bench_res1 bench) (floor (* t1 10) loops))  ;bench_res1[bench] = (int)(t1 * 10 / loops);
        (format t "Elapsed time for ~D loops: ~D ms; estimation for 10 loops: ~D ms~%" loops t1 (aref bench_res1 bench))
        ; (nth bench bench_res1)
      )(progn
        (setf (aref bench_res1 bench) -1)
      ))
    ))

    (format t "Times for all benchmarks (10 loops, ms):~%")
    (format t "BM Results (Lisp)      : ")


    (loop for bench fixnum from bench1 to bench2 do
      (format t "~7D" (aref bench_res1 bench)))
    (format t "~%")

    (format t "~&Total elapsed time: ~D ms~%" (- (get_ms) start_t))


  ))


(main (length *ARGS*) *ARGS*)

;end
