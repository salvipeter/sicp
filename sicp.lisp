;;; -*- mode: lisp; syntax: common-lisp -*-

;;; SICP code translated to Common Lisp, with exercises

;; The eval at the end of this file installs a local feature
;; that hides the solutions to the exercises (requires Emacs 22).
;;
;; Usage:
;;
;; Call HIDE-EXERCISE-HIDE-ALL to hide all exercise solutions
;; Call HIDE-EXERCISE-SHOW-ALL to show all exercise solutions
;; Press C-c s on the solution text to toggle visibility

;;; Conversion notes:
;;; - names are CLified:
;;; - functions with names of CL function names get a % prefix
;;; - functions redefinitions are renamed and documented*
;;; - flet/labels instead of defun inside defun
;;; - some SICP-defined functions are substituted with their CL equivalents
;;; - exercises may use CL-specific code or functions that are not yet learned
;;; - code (esp. example code) is moved around for a better flow

;;; * This allows the file to be compiled/loaded as a whole, but of course
;;;   you have to change the names in functions if you want to use redefined
;;;   versions of some utilities.

(eval-when (:compile-toplevel :load-toplevel :execute)
  (require :lispbuilder-sdl)
  (require :bordeaux-threads)
  (rename-package :bordeaux-threads :bordeaux-threads '(:threads)))

(in-package :cl-user)
(defpackage :sicp
  (:use :common-lisp))
(in-package :sicp)


;;; Section 1.1.4

(defun square (x) (* x x))

(defun sum-of-squares (x y)
  (+ (square x) (square y)))


;;; Section 1.1.6

(defun %abs (x)
  (cond ((> x 0) x)
	((= x 0) 0)
	((< x 0) (- x))))

(defun abs-2 (x)
  "With two branches."
  (cond ((< x 0) (- x))
	(t x)))

(defun abs-3 (x)
  "With IF instead of COND."
  (if (< x 0)
      (- x)
      x))

(defun %>= (x y)
  (or (> x y) (= x y)))

(defun >=-2 (x y)
  "Using <."
  (not (< x y)))

;;; Exercise 1.1 START

;;; => 10
;;; => 12
;;; => 8
;;; => 3
;;; => 6
;;; => A ()
;;; => B ()
;;; => 19
;;; => NIL (#f)
;;; => 4
;;; => 16
;;; => 6
;;; => 16

;;; Exercise 1.1 END

;;; Exercise 1.2 START

#+nil
(/ (+ 5 4 (- 2 (- 3 (+ 6 4/5))))
   (* 3 (- 6 2) (- 2 7)))

;;; Exercise 1.2 END

;;; Exercise 1.3 START

(defun sum-of-two-larger-square (x y z)
  (cond ((and (< x y) (< x z)) (sum-of-squares y z))
	((and (< y x) (< y z)) (sum-of-squares x z))
	(t (sum-of-squares x y))))

;;; Exercise 1.3 END

;;; Exercise 1.4 START

;;; The thing in the operator position is also evaluated, and if b is positive,
;;; it will be addition, otherwise it will be subtraction. Thus the result is
;;; a + |b|.

;;; Exercise 1.4 END

;;; Exercise 1.5 START

;;; P is infinite recursion (well, at least while the stack holds), and TEST
;;; returns 0 (not using its second parameter) if the first parameter is 0.
;;; So in applicative order, where the parameters are evaluated first, we will
;;; have infinite recursion, and with normal evaluation it will return 0.

;;; Exercise 1.5 END


;;; Section 1.1.7

(defun average (x y)
  (/ (+ x y) 2))

(defun improve (guess x)
  (average guess (/ x guess)))

(defun good-enough-p (guess x)
  (< (abs (- (square guess) x)) 0.001d0))

(defun sqrt-iter (guess x)
  (if (good-enough-p guess x)
      guess
      (sqrt-iter (improve guess x)
		 x)))

(defun %sqrt (x)
  (sqrt-iter 1.0d0 x))

;;; Exercise 1.6 START

;;; Because of the applicative order evaluation, the parameters of NEW-IF are
;;; evaluated before the actual test. Since in SQRT-ITER one of the parameters
;;; is a recursive call, the program would be stuck in infinite recursion.

;;; Exercise 1.6 END

;;; Exercise 1.7 START

;;; Small numbers:
;;;   For 0.00001, it returns 0.03135649010502881, while a much better
;;;   approximation would be: 0.0031622776601683794
;;;   This is because although the change is small, it is quite big
;;;   relative to the number in question.

;;; Big numbers:
;;;   For very large numbers, e.g. 10^100, the change in every guess
;;;   may be larger than 0.001, even though the guess already reached
;;;   an acceptable value.

(defun effective-good-enough-p (guess old)
  (< (abs (/ (- guess old) guess)) 0.001d0))

(defun effective-sqrt-iter (guess x old-guess)
  (if (effective-good-enough-p guess old-guess)
      guess
      (effective-sqrt-iter (improve guess x) x guess)))

(defun effective-sqrt (x)
  (effective-sqrt-iter 1.0d0 x 0.0d0))

;;; With these functions:
;;; 0.00001 => 0.0031622776602038957
;;; 10^100  => 1.0000000000002003 * 10^50

;;; Exercise 1.7 END

;;; Exercise 1.8 START

(defun cube-improve (guess x)
  (/ (+ (/ x (square guess)) (* 2.0d0 guess)) 3.0d0))

(defun cube-root-iter (guess x old-guess)
  (if (effective-good-enough-p guess old-guess)
      guess
      (cube-root-iter (cube-improve guess x) x guess)))

(defun cube-root (x)
  (cube-root-iter 1.0 x 0.0))

;;; Exercise 1.8 END


;;; Section 1.1.8

(defun %double (x) (+ x x))

(defun square-2 (x)
  "Using EXP."
  (exp (%double (log x))))

(defun sqrt-2 (x)
  "With nested functions."
  (labels ((improve (guess)
	     (average guess (/ x guess)))
	   (good-enough-p (guess)
	     (< (abs (- (square guess) x)) 0.001d0))
	   (sqrt-iter (guess)
	     (if (good-enough-p guess)
		 guess
		 (sqrt-iter (improve guess)))))
    (sqrt-iter 1.0d0)))


;;; Section 1.2.1

(defun factorial (n)
  (if (= n 1)
      1
      (* n (factorial (- n 1)))))

(defun fact-iter (product counter max-count)
  (if (> counter max-count)
      product
      (fact-iter (* counter product) (+ counter 1) max-count)))

(defun factorial-2 (n)
  "With iteration."
  (fact-iter 1 1 n))

;;; Exercise 1.9 START

;;; First function (recursive):
;; (+ 4 5)
;; (inc (+ 3 5))
;; (inc (inc (+ 2 5)))
;; (inc (inc (inc (+ 1 5))))
;; (inc (inc (inc (inc (+ 0 5)))))
;; (inc (inc (inc (inc 5))))
;; (inc (inc (inc 6)))
;; (inc (inc 7))
;; (inc 8)
;; 9

;;; Second function (iterative):
;; (+ 4 5)
;; (+ 3 6)
;; (+ 2 7)
;; (+ 1 8)
;; (+ 0 9)
;; 9

;;; Exercise 1.9 END

;;; Exercise 1.10 START

;;; => 1024
;;; => 65536
;;; => 65536

;;; f(n): 2n

;;;        / 2^n, if n /= 0
;;; g(n): <
;;;        \  0 , if n = 0

;;;        / 2^(2^(2^...2)...), if n /= 0   (n 2s)
;;; h(n): <
;;;        \        0         , if n = 0

;;; Exercise 1.10 END


;;; Section 1.2.2

(defun fib (n)
  (cond ((= n 0) 0)
	((= n 1) 1)
	(t (+ (fib (- n 1))
	      (fib (- n 2))))))

(defun fib-iter (a b count)
  (if (= count 0)
      b
      (fib-iter (+ a b) a (- count 1))))

(defun fib-2 (n)
  "With iteration."
  (fib-iter 1 0 n))

(defun first-denomination (kinds-of-coins)
  (cond ((= kinds-of-coins 1) 1)
	((= kinds-of-coins 2) 5)
	((= kinds-of-coins 3) 10)
	((= kinds-of-coins 4) 25)
	((= kinds-of-coins 5) 50)))
(defun cc (amount kinds-of-coins)
  (cond ((= amount 0) 1)
	((or (< amount 0) (= kinds-of-coins 0)) 0)
	(t (+ (cc amount
		  (- kinds-of-coins 1))
	      (cc (- amount
		     (first-denomination kinds-of-coins))
		  kinds-of-coins)))))
(defun count-change (amount)
  (cc amount 5))

;;; Exercise 1.11 START

;;; Recursive:
(defun f (n)
  (if (< n 3)
      n
      (+ (f (- n 1))
	 (* 2 (f (- n 2)))
	 (* 3 (f (- n 3))))))

;;; Iterative:
(defun f-iter (a b c count)
  (if (= count 0)
      a
      (f-iter (+ a (* 2 b) (* 3 c)) a b (- count 1))))
(defun f-2 (n)
  "With iteration."
  (f-iter 2 1 0 (- n 2)))

;;; Exercise 1.11 END

;;; Exercise 1.12 START

(defun pascals-triangle (n k)
  (if (or (= k 0) (= k n))
      1
      (+ (pascals-triangle (- n 1) (- k 1))
	 (pascals-triangle (- n 1) k))))

;;; Exercise 1.12 END

;;; Exercise 1.13 START

;;; It is enough to prove that Fib(n) = (phi^n - psi^n) / sqrt(5), because
;;; psi^n / sqrt(5) < 0.5 for every n.

;;; n = 0: (phi^0 - psi^0) / sqrt(5) = (1 - 1) / sqrt(5) = 0   OK
;;; n = 1: (phi^1 - psi^1) / sqrt(5) = sqrt(5) / sqrt(5) = 1   OK

;;; Induction: we know that it works for n-1 and n-2.
;;; Fib(n) = Fib(n-1) + Fib(n-2) =
;;; (phi^(n-1) - psi^(n-1)) / sqrt(5) + (phi^(n-2) - psi^(n-2)) / sqrt(5) =
;;; 1/sqrt(5) * ((1 + phi) * phi^(n-2) - (1 + psi) * psi^(n-2)).

;;; Since phi^2 = 1 + phi, psi^2 = 1 + psi, this reduces to
;;; 1/sqrt(5) * (phi^n - psi^n), which is what we wanted to prove.

;;; Exercise 1.13 END


;;; Section 1.2.3

;;; Exercise 1.14 START

;;                                                                                        ,(11 5)
;;                                                                                       /   |
;;                                                                                      /    |
;;                                                                               ,(11 4)  (-39 5)
;;                                                                              /   |
;;                                                                             /    |
;;                                                                      ,(11 3)  (-14 5)
;;                                            _________________________/   |
;;                                           /                             |
;;                                    ,(11 2)                           ,(1  3)
;;                 __________________/   |                             /   |
;;                /                      |                            /    |
;;         ,(11 1)                    ,(6  2)                  ,(1  2)   (-9 3)
;;        /   |                      /   |                      /   |
;;       /    |                     /    |                     /    |
;; (11 0)  ,(10 1)           ,(6  1)     |              ,(1  1)   (-4 2)
;;        /   |             /   |        |             /   |
;;       /    |            /    |        |            /    |
;; (10 0)  ,(9  1)   (6  0)  ,(5  1)     |      (1  0)   (0  1)
;;        /   |             /   |        |
;;       /    |            /    |        |
;; (9  0)  ,(8  1)   (5  0)  ,(4  1)     |
;;        /   |             /   |        |
;;       /    |            /    |        |
;; (8  0)  ,(7  1)   (4  0)  ,(3  1)     |
;;        /   |             /   |        |
;;       /    |            /    |        |
;; (7  0)  ,(6  1)   (3  0)  ,(2  1)     |
;;        /   |             /   |        |
;;       /    |            /    |        |
;; (6  0)  ,(5  1)   (2  0)  ,(1  1)     |
;;        /   |             /   |        |
;;       /    |            /    |        |
;; (5  0)  ,(4  1)   (1  0)   (0  1)  ,(1  2)
;;        /   |                      /   |
;;       /    |                     /    |
;; (4  0)  ,(3  1)           ,(1  1)   (-4 2)
;;        /   |             /   |
;;       /    |            /    |
;; (3  0)  ,(2  1)   (1  0)   (0  1)
;;        /   |
;;       /    |
;; (2  0)  ,(1  1)
;;        /   |
;;       /    |
;; (1  0)   (0  1)

;;; Order of space growth: theta(n)
;;; Order of number of steps: theta(n^5)

;;; Exercise 1.14 END

;;; Exercise 1.15 START

;;; 5 times.
;;; Order of space growth: theta(log a)
;;; Order of number of steps: theta(log a)

;;; Exercise 1.15 END


;;; Section 1.2.4

(defun %expt (b n)
  (if (= n 0)
      1
      (* b (%expt b (- n 1)))))

(defun expt-iter (b counter product)
  (if (= counter 0)
      product
      (expt-iter b
		 (- counter 1)
		 (* b product))))
(defun expt-2 (b n)
  "With iteration."
  (expt-iter b n 1))

(defun fast-expt (b n)
  (cond ((= n 0) 1)
	((evenp n) (square (fast-expt b (/ n 2))))
	(t (* b (fast-expt b (- n 1))))))

(defun %evenp (n)
  (= (mod n 2) 0))

;;; Exercise 1.16 START

(defun fast-expt-iter (b n a)
  (cond ((= n 0) a)
	((evenp n) (fast-expt-iter (square b) (/ n 2) a))
	(t (fast-expt-iter b (- n 1) (* b a)))))
(defun fast-expt-2 (b n)
  "With iteration."
  (fast-expt-iter b n 1))

;;; Exercise 1.16 END

;;; Exercise 1.17 START

(defun halve (even-number) (/ even-number 2))

(defun fast-* (a b)
  (cond ((= b 0) 0)
	((evenp b) (%double (fast-* a (halve b))))
	(t (+ a (fast-* a (- b 1))))))

;;; Exercise 1.17 END

;;; Exercise 1.18 START

(defun fast-*-iter (a b res)
  (cond ((= b 0) res)
	((evenp b) (fast-*-iter (%double a) (halve b) res))
	(t (fast-*-iter a (- b 1) (+ a res)))))
(defun fast-*-2 (a b)
  "With iteration."
  (fast-*-iter a b 0))

;;; Exercise 1.18 END

;;; Exercise 1.19 START

;;; T^2(a,b) = [(bp+aq)q + (bq+aq+ap)q + (bq+aq+ap)p, (bp+aq)p + (bq+aq+ap)q] =
;;;          = [((2pq+q^2) + (p^2+q^2))a + (2pq+q^2)b, (2pq+q^2)a + (p^2+q^2)b]
;;; ... which is T(a,b) for p' = p^2 + q^2 and q' = 2pq + q^2.

(defun fib-iter-2 (a b p q count)
  "A more general approach."
  (cond ((= count 0) b)
	((evenp count)
	 (fib-iter-2 a b
		     (sum-of-squares p q)
		     (+ (* 2 p q) (square q))
		     (/ count 2)))
	(t (fib-iter-2 (+ (* b q) (* a q) (* a p))
		       (+ (* b p) (* a q))
		       p q (- count 1)))))
(defun fib-3 (n)
  "Using a more general approach"
  (fib-iter-2 1 0 0 1 n))

;;; Exercise 1.19 END


;;; Section 1.2.5

(defun %gcd (a b)
  (if (= b 0)
      a
      (%gcd b (mod a b))))

;;; Exercise 1.20 START

;; (gcd 206 40)
;; (gcd 40 (mod 206 40))
;; (gcd (mod 206 40) (mod 40 (mod 206 40)))
;; (gcd (mod 40 (mod 206 40)) (mod (mod 206 40) (mod 40 (mod 206 40))))
;; (gcd (mod (mod 206 40) (mod 40 (mod 206 40)))
;;      (mod (mod 40 (mod 206 40)) (mod (mod 206 40) (mod 40 (mod 206 40)))))

;;; In every iteration, the `b' variable is evaluated, which means the MOD
;;; function is called 14 times ((+ 1 2 4 7), which is a kind of Fibonacci
;;; sequence, F(n) = F(n-1) + F(n-2) + 1), plus `a' is also evaluated at
;;; the end (4), so there were 18 evaluations.

;;; In applicative order, every function call means one evaluation, which
;;; means 4 evaluations.

;;; Exercise 1.20 END


;;; Section 1.2.6

(defun dividesp (a b)
  (= (mod b a) 0))
(defun find-divisor (n test-divisor)
  (cond ((> (square test-divisor) n) n)
	((dividesp test-divisor n) test-divisor)
	(t (find-divisor n (+ test-divisor 1)))))
(defun smallest-divisor (n)
  (find-divisor n 2))

(defun primep (n)
  (= n (smallest-divisor n)))

(defun expmod (base exp m)
  (cond ((= exp 0) 1)
	((evenp exp)
	 (mod (square (expmod base (/ exp 2) m))
	      m))
	(t (mod (* base (expmod base (- exp 1) m))
		m))))

(defun fermat-test (n)
  (flet ((try-it (a) (= (expmod a n n) a)))
    (try-it (+ 1 (random (- n 1))))))

(defun fast-primep (n times)
  (cond ((= times 0) t)
	((fermat-test n) (fast-primep n (- times 1)))
	(t nil)))

;;; Exercise 1.21 START

#+nil
(smallest-divisor 199)
;;; => 199
#+nil
(smallest-divisor 1999)
;;; => 1999
#+nil
(smallest-divisor 19999)
;;; => 7

;;; Exercise 1.21 END

;;; Exercise 1.22 START

(defun find-a-prime (min)
  (if (time (primep min))
      min
      (find-a-prime (+ min 2))))
(defun search-for-primes (minimum count)
  (when (> count 0)
    (let ((p (find-a-prime minimum)))
      (format t "~d~%" p)
      (search-for-primes (+ p 2) (1- count)))))

#+nil
(search-for-primes (1+ (expt 10 12)) 3)
;;; => 0.419s, 0.363s, 0.429s    (sum:  1.211)
#+nil
(search-for-primes (1+ (expt 10 13)) 3)
;;; => 1.22s, 1.286s, 1.766s     (sum:  4.272)
#+nil
(search-for-primes (1+ (expt 10 14)) 3)
;;; => 3.933s, 4.272s, 4.121s    (sum: 12.326)
#+nil
(search-for-primes (1+ (expt 10 15)) 3)
;;; => 11.875s, 12.338s, 12.975s (sum: 37.188)

;;; The prediction is quite good, especially at the higher end.
;;; It seems that it really runs in time proportional to the required steps.

;;; Exercise 1.22 END

;;; Exercise 1.23 START

(defun find-divisor-2 (n test-divisor)
  "Search using only 2 and odd numbers."
  (flet ((next (n) (if (= n 2) 3 (+ n 2))))
    (cond ((> (square test-divisor) n) n)
	  ((dividesp test-divisor n) test-divisor)
	  (t (find-divisor-2 n (next test-divisor))))))

;; 1000000000039: 0.257s
;; 1000000000061: 0.219s
;; 1000000000063: 0.214s

;; 10000000000037: 0.588s
;; 10000000000051: 0.611s
;; 10000000000099: 0.627s

;; 100000000000031: 1.812s
;; 100000000000067: 2.551s
;; 100000000000097: 1.89s

;; 1000000000000037: 5.897s
;; 1000000000000091: 6.563s
;; 1000000000000159: 6.439s

;;; The ratio is about 0.5.

;;; Exercise 1.23 END

;;; Exercise 1.24 START

;;; It returns in an instant for all numbers, not the expected logarithm.
;;; It may be because different types of steps take different times.

;;; Exercise 1.24 END

;;; Exercise 1.25 START

;;; She is not correct, this implementation would lead to first create a very
;;; large number and then take its remainder, which is also expensive.

;;; Exercise 1.25 END

;;; Exercise 1.26 START

;;; He evaluates every call to EXPMOD twice.

;;; Exercise 1.26 END

;;; Exercise 1.27 START

(defun thorough-fermat-test (n)
  (labels ((try-it (k)
	     (cond ((= k n) t)
		   ((= (expmod k n n) k) (try-it (1+ k)))
		   (t nil))))
    (try-it 1)))

#+nil
(mapcar #'thorough-fermat-test '(561 1105 1729 2465 2821 6601))
;;; => (T T T T T T)

;;; Exercise 1.27 END

;;; Exercise 1.28 START

(defun expmod-2 (base exp m)
  "For the Miller-Rabin test."
  (cond ((= exp 0) 1)
	((evenp exp)
	 (let* ((tmp (expmod-2 base (/ exp 2) m))
		(rem (mod (square tmp) m)))
	   (if (and (= rem 1) (/= tmp 1) (/= tmp (1- m)))
	       0
	       rem)))
	(t (mod (* base (expmod-2 base (- exp 1) m))
		m))))
(defun miller-rabin-test (n)
  (flet ((try-it (a)
	   (= (expmod-2 a (1- n) n) 1)))
    (try-it (+ 1 (random (- n 1))))))
(defun fast-primep-2 (n times)
  "Using the Miller-Rabin test."
  (cond ((= times 0) t)
	((miller-rabin-test n) (fast-primep-2 n (- times 1)))
	(t nil)))

#+nil
(mapcar (lambda (n) (fast-primep-2 n 10)) '(561 1105 1729 2465 2821 6601))
;;; => (NIL NIL NIL NIL NIL NIL)

;;; Exercise 1.28 END


;;; Section 1.3

(defun cube (x) (* x x x))


;;; Section 1.3.1

(defun sum-integers (a b)
  (if (> a b)
      0
      (+ a (sum-integers (+ a 1) b))))

(defun sum-cubes (a b)
  (if (> a b)
      0
      (+ (cube a) (sum-cubes (+ a 1) b))))

(defun pi-sum (a b)
  (if (> a b)
      0
      (+ (/ 1.0d0 (* a (+ a 2)))
	 (pi-sum (+ a 4) b))))

(defun sum (term a next b)
  (if (> a b)
      0
      (+ (funcall term a)
	 (sum term (funcall next a) next b))))

(defun inc (n) (+ n 1))
(defun sum-cubes-2 (a b)
  "Using SUM."
  (sum #'cube a #'inc b))

(defun %identity (x) x)

(defun sum-integers-2 (a b)
  "Using SUM."
  (sum #'identity a #'inc b))

(defun pi-sum-2 (a b)
  "Using SUM."
  (flet ((pi-term (x) (/ 1.0d0 (* x (+ x 2))))
	 (pi-next (x) (+ x 4)))
    (sum #'pi-term a #'pi-next b)))

(defun integral (f a b dx)
  (flet ((add-dx (x) (+ x dx)))
    (* (sum f (+ a (/ dx 2.0d0)) #'add-dx b)
     dx)))

;;; Exercise 1.29 START

(defun simpson-integral (f a b n)
  (let ((h (/ (- b a) n)))
    (* (/ h 3.0d0)
       (loop
	  for k from 0 to n
	  for yk = (funcall f (+ a (* k h)))
	  sum (* (cond ((or (= k 0) (= k n)) 1)
		       ((oddp k) 4.0d0)
		       (t 2.0d0))
		 yk)))))

#+nil
(simpson-integral #'cube 0.0d0 1.0d0 100)
;;; => 0.25

;;; Exercise 1.29 END

;;; Exercise 1.30 START

(defun sum-2 (term a next b)
  "Iterative version."
  (labels ((it (a result)
	     (if (> a b)
		 result
		 (it (funcall next a)
		     (+ result (funcall term a))))))
    (it a 0.0d0)))

;;; Exercise 1.30 END

;;; Exercise 1.31 START

(defun product (term a next b)
  (if (> a b)
      1.0d0
      (* (funcall term a)
	 (product term (funcall next a) next b))))

(defun factorial-3 (n)
  "Using PRODUCT."
  (product #'identity 1 #'1+ n))

(defun pi-4 (n)
  (* (product (lambda (x) (/ (* 2 x) (1+ (* 2 x)))) 1 #'1+ (floor n 2))
     (product (lambda (x) (/ (* 2 x) (1- (* 2 x)))) 2 #'1+ (floor n 2))))

(defun product-iter (term a next b)
  (labels ((it (a result)
	     (if (> a b)
		 result
		 (it (funcall next a)
		     (* result (funcall term a))))))
    (it a 1.0d0)))

;;; Exercise 1.31 END

;;; Exercise 1.32 START

(defun accumulate (combiner null-value term a next b)
  (if (> a b)
      null-value
      (funcall combiner
	       (funcall term a)
	       (accumulate combiner null-value term (funcall next a) next b))))

(defun sum-3 (term a next b)
  "Using ACCUMULATE."
  (accumulate #'+ 0.0d0 term a next b))

(defun product-2 (term a next b)
  "Using ACCUMULATE."
  (accumulate #'* 0.0d0 term a next b))

(defun accumulate-iter (combiner null-value term a next b)
  (labels ((it (a result)
	     (if (> a b)
		 result
		 (it (funcall next a)
		     (funcall combiner result (funcall term a))))))
    (it a null-value)))

;;; Exercise 1.32 END

;;; Exercise 1.33 START

(defun filtered-accumulate (combiner null-value filter term a next b)
  (labels ((it (a result)
	     (cond ((> a b) result)
		   ((funcall filter a)
		    (it (funcall next a)
			(funcall combiner result (funcall term a))))
		   (t (it (funcall next a) result)))))
    (it a null-value)))

(defun sum-of-squares-2 (a b)
  "Using FILTERED-ACCUMULATE."
  (filtered-accumulate #'+ 0 #'primep #'identity a #'1+ b))

(defun sum-of-relative-primes (n)
  (filtered-accumulate #'+ 0 (lambda (k) (= (gcd n k) 1)) #'identity 1 #'1+ n))

;;; Exercise 1.33 END


;;; Section 1.3.2

(defun pi-sum-3 (a b)
  "Using LAMBDA."
  (sum (lambda (x) (/ 1.0 (* x (+ x 2))))
       a
       (lambda (x) (+ x 4))
       b))

(defun integral-2 (f a b dx)
  "Using LAMDBA."
  (* (sum f
	  (+ a (/ dx 2.0d0))
	  (lambda (x) (+ x dx)) b)
     dx))

(defmacro define (name &body body)
  "Put the function returned by BODY into NAME."
  `(progn (defun ,name () nil)
	  (setf (symbol-function ',name) (progn ,@body))))

(define plus4 (lambda (x) (+ x 4)))

(defun f-3 (x y)
  "Another `noname' function."
  (flet ((f-helper (a b)
	   (+ (* x (square a))
	      (* y b)
	      (* a b))))
    (f-helper (+ 1 (* x y))
	      (- 1 y))))

(defun f-4 (x y)
  "Like F-3, but with LAMBDA."
  (funcall (lambda (a b)
	     (+ (* x (square a))
		(* y b)
		(* a b)))
	   (+ 1 (* x y))
	   (- 1 y)))

(defun f-5 (x y)
  "Like F-4, but with LET."
  (let ((a (+ 1 (* x y)))
	(b (- 1 y)))
    (+ (* x (square a))
      (* y b)
      (* a b))))

;;; Exercise 1.34 START

;;; An error would be signaled, because 2 is not a function.

;;; Exercise 1.34 END


;;; Section 1.3.3

(defun close-enough-p (x y)
  (< (abs (- x y)) 0.001))

(defun %search (f neg-point pos-point)
  (let ((midpoint (average neg-point pos-point)))
    (if (close-enough-p neg-point pos-point)
	midpoint
	(let ((test-value (funcall f midpoint)))
	  (cond ((> test-value 0)
		 (%search f neg-point midpoint))
		((< test-value 0)
		 (%search f midpoint pos-point))
		(t midpoint))))))

(defun half-interval-method (f a b)
  (let ((a-value (funcall f a))
	(b-value (funcall f b)))
    (cond ((and (< a-value 0) (> b-value 0))
	   (%search f a b))
	  ((and (< b-value 0) (> a-value 0))
	   (%search f b a))
	  (t (error "Values are not of opposite sign: ~d, ~d" a b)))))

(defparameter *tolerance* 0.00001)
(defun fixed-point (f first-guess)
  (labels ((close-enough-p (v1 v2) (< (abs (- v1 v2)) *tolerance*))
	   (try (guess)
	     (let ((next (funcall f guess)))
	       (if (close-enough-p guess next)
		   next
		   (try next)))))
    (try first-guess)))

(defun sqrt-3 (x)
  "Using FIXED-POINT."
  (fixed-point (lambda (y) (/ x y)) 1.0d0))

(defun sqrt-4 (x)
  "Using FIXED-POINT with average damping."
  (fixed-point (lambda (y) (average y (/ x y))) 1.0d0))

;;; Exercise 1.35 START

;;; 1 + 1/phi = 1 + 2 / (1 + sqrt(5)) = (3 + sqrt(5)) / (1 + sqrt(5)) =
;;; = (3 + sqrt(5)) * (1 - sqrt(5)) / -4 = (2 + 2 * sqrt(5)) / 4 = phi, Q.E.D.

#+nil
(fixed-point (lambda (x) (1+ (/ x))) 1.0d0)
;;; => 1.6180327868852458

;;; Exercise 1.35 END

;;; Exercise 1.36 START

(defun fixed-point-2 (f first-guess)
  "Printing the approximations."
  (labels ((close-enough-p (v1 v2)
	     (< (abs (- v1 v2)) *tolerance*))
	   (try (guess)
	     (format t "Trying ~a...~%" guess)
	     (let ((next (funcall f guess)))
	       (if (close-enough-p guess next)
		   next
		   (try next)))))
    (try first-guess)))

#+nil
(fixed-point-2 (lambda (x) (log 1000 x)) 2.0d0)
;;; => 4.55554095103843, 34 steps
#+nil
(fixed-point-2 (lambda (x) (average x (log 1000 x))) 2.0d0)
;;; => 4.555546590269705, 9 steps

;;; Exercise 1.36 END

;;; Exercise 1.37 START

(defun cont-frac (n d k)
  (labels ((rec (i)
	     (if (> i k)
		 0.0d0
		 (/ (funcall n i)
		    (+ (funcall d i) (rec (1+ i)))))))
    (rec 1)))

#+nil
(/ 2.0d0 (1+ (sqrt 5.0d0)))
;;; => 0.6180339887498948

#+nil
(cont-frac (lambda (i)
	     (declare (ignore i))
	     1.0)
	   (lambda (i)
	     (declare (ignore i))
	     1.0)
	   11)
;;; => 0.6180555555555556

(defun cont-frac-iter (n d k)
  (labels ((it (i result)
	     (if (= i 0)
		 result
		 (it (1- i) (/ (funcall n i)
			       (+ (funcall d i) result))))))
    (it k 0.0d0)))

;;; Exercise 1.37 END

;;; Exercise 1.38 START

#+nil
(+ 2 (cont-frac (lambda (i)
		  (declare (ignore i))
		  1.0d0)
		(lambda (i)
		  (if (= (mod i 3) 2)
		      (* (/ (1+ i) 3) 2.0d0)
		      1.0d0))
		100))
;;; => 2.7182818284590455

;;; Exercise 1.38 END

;;; Exercise 1.39 START

(defun tan-cf (x k)
  (cont-frac (lambda (i) (if (= i 1) x (* -1.0d0 x x)))
	     (lambda (i) (1- (* 2 i)))
	     k))

;;; Exercise 1.39 END


;;; Section 1.3.4

(defun average-damp (f)
  (lambda (x) (average x (funcall f x))))

(defun sqrt-5 (x)
  "Using FIXED-POINT and AVERAGE-DAMP."
  (fixed-point (average-damp (lambda (y) (/ x y)))
	       1.0d0))

(defun cube-root-2 (x)
  "Using FIXED-POINT and AVERAGE-DAMP."
  (fixed-point (average-damp (lambda (y) (/ x (square y))))
	       1.0d0))

(defparameter *dx* 0.00001)

(defun deriv (g)
  (lambda (x)
    (/ (- (funcall g (+ x *dx*)) (funcall g x))
       *dx*)))

(defun newton-transform (g)
  (lambda (x)
    (- x (/ (funcall g x) (funcall (deriv g) x)))))
(defun newtons-method (g guess)
  (fixed-point (newton-transform g) guess))

(defun sqrt-6 (x)
  "Using NEWTONS-METHOD."
  (newtons-method (lambda (y) (- (square y) x))
		  1.0d0))

(defun fixed-point-of-transform (g transform guess)
  (fixed-point (funcall transform g) guess))

(defun sqrt-7 (x)
  "Using FIXED-POINT-OF-TRANSFORM and AVERAGE-DAMP."
  (fixed-point-of-transform (lambda (y) (/ x y))
			    #'average-damp
			    1.0d0))

(defun sqrt-8 (x)
  "Using FIXED-POINT-OF-TRANSFORM and NEWTONS-METHOD."
  (fixed-point-of-transform (lambda (y) (- (square y) x))
			    #'newton-transform
			    1.0d0))

;;; Exercise 1.40 START

(defun cubic (a b c)
  (lambda (x)
    (+ (* x x x) (* a x x) (* b x) c)))

;;; Exercise 1.40 END

;;; Exercise 1.41 START

(defun double-2 (f)
  "Calls a function twice."
  (lambda (x) (funcall f (funcall f x))))

#+nil
(funcall (funcall (double-2 (double-2 #'double-2)) #'1+) 5)
;;; => 21

;;; Exercise 1.41 END

;;; Exercise 1.42 START

(defun compose (f g)
  (lambda (x) (funcall f (funcall g x))))

;;; Exercise 1.42 END

;;; Exercise 1.43 START

(defun repeated (f n)
  (if (= n 0)
      #'identity
      (compose f (lambda (x) (funcall (repeated f (1- n)) x)))))

;;; Exercise 1.43 END

;;; Exercise 1.44 START

(defun smooth (f)
  (lambda (x)
    (/ (+ (funcall f (- x *dx*)) (funcall f x) (funcall f (+ x *dx*)))
       3)))

(defun repeatedly-smooth (f n)
  (funcall (repeated #'smooth n) f))

;;; Exercise 1.44 END

;;; Exercise 1.45 START

(defun nth-root-test (x n k)
  "Nth root with average-damping K times (for testing)."
  (fixed-point (funcall (repeated #'average-damp k)
			(lambda (y) (/ x (expt y (1- n)))))
	       1.0d0))

;;; ((2 1) (3 1) (4 2) (5 2) (6 2) (7 2) (8 3) (9 3) (10 3) ... (15 3) (16 4))
;;; It seems like we need to call the damping (floor (log n 2)) times.

(defun nth-root (x n)
  (fixed-point (funcall (repeated #'average-damp (floor (log n 2)))
			(lambda (y) (/ x (expt y (1- n)))))
	       1.0d0))

;;; Exercise 1.45 END

;;; Exercise 1.46 START

(defun iterative-improve (good-enough-p improve)
  (lambda (guess)
    (labels ((rec (guess)
	       (if (funcall good-enough-p guess)
		   guess
		   (rec (funcall improve guess)))))
      (rec guess))))

(defun sqrt-9 (x)
  "Using ITERATIVE-IMPROVE."
  (funcall
   (iterative-improve (lambda (guess) (< (abs (- (square guess) x)) 0.001d0))
		      (lambda (guess) (average guess (/ x guess))))
   x))

;;; This implementation executes F twice as much than required.
(defun fixed-point-3 (f first-guess)
  "Using ITERATIVE-IMPROVE."
  (funcall
   (iterative-improve (lambda (guess)
			(< (abs (- guess (funcall f guess)))
			   *tolerance*))
		      f)
   first-guess))

;;; Exercise 1.46 END


;;; Section 2.1.1

(defparameter *x* (cons 1 2))
(defparameter *y* (cons 3 4))
(defparameter *z* (cons *x* *y*))

(defun make-rat (n d) (cons n d))

(defun numer (x) (car x))

(defun denom (x) (cdr x))

(defun print-rat (x)
  (format t "~d/~d~%" (numer x) (denom x)))

(defun add-rat (x y)
  (make-rat (+ (* (numer x) (denom y))
	       (* (numer y) (denom x)))
	    (* (denom x) (denom y))))
(defun sub-rat (x y)
  (make-rat (- (* (numer x) (denom y))
	       (* (numer y) (denom x)))
	    (* (denom x) (denom y))))
(defun mul-rat (x y)
  (make-rat (* (numer x) (numer y))
	    (* (denom x) (denom y))))
(defun div-rat (x y)
  (make-rat (* (numer x) (denom y))
	    (* (denom x) (numer y))))
(defun equal-rat-p (x y)
  (= (* (numer x) (denom y))
     (* (numer y) (denom x))))

(defparameter *one-half* (make-rat 1 2))
(defparameter *one-third* (make-rat 1 3))

(defun make-rat-2 (n d)
  "Simplifies the rational."
  (let ((g (gcd n d)))
    (cons (/ n g) (/ d g))))

;;; Exercise 2.1 START

(defun make-rat-3 (n d)
  "Simplifies the rational and puts the sign on the numerator."
  (let ((g (gcd n d))
	(sign (if (< (* n d) 0) -1 1)))
    (cons (* sign (/ (abs n) g)) (/ (abs d) g))))

;;; Exercise 2.1 END

;;; Section 2.1.2

(defun make-rat-4 (n d)
  "Just like MAKE-RAT."
  (cons n d))
(defun numer-2 (x)
  "Returns the numerator of the simplified rational"
  (let ((g (gcd (car x) (cdr x))))
    (/ (car x) g)))
(defun denom-2 (x)
  "Returns the denominator of the simplified rational"
  (let ((g (gcd (car x) (cdr x))))
    (/ (cdr x) g)))

;;; Exercise 2.2 START

(defclass point ()
  ((x :initarg :x :accessor x-point)
   (y :initarg :y :accessor y-point)))
(defun make-point (x y)
  (make-instance 'point :x x :y y))
(defmethod print-object ((p point) stream)
  (format stream "(~a,~a)" (x-point p) (y-point p)))

(defclass segment ()
  ((start :initarg :start :accessor start-segment)
   (end :initarg :end :accessor end-segment)))
(defun make-segment (start end)
  (make-instance 'segment :start start :end end))

(defun midpoint-segment (segment)
  (let ((start (start-segment segment))
	(end (end-segment segment)))
    (make-point (/ (+ (x-point start) (x-point end)) 2)
		(/ (+ (y-point start) (y-point end)) 2))))

;;; Exercise 2.2 END

;;; Exercise 2.3 START

(defun segment-length (segment)
  (let ((start (start-segment segment))
	(end (end-segment segment)))
    (sqrt (+ (square (- (x-point end) (x-point start)))
	     (square (- (y-point end) (y-point start)))))))

;;; First representation (segments)
(defstruct rectangle
  "The two segments LEFT and TOP must share a point."
  left top)

(defun rectangle-length-u (rectangle)
  (segment-length (rectangle-top rectangle)))

(defun rectangle-length-v (rectangle)
  (segment-length (rectangle-left rectangle)))

;;; Second representation (points)
(defstruct rectangle-2
  "LEFT and RIGHT are the endpoints of the base line.
The other two endpoints are calculated by going `up' from RIGHT in
anti-clockwise direction HEIGHT units."
  left right height)

(defun rectangle-2-length-u (rectangle)
  (segment-length (make-segment (rectangle-2-right rectangle)
				(rectangle-2-left rectangle))))

(defun rectangle-2-length-v (rectangle)
  (rectangle-2-height rectangle))

;;; Perimeter & area
(defun perimeter (rectangle)
  (* (+ (rectangle-length-u rectangle)
	(rectangle-length-v rectangle))
     2))

(defun area (rectangle)
  (* (rectangle-length-u rectangle)
     (rectangle-length-v rectangle)))

;;; Exercise 2.3 END


;;; Section 2.1.3

(defun %cons (x y)
  (flet ((dispatch (m)
	   (cond ((= m 0) x)
		 ((= m 1) y)
		 (t (error "Argument is ~a, not 0 or 1 -- %CONS" m)))))
    #'dispatch))

(defun %car (z)
  (funcall z 0))

(defun %cdr (z)
  (funcall z 1))

;;; Exercise 2.4 START

;;; CONS creates a function, that takes a function with two parameters as its
;;; argument, and calls the function on the original arguments to CONS.
;;; Since CAR calls this with the function that returns the first of its two
;;; arguments, the result is correct.

(defun cons-2 (x y)
  "A different representation."
  (lambda (m) (funcall m x y)))

(defun car-2 (z)
  "A different representation."
  (funcall z (lambda (p q) (declare (ignore q)) p)))

(defun cdr-2 (z)
  "A different representation."
  (funcall z (lambda (p q) (declare (ignore p)) q)))

;;; Exercise 2.4 END

;;; Exercise 2.5 START

;;; The factorization of these numbers is unequivocal.

(defun cons-3 (a b)
  "A different representation."
  (* (expt 2 a) (expt 3 b)))

(defun car-3 (z)
  "A different representation."
  (if (= (mod z 2) 0)
      (1+ (car-3 (/ z 2)))
      0))

(defun cdr-3 (z)
  "A different representation."
  (if (= (mod z 3) 0)
      (1+ (cdr-3 (/ z 3)))
      0))

;;; Exercise 2.5 END

;;; Exercise 2.6 START

(defun print-number (n)
  "For testing."
  (funcall (funcall n #'1+) 0))

(defun zero (f)
  (declare (ignore f))
  (lambda (x)
    x))

(defun one (f)
  (lambda (x)
    (funcall f x)))

(defun two (f)
  (lambda (x)
    (funcall f (funcall f x))))

(defun %+ (a b)
  (lambda (f)
    (lambda (x)
      (funcall (funcall a f) (funcall (funcall b f) x)))))

(defun %* (a b)
  (lambda (f)
    (lambda (x)
      (funcall (funcall a (funcall b f)) x))))

;;; Exercise 2.6 END


;;; Section 2.1.4

;;; Exercise 2.7 START

(defun make-interval (a b)
  (cons a b))

(defun lower-bound (interval)
  (car interval))

(defun upper-bound (interval)
  (cdr interval))

;;; Exercise 2.7 END

(defun add-interval (x y)
  (make-interval (+ (lower-bound x) (lower-bound y))
		 (+ (upper-bound x) (upper-bound y))))

(defun mul-interval (x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
	(p2 (* (lower-bound x) (upper-bound y)))
	(p3 (* (upper-bound x) (lower-bound y)))
	(p4 (* (upper-bound x) (upper-bound y))))
    (make-interval (min p1 p2 p3 p4)
		   (max p1 p2 p3 p4))))

(defun div-interval (x y)
  (mul-interval x
		(make-interval (/ 1.0d0 (upper-bound y))
			       (/ 1.0d0 (lower-bound y)))))

(defun make-center-width (c w)
  (make-interval (- c w) (+ c w)))
(defun center (i)
  (/ (+ (lower-bound i) (upper-bound i)) 2.0d0))
(defun width (i)
  (/ (- (upper-bound i) (lower-bound i)) 2.0d0))

(defun par1 (r1 r2)
  (div-interval (mul-interval r1 r2)
		(add-interval r1 r2)))
(defun par2 (r1 r2)
  (let ((one (make-interval 1 1)))
    (div-interval one
		  (add-interval (div-interval one r1)
				(div-interval one r2)))))

;;; Exercise 2.8 START

;;; The difference of two intervals is the same as the sum of the first and
;;; the inverse of the second.
;;; Intervals can be negated by taking their negative bounds in reverse order.

(defun negate-interval (x)
  (make-interval (- (upper-bound x)) (- (lower-bound x))))

(defun sub-interval (x y)
  (add-interval x (negate-interval y)))

;;; Exercise 2.8 END

;;; Exercise 2.9 START

;;; Sum/Difference
;;; Since negation doesn't change the width, we can treat these as one.
;;; (Note: that's why the interval gets larger with difference as well)
;;; (AL, AU) + (BL, BU) = (AL+BL, AU+BU)
;;; width(sum) = (AU+BU - AL+BL)/2 = (AU-AL)/2 + (BU-BL)/2 = width(A)+width(B)

;;; Multiplication
;;; (1, 2) * (1, 3) = (1, 6) => 5/2
;;; (2, 3) * (1, 3) = (2, 9) => 7/2
;;; ... but (1, 2) and (2, 3) have the same width.

;;; Division
;;; (2, 4) / (1, 2) = (1, 4) => 3/2
;;; (4, 6) / (1, 2) = (2, 6) =>  2
;;; ... but (2, 4) and (4, 6) have the same width.

;;; Exercise 2.9 END

;;; Exercise 2.10 START

(defun div-interval-2 (x y)
  "Error if the divisor interval spans 0."
  (assert (not (and (<= (lower-bound y) 0) (>= (upper-bound y) 0))) (y)
	  "Division by an interval that spans 0.")
  (mul-interval x
		(make-interval (/ 1.0d0 (upper-bound y))
			       (/ 1.0d0 (lower-bound y)))))

;;; Exercise 2.10 END

;;; Exercise 2.11 START

(defun mul-interval-2 (x y)
  "A more efficient multiplication."
  (let ((a (lower-bound x))
	(b (upper-bound x))
	(c (lower-bound y))
	(d (upper-bound y)))
    (cond ((and (>= a 0) (>= b 0) (>= c 0) (>= d 0))
	   (make-interval (* a c) (* b d)))
	  ((and (>= a 0) (>= b 0) (< c 0) (>= d 0))
	   (make-interval (* a c) (* b d)))
	  ((and (>= a 0) (>= b 0) (< c 0) (< d 0))
	   (make-interval (* a d) (* b c)))
	  ((and (< a 0) (>= b 0) (>= c 0) (>= d 0))
	   (make-interval (* a c) (* b d)))
	  ((and (< a 0) (>= b 0) (< c 0) (>= d 0))
	   (make-interval (min (* a d) (* b c)) (max (* a c) (* b d))))
	  ((and (< a 0) (>= b 0) (< c 0) (< d 0))
	   (make-interval (* b c) (* a c)))
	  ((and (< a 0) (< b 0) (>= c 0) (>= d 0))
	   (make-interval (* a d) (* b c)))
	  ((and (< a 0) (< b 0) (< c 0) (>= d 0))
	   (make-interval (* a d) (* a c)))
	  ((and (< a 0) (< b 0) (< c 0) (< d 0))
	   (make-interval (* b d) (* a c))))))

;;; Exercise 2.11 END

;;; Exercise 2.12 START

(defun make-center-percent (c p)
  (make-interval (- c (* c p 0.01d0)) (+ c (* c p 0.01d0))))
(defun percent (i)
  (* (/ (width i) (center i)) 100.0d0))

;;; Exercise 2.12 END

;;; Exercise 2.13 START

;;; (AC, AT) * (BC, BT) =
;;; = ((AC - AC*AT) * (BC - BC*BT), (AC + AC*AT) * (BC + BC*BT)) =
;;; = ((AC*AB * (1 - AT - BT + AT*BT)), (AC*AB * (1 + AT + BT + AT*BT)))
;;; We can say that AT*BT is small, and then the tolerance is AT + BT.

;;; Exercise 2.13 END

;;; Exercise 2.14 START

(defun print-interval (i)
  "For testing."
  (format t "~a +/- ~a~%" (center i) (percent i)))

#+nil
(print-interval (par1 (make-center-percent 10 1) (make-center-percent 20 2)))
;;; => 6.673186996387885d0 +/- 4.6637353852303285
#+nil
(print-interval (par2 (make-center-percent 10 1) (make-center-percent 20 2)))
;;; => 6.6665184773548205d0 +/- 1.3334000200059877

;;; A/A is not simply 1 in this context.

;;; Exercise 2.14 END

;;; Exercise 2.15 START

;;; Yes, she's right, because every appearance of a variable means
;;; an uncertainty factor.

;;; Exercise 2.15 END

;;; Exercise 2.16 START

;;; Not only the number of variable appearances, but also the order of
;;; the operations is an important factor. Since different operations have
;;; different effect on the width, such an interval-arithmetic package is
;;; infeasible.

;;; Exercise 2.16 END


;;; Section 2.2.1

(defparameter *one-through-four* (list 1 2 3 4))

(defun list-ref (items n)
  (if (= n 0)
      (car items)
      (list-ref (cdr items) (- n 1))))
(defparameter *squares* (list 1 4 9 16 25))

(defun %length (items)
  (if (null items)
      0
      (+ 1 (%length (cdr items)))))
(defparameter *odds* (list 1 3 5 7))

(defun length-2 (items)
  "Iterative version."
  (labels ((length-iter (a count)
	     (if (null a)
		 count
		 (length-iter (cdr a) (+ 1 count)))))
    (length-iter items 0)))

(defun %append (list1 list2)
  (if (null list1)
      list2
      (cons (car list1) (%append (cdr list1) list2))))

;;; Exercise 2.17 START

(defun last-pair (items)
  (if (null (cdr items))
      items
      (last-pair (cdr items))))

;;; Exercise 2.17 END

;;; Exercise 2.18 START

(defun %reverse (items)
  (labels ((rec (lst acc)
	     (if (null lst)
		 acc
		 (rec (cdr lst) (cons (car lst) acc)))))
    (rec items nil)))

;;; Exercise 2.18 END

;;; Exercise 2.19 START

(defun first-denomination-2 (coin-values)
  "Using coin value list."
  (car coin-values))

(defun except-first-denomination-2 (coin-values)
  "Using coin value list."
  (cdr coin-values))

(defun no-more-p-2 (coin-values)
  "Using coin value list."
  (null coin-values))

(defun cc-2 (amount coin-values)
  "Using coin value list."
  (cond ((= amount 0) 1)
	((or (< amount 0) (no-more-p-2 coin-values)) 0)
	(t (+ (cc-2 amount
		    (except-first-denomination-2 coin-values))
	      (cc-2 (- amount
		       (first-denomination-2 coin-values))
		  coin-values)))))

;;; Exercise 2.19 END

;;; Exercise 2.20 START

(defun same-parity (a &rest rest)
  (let ((parity (mod a 2)))
    (labels ((rec (lst)
	       (cond ((null lst) nil)
		     ((= (mod (car lst) 2) parity)
		      (cons (car lst) (rec (cdr lst))))
		     (t (rec (cdr lst))))))
      (cons a (rec rest)))))

;;; Exercise 2.20 END

(defun scale-list (items factor)
  (if (null items)
      nil
      (cons (* (car items) factor)
	    (scale-list (cdr items) factor))))

(defun %mapcar (proc items)
  (if (null items)
      nil
      (cons (funcall proc (car items))
	    (%mapcar proc (cdr items)))))

(defun scale-list-2 (items factor)
  "Using MAPCAR."
  (mapcar (lambda (x) (* x factor)) items))

;;; Exercise 2.21 START

(defun square-list (items)
  (if (null items)
      nil
      (cons (square (car items)) (square-list (cdr items)))))
(defun square-list-2 (items)
  "Using MAPCAR."
  (mapcar (lambda (x) (* x x)) items))

;;; Exercise 2.21 END

;;; Exercise 2.22 START

;;; (a) He always places the next element on the start of the list.
;;; (b) The second argument to CONS should be a list, to construct a list.

;;; Exercise 2.22 END

;;; Exercise 2.23 START

(defun %mapc (function list)
  (unless (null list)
    (funcall function (car list))
    (%mapc function (cdr list))))

;;; Exercise 2.23 END


;;; Section 2.2.2

(defparameter *x*
  (cons (list 1 2) (list 3 4)))

(defun count-leaves (x)
  (cond ((null x) 0)
	((not (consp x)) 1)
	(t (+ (count-leaves (car x))
	      (count-leaves (cdr x))))))

;;; Exercise 2.24 START

;;; => (1 (2 (3 4)))

;; +---+---+    +---+---+
;; | * | *----->| * | / |
;; +-|-+---+    +-|-+---+
;;   |            |
;;   V            V
;; +---+        +---+---+    +---+---+
;; | 1 |        | * | *----->| * | / |
;; +---+        +-|-+---+    +-|-+---+
;;                |            |
;;                V            V
;;              +---+        +---+---+    +---+---+
;;              | 2 |        | * | *----->| * | / |
;;              +---+        +-|-+---+    +-|-+---+
;;                             |            |
;;                             V            V
;;                           +---+        +---+
;;                           | 3 |        | 4 |
;;                           +---+        +---+

;;     *
;;    / \
;;   /   \
;;  /     \
;; 1       *
;;        / \
;;       /   \
;;      /     \
;;     2       *
;;            / \
;;           /   \
;;          /     \
;;         3       4

;;; Exercise 2.24 END

;; Exercise 2.25 START

#+nil
(car (cdr (car (cdr (cdr '(1 3 (5 7) 9))))))
#+nil
(car (car '((7))))
#+nil
(let ((lst '(1 (2 (3 (4 (5 (6 7))))))))
  (car (cdr (car (cdr (car (cdr (car (cdr (car (cdr (car (cdr lst)))))))))))))

;; Exercise 2.25 END

;;; Exercise 2.26 START

;;; => (1 2 3 4 5 6)
;;; => ((1 2 3) 4 5 6)
;;; => ((1 2 3) (4 5 6))

;;; Exercise 2.26 END

;;; Exercise 2.27 START

(defun deep-reverse (lst)
  (labels ((rec (tree acc)
	     (cond ((null tree) acc)
		   ((atom tree) tree)
		   (t (rec (cdr tree) (cons (rec (car tree) nil) acc))))))
    (rec lst nil)))

;;; Exercise 2.27 END

;;; Exercise 2.28 START

(defun fringe (lst)
  (cond ((null lst) nil)
	((atom lst) (list lst))
	(t (append (fringe (car lst)) (fringe (cdr lst))))))

;;; Exercise 2.28 END

;;; Exercise 2.29 START

(defun make-mobile (left right)
  (list left right))

(defun make-branch (length structure)
  (list length structure))

(defun left-branch (mobile)
  (first mobile))
(defun right-branch (mobile)
  (second mobile))

(defun branch-length (branch)
  (first branch))
(defun branch-structure (branch)
  (second branch))

(defun mobilep (obj)
  (consp obj))

(defun total-weight (mobile)
  "Return MOBILE if it is a weight."
  (if (mobilep mobile)
      (+ (total-weight (branch-structure (left-branch mobile)))
	 (total-weight (branch-structure (right-branch mobile))))
      mobile))

(defun balancedp (mobile)
  (and (= (* (branch-length (left-branch mobile))
	     (total-weight (branch-structure (left-branch mobile))))
	  (* (branch-length (right-branch mobile))
	     (total-weight (branch-structure (right-branch mobile)))))
       (or (not (mobilep (branch-structure (left-branch mobile))))
	   (balancedp (branch-structure (left-branch mobile))))
       (or (not (mobilep (branch-structure (right-branch mobile))))
	   (balancedp (branch-structure (right-branch mobile))))))

;;; I should change RIGHT-BRANCH and BRANCH-STRUCTURE.

;;; Exercise 2.29 END

(defun scale-tree (tree factor)
  (cond ((null tree) nil)
	((not (consp tree)) (* tree factor))
	(t (cons (scale-tree (car tree) factor)
		 (scale-tree (cdr tree) factor)))))

(defun scale-tree-2 (tree factor)
  "Using MAPCAR."
  (mapcar (lambda (sub-tree)
	  (if (consp sub-tree)
	      (scale-tree-2 sub-tree factor)
	      (* sub-tree factor)))
	tree))

;;; Exercise 2.30 START

(defun square-tree (tree)
  (cond ((null tree) nil)
	((not (consp tree)) (* tree tree))
	(t (cons (square-tree (car tree))
		 (square-tree (cdr tree))))))

(defun square-tree-2 (tree)
  "Using MAPCAR."
  (mapcar (lambda (sub-tree)
	    (if (consp sub-tree)
		(square-tree-2 sub-tree)
		(* sub-tree sub-tree)))
	  tree))

;;; Exercise 2.30 END

;;; Exercise 2.31 START

(defun tree-map (function tree)
  (cond ((null tree) nil)
	((not (consp tree)) (funcall function tree))
	(t (cons (tree-map function (car tree))
		 (tree-map function (cdr tree))))))

;;; Exercise 2.31 END

;;; Exercise 2.32 START

(defun subsets (s)
  (if (null s)
      (list nil)
      (let ((rest (subsets (cdr s))))
	(append rest (mapcar (lambda (lst) (cons (car s) lst)) rest)))))

;;; For every element, we can choose whether to include it or not.
;;; If we already have the subsets of all the elements but one, then
;;; those subsets are subsets of the whole set, but they are also subset
;;; if we insert that last element to them, so the final result is
;;; (append rest (insert-last-element-to rest)).

;;; Exercise 2.32 END


;;; Section 2.2.3

(defun sum-odd-squares (tree)
  (cond ((null tree) 0)
        ((not (consp tree))
         (if (oddp tree) (square tree) 0))
        (t (+ (sum-odd-squares (car tree))
	      (sum-odd-squares (cdr tree))))))

(defun even-fibs (n)
  (labels ((next (k)
	     (if (> k n)
		 nil
		 (let ((f (fib k)))
		   (if (evenp f)
		       (cons f (next (+ k 1)))
		       (next (+ k 1)))))))
    (next 0)))

(defun filter (predicate sequence)
  (cond ((null sequence) nil)
	((funcall predicate (car sequence))
	 (cons (car sequence)
	       (filter predicate (cdr sequence))))
	(t (filter predicate (cdr sequence)))))

(defun accumulate-seq (op initial sequence)
  (if (null sequence)
      initial
      (funcall op (car sequence)
	       (accumulate-seq op initial (cdr sequence)))))

(defun enumerate-interval (low high)
  (if (> low high)
      nil
      (cons low (enumerate-interval (+ low 1) high))))

(defun enumerate-tree (tree)
  (cond ((null tree) nil)
	((not (consp tree)) (list tree))
	(t (append (enumerate-tree (car tree))
		   (enumerate-tree (cdr tree))))))

(defun sum-odd-squares-2 (tree)
  "Using ACCUMULATE-SEQ."
  (accumulate-seq #'+
		  0
		  (mapcar #'square
			  (filter #'oddp
				  (enumerate-tree tree)))))

(defun even-fibs-2 (n)
  "Using ACCUMULATE-SEQ."
  (accumulate-seq #'cons
		  nil
		  (filter #'evenp
			  (mapcar #'fib
				  (enumerate-interval 0 n)))))

(defun list-fib-squares (n)
  (accumulate-seq #'cons
		  nil
		  (mapcar #'square
			  (mapcar #'fib
				  (enumerate-interval 0 n)))))

(defun product-of-squares-of-odd-elements (sequence)
  (accumulate-seq #'*
		  1
		  (mapcar #'square
			  (filter #'oddp sequence))))

#+nil
(defun salary-of-highest-paid-programmer (records)
  (accumulate-seq #'max
		  0
		  (mapcar #'salary
			  (filter #'programmerp records))))

;;; Exercise 2.33 START

(defun mapcar-2 (p sequence)
  "Using ACCUMULATE-SEQ."
  (accumulate-seq (lambda (x y) (cons (funcall p x) y)) nil sequence))
(defun append-2 (seq1 seq2)
  "Using ACCUMULATE-SEQ."
  (accumulate-seq #'cons seq2 seq1))
(defun length-3 (sequence)
  "Using ACCUMULATE-SEQ."
  (accumulate-seq (lambda (x y) (declare (ignore x)) (1+ y)) 0 sequence))

;;; Exercise 2.33 END

;;; Exercise 2.34 START

(defun horner-eval (x coefficient-sequence)
  (accumulate-seq (lambda (this-coeff higher-terms)
		    (+ this-coeff (* higher-terms x)))
		  0
		  coefficient-sequence))

;;; Exercise 2.34 END

;;; Exercise 2.35 START

(defun count-leaves-2 (tree)
  "Using ACCUMULATE-SEQ."
  (accumulate-seq #'+ 0
		  (mapcar (lambda (x) (declare (ignore x)) 1)
			  (enumerate-tree tree))))

;;; Exercise 2.35 END

;;; Exercise 2.36 START

(defun accumulate-n (op init seqs)
  (if (null (car seqs))
      nil
      (cons (accumulate-seq op init (mapcar #'car seqs))
	    (accumulate-n op init (mapcar #'cdr seqs)))))

;;; Exercise 2.36 END

;;; Exercise 2.37 START

(defun dot-product (v w)
  (accumulate-seq #'+ 0 (mapcar #'* v w)))

(defun matrix-*-vector (m v)
  (mapcar (lambda (row) (dot-product row v)) m))
(defun transpose (mat)
  (accumulate-n #'cons nil mat))
(defun matrix-*-matrix (m n)
  (let ((cols (transpose n)))
    (mapcar (lambda (row)
	      (mapcar (lambda (col)
			(dot-product row col))
		      cols))
	    m)))

;;; Exercise 2.37 END

;;; Exercise 2.38 START

;;; => 3/2
;;; => 1/6
;;; => (1 (2 (3 NIL)))
;;; => (((NIL 1) 2) 3)

;;; FOLD-LEFT and FOLD-RIGHT are the same, if OP is associative.

;;; Exercise 2.38 END

;;; Exercise 2.39 START

(defun reverse-2 (sequence)
  "Using REDUCE from the end (FOLD-RIGHT)."
  (reduce (lambda (x y) (append y (list x)))
	  sequence :initial-value nil :from-end t))
(defun reverse-3 (sequence)
  "Using REDUCE from the start (FOLD-LEFT)."
  (reduce (lambda (x y) (cons y x))
	  sequence :initial-value nil :from-end nil))

;;; Exercise 2.39 END


(defun flatmap (proc seq)
  (accumulate-seq #'append nil (mapcar proc seq)))

(defun prime-sum-p (pair)
  (primep (+ (car pair) (cadr pair))))

(defun make-pair-sum (pair)
  (list (car pair) (cadr pair) (+ (car pair) (cadr pair))))

(defun prime-sum-pairs (n)
  (mapcar #'make-pair-sum
	  (filter #'prime-sum-p
		  (flatmap
		   (lambda (i)
		     (mapcar (lambda (j) (list i j))
			     (enumerate-interval 1 (- i 1))))
		   (enumerate-interval 1 n)))))

(defun permutations (s)
  (if (null s)
      (list nil)
      (flatmap (lambda (x)
		 (mapcar (lambda (p) (cons x p))
			 (permutations (remove x s))))
	       s)))

(defun %remove (item sequence)
  (filter (lambda (x) (not (= x item)))
	  sequence))

;;; Exercise 2.40 START

(defun unique-pairs (n)
  (flatmap (lambda (i)
	     (mapcar (lambda (j) (list i j))
		     (enumerate-interval 1 (1- i))))
	   (enumerate-interval 1 n)))

(defun prime-sum-pairs-2 (n)
  "Using UNIQUE-PAIRS."
  (mapcar #'make-pair-sum
	  (filter #'prime-sum-p
		  (unique-pairs n))))

;;; Exercise 2.40 END

;;; Exercise 2.41 START

(defun unique-triples (n)
  (mapcan (lambda (i)
	    (mapcan (lambda (j)
		      (mapcar (lambda (k) (list i j k))
			      (enumerate-interval 1 (- j 1))))
		    (enumerate-interval 1 (- i 1))))
	  (enumerate-interval 1 n)))

(defun triples-sum-to-s (n s)
  (filter (lambda (triple) (= (reduce #'+ triple) s))
	  (unique-triples n)))

;;; Exercise 2.41 END

;;; Exercise 2.42 START

(defconstant +empty-board+ nil)
(defun adjoin-position (new-row k rest-of-queens)
  (declare (ignore k))
  (cons new-row rest-of-queens))
(defun safep (k positions)
  "Check the last row."
  (declare (ignore k))
  (labels ((rec (pos distance lst)
	     (if (null lst)
		 t
		 (and (/= (abs (- pos (car lst))) distance)
		      (rec pos (1+ distance) (cdr lst))))))
    (and (not (member (car positions) (cdr positions)))
	 (rec (car positions) 1 (cdr positions)))))

(defun queens (board-size)
  (labels ((queen-cols (k)
	     (if (= k 0)
		 (list +empty-board+)
		 (filter
		  (lambda (positions) (safep k positions))
		  (mapcan
		   (lambda (rest-of-queens)
		     (mapcar (lambda (new-row)
			       (adjoin-position new-row k rest-of-queens))
			     (enumerate-interval 1 board-size)))
		   (queen-cols (- k 1)))))))
    (queen-cols board-size)))

;;; Exercise 2.42 END

;;; Exercise 2.43 START

;;; Because the inner loop is executed a lot more times, and now he has the
;;; recursion in it. It should take about T * 8! time to solve.

;;; Exercise 2.43 END


;;; Section 2.2.4

;;; Exercise 2.46 START

(defun make-vect (x y)
  (cons x y))

(defun xcor-vect (v)
  (car v))
(defun ycor-vect (v)
  (cdr v))

(defun add-vect (v w)
  (make-vect (+ (xcor-vect v) (xcor-vect w))
	     (+ (ycor-vect v) (ycor-vect w))))

(defun scale-vect (s v)
  (make-vect (* (xcor-vect v) s) (* (ycor-vect v) s)))

(defun sub-vect (v w)
  (add-vect v (scale-vect -1 w)))

;;; Exercise 2.46 END

;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; SDL Specific Stuff ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; (1) Call (OPEN-SDL-WINDOW width height)
;;; (2) Play around (use (CLEAR-WINDOW) to clear the contents)
;;; (3) Call (SDL:QUIT-SDL)

;;; E.g.
#|
(open-sdl-window 400 400)
(defparameter *frame*
  (make-frame (make-vect 0 400) (make-vect 400 0) (make-vect 0 -400)))
(funcall (square-limit #'wave 3) *frame*)
(sdl:quit-sdl)
|#
;;; Note:
;;; Implementation of the ROGERS function is left as an exercise to the reader.

(defun clear-window ()
  (sdl:clear-display sdl:*white*)
  (sdl:update-display))

(defun open-sdl-window (width height)
  (sdl:init-sdl)
  (sdl:window width height :title-caption "Painter")
  (clear-window))

(defun draw-line (start end)
  (sdl:draw-line (sdl:point :x (round (xcor-vect start)) :y (round (ycor-vect start)))
                 (sdl:point :x (round (xcor-vect end))   :y (round (ycor-vect end)))
                 :color sdl:*black*)
  (sdl:update-display))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; SDL Specific Stuff Ends ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Exercise 2.47 START

;;; First representation
(defun make-frame (origin edge1 edge2)
  (list origin edge1 edge2))
(defun origin-frame (frame)
  (first frame))
(defun edge1-frame (frame)
  (second frame))
(defun edge2-frame (frame)
  (third frame))

;;; Second representation
(defun make-frame-2 (origin edge1 edge2)
  "Another representation."
  (cons origin (cons edge1 edge2)))
(defun origin-frame-2 (frame)
  "Another representation."
  (car frame))
(defun edge1-frame-2 (frame)
  "Another representation."
  (cadr frame))
(defun edge2-frame-2 (frame)
  "Another representation."
  (cddr frame))

;;; Exercise 2.47 END

(defun frame-coord-map (frame)
  (lambda (v)
    (add-vect
     (origin-frame frame)
     (add-vect (scale-vect (xcor-vect v)
			   (edge1-frame frame))
	       (scale-vect (ycor-vect v)
			   (edge2-frame frame))))))

;;; Exercise 2.48 START

(defun make-painter-segment (start end)
  (cons start end))
(defun start-painter-segment (segment)
  (car segment))
(defun end-painter-segment (segment)
  (cdr segment))

;;; Exercise 2.48 END

(defun segments->painter (segment-list)
  (lambda (frame)
    (dolist (segment segment-list)
      (draw-line (funcall (frame-coord-map frame)
			  (start-painter-segment segment))
		 (funcall (frame-coord-map frame)
			  (end-painter-segment segment))))))

(defun transform-painter (painter origin corner1 corner2)
  (lambda (frame)
    (let ((m (frame-coord-map frame)))
      (let ((new-origin (funcall m origin)))
	(funcall painter
		 (make-frame new-origin
			     (sub-vect (funcall m corner1) new-origin)
			     (sub-vect (funcall m corner2) new-origin)))))))

(defun flip-vert (painter)
  (transform-painter painter
		     (make-vect 0.0 1.0)
		     (make-vect 1.0 1.0)
		     (make-vect 0.0 0.0)))

(defun shrink-to-upper-right (painter)
  (transform-painter painter
		     (make-vect 0.5 0.5)
		     (make-vect 1.0 0.5)
		     (make-vect 0.5 1.0)))

(defun rotate90 (painter)
  (transform-painter painter
		     (make-vect 1.0 0.0)
		     (make-vect 1.0 1.0)
		     (make-vect 0.0 0.0)))

(defun squash-inwards (painter)
  (transform-painter painter
		     (make-vect 0.0 0.0)
		     (make-vect 0.65 0.35)
		     (make-vect 0.35 0.65)))

(defun beside (painter1 painter2)
  (let ((split-point (make-vect 0.5 0.0)))
    (let ((paint-left
	   (transform-painter painter1
			      (make-vect 0.0 0.0)
			      split-point
			      (make-vect 0.0 1.0)))
	  (paint-right
	   (transform-painter painter2
			      split-point
			      (make-vect 1.0 0.0)
			      (make-vect 0.5 1.0))))
      (lambda (frame)
	(funcall paint-left frame)
	(funcall paint-right frame)))))

;;; Exercise 2.49 START

(define painter-outline
  (segments->painter
   (list (make-painter-segment (make-vect 0 0) (make-vect 0 1))
	 (make-painter-segment (make-vect 0 1) (make-vect 1 1))
	 (make-painter-segment (make-vect 1 1) (make-vect 1 0))
	 (make-painter-segment (make-vect 0 0) (make-vect 0 0)))))

(define painter-x
  (segments->painter
   (list (make-painter-segment (make-vect 0 0) (make-vect 1 1))
	 (make-painter-segment (make-vect 0 1) (make-vect 1 0)))))

(defun segments-from-coordinates (coords)
  (if (null coords)
      nil
      (cons (make-painter-segment (make-vect (first coords) (second coords))
				  (make-vect (third coords) (fourth coords)))
	    (segments-from-coordinates (subseq coords 4)))))

(defparameter *diamond*
  '(0.5 0 1 0.5 1 0.5 0.5 1 0.5 1 0 0.5 0 0.5 0.5 0))

(define painter-diamond
  (segments->painter (segments-from-coordinates *diamond*)))

(defparameter *wave*
  (append '(0.00 0.85 0.15 0.60 0.15 0.60 0.30 0.65 0.30 0.65 0.40 0.65
	    0.40 0.65 0.35 0.85 0.35 0.85 0.40 1.00)
	  '(0.60 1.00 0.65 0.85 0.65 0.85 0.60 0.65 0.60 0.65 0.75 0.65
	    0.75 0.65 1.00 0.35)
	  '(1.00 0.15 0.60 0.45 0.60 0.45 0.75 0.00)
	  '(0.60 0.00 0.50 0.30 0.50 0.30 0.40 0.00)
	  '(0.25 0.00 0.35 0.50 0.35 0.50 0.30 0.60 0.30 0.60 0.15 0.40
	    0.15 0.40 0.00 0.65)))

(define wave
  (segments->painter (segments-from-coordinates *wave*)))

;;; Exercise 2.49 END

;;; Exercise 2.50 START

(defun flip-horiz (painter)
  (transform-painter painter
		     (make-vect 1.0 0.0)
		     (make-vect 0.0 0.0)
		     (make-vect 1.0 1.0)))

(defun rotate180 (painter)
  (flip-horiz (flip-vert painter)))

(defun rotate270 (painter)
  (rotate90 (rotate180 painter)))

;;; Exercise 2.50 END

;;; Exercise 2.51 START

(defun below (painter1 painter2)
  (let ((split-point (make-vect 0.0 0.5)))
    (let ((paint-bottom
	   (transform-painter painter1
			      (make-vect 0.0 0.0)
			      (make-vect 1.0 0.0)
			      split-point))
	  (paint-top
	   (transform-painter painter2
			      split-point
			      (make-vect 1.0 0.5)
			      (make-vect 0.0 1.0))))
      (lambda (frame)
	(funcall paint-bottom frame)
	(funcall paint-top frame)))))

(defun below-2 (painter1 painter2)
  "Using BESIDE and rotations."
  (rotate270 (beside (rotate90 painter2) (rotate90 painter1))))

;;; Exercise 2.51 END

(define wave2 (beside #'wave (flip-vert #'wave)))
(define wave4 (below #'wave2 #'wave2))

(defun flipped-pairs (painter)
  (let ((painter2 (beside painter (flip-vert painter))))
    (below painter2 painter2)))

(define wave4-2 (flipped-pairs #'wave))

(defun right-split (painter n)
  (if (= n 0)
      painter
      (let ((smaller (right-split painter (- n 1))))
	(beside painter (below smaller smaller)))))

;;; Exercise 2.44 START

(defun up-split (painter n)
  (if (= n 0)
      painter
      (let ((smaller (up-split painter (- n 1))))
	(below painter (beside smaller smaller)))))

;;; Exercise 2.44 END

(defun corner-split (painter n)
  (if (= n 0)
      painter
      (let ((up (up-split painter (- n 1)))
	    (right (right-split painter (- n 1))))
	(let ((top-left (beside up up))
	      (bottom-right (below right right))
	      (corner (corner-split painter (- n 1))))
	  (beside (below painter top-left)
		  (below bottom-right corner))))))

(defun square-limit (painter n)
  (let ((quarter (corner-split painter n)))
    (let ((half (beside (flip-horiz quarter) quarter)))
      (below (flip-vert half) half))))

(defun square-of-four (tl tr bl br)
  (lambda (painter)
    (let ((top (beside (funcall tl painter) (funcall tr painter)))
	  (bottom (beside (funcall bl painter) (funcall br painter))))
      (below bottom top))))

(defun flipped-pairs-2 (painter)
  "Using SQUARE-OF-FOUR."
  (let ((combine4 (square-of-four #'identity #'flip-vert
				  #'identity #'flip-vert)))
    (funcall combine4 painter)))

(defun square-limit-2 (painter n)
  "Using SQUARE-OF-FOUR."
  (let ((combine4 (square-of-four #'flip-horiz #'identity
				  #'rotate180 #'flip-vert)))
    (funcall combine4 (corner-split painter n))))

;;; Exercise 2.45 START

(defun split (fn1 fn2)
  (labels ((rec (painter n)
	     (if (= n 0)
		 painter
		 (let ((smaller (rec painter (- n 1))))
		   (funcall fn1 (funcall fn2 smaller smaller) painter)))))
    #'rec))

;;; Exercise 2.45 END

;;; Exercise 2.52 START

(defparameter *smiling*
  (append '(0.00 0.85 0.15 0.60 0.15 0.60 0.30 0.65 0.30 0.65 0.40 0.65
	    0.40 0.65 0.35 0.85 0.35 0.85 0.40 1.00)
	  '(0.60 1.00 0.65 0.85 0.65 0.85 0.60 0.65 0.60 0.65 0.75 0.65
	    0.75 0.65 1.00 0.35)
	  '(1.00 0.15 0.60 0.45 0.60 0.45 0.75 0.00)
	  '(0.60 0.00 0.50 0.30 0.50 0.30 0.40 0.00)
	  '(0.25 0.00 0.35 0.50 0.35 0.50 0.30 0.60 0.30 0.60 0.15 0.40
	    0.15 0.40 0.00 0.65)
	  '(0.45 0.80 0.47 0.75 0.47 0.75 0.53 0.75 0.53 0.75 0.55 0.80)))

(define smiling
  (segments->painter (segments-from-coordinates *smiling*)))

(defun corner-split-2 (painter n)
  "Using only one instance of of UP and RIGHT."
  (if (= n 0)
      painter
      (let ((up (up-split painter (- n 1)))
	    (right (right-split painter (- n 1)))
	    (corner (corner-split-2 painter (- n 1))))
	(beside (below painter up)
		(below right corner)))))

(defun square-limit-3 (painter n)
  "Putting the large ones in the corners."
  (let ((quarter (corner-split-2 painter n)))
    (let ((half (beside quarter (flip-horiz quarter))))
      (below half (flip-vert half)))))

;;; Exercise 2.52 END


;;; Section 2.3.1

(defparameter *a* 1)

(defparameter *b* 2)

(defun %member (item x)
  (cond ((null x) nil)
	((eq item (car x)) x)
	(t (%member item (cdr x)))))

;;; Exercise 2.53 START

;;; => (a b c)
;;; => ((GEORGE))
;;; => ((Y1 Y2))
;;; => (Y1 Y2)
;;; => NIL
;;; => NIL
;;; => (RED SHOES BLUE SOCKS)

;;; Exercise 2.53 END

;;; Exercise 2.54 START

(defun %equalp (a b)
  (if (and (atom a) (atom b))
      (eq a b)
      (and (consp a) (consp b)
	   (%equalp (car a) (car b))
	   (%equalp (cdr a) (cdr b)))))

;;; Exercise 2.54 END

;;; Exercise 2.55 START

;;; (car ''abracadabra) = (car (quote (quote abracadabra))) =
;;; (car '(quote abracadabra)) = quote.

;;; Exercise 2.55 END


;;; Section 2.3.2

(defun variablep (x)
  (symbolp x))

(defun same-variable-p (v1 v2)
  (and (variablep v1) (variablep v2) (eq v1 v2)))

(defun make-sum-without-simplification (a1 a2)
  (list '+ a1 a2))

(defun sump (x)
  (and (consp x) (eq (car x) '+)))

(defun addend (s) (cadr s))

(defun augend (s) (caddr s))

(defun make-product-without-simplification (m1 m2)
  (list '* m1 m2))

(defun productp (x)
  (and (consp x) (eq (car x) '*)))

(defun multiplier (p) (cadr p))

(defun multiplicand (p) (caddr p))

(defun =numberp (exp num)
  (and (numberp exp) (= exp num)))

(defun make-sum (a1 a2)
  (cond ((=numberp a1 0) a2)
	((=numberp a2 0) a1)
	((and (numberp a1) (numberp a2)) (+ a1 a2))
	(t (list '+ a1 a2))))

(defun make-product (m1 m2)
  (cond ((or (=numberp m1 0) (=numberp m2 0)) 0)
	((=numberp m1 1) m2)
	((=numberp m2 1) m1)
	((and (numberp m1) (numberp m2)) (* m1 m2))
	(t (list '* m1 m2))))

;;; Exercise 2.56 START

(defun exponentiationp (x)
  (and (consp x) (eq (car x) '**)))

(defun base (e) (cadr e))

(defun exponent (e) (caddr e))

(defun make-exponentiation (b e)
  (cond ((=numberp e 0) 1)
	((=numberp e 1) b)
	(t (list '** b e))))

;;; Continued below...

;;; Exercise 2.56 END

(defun symbolic-deriv (exp var)
  (cond ((numberp exp) 0)
	((variablep exp)
	 (if (same-variable-p exp var) 1 0))
	((sump exp)
	 (make-sum (symbolic-deriv (addend exp) var)
		   (symbolic-deriv (augend exp) var)))
	((productp exp)
	 (make-sum
	  (make-product (multiplier exp)
			(symbolic-deriv (multiplicand exp) var))
	  (make-product (symbolic-deriv (multiplier exp) var)
			(multiplicand exp))))
;;; Exercise 2.56 START

	((exponentiationp exp)
	 (make-product
	  (make-product (exponent exp)
			(make-exponentiation (base exp)
					     (make-sum (exponent exp) -1)))
	  (symbolic-deriv (base exp) var)))

;;; Exercise 2.56 END
	(t (error "unknown expression type ~a -- SYMBOLIC-DERIV" exp))))

;;; Exercise 2.57 START

(defun augend-2 (s)
  "Allow any number of arguments"
  (if (> (length s) 3)
      (cons '+ (cddr s))
      (caddr s)))

(defun multiplicand-2 (p)
  "Allow any number of arguments"
  (if (> (length p) 3)
      (cons '* (cddr p))
      (caddr p)))

;;; Exercise 2.57 END

;;; Exercise 2.58 START

(defun infix-sump (x)
  (and (consp x) (eq (second x) '+)))

(defun infix-addend (s) (first s))

(defun infix-augend (s) (third s))

(defun make-infix-sum (a1 a2)
  (cond ((=numberp a1 0) a2)
	((=numberp a2 0) a1)
	((and (numberp a1) (numberp a2)) (+ a1 a2))
	(t (list a1 '+ a2))))

(defun infix-productp (x)
  (and (consp x) (eq (second x) '*)))

(defun infix-multiplier (p) (first p))

(defun infix-multiplicand (p) (third p))

(defun make-infix-product (m1 m2)
  (cond ((or (=numberp m1 0) (=numberp m2 0)) 0)
	((=numberp m1 1) m2)
	((=numberp m2 1) m1)
	((and (numberp m1) (numberp m2)) (* m1 m2))
	(t (list m1 '* m2))))

;;; (b)

(defun expression-operators (x)
  (if (or (atom x) (< (length x) 2))
      nil
      (cons (second x) (expression-operators (subseq x 2)))))

(defun expression-type (x)
  (let ((ops (expression-operators x)))
    (or (and (find '+ ops) 'sum)
	(and (find '* ops) 'product))))

(defun unbox (x)
  (if (= (length x) 1)
      (car x)
      x))

(defun clever-sump (x)
  (eq (expression-type x) 'sum))

(defun clever-addend (s)
  (unbox (subseq s 0 (position '+ s))))

(defun clever-augend (s)
  (unbox (subseq s (1+ (position '+ s)))))

(defun clever-productp (x)
  (eq (expression-type x) 'product))

(defun clever-multiplier (p)
  (unbox (subseq p 0 (position '* p))))

(defun clever-multiplicand (p)
  (unbox (subseq p (1+ (position '* p)))))

;;; Exercise 2.58 END


;;; Section 2.3.3

(defun element-of-set-p (x set)
  (cond ((null set) nil)
	((equal x (car set)) t)
	(t (element-of-set-p x (cdr set)))))

(defun adjoin-set (x set)
  (if (element-of-set-p x set)
      set
      (cons x set)))

(defun intersection-set (set1 set2)
  (cond ((or (null set1) (null set2)) '())
	((element-of-set-p (car set1) set2)
	 (cons (car set1)
	       (intersection-set (cdr set1) set2)))
	(t (intersection-set (cdr set1) set2))))

;;; Exercise 2.59 START

(defun union-set (set1 set2)
  (cond ((null set1) set2)
	((element-of-set-p (car set1) set2)
	 (union-set (cdr set1) set2))
	(t (cons (car set1)
		 (union-set (cdr set1) set2)))))

;;; Exercise 2.59 END

;;; Exercise 2.60 START

(defun duplicate-adjoin-set (x set)
  (cons x set))

(defun duplicate-union-set (set1 set2)
  (append set1 set2))

;;; The other functions are the same.
;;; ADJOIN-SET is faster a bit, but the others are much slower because of the
;;; surplus in the list.
;;; May be better when doing a lot of adjoining.

;;; Exercise 2.60 END

(defun ordered-element-of-set-p (x set)
  (cond ((null set) nil)
	((= x (car set)) t)
	((< x (car set)) nil)
	(t (ordered-element-of-set-p x (cdr set)))))

(defun ordered-intersection-set (set1 set2)
  (if (or (null set1) (null set2))
      '()
      (let ((x1 (car set1))
	    (x2 (car set2)))
	(cond ((= x1 x2)
	       (cons x1 (ordered-intersection-set (cdr set1) (cdr set2))))
	      ((< x1 x2)
	       (ordered-intersection-set (cdr set1) set2))
	      ((< x2 x1)
	       (ordered-intersection-set set1 (cdr set2)))))))

;;; Exercise 2.61 START

(defun ordered-adjoin-set (x set)
  (cond ((null set) (list x))
	((= x (car set)) set)
	((< x (car set)) (cons x set))
	((> x (car set))
	 (cons (car set) (ordered-adjoin-set x (cdr set))))))

;;; Exercise 2.61 END

;;; Exercise 2.62 START

(defun ordered-union-set (set1 set2)
  (cond ((null set1) set2)
	((null set2) set1)
	((= (car set1) (car set2))
	 (cons (car set1) (ordered-union-set (cdr set1) (cdr set2))))
	((< (car set1) (car set2))
	 (cons (car set1) (ordered-union-set (cdr set1) set2)))
	((> (car set1) (car set2))
	 (cons (car set2) (ordered-union-set set1 (cdr set2))))))

;;; Exercise 2.62 END

(defun binary-tree-entry (tree) (car tree))
(defun binary-tree-left-branch (tree) (cadr tree))
(defun binary-tree-right-branch (tree) (caddr tree))
(defun make-binary-tree (entry left right)
  (list entry left right))

(defun binary-tree-element-of-set-p (x set)
  (cond ((null set) nil)
	((= x (binary-tree-entry set)) t)
	((< x (binary-tree-entry set))
	 (binary-tree-element-of-set-p x (binary-tree-left-branch set)))
	((> x (binary-tree-entry set))
	 (binary-tree-element-of-set-p x (binary-tree-right-branch set)))))

(defun binary-tree-adjoin-set (x set)
  (cond ((null set) (make-binary-tree x '() '()))
	((= x (binary-tree-entry set)) set)
	((< x (binary-tree-entry set))
	 (make-binary-tree
	  (binary-tree-entry set)
	  (binary-tree-adjoin-set x (binary-tree-left-branch set))
	  (binary-tree-right-branch set)))
	((> x (binary-tree-entry set))
	 (make-binary-tree
	  (binary-tree-entry set)
	  (binary-tree-left-branch set)
	  (binary-tree-adjoin-set x (binary-tree-right-branch set))))))

;;; Exercise 2.63 START

(defun tree->list-1 (tree)
  (if (null tree)
      '()
      (append (tree->list-1 (binary-tree-left-branch tree))
              (cons (binary-tree-entry tree)
                    (tree->list-1 (binary-tree-right-branch tree))))))

(defun tree->list-2 (tree)
  (labels ((copy-to-list (tree result-list)
	     (if (null tree)
		 result-list
		 (copy-to-list
		  (binary-tree-left-branch tree)
		  (cons (binary-tree-entry tree)
			(copy-to-list (binary-tree-right-branch tree)
				      result-list))))))
    (copy-to-list tree '())))

;;; (a) They are just recursive/iterative counterparts.
;;;     They produce an ordered list: (1 3 5 7 9 11)
;;; (b) They have the same growth.

;;; Exercise 2.63 END

;;; Exercise 2.64 START

(defun partial-tree (elts n)
  (if (= n 0)
      (cons '() elts)
      (let ((left-size (floor (- n 1) 2)))
	(let ((left-result (partial-tree elts left-size)))
	  (let ((left-tree (car left-result))
		(non-left-elts (cdr left-result))
		(right-size (- n (+ left-size 1))))
	    (let ((this-entry (car non-left-elts))
		  (right-result (partial-tree (cdr non-left-elts)
					      right-size)))
	      (let ((right-tree (car right-result))
		    (remaining-elts (cdr right-result)))
		(cons (make-binary-tree this-entry left-tree right-tree)
		      remaining-elts))))))))

(defun list->tree (elements)
  (car (partial-tree elements (length elements))))

;; (a) PARTIAL-TREE creates a balanced tree from the first N elements of ELTS.
;;     The remaining elements are just left alone, e.g. after a call with
;;     N = 3, it would return something like (TREE E4 E5 E6).
;;     We can create a whole tree by calling it with the # of elements in ELTS.
;;     The CAR is needed because it returns with (TREE).
;;     It creates the balanced tree by first creating a balanced tree on the
;;     left, then on the right, with half of the elements, recursively.
;;
;;      5
;;     / \
;;    /   \
;;   /     \
;;  /       \
;; 1         9
;;  \       / \
;;   \     /   \
;;    3   7    11
;;
;; (b) O(n).

;;; Exercise 2.64 END

;;; Exercise 2.65 START

(defun binary-tree-union-set (set1 set2)
  (list->tree
   (ordered-union-set (tree->list-1 set1) (tree->list-1 set2))))

(defun binary-tree-intersection-set (set1 set2)
  (list->tree
   (ordered-intersection-set (tree->list-1 set1) (tree->list-1 set2))))

;;; Exercise 2.65 END

#+nil
(defun lookup (given-key set-of-records)
  (cond ((null set-of-records) nil)
	((equal given-key (key (car set-of-records)))
	 (car set-of-records))
	(t (lookup given-key (cdr set-of-records)))))

;;; Exercise 2.66 START

#+nil
(defun lookup (given-key set-of-records)
  (cond ((null set-of-records) nil)
	((equal given-key (key (entry set-of-records))) (entry set-of-records))
	((< given-key (key (entry set-of-records)))
	 (lookup given-key (left-branch set-of-records)))
	((> given-key (key (entry set-of-records)))
	 (lookup given-key (right-branch set-of-records)))))

;;; Exercise 2.66 END


;;; Section 2.3.4

(defun make-leaf (symbol weight)
  (list 'leaf symbol weight))
(defun leafp (object)
  (eq (car object) 'leaf))
(defun symbol-leaf (x) (cadr x))
(defun weight-leaf (x) (caddr x))

(defun huffman-left-branch (tree) (car tree))
(defun huffman-right-branch (tree) (cadr tree))
(defun symbols (tree)
  (if (leafp tree)
      (list (symbol-leaf tree))
      (caddr tree)))
(defun weight (tree)
  (if (leafp tree)
      (weight-leaf tree)
      (cadddr tree)))
(defun make-code-tree (left right)
  (list left
	right
	(append (symbols left) (symbols right))
	(+ (weight left) (weight right))))

(defun choose-branch (bit branch)
  (cond ((= bit 0) (huffman-left-branch branch))
	((= bit 1) (huffman-right-branch branch))
	(t (error "bad bit ~a -- CHOOSE-BRANCH" bit))))
(defun decode (bits tree)
  (labels ((decode-1 (bits current-branch)
	     (if (null bits)
		 '()
		 (let ((next-branch (choose-branch (car bits) current-branch)))
		   (if (leafp next-branch)
		       (cons (symbol-leaf next-branch)
			     (decode-1 (cdr bits) tree))
		       (decode-1 (cdr bits) next-branch))))))
    (decode-1 bits tree)))

(defun huffman-adjoin-set (x set)
  (cond ((null set) (list x))
	((< (weight x) (weight (car set))) (cons x set))
	(t (cons (car set)
		 (huffman-adjoin-set x (cdr set))))))

(defun make-leaf-set (pairs)
  (if (null pairs)
      '()
      (let ((pair (car pairs)))
	(huffman-adjoin-set (make-leaf (car pair)
				       (cadr pair))
			    (make-leaf-set (cdr pairs))))))

;;; Exercise 2.67 START

(defparameter *sample-tree*
  (make-code-tree (make-leaf 'A 4)
                  (make-code-tree
                   (make-leaf 'B 2)
                   (make-code-tree (make-leaf 'D 1)
                                   (make-leaf 'C 1)))))

(defparameter *sample-message* '(0 1 1 0 0 1 0 1 0 1 1 1 0))

#+nil
(decode *sample-message* *sample-tree*)
;;; => (A D A B B C A)

;;; Exercise 2.67 END

;;; Exercise 2.68 START

(defun encode-symbol (symbol tree)
  (labels ((rec (branch)
	     (if (leafp branch)
		 '()
		 (if (member symbol (symbols (huffman-left-branch branch)))
		     (cons 0 (rec (huffman-left-branch branch)))
		     (cons 1 (rec (huffman-right-branch branch)))))))
    (if (member symbol (symbols tree))
	(rec tree)
	(error "Symbol ~a is not in the tree -- ENCODE-SYMBOL" symbol))))

(defun encode (message tree)
  (if (null message)
      '()
      (append (encode-symbol (car message) tree)
	      (encode (cdr message) tree))))

;;; Exercise 2.68 END

;;; Exercise 2.69 START

(defun successive-merge (leaves)
  (if (null (cdr leaves))
      (car leaves)
      (successive-merge
       (huffman-adjoin-set (make-code-tree (first leaves) (second leaves))
			   (subseq leaves 2)))))

(defun generate-huffman-tree (pairs)
  (successive-merge (make-leaf-set pairs)))

;;; Exercise 2.69 END

;;; Exercise 2.70 START

(defparameter *rock-song-tree*
  (generate-huffman-tree '((a 2) (na 16) (boom 1) (sha 3) (get 2) (yip 9)
			   (job 2) (wah 1))))

#+nil
(encode '(get a job
          sha na na na na na na na na
          get a job
          sha na na na na na na na na
          wah yip yip yip yip yip yip yip yip yip
          sha boom)
        *rock-song-tree*)
;;; =>
;; 111111100111101110000000001111111001111011100000000011010101010101010101010111011011 (84 bits)

;;; 8 symbols means we can use a 3-bit fixed length encoding.
;;; For a 36 symbol length message, it would be 108 bits long, so we have
;;; a 77.7% compression rate.

;;; Exercise 2.70 END

;;; Exercise 2.71 START

;;    *
;;   / \
;;  /   \
;; 16    *
;;      / \
;;     /   \
;;    8     *
;;         / \
;;        /   \
;;       4     *
;;            / \
;;           /   \
;;          2     1

;;     *
;;    / \
;;   /   \
;; 512    *
;;       / \
;;      /   \
;;    256    *
;;          / \
;;         /   \
;;       128    *
;;             / \
;;            /   \
;;           64    *
;;                / \
;;               /   \
;;              32    *
;;                   / \
;;                  /   \
;;                 16    *
;;                      / \
;;                     /   \
;;                    8     *
;;                         / \
;;                        /   \
;;                       4     *
;;                            / \
;;                           /   \
;;                          2     1

;;; 1 bit for the most frequent, n-1 bits for the least frequent symbol.

;;; Exercise 2.71 END

;;; Exercise 2.72 START

;;; In general, it should be O(log n).

;;; In the 2^n frequency case:
;;; - most frequent: O(n)
;;; - least frequent: O(n^2)

;;; Exercise 2.72 END


;;; Section 2.4.1

(defun rectangular-real-part (z) (car z))
(defun rectangular-imag-part (z) (cdr z))
(defun rectangular-magnitude (z)
  (sqrt (+ (square (rectangular-real-part z))
	   (square (rectangular-imag-part z)))))
(defun rectangular-angle (z)
  (atan (rectangular-imag-part z) (rectangular-real-part z)))
(defun rectangular-make-from-real-imag (x y) (cons x y))
(defun rectangular-make-from-mag-ang (r a)
  (cons (* r (cos a)) (* r (sin a))))

(defun polar-magnitude (z) (car z))
(defun polar-angle (z) (cdr z))
(defun polar-real-part (z)
  (* (polar-magnitude z) (cos (polar-angle z))))
(defun polar-imag-part (z)
  (* (polar-magnitude z) (sin (polar-angle z))))
(defun polar-make-from-mag-ang (r a) (cons r a))
(defun polar-make-from-real-imag (x y)
  (cons (sqrt (+ (square x) (square y)))
	(atan y x)))


;;; Section 2.4.2

(defun attach-tag (type-tag contents)
  (cons type-tag contents))
(defun type-tag (datum)
  (if (consp datum)
      (car datum)
      (error "Bad tagged datum ~a -- TYPE-TAG" datum)))
(defun contents (datum)
  (if (consp datum)
      (cdr datum)
      (error "Bad tagged datum ~a -- CONTENTS" datum)))

(defun rectangularp (z)
  (eq (type-tag z) 'rectangular))
(defun polarp (z)
  (eq (type-tag z) 'polar))

(defun real-part-rectangular (z) (car z))
(defun imag-part-rectangular (z) (cdr z))
(defun magnitude-rectangular (z)
  (sqrt (+ (square (real-part-rectangular z))
	   (square (imag-part-rectangular z)))))
(defun angle-rectangular (z)
  (atan (imag-part-rectangular z)
	(real-part-rectangular z)))
(defun make-from-real-imag-rectangular (x y)
  (attach-tag 'rectangular (cons x y)))
(defun make-from-mag-ang-rectangular (r a)
  (attach-tag 'rectangular
	      (cons (* r (cos a)) (* r (sin a)))))

(defun magnitude-polar (z) (car z))
(defun angle-polar (z) (cdr z))
(defun real-part-polar (z)
  (* (magnitude-polar z) (cos (angle-polar z))))
(defun imag-part-polar (z)
  (* (magnitude-polar z) (sin (angle-polar z))))
(defun make-from-mag-ang-polar (r a)
  (attach-tag 'polar (cons r a)))
(defun make-from-real-imag-polar (x y)
  (attach-tag 'polar
	      (cons (sqrt (+ (square x) (square y)))
		    (atan y x))))

(defun real-part-explicit-dispatch (z)
  (cond ((rectangularp z)
	 (real-part-rectangular (contents z)))
	((polarp z)
	 (real-part-polar (contents z)))
	(t (error "Unknown type ~a -- REAL-PART-EXPLICIT-DISPATCH" z))))
(defun imag-part-explicit-dispatch (z)
  (cond ((rectangularp z)
	 (imag-part-rectangular (contents z)))
	((polarp z)
	 (imag-part-polar (contents z)))
	(t (error "Unknown type ~a -- IMAG-PART-EXPLICIT-DISPATCH" z))))
(defun magnitude-explicit-dispatch (z)
  (cond ((rectangularp z)
	 (magnitude-rectangular (contents z)))
	((polarp z)
	 (magnitude-polar (contents z)))
	(t (error "Unknown type ~a -- MAGNITUDE-EXPLICIT-DISPATCH" z))))
(defun angle-explicit-dispatch (z)
  (cond ((rectangularp z)
	 (angle-rectangular (contents z)))
	((polarp z)
	 (angle-polar (contents z)))
	(t (error "Unknown type ~a -- ANGLE-EXPLICIT-DISPATCH" z))))

(defun make-from-real-imag (x y)
  (make-from-real-imag-rectangular x y))
(defun make-from-mag-ang (r a)
  (make-from-real-imag-polar r a))

(defun add-complex (z1 z2)
  (make-from-real-imag (+ (real-part-explicit-dispatch z1)
			  (real-part-explicit-dispatch z2))
		       (+ (imag-part-explicit-dispatch z1)
			  (imag-part-explicit-dispatch z2))))

(defun sub-complex (z1 z2)
  (make-from-real-imag (- (real-part-explicit-dispatch z1)
			  (real-part-explicit-dispatch z2))
		       (- (imag-part-explicit-dispatch z1)
			  (imag-part-explicit-dispatch z2))))

(defun mul-complex (z1 z2)
  (make-from-mag-ang (* (magnitude-explicit-dispatch z1)
			(magnitude-explicit-dispatch z2))
		     (+ (angle-explicit-dispatch z1)
			(angle-explicit-dispatch z2))))


;;; Section 2.4.3

;;; A quick hack to make the examples work
(let ((table (make-hash-table)))
  (defun %get (op type)
    (cdr (assoc type (gethash op table) :test #'equal)))
  (defun put (op type item)
    (if (%get op type)
	(setf (cdr (assoc type (gethash op table) :test #'equal)) item)
	(setf (gethash op table)
	      (cons (cons type item) (gethash op table))))))

(defun install-rectangular-package ()
  (labels ((real-part (z) (car z))
	   (imag-part (z) (cdr z))
	   (make-from-real-imag (x y) (cons x y))
	   (magnitude (z)
	     (sqrt (+ (square (real-part z))
		      (square (imag-part z)))))
	   (angle (z)
	     (atan (imag-part z) (real-part z)))
	   (make-from-mag-ang (r a)
	     (cons (* r (cos a)) (* r (sin a))))
	   (tag (x) (attach-tag 'rectangular x)))
    (put 'real-part '(rectangular) #'real-part)
    (put 'imag-part '(rectangular) #'imag-part)
    (put 'magnitude '(rectangular) #'magnitude)
    (put 'angle '(rectangular) #'angle)
    (put 'make-from-real-imag 'rectangular
	 (lambda (x y) (tag (make-from-real-imag x y))))
    (put 'make-from-mag-ang 'rectangular
	 (lambda (r a) (tag (make-from-mag-ang r a))))
    'done))

;;; Let's install it:
(install-rectangular-package)

(defun install-polar-package ()
  (labels ((magnitude (z) (car z))
	   (angle (z) (cdr z))
	   (make-from-mag-ang (r a) (cons r a))
	   (real-part (z)
	     (* (magnitude z) (cos (angle z))))
	   (imag-part (z)
	     (* (magnitude z) (sin (angle z))))
	   (make-from-real-imag (x y)
	     (cons (sqrt (+ (square x) (square y)))
		   (atan y x)))
	   (tag (x) (attach-tag 'polar x)))
    (put 'real-part '(polar) #'real-part)
    (put 'imag-part '(polar) #'imag-part)
    (put 'magnitude '(polar) #'magnitude)
    (put 'angle '(polar) #'angle)
    (put 'make-from-real-imag 'polar
	 (lambda (x y) (tag (make-from-real-imag x y))))
    (put 'make-from-mag-ang 'polar
	 (lambda (r a) (tag (make-from-mag-ang r a))))
    'done))

;;; Let's install it:
(install-polar-package)

(defun apply-generic (op &rest args)
  (let ((type-tags (mapcar #'type-tag args)))
    (let ((proc (%get op type-tags)))
      (if proc
	  (apply proc (mapcar #'contents args))
	  (error "No method ~a for these types: ~a -- APPLY-GENERIC"
		 op type-tags)))))

(defun real-part (z)
  "Using APPLY-GENERIC."
  (apply-generic 'real-part z))
(defun imag-part (z)
  "Using APPLY-GENERIC."
  (apply-generic 'imag-part z))
(defun magnitude (z)
  "Using APPLY-GENERIC."
  (apply-generic 'magnitude z))
(defun angle (z)
  "Using APPLY-GENERIC."
  (apply-generic 'angle z))

(defun make-from-real-imag-2 (x y)
  "Using GET."
  (funcall (%get 'make-from-real-imag 'rectangular) x y))
(defun make-from-mag-ang-2 (r a)
  "Using GET."
  (funcall (%get 'make-from-mag-ang 'polar) r a))

;;; Exercise 2.73 START

(defun operator (exp) (car exp))
(defun operands (exp) (cdr exp))

(defun dispatched-symbolic-deriv (exp var)
  (cond ((numberp exp) 0)
	((variablep exp) (if (same-variable-p exp var) 1 0))
	(t (funcall (%get 'deriv (operator exp)) (operands exp) var))))

;;; (a) Derivation is now done as a dynamic dispatch of the operation DERIV,
;;;     according to the operator in EXP.
;;;     Numbers and variables (symbols) are not lists, and thus cannot have
;;;     a type designator. Of course, we could have put them in OPERATOR and
;;;     OPERANDS, but it would be less intuitive.

;;; (b)

(defun install-sum-derivation ()
  (labels ((addend (s) (car s))
	   (augend (s) (cadr s))
	   (deriv (s var)
	     (make-sum (dispatched-symbolic-deriv (addend s) var)
		       (dispatched-symbolic-deriv (augend s) var))))
    (put 'deriv '+ #'deriv))
  'done)

;;; Let's install it:
(install-sum-derivation)

(defun install-product-derivation ()
  (labels ((multiplier (p) (car p))
	   (multiplicand (p) (cadr p))
	   (deriv (p var)
	     (make-sum
	      (make-product (multiplier p)
			    (dispatched-symbolic-deriv (multiplicand p) var))
	      (make-product (dispatched-symbolic-deriv (multiplier p) var)
			    (multiplicand p)))))
    (put 'deriv '* #'deriv))
  'done)

;;; Let's install it:
(install-product-derivation)

;;; (c)

(defun install-exponent-derivation ()
  (labels ((base (e) (car e))
	   (exponent (e) (cadr e))
	   (deriv (e var)
	     (make-product
	      (make-product
	       (exponent e)
	       (make-exponentiation
		(base e)
		(make-sum (exponent e) -1)))
	      (dispatched-symbolic-deriv (base e) var))))
    (put 'deriv '** #'deriv))
  'done)

;;; Let's install it:
(install-exponent-derivation)

;;; (d) The install procedures should place the corresponding functions in
;;;     the appropriate place.

;;; Exercise 2.73 END

;;; Exercise 2.74 START

;;; (a) Every record file should have a list wrapped around the original
;;;     format, with a division (format) designator as its CAR and the
;;;     records formatted by the divison as its CDR. Then the following
;;;     function would work:

#+nil
(defun get-record (employee file)
  "Returns NIL if not found."
  (with-open-file (s file)
    (let ((records (read s)))
      (funcall (%get 'get-record (car records)) (cdr records)))))

;;; (b) The same applies to the records: they should be wrapped with a list
;;;     containing a division identifier.

#+nil
(defun get-salary (record)
  (funcall (%get 'get-salary (car record)) (cdr record)))

;;; (c)

#+nil
(defun find-employee (employee list-of-files)
  (get-record employee
	      (find employee list-of-files :test #'get-record)))

;;; (d) Their data format should be wrapped with a type designator, too;
;;;     and installer-procedures should be created for the data handling
;;;     functions (this should be done in (a) and (b), too).

;;; Exercise 2.74 END

(defun make-from-real-imag-obj (x y)
  "Object with message passing."
  (flet ((dispatch (op)
	   (cond ((eq op 'real-part) x)
		 ((eq op 'imag-part) y)
		 ((eq op 'magnitude)
		  (sqrt (+ (square x) (square y))))
		 ((eq op 'angle) (atan y x))
		 (t (error "Unknown op: ~a -- MAKE-FROM-REAL-IMAG-OBJ" op)))))
    #'dispatch))

(defun apply-generic-obj (op arg)
  "Generic with message passing."
  (funcall arg op))

;;; Exercise 2.75 START

(defun make-from-mag-ang-obj (r a)
  "Object with message pasing."
  (flet ((dispatch (op)
	   (cond ((eq op 'magnitude) r)
		 ((eq op 'angle) a)
		 ((eq op 'real-part) (* r (cos a)))
		 ((eq op 'imag-part) (* r (sin a)))
		 (t (error "Unknown op: ~a -- MAKE-FROM-MAG-ANG-OBJ" op)))))
    #'dispatch))

;;; Exercise 2.75 END

;;; Exercise 2.76 START

;;; Explicit dispatch
;;; -----------------
;;; New operator: create a dispatcher function for the operator that calls
;;;               the individual types' operator as needed
;;; New type    : for every dispatcher, insert a clause for the new type,
;;;               where the type should implement the given operator

;;; Data-directed dispatch
;;; ----------------------
;;; New operator: for every install procedure that implements this operator,
;;;               insert a new function and put it in the table
;;; New type    : create an install procedure for all operators that should be
;;;               implemented

;;; Message-passing style
;;; ---------------------
;;; New operator: for every object creator that should implement this operator,
;;;               insert a message type with the function definition
;;; New type    : create an object creator function that returns a dispatcher
;;;               function

;;; When new types must often be added, data-directed dispatch may be better.
;;; When new operations must ofen be added, message-passing may be better.

;;; Exercise 2.76 END


;;; Section 2.5.1

(defun add (x y) (apply-generic 'add x y))
;;; Exercise 2.88 START

(defun negate (x) (apply-generic 'negate x))
;;; Continued below...

;;; Exercise 2.88 END
(defun sub (x y) (apply-generic 'sub x y))
(defun mul (x y) (apply-generic 'mul x y))
(defun div (x y) (apply-generic 'div x y))

;;; Exercise 2.94 START

(defun greatest-common-divisor (x y)
  (apply-generic 'gcd x y))
;;; Continued below...

;;; Exercise 2.94 END

;;; Exercise 2.97 START

(defun %reduce (n d)
  (apply-generic 'reduce n d))
;;; Continued below...

;;; Exercise 2.97 END

;;; Exercise 2.79 START

(defun equp (x y) (apply-generic 'equp x y))
;;; Continued below...

;;; Exercise 2.79 END

;;; Exercise 2.80 START

(defun =zerop (x) (apply-generic '=zerop x))
;;; Continued below...

;;; Exercise 2.80 END

(defun install-lisp-number-package ()
  (flet ((tag (x) (attach-tag 'lisp-number x)))
    (put 'add '(lisp-number lisp-number)
	 (lambda (x y) (tag (+ x y))))
;;; Exercise 2.88 START

    (put 'negate '(lisp-number)
	 (lambda (x) (tag (- x))))
;;; Continued below...

;;; Exercise 2.88 END
    (put 'sub '(lisp-number lisp-number)
	 (lambda (x y) (tag (- x y))))
    (put 'mul '(lisp-number lisp-number)
	 (lambda (x y) (tag (* x y))))
    (put 'div '(lisp-number lisp-number)
	 (lambda (x y) (tag (/ x y))))
;;; Exercise 2.94 START

    (put 'gcd '(lisp-number lisp-number)
	 (lambda (x y) (tag (gcd x y))))
;;; Continued below...

;;; Exercise 2.94 END
;;; Exercise 2.97 START

    (put 'reduce '(lisp-number lisp-number)
	 (lambda (n d) (tag (let ((g (gcd n d))) (list (/ n g) (/ d g))))))
;;; Continued below...

;;; Exercise 2.97 END
;;; Exercise 2.79 START

    (put 'equp '(lisp-number lisp-number) #'=)
;;; Continued below...

;;; Exercise 2.79 END
;;; Exercise 2.80 START

    (put '=zerop '(lisp-number) #'zerop)
;;; Continued below...

;;; Exercise 2.80 END
    (put 'make 'lisp-number
	 (lambda (x) (tag x)))
    'done))

;;; Let's install it:
(install-lisp-number-package)

(defun make-lisp-number (n)
  (funcall (%get 'make 'lisp-number) n))

(defun install-rational-package ()
  (labels ((numer (x) (car x))
	   (denom (x) (cdr x))
	   (make-rat (n d)
	     (let ((g (gcd n d)))
	       (cons (/ n g) (/ d g))))
	   (add-rat (x y)
	     (make-rat (+ (* (numer x) (denom y))
			  (* (numer y) (denom x)))
		       (* (denom x) (denom y))))
;;; Exercise 2.88 START

	   (negate-rat (x)
	     (make-rat (- (numer x)) (denom x)))
;;;        Continued below...

;;; Exercise 2.88 END
	   (sub-rat (x y)
	     (make-rat (- (* (numer x) (denom y))
			  (* (numer y) (denom x)))
		       (* (denom x) (denom y))))
	   (mul-rat (x y)
	     (make-rat (* (numer x) (numer y))
		       (* (denom x) (denom y))))
	   (div-rat (x y)
	     (make-rat (* (numer x) (denom y))
		       (* (denom x) (numer y))))
;;; Exercise 2.79 START

	   (equp-rat (x y)
	     (and (= (numer x) (numer y))
		  (= (denom x) (denom y))))
;;;        Continued below...

;;; Exercise 2.79 END
;;; Exercise 2.80 START

	   (=zerop-rat (x) (= (numer x) 0))
;;;        Continued below...

;;; Exercise 2.80 END
	   (tag (x) (attach-tag 'rational x)))
    (put 'add '(rational rational)
	 (lambda (x y) (tag (add-rat x y))))
;;; Exercise 2.88 START

    (put 'negate '(rational)
	 (lambda (x) (tag (negate-rat x))))
;;; Continued below...

;;; Exercise 2.88 END
    (put 'sub '(rational rational)
	 (lambda (x y) (tag (sub-rat x y))))
    (put 'mul '(rational rational)
	 (lambda (x y) (tag (mul-rat x y))))
    (put 'div '(rational rational)
	 (lambda (x y) (tag (div-rat x y))))
;;; Exercise 2.79 START

    (put 'equp '(rational rational) #'equp-rat)
;;; Continued below...

;;; Exercise 2.79 END
;;; Exercise 2.80 START

    (put '=zerop '(rational) #'=zerop-rat)
;;; Continued below...

;;; Exercise 2.80 END
    (put 'make 'rational
	 (lambda (n d) (tag (make-rat n d))))
    'done))

;;; Let's install it:
(install-rational-package)

(defun make-rational (n d)
  (funcall (%get 'make 'rational) n d))

(defun install-complex-package ()
  (labels ((make-from-real-imag (x y)
	     (funcall (%get 'make-from-real-imag 'rectangular) x y))
	   (make-from-mag-ang (r a)
	     (funcall (%get 'make-from-mag-ang 'polar) r a))
	   (add-complex (z1 z2)
	     (make-from-real-imag (+ (real-part z1) (real-part z2))
				  (+ (imag-part z1) (imag-part z2))))
;;; Exercise 2.88 START

	   (negate-complex (z)
	     (make-from-real-imag (- (real-part z)) (- (imag-part z))))
;;;        Continued below...

;;; Exercise 2.88 END
	   (sub-complex (z1 z2)
	     (make-from-real-imag (- (real-part z1) (real-part z2))
				  (- (imag-part z1) (imag-part z2))))
	   (mul-complex (z1 z2)
	     (make-from-mag-ang (* (magnitude z1) (magnitude z2))
				(+ (angle z1) (angle z2))))
	   (div-complex (z1 z2)
	     (make-from-mag-ang (/ (magnitude z1) (magnitude z2))
				(- (angle z1) (angle z2))))
;;; Exercise 2.79 START

	   (equp-complex (z1 z2)
	     (and (= (real-part z1) (real-part z2)
		     (imag-part z1) (imag-part z2))))
;;;        Continued below...

;;; Exercise 2.79 END
;;; Exercise 2.80 START

	   (=zerop-complex (z) (= (magnitude z) 0))
;;;        Continued below...

;;; Exercise 2.80 END
	   (tag (z) (attach-tag 'complex z)))
    (put 'add '(complex complex)
	 (lambda (z1 z2) (tag (add-complex z1 z2))))
;;; Exercise 2.88 START

    (put 'negate '(complex)
	 (lambda (z) (tag (negate-complex z))))
;;; Continued below...

;;; Exercise 2.88 END
    (put 'sub '(complex complex)
	 (lambda (z1 z2) (tag (sub-complex z1 z2))))
    (put 'mul '(complex complex)
	 (lambda (z1 z2) (tag (mul-complex z1 z2))))
    (put 'div '(complex complex)
	 (lambda (z1 z2) (tag (div-complex z1 z2))))
;;; Exercise 2.79 START

    (put 'equp '(complex complex) #'equp-complex)

;;; Exercise 2.79 END
;;; Exercise 2.80 START

    (put '=zerop '(complex) #'=zerop-complex)

;;; Exercise 2.80 END
    (put 'make-from-real-imag 'complex
	 (lambda (x y) (tag (make-from-real-imag x y))))
    (put 'make-from-mag-ang 'complex
	 (lambda (r a) (tag (make-from-mag-ang r a))))
    'done))

;;; Let's install it:
(install-complex-package)

(defun make-complex-from-real-imag (x y)
  (funcall (%get 'make-from-real-imag 'complex) x y))
(defun make-complex-from-mag-ang (r a)
  (funcall (%get 'make-from-mag-ang 'complex) r a))

;;; Exercise 2.77 START

(put 'real-part '(complex) #'real-part)
(put 'imag-part '(complex) #'imag-part)
(put 'magnitude '(complex) #'magnitude)
(put 'angle '(complex) #'angle)

;;; The function MAGNITUDE already dispatches from the implementation type
;;; of complex numbers. With the lines above we tell the table to call this
;;; dispatcher when MAGNITUDE is called with a COMPLEX-tagged number.

;;; Trace:
;; (magnitude '(COMPLEX RECTANGULAR 3 . 4))
;; (apply-generic 'magnitude '(COMPLEX RECTANGULAR 3 . 4))
;; (magnitude '(RECTANGULAR 3 . 4))
;; (apply-generic 'magnitude '(RECTANGULAR 3 . 4))
;; (<local function in install-rectangular-package> '(3 . 4))
;;; => 5.0

;;; Exercise 2.77 END

;;; Exercise 2.78 START

(defun attach-tag-2 (type-tag contents)
  "Numbers are without tags."
  (if (numberp contents)
      contents
      (cons type-tag contents)))
(defun type-tag-2 (datum)
  "Numbers are without tags."
  (cond ((consp datum) (car datum))
	((numberp datum) 'lisp-number)
	(t (error "Bad tagged datum ~a -- TYPE-TAG-2" datum))))
(defun contents-2 (datum)
  "Numbers are without tags."
  (cond ((consp datum) (cdr datum))
	((numberp datum) datum)
	(t (error "Bad tagged datum ~a -- CONTENTS-2" datum))))

;;; Now install-lisp-number-package can be as simple as:

(defun install-lisp-number-package-2 ()
  "Numbers are without tags."
  (put 'add '(lisp-number lisp-number) #'+)
  (put 'sub '(lisp-number lisp-number) #'-)
  (put 'mul '(lisp-number lisp-number) #'*)
  (put 'div '(lisp-number lisp-number) #'/)
  (put 'make 'lisp-number #'identity)
  'done)

;;; Exercise 2.78 END


;;; Section 2.5.2

;;; In install-complex-package:
#+nil
(labels (...
	 (add-complex-to-lispnum (z x)
	   (make-from-real-imag (+ (real-part z) x)
				(imag-part z))))
  ...
  (put 'add '(complex lisp-number)
       (lambda (z x) (tag (add-complex-to-lispnum z x)))))

;;; A quick hack to make the examples work
(let ((table (make-hash-table)))
  (defun get-coercion (op type)
    (cdr (assoc type (gethash op table) :test #'equal)))
  (defun put-coercion (op type item)
    (if (get-coercion op type)
	(setf (cdr (assoc type (gethash op table) :test #'equal)) item)
	(setf (gethash op table)
	      (cons (cons type item) (gethash op table))))))

(defun lisp-number->complex (n)
  (make-complex-from-real-imag (contents n) 0))

(put-coercion 'lisp-number 'complex #'lisp-number->complex)

(defun apply-generic-with-coercion (op &rest args)
  "Coercion only for functions with 2 arguments."
  (let ((type-tags (mapcar #'type-tag args)))
    (let ((proc (%get op type-tags)))
      (if proc
	  (apply proc (mapcar #'contents args))
	  (if (= (length args) 2)
	      (let ((type1 (car type-tags))
		    (type2 (cadr type-tags))
		    (a1 (car args))
		    (a2 (cadr args)))
		(let ((t1->t2 (get-coercion type1 type2))
		      (t2->t1 (get-coercion type2 type1)))
		  (cond (t1->t2
			 (apply-generic-with-coercion op
						      (funcall t1->t2 a1) a2))
			(t2->t1
			 (apply-generic-with-coercion op
						      a1 (funcall t2->t1 a2)))
			(t (error "No method ~a for these types: ~a"
				  op type-tags)))))
	      (error "No method ~a for these types: ~a" op type-tags))))))

;;; Exercise 2.81 START

;;; (a) It would get stuck in an infinite recursion.
;;;     (exp z1 z2) is not defined for complex numbers, so
;;;     APPLY-GENERIC-WITH-COERCION tries to find a coercion, which he finds:
;;;     it coerces the two complex numbers to complex numbers, which results
;;;     in the call (APPLY-GENERIC-WITH-COERCION exp z1 z2), ie. the same call.

;;; (b) APPLY-GENERIC-WITH-COERCION works correctly, and it may even be bad to
;;;     disable coercion for different types. For example, a vector can be
;;;     coerced to a matrix, but matrix-*-vector can only be called with a
;;;     matrix and a vector, so coercing is needed, when it is called with
;;;     two vectors.

;;; (c)

(defun apply-generic-with-coercion-2 (op &rest args)
  "Coercion only for functions with 2 arguments.

Coercion is not tried if the arguments have the same type."
  (let ((type-tags (mapcar #'type-tag args)))
    (let ((proc (%get op type-tags)))
      (if proc
	  (apply proc (mapcar #'contents args))
	  (if (and (= (length args) 2)
		   (not (eq (car type-tags) (cadr type-tags))))
	      (let ((type1 (car type-tags))
		    (type2 (cadr type-tags))
		    (a1 (car args))
		    (a2 (cadr args)))
		(let ((t1->t2 (get-coercion type1 type2))
		      (t2->t1 (get-coercion type2 type1)))
		  (cond (t1->t2
			 (apply-generic-with-coercion-2 op
							(funcall t1->t2 a1) a2))
			(t2->t1
			 (apply-generic-with-coercion-2 op
							a1 (funcall t2->t1 a2)))
			(t (error "No method ~a for these types: ~a"
				  op type-tags)))))
	      (error "No method ~a for these types: ~a" op type-tags))))))

;;; Exercise 2.81 END

;;; Exercise 2.82 START

(defun apply-generic-with-coercion-3 (op &rest args)
  "Coercion for functions with any number of arguments."
  (let ((type-tags (mapcar #'type-tag args)))
    (let ((proc (%get op type-tags)))
      (if proc
	  (apply proc (mapcar #'contents args))
	  (labels ((get-coercions (t2)
		     (let ((lst (mapcar (lambda (t1)
					  (or (and (eq t1 t2) 'same-types)
					      (get-coercion t1 t2)))
					type-tags)))
		       (and (not (member nil lst)) lst)))
		   (try-types (types)
		     (if (null types)
			 (error "No method ~a for these types: ~a" op type-tags)
			 (let ((coercions (get-coercions (car types))))
			   (if coercions
			       (apply-generic-with-coercion-3
				op (mapcar (lambda (coercion arg)
					     (if (eq coercion 'same-types)
						 arg
						 (funcall coercion arg)))
					   coercions args))
			       (try-types (cdr types)))))))
	    (try-types type-tags))))))

;;; Example when this does not work:
;;; Take the function that solves the linear equation Ax = b, where
;;; A is a matrix of the form M^T * v * M, x and b are vectors.
;;; The solver must be called with 3 arguments: (solver M v b)
;;; The first argument must be a matrix, the other two are vectors.
;;; If we have a one-column matrix that we want to use as a column vector,
;;; the function above would not find the correct coercion.

;;; Exercise 2.82 END

;;; Exercise 2.83 START

#+nil
(progn
  (defun integer->rational (n)
    (make-rational n 1))
  (defun rational->real (x)
    (div (make-real (numer x)) (make-real (denom x))))
  (defun real->complex (x)
    (make-complex-from-real-imag x 0))

  ;;; Installation:
  (defun raise (x) (apply-generic 'raise x))
  (put 'raise '(integer) #'integer->rational)
  (put 'raise '(rational) #'rational->real)
  (put 'raise '(real) #'real->complex))

;;; Exercise 2.83 END

;;; Exercise 2.84 START

#+nil
(defun apply-generic-with-coercion-4 (op &rest args)
  "Coercion by successive raising (only for functions with 2 arguments)."
  (let ((type-tags (mapcar #'type-tag args)))
    (let ((proc (%get op type-tags)))
      (if proc
	  (apply proc (mapcar #'contents args))
	  (if (= (length args) 2)
	      (labels ((try-raise (arg type)
			 (if (eq (type-tag arg) type)
			     arg
			     (let ((raised (raise arg)))
			       (if raised
				   (try-raise raised type)
				   nil)))))
		(let ((type1 (car type-tags))
		      (type2 (cadr type-tags))
		      (a1 (car args))
		      (a2 (cadr args)))
		  (let ((raised-a1 (try-raise a1 type2)))
		    (if raised-a1
			(apply-generic-with-coercion-4 op raised-a1 a2)
			(let ((raised-a2 (try-raise type1 a2)))
			  (if raised-a2
			      (apply-generic-with-coercion-4 op a1 raised-a2)
			      (error "No method ~a for these types: ~a"
				     op type-tags)))))))
	      (error "No method ~a for these types: ~a" op type-tags))))))

;;; Exercise 2.84 END

;;; Exercise 2.85 START

#+nil
(progn
  ;;; In install-rational-package:
  (labels (...
	   (project (x) (make-integer (numer (x)))))
    ...
    (put 'project '(rational) #'project))

  ;;; In install-real-package:
  (labels (...
	   (project (x) (approximate-with-rational x)))
    ...
    (put 'project '(real) #'project))

  ;;; ... where APPROXIMATE-WITH-RATIONAL can be a quite complex function, but
  ;;; we are satisfied now with the following:
  (defparameter *precision* 5 "Number of digits to approximate.")
  (defun approximate-with-rational (x)
    (let ((tens (expt 10 *precision)))
      (make-rational (floor (* x tens)) tens)))

  ;;; In install-complex-package:
  (labels (...
	   (project (z) (make-real (real-part z))))
    ...
    (put 'project '(complex) #'project))

  (defun drop (x)
    (let ((projection (%get 'project (type-tag x))))
      (if projection
	  (let ((projected (funcall projection (contents x))))
	    (if (equp (raise projected) x)
		(drop projected)
		x))
	  x))))

;;; Replace the line in APPLY-GENERIC-WITH-COERCION-4
;; (apply proc (mapcar #'contents args))
;;; with
;; (drop (apply proc (mapcar #'contents args)))

;;; Exercise 2.85 END

;;; Exercise 2.86 START

;;; 1. In INSTALL-COMPLEX-PACKAGE, replace every +,-,*,/ with add,sub,mul,div
;;; 2. In INSTALL-RECTANGULAR-PACKAGE and INSTALL-POLAR-PACKAGE, the
;;;    functions SIN, COS, ATAN and SQUARE should be made
;;;    generic. Since the trigonometric operations return real values,
;;;    this is not really a problem - if SINE / COSINE are defined for
;;;    real numbers, the rest is done by APPLY-GENERIC-WITH-COERCION.
;;;    As for SQUARE, it can be defined like:

#+nil
(defun generic-square (x) (mul x x))

;;; Exercise 2.86 END


;;; Section 2.5.3

(defun install-polynomial-package ()
  (labels ((make-poly (variable term-list)
	     (cons variable term-list))
	   (variable (p) (car p))
	   (term-list (p) (cdr p))
	   (variablep (x) (symbolp x))
	   (same-variable-p (v1 v2)
	     (and (variablep v1) (variablep v2) (eq v1 v2)))
	   (the-empty-termlist () '())
	   (first-term (term-list) (car term-list))
	   (rest-terms (term-list) (cdr term-list))
	   (empty-termlist-p (term-list) (null term-list))
	   (make-term (order coeff) (list order coeff))
	   (order (term) (car term))
	   (coeff (term) (cadr term))
	   (adjoin-term (term term-list)
	     (if (=zerop (coeff term))
		 term-list
		 (cons term term-list)))
	   (add-terms (L1 L2)
	     (cond ((empty-termlist-p L1) L2)
		   ((empty-termlist-p L2) L1)
		   (t (let ((t1 (first-term L1))
			    (t2 (first-term L2)))
			(cond ((> (order t1) (order t2))
			       (adjoin-term t1 (add-terms (rest-terms L1) L2)))
			      ((< (order t1) (order t2))
			       (adjoin-term t2 (add-terms L1 (rest-terms L2))))
			      (t (adjoin-term
				  (make-term (order t1)
					     (add (coeff t1) (coeff t2)))
				  (add-terms (rest-terms L1)
					     (rest-terms L2)))))))))
	   (add-poly (p1 p2)
	     (if (same-variable-p (variable p1) (variable p2))
		 (make-poly (variable p1)
			    (add-terms (term-list p1) (term-list p2)))
		 (error "Polys ~a, ~a not in same var -- ADD-POLY" p1 p2)))
;;; Exercise 2.88 START

	   (negate-terms (term-list)
	     (if (null term-list)
		 (the-empty-termlist)
		 (let ((term (first-term term-list)))
		   (adjoin-term (make-term (order term) (negate (coeff term)))
				(negate-terms (rest-terms term-list))))))
	   (negate-poly (p)
	     (make-poly (variable p) (negate-terms (term-list p))))
	   (sub-terms (L1 L2)
	     (cond ((empty-termlist-p L1) (negate-terms L2))
		   ((empty-termlist-p L2) L1)
		   (t (let ((t1 (first-term L1))
			    (t2 (first-term L2)))
			(cond ((> (order t1) (order t2))
			       (adjoin-term t1 (sub-terms (rest-terms L1) L2)))
			      ((< (order t1) (order t2))
			       (adjoin-term (make-term (order t2)
						       (negate (coeff t2)))
					    (sub-terms L1 (rest-terms L2))))
			      (t (adjoin-term
				  (make-term (order t1)
					     (sub (coeff t1) (coeff t2)))
				  (sub-terms (rest-terms L1)
					     (rest-terms L2)))))))))
	   (sub-poly (p1 p2)
	     (if (same-variable-p (variable p1) (variable p2))
		 (make-poly (variable p1)
			    (sub-terms (term-list p1) (term-list p2)))
		 (error "Polys ~a, ~a not in same var -- SUB-POLY" p1 p2)))
;;;        Continued below...

;;; Exercise 2.88 END
	   (mul-term-by-all-terms (t1 L)
	     (if (empty-termlist-p L)
		 (the-empty-termlist)
		 (let ((t2 (first-term L)))
		   (adjoin-term
		    (make-term (+ (order t1) (order t2))
			       (mul (coeff t1) (coeff t2)))
		    (mul-term-by-all-terms t1 (rest-terms L))))))
	   (mul-terms (L1 L2)
	     (if (empty-termlist-p L1)
		 (the-empty-termlist)
		 (add-terms (mul-term-by-all-terms (first-term L1) L2)
			    (mul-terms (rest-terms L1) L2))))
	   (mul-poly (p1 p2)
	     (if (same-variable-p (variable p1) (variable p2))
		 (make-poly (variable p1)
			    (mul-terms (term-list p1) (term-list p2)))
		 (error "Polys ~a, ~a not in same var -- MUL-POLY" p1 p2)))
;;; Exercise 2.91 START

	   (mul-term-termlist (term term-list)
	     (mul-terms (adjoin-term term (the-empty-termlist)) term-list))
	   (div-terms (L1 L2)
	     (if (empty-termlist-p L1)
		 (list (the-empty-termlist) (the-empty-termlist))
		 (let ((t1 (first-term L1))
		       (t2 (first-term L2)))
		   (if (> (order t2) (order t1))
		       (list (the-empty-termlist) L1)
		       (let* ((new-c (div (coeff t1) (coeff t2)))
			      (new-o (- (order t1) (order t2)))
			      (new-term (make-term new-o new-c))
			      (rest-of-result
			       (div-terms
				(sub-terms L1 (mul-term-termlist new-term L2))
				L2)))
			 (cons (adjoin-term new-term (car rest-of-result))
			       (cdr rest-of-result)))))))
	   (div-poly (p1 p2)
	     (if (same-variable-p (variable p1) (variable p2))
		 (let ((q-r (div-terms (term-list p1) (term-list p2))))
		   (list (make-poly (variable p1) (car q-r))
			 (make-poly (variable p1) (cadr q-r))))
		 (error "Polys ~a, ~a not in same var -- DIV-POLY" p1 p2)))
;;;        Continued below...

;;; Exercise 2.91 END
;;; Exercise 2.94/96 START

	   (mul-num-termlist (num L)
	     (if (empty-termlist-p L)
		 (the-empty-termlist)
		 (let ((term (first-term L)))
		   (adjoin-term (make-term (order term)
					   (mul num (coeff term)))
				(mul-num-termlist num (rest-terms L))))))
	   (pseudoremainder-terms (L1 L2)
	     (if (empty-termlist-p L1)
		 (the-empty-termlist)
		 (let* ((o1 (order (first-term L1)))
			(o2 (order (first-term L2)))
			(c (coeff (first-term L2)))
			;; This should be a generic exponentiation on C,
			;; but now we take the value of the LISP-NUMBER (CDR),
			;; and then create a LISP-NUMBER from it again
			(factor-num (expt (cdr c) (+ 1 o1 (- o2))))
			(factor (make-lisp-number factor-num)))
		   (cadr (div-terms (mul-num-termlist factor L1) L2)))))
	   (gcd-of-terms (term L)
	     (if (empty-termlist-p L)
		 (coeff term)
		 (greatest-common-divisor
		  (coeff term) (gcd-of-terms (first-term L) (rest-terms L)))))
	   (simplify-terms (L n)
	     (if (empty-termlist-p L)
		 (the-empty-termlist)
		 (let ((term (first-term L)))
		   (adjoin-term (make-term (order term) (div (coeff term) n))
				(simplify-terms (rest-terms L) n)))))
	   (gcd-terms (L1 L2)
	     (if (empty-termlist-p L2)
		 (simplify-terms
		  L1 (gcd-of-terms (first-term L1) (rest-terms L1)))
		 (gcd-terms L2 (pseudoremainder-terms L1 L2))))
	   (gcd-poly (p1 p2)
	     (if (same-variable-p (variable p1) (variable p2))
		 (gcd-terms (term-list p1) (term-list p2))
		 (error "Polys ~a, ~a not in same var -- GCD-POLY" p1 p2)))
;;;        Continued below...

;;; Exercise 2.94/96 END
;;; Exercise 2.97 START

	   (reduce-terms (n d)
	     (if (empty-termlist-p n)
		 (list (the-empty-termlist) (the-empty-termlist))
		 (let* ((gcd (gcd-terms n d))
			(o1 (max (order (first-term n))
				 (order (first-term d))))
			(o2 (order (first-term gcd)))
			(c (coeff (first-term gcd)))
			;; This should be a generic exponentiation on C,
			;; but now we take the value of the LISP-NUMBER (CDR),
			;; and then create a LISP-NUMBER from it again
			(factor-num (expt (cdr c) (+ 1 o1 (- o2))))
			(factor (make-lisp-number factor-num))
			(gcd-terms (mul-num-termlist factor gcd)))
		   (list (div-terms n gcd-terms)
			 (div-terms d gcd-terms)))))
	   (reduce-poly (p1 p2)
	     (if (same-variable-p (variable p1) (variable p2))
		 (let ((terms (reduce-terms (term-list p1) (term-list p2))))
		   (list (make-poly (variable p1) (car terms))
			 (make-poly (variable p1) (cadr terms))))
		 (error "Polys ~a, ~a not in same var -- REDUCE-POLY" p1 p2)))
;;;        Continued below...

;;; Exercise 2.97 END
;;; Exercise 2.87 START

	   (just-zero-terms (terms)
	     (or (empty-termlist-p terms)
		 (and (=zerop (coeff (first terms)))
		      (just-zero-terms (cdr terms)))))
	   (=zerop-poly (p)
	     (just-zero-terms (term-list p)))
;;;        Continued below...

;;; Exercise 2.87 END
	   (tag (p) (attach-tag 'polynomial p)))
    (put 'add '(polynomial polynomial)
	 (lambda (p1 p2) (tag (add-poly p1 p2))))
;;; Exercise 2.88 START

    (put 'negate '(polynomial)
	 (lambda (p) (tag (negate-poly p))))
    (put 'sub '(polynomial polynomial)
	 (lambda (p1 p2) (tag (sub-poly p1 p2))))

;;; Exercise 2.88 END
    (put 'mul '(polynomial polynomial)
	 (lambda (p1 p2) (tag (mul-poly p1 p2))))
;;; Exercise 2.91 START

    (put 'div '(polynomial polynomial)
	 (lambda (p1 p2)
	   (let ((quotient-remainder (div-poly p1 p2)))
	     (list (tag (car quotient-remainder))
		   (tag (cadr quotient-remainder))))))

;;; Exercise 2.91 END
;;; Exercise 2.94 START

    (put 'gcd '(polynomial polynomial)
	 (lambda (p1 p2) (tag (gcd-poly p1 p2))))

;;; Exercise 2.94 END
;;; Exercise 2.97 START

    (put 'reduce '(polynomial polynomial)
	 (lambda (p1 p2)
	   (let ((reduced (reduce-poly p1 p2)))
	     (list (tag (car reduced))
		   (tag (cadr reduced))))))
;;; Continued below...

;;; Exercise 2.97 END
;;; Exercise 2.87 START

    (put '=zerop '(polynomial) #'=zerop-poly)

;;; Exercise 2.87 END
    (put 'make 'polynomial
	 (lambda (var terms) (tag (make-poly var terms))))
    'done))

;;; Let's install it:
(install-polynomial-package)

(defun make-polynomial (var terms)
  (funcall (%get 'make 'polynomial) var terms))

;;; Exercise 2.89-90 START

;;; "Constant" functions
(defun make-term (order coeff) (list order coeff))
(defun order (term) (car term))
(defun coeff (term) (cadr term))

;;; Dispatched functions
(defun the-empty-termlist (type)
  (funcall (%get 'the-empty-termlist type)))
(defun first-term (term-list)
  (apply-generic 'first-term term-list))
(defun rest-terms (term-list)
  (apply-generic 'rest-terms term-list))
(defun empty-termlist-p (term-list)
  (apply-generic 'empty-termlist-p term-list))
(defun adjoin-term (term term-list)
  "In an ideal case, this should dispatch on TERM, too."
  (funcall (%get 'adjoin-term (type-tag term-list)) term (contents term-list)))

(defun install-sparse-polynomial-package ()
  (flet ((make-term-list (term-list) term-list)
	 (the-empty-termlist () '())
	 (first-term (term-list) (car term-list))
	 (rest-terms (term-list) (cdr term-list))
	 (empty-termlist-p (term-list) (null term-list))
	 (adjoin-term (term term-list)
	   (if (=zerop (coeff term))
	       term-list
	       (cons term term-list)))
	 (tag (p) (attach-tag 'sparse p)))
    (put 'the-empty-termlist 'sparse
	 (lambda () (tag (the-empty-termlist))))
    (put 'first-term '(sparse) #'first-term)
    (put 'rest-terms '(sparse)
	 (lambda (term-list) (tag (rest-terms term-list))))
    (put 'empty-termlist-p '(sparse) #'empty-termlist-p)
    (put 'adjoin-term 'sparse
	 (lambda (term term-list) (tag (adjoin-term term term-list))))
    (put 'make 'sparse
	 (lambda (term-list) (tag (make-term-list term-list))))
    'done))

;;; Let's install it:
(install-sparse-polynomial-package)

(defun make-sparse-term-list (term-list)
  (funcall (%get 'make 'sparse) term-list))

(defun install-dense-polynomial-package ()
  "Only works with positive powers."
  (labels ((make-term-list (term-list) (cons (1- (length term-list)) term-list))
	   (the-empty-termlist () (list -1))
	   (max-term (term-list) (car term-list))
	   (terms (term-list) (cdr term-list))
	   (first-term (term-list) (make-term (max-term term-list)
					      (cadr term-list)))
	   (rest-terms (term-list) (cons (1- (max-term term-list))
					 (cdr (terms term-list))))
	   (empty-termlist-p (term-list) (null (terms term-list)))
	   (zeroes (n)
	     (if (= n 0)
		 nil
		 (cons (make-lisp-number 0) (zeroes (1- n)))))
	   (adjoin-term (term term-list)
	     (if (=zerop (coeff term))
		 term-list
		 (make-term-list (cons (coeff term)
				       (append (zeroes (- (order term)
							  (max-term term-list)
							  1))
					       (terms term-list))))))
	   (tag (p) (attach-tag 'dense p)))
    (put 'the-empty-termlist 'dense
	 (lambda () (tag (the-empty-termlist))))
    (put 'first-term '(dense) #'first-term)
    (put 'rest-terms '(dense)
	 (lambda (term-list) (tag (rest-terms term-list))))
    (put 'empty-termlist-p '(dense) #'empty-termlist-p)
    (put 'adjoin-term 'dense
	 (lambda (term term-list) (tag (adjoin-term term term-list))))
    (put 'make 'dense
	 (lambda (term-list) (tag (make-term-list term-list))))
    'done))

;;; Let's install it:
(install-dense-polynomial-package)

(defun make-dense-term-list (term-list)
  (funcall (%get 'make 'dense) term-list))

(defun install-clever-polynomial-package ()
  (labels ((make-poly (variable term-list)
	     (cons variable term-list))
	   (make-sparse-poly (variable term-list)
	     (cons variable (make-sparse-term-list term-list)))
	   (make-dense-poly (variable term-list)
	     (cons variable (make-dense-term-list term-list)))
	   (variable (p) (car p))
	   (term-list (p) (cdr p))
	   (variablep (x) (symbolp x))
	   (same-variable-p (v1 v2)
	     (and (variablep v1) (variablep v2) (eq v1 v2)))
	   (add-terms (L1 L2)
	     (cond ((empty-termlist-p L1) L2)
		   ((empty-termlist-p L2) L1)
		   (t (let ((t1 (first-term L1))
			    (t2 (first-term L2)))
			(cond ((> (order t1) (order t2))
			       (adjoin-term t1 (add-terms (rest-terms L1) L2)))
			      ((< (order t1) (order t2))
			       (adjoin-term t2 (add-terms L1 (rest-terms L2))))
			      (t (adjoin-term
				  (make-term (order t1)
					     (add (coeff t1) (coeff t2)))
				  (add-terms (rest-terms L1)
					     (rest-terms L2)))))))))
	   (add-poly (p1 p2)
	     (if (same-variable-p (variable p1) (variable p2))
		 (make-poly (variable p1)
			    (add-terms (term-list p1) (term-list p2)))
		 (error "Polys ~a, ~a not in same var -- ADD-POLY" p1 p2)))
	   (negate-terms (term-list)
	     (if (empty-termlist-p term-list)
		 term-list
		 (let ((term (first-term term-list)))
		   (adjoin-term (make-term (order term) (negate (coeff term)))
				(negate-terms (rest-terms term-list))))))
	   (negate-poly (p)
	     (let ((terms (term-list p)))
	       (make-poly (variable p) (negate-terms terms))))
	   (sub-terms (L1 L2)
	     (cond ((empty-termlist-p L1) (negate-terms L2))
		   ((empty-termlist-p L2) L1)
		   (t (let ((t1 (first-term L1))
			    (t2 (first-term L2)))
			(cond ((> (order t1) (order t2))
			       (adjoin-term t1 (sub-terms (rest-terms L1) L2)))
			      ((< (order t1) (order t2))
			       (adjoin-term (make-term (order t2)
						       (negate (coeff t2)))
					    (sub-terms L1 (rest-terms L2))))
			      (t (adjoin-term
				  (make-term (order t1)
					     (sub (coeff t1) (coeff t2)))
				  (sub-terms (rest-terms L1)
					     (rest-terms L2)))))))))
	   (sub-poly (p1 p2)
	     (if (same-variable-p (variable p1) (variable p2))
		 (make-poly (variable p1)
			    (sub-terms (term-list p1) (term-list p2)))
		 (error "Polys ~a, ~a not in same var -- SUB-POLY" p1 p2)))
	   (mul-term-by-all-terms (t1 L type)
	     (if (empty-termlist-p L)
		 (the-empty-termlist type)
		 (let ((t2 (first-term L)))
		   (adjoin-term
		    (make-term (+ (order t1) (order t2))
			       (mul (coeff t1) (coeff t2)))
		    (mul-term-by-all-terms t1 (rest-terms L) type)))))
	   (mul-terms (L1 L2 type)
	     (if (empty-termlist-p L1)
		 (the-empty-termlist type)
		 (add-terms (mul-term-by-all-terms (first-term L1) L2 type)
			    (mul-terms (rest-terms L1) L2 type))))
	   (choose-representation (p1 p2) ; use sprase only if both are sparse
	     (if (or (eq (type-tag (term-list p1)) 'dense)
		     (eq (type-tag (term-list p2)) 'dense))
		 'dense
		 'sparse))
	   (mul-poly (p1 p2)
	     (if (same-variable-p (variable p1) (variable p2))
		 (make-poly (variable p1)
			    (mul-terms (term-list p1) (term-list p2)
				       (choose-representation p1 p2)))
		 (error "Polys ~a, ~a not in same var -- MUL-POLY" p1 p2)))
	   (mul-term-termlist (term term-list type)
	     (mul-terms (adjoin-term term (the-empty-termlist type))
			term-list type))
	   (div-terms (L1 L2 type)
	     (if (empty-termlist-p L1)
		 (list (the-empty-termlist type) (the-empty-termlist type))
		 (let ((t1 (first-term L1))
		       (t2 (first-term L2)))
		   (if (> (order t2) (order t1))
		       (list (the-empty-termlist type) L1)
		       (let* ((new-c (div (coeff t1) (coeff t2)))
			      (new-o (- (order t1) (order t2)))
			      (new-term (make-term new-o new-c))
			      (rest-of-result
			       (div-terms
				(sub-terms
				 L1 (mul-term-termlist new-term L2 type))
				L2 type)))
			 (cons (adjoin-term new-term (car rest-of-result))
			       (cdr rest-of-result)))))))
	   (div-poly (p1 p2)
	     (if (same-variable-p (variable p1) (variable p2))
		 (let ((q-r (div-terms (term-list p1) (term-list p2)
				       (choose-representation p1 p2))))
		   (list (make-poly (variable p1) (car q-r))
			 (make-poly (variable p1) (cadr q-r))))
		 (error "Polys ~a, ~a not in same var -- DIV-POLY" p1 p2)))
	   (just-zero-terms (terms)
	     (or (empty-termlist-p terms)
		 (and (=zerop (coeff (first terms)))
		      (just-zero-terms (cdr terms)))))
	   (=zerop-poly (p)
	     (just-zero-terms (term-list p)))
	   (tag (p) (attach-tag 'clever-polynomial p)))
    (put 'add '(clever-polynomial clever-polynomial)
	 (lambda (p1 p2) (tag (add-poly p1 p2))))
    (put 'negate '(clever-polynomial)
	 (lambda (p) (tag (negate-poly p))))
    (put 'sub '(clever-polynomial clever-polynomial)
	 (lambda (p1 p2) (tag (sub-poly p1 p2))))
    (put 'mul '(clever-polynomial clever-polynomial)
	 (lambda (p1 p2) (tag (mul-poly p1 p2))))
    (put 'div '(clever-polynomial clever-polynomial)
	 (lambda (p1 p2)
	   (let ((quotient-remainder (div-poly p1 p2)))
	     (list (tag (car quotient-remainder))
		   (tag (cadr quotient-remainder))))))
    (put '=zerop '(clever-polynomial) #'=zerop-poly)
    (put 'make-sparse 'clever-polynomial
	 (lambda (var terms) (tag (make-sparse-poly var terms))))
    (put 'make-dense 'clever-polynomial
	 (lambda (var terms) (tag (make-dense-poly var terms))))
    'done))

;;; Let's install it:
(install-clever-polynomial-package)

(defun make-sparse-polynomial (var terms)
  (funcall (%get 'make-sparse 'clever-polynomial) var terms))
(defun make-dense-polynomial (var terms)
  (funcall (%get 'make-dense 'clever-polynomial) var terms))

;;; Exercise 2.89-90 END

;;; Exercise 2.92 START

;;; Let's see the reordering algorithm, the rest is almost automatic work.
;;; We take the simplest case, when every coefficient is a polynom of the
;;; other variable (every other case can be simplified to this one).

;;; This code should go in the LABELS section of install-polynomial-package.
;;; Now I write them here with DEFUNs:
#+nil
(defun collect (order terms x)
  "Collects the coefficient of y^ORDER from TERMS."
  (if (null terms)
      (make-polynomial x '())
      (let ((y-terms (term-list (coeff (first-term terms)))))
	(if (and (not (empty-termlist-p y-terms))
		 (= (order (first-term y-terms)) order))
	    (add (let ((new-term (make-term (order (first-term terms))
					    (coeff (first-term y-terms)))))
		   (make-polynomial
		    x (adjoin-term new-term (the-empty-termlist))))
		 (collect order (rest-terms terms) x))
	    (collect order (rest-terms terms) x)))))
#+nil
(defun max-order (terms &optional (max -1))
  "Returns the maximal order of the coefficient polynoms."
  (if (empty-termlist-p terms)
      max
      (let ((y-terms (term-list (coeff (first-term terms)))))
	(if (empty-termlist-p y-terms)
	    (max-order (rest-terms terms) max)
	    (let ((order (order (first-term y-terms))))
	      (max-order (rest-terms terms) (if (> order max) order max)))))))
#+nil
(defun mapcar-termlist (function terms)
  "Like MAPCAR, but on term lists."
  (if (empty-termlist-p terms)
      (the-empty-termlist)
      (adjoin-term (funcall function (first-term terms))
		   (mapcar-termlist function (rest-terms terms)))))
#+nil
(defun reorder-rec (terms x y)
  "The main function that deletes the processed y-terms."
  (let ((max (max-order terms)))
    (if (< max 0)
	(make-polynomial y '())
	(add (let ((new-term (make-term max (collect max terms x))))
	       (make-polynomial y (adjoin-term new-term (the-empty-termlist))))
	     (reorder-rec
	      (mapcar-termlist
	       (lambda (term)
		 (let ((y-terms (term-list (coeff term))))
		   (if (and (not (empty-termlist-p y-terms))
			    (= (order (first-term y-terms)) max))
		       (make-term (order term)
				  (make-polynomial y (rest-terms y-terms)))
		       term)))
	       terms)
	      x y)))))
#+nil
(defun reorder-poly (p)
  "Just calls REORDER-REC."
  (reorder-rec (term-list p) (variable p)
	       (variable (coeff (first-term (term-list p))))))

;;; The actual ordering of the variables can be, for example, alphabetical:
(defun variable-less (v1 v2)
  (and (string< (symbol-name v1) (symbol-name v2)) t))

;;; Exercise 2.92 END

;;; Exercise 2.93/97 START

(defun install-generic-rational-package ()
  (labels ((numer (x) (car x))
	   (denom (x) (cdr x))
	   (make-rat (n d)
	     (let ((reduced (%reduce n d)))
	       (cons (car reduced) (cadr reduced))))
	   (add-rat (x y)
	     (make-rat (add (mul (numer x) (denom y))
			    (mul (numer y) (denom x)))
		       (mul (denom x) (denom y))))
	   (negate-rat (x)
	     (make-rat (negate (numer x)) (denom x)))
	   (sub-rat (x y)
	     (make-rat (sub (mul (numer x) (denom y))
			    (mul (numer y) (denom x)))
		       (mul (denom x) (denom y))))
	   (mul-rat (x y)
	     (make-rat (mul (numer x) (numer y))
		       (mul (denom x) (denom y))))
	   (div-rat (x y)
	     (make-rat (mul (numer x) (denom y))
		       (mul (denom x) (numer y))))
	   (equp-rat (x y)
	     (and (equp (numer x) (numer y))
		  (equp (denom x) (denom y))))
	   (=zerop-rat (x) (=zerop (numer x)))
	   (tag (x) (attach-tag 'generic-rational x)))
    (put 'add '(generic-rational generic-rational)
	 (lambda (x y) (tag (add-rat x y))))
    (put 'negate '(generic-rational)
	 (lambda (x) (tag (negate-rat x))))
    (put 'sub '(generic-rational generic-rational)
	 (lambda (x y) (tag (sub-rat x y))))
    (put 'mul '(generic-rational generic-rational)
	 (lambda (x y) (tag (mul-rat x y))))
    (put 'div '(generic-rational generic-rational)
	 (lambda (x y) (tag (div-rat x y))))
    (put 'equp '(generic-rational generic-rational) #'equp-rat)
    (put '=zerop '(generic-rational) #'=zerop-rat)
    (put 'make 'generic-rational
	 (lambda (n d) (tag (make-rat n d))))
    'done))

;;; Let's install it:
(install-generic-rational-package)

(defun make-generic-rational (n d)
  (funcall (%get 'make 'generic-rational) n d))

;;; Exercise 2.93/97 END

;;; Exercise 2.95 START

;;; The GCD algorithm works only for integers, and there are fractions in
;;; the example.

;;; Exercise 2.95 END


;;; Section 3.1.1

(defparameter *balance* 100)

(defun withdraw (amount)
  (if (>= *balance* amount)
      (progn (setq *balance* (- *balance* amount))
	     *balance*)
      "Insufficient funds"))

(let ((balance 100))
  (defun new-withdraw (amount)
    (if (>= balance amount)
	(progn (setq balance (- balance amount))
	       balance)
	"Insufficient funds")))

(defun make-withdraw (balance)
  (lambda (amount)
    (if (>= balance amount)
	(progn (setq balance (- balance amount))
	       balance)
	"Insufficient funds")))

(defun make-account (balance)
  (labels ((withdraw (amount)
	     (if (>= balance amount)
		 (progn (setq balance (- balance amount))
			balance)
		 "Insufficient funds"))
	   (deposit (amount)
	     (setq balance (+ balance amount))
	     balance)
	   (dispatch (m)
	     (cond ((eq m 'withdraw) #'withdraw)
		   ((eq m 'deposit) #'deposit)
		   (t (error "Unknown request ~a -- MAKE-ACCOUNT" m)))))
    #'dispatch))

;;; Exercise 3.1 START

(defun make-accumulator (sum)
  (lambda (x)
    (incf sum x)))

;;; Exercise 3.1 END

;;; Exercise 3.2 START

(defun make-monitored (f)
  (let ((count 0))
    (lambda (x)
      (case x
	(how-many-calls-p count)
	(reset-count (setq count 0))
	(t (incf count)
	   (funcall f x))))))

;;; Exercise 3.2 END

;;; Exercise 3.3 START

(defun make-protected-account (balance password)
  (labels ((withdraw (amount)
	     (if (>= balance amount)
		 (progn (setq balance (- balance amount))
			balance)
		 "Insufficient funds"))
	   (deposit (amount)
	     (setq balance (+ balance amount))
	     balance)
	   (dispatch (pw m)
	     (if (eq pw password)
		 (cond ((eq m 'withdraw) #'withdraw)
		       ((eq m 'deposit) #'deposit)
		       (t (error "Unknown request ~a -- MAKE-PROTECTED-ACCOUNT"
				 m)))
		 (lambda (x)
		   (declare (ignore x))
		   "Incorrect password"))))
    #'dispatch))

;;; Exercise 3.3 END

;;; Exercise 3.4 START

#+nil
(defun make-protected-account (balance password)
  (let ((count 0))
    (labels (...
	     (dispatch (pw m)
	       (cond ((eq pw password)
		      (setq count 0)
		      ...)
		     ((<= count 7)
		      ...)
		     (t (call-the-cops)))))
      #'dispatch)))

;;; Exercise 3.4 END


;;; Section 3.1.2

;;; A quick hack to make the examples work
(defvar *random-init*)
(let* ((n 10000)
       (cache (make-array n :element-type 'fixnum)))
  (dotimes (i n)
    (setf (elt cache i) i))
  (dotimes (i (1- n))
    (let ((j (random (- n i 1))))
      (rotatef (elt cache i) (elt cache (+ i j 1)))))
  (setf *random-init* (elt cache 0))
  (defun rand-update (x)
    (elt cache (mod (1+ (position x cache)) n))))

(let ((x *random-init*))
  (defun rand ()
    (setq x (rand-update x))))

(defun monte-carlo (trials experiment)
  (labels ((it (trials-remaining trials-passed)
	     (cond ((= trials-remaining 0)
		    (/ trials-passed trials))
		   ((funcall experiment)
		    (it (- trials-remaining 1) (+ trials-passed 1)))
		   (t (it (- trials-remaining 1) trials-passed)))))
    (it trials 0)))
(defun cesaro-test ()
  (= (gcd (rand) (rand)) 1))
(defun estimate-pi (trials)
  (sqrt (/ 6 (monte-carlo trials #'cesaro-test))))

(defun random-gcd-test (trials initial-x)
  (labels ((it (trials-remaining trials-passed x)
	     (let ((x1 (rand-update x)))
	       (let ((x2 (rand-update x1)))
		 (cond ((= trials-remaining 0)
			(/ trials-passed trials))
		       ((= (gcd x1 x2) 1)
			(it (- trials-remaining 1) (+ trials-passed 1) x2))
		       (t (it (- trials-remaining 1) trials-passed x2)))))))
    (it trials 0 initial-x)))
(defun estimate-pi-non-modular (trials)
  (sqrt (/ 6 (random-gcd-test trials *random-init*))))

;;; Exercise 3.5 START

(defun random-in-range (low high)
  (let ((range (- high low)))
    (+ low (random range))))

(defun estimate-integral (pred x1 y1 x2 y2 trials)
  (let ((xlen (- x2 x1))
	(ylen (- y2 y1)))
    (flet ((insidep ()
	     (funcall pred (+ x1 (random xlen)) (+ y1 (random ylen)))))
      (* xlen ylen (monte-carlo trials #'insidep)))))

(defun estimate-pi-by-integration (trials)
  (/ (estimate-integral (lambda (x y) (<= (+ (* x x) (* y y)) #.(* 500 500)))
			-500 -500 500 500 trials)
     #.(* 500.0 500.0)))

;;; Exercise 3.5 END

;;; Exercise 3.6 START

(let ((x *random-init*))
  (defun resetable-rand (command)
    (ecase command
      (generate (setq x (rand-update x)))
      (reset (lambda (new-x) (setq x new-x) t)))))

;;; Exercise 3.6 END


;;; Section 3.1.3

(defun make-simplified-withdraw (balance)
  (lambda (amount)
    (setq balance (- balance amount))
    balance))

(defun make-decrementer (balance)
  (lambda (amount)
    (- balance amount)))

(defun factorial-imperative (n)
  (let ((product 1)
	(counter 1))
    (labels ((iter ()
		   (if (> counter n)
		       product
		       (progn (setq product (* counter product))
			      (setq counter (+ counter 1))
			      (iter)))))
      (iter))))

;;; Exercise 3.7 START

(defun make-joint (old-acc old-pass password)
  (flet ((dispatch (pw m)
	   (if (eq pw password)
	       (funcall old-acc old-pass m)
	       (lambda (x)
		 (declare (ignore x))
		 "Incorrect password"))))
    #'dispatch))

;;; Exercise 3.7 END

;;; Exercise 3.8 START

(let ((x 0))
  (defun f-6 (n)
    (prog1 x
      (setq x n))))

;;; Exercise 3.8 END


;;; Section 3.2

;;; Exercise 3.9 START

;;; Recursive version:
;;; ------------------
;;                                      OO+
;;                                     /  |
;;                                    /   V
;;                    +--------------/----------------------------+
;; global environment | factorial --/                             |
;;                    |                                           |
;; (factorial 6)      |                                           |
;;                    +-------------------------------------------+
;;                      ^           ^           ^               ^
;;                   +-----+     +-----+     +-----+         +-----+
;;               E1->| n 6 | E2->| n 5 | E3->| n 6 | ... E6->| n 1 |
;;                   +-----+     +-----+     +-----+         +-----+

;;; Iterative version:
;;; ------------------
;;                                      OO+      OO+
;;                                     /  |     /  |
;;                                    /   V    /   V
;;                    +--------------/--------/--------------------------+
;; global environment | factorial --/        /                           |
;;                    | fact-iter ----------/                            |
;; (factorial 6)      |                                                  |
;;                    +--------------------------------------------------+
;;                      ^               ^                         ^
;;                   +-----+     +-------------+          +---------------+
;;                   |     |     | product   1 |          | product   720 |
;;               E1->| n 6 | E2->| counter   1 | ...  E8->| counter     7 |
;;                   |     |     | max-count 6 |          | max-count   6 |
;;                   +-----+     +-------------+          +---------------+

;;; Exercise 3.9 END

;;; Exercise 3.10 START

;;                                               OO+
;;                                              /  |
;;                                             /   V
;;                    +-----------------------/--------------------------+
;; global environment | make-withdraw -------/                           |
;;                    | W1 ------------------------+                     |
;;                    |                            |                     |
;;                    +----------------------------|---------------------+
;;                      ^                          |
;;            +--------------------+               |
;;        E1->| initial-amount 100 |<---\          |
;;            +--------------------+    |          |
;;                      ^       |       |          |
;;                      |       +------OO (let)    |
;;            +--------------------+               |
;;        E2->| balance        100 |<-------------OO
;;            +--------------------+
;;                      ^
;;            +--------------------+
;;        E3->| amount          50 |
;;            +--------------------+

;;                                               OO+
;;                                              /  |
;;                                             /   V
;;                    +-----------------------/--------------------------+
;; global environment | make-withdraw -------/                           |
;;                    | W1 ---------------------+                        |
;;                    | W2 ---------------------|-+                      |
;;                    +-------------------------|-|----------------------+
;;                      ^                       | |             ^
;;            +--------------------+            | |   +--------------------+
;;        E1->| initial-amount 100 |<---\       | |   | initial-amount 100 |<-E4
;;            +--------------------+    |       | |   +--------------------+
;;                      ^       |       |       | |             ^
;;                      |       +------OO (let) | |             |
;;            +--------------------+            | |   +--------------------+
;;        E2->| balance         50 |<----------OO OO->| balance        100 |<-E5
;;            +--------------------+                  +--------------------+

;;; Exercise 3.10 END

;;; Exercise 3.11 START

;;                                               OO+
;;                                              /  |
;;                                             /   V
;;                    +-----------------------/--------------------------+
;; global environment | make-account --------/                           |
;;                    | acc ------------+                                |
;;                    +-----------------|--------------------------------+
;;                      ^               |
;;            +------------+            |
;;            | balance 50 |<-----------|-----+
;;            | dispatch --+-----+      |     |
;;        E1->| deposit ---+---+ |      |     |
;;            | withdraw --+-+ | |      |     |
;;            +------------+ | | |      |     |
;;              ^ ^  ^  ^    | | |      |     |
;;              | |  |  |    | | |      |     |
;;              | \  \  \_OO_/ / /      |     |
;;              |  \  \___OO__/ /      /      |
;;              |   \_____OO___/      /	      |
;;              |          ^         /	      |
;;              |          \________/	      |
;;              |			      |
;;            +------------+		      |
;;        E2->| m 'deposit |		      |
;;            +------------+		      |
;;					      |
;;                   +------------------------+
;;                   |
;;            +------------+
;;        E3->| amount  50 |
;;            +------------+

;;; (same for the withdraw operation)
;;; The local state of ACC is kept in E1.
;;; If we define a new account, it will have its own environment,
;;; only the global environment will be shared between acc and acc2.

;;; Exercise 3.11 END


;;; Section 3.3.1

;;; Exercise 3.12 START

;;; After APPEND:
;;; => (B)

;; X:                        Y:
;; +---+---+    +---+---+    +---+---+    +---+---+
;; | * | *----->| * | / |    | * | *----->| * | / |
;; +-|-+---+    +-|-+---+    +-|-+---+    +-|-+---+
;;   |            |	       |            |
;;   V            V	       V            V
;; +---+        +---+	     +---+        +---+
;; | A |        | B |	     | C |        | D |
;; +---+        +---+        +---+        +---+
;;
;; Z:
;; +---+---+    +---+---+    +---+---+    +---+---+
;; | * | *----->| * | *----->| * | *----->| * | / |
;; +-|-+---+    +-|-+---+    +-|-+---+    +-|-+---+
;;   |            |	       |            |
;;   V            V	       V            V
;; +---+        +---+	     +---+        +---+
;; | A |        | B |	     | C |        | D |
;; +---+        +---+        +---+        +---+

;;; After NCONC:
;;; => (B C D)

;; X, W:                     Y:
;; +---+---+    +---+---+    +---+---+    +---+---+
;; | * | *----->| * | *----->| * | *----->| * | / |
;; +-|-+---+    +-|-+---+    +-|-+---+    +-|-+---+
;;   |            |	       |            |
;;   V            V	       V            V
;; +---+        +---+	     +---+        +---+
;; | A |        | B |	     | C |        | D |
;; +---+        +---+        +---+        +---+

;;; Exercise 3.12 END

;;; Exercise 3.13 START

;;   +----------------------------------+
;;   |                                  |
;;   V                                  |
;; +---+---+    +---+---+    +---+---+  |
;; | * | *----->| * | *----->| * | *----+
;; +-|-+---+    +-|-+---+    +-|-+---+
;;   |            |	       |
;;   V            V	       V
;; +---+        +---+	     +---+
;; | A |        | B |	     | C |
;; +---+        +---+        +---+

;;; LAST-PAIR would get in an infinite recursion with a cyclic list.

;;; Exercise 3.13 END

;;; Exercise 3.14 START

;;; MYSTERY reverses a list, meanwhile altering the original sequence.
;;; V: (A)
;;; W: (D C B A)

;;; Before:
;;
;; V:
;; +---+---+    +---+---+    +---+---+    +---+---+
;; | * | *----->| * | *----->| * | *----->| * | / |
;; +-|-+---+    +-|-+---+    +-|-+---+    +-|-+---+
;;   |            |	       |            |
;;   V            V	       V            V
;; +---+        +---+	     +---+        +---+
;; | A |        | B |	     | C |        | D |
;; +---+        +---+        +---+        +---+

;;; After:
;;                   +----------------+
;;      +------------|---+        +---|------------+
;;      V            V   |        V   |            |
;;    +---+---+    +---+-|-+    +---+-|-+    +---+-|-+
;; V: | * | / |    | * | * |    | * | * | W: | * | * |
;;    +-|-+---+    +-|-+---+    +-|-+---+    +-|-+---+
;;      |            |            |            |
;;      V            V            V            V
;;    +---+        +---+        +---+        +---+
;;    | A |        | B |        | C |        | D |
;;    +---+        +---+        +---+        +---+

;;; Exercise 3.14 END

;;; Exercise 3.15 START

;;           +---+---+
;; Z1 ------>| * | * |
;;           +-|-+-|-+
;;             |   |
;;             V   V
;;           +---+---+    +---+---+
;;  X ------>| * | *----->| * | / |
;;           +-|-+---+    +-|-+---+
;; +-----+     |            |
;; | WOW |<----+            V
;; +-----+   +---+        +---+
;;           | A |        | B |
;;           +---+        +---+

;;           +---+---+    +---+---+    +---+---+
;; Z2 ------>| * | *----->| * | *----->| * | / |
;;           +-|-+---+    +-|-+---+    +-|-+---+
;;             |            |            |
;;             |            V            V
;;             | +-----+  +---+        +---+
;;             | | WOW |  | A |        | B |
;;             | +-----+  +---+        +---+
;;             |    ^                    ^
;;             |    +-------+            |
;;             |          +-|-+---+    +-|-+---+
;;             +----------| * | *----->| * | / |
;;                        +---+---+    +---+---+

;;; Exercise 3.15 END

;;; Exercise 3.16 START

;;; 3:
;;           +---+---+    +---+---+    +---+---+
;; X1 ------>| * | *----->| * | *----->| * | / |
;;           +-|-+---+    +-|-+---+    +-|-+---+
;;             |            |            |
;;             V            V            V
;;           +---+        +---+        +---+
;;           | A |        | B |        | C |
;;           +---+        +---+        +---+

;;; 4:
;;                          +------------+
;;                          |            V
;;           +---+---+    +-|-+---+    +---+---+
;; X2 ------>| * | *----->| * | *----->| * | / |
;;           +-|-+---+    +---+---+    +-|-+---+
;;             |                         |
;;             V                         V
;;           +---+                     +---+
;;           | A |                     | C |
;;           +---+                     +---+

;;; 7:
;;                          +------------+
;;                          |            V
;;           +---+---+    +-|-+---+    +---+---+
;; X3 ------>| * | *----->| * | *----->| * | / |
;;           +-|-+---+    +---+---+    +-|-+---+
;;             |            ^            |
;;             +------------+            V
;;                                     +---+
;;                                     | C |
;;                                     +---+

;;; Never:
;;                          +---+
;;                          |   V|
;;           +---+---+    +-|-+---+    +---+---+
;; X4 ------>| * | *----->| * | *----->| * | / |
;;           +-|-+---+    +---+---+    +-|-+---+
;;             |                         |
;;             V                         V
;;           +---+                     +---+
;;           | A |                     | C |
;;           +---+                     +---+

;;; Exercise 3.16 END

;;; Exercise 3.17 START

(defun count-pairs (x)
  (let ((visited '()))
    (labels ((rec (y)
	       (cond ((or (atom y) (member y visited :test #'eq)) 0)
		     (t (push y visited)
			(+ (rec (car y)) (rec (cdr y)) 1)))))
      (rec x))))

;;; Exercise 3.17 END

;;; Exercise 3.18 START

(defun cyclicp (x &optional acc)
  (unless (null x)
    (or (and (member x acc :test #'eq) t)
	(cyclicp (cdr x) (cons x acc)))))

;;; Exercise 3.18 END

;;; Exercise 3.19 START

(defun clever-cyclic-p (x)
  (labels ((rec (slow fast)
	     (cond ((null fast) nil)
		   ((eq slow fast) t)
		   (t (rec (cdr slow) (cddr fast))))))
    (unless (atom x)
      (rec (cdr x) (cddr x)))))

;;; Exercise 3.19 END

(defun cons-4 (x y)
  "Procedural representation of a mutable pair."
  (labels ((set-x (v) (setq x v))
	   (set-y (v) (setq y v))
	   (dispatch (m)
	     (cond ((eq m 'car) x)
		   ((eq m 'cdr) y)
		   ((eq m 'set-car) #'set-x)
		   ((eq m 'set-cdr) #'set-y)
		   (t (error "Undefined operation ~a -- CONS-4" m)))))
    #'dispatch))
(defun car-4 (z)
  "Procedural representation of a mutable pair."
  (funcall z 'car))
(defun cdr-4 (z)
  "Procedural representation of a mutable pair."
  (funcall z 'cdr))
(defun set-car (z new-value)
  (funcall (funcall z 'set-car) new-value)
  z)
(defun set-cdr (z new-value)
  (funcall (funcall z 'set-cdr) new-value)
  z)

;;; Exercise 3.20 START

;;; Skip this one.

;;; Exercise 3.20 END


;;; Section 3.3.2

(defun front-ptr (queue) (car queue))
(defun rear-ptr (queue) (cdr queue))
(defun set-front-ptr (queue item) (rplaca queue item))
(defun set-rear-ptr (queue item) (rplacd queue item))
(defun empty-queue-p (queue) (null (front-ptr queue)))
(defun make-queue () (cons '() '()))
(defun front-queue (queue)
  (if (empty-queue-p queue)
      (error "FRONT called with an empty queue")
      (car (front-ptr queue))))
(defun insert-queue (queue item)
  (let ((new-pair (cons item '())))
    (cond ((empty-queue-p queue)
	   (set-front-ptr queue new-pair)
	   (set-rear-ptr queue new-pair))
	  (t
	   (rplacd (rear-ptr queue) new-pair)
	   (set-rear-ptr queue new-pair))))
  queue)
(defun delete-queue (queue)
  (cond ((empty-queue-p queue)
	 (error "DELETE called with an empty queue"))
	(t
	 (set-front-ptr queue (cdr (front-ptr queue)))
	 queue)))

;;; Exercise 3.21 START

;;; The second instance of the last item is actually the same element
;;; printed twice - once as the last element of the list starting at
;;; front-ptr, and once as the first (and last) element of the list
;;; starting at rear-ptr. When the queue becomes empty, the rear-ptr
;;; may retain the last value, but is of no consequence.

(defun print-queue (queue)
  (format t "<~{~a~^ ~}>~%" (front-ptr queue)))

;;; Exercise 3.21 END

;;; Exercise 3.22 START

(defun make-queue-2 ()
  "With dispatch."
  (let ((front-ptr '())
	(rear-ptr '()))
    (labels ((empty-queue-p () (null front-ptr))
	     (front-queue ()
	       (if (empty-queue-p)
		   (error "FRONT called with an empty queue")
		   (car front-ptr)))
	     (insert-queue (item)
	       (let ((new-pair (cons item '())))
		 (cond ((empty-queue-p)
			(setq front-ptr new-pair)
			(setq rear-ptr new-pair))
		       (t
			(rplacd rear-ptr new-pair)
			(setq rear-ptr new-pair)))))
	     (delete-queue ()
	       (if (empty-queue-p)
		   (error "DELETE called with an empty queue")
		   (setq front-ptr (cdr front-ptr))))
	     (print-queue ()
	       (format t "<~{~a~^ ~}>~%" front-ptr))
	     (dispatch (m)
	       (cond ((eq m 'empty-queue-p) #'empty-queue-p)
		     ((eq m 'front-queue) #'front-queue)
		     ((eq m 'insert-queue) #'insert-queue)
		     ((eq m 'delete-queue) #'delete-queue)
		     ((eq m 'print-queue) #'print-queue)
		     (t (error "Unknown request ~a -- MAKE-QUEUE-2" m)))))
      #'dispatch)))

;;; Exercise 3.22 END

;;; Exercise 3.23 START

;;; Using a doubly linked list, and some of queue's low-level interface
;;; (rear-ptr points to a tailing item (REAR) after the last element)
(defun make-dlink (item) (cons item (cons nil nil)))
(defun dlink-value (item) (car item))
(defun dlink-previous (item) (cadr item))
(defun dlink-next (item) (cddr item))
(defun dlink-set-previous (item previous) (rplaca (cdr item) previous))
(defun dlink-set-next (item next) (rplacd (cdr item) next))
(defun empty-deque-p (deque)
  (or (null (front-ptr deque))
      (null (dlink-previous (rear-ptr deque)))))
(defun make-deque () (cons '() (make-dlink 'rear)))
(defun front-deque (deque)
  (if (empty-deque-p deque)
      (error "FRONT called with an empty deque")
      (dlink-value (front-ptr deque))))
(defun rear-deque (deque)
  (if (empty-deque-p deque)
      (error "REAR called with an empty deque")
      (dlink-value (dlink-previous (rear-ptr deque)))))
(defun front-insert-deque (deque item)
  (let ((new-dlink (make-dlink item)))
    (cond ((empty-deque-p deque)
	   (set-front-ptr deque new-dlink)
	   (dlink-set-next new-dlink (rear-ptr deque))
	   (dlink-set-previous (rear-ptr deque) new-dlink))
	  (t
	   (dlink-set-next new-dlink (front-ptr deque))
	   (dlink-set-previous (front-ptr deque) new-dlink)
	   (set-front-ptr deque new-dlink))))
  t)
(defun rear-insert-deque (deque item)
  (let ((new-dlink (make-dlink item)))
    (cond ((empty-deque-p deque)
	   (set-front-ptr deque new-dlink)
	   (dlink-set-previous (rear-ptr deque) new-dlink))
	  (t
	   (dlink-set-next (dlink-previous (rear-ptr deque)) new-dlink)
	   (dlink-set-next new-dlink (rear-ptr deque))
	   (dlink-set-previous new-dlink (dlink-previous (rear-ptr deque)))
	   (dlink-set-previous (rear-ptr deque) new-dlink))))
  t)
(defun front-delete-deque (deque)
  (cond ((empty-deque-p deque)
	 (error "FRONT-DELETE called with an empty deque"))
	(t
	 (set-front-ptr deque (dlink-next (front-ptr deque)))
	 (dlink-set-previous (front-ptr deque) nil)
	 t)))
(defun rear-delete-deque (deque)
  (cond ((empty-deque-p deque)
	 (error "REAR-DELETE called with an empty deque"))
	(t
	 (dlink-set-previous (rear-ptr deque)
			     (dlink-previous (dlink-previous (rear-ptr deque))))
	 (dlink-set-next (dlink-previous (rear-ptr deque)) (rear-ptr deque))
	 t)))
(defun print-deque (deque)
  (princ #\<)
  (if (empty-deque-p deque)
      (format t "Empty deque")
      (do ((ptr (front-ptr deque) (dlink-next ptr)))
	  ((eq ptr (rear-ptr deque)))
	(princ (dlink-value ptr))
	(unless (eq (dlink-next ptr) (rear-ptr deque))
	  (princ #\Space))))
  (princ #\>)
  (terpri))

;;; Exercise 3.23 END


;;; Section 3.3.3

(defun lookup (key table)
  (let ((record (assoc key (cdr table))))
    (when record
      (cdr record))))
(defun insert (key value table)
  (let ((record (assoc key (cdr table))))
    (if record
	(rplacd record value)
	(rplacd table (cons (cons key value) (cdr table)))))
  'ok)
(defun make-table () (list '*table*))

(defun lookup-2 (key-1 key-2 table)
  "Two-dimensional tables."
  (let ((subtable (assoc key-1 (cdr table))))
    (when subtable
      (let ((record (assoc key-2 (cdr subtable))))
	(when record
	  (cdr record))))))
(defun insert-2 (key-1 key-2 value table)
  "Two-dimensional tables."
  (let ((subtable (assoc key-1 (cdr table))))
    (if subtable
	(let ((record (assoc key-2 (cdr subtable))))
	  (if record
	      (rplacd record value)
	      (rplacd subtable (cons (cons key-2 value) (cdr subtable)))))
	(rplacd table (cons (list key-1 (cons key-2 value)) (cdr table)))))
  'ok)

(defun make-table-2 ()
  "Using dispatch."
  (let ((local-table (list '*table*)))
    (labels ((lookup (key-1 key-2)
	       (let ((subtable (assoc key-1 (cdr local-table))))
		 (when subtable
		   (let ((record (assoc key-2 (cdr subtable))))
		     (when record
		       (cdr record))))))
	     (insert (key-1 key-2 value)
	       (let ((subtable (assoc key-1 (cdr local-table))))
		 (if subtable
		     (let ((record (assoc key-2 (cdr subtable))))
		       (if record
			   (rplacd record value)
			   (rplacd subtable
				   (cons (cons key-2 value)
					 (cdr subtable)))))
		     (rplacd local-table
			     (cons (list key-1
					 (cons key-2 value))
				   (cdr local-table)))))
	       'ok)
	     (dispatch (m)
	       (cond ((eq m 'lookup-proc) #'lookup)
		     ((eq m 'insert-proc) #'insert)
		     (t (error "Unknown operation -- TABLE")))))
      #'dispatch)))

(defvar *operation-table* (make-table-2))
(define get-2
  "Using MAKE-TABLE-2."
  (funcall *operation-table* 'lookup-proc))
(define put-2
  "Using MAKE-TABLE-2."
  (funcall *operation-table* 'insert-proc))

;;; Exercise 3.24 START

(defun make-table-3 (&optional (same-key-p #'equal))
  "Using dispatch and a test function."
  (let ((local-table (list '*table*)))
    (labels ((lookup (key-1 key-2)
	       (let ((subtable (assoc key-1 (cdr local-table)
				      :test same-key-p)))
		 (when subtable
		   (let ((record (assoc key-2 (cdr subtable)
					:test same-key-p)))
		     (when record
		       (cdr record))))))
	     (insert (key-1 key-2 value)
	       (let ((subtable (assoc key-1 (cdr local-table)
				      :test same-key-p)))
		 (if subtable
		     (let ((record (assoc key-2 (cdr subtable)
					  :test same-key-p)))
		       (if record
			   (rplacd record value)
			   (rplacd subtable
				   (cons (cons key-2 value)
					 (cdr subtable)))))
		     (rplacd local-table
			     (cons (list key-1
					 (cons key-2 value))
				   (cdr local-table)))))
	       'ok)
	     (dispatch (m)
	       (cond ((eq m 'lookup-proc) #'lookup)
		     ((eq m 'insert-proc) #'insert)
		     (t (error "Unknown operation -- TABLE")))))
      #'dispatch)))

;;; Exercise 3.24 END

;;; Exercise 3.25 START

(defun make-table-4 ()
  "Using dispatch and an arbitrary number of keys."
  (let ((local-table (list '*table*)))
    (labels ((lookup-rec (keys table)
	       (if (null keys)
		   (cdr table)
		   (let ((subtable (assoc (car keys) (cdr table))))
		     (when subtable
		       (lookup-rec (cdr keys) subtable)))))
	     (lookup (keys)
	       (lookup-rec keys local-table))
	     (create-item (keys value)
	       (if (null (cdr keys))
		   (cons (car keys) value)
		   (list (car keys) (create-item (cdr keys) value))))
	     (insert-rec (keys value table)
	       (if (null keys)
		   (rplacd table value)
		   (let ((subtable (assoc (car keys) (cdr table))))
		     (if subtable
			 (insert-rec (cdr keys) value subtable)
			 (rplacd table
				 (cons (create-item keys value)
				       (cdr table)))))))
	     (insert (keys value)
	       (insert-rec keys value local-table)
	       'ok)
	     (dispatch (m)
	       (cond ((eq m 'lookup-proc) #'lookup)
		     ((eq m 'insert-proc) #'insert)
		     (t (error "Unknown operation -- TABLE")))))
      #'dispatch)))

;;; Exercise 3.25 END

;;; Exercise 3.26 START

;;; Higher dimensional tables can be derived from one-dimensional ones
;;; the same way as in the examples. So let's do just a one-dimensional
;;; binary tree "table".

(defun make-table-5 ()
  "Using dispatch and a binary tree representation"
  (let ((local-tree nil))
    (labels ((key (tree) (caar tree))
	     (value (tree) (cdar tree))
	     (left (tree) (cadr tree))
	     (right (tree) (cddr tree))
	     (lookup-rec (key tree)
	       (cond ((null tree) nil)
		     ((= (key tree) key) (value tree))
		     ((< (key tree) key) (lookup-rec key (left tree)))
		     (t (lookup-rec key (right tree)))))
	     (lookup (key) (lookup-rec key local-tree))
	     (new-item (key value)
	       (cons (cons key value) (cons nil nil)))
	     (insert-rec (key value tree)
	       (cond ((null tree) nil)
		     ((= (key tree) key) (rplacd (car tree) value))
		     ((< (key tree) key)
		      (unless (insert-rec key value (left tree))
			(rplaca (cdr tree) (new-item key value))))
		     (t (unless (insert-rec key value (right tree))
			  (rplacd (cdr tree) (new-item key value))))))
	     (insert (key value)
	       (if (null local-tree)
		   (setq local-tree (new-item key value))
		   (insert-rec key value local-tree))
	       'ok)
	     (dispatch (m)
	       (cond ((eq m 'lookup-proc) #'lookup)
		     ((eq m 'insert-proc) #'insert)
		     (t (error "Unknown operation -- TABLE")))))
      #'dispatch)))

;;; Exercise 3.26 END

;;; Exercise 3.27 START

;;; Skip the diagram drawing...

;;; Without memoization, calculating fib(n) means calculating
;;; fib(n-1) and fib(n-2), which results in O(2^n).
;;; But remembering previously calculated elements of the Fibonacci
;;; series is O(1), so one of two "threads" will not count, since
;;; when we're done with one, we are also done (or almost done)
;;; with the other.

;;; If we just use (memoize fib), the recursive calls will call
;;; fib instead of its memoized version, so there will be no
;;; speedup.

;;; Exercise 3.27 END


;;; Section 3.3.4

(defun make-wire ()
  (let ((signal-value 0)
	(action-procedures '()))
    (labels ((set-my-signal (new-value)
	       (unless (= signal-value new-value)
		 (setq signal-value new-value)
		 (mapcar #'funcall action-procedures))
	       'done)
	     (accept-action-procedure (proc)
	       (setq action-procedures (cons proc action-procedures))
	       (funcall proc))
	     (dispatch (m)
	       (cond ((eq m 'get-signal) signal-value)
		     ((eq m 'set-signal) #'set-my-signal)
		     ((eq m 'add-action) #'accept-action-procedure)
		     (t (error "Unknown operation ~a -- WIRE" m)))))
      #'dispatch)))
(defun get-signal (wire)
  (funcall wire 'get-signal))
(defun set-signal (wire new-value)
  (funcall (funcall wire 'set-signal) new-value))
(defun add-action (wire action-procedure)
  (funcall (funcall wire 'add-action) action-procedure))

(defun make-time-segment (time queue)
  (cons time queue))
(defun segment-time (s) (car s))
(defun segment-queue (s) (cdr s))
(defun make-agenda () (list 0))
(defun current-time (agenda) (car agenda))
(defun set-current-time (agenda time)
  (rplaca agenda time))
(defun segments (agenda) (cdr agenda))
(defun set-segments (agenda segments)
  (rplacd agenda segments))
(defun first-segment (agenda) (car (segments agenda)))
(defun rest-segments (agenda) (cdr (segments agenda)))
(defun empty-agenda-p (agenda)
  (null (segments agenda)))
(defun add-to-agenda (time action agenda)
  (labels ((belongs-before-p (segments)
	     (or (null segments)
		 (< time (segment-time (car segments)))))
	   (make-new-time-segment (time action)
	     (let ((q (make-queue)))
	       (insert-queue q action)
	       (make-time-segment time q)))
	   (add-to-segments (segments)
	     (if (= (segment-time (car segments)) time)
		 (insert-queue (segment-queue (car segments)) action)
		 (let ((rest (cdr segments)))
		   (if (belongs-before-p rest)
		       (rplacd segments
			       (cons (make-new-time-segment time action) rest))
		       (add-to-segments rest))))))
    (let ((segments (segments agenda)))
      (if (belongs-before-p segments)
	  (set-segments agenda
			(cons (make-new-time-segment time action) segments))
	  (add-to-segments segments)))))
(defun remove-first-agenda-item (agenda)
  (let ((q (segment-queue (first-segment agenda))))
    (delete-queue q)
    (when (empty-queue-p q)
      (set-segments agenda (rest-segments agenda)))))
(defun first-agenda-item (agenda)
  (if (empty-agenda-p agenda)
      (error "Agenda is empty -- FIRST-AGENDA-ITEM")
      (let ((first-seg (first-segment agenda)))
	(set-current-time agenda (segment-time first-seg))
	(front-queue (segment-queue first-seg)))))

(defvar *the-agenda* (make-agenda))
(defun after-delay (delay action)
  (add-to-agenda (+ delay (current-time *the-agenda*)) action *the-agenda*))
(defun propagate ()
  (if (empty-agenda-p *the-agenda*)
      'done
      (let ((first-item (first-agenda-item *the-agenda*)))
	(funcall first-item)
	(remove-first-agenda-item *the-agenda*)
	(propagate))))

(defparameter *inverter-delay* 2)
(defparameter *and-gate-delay* 3)
(defparameter *or-gate-delay* 5)
(defun logical-not (s)
  (cond ((= s 0) 1)
	((= s 1) 0)
	(t (error "Invalid signal ~a" s))))
(defun logical-and (s1 s2)
  (cond ((and (/= s1 0) (/= s1 1))
	 (error "Invalid signal ~a" s1))
	((and (/= s2 0) (/= s2 1))
	 (error "Invalid signal ~a" s2))
	((and (= s1 1) (= s2 1)) 1)
	(t 0)))
;;; Exercise 3.28 START

(defun logical-or (s1 s2)
  (cond ((and (/= s1 0) (/= s1 1))
	 (error "Invalid signal ~a" s1))
	((and (/= s2 0) (/= s2 1))
	 (error "Invalid signal ~a" s2))
	((and (= s1 0) (= s2 0)) 0)
	(t 1)))

;;; Continued below...

;;; Exercise 3.28 END
(defun inverter (input output)
  (flet ((invert-input ()
	   (let ((new-value (logical-not (get-signal input))))
	     (after-delay *inverter-delay*
			  (lambda () (set-signal output new-value))))))
    (add-action input #'invert-input))
  'ok)
(defun and-gate (a1 a2 output)
  (flet ((and-action-procedure ()
	   (let ((new-value (logical-and (get-signal a1) (get-signal a2))))
	     (after-delay *and-gate-delay*
			  (lambda () (set-signal output new-value))))))
    (add-action a1 #'and-action-procedure)
    (add-action a2 #'and-action-procedure))
  'ok)
;;; Exercise 3.28 START

(defun or-gate (a1 a2 output)
  (flet ((or-action-procedure ()
	   (let ((new-value (logical-or (get-signal a1) (get-signal a2))))
	     (after-delay *or-gate-delay*
			  (lambda () (set-signal output new-value))))))
    (add-action a1 #'or-action-procedure)
    (add-action a2 #'or-action-procedure))
  'ok)

;;; Exercise 3.28 END

;;; Exercise 3.29 START

(defun or-gate-2 (a1 a2 output)
  "Implemented with INVERTER and AND-GATE."
  (let ((b1 (make-wire))
	(b2 (make-wire))
	(c (make-wire)))
    (inverter a1 b1)
    (inverter a2 b2)
    (and-gate b1 b2 c)
    (inverter c output))
  'ok)

;;; The delay is (+ (* 2 *INVERTER-DELAY*) *AND-GATE-DELAY*).

;;; Exercise 3.29 END

(defun half-adder (a b s c)
  (let ((d (make-wire))
	(e (make-wire)))
    (or-gate a b d)
    (and-gate a b c)
    (inverter c e)
    (and-gate d e s))
  'ok)
(defun full-adder (a b c-in sum c-out)
  (let ((s (make-wire))
	(c1 (make-wire))
	(c2 (make-wire)))
    (half-adder b c-in s c1)
    (half-adder a s sum c2)
    (or-gate c1 c2 c-out))
  'ok)

;;; Exercise 3.30 START

(defun ripple-carry-adder (al bl sl c)
  (if (null (cdr al))
      (let ((zero-wire (make-wire)))
	(full-adder (car al) (car bl) zero-wire (car sl) c))
      (let ((carry-wire (make-wire))) 
	(full-adder (car al) (car bl) carry-wire (car sl) c)
	(ripple-carry-adder (cdr al) (cdr bl) (cdr sl) carry-wire))))

;;; Let's do some tests:
#+nil
(let* ((a '(1 0 1 1))
       (b '(0 1 1 1))
       (n (length a))
       (input1 (loop repeat n collect (make-wire)))
       (input2 (loop repeat n collect (make-wire)))
       (sum (loop repeat n collect (make-wire)))
       (carry (make-wire)))
  (mapcar (lambda (wire x) (set-signal wire x)) input1 a)
  (mapcar (lambda (wire x) (set-signal wire x)) input2 b)
  (ripple-carry-adder input1 input2 sum carry)
  (propagate)
  (list (mapcar #'get-signal sum) (get-signal carry)))
;;; => ((0 0 1 0) 1)

;;; As for the delay, let's see them one by one:
;;; half-adder: h1 = max(and + not, or) + and; h2 = and
;;; full-adder: f1 = 2 * h1; f2 = h1 + h2 + or
;;; ripple-carry = n * f2
;;; ... = n * (max(and + not, or) + 2 * and + or)

;;; Exercise 3.30 END

(defun probe (name wire)
  (add-action wire
	      (lambda ()
		(format t "~&~a ~a New value = ~a~%"
			name (current-time *the-agenda*)
			(get-signal wire)))))

;;; Exercise 3.31 START

;;; The wires that were not assigned signals would not propagate their
;;; values. In the first half-adder example, this means, that the wire
;;; between the and-gate and the inverse was on 0, but the wire between
;;; the inverse and the final end was not set to 1, but was also 0. So
;;; when the signal from the upper wire called the and-gate on the lower
;;; wire, its output was 0 so the wire didn't see any change and thus
;;; didn't propagate to the inverter, so the final and-gate still had a
;;; 0 instead of a 1.

;;; Exercise 3.31 END

;;; Exercise 3.32 START

;;; Let's do some tests:
#+nil
(let ((input1 (make-wire))
      (input2 (make-wire))
      (result (make-wire)))
  (set-current-time *the-agenda* 0)
  (and-gate input1 input2 result)
  (probe 'and result)
  (set-signal input1 0)
  (set-signal input2 1)
  (propagate)
  (set-signal input1 1)
  (set-signal input2 0)
  (propagate))
;;; =>
;; AND 0 New value = 0
;; AND 6 New value = 1 (this is a temporary value - no problem)
;; AND 6 New value = 0

;;; Now install these LIFO "queue" functions:
#+nil
(progn
  (defun make-queue () (cons 'head nil))
  (defun insert-queue (queue value) (rplacd queue (cons value (cdr queue))))
  (defun delete-queue (queue) (rplacd queue (cddr queue)))
  (defun empty-queue-p (queue) (null (cdr queue)))
  (defun front-queue (queue) (cadr queue)))

;;; =>
;; AND 0 New value = 0
;; AND 6 New value = 1 (the result is 1 - that's no good)

;;; This is because the actions stored in the segment's queue are calculated
;;; at the time the signals are given, so they reflect the state of that
;;; moment. In the and-gate example, when we set the first input to 1,
;;; it calls the and-gate that creates an event to set the result to 1,
;;; since at this state, both of its inputs are 1. Setting the second input
;;; to 0 calls the and-gate once again that creates the event to set the
;;; result to 0. That is why a FIFO flips to 1 for a moment, but that's OK;
;;; a LIFO, on the other hand, will reflect the state after the first change
;;; in the inputs.

;;; Exercise 3.32 END


;;; Section 3.3.5

(defun for-each-except (exception procedure list)
  (labels ((loop-on (items)
	      (cond ((null items) 'done)
		    ((eq (car items) exception) (loop-on (cdr items)))
		    (t (funcall procedure (car items)) (loop-on (cdr items))))))
    (loop-on list)))

(defun make-connector ()
  (let ((value nil)
	(informant nil)
	(constraints '()))
    (labels ((set-my-value (newval setter)
	       (cond ((not (has-value-p #'me))
		      (setq value newval)
		      (setq informant setter)
		      (for-each-except setter #'inform-about-value constraints))
		     ((not (= value newval))
		      (error "Contradiction: ~a" (list value newval)))
		     (t 'ignored)))
	     (forget-my-value (retractor)
	       (if (eq retractor informant)
		   (progn
		     (setq informant nil)
		     (for-each-except retractor #'inform-about-no-value
				      constraints))
		   'ignored))
	     (connect (new-constraint)
	       (unless (member new-constraint constraints)
		 (push new-constraint constraints))
	       (when (has-value-p #'me)
		 (inform-about-value new-constraint)))
	     (me (request)
	       (cond ((eq request 'has-value-p) (if informant t nil))
		     ((eq request 'value) value)
		     ((eq request 'set-value) #'set-my-value)
		     ((eq request 'forget) #'forget-my-value)
		     ((eq request 'connect) #'connect)
		     (t (error "Unknown operation ~a -- CONNECTOR" request)))))
      #'me)))

(defun has-value-p (connector)
  (funcall connector 'has-value-p))
(defun get-value (connector)
  (funcall connector 'value))
(defun set-value (connector new-value informant)
  (funcall (funcall connector 'set-value) new-value informant))
(defun forget-value (connector retractor)
  (funcall (funcall connector 'forget) retractor))
(defun connect (connector new-constraint)
  (funcall (funcall connector 'connect) new-constraint))

(defun inform-about-value (constraint)
  (funcall constraint 'I-have-a-value))
(defun inform-about-no-value (constraint)
  (funcall constraint 'I-lost-my-value))

(defun adder-constraint (a1 a2 sum)
  (labels ((process-new-value ()
	     (cond ((and (has-value-p a1) (has-value-p a2))
		    (set-value sum (+ (get-value a1) (get-value a2)) #'me))
		   ((and (has-value-p a1) (has-value-p sum))
		    (set-value a2 (- (get-value sum) (get-value a1)) #'me))
		   ((and (has-value-p a2) (has-value-p sum))
		    (set-value a1 (- (get-value sum) (get-value a2)) #'me))))
	   (process-forget-value ()
	     (forget-value sum #'me)
	     (forget-value a1 #'me)
	     (forget-value a2 #'me)
	     (process-new-value))
	   (me (request)
	     (cond ((eq request 'I-have-a-value) (process-new-value))
		   ((eq request 'I-lost-my-value) (process-forget-value))
		   (t (error "Unknown request ~a -- ADDER-CONSTRAINT"
			     request)))))
    (connect a1 #'me)
    (connect a2 #'me)
    (connect sum #'me)
    #'me))

(defun multiplier-constraint (m1 m2 product)
  (labels ((process-new-value ()
	     (cond ((or (and (has-value-p m1) (= (get-value m1) 0))
			(and (has-value-p m2) (= (get-value m2) 0)))
		    (set-value product 0 #'me))
		   ((and (has-value-p m1) (has-value-p m2))
		    (set-value product (* (get-value m1) (get-value m2)) #'me))
		   ((and (has-value-p m1) (has-value-p product))
		    (set-value m2 (/ (get-value product) (get-value m1)) #'me))
		   ((and (has-value-p m2) (has-value-p product))
		    (set-value m1 (/ (get-value product) (get-value m2))
			       #'me))))
	   (process-forget-value ()
	     (forget-value product #'me)
	     (forget-value m1 #'me)
	     (forget-value m2 #'me)
	     (process-new-value))
	   (me (request)
	     (cond ((eq request 'I-have-a-value) (process-new-value))
		   ((eq request 'I-lost-my-value) (process-forget-value))
		   (t (error "Unknown request ~a -- MULTIPLIER-CONSTRAINT"
			     request)))))
    (connect m1 #'me)
    (connect m2 #'me)
    (connect product #'me)
    #'me))

(defun constant-constraint (value connector)
  (flet ((me (request)
	   (error "Unknown request ~a -- CONSTANT-CONSTRAINT" request)))
    (connect connector #'me)
    (set-value connector value #'me)
    #'me))

(defun probe-constraint (name connector)
  (labels ((print-probe (value)
	     (format t "Probe: ~a = ~a~%" name value))
	   (process-new-value ()
	     (print-probe (get-value connector)))
	   (process-forget-value ()
	     (print-probe "?"))
	   (me (request)
	     (cond ((eq request 'I-have-a-value)
		    (process-new-value))
		   ((eq request 'I-lost-my-value)
		    (process-forget-value))
		   (t (error "Unknown request ~a -- PROBE-CONSTRAINT"
			     request)))))
    (connect connector #'me)
    #'me))

(defun celsius-fahrenheit-converter (c f)
  (let ((u (make-connector))
	(v (make-connector))
	(w (make-connector))
	(x (make-connector))
	(y (make-connector)))
    (multiplier-constraint c w u)
    (multiplier-constraint v x u)
    (adder-constraint v y f)
    (constant-constraint 9 w)
    (constant-constraint 5 x)
    (constant-constraint 32 y))
  'ok)

;;; Exercise 3.33 START

(defun averager-constraint (a b c)
  (let ((d (make-connector))
	(e (make-connector)))
    (constant-constraint 2 d)
    (multiplier-constraint c d e)
    (adder-constraint a b e))
  'ok)

;;; Exercise 3.33 END

;;; Exercise 3.34 START

;;; It would not tell a from b, because MULTIPLIER-CONSTRAINT needs two
;;; data to deduce a third.

;;; Exercise 3.34 END

;;; Exercise 3.35 START

(defun squarer-constraint (a b)
  (labels ((process-new-value ()
	     (cond ((has-value-p b)
		    (if (< (get-value b) 0)
			(error "square less than 0 (~a) -- SQUARER"
			       (get-value b))
			(set-value a (sqrt (get-value b)) #'me)))
		   ((has-value-p a)
		    (let ((x (get-value a)))
		      (set-value b (* x x) #'me))))) 
	   (process-forget-value ()
	     (forget-value a #'me)
	     (forget-value b #'me)
	     (process-new-value))
	   (me (request)
	     (cond ((eq request 'I-have-a-value) (process-new-value))
		   ((eq request 'I-lost-my-value) (process-forget-value))
		   (t (error "Unknown request ~a -- SQUARER-CONSTRAINT"
			     request)))))
    (connect a #'me)
    (connect b #'me)
    #'me))

;;; Exercise 3.35 END

;;; Exercise 3.36 START

;;; Skip this one.

;;; Exercise 3.36 END

;;; Exercise 3.37 START

(defun c+ (x y)
  (let ((z (make-connector)))
    (adder-constraint x y z)
    z))

(defun c- (x y)
  (let ((z (make-connector)))
    (adder-constraint y z x)
    z))

(defun c* (x y)
  (let ((z (make-connector)))
    (multiplier-constraint x y z)
    z))

(defun c/ (x y)
  (let ((z (make-connector)))
    (multiplier-constraint y z x)
    z))

(defun cv (x)
  (let ((y (make-connector)))
    (constant-constraint x y)
    y))

(defun celsius-fahrenheit-converter-2 (x)
  "With shorter notation."
  (c+ (c* (c/ (cv 9) (cv 5)) x) (cv 32)))

;;; Exercise 3.37 END


;;; Section 3.4.1

;;; Exercise 3.38 START

;;; (a)
;;; (100+10-20)/2 = (100-20+10)/2 = 45
;;; (100+10)/2-20 = 35
;;; (100)/2+10-20 = (100)/2-20+10 = 40
;;; (100-20)/2+10 = 50
;;; => $35, $40, $45, $50

;;; (b)
;;; One or both of the other transactions effect may be nullified:
;;; (100+10)/2 = 55
;;; 100/2+10   = 60
;;; 100/2-20   = 30
;;; 100+10-20  = 90
;;; 100+10     = 110
;;; 100-20     = 80
;;; => $30, $55, $60, $80, $90, $110

;;; Skip the diagram drawing...

;;; Exercise 3.38 END


;;; Section 3.4.2

;;; A quick hack to make the examples work
(defun parallel-execute (&rest procedures)
  (let ((threads (mapcar #'threads:make-thread procedures)))
    (loop while (some #'threads:thread-alive-p threads) do (sleep 0.1))))

(defun make-serializer ()
  "A thread-safe version using built-in mutexes."
  (let ((mutex (threads:make-lock)))
    (lambda (p)
      (flet ((serialized-p (&rest args)
	       (threads:acquire-lock mutex)
	       (let ((val (apply p args)))
		 (threads:release-lock mutex)
		 val)))
	#'serialized-p))))

(defun make-account-2 (balance)
  "Serialized version."
  (labels ((withdraw (amount)
	     (if (>= balance amount)
		 (progn
		   (setq balance (- balance amount))
		   balance)
		 "Insufficient funds"))
	   (deposit (amount)
	     (setq balance (+ balance amount))
	     balance))
    (let ((protected (make-serializer)))
      (flet ((dispatch (m)
	       (cond ((eq m 'withdraw) (funcall protected #'withdraw))
		     ((eq m 'deposit) (funcall protected #'deposit))
		     ((eq m 'balance) balance)
		     (t (error "Unknown request ~a -- MAKE-ACCOUNT-2" m)))))
	#'dispatch))))

;;; Exercise 3.39 START

;;; 100, 101 and 121.

;;; Exercise 3.39 END

;;; Exercise 3.40 START

;;; 10^k, k = 2..6, from which only 10^6 remains in the serialized version.

;;; Exercise 3.40 END

;;; Exercise 3.41 START

;;; It is a read-only action, so it cannot invalidate the account.

;;; Exercise 3.41 END

;;; Exercise 3.42 START

;;; Yes, it is a safe change, with the same concurrency as the original.

;;; Exercise 3.42 END

(defun exchange (account1 account2)
  (let ((difference (- (funcall account1 'balance)
		       (funcall account2 'balance))))
    (funcall (funcall account1 'withdraw) difference)
    (funcall (funcall account2 'deposit) difference)))

#+nil
(let ((a1 (make-account-2 10))
      (a2 (make-account-2 20))
      (a3 (make-account-2 30)))
  (parallel-execute (lambda () (exchange a1 a3))
		    (lambda () (exchange a2 a3)))
  (list (funcall a1 'balance) (funcall a2 'balance) (funcall a3 'balance)))

(defun make-account-and-serializer (balance)
  (labels ((withdraw (amount)
	     (if (>= balance amount)
		 (progn
		   (setq balance (- balance amount))
		   balance)
		 "Insufficient funds"))
	   (deposit (amount)
	     (setq balance (+ balance amount))
	     balance))
    (let ((balance-serializer (make-serializer)))
      (flet ((dispatch (m)
	       (cond ((eq m 'withdraw) #'withdraw)
		     ((eq m 'deposit) #'deposit)
		     ((eq m 'balance) balance)
		     ((eq m 'serializer) balance-serializer)
		     (t
		      (error "Unknown request ~a -- MAKE-ACCOUNT-AND-SERIALIZER"
			     m)))))
	#'dispatch))))

(defun deposit (account amount)
  (let ((s (funcall account 'serializer))
	(d (funcall account 'deposit)))
    (funcall (funcall s d) amount)))

(defun serialized-exchange (account1 account2)
  (let ((serializer1 (funcall account1 'serializer))
	(serializer2 (funcall account2 'serializer)))
    (funcall (funcall serializer1 (funcall serializer2 #'exchange))
	     account1 account2)))

;;; Exercise 3.43 START

;;; If the exchanges are run sequentially, after every atomic step there
;;; will be exactly one account with $10, $20 and $30, though their
;;; order is unknown.

;;; The problem with the original version is explained in the text.
;;; However, the deposit and withdraw actions come in pairs (with
;;; equal amounts), so the sum will not change.

;;; Skip the diagram drawing...

;;; Exercise 3.43 END

;;; Exercise 3.44 START

;;; It is safe, because we do not calculate the amount based on the
;;; balances.

;;; Exercise 3.44 END

;;; Exercise 3.45 START

;;; The problem is that if there is automatic serializing, it will clash
;;; with the explicit one in cases like serialized-exchange. Since the
;;; mutex is set, it cannot do the deposit/withdrawal and will loop
;;; infinitely.

;;; Exercise 3.45 END

;;; Exercise 3.46 START

;;; Skip this one.

;;; Exercise 3.46 END

;;; Exercise 3.47 START

(defun make-semaphore (max)
  (cons (threads:make-lock) (cons 0 max)))
(defun semaphore-acquire (semaphore)
  (flet ((try ()
	   (threads:acquire-lock (car semaphore))
	   (prog1 (when (< (cadr semaphore) (cddr semaphore))
		    (rplaca (cdr semaphore) (1+ (cadr semaphore)))
		    t)
	     (threads:release-lock (car semaphore)))))
    (loop until (try))))
(defun semaphore-release (semaphore)
  (threads:acquire-lock (car semaphore))
  (when (= (cadr semaphore) 0)
    (error "This semaphore is not acquired."))
  (rplaca (cdr semaphore) (1- (cadr semaphore)))
  (threads:release-lock (car semaphore)))

;;; test-and-set operations skipped (since we have basic mutexes)

;;; Exercise 3.47 END

;;; Exercise 3.48 START

;;; If every process acquires the accounts in ascending order, then they
;;; cannot acquire an account with an id less than the one it already
;;; possesses. The problem with the exchange example in the text is that
;;; Paul has taken a2 and wants a1, Peter has taken a1 and wants a2. If
;;; we use a fixed order for acquisition, this cannot happen.

(defparameter *max-number* (cons (threads:make-lock) 0))

(defun make-account-and-serializer-2 (balance)
  "Avoids deadlock with numbering."
  (labels ((withdraw (amount)
	     (if (>= balance amount)
		 (progn
		   (setq balance (- balance amount))
		   balance)
		 "Insufficient funds"))
	   (deposit (amount)
	     (setq balance (+ balance amount))
	     balance))
    (let ((balance-serializer (make-serializer))
	  (id (progn
		(threads:acquire-lock (car *max-number*))
		(let ((old (cdr *max-number*)))
		  (rplacd *max-number* old)
		  (threads:release-lock (car *max-number*))
		  old))))
      (flet ((dispatch (m)
	       (cond ((eq m 'withdraw) #'withdraw)
		     ((eq m 'deposit) #'deposit)
		     ((eq m 'balance) balance)
		     ((eq m 'serializer) balance-serializer)
		     ((eq m 'id) id)
		     (t (error
			 "Unknown request ~a -- MAKE-ACCOUNT-AND-SERIALIZER-2"
			 m)))))
	#'dispatch))))

(defun serialized-exchange-2 (account1 account2)
  "Avoids deadlock with numbering."
  (let* ((less (< (funcall account1 'id) (funcall account2 'id)))
	 (serializer1 (funcall (if less account1 account2) 'serializer))
	 (serializer2 (funcall (if less account2 account1) 'serializer)))
    (funcall (funcall serializer1 (funcall serializer2 #'exchange))
	     account1 account2)))

;;; Exercise 3.48 END

;;; Exercise 3.49 START

;;; Imagine a shared version of the constraint propagation discussed in
;;; this chapter (ie. the connectors can be set simultaneously).
;;; Here the connectors you want to access are not known beforehand, so
;;; a deadlock may occur (although Good Programming can eliminate it,
;;; in this case).

;;; Exercise 3.49 END


;;; Section 3.5.1

(defun memo-proc (proc)
  (let (already-run-p result)
    (lambda ()
      (unless already-run-p
	(setq result (funcall proc))
	(setq already-run-p t))
      result)))
(defmacro delay (exp)
  `(memo-proc (lambda () ,exp)))
(defun force (delayed-object)
  (funcall delayed-object))

(defconstant +the-empty-stream+ nil)
(defun stream-null (stream) (null stream))
(defun stream-car (stream) (car stream))
(defun stream-cdr (stream) (force (cdr stream)))
(defmacro cons-stream (a b) `(cons ,a (delay ,b)))

(defun stream-ref (s n)
  (if (= n 0)
      (stream-car s)
      (stream-ref (stream-cdr s) (- n 1))))
(defun stream-mapcar (proc s)
  (if (stream-null s)
      +the-empty-stream+
      (cons-stream (funcall proc (stream-car s))
		   (stream-mapcar proc (stream-cdr s)))))
(defun stream-mapc (proc s)
  (if (stream-null s)
      'done
      (progn (funcall proc (stream-car s))
	     (stream-mapc proc (stream-cdr s)))))
(defun display-line (x) (princ x) (terpri))
(defun display-stream (s) (stream-mapc #'display-line s))
(defun display-n (s n)
  "A more convenient version."
  (loop for k from 0 below n do (display-line (stream-ref s k))))
(defun stream-enumerate-interval (low high)
  (if (> low high)
      +the-empty-stream+
      (cons-stream low (stream-enumerate-interval (1+ low) high))))
(defun stream-filter (pred stream)
  (cond ((stream-null stream) +the-empty-stream+)
	((funcall pred (stream-car stream))
	 (cons-stream (stream-car stream)
		      (stream-filter pred (stream-cdr stream))))
	(t (stream-filter pred (stream-cdr stream)))))

;;; Exercise 3.50 START

(defun stream-mapcar-2 (proc &rest argstreams)
  "With multiple streams."
  (if (stream-null (car argstreams))
      +the-empty-stream+
      (cons-stream
       (apply proc (mapcar #'stream-car argstreams))
       (apply #'stream-mapcar-2 proc (mapcar #'stream-cdr argstreams)))))

;;; Exercise 3.50 END

;;; Exercise 3.51 START

#+nil
(flet ((show (x)
	 (display-line x)
	 x))
  (let ((x (stream-mapcar #'show (stream-enumerate-interval 0 10))))
    (stream-ref x 5)
    (stream-ref x 7)))

;;; The program above displays the numbers 0 to 7. The numbers 0..5 are
;;; not displayed twice, since the evaluation of the delayed expression
;;; is optimized in our implementation.

;;; Exercise 3.51 END

;;; Exercise 3.52 START

#+nil
(let ((sum 0))
  (flet ((accum (x)
	   (setq sum (+ x sum))
	   sum))
    (let* ((seq (stream-mapcar #'accum (stream-enumerate-interval 1 20)))
	   (y (stream-filter #'evenp seq))
	   (z (stream-filter (lambda (x) (= (mod x 5) 0)) seq)))
      (stream-ref y 7)			; the 7th even triangle number
      (display-stream z))))

;;; Prints those among the first 20 triangle numbers, that are divisible by 5.
;;; Sum is going on the first 20 triangle numbers.

;;; Exercise 3.52 END


;;; Section 3.5.2

(defun integers-starting-from (n)
  (cons-stream n (integers-starting-from (1+ n))))
(defparameter *integers* (integers-starting-from 1))
(defun divisiblep (x y) (= (mod x y) 0))
(defun fibgen (a b) (cons-stream a (fibgen b (+ a b))))
(defun sieve (stream)
  (cons-stream
   (stream-car stream)
   (sieve (stream-filter (lambda (x) (not (divisiblep x (stream-car stream))))
			 (stream-cdr stream)))))
(defun add-streams (s1 s2)
  (stream-mapcar-2 #'+ s1 s2))
(defun scale-stream (stream factor)
  (stream-mapcar (lambda (x) (* x factor)) stream))

(defvar *primes*)
(defun primep-2 (n)
  "Uses (and builds) *PRIMES*."
  (labels ((rec (ps)
	     (cond ((> (square (stream-car ps)) n) t)
		   ((divisiblep n (stream-car ps)) nil)
		   (t (rec (stream-cdr ps))))))
    (rec *primes*)))
(setq *primes*
      (cons-stream 2 (stream-filter #'primep-2 (integers-starting-from 3))))

;;; Exercise 3.53 START

;;; Starts with 1 and adds itself to itself, so it generates the exponent of 2.

;;; Exercise 3.53 END

;;; Exercise 3.54 START

(defun mul-streams (s1 s2)
  (stream-mapcar-2 #'* s1 s2))
(defvar *factorials*)
(setq *factorials*
      (cons-stream 1 (mul-streams *factorials* (integers-starting-from 2))))

;;; Exercise 3.54 END

;;; Exercise 3.55 START

(defun partial-sums (stream)
  (let (result)
    (setq result
	  (cons-stream (stream-car stream)
		       (add-streams result (stream-cdr stream))))))

;;; Exercise 3.55 END

;;; Exercise 3.56 START

(defun merge-streams (s1 s2)
  (cond ((stream-null s1) s2)
	((stream-null s2) s1)
	(t (let ((s1car (stream-car s1))
		 (s2car (stream-car s2)))
	     (cond ((< s1car s2car)
		    (cons-stream s1car (merge-streams (stream-cdr s1) s2)))
		   ((> s1car s2car)
		    (cons-stream s2car (merge-streams s1 (stream-cdr s2))))
		   (t (cons-stream s1car (merge-streams (stream-cdr s1)
							(stream-cdr s2)))))))))

(defvar *hamming-series*)
(setq *hamming-series*
      (cons-stream
       1
       (merge-streams (scale-stream *hamming-series* 2)
		      (merge-streams (scale-stream *hamming-series* 3)
				     (scale-stream *hamming-series* 5)))))

;;; Exercise 3.56 END

;;; Exercise 3.57 START

;;; Assuming that no evaluation has been made, n-1.
;;; With the other implementation it would be O(2^n) [see Ex. 3.27].

;;; Exercise 3.57 END

;;; Exercise 3.58 START

(defun expand (num den radix)
  (cons-stream (floor (* num radix) den)
	       (expand (mod (* num radix) den) den radix)))
;;; Gives the fractional part of a rational number num/den in radix,
;;; for example (EXPAND 2 3 10) = (6 6 6 ...), (EXPAND 2 3 3) = (2 0 0 ...).
;;; It starts with the first negative exponent, so for numbers that have
;;; num >= den, the first value will not be a digit.

;;; (EXPAND 1 7 10) = (1 4 2 8 5 7)*,  since 1/7 = 0.14285714285714...
;;; (EXPAND 3 8 10) = (3 7 5 0 0 ...), since 3/8 = 0.375

;;; Exercise 3.58 END

;;; Exercise 3.59 START

(defun div-streams (s1 s2)
  (stream-mapcar-2 #'/ s1 s2))
(defun integrate-series (stream)
  (div-streams stream (integers-starting-from 1)))
(defvar *exp-series*)
(setq *exp-series* (cons-stream 1 (integrate-series *exp-series*)))
(defvar *cosine-series*)
(defvar *sine-series*)
(setq *cosine-series*
      (cons-stream 1 (scale-stream (integrate-series *sine-series*) -1)))
(setq *sine-series* (cons-stream 0 (integrate-series *cosine-series*)))

;;; Exercise 3.59 END

;;; Exercise 3.60 START

(defun mul-series (s1 s2)
  (cons-stream (* (stream-car s1) (stream-car s2))
	       (add-streams (scale-stream (stream-cdr s1) (stream-car s2))
			    (mul-series s1 (stream-cdr s2)))))

;;; Exercise 3.60 END

;;; Exercise 3.61 START

(defun invert-unit-series (s)
  (let (result)
    (setq result
	  (cons-stream 1
		       (scale-stream (mul-series (stream-cdr s) result) -1)))))

;;; Exercise 3.61 END

;;; Exercise 3.62 START

(defun div-series (s1 s2)
  (let ((s2car (stream-car s2)))
    (if (= s2car 0)
	(error "Stream starts with 0: ~a -- DIV-SERIES" s2)
	(scale-stream
	 (mul-series s1 (invert-unit-series (scale-stream s2 (/ s2car))))
	 (/ s2car)))))

(defparameter *tangent-series* (div-series *sine-series* *cosine-series*))

;;; Exercise 3.62 END


;;; Section 3.5.3

(defun sqrt-improve (guess x)
  (average guess (/ x guess)))
(defun sqrt-stream (x)
  (let (guesses)
    (setq guesses
	  (cons-stream 1.0d0
		       (stream-mapcar (lambda (guess) (sqrt-improve guess x))
				      guesses)))))

(defun pi-summands (n)
  (cons-stream (/ 1.0d0 n) (stream-mapcar #'- (pi-summands (+ n 2)))))
(defparameter *pi-stream* (scale-stream (partial-sums (pi-summands 1)) 4))
(defun euler-transform (s)
  (let ((s0 (stream-ref s 0))
	(s1 (stream-ref s 1))
	(s2 (stream-ref s 2)))
    (cons-stream (- s2 (/ (square (- s2 s1))
			  (+ s0 (* -2 s1) s2)))
		 (euler-transform (stream-cdr s)))))
(defun make-tableau (transform s)
  (cons-stream s (make-tableau transform (funcall transform s))))
(defun accelerated-sequence (transform s)
  (stream-mapcar #'stream-car (make-tableau transform s)))

;;; Exercise 3.63 START

;;; Calling SQRT-STREAM recursively creates a _new_ stream, so every
;;; element of the stream has to be computed again.
;;; If delay would not use memo-proc, they would be almost equivalent
;;; (almost, because there is still a price for the recursive call).

;;; Exercise 3.63 END

;;; Exercise 3.64 START

(defun stream-limit (s tolerance)
  (let ((e1 (stream-car s))
	(e2 (stream-car (stream-cdr s))))
    (if (< (abs (- e1 e2)) tolerance)
	e2
	(stream-limit (stream-cdr s) tolerance))))
(defun sqrt-10 (x tolerance)
  "Using STREAM-LIMIT."
  (stream-limit (sqrt-stream x) tolerance))

;;; Exercise 3.64 END

;;; Exercise 3.65 START

(defun ln2-summands (n)
  (cons-stream (/ 1.0d0 n) (stream-mapcar #'- (ln2-summands (1+ n)))))
(defparameter *ln2-stream* (partial-sums (ln2-summands 1)))

;;; Without acceleration, 10^-3 precision is with about 10^3 iterations.
;;; With a single euler transform, about 10 iterations.
;;; With tableau, only 3 (10 iterations reach the double precision limit).

;;; Exercise 3.65 END

(defun interleave (s1 s2)
  (if (stream-null s1)
      s2
      (cons-stream (stream-car s1) (interleave s2 (stream-cdr s1)))))
(defun pairs (s1 s2)
  (cons-stream (list (stream-car s1) (stream-car s2))
	       (interleave
		(stream-mapcar (lambda (x) (list (stream-car s1) x))
			       (stream-cdr s2))
		(pairs (stream-cdr s1) (stream-cdr s2)))))

;;; Exercise 3.66 START

;;; Let f(n, k) be the index of the pair (n, k) in the stream.
;;; Then f(n, 0) = 2^n - 2, and f(n, k) = f(n, 0) + 2^(n-1) + (k-n-1) * 2^n.
;;; So:
#+nil
(labels ((f (n k)
	   (if (= k 0)
	       (- (expt 2 n) 2)
	       (+ (f n 0) (expt 2 (1- n)) (* (- k n 1) (expt 2 n))))))
  (format t "(  1, 100) => ~a~%( 99, 100) => ~a~%(100, 100) => ~a~%"
	  (f 1 100) (f 99 100) (f 100 100)))
;;; =>
;; (  1, 100) => 197
;; ( 99, 100) => 950737950171172051122527404030
;; (100, 100) => 633825300114114700748351602686

;;; Exercise 3.66 END

;;; Exercise 3.67 START

(defun all-pairs (s1 s2)
  (cons-stream (list (stream-car s1) (stream-car s2))
	       (interleave
		(interleave
		 (stream-mapcar (lambda (x) (list (stream-car s1) x))
				(stream-cdr s2))
		 (stream-mapcar (lambda (x) (list x (stream-car s2)))
				(stream-cdr s1)))
		(all-pairs (stream-cdr s1) (stream-cdr s2)))))

;;; Exercise 3.67 END

;;; Exercise 3.68 START

;;; Let s, t = (1 2 3).
;;; (pairs s t) = (interleave ((1 1) (1 2) (1 3)) (pairs (2 3) (2 3)))
;;; (pairs (2 3) (2 3)) = (interleave ((2 2) (2 3)) (pairs (3) (3)))
;;; (pairs (3) (3)) = (interleave ((3 3)) (pairs ())) = ((3 3))

;;; That seems all right. But we have infinite streams, and this creates
;;; a problem: the recursive call is not delayed, so it just recurses
;;; infinitely, without generating a value.

;;; Exercise 3.68 END

;;; Exercise 3.69 START

(defun triples (s1 s2 s3)
  (cons-stream (list (stream-car s1) (stream-car s2) (stream-car s3))
	       (interleave
		(stream-mapcar (lambda (pair) (cons (stream-car s1) pair))
			       (stream-cdr (pairs s2 s3)))
		(triples (stream-cdr s1) (stream-cdr s2) (stream-cdr s3)))))

(defparameter *pythagorean-triples*
  (stream-filter (lambda (triple)
		   (= (+ (square (first triple)) (square (second triple)))
		      (square (third triple))))
		 (triples *integers* *integers* *integers*)))

;;; Exercise 3.69 END

;;; Exercise 3.70 START

(defun merge-weighted (s1 s2 weight)
  (cond ((stream-null s1) s2)
	((stream-null s2) s1)
	(t (let ((s1car (stream-car s1))
		 (s2car (stream-car s2)))
	     (cond ((< (funcall weight s1car) (funcall weight s2car))
		    (cons-stream s1car
				 (merge-weighted (stream-cdr s1) s2 weight)))
		   ((not (equal s1car s2car))
		    (cons-stream s2car
				 (merge-weighted s1 (stream-cdr s2) weight)))
		   (t (cons-stream s1car
				   (merge-weighted (stream-cdr s1)
						   (stream-cdr s2)
						   weight))))))))

(defun weighted-pairs (s1 s2 weight)
  (cons-stream (list (stream-car s1) (stream-car s2))
	       (merge-weighted
		(stream-mapcar (lambda (x) (list (stream-car s1) x))
			       (stream-cdr s2))
		(weighted-pairs (stream-cdr s1) (stream-cdr s2) weight)
		weight)))

(defparameter *pairs1*
  (weighted-pairs *integers* *integers*
		  (lambda (pair) (+ (first pair) (second pair)))))
(defparameter *pairs2*
  (stream-filter
   (lambda (pair)
     (and (notany (lambda (p) (divisiblep (first pair) p)) '(2 3 5))
	  (notany (lambda (p) (divisiblep (second pair) p)) '(2 3 5))))
   (weighted-pairs *integers* *integers*
		   (lambda (pair)
		     (let ((i (first pair))
			   (j (second pair)))
		       (+ (* 2 i) (* 3 j) (* 5 i j)))))))

;;; Exercise 3.70 END

;;; Exercise 3.71 START

(defun stream-elements (stream n)
  "Returns a list of the first N elements of STREAM."
  (if (= n 0)
      '()
      (cons (stream-car stream)
	    (stream-elements (stream-cdr stream) (1- n)))))

(defun consecutive-filter (stream fn k)
  "Removes every element but those, for whom FN gives the same value K times."
  (if (stream-null stream)
      +the-empty-stream+
      (let ((elements (stream-elements stream k)))
	(if (apply #'= (mapcar fn elements))
	    (cons-stream elements
			 (consecutive-filter (stream-cdr stream) fn k))
	    (consecutive-filter (stream-cdr stream) fn k)))))

#+nil
(flet ((weight (pair) (+ (expt (first pair) 3) (expt (second pair) 3))))
  (let ((s (weighted-pairs *integers* *integers* #'weight)))
    (display-n (stream-mapcar (lambda (x) (weight (first x)))
			      (consecutive-filter s #'weight 2)) 6)))
;;; =>
;; 1729
;; 4104
;; 13832
;; 20683
;; 32832
;; 39312

;;; Exercise 3.71 END

;;; Exercise 3.72 START

(defparameter *sum-of-squares*
  (flet ((weight (pair) (+ (square (first pair)) (square (second pair)))))
    (let ((s (weighted-pairs *integers* *integers* #'weight)))
      (consecutive-filter s #'weight 3))))

;;; Exercise 3.72 END

(defun integral-3 (integrand initial-value dt)
  "With streams."
  (let (int)
    (setq int (cons-stream initial-value
			   (add-streams (scale-stream integrand dt) int)))))

;;; Exercise 3.73 START

(defun RC (R C dt)
  (lambda (i v0)
    (add-streams (integral-3 (scale-stream i (/ C)) v0 dt)
		 (scale-stream i R))))

;;; Exercise 3.73 END

;;; Exercise 3.74 START

(defun sign-change-detector (a b)
  (cond ((and (< a 0) (>= b 0)) 1)
	((and (>= a 0) (< b 0)) -1)
	(t 0)))
(defun zero-crossings (sense-data)
  (stream-mapcar-2 #'sign-change-detector sense-data (stream-cdr sense-data)))

;;; Exercise 3.74 END

;;; Exercise 3.75 START

(defun make-zero-crossings (input-stream last-average last-value)
  (let ((avpt (/ (+ (stream-car input-stream) last-value) 2)))
    (cons-stream (sign-change-detector avpt last-average)
		 (make-zero-crossings (stream-cdr input-stream) avpt
				      (stream-car input-stream)))))

;;; Exercise 3.75 END

;;; Exercise 3.76 START

(defun smooth-2 (stream)
  "Signal smoothing."
  (stream-mapcar-2 (lambda (x y) (/ (+ x y) 2))
		   stream (cons-stream (stream-car stream) stream)))
(defun make-zero-crossings-2 (input-stream transform)
  "A more modular implementation."
  (zero-crossings (funcall transform input-stream)))
#+nil
(make-zero-crossings-2 input-stream #'smooth-2)

;;; Exercise 3.76 END


;;; Section 3.5.4

(defun integral-4 (delayed-integrand initial-value dt)
  "With streams and delayed integrand."
  (let (int)
    (setq int
	  (cons-stream initial-value
		       (let ((integrand (force delayed-integrand)))
			 (add-streams (scale-stream integrand dt) int))))))
(defun solve (f y0 dt)
  (let* (dy
	 (y (integral-4 (delay dy) y0 dt)))
    (setq dy (stream-mapcar f y))
    y))

;;; Exercise 3.77 START

(defun integral-5 (delayed-integrand initial-value dt)
  (cons-stream initial-value
	       (let ((integrand (force delayed-integrand)))
		 (if (stream-null integrand)
		     +the-empty-stream+
		     (integral-5 (delay (stream-cdr integrand))
				 (+ (* dt (stream-car integrand))
				    initial-value)
				 dt)))))

;;; Exercise 3.77 END

;;; Exercise 3.78 START

(defun solve-2nd (a b y0 dy0 dt)
  (let* (ddy
	 (dy (integral-4 (delay ddy) dy0 dt))
	 (y (integral-4 (delay dy) y0 dt)))
    (setq ddy (add-streams (scale-stream dy a) (scale-stream y b)))))

;;; Exercise 3.78 END

;;; Exercise 3.79 START

(defun solve-general-2nd (f y0 dy0 dt)
  (let* (ddy
	 (dy (integral-4 (delay ddy) dy0 dt))
	 (y (integral-4 (delay dy) y0 dt)))
    (setq ddy (stream-mapcar-2 f dy y))))

;;; Exercise 3.79 END

;;; Exercise 3.80 START

(defun RLC (R L C dt)
  (lambda (vc0 il0)
    (let* (dvc
	   dil
	   (vc (integral-4 (delay dvc) vc0 dt))
	   (il (integral-4 (delay dil) il0 dt)))
      (setq dvc (scale-stream il (/ -1 C))
	    dil (add-streams (scale-stream vc (/ L))
			     (scale-stream il (- (/ R L)))))
      (cons vc il))))

(defparameter *RLC-state-stream*
  (funcall (RLC 1 1 0.2 0.1) 10 0))

;;; Exercise 3.80 END


;;; Section 3.5.5

(defvar *random-numbers*)
(setq *random-numbers*
      (cons-stream *random-init*
		   (stream-mapcar #'rand-update *random-numbers*)))
(defun map-successive-pairs (f s)
  (cons-stream (funcall f (stream-car s) (stream-car (stream-cdr s)))
	       (map-successive-pairs f (stream-cdr (stream-cdr s)))))
(defparameter *cesaro-stream*
  (map-successive-pairs (lambda (r1 r2) (= (gcd r1 r2) 1)) *random-numbers*))
(defun monte-carlo-2 (experiment-stream passed failed)
  "With streams."
  (flet ((next (passed failed)
	   (cons-stream (/ passed (+ passed failed))
			(monte-carlo-2 (stream-cdr experiment-stream)
				       passed failed))))
    (if (stream-car experiment-stream)
	(next (1+ passed) failed)
	(next passed (1+ failed)))))
(defparameter *pi-stream*
  (stream-mapcar (lambda (p) (if (= p 0) 0 (sqrt (/ 6 p))))
		 (monte-carlo-2 *cesaro-stream* 0 0)))

;;; Exercise 3.81 START

(defun resettable-random-stream (requests &optional (init *random-init*))
  (cons-stream init
	       (cond ((eq (stream-car requests) 'generate)
		      (resettable-random-stream
		       (stream-cdr requests) (rand-update init)))
		     ((eq (stream-car requests) 'reset)
		      (resettable-random-stream
		       (stream-cdr (stream-cdr requests))
		       (stream-car (stream-cdr requests))))
		     (t (error "Unknown request ~a -- RANDOM-STREAM"
			       (stream-car requests))))))
;;; For testing
(defun stream-from-list (lst)
  (when lst
    (cons-stream (car lst) (stream-from-list (cdr lst)))))
#+nil
(display-n (resettable-random-stream
	    (stream-from-list '(generate generate reset 100 generate)))
	   5)

;;; Exercise 3.81 END

;;; Exercise 3.82 START

(defun estimate-integral-stream (pred x1 y1 x2 y2)
  (let ((size (* (- x2 x1) (- y2 y1)))
	(xy (map-successive-pairs
	     (lambda (x y)
	       (list (+ x1 (mod x (- x2 x1))) (+ y1 (mod y (- y2 y1)))))
	     *random-numbers*)))
    (scale-stream
     (div-streams
      (partial-sums
       (stream-mapcar (lambda (xy) (if (apply pred xy) 1 0)) xy))
      *integers*)
     size)))

(defun estimate-pi-by-integration-2 (trials)
  "Using streams."
  (/ (stream-ref (estimate-integral-stream
		  (lambda (x y) (<= (+ (* x x) (* y y)) #.(* 500 500)))
		  -500 -500 500 500)
		 trials)
     #.(* 500.0 500.0)))

;;; Exercise 3.82 END


;;; Section 4.1.1

(defun eval-1 (exp env)
  "%EVAL, defined in Exercise 4.3, is much better."
  (cond ((self-evaluating-p exp) exp)
        ((variablep exp) (lookup-variable-value exp env))
        ((quotedp exp) (text-of-quotation exp))
        ((assignmentp exp) (eval-assignment exp env))
        ((definitionp exp) (eval-definition exp env))
        ((ifp exp) (eval-if exp env))
        ((lambdap exp)
         (make-procedure (lambda-parameters exp)
                         (lambda-body exp)
                         env))
        ((beginp exp) (eval-sequence (begin-actions exp) env))
        ((condp exp) (eval-1 (cond->if exp) env))
        ((applicationp exp)
         (%apply (eval-1 (operator exp) env)
                 (list-of-values (operands exp) env)))
        (t (error "Unknown expression type in ~a -- EVAL" exp))))

(defun %apply (procedure arguments)
  (cond ((primitive-procedure-p procedure)
         (apply-primitive-procedure procedure arguments))
        ((compound-procedure-p procedure)
         (eval-sequence
          (procedure-body procedure)
          (extend-environment (procedure-parameters procedure)
                              arguments
                              (procedure-environment procedure))))
        (t (error "Unknown procedure type in ~a -- APPLY" procedure))))

(defun list-of-values (exps env)
  (if (no-operands-p exps)
      '()
      (cons (%eval (first-operand exps) env)
            (list-of-values (rest-operands exps) env))))

(defun eval-if (exp env)
  (if (truep (%eval (if-predicate exp) env))
      (%eval (if-consequent exp) env)
      (%eval (if-alternative exp) env)))

(defun eval-sequence (exps env)
  (cond ((last-exp-p exps) (%eval (first-exp exps) env))
        (t (%eval (first-exp exps) env)
           (eval-sequence (rest-exps exps) env))))

(defun eval-assignment (exp env)
  (set-variable-value (assignment-variable exp)
                      (%eval (assignment-value exp) env)
                      env)
  'ok)

(defun eval-definition (exp env)
  (define-variable (definition-variable exp)
                   (%eval (definition-value exp) env)
                   env)
  'ok)

;;; Exercise 4.1 START

(defun list-of-values-left-to-right (exps env)
  (if (no-operands-p exps)
      '()
      (let ((first (%eval (first-operand exps) env)))
        (cons first (list-of-values (rest-operands exps) env)))))

(defun list-of-values-right-to-left (exps env)
  (if (no-operands-p exps)
      '()
      (let ((rest (list-of-values (rest-operands exps) env)))
        (cons (%eval (first-operand exps) env) rest))))

;;; Exercise 4.1 END


;;; Section 4.1.2

(defun self-evaluating-p (exp)
  (or (numberp exp) (stringp exp)))

;;; VARIABLEP same as in Section 2.3.2

(defun quotedp (exp)
  (tagged-list-p exp 'quote))

(defun text-of-quotation (exp)
  (cadr exp))

(defun tagged-list-p (exp tag)
  (and (consp exp) (eq (car exp) tag)))

(defun assignmentp (exp)
  (tagged-list-p exp 'set!))

(defun assignment-variable (exp)
  (cadr exp))

(defun assignment-value (exp)
  (caddr exp))

(defun definitionp (exp)
  (tagged-list-p exp 'define))

(defun definition-variable (exp)
  (if (symbolp (cadr exp))
      (cadr exp)
      (caadr exp)))

(defun definition-value (exp)
  (if (symbolp (cadr exp))
      (caddr exp)
      (make-lambda (cdadr exp) (cddr exp))))

(defun lambdap (exp)
  (tagged-list-p exp 'lambda))

(defun lambda-parameters (exp)
  (cadr exp))

(defun lambda-body (exp)
  (cddr exp))

(defun make-lambda (parameters body)
  (cons 'lambda (cons parameters body)))

(defun ifp (exp)
  (tagged-list-p exp 'if))

(defun if-predicate (exp)
  (cadr exp))

(defun if-consequent (exp)
  (caddr exp))

(defun if-alternative (exp)
  (if (not (null (cdddr exp)))
      (cadddr exp)
      'false))

(defun make-if (predicate consequent alternative)
  (list 'if predicate consequent alternative))

(defun beginp (exp)
  (tagged-list-p exp 'begin))

(defun begin-actions (exp)
  (cdr exp))

(defun last-exp-p (seq)
  (null (cdr seq)))

(defun first-exp (seq)
  (car seq))

(defun rest-exps (seq)
  (cdr seq))

(defun sequence->exp (seq)
  (cond ((null seq) seq)
        ((last-exp-p seq) (first-exp seq))
        (t (make-begin seq))))

(defun make-begin (seq)
  (cons 'begin seq))

(defun applicationp (exp)
  (consp exp))

;;; OPERATOR and OPERANDS same as in Exercise 2.73

(defun no-operands-p (ops)
  (null ops))

(defun first-operand (ops)
  (car ops))

(defun rest-operands (ops)
  (rest ops))

(defun condp (exp)
  (tagged-list-p exp 'cond))

(defun cond-clauses (exp)
  (cdr exp))

(defun cond-else-clause-p (clause)
  (eq (cond-predicate clause) 'else))

(defun cond-predicate (clause)
  (car clause))

(defun cond-actions (clause)
  (cdr clause))

(defun cond->if (exp)
  (expand-clauses (cond-clauses exp)))

(defun expand-clauses-1 (clauses)
  "EXPAND-CLAUSES will be defined in Exercise 4.5."
  (if (null clauses)
      'false
      (let ((first (car clauses))
            (rest (cdr clauses)))
        (if (cond-else-clause-p first)
            (if (null rest)
                (sequence->exp (cond-actions first))
                (error "ELSE clause isn't last in ~a -- COND->IF" clauses))
            (make-if (cond-predicate first)
                     (sequence->exp (cond-actions first))
                     (expand-clauses-1 rest))))))

;;; Exercise 4.2 START

;;; (a)
;;; In the current implementation, APPLICATIONP is just CONSP,
;;; so moving this test to the front would result in evaluating
;;; (define x 3) as an application instead of a definition.

;;; (b)
;;; We need to use the following functions:

(defun applicationp-1 (exp)
  "Applications are denoted by CALL, e.g. (call f x y)."
  (tagged-list-p exp 'call))

(defun operator-1 (exp)
  "Applications are denoted by CALL, e.g. (call f x y).
Operator is F."
  (cadr exp))

(defun operands-1 (exp)
  "Applications are denoted by CALL, e.g. (call f x y).
Operands are (X Y)."
  (cddr exp))

;;; Exercise 4.2 END

;;; Exercise 4.3 START

(defun %eval (exp env)
  "Data-directed dispatch."
  (cond ((self-evaluating-p exp) exp)
        ((variablep exp) (lookup-variable-value exp env))
        ((and (symbolp (car exp)) (get (car exp) 'eval))
         (funcall (get (car exp) 'eval) exp env))
        ((applicationp exp)
         (%apply (%eval (operator exp) env)
                 (list-of-values (operands exp) env)))
        (t (error "Unknown expression type in ~a -- EVAL" exp))))

(defun install-function (tag fn)
  (setf (get tag 'eval) fn))

(install-function 'quote (lambda (exp env)
                           (declare (ignore env))
                           (text-of-quotation exp)))
(install-function 'set! #'eval-assignment)
(install-function 'define #'eval-definition)
(install-function 'if #'eval-if)
(install-function 'lambda (lambda (exp env)
                            (make-procedure (lambda-parameters exp)
                                            (lambda-body exp)
                                            env)))
(install-function 'begin (lambda (exp env)
                           (eval-sequence (begin-actions exp) env)))
(install-function 'cond (lambda (exp env)
                          (%eval (cond->if exp) env)))

;;; Exercise 4.3 END

;;; Exercise 4.4 START

(defun andor-exps (exp)
  (cdr exp))

(defun eval-and (exp env)
  (labels ((rec (exps)
             (cond ((null exps) 'true)
                   ((truep (%eval (first-exp exps) env))
                    (rec (rest-exps exps)))
                   (t 'false))))
    (rec (andor-exps exp))))

(defun eval-or (exp env)
  (labels ((rec (exps)
             (cond ((null exps) 'false)
                   ((truep (%eval (first-exp exps) env)) 'true)
                   (t (rec (rest-exps exps))))))
    (rec (andor-exps exp))))

(install-function 'and #'eval-and)
(install-function 'or #'eval-or)

;;; We can also convert an AND or OR expression to IFs:

(defun and->if (exp)
  (labels ((expand (exps)
             (if (null exps)
                 'true
                 (make-if (first-exp exps)
                          (expand (rest-exps exps))
                          'false))))
    (expand (andor-exps exp))))

(defun or->if (exp)
  (labels ((expand (exps)
             (if (null exps)
                 'false
                 (make-if (first-exp exps)
                          'true
                          (expand (rest-exps exps))))))
    (expand (andor-exps exp))))

(install-function 'and-1 (lambda (exp env)
                           (%eval (and->if exp) env)))
(install-function 'or-1 (lambda (exp env)
                          (%eval (or->if exp) env)))

;;; Exercise 4.4 END

;;; Exercise 4.5 START

(defun cond-arrow-clause-p (clause)
  (eq (cadr clause) '=>))

(defun cond-arrow-action (clause)
  (caddr clause))

(defun make-application (operator operands)
  (cons operator operands))

(defun expand-clauses (clauses)
  "Also handles the => syntax."
  (if (null clauses)
      'false
      (let ((first (car clauses))
            (rest (cdr clauses)))
        (cond ((cond-else-clause-p first)
               (if (null rest)
                   (sequence->exp (cond-actions first))
                   (error "ELSE clause isn't last in ~a -- COND->IF" clauses)))
              ((cond-arrow-clause-p first)
               (let ((sym (gensym)))
                 (make-application
                  (make-lambda (list sym)
                    (list (make-if sym
                                   (make-application (cond-arrow-action first)
                                                     (list sym))
                                   (expand-clauses rest))))
                  (list (cond-predicate first)))))
              (t (make-if (cond-predicate first)
                          (sequence->exp (cond-actions first))
                          (expand-clauses rest)))))))

;;; Exercise 4.5 END

;;; Exercise 4.6 START

(defun let-vars (exp)
  (mapcar #'car (cadr exp)))

(defun let-vals (exp)
  (mapcar #'cadr (cadr exp)))

(defun let-body (exp)
  (cddr exp))

(defun let->combination (exp)
  (flet ((expand (vars vals exps)
           (make-application
            (make-lambda vars exps)
            vals)))
    (expand (let-vars exp) (let-vals exp) (let-body exp))))

;;; We will install let->combination-1 instead.
#+nil
(install-function 'let (lambda (exp env)
                         (%eval (let->combination exp) env)))

;;; Exercise 4.6 END

;;; Exercise 4.7 START

;;; It's sufficient to use the already implemented LET.

(defun make-let (vars vals exps)
  (append (list 'let (mapcar #'list vars vals))
          exps))

(defun let*->nested-lets (exp)
  (labels ((expand (vars vals exps)
             (if (null vars)
                 exps
                 (list (make-let (list (car vars))
                                 (list (car vals))
                                 (expand (cdr vars) (cdr vals) exps))))))
    (car (expand (let-vars exp) (let-vals exp) (let-body exp)))))

(install-function 'let* (lambda (exp env)
                          (%eval (let*->nested-lets exp) env)))

;;; Exercise 4.7 END

;;; Exercise 4.8 START

;;; This is a good place to use the Y-combinator!
;;; But we can also use define, which is simpler.

(defun named-let-p (exp)
  (variablep (cadr exp)))

(defun named-let-name (exp)
  (cadr exp))

(defun named-let-vars (exp)
  (mapcar #'car (caddr exp)))

(defun named-let-vals (exp)
  (mapcar #'cadr (caddr exp)))

(defun named-let-body (exp)
  (cdddr exp))

(defun make-definition (var exp)
  (list 'define var exp))

(defun let->combination-1 (exp)
  "Supports named LETs."
  (flet ((expand (name vars vals exps)
           (make-begin
            (list (make-definition name
                    (make-lambda vars exps))
                  (make-application name vals)))))
    (if (named-let-p exp)
        (expand (named-let-name exp)
                (named-let-vars exp)
                (named-let-vals exp)
                (named-let-body exp))
        (let->combination exp))))

(install-function 'let (lambda (exp env)
                         (%eval (let->combination-1 exp) env)))

;;; Exercise 4.8 END

;;; Exercise 4.9 START

;;; Standard DO:
;;;   (do bindings (pred . exps) body)
;;; where a binding is (var start next),
;;; PRED gives the condition to stop,
;;; and EXPS is a list of expressions to generate the value.

;;; Example:
;; (do ((a 1 (+ a b))
;;      (b 1 a))
;;     ((> a 100) (list a b))
;;   (display a))
;;; => (144 89), printing 1 2 3 5 8 13 21 34 55 89

;;; Expansion:
;; (let rec ((a 1)
;;           (b 1))
;;   (if (> a 100)
;;       (begin (list a b))
;;       (begin (display a)
;;              (rec (+ a b) a))))

(defun do-vars (exp)
  (mapcar #'car (cadr exp)))

(defun do-vals (exp)
  (mapcar #'cadr (cadr exp)))

(defun do-updates (exp)
  (mapcar #'caddr (cadr exp)))

(defun do-predicate (exp)
  (caaddr exp))

(defun do-value-exps (exp)
  (cdaddr exp))

(defun do-body (exp)
  (cdddr exp))

(defun make-named-let (name vars vals exps)
  (append (list 'let name (mapcar #'list vars vals))
          exps))

(defun do->letcond (exp)
  (let* ((sym (gensym))
         (next (make-application sym (do-updates exp))))
    (make-named-let sym (do-vars exp) (do-vals exp)
      (list (make-if (do-predicate exp)
                     (make-begin (do-value-exps exp))
                     (make-begin (append (do-body exp) (list next))))))))

(install-function 'do (lambda (exp env)
                         (%eval (do->letcond exp) env)))

;;; Exercise 4.9 END

;;; Exercise 4.10 START

;;; If we cannot change EVAL, we still have to use the first element as a tag.
;;; But otherwise we are free to do (almost) anything.
;;; Exercise 4.5 has already shown that we can modify the syntax,
;;; just by changing EXPAND-CLAUSES.

;;; Exercise 4.10 END


;;; Section 4.1.3

(defun truep (x)
  (not (eq x 'false)))

(defun falsep (x)
  (eq x 'false))

(defun make-procedure (parameters body env)
  (list 'procedure parameters body env))

(defun compound-procedure-p (p)
  (tagged-list-p p 'procedure))

(defun procedure-parameters (p)
  (cadr p))

(defun procedure-body (p)
  (caddr p))

(defun procedure-environment (p)
  (cadddr p))

(defun enclosing-environment (env)
  (cdr env))

(defun first-frame (env)
  (car env))

(defparameter *the-empty-environment* '())

(defun make-env-frame (variables values)
  "MAKE-FRAME was used in Section 2.2.4."
  (cons variables values))

(defun frame-variables (frame)
  (car frame))

(defun frame-values (frame)
  (cdr frame))

(defun add-binding-to-frame (var val frame)
  (push var (car frame))
  (push val (cdr frame)))

(defun extend-environment (vars vals base-env)
  (if (= (length vars) (length vals))
      (cons (make-env-frame vars vals) base-env)
      (if (< (length vars) (length vals))
          (error "Too many arguments supplied: ~a, ~a" vars vals)
          (error "Too few arguments supplied: ~a, ~a" vars vals))))

(defun lookup-variable-value (var env)
  "This version uses an extra ENV argument for SCAN.
This is easier to understand than the nested defines."
  (labels ((scan (vars vals env)
             (cond ((null vars)
                    (env-loop (enclosing-environment env)))
                   ((eq var (car vars))
                    (car vals))
                   (t (scan (cdr vars) (cdr vals) env))))
           (env-loop (env)
             (if (eq env *the-empty-environment*)
                 (error "Unbound variable: ~a" var)
                 (let ((frame (first-frame env)))
                   (scan (frame-variables frame) (frame-values frame) env)))))
    (env-loop env)))

(defun set-variable-value (var val env)
  "As above."
  (labels ((scan (vars vals env)
             (cond ((null vars)
                    (env-loop (enclosing-environment env)))
                   ((eq var (car vars))
                    (setf (car vals) val))
                   (t (scan (cdr vars) (cdr vals) env))))
           (env-loop (env)
             (if (eq env *the-empty-environment*)
                 (error "Unbound variable: ~a -- SET!" var)
                 (let ((frame (first-frame env)))
                   (scan (frame-variables frame) (frame-values frame) env)))))
    (env-loop env)))

(defun define-variable (var val env)
  (let ((frame (first-frame env)))
    (labels ((scan (vars vals)
               (cond ((null vars)
                      (add-binding-to-frame var val frame))
                     ((eq var (car vars))
                      (setf (car vals) val))
                     (t (scan (cdr vars) (cdr vals))))))
      (scan (frame-variables frame) (frame-values frame)))))

;;; Exercise 4.11 START

(defun make-env-frame-1 (variables values)
  "Representation as a list of bindings."
  (mapcar #'cons variables values))

(defun frame-variables-1 (frame)
  "Representation as a list of bindings."
  (mapcar #'car frame))

(defun frame-values-1 (frame)
  "Representation as a list of bindings."
  (mapcar #'cdr frame))

(defun add-binding-to-frame-1 (var val frame)
  "Representation as a list of bindings."
  (push (cons var val) frame))

;;; Exercise 4.11 END

;;; Exercise 4.12 START

(defun find-var-in-frame (frame var exp final)
  "EXP is a function taking a list, the head is what we searched for.
FINAL is a function taking no arguments, called when not found."
  (labels ((scan (vars vals)
             (cond ((null vars)
                    (funcall final))
                   ((eq var (car vars))
                    (funcall exp vals))
                   (t (scan (cdr vars) (cdr vals))))))
    (scan (frame-variables frame) (frame-values frame))))

(defun find-var-in-env (env var exp)
  "EXP is a function taking a list, the head is what we searched for."
  (labels ((env-loop (env)
             (if (eq env *the-empty-environment*)
                 (error "Unbound variable: ~a" var)
                 (let ((frame (first-frame env))
                       (next (enclosing-environment env)))
                   (find-var-in-frame frame var exp
                                      (lambda () (env-loop next)))))))
    (env-loop env)))

(defun lookup-variable-value-1 (var env)
  "Using FIND-VAR-IN-ENV."
  (find-var-in-env env var
    (lambda (vals) (car vals))))

(defun set-variable-value-1 (var val env)
  "Using FIND-VAR-IN-ENV."
  (find-var-in-env env var
    (lambda (vals) (setf (car vals) val))))

(defun define-variable-1 (var val env)
  "Using FIND-VAR-IN-FRAME."
  (let ((frame (first-frame env)))
    (find-var-in-frame frame var
      (lambda (vals) (setf (car vals) val))
      (lambda () (add-binding-to-frame var val frame)))))

;;; Exercise 4.12 END

;;; Exercise 4.13 START

;;; From its name, one would think that the symbol
;;; does not have a binding after the operation.
;;; That would mean removing it from all frames.

;;; On the other hand, this could unbind symbols
;;; that enclosing procedures depend on.

(defun make-unbound (var env)
  "Only in the first frame."
  (let ((frame (first-frame env)))
    (find-var-in-frame frame var
      (lambda (vals) (pop vals))
      (lambda () nil))))

(defun make-unbound-1 (var env)
  "In all frames."
  (unless (eq env *the-empty-environment*)
    (make-unbound var env)
    (make-unbound-1 var (enclosing-environment env))))

;;; Exercise 4.13 END


;;; Section 4.1.4

(defun setup-environment ()
  (let ((initial-env (extend-environment (primitive-procedure-names)
                                         (primitive-procedure-objects)
                                         *the-empty-environment*)))
    (define-variable 'true 'true initial-env)
    (define-variable 'false 'false initial-env)
    initial-env))

(defvar *the-global-environment*)

(defun primitive-procedure-p (proc)
  (tagged-list-p proc 'primitive))

(defun primitive-implementation (proc)
  (cadr proc))

(defun lisp->scheme (x)
  "For convenience."
  (cond ((atom x) (list x (symbol-function x)))
        ((functionp (second x)) x)
        (t (list (first x) (symbol-function (second x))))))

(defun tfify (fn)
  "Create a TRUE/FALSE function."
  (lambda (&rest x)
    (if (apply fn x)
        'true
        'false)))

(defparameter *primitive-procedures*
  (mapcar #'lisp->scheme
          `(car cdr cadr cons list assoc + - * / (display print)
            (null? ,(tfify #'null)) (eq? ,(tfify #'eq))
            (< ,(tfify #'<)) (> ,(tfify #'>)) (= ,(tfify #'=)))))

(defun primitive-procedure-names ()
  (mapcar #'car *primitive-procedures*))

(defun primitive-procedure-objects ()
  (mapcar (lambda (proc)
            (list 'primitive (cadr proc)))
          *primitive-procedures*))

(defun apply-primitive-procedure (proc args)
  (apply (primitive-implementation proc) args))

(defparameter *input-prompt* ";;; M-Eval input:")
(defparameter *output-prompt* ";;; M-Eval value:")

(defun driver-loop ()
  (prompt-for-input *input-prompt*)
  (let* ((input (read))
         (output (%eval input *the-global-environment*)))
    (announce-output *output-prompt*)
    (user-print output))
  (driver-loop))

(defun prompt-for-input (string)
  (format t "~%~%~a~%" string))

(defun announce-output (string)
  (format t "~%~a~%" string))

(defun user-print (object)
  (if (compound-procedure-p object)
      (print (list 'compound-procedure
                   (procedure-parameters object)
                   (procedure-body object)
                   '<procedure-env>))
      (print object)))

(setf *the-global-environment* (setup-environment))

;;; Exercise 4.14 START

;;; Defining MAP as
;;
;; (define (map fn lst)
;;   (if (null? lst)
;;       '()
;;       (cons (fn (car lst))
;;             (map fn (cdr lst)))))
;;
;;; obviously works.

;;; The interesting question is, why does the host's map not work?
;;; The problem is the function argument.
;;; The host function needs a primitive host function,
;;; while what we have is a procedure (either primitive or compound).

;;; Exercise 4.14 END


;;; Section 4.1.5


;;Local Variables:
;;eval: (progn
;;
;;   (defvar hide-exercise-overlays nil)
;;
;;   (defun hide-exercise-open/close (overlay &optional closep)
;;     (cond (closep
;; 	   (overlay-put overlay 'display
;; 			(concat (overlay-get overlay 'title) "..."))
;; 	   (overlay-put overlay 'face '(:underline t))
;; 	   (overlay-put overlay 'help-echo "Press C-c s to toggle visibility")
;; 	   (overlay-put overlay 'hidden t))
;; 	  (t
;; 	   (overlay-put overlay 'display nil)
;; 	   (overlay-put overlay 'face nil)
;; 	   (overlay-put overlay 'help-echo nil)
;; 	   (overlay-put overlay 'hidden nil))))
;;
;;   (defun hide-exercise-toggle (overlay)
;;     (hide-exercise-open/close overlay (not (overlay-get overlay 'hidden))))
;;
;;   (defun hide-exercise-hide-all ()
;;     (interactive)
;;     (remove-overlays (point-min) (point-max) 'type 'hide-exercise)
;;     (setq hide-exercise-overlays nil)
;;     (let ((regexp "^\\(;;; Exercise [^ ]+ \\)START"))
;;       (save-excursion
;; 	(goto-char (point-min))
;; 	(while (search-forward-regexp regexp nil t)
;; 	  (let ((start (line-beginning-position))
;; 		(title (match-string 1)))
;; 	    (search-forward (concat title "END"))
;; 	    (let ((overlay (make-overlay start (point) nil t nil)))
;; 	      (push overlay hide-exercise-overlays)
;; 	      (overlay-put overlay 'title title)
;; 	      (overlay-put overlay 'type 'hide-exercise)
;; 	      (hide-exercise-toggle overlay)))))))
;;
;;   (defun hide-exercise-show-all ()
;;     (interactive)
;;     (dolist (overlay hide-exercise-overlays)
;;       (hide-exercise-open/close overlay)))
;;
;;   (defun hide-exercise-find-overlay ()
;;     (let ((point (point))
;; 	  (lst hide-exercise-overlays)
;; 	  (found nil))
;;       (while (and lst (not found))
;; 	(let ((overlay (car lst)))
;; 	  (when (and (<= (overlay-start overlay) point)
;; 		     (<= point (overlay-end overlay)))
;; 	    (setq found overlay))
;; 	  (setq lst (cdr lst))))
;;       found))
;;
;;   (defun hide-exercise-at-point ()
;;     (interactive)
;;     (let ((overlay (hide-exercise-find-overlay)))
;;       (when overlay
;; 	(hide-exercise-toggle overlay))))
;;
;;   (local-set-key (kbd "C-c s") 'hide-exercise-at-point)
;;
;;   (hide-exercise-hide-all)
;;
;;   t)
;;End:
