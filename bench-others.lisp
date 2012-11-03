;;; Copyright (c) 2011, James M. Lawrence. All rights reserved.
;;; 
;;; Redistribution and use in source and binary forms, with or without
;;; modification, are permitted provided that the following conditions
;;; are met:
;;; 
;;;     * Redistributions of source code must retain the above copyright
;;;       notice, this list of conditions and the following disclaimer.
;;; 
;;;     * Redistributions in binary form must reproduce the above
;;;       copyright notice, this list of conditions and the following
;;;       disclaimer in the documentation and/or other materials provided
;;;       with the distribution.
;;; 
;;;     * Neither the name of the project nor the names of its
;;;       contributors may be used to endorse or promote products derived
;;;       from this software without specific prior written permission.
;;; 
;;; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
;;; "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
;;; LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
;;; A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
;;; HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
;;; SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
;;; LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
;;; DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
;;; THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
;;; (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
;;; OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

(in-package #:bench-others)

(defparameter *trials* 8)

(defparameter *rehearsals* 4)

(defparameter *repeat-gc* 20)

(defparameter *benches* '(bench-fib))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter *apis*
    '((eager-future2:pexec      eager-future2:yield)
      (pcall:pexec              pcall:join)
      (lparallel.promise:future lparallel.promise:force)))

  (defparameter *fib-fns*  '(eager-fib
                             pcall-fib
                             lparallel-fib
                             lparallel-fib*)))

(defun reset ()
  (sleep 0.2)
  (repeat (* 3 *repeat-gc*) (gc)))

(defun make-cleanup ()
  (make-bench-spec
   :args-fn (lambda () nil)
   :exec-fn (lambda () (repeat *repeat-gc* (gc)))
   :desc-fn (lambda (&rest args) (declare (ignore args)))))

(defmacro/once collect-trials (&once trials &body body)
  `(progn
     (assert (>= ,trials 2))
     (collect (make-cleanup))
     (repeat (- ,trials 1)
       (collect ,@body))))

(defmacro define-fib ()
  `(progn
     ,@(loop
          :for name :in *fib-fns*
          :for (future force) :in *apis*
          :collect `(defun ,name (n)
                      (if (< n 2)
                          n
                          (let* ((f1 (,future (,name (- n 1))))
                                 (f2 (,name (- n 2))))
                            (+ (,force f1) f2)))))))

(define-fib)

(defpun* lparallel-fib* (n)
  (if (< n 2)
      n
      (plet ((a (lparallel-fib* (- n 1)))
             (b (lparallel-fib* (- n 2))))
        (+ (the fixnum a) (the fixnum b)))))

(defun bench-fib ()
  (let ((fns        *fib-fns*)
        (trials     *trials*)
        (rehearsals *rehearsals*))
    (bench
     (length fns)
     trials
     rehearsals
     (collecting1
       (dolist (fn fns)
         (dolist (n '(8 12 16 17 18 19 20))
           (rebind (fn n)
             (collect-trials trials
               (make-bench-spec
                :args-fn (lambda ()
                           (list n))
                :exec-fn (lambda (n)
                           (funcall fn n))
                :desc-fn (lambda (time)
                           (format nil
                                   "~&n ~3,d | ~15,a ~8,d~%"
                                   n fn time)))))))))))

(defun run-suite (worker-count reset-fn &rest fns)
  (when fns
    (lparallel-bench:with-temp-kernel ((- worker-count 1) :spin-count 20000)
      (sleep 1.0)
      (dolist (fn fns)
        (funcall reset-fn)
        (funcall fn))))
  nil)

(defun execute (worker-count)
  (setf (pcall:thread-pool-size) worker-count)
  (eager-future2:advise-thread-pool-size worker-count)
  (apply #'run-suite worker-count #'reset *benches*))
