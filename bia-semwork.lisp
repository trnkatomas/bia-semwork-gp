#!/usr/local/bin/sbcl --script
(defparameter *grammar-rules* (list '(get-2-ar-op get-expresion get-expresion)
                                '(get-1-ar-op get-expresion)
                                '(get-var)
                                '(get-constant)))
(defparameter *2-ar-op* '(* + / -))
(defparameter *1-ar-op* '(cos sin tan exp))
(defparameter *vars* '(X Y))

(defparameter *generations* 100)
(defparameter *max-depth* 20)
(defparameter *pop-size* 500)
(defparameter *evaluations* 1000)
(defparameter *tournament-size* 5)
(defparameter *mutate-probability* 90) ; in percentage
(defparameter *size-of-new-pop* 2); 2*(pop-size/size-of-new-pop) is selected for breading
(defparameter *error-constant* 10000000)
(defparameter *num-of-samples* 500)
(defparameter *best-individual* 100000000000)
(defparameter *file-output-evo* (concatenate 'string (namestring *default-pathname-defaults*) "res-evo"))
(defparameter *file-input-dir* (namestring *default-pathname-defaults*))
(defparameter *input-fns* (list "ackley_fun" "beale_fun" "booth_fun" "de_jong_fun" "griewangk_fun" "himmelblau_fun" "rosenbrock_fun"))
(defparameter *functions* (list '(+ (* -20 (EXP (* -0.2 (SQRT (/  (+ (* X X) (* Y Y)) 2)))) (* -1 (EXP (/ (+ (COS (* 2 pi X)) (COS (* 2 pi Y))) 2)))) 20 (EXP 1)) ;ackley fun
                                '(+ (* (- 1.5 (* X (- 1 Y))) (- 1.5 (* X (- 1 Y))))
                                    (* (- 2.25 (* X (- 1 (* Y Y)))) (- 2.25 (* X (- 1 (* Y Y)))))
                                    (* (- 2.625 (* X (- 1 (* Y Y Y)))) (- 2.625 (* X (- 1 (* Y Y Y)))))) ; beale_fun 
                                '(+ (* (+ X (* 2 Y) -7) (+ X (* 2 Y) -7)) (* (+ (* 2 X) Y -5) (+ (* 2 X) Y -5))) ; booth_fun
                                '(+ (* X X) (* Y Y)) ; de_jong_fun
                                '(+ (* 100 (* (- Y (* X X)) (- Y (* X X)))) (* (- 1 X) (- 1 X))) ; rosenbrock_fun
                                '(+ (* (+ (* X X) Y -11) (+ (* X X) Y -11)) (* (+ X (* Y Y) -7) (+ X (* Y Y) -7))) ; himmelblau_fun
                                '(+ (/ (* X X) 4000) (/ (* Y Y) 4000) (* -1 (* (COS X) (COS (/ Y (SQRT 2))))) 1) ; griewangk_fun
                                ))

(defun read-data (&optional (dataFncSwitch t) (funcId 0))
  (let* (
         (file (concatenate 'string *file-input-dir* 
                            (if dataFncSwitch (nth funcId *input-fns*) "samples") ".txt"))
        (stream (open file))
        (content (read stream))
        ) 
        (progn (close stream) (values content))))

(defun write-fitness (data &optional (func-name "") (gen 0))
(progn 
  (with-open-file (stream (concatenate 'string *file-output-evo* "fitness-" func-name "-" (write-to-string gen) ".txt") :direction :output :if-exists :supersede)
    (mapcar (lambda (x) (format stream "~f " x)) data)
  ) ))

(defun write-results (origData evolved &optional (func-name "") (gen 0))
(progn 
  (with-open-file (stream (concatenate 'string *file-output-evo* "-" func-name "-" (write-to-string gen) ".txt") :direction :output :if-exists :supersede)
    (format stream "~S ~S~%" (car evolved) (second evolved))
    (mapcar (lambda (x) (format stream "~f " x)) (third evolved))
  ) ) )

(defun get-2-ar-op ()
  (nth (random (length *2-ar-op*) (make-random-state t)) *2-ar-op*))

(defun get-1-ar-op ()
  (nth (random (length *1-ar-op*) (make-random-state t)) *1-ar-op*))

(defun get-var ()
  (nth (random (length *vars*) (make-random-state t)) *vars*))

(defun get-constant ()
  (random *max-depth* (make-random-state t)))

(defun get-expresion ()
  (nth (random (length *grammar-rules*) (make-random-state t)) *grammar-rules*))

(defun generate-function (input &optional (depth 0))
  (if (= depth *max-depth*)
      (if (eq 'get-expresion input) (get-var) (funcall input))
    (if (listp input) ((lambda (ll) (if (= 1 (length ll)) (car ll) ll))  (mapcar (lambda (x) (generate-function x (+ 1 depth))) input))
      (if (eq 'get-expresion input) (generate-function (funcall input) (+ 1 depth))
        (funcall input)))))

(defun new-individual ()
  (generate-function 'get-expresion))

;; Koza's approach to crossover
(defun count-crossover-points (program)
  (if (consp program)
      (+ 1 (reduce #'+ (mapcar #'count-crossover-points
                               (rest program))))
      1))

(defun get-subtree (tree pointer-to-tree index)
  (if (= index 0)
      (values pointer-to-tree (copy-tree tree) index)
      (if (consp tree)
          (do* ((tail (rest tree) (rest tail))
                (argument (first tail) (first tail)))
               ((not tail) (values nil nil index))
            (multiple-value-bind
                (new-pointer new-tree new-index)
                (get-subtree argument tail (- index 1))
              (if (= new-index 0)
                  (return
                    (values new-pointer new-tree new-index))
                  (setf index new-index))))
          (values nil nil index))))

(defun mutate (indi)
    (let* (
           (new-indi (copy-tree indi))
           (mutate-point (random (count-crossover-points indi) (make-random-state t)))
           (new-subtree (new-individual))
           )
      (multiple-value-bind (subtree-pointer sub-fragment) (get-subtree new-indi new-indi mutate-point) (setf (first subtree-pointer) new-subtree))
new-indi 
))

(defun cross-over (first second)
  (let (
         (new-f (copy-tree first))
         (new-s (copy-tree second))
         (cross-p-f (random (count-crossover-points first) (make-random-state t)))
         (cross-p-s (random (count-crossover-points second) (make-random-state t)))
         )
    (multiple-value-bind  (f-subtree-pointer f-fragment) (get-subtree new-f new-f cross-p-f)
    (multiple-value-bind  (s-subtree-pointer s-fragment) (get-subtree new-s new-s cross-p-s)
    (setf (first f-subtree-pointer) s-fragment)
    (setf (first s-subtree-pointer) f-fragment)))
    (list new-f new-s)
))

(defun func-depth (function)
  (if (listp function) 
      (+ 1 (apply #'max (mapcar (lambda (x) (func-depth x)) function)))
        1))

(defun tournament-selection (pop tournament-size &optional (trial 1) (ind nil))
  (if (= trial tournament-size) ind
  (let* (
         (index (random (length pop) (make-random-state t)))
        (new-ind (nth index pop))
        )
    (tournament-selection pop tournament-size (+ trial 1) (if ind (if (< (second new-ind) (second ind)) new-ind ind) new-ind)        
))))

(defun init-pop (&optional (size 0) (pop '()))
  (if (< size *pop-size*)
      (let* (
             (new (new-individual))
             )
        (if (member new pop :test #'equal) (init-pop size pop)
        (init-pop (+ size 1) (append pop (list new)))))
    pop))

(defun norm-err-fun (x y)
  (apply #'+ (mapcar (lambda (a b) (sqrt (* (- a b) (- a b)))) x y)))

(defun rms-err-fun (x y &optional (lx (length x)))
  (handler-case (sqrt (* (/ 1 lx) (apply #'+ (mapcar (lambda (a b) (* (- a b) (- a b))) x y))))
      (arithmetic-error () *error-constant*)))

(defun eval-pop (pop data function)
  (let* (
         (funcVals (mapcar (lambda (x) (test-val function (car x) (second x))) data))
         (popVals (mapcar (lambda (x) (mapcar (lambda (z) (test-val x (car z) (second z))) data)) pop))
         (errors (mapcar (lambda (x) (rms-err-fun x funcVals)) popVals))
         )
    (list funcVals (mapcar (lambda (x y z) (list x y z)) pop errors popVals))))

(defun sort-pop (pop data function)
(let* (
      (res (eval-pop pop data function))
      (funcVals (car res))
      (results (cdr res))
      (sorted-pop (sort (car results) #'< :key #'cadr))      
      (best (car sorted-pop))
      )
(setq *best-individual* (if (numberp (second best)) (if (< (second best) *best-individual*) (second best) *best-individual*)))
sorted-pop
))

(defun print-pop (pop)
  (mapcan (lambda (x) (pprint x)) pop))

(defun test-val (expr value-x value-y)
  (progn (setq x value-x) (setq y value-y)
    (handler-case (eval expr)
      (arithmetic-error () *error-constant*));(format t "division by zero caught!~%")))
    ))

(defun choose-for-crossover (pop number-of-selected &optional (selected 0) (output nil))
  (if (= selected number-of-selected) output
    (let (
          (ind (tournament-selection pop *tournament-size*))
          )
      (choose-for-crossover pop number-of-selected (+ selected 1) (if output (cons ind output) (list ind))
 ))))
  
(defun repair (ind)
  (cond ((not (listp ind)) ind)
        ((= (length ind) 1) (if (= (func-depth ind) 2) (car ind) (repair (car ind))))
        (t (mapcar #'repair ind))))

(defun valid (ind)
  (cond ((not (listp ind)) t)
        ((or (member (car ind) *2-ar-op*) (member (car ind) *1-ar-op*)) t)
        (t nil)))

(defun check-depth (data)
  (mapcan (lambda (x) (if (<= (func-depth (car x)) *max-depth*) (list x) (if (numberp (second x))
                                                                           (if (< (second x) *best-individual*)
                                                                            (progn (setq *best-individual* (second x))
                                                                                   (setq *max-depth* (func-depth (car x)))
                                                                                    (values (list x))))))) data))

(defun merge-to-new-pop (old-pop off-pop &optional (new-pop '()) (pop-size *pop-size*))
  (cond  ((= (length new-pop) pop-size) new-pop)
         ;((not old-pop) (merge-to-new-pop old-pop (cdr off-pop) (cons (car off-pop) new-pop)))
         ((not off-pop) (merge-to-new-pop (cdr old-pop) off-pop (cons (car old-pop) new-pop)))
         ((<= (second (car old-pop)) (second (car off-pop))) (merge-to-new-pop (cdr old-pop) off-pop (cons (car old-pop) new-pop)))
         ((> (second (car old-pop)) (second (car off-pop)))(merge-to-new-pop old-pop (cdr off-pop) (cons (car off-pop) new-pop)))))

(defun choose-random-data (data count &optional (left count))
    (if (= left 0) (subseq data (- (length data) count) (length data))
      (progn (rotatef (nth (random (- (- (length data) 1) (- count left)) (make-random-state t)) data) (nth (- (- (length data) 1) (- count left)) data))
        (choose-random-data data count (- left 1)))))

(defun run-it (&optional (function-num 0) (pop (init-pop)) (function (nth function-num *functions*)) (data (choose-random-data (read-data nil) (- (* 5 *num-of-samples*) 1)))
                (test-data (subseq data 0 *num-of-samples*)) (train-data (subseq data *num-of-samples* (length data))) (gen-count 0))
(if (= gen-count *generations*) (car pop)
(let* (
       (sorted-pop (sort-pop pop train-data function))
       (ch-for-c1 (choose-for-crossover sorted-pop (/ *pop-size* *size-of-new-pop*)))
       (ch-for-c2 (choose-for-crossover sorted-pop (/ *pop-size* *size-of-new-pop*)))
       (cross-overed (mapcar (lambda (a b) (cross-over (if (listp (car a)) (car a) (list (car a))) (if (listp (car b)) (car b) (list (car b))))) ch-for-c1 ch-for-c2))
       (flatten-crossovered (mapcan (lambda (x) (list (car x) (second x))) cross-overed))
       (valid-crossovered (mapcan (lambda (x) (if (valid (repair x)) (list (repair x)))) flatten-crossovered))
       (mutated (mapcar (lambda (x) (if (> (random 100 (make-random-state t)) *mutate-probability*) (mutate (if (listp x) x (list x))))) valid-crossovered))
       (valid-mutated (mapcan (lambda (x) (if (valid (repair x)) (list (repair x)))) mutated))
       (sorted-ofspring-pop-t (sort-pop valid-mutated test-data function))
       (sorted-pop-t (sort-pop pop test-data function))
       (sorted-ofspring-pop-t-chd (check-depth sorted-ofspring-pop-t))
       (new-pop (reverse (merge-to-new-pop sorted-pop-t sorted-ofspring-pop-t-chd)))
       (best (car new-pop))
       )
(format t "~d. generation - f: ~S - best function ~s with error: ~f~%" gen-count (nth function-num *input-fns*) (car best) (second best))
(write-results nil (car new-pop) (nth function-num *input-fns*) gen-count)
(write-fitness (mapcar #'second new-pop) (nth function-num *input-fns*) gen-count)
 (run-it function-num (mapcar #'car new-pop)  function data test-data train-data (+ gen-count 1))
)))

(defun gen-sequence (length)
(let (
  (seq (make-sequence 'list length :initial-element 1))
)
(reverse (reduce (lambda (acc x) (cons (+ (first acc) x) acc)) seq :initial-value (list 0)
  ))))

; will run the evolution for all the functions
(mapcar (lambda (x) (run-it x)) (gen-sequence (length *functions*)))
; will run the evolution fot the first funciton
;(run-it)


	