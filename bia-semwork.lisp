#!/usr/local/bin/sbcl --script
(defparameter *grammar-rules* (list '(get-2-ar-op get-expresion get-expresion)
                                '(get-1-ar-op get-expresion)
                                '(get-var)
                                '(get-constant)))
(defparameter *2-ar-op* '(* + / -))
(defparameter *1-ar-op* '(cos sin tan exp))
(defparameter *vars* '(X Y))

(defparameter *generations* 100)
(defparameter *max-depth* 10)
(defparameter *pop-size* 500)
(defparameter *evaluations* 1000)
(defparameter *tournament-size* 5)
(defparameter *mutate-probability* 90) ; in percentage
(defparameter *error-constant* 10000000)
(defparameter *num-of-samples* 500)
(defparameter *file-output-orig* "/home/tomas/Documents/skola/FEL-OI/magistr/2.semestr/BIA/semestralka/res-orig")
(defparameter *file-output-evo* "/home/tomas/Documents/skola/FEL-OI/magistr/2.semestr/BIA/semestralka/res-evo")
(defparameter *file-input* "/home/tomas/Documents/skola/FEL-OI/magistr/2.semestr/BIA/semestralka/input.txt")
(defparameter *file-input-dir* "/home/tomas/Documents/skola/FEL-OI/magistr/2.semestr/BIA/semestralka/")
(defparameter *input-fns* (list "ackley_fun" "beale_fun" "booth_fun" "de_jong_fun" "griewangk_fun" "himmelblau_fun" " rosenbrock_fun"))
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

(defun write-results (origData evolved &optional (func-name "") (gen 0))
(progn 
  ;(with-open-file (stream (concatenate 'string *file-output-orig* "-" (write-to-string gen) ".txt") :direction :output :if-exists :supersede)
  ;  (mapcar (lambda (x) (format stream "~f " x)) origData)
  ;)
  (with-open-file (stream (concatenate 'string *file-output-evo* "-" func-name "-" (write-to-string gen) ".txt") :direction :output :if-exists :supersede)
    (format stream "~S ~S~%" (car evolved) (second evolved))
    (mapcar (lambda (x) (format stream "~f " x)) (third evolved))
  ) ) )

(defun get-2-ar-op ()
  (nth (random (length *2-ar-op*)) *2-ar-op*))

(defun get-1-ar-op ()
  (nth (random (length *1-ar-op*)) *1-ar-op*))

(defun get-var ()
  (nth (random (length *vars*)) *vars*))

(defun get-constant ()
  (random *max-depth*))

(defun get-expresion ()
  (nth (random (length *grammar-rules*)) *grammar-rules*))

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
  "Counts the number of points in the tree (program).  
   This includes functions as well as terminals."
  (if (consp program)
      (+ 1 (reduce #'+ (mapcar #'count-crossover-points
                               (rest program))))
      1))

(defun get-subtree (tree pointer-to-tree index)
  "Given a tree or subtree, a pointer to that tree/subtree and
   an index return the component subtree that is numbered by
   Index.  We number left to right, depth first."
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
           (mutate-point (random (count-crossover-points indi)))
           (new-subtree (new-individual))
           )
      (multiple-value-bind (subtree-pointer sub-fragment) (get-subtree new-indi new-indi mutate-point) (setf (first subtree-pointer) new-subtree))
new-indi 
))

; zkontrolovat nevalidni reseni, co je pres tak odriznu,
; kdyz ma vetsi hloubku, tak ho tam dam jenom kdyz ma vetsi fitness (uplne nejlepsi)
(defun cross-over (first second)
  (let (
         (new-f (copy-tree first))
         (new-s (copy-tree second))
         (cross-p-f (random (count-crossover-points first)))
         (cross-p-s (random (count-crossover-points second)))
         )
    (multiple-value-bind  (f-subtree-pointer f-fragment) (get-subtree new-f new-f cross-p-f)
    (multiple-value-bind  (s-subtree-pointer s-fragment) (get-subtree new-s new-s cross-p-s)
    (setf (first f-subtree-pointer) s-fragment)
    (setf (first s-subtree-pointer) f-fragment)))
    (list new-f new-s)
))

;select tree  - 10% nodes
(defun func-depth (function)
  (if (listp function) 
      (+ 1 (apply #'max (mapcar (lambda (x) (func-depth x)) function)))
        1))

(defun tournament-selection (pop tournament-size &optional (trial 1) (ind nil))
  (if (= trial tournament-size) ind
  (let* (
         (index (random (length pop)))
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
;  (sqrt (* (- x y) (- x y))))
  (apply #'+ (mapcar (lambda (a b) (sqrt (* (- a b) (- a b)))) x y)))

(defun rms-err-fun (x y &optional (lx (length x)))
  (handler-case (sqrt (* (/ 1 lx) (apply #'+ (mapcar (lambda (a b) (* (- a b) (- a b))) x y))))
      (arithmetic-error () *error-constant*)))
  

(defun eval-pop (pop data function)
  (let* (
         (funcVals (mapcar (lambda (x) (test-val function (car x) (second x))) data))
         (popVals (mapcar (lambda (x) (mapcar (lambda (z) (test-val x (car z) (second z))) data)) pop))
         ;(errors (mapcar (lambda (x) (apply #'+ (mapcar (lambda (a b) (norm-err-fun a b)) x funcVals))) popVals))
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
;(progn 
  ;(write-results funcVals (third best))
  ;(format t "best function ~s with error: ~f~%" (car best) (second best)))
sorted-pop
))

; na konec nutna crosvalidace - 
; pocitani chyby

; ruseni a neruseni nevalidnich stromu?
; nevalidni individualove - opravit? (nejtezsi)
;                         - vratit nejakou konstantu

(defun print-pop (pop)
  (mapcan (lambda (x) (pprint x)) pop))

(defun test-val (expr value-x value-y)
  (progn (setq x value-x) (setq y value-y)
    (handler-case (eval expr)
      ;(division-by-zero () 100000));(format t "division by zero caught!~%")))
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

(defun merge-to-new-pop (old-pop off-pop &optional (new-pop '()) (pop-size *pop-size*))
  (cond  ((= (length new-pop) pop-size) new-pop)
         ;((not old-pop) (merge-to-new-pop old-pop (cdr off-pop) (cons (car off-pop) new-pop)))
         ((not off-pop) (merge-to-new-pop (cdr old-pop) off-pop (cons (car old-pop) new-pop)))
         ((<= (second (car old-pop)) (second (car off-pop))) (merge-to-new-pop (cdr old-pop) off-pop (cons (car old-pop) new-pop)))
         ((> (second (car old-pop)) (second (car off-pop)))(merge-to-new-pop old-pop (cdr off-pop) (cons (car off-pop) new-pop)))))

(defun choose-random-data (data count &optional (left count))
    (if (= left 0) (subseq data (- (length data) count) (length data))
      (progn (rotatef (nth (random (- (- (length data) 1) (- count left))) data) (nth (- (- (length data) 1) (- count left)) data))
        (choose-random-data data count (- left 1)))))

(defun run-it (&optional (function-num 0) (pop (init-pop)) (function (nth function-num *functions*)) (data (choose-random-data (read-data nil) *num-of-samples*)) (gen-count 0))
;  (setq pop (init-pop)) (setq function (car *functions*)) (setq data (subseq (read-data nil) 0 100)))
 (if (= gen-count *generations*) (car pop)
(let* (
       (sorted-pop (sort-pop pop data function))
       (ch-for-c1 (choose-for-crossover sorted-pop (/ *pop-size* 10)))
       (ch-for-c2 (choose-for-crossover sorted-pop (/ *pop-size* 10)))
       (cross-overed (mapcar (lambda (a b) (cross-over (if (listp (car a)) (car a) (list (car a))) (if (listp (car b)) (car b) (list (car b))))) ch-for-c1 ch-for-c2))
       (flatten-crossovered (mapcan (lambda (x) (list (car x) (second x))) cross-overed))
       (valid-crossovered (mapcan (lambda (x) (if (valid (repair x)) (list (repair x)))) flatten-crossovered))
       (mutated (mapcar (lambda (x) (if (> (random 100) *mutate-probability*) (mutate (if (listp x) x (list x))))) valid-crossovered))
       (valid-mutated (mapcan (lambda (x) (if (valid (repair x)) (list (repair x)))) mutated))
       (sorted-ofspring-pop (sort-pop valid-mutated data function))
       (new-pop (reverse (merge-to-new-pop sorted-pop sorted-ofspring-pop)))
       (best (car new-pop))
       )
  ;(values sorted-pop sorted-ofspring-pop)  );(print-pop cross-overed)))
(format t "~d. generation - f: ~S - best function ~s with error: ~f~%" gen-count (nth function-num *input-fns*) (car best) (second best))
(write-results nil (car new-pop) (nth function-num *input-fns*) gen-count)
 (run-it function-num (mapcar #'car new-pop)  function data (+ gen-count 1))
)))

(defun gen-sequence (length)
(let (
  (seq (make-sequence 'list length :initial-element 1))
)
(reverse (reduce (lambda (acc x) (cons (+ (first acc) x) acc)) seq :initial-value (list 0)
  ))))

;(format t "~S~%" (gen-sequence (length *functions*)))
;(format t "~S~%" (choose-random-data (gen-sequence 10) (length *functions*)))
(mapcar (lambda (x) (run-it x)) (gen-sequence (length *functions*)))
;(run-it)

	