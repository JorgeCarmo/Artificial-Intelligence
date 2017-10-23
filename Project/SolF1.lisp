;Grupo 72: Ricardo Tavares n78198, Jorge Carmo n79702
;;; These functions, and any other ones needed must be implemented

;;; Utilizar estes includes para os testes na versao local
;;; comentar antes de submeter
(load "datastructures.lisp")
(load "auxfuncs.lisp")

;;; Utilizar estes includes para a versao a submeter
; tirar o comentario antes de submeter
;(load "datastructures.fas")
;(load "auxfuncs.fas")

(defun isObstaclep (pos track) 
  "check if there is an obstacle at position pos of the track"
  (let ((posx (car pos)) (posy (nth 1 pos)))
  (eq (nth posy (nth posx (track-env track))) NIL)))

(defun isGoalp (st) 
  "check if st is a goal state"
  (if (position (state-pos st) (track-endpositions (state-track st)) :test #'equal) t NIL))
  
(defun nextState (st act)
  "generate the nextState after state st and action act"
  (let* ((newspeed (mapcar '+ act (state-vel st))) (newpos (mapcar '+ newspeed (state-pos st))) (newcost 1) (ctrack (state-track st)) (newstate ))
  (if (isObstaclep newpos ctrack) (setf newspeed '(0 0) newpos (state-pos st) newcost 20))
  (setf newstate (make-STATE :POS newpos	:VEL newspeed	:ACTION act	:COST newcost :TRACK ctrack))
  (if (isGoalp newstate) (setf (state-cost newstate) -100)) newstate))