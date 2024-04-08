(defun RADIX-Sort (cola multiplicador)
  (if (< multiplicador 100)
      (progn
        (mapcar #'(lambda (x) (llenar-lista-colar x multiplicador cola)) cola)
        (mapcar #'llenar-colas lista-colas)
        (print "vuelta")
        (RADIX-Sort cola (* 10 multiplicador))
      )
      (print "Fin")
  )
)

(defun llenar-lista-colar (numero multiplicador cola)
  (push numero
        (nth (funcall (lambda (x) (+ (mod (/ x (truncate multiplicador)) 10) 0)) numero) lista-colas)
  )
  (pop cola)
)


(defun llenar-colas (lista)
  (if (null lista)
      (pop lista-colas)
      (progn 
        (push (car lista) cola)
        (llenar-colas (cdr lista))
      )
  )
)

(defvar cola '())
(defvar lista-colas (list (list) (list) (list) (list) (list) (list) (list) (list) (list) (list) (list) (list) (list) (list) (list) (list) (list) (list) (list) (list)))

(push 2 cola)
(push 3 cola)
(push 4 cola)
(push 1 cola)

(print cola)
(RADIX-Sort cola 1)
(print (reverse cola))
