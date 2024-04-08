(defvar cola '())
(defvar lista-colas (list (list) (list) (list) (list) (list) (list) (list) (list) (list) (list) (list) (list) (list) (list) (list) (list) (list) (list) (list) (list)))

(push 2 cola)
(push 3 cola)
(push 4 cola)
(push 1 cola)



(defun RADIX-Sort (multiplicador)
    (if (< multiplicador 100)
        (progn
            (mapcar #'(lambda (x) (llenar-lista-colar x multiplicador)) cola)
            (mapcar #'llenar-colas lista-colas)
            (print "vuelta")
            (radix-sort (* 10 multiplicador))
        )
        (print "Fin")
    )
)

(defun llenar-lista-colar (numero multiplicador)
    (push (car cola)
            (nth (funcall (lambda (x) (+ (mod (/ x multiplicador) 10) 0)) (car cola)) lista-colas)
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

(print cola)
(radix-sort 1)
(print (reverse cola))



;Solución: Crear una cola con 20 colar, llenar esas 20 colas y reordenar