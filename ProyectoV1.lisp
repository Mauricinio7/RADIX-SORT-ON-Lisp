(defvar cola '())
(defvar lista-colas (list (list) (list) (list) (list) (list) (list) (list) (list) (list) (list) (list) (list) (list) (list) (list) (list) (list) (list) (list) (list)))

(push 21 cola)
(push 35 cola)
(push 42 cola)
(push 1 cola)
(push 2 cola)
(push 14 cola)
(push 143 cola)
(push 21 cola)



(defun RADIX-Sort (multiplicador)
    (if (< multiplicador 100000)
        (progn
            (mapcar #'(lambda (x) (llenar-lista-colar x multiplicador)) cola)
            (mapcar #'llenar-colas lista-colas)
            (setq lista-colas (list (list) (list) (list) (list) (list) (list) (list) (list) (list) (list) (list) (list) (list) (list) (list) (list) (list) (list) (list) (list)))
            (radix-sort (* multiplicador 10))
        )
        (print (reverse cola))
    )
)

(defun llenar-lista-colar (numero multiplicador)    
    (push (car cola)
            (nth (funcall (lambda (x) (+ (mod (truncate (/ x multiplicador)) 10) 0)) (car cola)) lista-colas)
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




;Solución: Crear una cola con 20 colar, llenar esas 20 colas y reordenar
;Esta casi listo 