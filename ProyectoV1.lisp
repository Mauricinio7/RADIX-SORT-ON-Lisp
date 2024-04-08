(defvar cola '())
(defvar lista-colas (list (list) (list) (list) (list) (list) (list) (list) (list) (list) (list) (list) (list) (list) (list) (list) (list) (list) (list) (list) (list)))

(push 2 cola)
(push 3 cola)
(push 4 cola)
(push 1 cola)



(defun RADIX-Sort (multiplicador)
    (if (null cola)
        (print "doing");llama a la función para reordenar la cola
        (push (car cola)
            (nth (funcall (lambda (x) (+ (mod (/ x multiplicador) 10) 0)) (car cola)) lista-colas)
        )
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
        (return-from llenar-colas 0)
        (progn 
            (push (car lista) cola)
            (llenar-colas (cdr lista))
        )
    )
)

(print cola)
(mapcar #'(lambda (x) (llenar-lista-colar x 1)) cola)
(mapcar #'llenar-colas lista-colas)
(print lista-colas)
(print cola)
(print (reverse cola))




;Solución: Crear una cola con 20 colar, llenar esas 20 colas y reordenar