;configuracion de cada nodo
; nodo= (estado turno regla nivel)
;f_eval=(0.5*(nro. de cuad. con 2 lineas)+0.8*(nro. de cuad. mios))-(0.2*(nro. de cuad. con 3 lineas)+0.2*(nro. de cuad. de adversario))

;(setq *Tablero* '(  nil -1  nil  1  nil
;	                nil nil  1  nil nil
;	                nil nil nil  1  nil
;	                nil nil -1  nil nil
;	                nil  1  nil -1  nil)
;
;      *Cuad* '((1 5 7 11) (3 7 9 13) (11 15 17 21) (13 17 19 23))
;
;      *Turno* 1
;
;      *NroHumano* 0
;      *NroOrdenador* 0
;)

(defun algoritmo_minimax (tablero)
  (setq *Cuad* '((1 5 7 11) (3 7 9 13) (11 15 17 21) (13 17 19 23))
        ;*Turno* 1
        ;*NroHumano* 0
        ;*NroOrdenador* 0)
  )   
  (print tablero)
  (setq nodoi (list tablero 1 0 0))
  (setq resultado (val_minimax nodoi))
  (format t "~%La linea a trazar debe ser:~s  valor:~s nivel:~s" (nth 0 resultado) (nth 1 resultado) (nth 2 resultado))
  (nth 0 resultado)
)

(defun val_minimax (nodo)
	(let ((maximo -50) (minimo 50) (valor '()) (aux '()))
		;(format t "~%Nodo: ~s Turno:~s Regla:~s Nivel:~s" (nth 0 nodo) (nth 1 nodo) (nth 2 nodo) (nth 3 nodo))

		(if (nodo_terminal nodo)
			(f_eval nodo)
			;else
			(progn
				;(print "No terminal")		
				(if (>= (nth 3 nodo) 3)   ;3 es el maximo nivel de profundidad del arbol
					(f_eval nodo)
					;else
					(progn
						(cond 
							((= (nth 1 nodo) 1) 
								(dolist (x (sucesores nodo))
									(setq aux (val_minimax x))
									(if (>= (nth 1 aux) maximo)
										(progn 
											(setq valor (list (nth 2 x) (nth 1 aux) (nth 3 aux)))
											;(print valor)
											(setq maximo (nth 1 aux))
										)
									)
								)
							)
							;para el min
							((= (nth 1 nodo) -1) 
								(dolist (x (sucesores nodo))
									(setq aux (val_minimax x))
									(if (< (nth 1 aux) minimo)
										(progn 
											(setq valor (list (nth 2 x) (nth 1 aux) (nth 3 aux) ))
											(setq minimo (nth 1 aux))
												;(print valor)
										)
									)
								)
							)						
						)
						valor
					)
				)
			)
		)
	)
)

(defun f_eval (nodo)
	(list (nth 2 nodo) (feval (nth 0 nodo) (nth 1 nodo)))
)

(defun nodo_terminal (nodo)
	(setq vacias 0)
	(setq indices '(1 3 5 7 9 11 13 15 17 19 21 23))
	(dolist (x indices)
		(if (equal (nth x (nth 0 nodo)) nil) (setq vacias (+ 1 vacias)))
		                  ;(tablero)
	)
	(if (= vacias 0) t nil)
)

(defun f_utilidad (nodo)
	(cond ((= (nth 0 nodo) 0) (list (nth 2 nodo) 0))
		((= (nth 0 nodo) 1) (list (nth 2 nodo) 10))
		((= (nth 0 nodo) 2) (list (nth 2 nodo) 20))
	)
)

(defun posicionOrig-arbol (linea)
		 (- (* (+ linea 1) 2) 1 ) 
)



(defun sucesores (nodo)
	(setq listaSucesor '())
	(setq turnoHijo (nth 1 nodo))
    (setq tablero (nth 0 nodo))
    (setq turnoAux 0)
    (setq Aux '())

	( dotimes (contador 12)
           
             (if (equal (nth (posicionOrig-arbol contador) tablero ) nil) 
               (progn

               	(setq tableroaux (editar-tablero-arbol (posicionOrig-arbol contador) (nth 1 nodo) tablero  ))
                (setq Aux (verificarCuad-arbol (posicionOrig-arbol contador) (nth 1 nodo) tableroaux))
               	(setq tableroaux (nth 0 Aux))
                (setq turnoAux (nth 1 Aux))
               	 

				(setq nodoaux  (list tableroaux turnoAux (+ contador 1) (+ (nth 3 nodo) 1) ) )
				(print nodoaux)
				(print (f_eval nodoaux))
				(setq listaSucesor (append listaSucesor (list nodoaux) ) )
               	)    
			

			 )
			 (setq turnoAux turnoHijo)
             	
    )
    listaSucesor
)


(defun pintar-arbol (pos tur tablero)
  (editar-tablero-arbol pos tur tablero)
  
)
;funcion para probar si un elemento pertenece a una lista
(defun probar-arbol (elemento lista)
  (if (equal nil (member elemento lista))
      (setq valor nil)   ;no esta
      (setq valor t)     ;si esta
  )
)
;funcion para identificar el cuadrado a pintar
(defun llenarCuad-arbol (numCuad)
  (cond ((equal numCuad 0) 6)
        ((equal numCuad 1) 8)
        ((equal numCuad 2) 16)
        ((equal numCuad 3) 18)
  )
)
;funcion para verificar si se lleno algun cuadrado
(defun verificarCuad-arbol (linea turno tablero)
	
        (setq tabPaint tablero)
	(setq tab tablero)
        (setq bandera nil)
        (setq turnof 1)
  (dotimes (i 4)
     (setq cnt 0)
     (setq cuadradito (nth i *Cuad*))
     (if (probar-arbol linea cuadradito)
         (progn
              (dolist (x cuadradito)
                  (if (not(equal (nth x tab) nil))
                      (setq cnt (+ 1 cnt))
                  )
               )
               (if (= cnt 4)
                   (progn
                       (setq tabPaint (pintar-arbol (llenarCuad-arbol i) turno tab))
                       (setq bandera t)
                       ;(if (= turno 1)
                         ;(setq *NroHumano* (+ 1 *NroHumano*))
                         ;(setq *NroOrdenador* (+ 1 *NroOrdenador*))
                       ;)
                   )
               )
         )     
     )
  )
  (if (equal bandera nil) (setq turnof (* -1 turno)) (setq turnof turno))

   (list tabPaint turnof)
   ;si no cerramos un cuadrado cambiamos de turno
)



(defun editar-tablero-arbol (posicion elemento tablero)
  (setq tab tablero)
  (setq listaux '())  ;lista auxiliar
  (setq cnt 0)        ;inicializamos el contador
  (if (< posicion (length tab))
      (progn
         ;(print "caso correcto")
         ;aqui vamos a empezar a recorrer la lista
         (dolist (x tab)
            ;(print x)
            ;(print cnt)
            (if (or (< cnt posicion) (> cnt posicion))
                (progn
                  (setq listaux (append listaux (list x)))
                  ;(format t "Lista=~s ~&" listaux)
                )
            )
            (if (= cnt posicion) 
                (progn
                    (setq listaux (append listaux (list elemento)))
                    ;(format t "Lista=~s ~&" listaux)
                 )
            )
            (setq cnt (+ cnt 1))
         )
         listaux
      )
  ;si no cumple
      (print "malo")
  )
)

; f_eval=(0.5*(nro. de cuad. con 2 lineas)+0.8*(nro. de cuad. mios))-(0.2*(nro. de cuad. con 3 lineas)+0.2*(nro. de cuad. de adversario))

(defun feval (tablero turno)	
	(setq num_cuad_2 0) ; numero de cuadrados con 2 lineas
	(setq num_cuad_3 0) ; numero de cuadrados con 3 lineas
	(setq num_lineas 0) ; variable que va ir contando el numero de lineas de cada cuadrado
	(setq fun_eval 0) ; valor de la funcion de evaluacion	
	(dotimes (i (length tablero))

		(if (equal i 6)
			(progn
				(if (not (equal nil (nth 1 tablero)))
					(setq num_lineas (+ num_lineas 1))
				)
				(if (not (equal nil (nth 5 tablero)))
					(setq num_lineas (+ num_lineas 1))
				)
				(if (not (equal nil (nth 7 tablero)))
					(setq num_lineas (+ num_lineas 1))
				)
				(if (not (equal nil (nth 11 tablero)))
					(setq num_lineas (+ num_lineas 1))
				)
			)
		)

		(if (= num_lineas 2)
			(setq num_cuad_2 (+ num_cuad_2 1))
			(if (= num_lineas 3)
				(setq num_cuad_3 (+ num_cuad_3 1))
			)
		)
		
		(setq num_lineas 0)
		
		(if (equal i 8)
			(progn
				(if (not (equal nil (nth 3 tablero)))
					(setq num_lineas (+ num_lineas 1))
				)
				(if (not (equal nil (nth 7 tablero)))
					(setq num_lineas (+ num_lineas 1))
				)
				(if (not (equal nil (nth 9 tablero)))
					(setq num_lineas (+ num_lineas 1))
				)
				(if (not (equal nil (nth 13 tablero)))
					(setq num_lineas (+ num_lineas 1))
				)
			)
		)
		
		(if (= num_lineas 2)
			(setq num_cuad_2 (+ num_cuad_2 1))
			(if (= num_lineas 3)
				(setq num_cuad_3 (+ num_cuad_3 1))
			)
		)
		
		(setq num_lineas 0)
		
		(if (equal i 16)
			(progn
				(if (not (equal nil (nth 11 tablero)))
					(setq num_lineas (+ num_lineas 1))
				)
				(if (not (equal nil (nth 15 tablero)))
					(setq num_lineas (+ num_lineas 1))
				)
				(if (not (equal nil (nth 17 tablero)))
					(setq num_lineas (+ num_lineas 1))
				)
				(if (not (equal nil (nth 21 tablero)))
					(setq num_lineas (+ num_lineas 1))
				)
			)
		)
		
		(if (= num_lineas 2)
			(setq num_cuad_2 (+ num_cuad_2 1))
			(if (= num_lineas 3)
				(setq num_cuad_3 (+ num_cuad_3 1))
			)
		)
		
		(setq num_lineas 0)
		
		(if (equal i 18)
			(progn
				(if (not (equal nil (nth 13 tablero)))
					(setq num_lineas (+ num_lineas 1))
				)
				(if (not (equal nil (nth 17 tablero)))
					(setq num_lineas (+ num_lineas 1))
				)
				(if (not (equal nil (nth 19 tablero)))
					(setq num_lineas (+ num_lineas 1))
				)
				(if (not (equal nil (nth 23 tablero)))
					(setq num_lineas (+ num_lineas 1))
				)
			)
		)
		
		(if (= num_lineas 2)
			(setq num_cuad_2 (+ num_cuad_2 1))
			(if (= num_lineas 3)
				(setq num_cuad_3 (+ num_cuad_3 1))
			)
		)
		
		(setq num_lineas 0)
	)
	
	(setq cnt1 0) ; numero de cuadrados de 1
	(setq cnt_1 0) ; numero de cuadrados de -1
	
	(if (equal 1 (nth  6 tablero))
		(setq cnt1 (+ cnt1 1))
		(if (equal -1 (nth 6 tablero))
			(setq cnt_1 (+ cnt_1 1))
		)
	)
	(if (equal 1 (nth  8 tablero))
		(setq cnt1 (+ cnt1 1))
		(if (equal -1 (nth 8 tablero))
			(setq cnt_1 (+ cnt_1 1))
		)
	)
	(if (equal 1 (nth  16 tablero))
		(setq cnt1 (+ cnt1 1))
		(if (equal -1 (nth 16 tablero))
			(setq cnt_1 (+ cnt_1 1))
		)
	)
	(if (equal 1 (nth  18 tablero))
		(setq cnt1 (+ cnt1 1))
		(if (equal -1 (nth 18 tablero))
			(setq cnt_1 (+ cnt_1 1))
		)
	)
	
	; aplicando la formula tenemos
	;(format t "~%Numero de cuadrados de 1: ~S" cnt1);
	;(format t "~%Numero de cuadrados de -1: ~S" cnt_1);
	;(format t "~%Numero de cuadrados con 2 lineas ~S" num_cuad_2)
	;(format t "~%Numero de cuadrados con 3 lineas ~S" num_cuad_3)
	
	;f_eval=(0.5*(nro. de cuad. con 2 lineas)+0.8*(nro. de cuad. mios))-(0.2*(nro. de cuad. con 3 lineas)+0.2*(nro. de cuad. de adversario))
	
		(setq fun_eval (- (+ (* 0.5 num_cuad_2) (* 0.8 cnt1)) (+ (* 0.2 num_cuad_3) (* 0.2 cnt_1))))
		;(setq fun_eval (- (+ (* 0.5 num_cuad_2) (* 0.8 cnt_1)) (+ (* 0.2 num_cuad_3) (* 0.2 cnt1))))
	
	
	;(format t "~%Valor de la funcion de evaluacion: ~S" fun_eval)
	fun_eval
)
