;//////////////////////////////////////////////////////////////////////////
(defun inicializar-juego2()
  (setq
    *Tablero* '(nil nil nil nil nil
                nil nil nil nil nil
                nil nil nil nil nil
                nil nil nil nil nil
                nil nil nil nil nil)
    *lineaH* '---
    *lineaV* 'i
    *Turno* 1
    *hayGanador* NIL
    *FichaH* 'A
    *FichaO* 'B
    *NumeroFichas* 0
    *MaxNumFichas* 6
    *FichaVacia* NIL
    *Humano* 1
    *Ordenador* -1
                *LineasDisp* '(1 2 3 4 5 6 7 8 9 10 11 12)
                *NroHumano* 0
                *NroOrdenador* 0
                *Cuad* '((1 5 7 11) (3 7 9 13) (11 15 17 21) (13 17 19 23))
    *Metas* '((0 1 2) (3 4 5) (6 7 8) (0 3 6) (1 4 7) (2 5 8) (0 4 8) (2 4 6)) )
)

;funcion para editar elementos de lista cualquiera
(defun editar-elemento (posicion elemento lista)
  (setq listaux '())  ;lista auxiliar
  (setq cnt 0)        ;inicializamos el contador
  (if (< posicion (length lista))
      (progn
         (print "caso correcto")
         ;aqui vamos a empezar a recorrer la lista
         (dolist (x lista)
            (print x)
            (print cnt)
            (if (or (< cnt posicion) (> cnt posicion))
                (progn
                  (setq listaux (append listaux (list x)))
                  (format t "Lista=~s ~&" listaux)
                )
            )
            (if (= cnt posicion) 
                (progn
                    (setq listaux (append listaux (list elemento)))
                    (format t "Lista=~s ~&" listaux)
                 )
            )
            (setq cnt (+ cnt 1))
         )
         (setq lista listaux)
      )
  ;si no cumple
      (print "malo")
  )
)

;modificar elemento en el tablero
(defun editar-tablero (posicion elemento)
  (setq tab *tablero*)
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
         (setq *tablero* listaux)
      )
  ;si no cumple
      (print "malo")
  )
)

;funcion para probar si un elemento pertenece a una lista
(defun probar (elemento lista)
  (if (equal nil (member elemento lista))
      (setq valor nil)   ;no esta
      (setq valor t)     ;si esta
  )
)

;funcion de tipo de ficha
(defun tipo-ficha2(Ficha pos)
  (cond
    ( (probar pos '(1 3 11 13 21 23)) 
             (cond
                ((equal Ficha *FichaVacia*) (numero-linea pos))
                ((equal Ficha *Humano*) *lineah*)
                ((equal Ficha *Ordenador*) *lineah*)
             )
        ); si es horizontal
        ( (probar pos '(5 7 9 15 17 19)) 
             (cond
                ((equal Ficha *FichaVacia*) (numero-linea pos))
                ((equal Ficha *Humano*) *lineav*)
                ((equal Ficha *Ordenador*) *lineav*)
             )
        ); si es vertical
        ( (probar pos '(6 8 16 18)) 
             (cond
                ((equal Ficha *FichaVacia*) '-)
                ((equal Ficha *Humano*) *FichaH*)
                ((equal Ficha *Ordenador*) *FichaO*)
             )
        ); si es cuadrado

        (T 'fin)
  )
)


;funcion para mostrar el tablero
(defun mostrar-tablero2()
  (let
      ((tablero *tablero*) (lineah *lineaH*) (lineav *lineaV*) (turno *turno*))
      (format t "Turno : ~s Lista Tablero: ~s" turno tablero)
    (format t "~%| ~s~s~s~s~s | ~&| ~s ~a ~s ~a ~s | ~&| ~s~s~s~s~s | ~&| ~s ~a ~s ~a ~s | ~&| ~s~s~s~s~s |~%"
      'O       ;posicion 0
      (tipo-ficha2 (nth 1 tablero) 1)   ;posicion 1
      'O       ;posicion 2
      (tipo-ficha2 (nth 3 tablero) 3)   ;posicion 3
      'O       ;posicion 4
      (tipo-ficha2 (nth 5 tablero) 5)   ;posicion 5
      (tipo-ficha2 (nth 6 tablero) 6);posicion 6
      (tipo-ficha2 (nth 7 tablero) 7)   ;posicion 7
      (tipo-ficha2 (nth 8 tablero) 8);posicion 8
      (tipo-ficha2 (nth 9 tablero) 9)   ;posicion 9
      'O       ;posicion 10
      (tipo-ficha2 (nth 11 tablero) 11)   ;posicion 11
      'O       ;posicion 12
      (tipo-ficha2 (nth 13 tablero) 13)   ;posicion 13
      'O       ;posicion 14
      (tipo-ficha2 (nth 15 tablero) 15)   ;posicion 15
      (tipo-ficha2 (nth 16 tablero) 16)   ;posicion 16
      (tipo-ficha2 (nth 17 tablero) 17)   ;posicion 17
      (tipo-ficha2 (nth 18 tablero) 18)   ;posicion 18
      (tipo-ficha2 (nth 19 tablero) 19)   ;posicion 19
      'O       ;posicion 20
      (tipo-ficha2 (nth 21 tablero) 21)   ;posicion 21
      'O       ;posicion 22
      (tipo-ficha2 (nth 23 tablero) 23)   ;posicion 23
      'O       ;posicion 24
       
       )

  

  ) 
)

;asigna los numeros de línea vacios
(defun numero-linea (pos)
  (cond 
      ((equal 1 pos) '-1-)
      ((equal 3 pos) '-2-)
      ((equal 5 pos) '3)
      ((equal 7 pos) '4)
      ((equal 9 pos) '5)
      ((equal 11 pos) '-6-)
      ((equal 13 pos) '-7-)
      ((equal 15 pos) '8)
      ((equal 17 pos) '9)
      ((equal 19 pos) '10)
      ((equal 21 pos) '11-)
      ((equal 23 pos) '12-)
   )
)

;funcion para quitar de disponibles
(defun eliminar-disp (linea)
  (setq disponible *LineasDisp*)
  (setq listaux '())  ;lista auxiliar
  (setq cnt 0)        ;inicializamos el contador
  (dolist (x disponible)
  (if (not(equal x linea))
            (progn
      (setq listaux (append listaux (list x)))
      ;(format t "Lista=~s ~&" listaux)
      )
  )
  setq cnt (+ cnt 1))
  (setq *LineasDisp* listaux)
)

;funcion que devuelve la posicion real de la que ingresa el jugador
(defun dar-pos-real (lin)
  (cond 
    ((equal 1 lin) 1)
    ((equal 2 lin) 3)
    ((equal 3 lin) 5)
    ((equal 4 lin) 7)
    ((equal 5 lin) 9)
    ((equal 6 lin) 11)
    ((equal 7 lin) 13)
    ((equal 8 lin) 15)
    ((equal 9 lin) 17)
    ((equal 10 lin) 19)
    ((equal 11 lin) 21)
    ((equal 12 lin) 23)
  )
)

;funcion para identificar el cuadrado a pintar
(defun llenarCuad (numCuad)
  (cond ((equal numCuad 0) 6)
        ((equal numCuad 1) 8)
        ((equal numCuad 2) 16)
        ((equal numCuad 3) 18)
  )
)

;función para pintar cuadradito
(defun pintar (pos tur)
  (editar-tablero pos tur)
  
)

;funcion para verificar si se llenó algún cuadrado
(defun verificarCuad (linea turno)
  (setq bandera nil)
  (dotimes (i 4)
     (setq cnt 0)
     (setq cuadradito (nth i *Cuad*))
     (if (probar linea cuadradito)
         (progn
              (dolist (x cuadradito)
                  (if (not(equal (nth x *Tablero*) nil))
                      (setq cnt (+ 1 cnt))
                  )
               )
               (if (= cnt 4)
                   (progn
                       (pintar (llenarCuad i) turno)
                       (setq bandera t)
                       (if (= turno 1)
                         (setq *NroHumano* (+ 1 *NroHumano*))
                         (setq *NroOrdenador* (+ 1 *NroOrdenador*))
                       )
                   )
               )
         )     
     )
  )
  (if (equal bandera nil) (setq *turno* (* -1 *turno*))) ;si no cerramos un cuadrado cambiamos de turno
)

;funcion para jugar
(defun jugar-humano (linea) 

(setq disp *LineasDisp*)  

  (if (probar linea disp)
      (progn 
           (setq posreal (dar-pos-real linea))
           (editar-tablero posreal *humano*)
           (eliminar-disp linea)
           (verificarCuad posreal *turno*)
      )


  )
     (print "JUEGA HUMANO") 
)


(defun jugar-ordenador ()

  (if (> (length *LineasDisp*) 0)

    (progn
        (setq lineai (random (length *LineasDisp*))) 
        (setq linea (nth lineai *LineasDisp*))  

   

       (setq posreal (dar-pos-real linea))
       (editar-tablero posreal *Ordenador*)
       (eliminar-disp linea)
       (verificarCuad posreal *turno*)

       (if (= *turno* -1)
        (jugar-ordenador)
        )

       (print "JUEGA ORDENADOR") 

     )
  )
   

)


;funcion principal que inicia el juego


