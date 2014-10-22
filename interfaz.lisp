;//////////////////////////////////////////////////////////////////////////
;cargamos el archivo donde se encuentra el algoritmo minimax
(load "minimax.lisp")

;//////////////////////////////////////////////////////////////////////////
;Funcion que inicializa las variables globales que va a utilizar
; el juego cuadraditos (El timbiriche)
(defun inicializar-juego()
  (setq
    *Tablero* '(nil nil nil nil nil
                nil nil nil nil nil
                nil nil nil nil nil
                nil nil nil nil nil
                nil nil nil nil nil)   ; Lista Tablero que almacena la Matriz 5*5 del juego.
    *lineaHh* '-h-  ; Variable que pinta la linea horizontal del humano
    *lineaHo* '-o-  ; Variable que pinta la linea horizontal del ordenador
    *lineaVh* 'ih   ; Variable que pinta la linea vertical del ordenador
    *lineaVo* 'io   ; Variable que pinta la linea vertical del ordenador
    *Turno* -1      ; Variable que almacena el turno del juego
    *hayGanador* NIL ; Variable que verifica si hay ganador 
    *FichaH* 'A     ; Variable que pinta el cuadrado referente al humano
    *FichaO* 'B     ; Variable que pinta el cuadrado referente al ordenador
    *FichaVacia* NIL  ; Variable que pinta el cuadrado cuando este vacia
    *Humano* 1      ;Variable que identifica al humano como 1
    *Ordenador* -1  ;Variable que identifica al humano como -1
                *LineasDisp* '(1 2 3 4 5 6 7 8 9 10 11 12)   ;Lista que almacena las lineas posibles donde se pueden trazar
                *NroHumano* 0    ;Variable que almacena los cuadrados hechos por el Humano
                *NroOrdenador* 0   ;Variable que almacena los cuadrados hechos por el Ordenador
                *Cuad* '((1 5 7 11) (3 7 9 13) (11 15 17 21) (13 17 19 23))   ; Lista que almacena sublistas de las posiciones que forman un cuadrado en el tablero
   
	*Comienza* 0  ;Variable bandera que nos indica cuando el juego inicia
	)
	
)

;Agregamos en el tablero el elemento en la posicion querida.
(defun editar-tablero (posicion elemento)
  (setq tab *tablero*); Tablero auxiliar (tab) 
  (setq listaux '())  ;lista auxiliar
  (setq cnt 0)        ;inicializamos el contador
  (if (< posicion (length tab))
      (progn
         ;aqui vamos a empezar a recorrer la lista
         (dolist (x tab)
            (if (or (< cnt posicion) (> cnt posicion))
                (progn
                  (setq listaux (append listaux (list x)))  ;almacenamos los valores actuales del tablero si es que no se ha llegado a la posicion deseada para cambiar
                )
            )
            (if (= cnt posicion) 
                (progn
                    (setq listaux (append listaux (list elemento)))  ;Sobrescribimos en la posicion dada el nuevo elemento y luego lo almacenamos a Lista aux
                 )
            )
            (setq cnt (+ cnt 1)) ; Aumentados el contador para que recorra todas las posiciones del Tablero
         )
         (setq *tablero* listaux) ; Asignamos la Lista Auxiliar a la variable global *tablero*
      )
      ;si no cumple
      (print "Posicion no permitida")
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
(defun tipo-ficha(Ficha pos)
  (cond
    ( (probar pos '(1 3 11 13 21 23)) ; Si las posiciones pertenecen a una de las posiciones de las lineas horizontales
             (cond
                ((equal Ficha *FichaVacia*) (numero-linea pos))
                ((equal Ficha *Humano*) *lineahh*)
                ((equal Ficha *Ordenador*) *lineaho*)
             )
        )
        ( (probar pos '(5 7 9 15 17 19)) ; Si las posiciones pertenecen a una de las posiciones de las lineas Verticales
             (cond
                ((equal Ficha *FichaVacia*) (numero-linea pos))
                ((equal Ficha *Humano*) *lineavh*)
                ((equal Ficha *Ordenador*) *lineavo*)
             )
        )
        ( (probar pos '(6 8 16 18)) ; Si las posiciones pertenecen a una de las posiciones de los cuadrados encerrados [A] o [B]
             (cond
                ((equal Ficha *FichaVacia*) '-)
                ((equal Ficha *Humano*) *FichaH*)
                ((equal Ficha *Ordenador*) *FichaO*)
             )
        )

        (T 'fin)
  )
)


;funcion para mostrar el tablero
(defun mostrar-tablero()
  (let
      ((tablero *tablero*) )
      ;(format t "Turno : ~s Lista Tablero: ~s" turno tablero)
    (format t "~%| ~s~s~s~s~s | ~&| ~s ~a ~s ~a ~s | ~&| ~s~s~s~s~s | ~&| ~s ~a ~s ~a ~s | ~&| ~s~s~s~s~s |~%"
      'O       ;posicion 0
      (tipo-ficha (nth 1 tablero) 1)   ;posicion 1
      'O       ;posicion 2
      (tipo-ficha (nth 3 tablero) 3)   ;posicion 3
      'O       ;posicion 4
      (tipo-ficha (nth 5 tablero) 5)   ;posicion 5
      (tipo-ficha (nth 6 tablero) 6);posicion 6
      (tipo-ficha (nth 7 tablero) 7)   ;posicion 7
      (tipo-ficha (nth 8 tablero) 8);posicion 8
      (tipo-ficha (nth 9 tablero) 9)   ;posicion 9
      'O       ;posicion 10
      (tipo-ficha (nth 11 tablero) 11)   ;posicion 11
      'O       ;posicion 12
      (tipo-ficha (nth 13 tablero) 13)   ;posicion 13
      'O       ;posicion 14
      (tipo-ficha (nth 15 tablero) 15)   ;posicion 15
      (tipo-ficha (nth 16 tablero) 16)   ;posicion 16
      (tipo-ficha (nth 17 tablero) 17)   ;posicion 17
      (tipo-ficha (nth 18 tablero) 18)   ;posicion 18
      (tipo-ficha (nth 19 tablero) 19)   ;posicion 19
      'O       ;posicion 20
      (tipo-ficha (nth 21 tablero) 21)   ;posicion 21
      'O       ;posicion 22
      (tipo-ficha (nth 23 tablero) 23)   ;posicion 23
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

;funcion para quitar linea de la lista de disponibles
(defun eliminar-disp (linea)
  (setq disponible *LineasDisp*)
  (setq listaux '())  ;lista auxiliar
  (setq cnt 0)        ;inicializamos el contador
  (dolist (x disponible)
  (if (not(equal x linea))
            (progn
      (setq listaux (append listaux (list x)))  ;Almacena en la lista auxiliar todo los elementos diferentes de la linea a eliminar
     
      )
  )
  setq cnt (+ cnt 1))
  (setq *LineasDisp* listaux)  ;Actualizamos la Variable global de *LineasDisp*
)

;funcion que devuelve la posicion real, con respecto a la matriz 5x5, de la que ingresa el jugador
(defun dar-pos-real (linea)
  (cond 
    ((equal 1 linea) 1)
    ((equal 2 linea) 3)
    ((equal 3 linea) 5)
    ((equal 4 linea) 7)
    ((equal 5 linea) 9)
    ((equal 6 linea) 11)
    ((equal 7 linea) 13)
    ((equal 8 linea) 15)
    ((equal 9 linea) 17)
    ((equal 10 linea) 19)
    ((equal 11 linea) 21)
    ((equal 12 linea) 23)
  )
)

;funcion para identificar el cuadrado real a pintar
(defun llenarCuad (numCuad)
  (cond ((equal numCuad 0) 6)
        ((equal numCuad 1) 8)
        ((equal numCuad 2) 16)
        ((equal numCuad 3) 18)
  )
)

;función para pintar cuadraditos
(defun pintar (pos tur)
  (editar-tablero pos tur)
  
)

;funcion para verificar si se llenó algún cuadrado
(defun verificarCuad (linea turno)
  (setq bandera nil) ;Variable bandera que nos sirve para ver si Continua con el turno o no.
  (dotimes (i 4) ;Verifica por los 4 cuadrados del tablero
     (setq cnt 0)
     (setq cuadradito (nth i *Cuad*)) ;Almacenamos en cuadradito las posiciones de un cuadrado posible
     (if (probar linea cuadradito) ; Verificamos que si la linea a trazar pertenece a esa lista de cuadradito
         (progn
              (dolist (x cuadradito); recorremos la lista cuadradito
                  (if (not(equal (nth x *Tablero*) nil))
                      (setq cnt (+ 1 cnt)) ; Aumentamos el contador por cada vez que este trazada las posiciones de la lista cuadradito
                  )
               )
               (if (= cnt 4) ; Si el contador llega a 4, entonces tenemos que pintarlo
                   (progn
                       (pintar (llenarCuad i) turno)
                       (setq bandera t)
                       (if (= turno 1)
                         (setq *NroHumano* (+ 1 *NroHumano*))   ;Aumentamos el contador de cuadrados hechos por el humano
                         (setq *NroOrdenador* (+ 1 *NroOrdenador*)) ;Aumentamos el contador de cuadrados hechos por el ordenador
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

  (if (probar linea disp) ;Verificamos si la linea a trazar esta disponible
      (progn 
           (setq posreal (dar-pos-real linea)) ; identificamos la posicion real de la linea
           (editar-tablero posreal *humano*)  ; Editamos el elemento en el tablero de acuerdo a su posicion
           (eliminar-disp linea)  ; Eliminamos esa linea de la lista de Disponibles
           (verificarCuad posreal *turno*) ; Verificamos si con ese trazado se ha formado un cuadrado para el humano
      )
  )
     (print "JUEGA HUMANO") 
)


(defun jugar-ordenador ()

  (if (> (length *LineasDisp*) 0)

    (progn 
		;llamamos a algoritmo minimax, que nos devuelve linea a escoger
		(setq lineaC (algoritmo_minimax *Tablero*))

       (setq posreal (dar-pos-real lineaC)) ;identificamos la posicion real de acuerdo al tablero.
       (editar-tablero posreal *Ordenador*)  ; Editamos el elemento en el tablero de acuerdo a su posicion
       (eliminar-disp lineaC)   ; Eliminamos esa linea de la lista de Disponibles
       (verificarCuad posreal *turno*); Verificamos si con ese trazado se ha formado un cuadrado para el ordenador
       (if (= *turno* -1)
        (jugar-ordenador)
        )

       (print "JUEGA ORDENADOR") 

     )
  )
   
)