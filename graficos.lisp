(load "ltk")
(in-package :ltk)

;---------------------------------------------------
	; CARGAMOS EL ARCHIVO INTERFAZ.
(load "interfaz.lisp")



;---------------------------------------------------

(inicializar-juego2)

;---------------------------------------------------


(defvar *Posiciones*)
(defvar *colorFicha*)
(defvar *colorHumano*)
(defvar *colorOrdenador*)
(defvar *jugada*)
(defvar *turno*)

(setq 
  *colorLinea* '"#ccd2c8"
  *colorFicha* '"#394034"
  *colorHumano* '"#ff2f70"
  *colorOrdenador* '"#0083e8" )


;---------------------------------------------------
;EVALUACIONES PARA CADA LINEA
(defun val-linea (posx posy)
   (and (and (>= posx 80) (<= posx 240)) (and (>= posy 56) (<= posy 64) )  )

  )

(defun val-linea2 (posx posy)
   (and (and (>= posx 280) (<= posx 440)) (and (>= posy 56) (<= posy 64) )  )

  )
(defun val-linea3 (posx posy)
    (and (and (>= posx 56) (<= posx 64)) (and (>= posy 80) (<= posy 240) )  )
  )
(defun val-linea4 (posx posy)
     (and (and (>= posx 256) (<= posx 264)) (and (>= posy 80) (<= posy 240) )  )
  )
(defun val-linea5 (posx posy)
     (and (and (>= posx 456) (<= posx 464)) (and (>= posy 80) (<= posy 240) )  )
  )


(defun val-linea6 (posx posy)
     (and (and (>= posx 80) (<= posx 240)) (and (>= posy 256) (<= posy 264) )  )
  )
(defun val-linea7 (posx posy)
    (and (and (>= posx 280) (<= posx 440)) (and (>= posy 256) (<= posy 264) )  )
  )
(defun val-linea8 (posx posy)
    (and (and (>= posx 56) (<= posx 64)) (and (>= posy 280) (<= posy 440) )  )
  )
(defun val-linea9 (posx posy)
   (and (and (>= posx 256) (<= posx 264)) (and (>= posy 280) (<= posy 440) )  )
  )
(defun val-linea10 (posx posy)
    (and (and (>= posx 456) (<= posx 464)) (and (>= posy 280) (<= posy 440) )  )
  )


(defun val-linea11 (posx posy)
   (and (and (>= posx 80) (<= posx 240)) (and (>= posy 456) (<= posy 464) )  )
  )
(defun val-linea12 (posx posy)
   (and (and (>= posx 280) (<= posx 440)) (and (>= posy 456) (<= posy 464) )  )
  )
;---------------------------------------------------


(defun	posicion-linea (evento)

	(let ((indice -1) )
     
       (if (val-linea (event-x evento) (event-y evento) )  
             (setq indice 1)

             (if (val-linea2 (event-x evento) (event-y evento) )  
                  (setq indice 2)

                  (if (val-linea3 (event-x evento) (event-y evento) )  
                	  (setq indice 3)
                  
	                  (if (val-linea4 (event-x evento) (event-y evento) )  
	                  	  (setq indice 4)
							
						  (if (val-linea5 (event-x evento) (event-y evento) )  
			                  (setq indice 5)
			                  
			                  (if (val-linea6 (event-x evento) (event-y evento) )  
				                  (setq indice 6)
				                  
				                  (if (val-linea7 (event-x evento) (event-y evento) )  
					                  (setq indice 7)

					                  (if (val-linea8 (event-x evento) (event-y evento) )  
					                 	  (setq indice 8)               
					                  	  
					                  	  (if (val-linea9 (event-x evento) (event-y evento) )  
					                 	  	  (setq indice 9)               
					                  			
					                  		  (if (val-linea10 (event-x evento) (event-y evento) )  
					                 	  	  	  (setq indice 10)               
					                  			  
					                  			  (if (val-linea11 (event-x evento) (event-y evento) )  
					                 	  	  	 	  (setq indice 11)               
					                  				   
					                  				  (if (val-linea12 (event-x evento) (event-y evento) )  
					                 	  	  	 	       (setq indice 12)               
					                  
					       	                          )
					       	                      )	
					       	                  )	
					       	              )
					       	          )						                  
					              )	
			                  )	
			              )		                  
	                  )
                  )
                  
             )           

        )

     indice
     ) 
)

(defun tablero()
	(with-ltk ()
		(let* (
					;(img (make-image))
					(sc (make-instance 'canvas :height 520 :width 800))
					(lienzo (canvas sc))
					; Caminos por el tablero
					
					;(linea2 (create-line lienzo (list 60 360 360 360)))

                    (ovalo1 (create-oval lienzo 50 50 70 70))
					(ovalo2 (create-oval lienzo 250 50 270 70))
					(ovalo3 (create-oval lienzo 450 50 470 70))

					(linea1 (create-line lienzo (list 80 60 240 60) ))
					(linea2 (create-line lienzo (list 280 60 440 60) ))

					(linea3 (create-line lienzo (list 60 80 60 240) ))
					(linea4 (create-line lienzo (list 260 80 260 240) ))
					(linea5 (create-line lienzo (list 460 80 460 240) ))

 					(ovalo4 (create-oval lienzo 50 250 70 270))
					(ovalo5 (create-oval lienzo 250 250 270 270))
					(ovalo6 (create-oval lienzo 450 250 470 270))

					(linea6 (create-line lienzo (list 80 260 240 260) ))
					(linea7 (create-line lienzo (list 280 260 440 260) ))

					(linea8 (create-line lienzo (list 60 280 60 440) ))
					(linea9 (create-line lienzo (list 260 280 260 440) ))
					(linea10 (create-line lienzo (list 460 280 460 440) ))

					(ovalo7 (create-oval lienzo 50 450 70 470))
					(ovalo8 (create-oval lienzo 250 450 270 470))
					(ovalo9 (create-oval lienzo 450 450 470 470))

					(linea11 (create-line lienzo (list 80 460 240 460) ))
					(linea12 (create-line lienzo (list 280 460 440 460) ))
		
			 )

		(pack sc :expand 1 :fill :both)
		;(image-load img "iron-man.png")
		;(create-image sc 480 0 :image img)
		(wm-title *tk* "Cuadraditos 3X3")

		(bind lienzo "<ButtonPress-1>"
			(lambda (evento)

				(setq indiceLinea (posicion-linea evento) )
				(print indiceLinea)
				(format t "~&~s    ~s    " (event-x evento) (event-y evento)  )
				
				(cond
<<<<<<< HEAD
				( (= indiceLinea 0) (itemconfigure lienzo linea1 :fill *colorHumano*) )
				( (= indiceLinea 1) (itemconfigure lienzo linea2 :fill *colorHumano*) )
				( (= indiceLinea 2) (itemconfigure lienzo linea3 :fill *colorHumano*) )
				( (= indiceLinea 3) (itemconfigure lienzo linea4 :fill *colorHumano*) )
				( (= indiceLinea 4) (itemconfigure lienzo linea5 :fill *colorHumano*) )
				( (= indiceLinea 5) (itemconfigure lienzo linea6 :fill *colorHumano*) )
				( (= indiceLinea 6) (itemconfigure lienzo linea7 :fill *colorHumano*) )
				( (= indiceLinea 7) (itemconfigure lienzo linea8 :fill *colorHumano*) )
				( (= indiceLinea 8) (itemconfigure lienzo linea9 :fill *colorHumano*) ) 
				( (= indiceLinea 9) (itemconfigure lienzo linea10 :fill *colorHumano*) ) 
				( (= indiceLinea 10) (itemconfigure lienzo linea11 :fill *colorHumano*) ) 
				( (= indiceLinea 11) (itemconfigure lienzo linea12 :fill *colorHumano*) ) 
				((equal indiceLinea -1)  (print 'lugar-incorrecto) )
=======
				( (= indiceLinea 1) (itemconfigure lienzo linea1 :fill *colorHumano*) )
				( (= indiceLinea 2) (itemconfigure lienzo linea2 :fill *colorHumano*) )
				( (= indiceLinea 3) (itemconfigure lienzo linea3 :fill *colorHumano*) )
				( (= indiceLinea 4) (itemconfigure lienzo linea4 :fill *colorHumano*) )
				( (= indiceLinea 5) (itemconfigure lienzo linea5 :fill *colorHumano*) )
				( (= indiceLinea 6) (itemconfigure lienzo linea6 :fill *colorHumano*) )
				( (= indiceLinea 7) (itemconfigure lienzo linea7 :fill *colorHumano*) )
				( (= indiceLinea 8) (itemconfigure lienzo linea8 :fill *colorHumano*) )
				( (= indiceLinea 9) (itemconfigure lienzo linea9 :fill *colorHumano*) ) 
				( (= indiceLinea 10) (itemconfigure lienzo linea10 :fill *colorHumano*) ) 
				( (= indiceLinea 11) (itemconfigure lienzo linea11 :fill *colorHumano*) ) 
				( (= indiceLinea 12) (itemconfigure lienzo linea12 :fill *colorHumano*) ) 
				((equal indiceLinea -1)  (print 'lugar-incorrecto) ) 
>>>>>>> origin/master
				)



				

			)

		)
				    

		; Grosor de los caminos
		(itemconfigure lienzo linea1 :width 8)
		(itemconfigure lienzo linea1 :fill *colorLinea*)
	    (itemconfigure lienzo linea2 :width 8)
		(itemconfigure lienzo linea2 :fill *colorLinea*)
         
        (itemconfigure lienzo linea3 :width 8)
		(itemconfigure lienzo linea3 :fill *colorLinea*)
	    (itemconfigure lienzo linea4 :width 8)
		(itemconfigure lienzo linea4 :fill *colorLinea*)
		(itemconfigure lienzo linea5 :width 8)
		(itemconfigure lienzo linea5 :fill *colorLinea*)

         
		(itemconfigure lienzo ovalo1 :fill *colorFicha*)
		(itemconfigure lienzo ovalo1 :width 3)
		(itemconfigure lienzo ovalo2 :fill *colorFicha*)
		(itemconfigure lienzo ovalo2 :width 3)
		(itemconfigure lienzo ovalo3 :fill *colorFicha*)
		(itemconfigure lienzo ovalo3 :width 3)

		(itemconfigure lienzo ovalo4 :fill *colorFicha*)
		(itemconfigure lienzo ovalo4 :width 3)
		(itemconfigure lienzo ovalo5 :fill *colorFicha*)
		(itemconfigure lienzo ovalo5 :width 3)
		(itemconfigure lienzo ovalo6 :fill *colorFicha*)
		(itemconfigure lienzo ovalo6 :width 3)


		(itemconfigure lienzo linea6 :width 8)
		(itemconfigure lienzo linea6 :fill *colorLinea*)
	    (itemconfigure lienzo linea7 :width 8)
		(itemconfigure lienzo linea7 :fill *colorLinea*)
         
        (itemconfigure lienzo linea8 :width 8)
		(itemconfigure lienzo linea8 :fill *colorLinea*)
	    (itemconfigure lienzo linea9 :width 8)
		(itemconfigure lienzo linea9 :fill *colorLinea*)
		(itemconfigure lienzo linea10 :width 8)
		(itemconfigure lienzo linea10 :fill *colorLinea*)





		(itemconfigure lienzo ovalo7 :fill *colorFicha*)
		(itemconfigure lienzo ovalo7 :width 3)
		(itemconfigure lienzo ovalo8 :fill *colorFicha*)
		(itemconfigure lienzo ovalo8 :width 3)
		(itemconfigure lienzo ovalo9 :fill *colorFicha*)
		(itemconfigure lienzo ovalo9 :width 3)

		(itemconfigure lienzo linea11 :width 8)
		(itemconfigure lienzo linea11 :fill *colorLinea*)
	    (itemconfigure lienzo linea12 :width 8)
		(itemconfigure lienzo linea12 :fill *colorLinea*)

         )
      )

)

(tablero)

