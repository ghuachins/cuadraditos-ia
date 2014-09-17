










(defun tipo-ficha(Ficha)
  (cond
  	( (equal Ficha *FichaH*) *FichaH* )
  	( (equal Ficha *FichaO*) *FichaO* )
  	( (equal Ficha *FichaVacia*) 'O ) ) )

(defun inicializar-juego()
	(setq
		*Tablero* '(nil nil nil nil nil
		            nil nil nil nil nil
		            nil nil nil nil nil
		            nil nil nil nil nil
		            nil nil nil nil nil)
		*lineaH* '---
		*lineaV* '!
		*Turno* 1
		*hayGanador* NIL
		*FichaH* 'X
		*FichaO* 'O
		*NumeroFichas* 0
		*MaxNumFichas* 6
		*FichaVacia* NIL
		*Humano* 1

		*Ordenador* -1
		*Metas* '((0 1 2) (3 4 5) (6 7 8) (0 3 6) (1 4 7) (2 5 8) (0 4 8) (2 4 6)) )
)

(defun mostrar-tablero()
  (let
      ((tablero *tablero*) (lineah *lineaH*) (lineav *lineaV*))
    (format t "~&| ~s~s~s~s~s | ~&| ~s ~s ~s ~s ~s | ~&| ~s~s~s~s~s | ~&| ~s ~s ~s ~s ~s | ~&| ~s~s~s~s~s |"
	    (tipo-ficha (nth 0 tablero))
	    lineah
	    (tipo-ficha (nth 2 tablero))
	    lineah
	    (tipo-ficha (nth 4 tablero))
	    lineav
	    (tipo-ficha (nth 6 tablero))
	    lineav
	    (tipo-ficha (nth 8 tablero))
	    lineav
	    (tipo-ficha (nth 10 tablero))
	    lineah
	    (tipo-ficha (nth 12 tablero))
	    lineah
	    (tipo-ficha (nth 14 tablero))
	    lineav
	    (tipo-ficha (nth 16 tablero))
	    lineav
	    (tipo-ficha (nth 18 tablero))
	    lineav
	    (tipo-ficha (nth 20 tablero))
	    lineah
	    (tipo-ficha (nth 22 tablero))
	    lineah
	    (tipo-ficha (nth 24 tablero))
	     ) ) )

