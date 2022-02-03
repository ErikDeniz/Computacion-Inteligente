(deftemplate muelle
	(multislot nombre)
	(slot espacio (type NUMBER))
)

(deftemplate buque
	(multislot nombre)
	(multislot eslora)
)

(deftemplate buqueatracado
	(multislot nombre)
	(multislot eslora)
	(multislot muelleS)
)

(deffacts puertolaspalmas
	(muelle
	(nombre "Reina Sofia")
	(espacio 700))
	
	(muelle
	(nombre "Nelson Mandela")
	(espacio 400))
	
	(muelle
	(nombre "Santa Catalina")
	(espacio 200))

	(interfaz)
)
;---------------------------------------------------------------------------------------
(defrule pregunta 
	(interfaz)
	=>
	(printout t "
	================================================================
	__-Bienvenido al Puerto de Las Palmas de G.C.-__

	¿Desea atracar, desatracar o abandonar el proceso? 

	Escriba ·entrada· ·salida· o ·esc·
	
	" crlf)

	(assert(orden(readline)))
)
;---------------------------------------------------------------------------------------
(defrule entrada 
	(orden "entrada")
	=>
	(printout t "
	De acuerdo para buscar atraque introduzca:
	Nombre de su buque:
	Eslora en metros:
	" crlf)
	(assert(buque
		(nombre(readline))
		(eslora (read))))
)
;---------------------------------------------------------------------------------------

(defrule elegiratraque
	(orden "entrada")
	(buque (nombre ?)(eslora ?l))
	=>
	(printout t "
	Elija en que muelle prefiere atracar.
	·Santa Catalina·   ·Nelson Mandela·    ·Reina Sofia·
	" crlf)
	(assert(eleccion(readline)))
)
;---------------------------------------------------------------------------------------	

(defrule calcularespacio
	(orden "entrada")
	(buque (nombre ?b)(eslora ?l))
	(eleccion ?muelle)	
	(muelle (nombre "Reina Sofia")(espacio ?espR))
	(muelle (nombre "Santa Catalina")(espacio ?espS))
	(muelle (nombre "Nelson Mandela")(espacio ?espN))
	?hecho<-(muelle(nombre ?muelle))
	?hechob<-(buque (nombre ?b)(eslora ?l))
	?interf<-(interfaz)
	?orden<-(orden ?)
	?eleccion<-(eleccion ?)
	=>
	(printout t "
	El muelle elegido es "?muelle"
	" crlf)	
	(if (eq ?muelle "Reina Sofia") then
		
		(if (< ?espR ?l) then
		(printout t "XXXXX No hay espacio disponible en el muelle Reina Sofia. XXXXX
		" crlf)
		)
		
		(if (> ?espR ?l) then
			(bind ?espR (- ?espR ?l))
			(modify ?hecho (espacio ?espR))
			(printout t "************Buque " ?b " atracado.************
			"crlf)
			(assert (buqueatracado
		 	 	(nombre ?b)
		 	 	(eslora ?l)
		 	 	(muelleS ?muelle)))
		)
	)
	(if (eq ?muelle "Santa Catalina") then
		
		(if (< ?espS ?l) then
		(printout t "XXXXX No hay espacio disponible en el muelle Santa Catalina. XXXXX
		" crlf)
		)
		(if (> ?espS ?l) then
			(bind ?espS (- ?espS ?l))
			(modify ?hecho (espacio ?espS))
			(printout t "************Buque " ?b " atracado.************
			" crlf)
			(assert (buqueatracado
		 	 	(nombre ?b)
		 	 	(eslora ?l)
		 	 	(muelleS ?muelle)))
		)
	)
	(if (eq ?muelle "Nelson Mandela") then
		(if (< ?espN ?l) then
		(printout t "XXXXX No hay espacio disponible en el muelle Nelson Mandela. XXXXX
		" crlf)
		)
		(if (> ?espN ?l) then
			(bind ?espN (- ?espN ?l))
			(modify ?hecho (espacio ?espN))
			(printout t "************Buque " ?b " atracado.************
			" crlf)
			(assert (buqueatracado
		 	 	(nombre ?b)
		 	 	(eslora ?l)
		 	 	(muelleS ?muelle)))
		)
	)

	(retract ?hechob)
	(retract ?eleccion)
	(retract ?interf)
	(retract ?orden)
	(assert (interfaz))
		
)
;---------------------------------------------------------------------------------------

(defrule salida 
	(orden "salida")
	=>
	(printout t "
	De acuerdo para salir indique el nombre de su buque: 
	" crlf)
	(assert(eleccionsalida(readline)))
)
;---------------------------------------------------------------------------------------
(defrule elegirsalida 
	(orden "salida")
	(eleccionsalida ?buque)
	(buqueatracado(nombre ?buque)(eslora ?ls)(muelleS ?muelleS))
	(muelle (nombre "Reina Sofia")(espacio ?espR))
	(muelle (nombre "Santa Catalina")(espacio ?espS))
	(muelle (nombre "Nelson Mandela")(espacio ?espN))
	?hechoS<-(muelle(nombre ?muelleS))
	?hechoba<-(buqueatracado(nombre ?buque)(eslora ?ls)(muelleS ?muelleS))
	?init<-(initial-fact)
	?interf<-(interfaz)
	?orden<-(orden ?)
	?eleccionsalida<-(eleccionsalida ?)
	=>
	(printout t "
	************De acuerdo su buque "?buque " sale del muelle " ?muelleS ".************
	" crlf)
	(if (eq ?muelleS "Reina Sofia") then
		(bind ?espR (+ ?espR ?ls))
		(modify ?hechoS (espacio ?espR))
	)
	(if (eq ?muelleS "Santa Catalina") then
		(bind ?espS (+ ?espS ?ls))
		(modify ?hechoS (espacio ?espS))
	)
	(if (eq ?muelleS "Nelson Mandela") then
		(bind ?espN (+ ?espN ?ls))
		(modify ?hechoS (espacio ?espN))
	)
	(retract ?hechoba)
	(retract ?interf)
	(retract ?orden)
	(retract ?eleccionsalida)
	(assert (interfaz))
)
;---------------------------------------------------------------------------------------

(defrule abandonar 
	(orden "esc")
	=>
	(printout t "
	Gracias por su visita. Abandonando el proceso.
	" crlf)
	(halt)(reset)
)	
;---------------------------------------------------------------------------------------


