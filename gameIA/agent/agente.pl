:- use_module(module_beliefs_update, [
 	update_beliefs/1,
 	time/1,
 	node/5,
	at/3,
	direction/1
]).

:- use_module(module_path_finding, [
 	buscar_plan_desplazamiento/4,
 	raiz/1,
 	padre/2,
 	esMeta/1
]).

:- use_module(extras, [
	append3/4
]).

:- dynamic plandesplazamiento/1.
:- dynamic cant_girar_seguidos/1.

deseables([copa, cofre, diamante, reloj(_X), pocion]).
cant_girar_seguidos(0).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% run(+Perc, -Action, -Text, -Beliefs)
%
% El predicado run/4 implementa el comportamiento del agente.
%
% Es ejecutado automáticamente por el juego una vez por ciclo.
% Implementa la interface con el mundo virtual, 
% recibiendo la información que el agente puede percibir del entorno
% y permite hacer que el agente actue en el mundo virtual
% No pueden cambiarse los parámetros, pero si el cuerpo.
% 
% El primer parámetro (Perc) recibe una lista con la percepción del agente.
% Los otros parámetros envían información al juego.
% La idea es que el agente actualice su estado interno usando Perc, 
% y luego decida que acción hacer instanciándola en el parámetro Action.
% Text es un texto con comillas simples como 'hola' que será mostrado en la pantalla del juego.
% Beliefs es una lista con un subconjunto de creencias del agente, particularmente las que hacer referencia a objetos.
%
% El parámetro Perc recibe una lista con el siguiente formato: [N1,...,Nk,A1,...Ap,Time,Dir]
% donde: 
% N1 a Nk son k elementos (k>0) de la forma node(Id, PosX, PosY, Costo, Conexiones),
% A1 a Ap son p elementos (p>0) de la forma at(IdNodo, TipoEntidad, IdEntidad),
% Time es el functor time(T), donde T es el tiempo actual (descendente) de la partida.
% Dir es el functor direction(D), donde D ∈ {w, s, a, d}.
% IMPORTANTE: La lista de la percepción no viene ordenada. El formato anterior es solo a modo de referencia.
%
% Action debe ser un functor totalmente instanciado (sin variables) que corresponda a una acción valida.
% Si Action tiene algún error el agente pierde el ciclo de ejecución.

run(Perc, Action, Text, Beliefs):-
	update_beliefs(Perc), % implementado en module_beliefs_update
	decide_action(Action, Text),
	
	write('ACCION ENVIADA: '),
	write(Action),
	write(' ('),
	write(Text),
	write(')\n'),
	
	findall(at(X, Y, Z), at(X, Y, Z), Beliefs).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% TO-DO
%
% decide_action(-Action, -Text)
%
% Decide la acción a realizar por el agente, instanciándola en el parámetro Action.
% Text es un texto con comillas simples como 'hola' que será mostrado en la pantalla del juego.
%
% En la implementación siguiente:
% El primer caso (1), sirve de ejemplo para levantar un objeto del terreno.
% El segundo caso (2) del predicado siempre tiene éxito, y hace que el agente se mueva de manera aleatoria.
% El tercer caso (3) permite al agente ejecutar la siguiente acción de movimiento de un plan guardado, calculado previamente.
% El cuarto caso (4) permite obtener un nuevo plan de movimiento usando A*.
% El quinto caso (5) hace que el agente gire en su posición, en sentido horario.
%
% Deberán completar la implementación del algoritmo de búsqueda A* para que funcionen los casos (3) y (4)
% Y eliminar el caso (2), para permitir al agente seguir el plan de movimientos.
%
% Pueden realizar todos los cambios de implementación que consideren necesarios.
% Esta implementación busca ser un marco para facilitar la resolución del proyecto.

% Si estoy en la misma posición que una copa, intento levantarla.
decide_action(Action, 'Quiero levantar algo...') :-
    at(MyNode, agente, me),
	node(MyNode, PosX, PosY, _, _),

	write('MyNode y posiciones\n'),

	at(MyNode, Deseable, IdGold),
	deseables(Deseables),
	member(Deseable, Deseables),

	write('Unifique Deseable e IdGold\n'),
    
	Action = levantar_tesoro(IdGold, PosX, PosY),

	write('Unifique Action\n'),
    
	retractall(at(MyNode, _, IdGold)),
	retractall(plandesplazamiento(_)),
	
	retractall(cant_girar_seguidos(_)),
	assert(cant_girar_seguidos(0)).

% Si tengo un plan de movimientos, ejecuto la siguiente acción.
decide_action(Action, 'Avanzar...'):-
	plandesplazamiento(Plan),
	length(Plan, LargoPlan),
	LargoPlan > 0,
	!,
	obtenerMovimiento(Plan, Destino, Resto),
	retractall(plandesplazamiento(_)),
	assert(plandesplazamiento(Resto)),
	Action = Destino,

	retractall(cant_girar_seguidos(_)),
	assert(cant_girar_seguidos(0)).
	
% Si no tengo un plan guardado, busco uno nuevo.
decide_action(Action, 'Avanzar con nuevo plan...'):-
 	busqueda_plan(Plan, _Destino, _Costo),
	Plan \= [],
	obtenerMovimiento(Plan, Action, Resto),
	assert(plandesplazamiento(Resto)),

	retractall(cant_girar_seguidos(_)),
	assert(cant_girar_seguidos(0)).

% Giro en sentido horario, para conocer mas terreno.
decide_action(Action, 'Girar para conocer el territorio...'):-
	cant_girar_seguidos(X),
	X < 4,
	
	% Si ya se giro 4 veces seguidas, se dio una vuelta completa (y presumiblemente no
	% se encontraron metas posibles), por tanto se opta por fallar y por caer en el caso de
	% movimiento alteatorio.

	direction(D),
	girar(D, Action),

	retractall(cant_girar_seguidos(_)),
	Y is X + 1
	assert(cant_girar_seguidos(Y)).

girar(w, girar(d)).
girar(d, girar(s)).
girar(s, girar(a)).
girar(a, girar(w)).

% Me muevo a una posición vecina seleccionada de manera aleatoria.
decide_action(Action, 'Me muevo a la posicion de al lado...'):-
	at(MyNode, agente, me),
	node(MyNode, _, _, _, AdyList),
	length(AdyList, LenAdyList), LenAdyList > 0,
	random_member([IdAdyNode, _CostAdyNode], AdyList),
	!,
	Action = avanzar(IdAdyNode),
	
	retractall(cant_girar_seguidos(_)),
	assert(cant_girar_seguidos(0)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% obtenerMovimiento(?Lista, ?Movimiento, ?Resto)
%
% Obtiene el primer movimiento de una lista de movimientos.
% 
obtenerMovimiento([], [], []).
obtenerMovimiento([X|Xs], X, Xs).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% busqueda_plan(-Plan, -Destino, -Costo)
%
% Busca un plan de desplazamiento hacia el tesoro que se encuentre mas cerca.
%	
busqueda_plan(Plan, Destino, Costo):-
	write('llegue a busqueda_plan\n'),
 	retractall(plandesplazamiento(_)),
	retractall(esMeta(_)),

	deseables(Deseables),

 	findall(Nodo, (
		at(Nodo, Type, _),
		member(Type, Deseables)
	), Metas), % nuevas metas

	Metas \= [],

	write('Metas: '),
	write(Metas),
	write('\n'),

 	buscar_plan_desplazamiento(Metas, Plan, Destino, Costo). % implementado en module_path_finding