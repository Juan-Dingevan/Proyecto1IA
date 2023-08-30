:- module(beliefs_update,
	  [
	    update_beliefs/1,
	    time/1,
	    node/5,
	    at/3,
		direction/1
	  ]).

:- dynamic time/1, node/5, at/3, direction/1.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% TO-DO
%
% update_beliefs(+Perc)
%
% IMPORTANTE: Debe exportarse todo predicado dinámico (creencia)
% manipulado por la actualización de creencias, para que puedan ser
% consultado por el resto del código del agente.
%
% El parámetro Perc recibe una lista con el siguiente formato: [N1,...,Nk,A1,...Ap,Time,Dir]
% donde: 
% N1 a Nk son k elementos (k>0) de la forma node(Id, PosX, PosY, Costo, Conexiones),
% A1 a Ap son p elementos (p>0) de la forma at(IdNodo, TipoEntidad, IdEntidad),
% Time es el functor time(T), donde T es el tiempo actual (descendente) de la partida.
% Dir es el functor direction(D), donde D ∈ {w, s, a, d}.
%
% Este agente básico, al recibir una nueva percepcion olvida todo lo que tenía guardado en su estado interno
% Y almance la información de la percepción nueva.
%
% Pueden realizar todos los cambios de implementación que consideren necesarios.
% Esta implementación busca ser un marco para facilitar la resolución del proyecto.

update_beliefs(Perc):-

	% El agente olvida todo lo que recordaba
	%retractall(time(_)),
	%retractall(direction(_)),
	%retractall(at(_, _, _)),

	member(time(T), Perc),						% Obtenemos el tiempo
	member(direction(D), Perc),					% Obtenemos la direccion

	findall(A,(
		member(A, Perc),
		A = at(_, _, _)
		), Ats), update_at(Ats),				% Obtenemos los at

	member(at(AtAgenteID, agente, me), Ats),	% Obtenemos un at distinguido: el del agente

	findall(N,(
		member(N, Perc),
		N = node(_,_,_,_,_)
		),Nodes), update_node(Nodes),			% Obtenemos los nodos

	update_time(T),								% Actualizamos el tiempo, ...
	update_direction(D),						% ... la direccion, ...
	
	update_node(Nodes),							% ... los nodos conocidos, ...
	update_at(Ats),								% ... y los at.
	
	remove_old_at(Nodes, Ats),					% Vemos si debemos olvidar algo
	
	update_agente(AtAgenteID).

% Un caso especial de los ats es el agente. Podemos olvidar inmediatamente la posicion del
% turno anterior sin necesidad de percibirla de nuevo, pues sabemos que el agente solo puede
% estar en un unico lugar en un dado momento.
update_agente(AtAgenteID) :- retractall(at(_, agente, me)), assert(at(AtAgenteID, agente, me)).

% Actualiza el tiempo, trivial
update_time(T) :- retractall(time(T)), assert(time(T)).

% Actualiza la direccion, trivial
update_direction(D) :- retractall(direction(D)), assert(direction(D)).

%actualiza los nodos (Notar que basta con olvidarlo, si es que existe, y re-asertarlo sin 
% ningun tipo de chequeos, pues los nodos son estaticos, no cambian entre un update y otro)
update_node(Nodes):- forall(member(Rel,Nodes), (retractall(Rel), assert(Rel))).

%caso base
update_at([]).

% si el elemento ya existe en las creencias y coincide en su totalidad no hacemos nada
update_at([At|Rest]):-
	At, !, update_at(Rest).

% si el elemento coincide solo parcialmente (o sea, es un reloj que cambio de tiempo),
% borramos y actualizamos
update_at([At|Rest]):-
	At = at(NodeID, Type, ObjID), 
	at(NodeID, OldType, _),
	Type \= OldType,
	!, 
	retractall(at(NodeID, _, _)),
	assert(at(NodeID, Type, ObjID)),
	update_at(Rest).

% si el elemento no existe en las creencias, lo agregamos a ellas
update_at([At|Rest]):- 
	not(At), 
	assert(At), 
	update_at(Rest).

% Queda un caso: se tiene un at/3 pero el elemento desaparece. En ese caso, llega
% un node/5 con la id de ese at, pero NO llega un at con esa id. Ahi, hacemos un retract
remove_old_at(NewNodes, NewAts) :-
	forall((
		member(NewNode, NewNodes),
		NewNode = node(NewID, _, _, _, _),    %Llega un nodo con un dado ID
		at(NewID, _, _),				   	  %En mi BDC tengo un at con ese mismo ID
		not(member(at(NewID, _, _), NewAts))  %Pero NO llega un at con esa ID, entonces desaparecio el obj
		), retractall(at(NewID, _, _))).