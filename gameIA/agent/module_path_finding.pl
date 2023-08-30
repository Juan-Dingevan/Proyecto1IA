:- module(path_finding,
	  [
	    buscar_plan_desplazamiento/4,
		raiz/1,
		padre/2,
		esMeta/1
	  ]).

:- use_module(module_beliefs_update, [node/5, at/3]).

:- dynamic padre/2, raiz/1, esMeta/1.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% eliminarPrimero(+Lista, +Elemento)
%
% Elimina el primer elemento de la lista.
%
eliminarPrimero([], []).
eliminarPrimero([_|Xs], Xs).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% seleccionar(+Nodo, +Frontera, +FronteraSinNodo)
%	
% Selecciona el primer nodo de la lista Frontera.
%	
seleccionar(Nodo, [Nodo|RestoLista], RestoLista).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% encontrarCamino(+Meta, -Camino)
%
% Encuentra un camino a un nodo Meta.
% Usa las relaciones padre(Hijo, Padre) que se van agregando a la base de conocimiento
% cuando se agregan nuevos vecinos a la nueva frontera, 
% en la busqueda de llegar de un nodo origen a uno destino.
%
encontrarCamino(Nodo, []):- raiz(Nodo), !.
encontrarCamino(Nodo, [P|Camino]):-
	padre(Nodo, P),
	encontrarCamino(P, Camino).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%		
%
% crearPlan(+Camino, -Plan)
%
% Crea plan de movimientos para un camino elegido.
% Para cada nodo de un camino, crea una lista de acciones de movimiento avanzar(IdNodo)
% donde IdNodo es un identificador de un nodo.
% Camino es una lista conteniendo identificadores de nodos.
%
crearPlan([], []).
crearPlan(Camino, Plan):-
	findall(avanzar(Nodo), member(Nodo, Camino), Plan).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% buscar_plan_desplazamiento(+Metas, -Plan, -Destino, -Costo)
% Agregar todas las metas como hechos esMeta(idNodoMeta)
% Si tiene al menos una meta, pone el nodo actual del agente como raiz del árbol de búsqueda
% y busca el camino desde la posición del agente a un meta
% usando A* (buscarEstrella/5)
%

buscar_plan_desplazamiento(Metas, Plan, Destino, Costo):-
	write('llegue a buscar_plan_desplazamiento\n'),
	
	forall(member(Meta, Metas), assert(esMeta(Meta))),
	at(MyNode, agente, me),
	length(Metas, CantMetas),
	CantMetas > 0,
	!,
	retractall(raiz(_)),
	assert(raiz(MyNode)),
	buscarEstrella([[MyNode, 0]], Metas, Camino, Costo, Destino),

	write('sali de buscarEstrella\n'),
	write('Camino = '),
	write(Camino),
	write('\n'),

	crearPlan(Camino, Plan).
	
buscar_plan_desplazamiento(_, [], [], 0).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% buscarEstrella(+Frontera, +Metas, ?Camino, ?Costo, ?Destino)
% 
% Busca el camino optimo desde la frontera hacia la meta mas cercana, utilizando la estrategia de busqueda A*.
%
	
buscarEstrella(Frontera, Metas, Camino, Costo, Destino):-
	buscar(Frontera, [], Metas, Destino),
	
	write('Sali de buscar/5 \n'),
	write('Destino = '),
	write(Destino),
	write('\n'),

	encontrarCamino(Destino, C),
	append([Destino], C, C2),	
	reverse(C2, C3),
	costoCamino(C3, Costo),
	eliminarPrimero(C3, Camino),
	retractall(esMeta(_)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% buscar(+Frontera, +Visitados, +Metas, -Destino)
% 
% Busca el camino optimo desde la frontera hacia la Meta, utilizando la estrategia de busqueda A*.
% No devuelve el camino como un parametro, sino que agrega las relaciones padre(Hijo, Padre)
% que permita luego encontrar el camino y su costo.
%
% Caso 1: Si el nodo es meta, termina la búsqueda.
% Caso 2: Si el nodo no es meta
% Selecciono el primer nodo de la frontera, 
% Genera los vecinos,
% Agregar nodo a visitados,
% Agregar vecinos a frontera, con los cuidados necesarios de A*
% y llama recursivmaente con la nueva frontera.
	
buscar(Frontera, _, _M, NodoID):-%cambiado
	write('EN BUSCAR 1:\n'),
	write('Frontera: '),
	write(Frontera),
	write('\n'),

	seleccionar([NodoID, _Costo], Frontera, _),

	write('Nodo seleccionado: '),
	write(NodoID),
	write('\n'),

	esMeta(NodoID),

	write('Enhorabuena, es una meta!'),
	write('\n'),

	!.

buscar(Frontera, Visitados, Metas, MM):-
	write('EN BUSCAR 2:\n'),
	write('Frontera: '),
	write(Frontera),
	write('\n'),

	seleccionar(Nodo, Frontera, FronteraSinNodo), % selecciona primer nodo de la frontera
	
	write('Nodo seleccionado: '),
	write(Nodo),
	write('\n'),

	generarVecinos(Nodo, Vecinos), % genera los vecinos del nodo - TO-DO

	write('Vecinos calculados: '),
	write(Vecinos),
	write('\n'),

	agregarAVisitados(Nodo, Visitados, NuevosVisitados), % agrega el nodo a lista de visitados
	
	agregar(FronteraSinNodo, Vecinos, NuevaFrontera, NuevosVisitados, VisitadosRevisados, Nodo, Metas), % agrega vecinos a la frontera - TO-DO
	
	write('Nueva Frontera: '),
	write(NuevaFrontera),
	write('\n'),

	write('Visitados Revisados: '),
	write(VisitadosRevisados),
	write('\n'),

	%fail,

	buscar(NuevaFrontera, VisitadosRevisados, Metas, MM). % continua la busqueda con la nueva frontera

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%	
%
% agregarAVisitados(+Nodo, +Visitados, ?VisitadosConNodo)
%
% Agrega un nodo a la lista de visitados.
%
agregarAVisitados(Nodo, Visitados, [Nodo | Visitados]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% costoCamino(+Lista, ?Costo)
%
% Calcula el costo del camino, 
% como la sumatoria de los costos de los nodos que forma el camino.
% Lista es una lista conteniendo identificadores de nodos, representando el camino.
%
costoCamino([], 0).

costoCamino([X|Xs], R):-
	node(X, _, _, CostoNodo, _),
	costoCamino(Xs, CostoResto),
	R is CostoNodo + CostoResto.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% calcularH(+Nodo, ?Resultado, +Meta)
%
% Calcula el valor de la heurística para el nodo Nodo a una Meta.
% La heurística es la distancia euclidea.
%
calcularH(NodoID, MetaID, Resultado):-
	node(MetaID, X2, Y2, _, _),
	node(NodoID, X1, Y1, _, _),
	distance([X1, Y1], [X2, Y2], Resultado).

distance([X1, Y1], [X2, Y2], Distance):-
	DX is X2 - X1,
	DY is Y2 - Y1,
	Distance is sqrt(DX^2 + DY^2).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% generarVecinos(+Nodo, -Vecinos)
%
% Obtiene los vecinos de un dado nodo.
% Nodo: Un par ordenado [ID, Costo], donde ID es el identificador de un nodo conocido y Costo es el costo de atravesarlo.
% Vecinos: Una lista de pares ordenados [ID_i, Costo_i], donde ID_i es el identificador del i-esimo nodo lindante al nodo con id ID
% 		   y Costo_i es la suma del costo del nodo con ID_i y el costo del nodo con id ID.
%
generarVecinos(Nodo, Vecinos) :-
	Nodo = [ID, Costo],
	node(ID, _, _, _, Conexiones),
	!,
	findall([NewID, NewPeso], (
				member([NewID, OldPeso], Conexiones),
				NewPeso is OldPeso + Costo
			), Vecinos).

% Si no se tiene un nodo con id ID, devolvemos vacio
% En teoria, nunca se cae en este caso
generarVecinos(Nodo, []) :-
	Nodo = [ID, _Costo],
	not(node(ID, _, _, _, _)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% mejorValorHeuristica(+ID, +Metas, -MejorH)
%
% Calcula todos los valores de H(ID) para los nodos Meta cuya ID es miembro de Metas
% Y unifica MejorH con el menor valor.
%
% ID: ID del nodo sobre el que calcular
% Metas: Lista de identificador de nodos meta.
% MejorH: menor valor de H(ID) calculado
%
mejorValorHeuristica(ID, Metas, MejorH) :-
	findall(H, (
		member(Meta, Metas),
		calcularH(ID, Meta, H)
	), Hs),
	min_list(Hs, MejorH).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% vecinosConFn(+Vecinos, +Metas, -VecinosConFn)
%
% Calcula f(N) para los vecinos.
%
% Vecinos: Una lista de pares ordenados [ID_i, Costo_i] donde Costo_i = g(ID_i)
% Metas: una lista de ID de nodos que son meta
% VecinosConFn: Una lista de pares ordenados [ID_i, CostoFn_i] donde CostoFn_i = f(ID_i) = g(ID_i) + h(ID_i)
%
vecinosConFn(Vecinos, Metas, VecinosConFn) :-
	findall([ID, NewCosto], (
		member([ID, OldCosto], Vecinos),
		mejorValorHeuristica(ID, Metas, MejorH),
		NewCosto is OldCosto + MejorH
	), VecinosConFn).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Predicados generado por ChatGPT
% Predicado para ordenar una lista según el costo
ordenar_por_costo([], []).
ordenar_por_costo([H | T], Sorted) :-
    ordenar_por_costo(T, SortedT),
    insertar_ordenado(H, SortedT, Sorted).

% Predicado para insertar un elemento en una lista ordenada
insertar_ordenado(X, [], [X]) :- !.
insertar_ordenado(X, [Y | Resto], [X, Y | Resto]) :-
    X = [_IDX, CostoX],
    Y = [_IDY, CostoY],
    CostoX =< CostoY, !.
insertar_ordenado(X, [Y | Resto], [Y | SortedResto]) :-
    insertar_ordenado(X, Resto, SortedResto).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% eliminarVecinosVisitados(+Vecinos, +Visitados, -VecinosSinVisitar)
% Elimina a todos los vecinos que fueron visitados previamente
eliminarVecinosVisitados(Vecinos, Visitados, VecinosSinVisitar) :-
	findall(VecinoNoVisitado, (
		VecinoNoVisitado = [ID, _],
		member(VecinoNoVisitado, Vecinos),
		not(member([ID, _], Visitados))
	), VecinosSinVisitar).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% asertarRelacionesPadreHijo(+Padre, +Hijos)
% Hace assert de la relacion padre(Hijo, Padre) entre el nodo Padre y todos los miembros de Hijos
% Padre es un nodo Padre de forma [ID, Costo]
% Hijos es una lista de nodos hijos de la misma forma
asertarRelacionesPadreHijo(Padre, Hijos) :-
	Padre = [PadreID, _],
	forall((
		member([HijoID, _], Hijos)
	), assert(padre(HijoID, PadreID))).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% agregar(+FronteraSinNodo, +Vecinos, -NuevaFrontera, +NuevosVisitados, -VisitadosRevisados, +Nodo, +Metas)
% FronteraSinNodo: obvio. Lista de listas de forma [ID, f(N)]
% Vecinos: Lista de listas de forma [ID, Costo]
% NuevaFrontera, idem FronteraSinNodo, pero ahora ya con todo agregado
% Visitados: Visitados que contiene a Nodo
% VisitadosRevisados: Visitados pero fijandonos si encontramos algun mejor camino a un dado nodo (generalmente va  aser igual a NuevosVisitados)
% Nodo: obvio
% Metas: Lista de ids de nodos metas.

%Nota: por ahora no hacemos revision de visitados.
agregar(FronteraSinNodo, Vecinos, NuevaFrontera, Visitados, NuevosVisitados, Nodo, Metas) :-
	eliminarVecinosVisitados(Vecinos, Visitados, VecinosSinVisitar),
	asertarRelacionesPadreHijo(Nodo, VecinosSinVisitar),
	vecinosConFn(VecinosSinVisitar, Metas, VecinosConFn),
	append(VecinosConFn, FronteraSinNodo, FronteraDesordenada),
	ordenar_por_costo(FronteraDesordenada, NuevaFrontera),
	Visitados = NuevosVisitados. %TEMP
	