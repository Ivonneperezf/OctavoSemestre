%
solucion(X):-
    X=[[_,_,chocolate],[_,_,galletas],[_,_,cafe],[_,_,vino]],
    pertenece([_,castillo,chocolate],X),
    pertenece([_,leon,Y],X),Y\==galletas,
    pertenece([carolina,_,Z],X),Z\==galletas,
    pertenece([fanny, diaz,W],X),W\==galletas,
    pertenece([_,olguin,_],X),
    pertenece([alejandra,A,cafe],X),A\==olguin,
    pertenece([ana,_,T],X),T\==vino.

	%Aqui solo selecciona las partes de la lista
	pertenece(X,[X|_]).
	pertenece(X,[_|Z]):-pertenece(X,Z).

%Simular el automata finito determinista-----------------------------------
%Define el automata
edoini(q0).
edofin(q3).
t(q0,a,q1).
t(q0,b,q2).
t(q1,a,q0).
t(q1,b,q3).
t(q2,a,q3).
t(q2,b,q0).
t(q3,a,q2).
t(q3,b,q1).

afd(E,L):-edoini(E), afd1(E,L).%Verifica si la E es estado inicial
%Caso base
afd1(E,[]):-edofin(E). %Si termino la lista y el estado donde quedo es estado final true
%caso recursivo
%Verifica si apartir del estado hace la transicion y ve el estado
afd1(E,[X|C]):-t(E,X,E1),afd1(E1,C).
%----------------------------------------------------------------------------



