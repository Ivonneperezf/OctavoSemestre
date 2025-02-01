%Problema de las n reinas

rpd(S):-inicializa(S),solucion(S).
%inicializa([1/Y1, 2/Y2, 3/Y3, 4/Y4, 5/Y5, 6/Y6, 7/Y7, 8/Y8]).
solucion([]).
solucion([X/Y|Otras]):-solucion(Otras),pertenece(Y,[1,2,3,4]), noataca(X/Y,Otras).

%Caso base
noataca(_,[]).
%Caso recursivo
noataca(X/Y,[X1/Y1|Otras]):-Y=\=Y1, Y1-Y=\=X1-X, Y1-Y=\=X-X1,noataca(X/Y,Otras).

%Caso base
pertenece(X,[X|_]).
pertenece(X,[_|C]):-pertenece(X,C).

inicializa([1/Y1, 2/Y2, 3/Y3, 4/Y4]).