residuo(X,Y,Z) :- Z is X mod Y.
divisionentera(X,Y,Z) :- Z is X//Y.

resultado1(X,Y) :- X<6, Y=1.
resultado1(X,Y) :- -X>=6, Y=2.

resultado(X,1):-X<6.
resultado(X,2):-X>=6.

maximo(X,Y,X):-X>Y.
maximo(X,Y,Y):-Y>=X.

minimo(X,Y,X) :- X<Y.
minimo(X,Y,Y) :- Y=<X.

%Caso recursivo
factorial(N,R) :- N>0,N1 is N-1, factorial(N1, R1), R is N * R1.
%Caso base
factorial(0,1).

%Caso base
fibonacci(0,0).
fibonacci(1,1).
%Caso recursivo
fibonacci(N,R) :- N1 is  N-1,N2 is  N-2, fibonacci(N1, R1), fibonacci(N2, R2), R is R1 + R2.




