%Caso base
fibonacci(0,0).
fibonacci(1,1).
%Caso recursivo
fibonacci(N,R) :- N1 is  N-1,N2 is  N-2, fibonacci(N1, R1), fibonacci(N2, R2), R is R1 + R2.


%Caso base
pertenece(X,[X|_]).
%Caso recursivo
pertenece(X,[_|R]) :- pertenece(X,R).

%Revisa si una lista tiene una cantidad par de elementos.
ipar([]).
ipar([_,_|R]) :- ipar(R).

impar([_]).
impar([_,_|R]) :- impar(R).

%Revisar si la lista tiene una cantidad par de ana's
paranas([]).
paranas([X|C]):-X=='ana',imparanas(C);X\=='ana',paranas(C).
imparanas([X|C]):-X=='ana',paranas(C);X\=='ana',imparanas(C).

%Cuenta la canidda de elemntos de la lista
%Caso baase
longlista([],0).
longlista([_|C],R):-longlista(C,R1), R is R1+1.

%Suma los elemntos de la lista
sumalista([X],X).
sumalista([X|C],R):-sumalista(C,R1), R is R1+X.

e1 :- write("Dame un numero"),read(X),write(X).
e2 :- write("Dame tu nombre entre comillas simples"),read(X),write(X).

imprimelista([]).
imprimelista([X|L]):-write(X),nl,imprimelista(L).



