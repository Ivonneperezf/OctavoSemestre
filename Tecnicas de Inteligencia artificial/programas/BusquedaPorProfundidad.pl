%Busqueda por profundidad
nmeta(12).
nmeta(7).
nmeta(11).
nmeta(14).
t(1,2).
t(1,3).
t(1,4).
t(1,14).
t(2,5).
t(2,6).
t(3,7).
t(3,8).
t(4,9).
t(4,10).
t(4,11).
t(5,12).
t(5,13).
%Caso base
bpp(N,[N]):-nmeta(N).
%Caso recursivo
%bpp(N,[N|R]):-t(N,N1),bpp(N1,R).
%Caso recursivo con corte o poda
bpp(N,[N|R]):-t(N,N1),bpp(N1,R),!.

%Caso con ciclos
nmeta2([celeste,carlo]).
t2([ana,juan],[daniela,pedro]).
t2([ana,juan],[karla,lucas]).
t2([ana,juan],[ale,david]).
t2([daniela,pedro],[rosa,abel]).
t2([karla,lucas],[rosa,abel]).
t2([karla,lucas],[rita,omar]).
t2([ale,david],[celeste,carlo]).
t2([rosa,abel],[celeste,carlo]).
t2([rita,omar],[rosa,abel]).
t2([rita,omar],[celeste,carlo]).

bpp2(N,[N]):-nmeta2(N).
bpp2(N,[N|R]):-t2(N,N1),bpp2(N1,R),!.

