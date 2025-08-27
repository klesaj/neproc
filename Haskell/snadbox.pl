% ---------- (a) vrstvy1(+BinStrom, ?SeznamVrstev) ----------
vrstvy1(nil, []).
vrstvy1(T, Layers) :-
        vrstvy1_q([T], Layers).

vrstvy1_q([], []).
vrstvy1_q(Level, [Vals|Rest]) :-
        maplist(node_value, Level, Vals),
        children(Level, Next),
        ( Next == [] -> Rest = []
        ; vrstvy1_q(Next, Rest)
        ).

node_value(b(V,_,_), V).

children([], []).
children([b(_,L,R)|Ns], Cs) :-
        child(L, CL),
        child(R, CR),
        children(Ns, CN),
        append(CL, CR, LR),
        append(LR, CN, Cs).

child(nil, []).
child(T, [T]) :- T \= nil.



% ---------- (b) vrstvy2(?BinStrom, +SeznamVrstev) ----------

vrstvy2(b(Root,L,R), [[Root]|Ls]) :-
        build(Ls, [b(Root,L,R)]).

build([], Ps) :- leaves(Ps).
build([Vals|More], Ps) :-
        distribute(Ps, Vals, Next),
        build(More, Next).

leaves([]).
leaves([b(_,nil,nil)|Qs]) :- leaves(Qs).

distribute([], [], []).
% 0 dě­tí
distribute([b(_,nil,nil)|Ps], Vs, Ns) :-
        distribute(Ps, Vs, Ns).
% jen levé
distribute([b(_,L,nil)|Ps], [C|Vs], [L|Ns]) :-
        L = b(C,_,_),
        distribute(Ps, Vs, Ns).
% jen pravé
distribute([b(_,nil,R)|Ps], [C|Vs], [R|Ns]) :-
        R = b(C,_,_),
        distribute(Ps, Vs, Ns).
% obě
distribute([b(_,L,R)|Ps], [C1,C2|Vs], [L,R|Ns]) :-
        L = b(C1,_,_),
        R = b(C2,_,_),
        distribute(Ps, Vs, Ns).



% ---------- (c) koncová rekurze ----------
% vrstvy1_q/2 i build/3 jsou koncově rekurzivní.

% ---------- (d) predikáty vyššího řádu ----------
% Žádný predikát vyššího řádu nebyl použit; maplist/3 by mohl nahradit node_value a children.


% trans(+Tree, -NewTree)
% NewTree je strom stejného tvaru, kde každé ohodnocení = aritmetický průměr
trans(Tree, NewTree) :-
        traverse(Tree, AvgVar, Sum, N, NewTree),
        AvgVar is Sum / N.

% traverse(+Tree, +AvgVar, -Sum, -Count, -OutTree)
traverse(t(V,Kids), A, S, C, t(A,OutKids)) :-
        trav_list(Kids, A, SK, CK, OutKids),
        S is V + SK,
        C is 1 + CK.

% trav_list(+ListOfTrees, +AvgVar, -Sum, -Count, -OutList)
trav_list([],    _, 0, 0, []).
trav_list([T|R], A, S, C, [OT|OR]) :-
        traverse(T, A, S1, C1, OT),
        trav_list(R, A, S2, C2, OR),
        S is S1 + S2,
        C is C1 + C2.
