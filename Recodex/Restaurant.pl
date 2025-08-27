
male(david).
male(thomas).

female(emma).
female(stella).

solve(Dumplings, Pasta, Soup, Trout) :-
    Table = [S0, S1, S2, S3],
    S0 = seat(P0, F0, B0),
    S1 = seat(P1, F1, B1),
    S2 = seat(P2, F2, B2),
    S3 = seat(P3, F3, B3),

    permutation([P0, P1, P2, P3], [david, emma, stella, thomas]),
    permutation([F0, F1, F2, F3], [dumplings, pasta, soup, trout]),
    permutation([B0, B1, B2, B3], [beer, wine, cider, iced_tea]),

    ((male(P0), male(P2)) ; (female(P0), female(P2))),
    ((male(P1), male(P3)) ; (female(P1), female(P3))),

    % The person with cider sat across from the person with trout.
    is_across(Table, beverage, cider, food, trout),

    % Dumplings came with beer.
    came_together(Table, dumplings, beer),

    % Mushroom soup came with cider.
    came_together(Table, soup, cider),

    % The person with pasta sat across from the person with beer.
    is_across(Table, food, pasta, beverage, beer),

    % David never drinks iced tea.
    member(seat(david, _, B), Table), B \= iced_tea,

    % Emma only drinks wine.
    member(seat(emma, _, wine), Table),

    % Stella does not like dumplings.
    member(seat(stella, F, _), Table), F \= dumplings,

    member(seat(Dumplings, dumplings, _), Table),
    member(seat(Pasta, pasta, _), Table),
    member(seat(Soup, soup, _), Table),
    member(seat(Trout, trout, _), Table).

is_across(Table, Field1, Value1, Field2, Value2) :-
    nth0(0, Table, Seat0),
    nth0(2, Table, Seat2),
    ( (seat_field_check(Seat0, Field1, Value1), seat_field_check(Seat2, Field2, Value2))
    ; (seat_field_check(Seat2, Field1, Value1), seat_field_check(Seat0, Field2, Value2))
    ).
is_across(Table, Field1, Value1, Field2, Value2) :-
    nth0(1, Table, Seat1),
    nth0(3, Table, Seat3),
    ( (seat_field_check(Seat1, Field1, Value1), seat_field_check(Seat3, Field2, Value2))
    ; (seat_field_check(Seat3, Field1, Value1), seat_field_check(Seat1, Field2, Value2))
    ).

came_together(Table, Food, Beverage) :-
    member(seat(_, Food, Beverage), Table).

seat_field_check(seat(Person, _, _), person, Person).
seat_field_check(seat(_, Food, _), food, Food).
seat_field_check(seat(_, _, Beverage), beverage, Beverage).
