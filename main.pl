:- dynamic book/8.

main:-
    writeln('Vitajte v databaze knih'),
    read_from_file('db'),
    repeat,
    menu,
    get(I),
    execute(I),
    I == 48,
    writeln('Koniec').

menu:-
    nl,
    writeln('1 - Pridat knihu'),
    writeln('2 - Vyradit knihu'),
    writeln('3 - Vyhladat knihu alebo skupinu knih'),
    writeln('4 - Zoradit knihy'),
    writeln('5 - Nacitaj zo suboru'),
    writeln('6 - Uloz do suboru'),
    writeln('0 - Ukoncit program'),
    nl.

findmenu:-
    nl,
    writeln('Moznosti vyhladania:'),
    writeln('1 - ID'),
    writeln('2 - Nazov knihy'),
    writeln('3 - Meno autora'),
    writeln('4 - Klucove slova'),
    writeln('0 - Spat na menu'),
    nl.

sortmenu():-
    nl,
    writeln('Moznosti zoradenia:'),
    writeln('1 - Nazov knihy'),
    writeln('2 - Meno autora'),
    writeln('3 - Datum vydania'),
    writeln('4 - Pocet stran'),
    writeln('0 - Spat na menu'),
    nl.

execute(49):- addbook(), !.
execute(50):- removebook(), !.
execute(51):- findbook(), !.
execute(52):- sortbooks(), !.
execute(53):- read_from_file('db'), !.
execute(54):- write_to_file('db'), !.
execute(48):- !.

addbook():-
    auto_increment(Id),
    writeln('Nazov knihy: '),
    read_atom(_),
    read_atom(Name),
    writeln('Meno autora: '),
    read_atom(Author),
    writeln('Vydavatelstvo: '),
    read_atom(Publisher),
    writeln('Datum vydania (MM-RR): '),
    read_atom(PublishDate),
    writeln('Klucove slova (oddelene ciarkov alebo medzerov):'),
    atom_list_input(KeyWords),
    writeln('Pocet stran: '),
    read_number(Pages),
    writeln('ISBN: '),
    read_atom(ISBN),

    assertz(book(Id, Name, Author, Publisher, PublishDate, KeyWords, Pages, ISBN)),
    write_to_file('db').

removebook():-
    writeln('Identifikacne cislo:'),
    read_atom(_),
    read_number(Id),
    writeln('ISBN: '),
    read_atom(ISBN),
    retract(book(Id,_,_,_,_,_,_,ISBN)),

    write_to_file('db').

read_from_file(F):-
    retractall(book(_,_,_,_,_,_,_,_)),
    see(F),
    repeat,
    read(Term), 
    (
        Term = end_of_file,
        !,
        seen
        ;
        assertz(Term),
        fail
    ).

write_to_file(F):-
    tell(F),
    book(Id, Name, Author, Publisher, PublishDate, KeyWords, Pages, ISBN),
    writeq(book(Id, Name, Author, Publisher, PublishDate, KeyWords, Pages, ISBN)),
    write('.'),
    nl,
    fail.

write_to_file(_):-told.

check_members([H|T], L):-
    not(member(H, L)), !,
    check_members(T, L).

check_members([H|_], L):-
    member(H, L), !.

check_keywords([], [_|_], []).

check_keywords([H|T], I, [H|T1]):-
    nth0(5, H, K),
    writeln(K),
    check_members(I, K), !,
    check_keywords(T, I, T1).

check_keywords([_|T], I, T):-
    check_keywords(T, I, T).

findbook():-
    writeln('Nazov knihy: '),
    read_atom(_),
    read_atom(Name),
    writeln('Meno autora: '),
    read_atom(Author),
    writeln('Vydavatelstvo: '),
    read_atom(Publisher),
    writeln('Datum vydania (MM-RR): '),
    read_atom(PublishDate),
    writeln('Klucove slova (oddelene ciarkov alebo medzerov):'),
    atom_list_input(KeyWords),
    writeln('Pocet stran: '),
    read_number(Pages),
    writeln('ISBN: '),
    read_atom(ISBN),

    findall([Id, Name, Author, Publisher, PublishDate, KW, Pages, ISBN], book(Id, Name, Author, Publisher, PublishDate, KW, Pages, ISBN), List),
    writeln('-------------'),
    check_keywords(List, KeyWords, NewList),
    print_books(NewList).

auto_increment(NewId):-
    findall(Id, book(Id,_,_,_,_,_,_,_), List),
    max_list(List, MaxId),
    NewId is MaxId + 1.

print_list([]).
print_list([X|Y]):-
    write(X),
    write(' '),
    print_list(Y).

print_books([]).
print_books([H|T]):-
    nth0(0, H, Id),
    write('ID knihy: '), writeln(Id),

    nth0(1, H, Name),
    write('Nazov knihy: '), writeln(Name),

    nth0(2, H, Author),
    write('Autor knihy: '), writeln(Author),

    nth0(3, H, Publisher),
    write('Vydavatelstvo '), writeln(Publisher),

    nth0(4, H, PublishDate),
    write('Datum vydania: '), writeln(PublishDate),

    nth0(5, H, KeyWords),
    write('Klucove slova: '), print_list(KeyWords), nl,

    nth0(6, H, Pages),
    write('Pocet stran: '), writeln(Pages),

    nth0(7, H, ISBN),
    write('ISBN: '), writeln(ISBN),

    nl,

    print_books(T).

atom_list_input(L):-
    current_input(In),
    read_line_to_codes(In, Input),
    not(is_empty(Input)), !,
    string_to_atom(Input, A),
    atomic_list_concat(P, ',', A),
    atomic_list_concat(P, '', S),
    atomic_list_concat(L, ' ', S).

read_atom(Atom):-
    current_input(Input),
    read_line_to_codes(Input,Codes),
    not(is_empty(Codes)),
    string_to_atom(Codes,Atom).

is_empty([]).

read_number(Number):-
    current_input(Input),
    read_line_to_codes(Input,Codes),
    not(is_empty(Codes)),
    number_codes(Number,Codes).

sortbooks():-
    repeat,
    sortmenu,
    get(I),
    exc_order(I),
    I == 48,
    writeln('Navrat do menu').

exc_order(49):- sortb(49), !.
exc_order(50):- sortb(50), !.
exc_order(51):- sortb(51), !.
exc_order(52):- sortb(52), !.
exc_order(48):- !.

sortb(49):-
    findall([Id, Name, Author, Publisher, PublishDate, KeyWords, Pages, ISBN], book(Id, Name, Author, Publisher, PublishDate, KeyWords, Pages, ISBN), List),
    predsort(nthcompare(2), List, SortedList),

    print_books(SortedList).

sortb(50):-
    findall([Id, Name, Author, Publisher, PublishDate, KeyWords, Pages, ISBN], book(Id, Name, Author, Publisher, PublishDate, KeyWords, Pages, ISBN), List),
    predsort(nthcompare(3), List, SortedList),

    print_books(SortedList).

sortb(51):-
    findall([Id, Name, Author, Publisher, PublishDate, KeyWords, Pages, ISBN], book(Id, Name, Author, Publisher, PublishDate, KeyWords, Pages, ISBN), List),
    process_swapdates(List, SwapedList),
    predsort(nthcompare(5), SwapedList, SortedSwapedList),
    process_swapdates(SortedSwapedList, SortedList),
    print_books(SortedList).

sortb(52):-
    findall([Id, Name, Author, Publisher, PublishDate, KeyWords, Pages, ISBN], book(Id, Name, Author, Publisher, PublishDate, KeyWords, Pages, ISBN), List),
    predsort(nthcompare(7), List, SortedList),

    print_books(SortedList).

nthcompare(N,<,A,B) :- nth1(N,A,X),nth1(N,B,Y), X @< Y.
nthcompare(_,>,_,_).

swapdates(List, Out):-
    nth0(4, List, X),
    atomic_list_concat(Y, '-', X),
    nth0(0, Y, Month),
    nth0(1, Y, Year),
    atomic_list_concat([Year, Month], '-', YM),
    select(X, List, YM, Out).

process_swapdates([],[]).
process_swapdates([H|T], [S|Out]):-
    swapdates(H, S),
    process_swapdates(T, Out).