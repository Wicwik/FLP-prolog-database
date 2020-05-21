%% program na manazment s databazov knih
%% Robert Belanec

%% definovanie dynamickeho termu book, ktory reprezentuje knihu s 9 parametrami
:- dynamic book/9.

%% predikat, ktory spusti program
main:-
    writeln('Vitajte v databaze knih'),
    read_from_file('db'),
    repeat,
    menu,
    get(I), %% volba, vracia asscii hodnotu znaku
    execute(I),
    I == 48, %% ak je volba 0, program konci
    writeln('Koniec').

%% predikat, ktroy vypise hlavne menu
menu:-
    nl,
    writeln('1 - Pridat knihu'),
    writeln('2 - Vyradit knihu'),
    writeln('3 - Vyhladat knihu alebo skupinu knih'),
    writeln('4 - Zoradit knihy'),
    writeln('5 - Nacitaj zo suboru'),
    writeln('6 - Uloz do suboru'),
    writeln('7 - Zoskupit podla zanrov'),
    writeln('0 - Ukoncit program'),
    nl.

%% predikat, ktory vypise menu pre zoradenie
sortmenu:-
    nl,
    writeln('Moznosti zoradenia:'),
    writeln('1 - Nazov knihy'),
    writeln('2 - Meno autora'),
    writeln('3 - Datum vydania'),
    writeln('4 - Pocet stran'),
    writeln('0 - Spat na menu'),
    nl.

%% predikat, ktory spusti vykonavanie ineho predikatu podla volby v predikate main
%% execute(+Number)
execute(49):- addbook(), !.
execute(50):- removebook(), !.
execute(51):- findbook(), !.
execute(52):- sortbooks(), !.
execute(53):- read_from_file('db'), !.
execute(54):- write_to_file('db'), !.
execute(55):- group_by_genre(), !.
execute(48):- !.

%% prida knihu do databazy a zapise novu databazu do suboru
addbook:-
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
    writeln('Zaner: '),
    read_atom(Genre),

    assertz(book(Id, Name, Author, Publisher, PublishDate, KeyWords, Pages, ISBN, Genre)),
    write_to_file('db').

%% vymaze knihu z databazy a zapise novu databazu do suboru 
removebook:-
    writeln('Identifikacne cislo:'),
    read_atom(_),
    read_number(Id),
    writeln('ISBN: '),
    read_atom(ISBN),
    retract(book(Id,_,_,_,_,_,_,ISBN,_)),

    write_to_file('db').

%% precita termy zo suboru a zapise ich do databazy
%% read_from_file(+Filename)
read_from_file(F):-
    retractall(book(_,_,_,_,_,_,_,_,_)), %% na zaciatku je potrebne vymazat databazu
    see(F),
    repeat,
    read(Term), 
    (
        Term = end_of_file, %% ak je koniec suboru zavrie subor
        !,
        seen
        ;
        assertz(Term), %% inak prida term do databazy
        fail
    ).

%% zapise termy z databazy do suboru
%% write_to_file(+Filename)
write_to_file(F):-
    tell(F),
    book(Id, Name, Author, Publisher, PublishDate, KeyWords, Pages, ISBN, Genre),
    writeq(book(Id, Name, Author, Publisher, PublishDate, KeyWords, Pages, ISBN, Genre)),
    write('.'),
    nl,
    fail.

%% po zapise zavrie subor
write_to_file(_):-told.

%% vracia true, ak sa aspon jeden element z prveho listu nachadza v druhom liste
%% check_members(+List, +List)
check_members([H|T], L):-
    not(member(H, L)), !,
    check_members(T, L).

check_members([H|_], L):-
    member(H, L), !.

%% Vytvori list knih, v ktorych sa nachadza niektore z klucovych slov
%% check_keywords(+InList, +KeyWords, -OutList)
check_keywords([], [_|_], []).

check_keywords([H|T], I, [H|T1]):-
    nth0(5, H, K),
    check_members(I, K), !,
    check_keywords(T, I, T1).

check_keywords([_|T], I, T):-
    check_keywords(T, I, T).

%% predikat na hladanie knihy alebo skupiny knih, ak pouzivatel necha vstup prazny, znamena to ze sa na danu premennu moze naviazat cokolvek
findbook:-
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
    writeln('Zaner: '),
    read_atom(Genre),

    findall([Id, Name, Author, Publisher, PublishDate, KW, Pages, ISBN, Genre], book(Id, Name, Author, Publisher, PublishDate, KW, Pages, ISBN, Genre), List),
    writeln('-------------'),
    check_keywords(List, KeyWords, NewList),
    print_books(NewList).

%% predikat, ktory vrati nove ID knihy, ktore je o 1 vacsie ako maximum zo vsetkych ID
%% auto_increment(-Id)
auto_increment(NewId):-
    findall(Id, book(Id,_,_,_,_,_,_,_,_), List),
    max_list(List, MaxId),
    NewId is MaxId + 1.

%% vypise elementy zoznamu za sebout oddelene ' '
%% print_list(+List)
print_list([]).
print_list([X|Y]):-
    write(X),
    write(' '),
    print_list(Y).

%% vypise knihy zo zoznamu podseba
%% print_books(+List)
print_books([]).
print_books([H|T]):-
    nth0(0, H, Id),
    write('ID knihy: '), writeln(Id),

    nth0(1, H, Name),
    write('Nazov knihy: '), writeln(Name),

    nth0(2, H, Author),
    write('Autor knihy: '), writeln(Author),

    nth0(3, H, Publisher),
    write('Vydavatelstvo: '), writeln(Publisher),

    nth0(4, H, PublishDate),
    write('Datum vydania: '), writeln(PublishDate),

    nth0(5, H, KeyWords),
    write('Klucove slova: '), print_list(KeyWords), nl,

    nth0(6, H, Pages),
    write('Pocet stran: '), writeln(Pages),

    nth0(7, H, ISBN),
    write('ISBN: '), writeln(ISBN),

    nth0(8, H, Genre),
    write('Zaner: '), writeln(Genre),

    nl,

    print_books(T).

%% term, ktory vracia true ak je pole prazdne
%% is_empty(+List)
is_empty([]).

%% predikat, ktory nacita zoznam atomov, odelenych ciarkov alebo medzerov a ulozi ich do pola
%% atom_list_input(-List)
atom_list_input(L):-
    current_input(In),
    read_line_to_codes(In, Input),
    not(is_empty(Input)), !, %% pre pripad ze pouzivatel zada prazdny vstup
    string_to_atom(Input, A), %% zmeni kody retazcov na atom
    atomic_list_concat(P, ',', A), %% rozdeli atom na polia s rozdelovacom ,
    atomic_list_concat(P, '', S), %% spoli list atomov do jedneho atomu
    atomic_list_concat(L, ' ', S). %% rozdeli atomy do pola atomov s rozdelovacom ' ', pre pripad ze existuje aj ciarka aj medzera

%% splni sa ak je prazdny vstup
atom_list_input(_).

%% predikat, ktory nacita atom
%% read_atom(-Atom)
read_atom(Atom):-
    current_input(Input),
    read_line_to_codes(Input,Codes),
    not(is_empty(Codes)), !, %% pre pripad ze pouzivatel zada prazdny vstup
    string_to_atom(Codes,Atom).

%% splni sa ak je prazdny vstup
read_atom(_).

%% predikat, ktory nacita cislo
%% read_number(-Number)
read_number(Number):-
    current_input(Input),
    read_line_to_codes(Input,Codes),
    not(is_empty(Codes)), !,  %% pre pripad ze pouzivatel zada prazdny vstup
    number_codes(Number,Codes).

%% splni sa ak je prazdny vstup
read_number(_).

%% predikat, ktory spusti vyber moznosti zoradenia
sortbooks:-
    repeat,
    sortmenu,
    get(I),
    exc_order(I),
    I == 48,
    writeln('Navrat do menu').

%% predikat, ktory spusti vykonavanie ineho predikatu podla volby v predikate sortbooks
%% exc_order(+Number)
exc_order(49):- sortb(49), !.
exc_order(50):- sortb(50), !.
exc_order(51):- sortb(51), !.
exc_order(52):- sortb(52), !.
exc_order(48):- !.

%% predikat, ktory zoradi pole podla zvolenej moznosti
%% sortb(+Number)
sortb(49):-
    findall([Id, Name, Author, Publisher, PublishDate, KeyWords, Pages, ISBN, Genre], book(Id, Name, Author, Publisher, PublishDate, KeyWords, Pages, ISBN, Genre), List),
    predsort(nthcompare(2), List, SortedList), %% predsort, zoraduje pole podla toho ci predikat nthcompare vracia true ablebo false

    print_books(SortedList).

sortb(50):-
    findall([Id, Name, Author, Publisher, PublishDate, KeyWords, Pages, ISBN, Genre], book(Id, Name, Author, Publisher, PublishDate, KeyWords, Pages, ISBN, Genre), List),
    predsort(nthcompare(3), List, SortedList), %% predsort, zoraduje pole podla toho ci predikat nthcompare vracia true ablebo false

    print_books(SortedList).

sortb(51):-
    findall([Id, Name, Author, Publisher, PublishDate, KeyWords, Pages, ISBN, Genre], book(Id, Name, Author, Publisher, PublishDate, KeyWords, Pages, ISBN, Genre), List),
    process_swapdates(List, SwapedList),
    predsort(nthcompare(5), SwapedList, SortedSwapedList), %% predsort, zoraduje pole podla toho ci predikat nthcompare vracia true ablebo false
    process_swapdates(SortedSwapedList, SortedList),
    print_books(SortedList).

sortb(52):-
    findall([Id, Name, Author, Publisher, PublishDate, KeyWords, Pages, ISBN, Genre], book(Id, Name, Author, Publisher, PublishDate, KeyWords, Pages, ISBN, Genre), List),
    predsort(nthcompare(7), List, SortedList), %% predsort, zoraduje pole podla toho ci predikat nthcompare vracia true ablebo false

    print_books(SortedList).

%% Predikat vrati True ablebo false podla toho ci n-ty prvok v prvom liste je mensi ako n-ty prvok v druhom liste 
%% nthcompare (+Number, +Operand, +ListA, +ListB)
nthcompare(N,<,A,B) :- nth1(N,A,X),nth1(N,B,Y), X @< Y. %% @< sluzi na porovnanie termov nie vyrazov
nthcompare(_,>,_,_).

%% vymeni MM-RR datum na RR-MM datum, aby sa dalo pole spravne zoradit podla datumu vydania
%% swapdates(+List, -List)
swapdates(List, Out):-
    nth0(4, List, X),
    atomic_list_concat(Y, '-', X),
    nth0(0, Y, Month),
    nth0(1, Y, Year),
    atomic_list_concat([Year, Month], '-', YM),
    select(X, List, YM, Out).

%% vymeni datumy vo vsetkych knihach
%% process_swapdates(+List, -List)
process_swapdates([],[]).
process_swapdates([H|T], [S|Out]):-
    swapdates(H, S),
    process_swapdates(T, Out).

%% vrati list zanrov knih
%% get_list_of_genres(+List, -List)
get_list_of_genres([],[]).
get_list_of_genres([H|T], [G|T1]):-
    nth0(8, H, G),
    get_list_of_genres(T, T1).

%% zo vstupneho listu vrati mnozinu prvkov
%% get_set(+List, -List)
get_set([],[]).
get_set([H|T], T1):-
    member(H, T), !,
    get_set(T, T1).

get_set([H|T], [H|T1]):-
    get_set(T, T1).

%% vypise na obrazovku, knihy zoskupene podla zanrov
%% print_by_genres(+List)
print_by_genres([H|T]):-
    write(H),
    writeln(':'),
    writeln('-------------'),
    findall([Id, Name, Author, Publisher, PublishDate, KeyWords, Pages, ISBN, H], book(Id, Name, Author, Publisher, PublishDate, KeyWords, Pages, ISBN, H), List),
    print_books(List),
    print_by_genres(T).

%% zoskupi knihy podla ich zanrov a vypise ich
group_by_genre:-
    findall([Id, Name, Author, Publisher, PublishDate, KeyWords, Pages, ISBN, Genre], book(Id, Name, Author, Publisher, PublishDate, KeyWords, Pages, ISBN, Genre), List),
    get_list_of_genres(List, Genres),
    get_set(Genres, S),
    print_by_genres(S).