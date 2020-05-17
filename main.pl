:- dynamic book/8.

main:-
    writeln('Vitajte v databaze knih'),
    read_from_file('db'),
    repeat,
    menu,
    get(I),
    execute(I),
    I == 57,
    writeln('Koniec').

menu:-
    nl,
    writeln('1 - Pridat knihu'),
    writeln('2 - Vyradit knihu'),
    writeln('3 - Vyhladat knihu alebo skupinu knih'),
    writeln('4 - Zoradit knihy'),
    writeln('9 - Ukoncit program'),
    nl.

findmenu:-
    nl,
    writeln('Moznosti vyhladania:'),
    writeln('1 - ID'),
    writeln('2 - Nazov knihy'),
    writeln('3 - Meno autora'),
    writeln('4 - Klucovych slov'),
    writeln('9 - Spat na menu'),
    nl.

execute(49):- addbook(), !.
execute(50):- removebook(), !.
execute(51):- findbook(), !.
execute(57):- !.

addbook():-
    writeln('Identifikacne cislo:'),
    read(Id),
    writeln('Nazov knihy: '),
    read(Name),
    writeln('Meno autora: '),
    read(Author),
    writeln('Vydavatelstvo: '),
    read(Publisher),
    writeln('Datum vydania (MM-RR): '),
    read(PublishDate),
    writeln('Zoznam klucovych slov: '),
    read(KeyWords),
    writeln('Pocet stran: '),
    read(Pages),
    writeln('ISBN: '),
    read(ISBN),

    %book(Id, Name, Author, Publisher, PublishDate, KeyWords, Pages, ISBN),
    assertz(book(Id, Name, Author, Publisher, PublishDate, KeyWords, Pages, ISBN)),
    write_to_file('db').

removebook():-
    writeln('Identifikacne cislo:'),
    read(Id),
    writeln('ISBN: '),
    read(ISBN),
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


findbook():-
    repeat,
    findmenu,
    get(I),
    exc_filter(I),
    I == 57,
    writeln('Navrat do menu').

exc_filter(49):- filter(49), !.
exc_filter(50):- filter(50), !.
exc_filter(51):- filter(51), !.
exc_filter(57):- !.

filter(49):-
    writeln('Zadaj ID:'),
    read(Id),
    findall([Id, Name, Author, Publisher, PublishDate, KeyWords, Pages, ISBN], book(Id, Name, Author, Publisher, PublishDate, KeyWords, Pages, ISBN), List),
    writeln('-------------'),
    print_books(List),
    fail.

filter(50):-
    writeln('Zadaj nazov knihy:'),
    read(Name),
    writeln(book(_,Name,_,_,_,_,_,_)),
    fail.

filter(51):-
    writeln('Zadaj meno autora:'),
    read(Author),
    writeln(book(_,_,Author,_,_,_,_,_)),
    fail.

auto_increment():-
    findall(Id, book(Id,_,_,_,_,_,_,_), List),
    writeln(List).

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