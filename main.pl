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

execute(49):- addbook(), !.
execute(50):- removebook(), !.
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
        seen;
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