king(richard).
king(henry).

brother(richard, john).
brother(john, richard).

crown(richard, head).

person(richard).
person(john).

if_king_then_person(A) :- king(A).
ifktp(A) :- king(A), person(A).

is_king(A) :- king(A).
is_person(A) :- person(A).
are_same(A, B) :- person(A) == person(B).
