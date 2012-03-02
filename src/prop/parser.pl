/*
  Copyright 2011 Jose Sebastian Reguera Candal

  This program is free software: you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation, either version 3 of the License, or
  (at your option) any later version.

  This program is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with this program.  If not, see <http://www.gnu.org/licenses/>.
*/
:- module(parser, []).

parse(Tokens, AST) :-
        phrase(prop(AST), Tokens).

prop(P) -->
        eq(P).

eq(P) -->
        binop(eq, imp, P).

imp(P) -->
        binop(imp, or, P).

or(P) -->
        binop(or, and, P).

and(P) -->
        binop(and, factor, P).

binop(Op, NextRule, Y) -->
        call(NextRule, X),
        binop(Op, NextRule, X, Y).

binop(Op, NextRule, X, Y) -->
        binop(Op), !,
        call(NextRule, Z),
        { G =.. [Op, X, Z] },
        binop(Op, NextRule, G, Y).
binop(_, _, X, X) -->
        [].

factor(not(P)) -->
        [-], !, factor(P).
factor(P) -->
        ['('], !, prop(P), [')'].
factor(1) -->
        ['1'], !.
factor(0) -->
        ['0'], !.
factor(var(P)) -->
        [P], { char_type(P, lower) }, !.

binop(eq) -->
        [<, -, >].
binop(imp) -->
        [-, >].
binop(or) -->
        [\, /].
binop(and) -->
        [/, \].

:- begin_tests(parser).

test(parse) :-
        parse([-, p], not(var(p))).

test(parse) :-
        parse(['1', /, \, '1', /, \, '0'], and(and(1, 1), 0)).

test(parse) :-
        parse(['1', -, >, '1', /, \, '0'], imp(1, and(1, 0))).

:- end_tests(parser).