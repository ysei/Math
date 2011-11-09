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
:- module(cnf, []).


remove_eq(eq(P, Q), and(imp(VP, VQ), imp(VQ, VP))) :-
        remove_eq(P, VP),
        remove_eq(Q, VQ).
remove_eq(imp(P, Q), imp(VP, VQ)) :-
        remove_eq(P, VP),
        remove_eq(Q, VQ).
remove_eq(and(P, Q), and(VP, VQ)) :-
        remove_eq(P, VP),
        remove_eq(Q, VQ).
remove_eq(or(P, Q), or(VP, VQ)) :-
        remove_eq(P, VP),
        remove_eq(Q, VQ).
remove_eq(not(P), not(VP)) :-
        remove_eq(P, VP).
remove_eq(var(P), var(P)).
remove_eq(0, 0).
remove_eq(1, 1).


remove_imp(imp(P, Q), or(not(VP), VQ)) :-
        remove_imp(P, VP),
        remove_imp(Q, VQ).
remove_imp(and(P, Q), and(VP, VQ)) :-
        remove_imp(P, VP),
        remove_imp(Q, VQ).
remove_imp(or(P, Q), or(VP, VQ)) :-
        remove_imp(P, VP),
        remove_imp(Q, VQ).
remove_imp(not(P), not(VP)) :-
        remove_imp(P, VP).
remove_imp(var(P), var(P)).
remove_imp(0, 0).
remove_imp(1, 1).


move_neg(and(P, Q), and(NP, NQ)) :-
        move_neg(P, NP),
        move_neg(Q, NQ).
move_neg(or(P, Q), or(NP, NQ)) :-
        move_neg(P, NP),
        move_neg(Q, NQ).
move_neg(not(P), N) :-
        move_neg_not(P, N).
move_neg(var(P), var(P)).
move_neg(0, 0).
move_neg(1, 1).

move_neg_not(or(P, Q), and(NP, NQ)) :-
        move_neg_not(P, NP),
        move_neg_not(Q, NQ).
move_neg_not(and(P, Q), or(NP, NQ)) :-
        move_neg_not(P, NP),
        move_neg_not(Q, NQ).
move_neg_not(not(P), Q) :-
        move_neg(P, Q).
move_neg_not(var(P), not(var(P))).
move_neg_not(0, 1).
move_neg_not(1, 0).


cnf(P0, P) :-
        remove_eq(P0, P1),
        remove_imp(P1, P).


:- begin_tests(cnf).

test(r) :-
        cnf(eq(var(p), var(q)),
            and(or(not(var(p)), var(q)),
                or(not(var(q)), var(p)))).

:- end_tests(cnf).