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
:- module(nat, []).

nat(0).
nat(s(N)) :-
        nat(N).

sum(0, M, M) :-
        nat(M).
sum(s(N), M, s(R)) :-
        sum(N, M, R).

prod(0, M, 0) :-
        nat(M).
prod(s(N), M, P) :-
        prod(N, M, P0),
        sum(M, P0, P).

less_eq(N, M) :-
        sum(N, _, M).

:- begin_tests(nat).

test(zero_is_nat) :-
        nat(0).

test(two_is_nat) :-
        nat(s(s(0))).

test(sum_2_1_is_3) :-
        sum(s(s(0)), s(0), s(s(s(0)))).

test(prod_2_3_is_6) :-
        prod(s(s(0)), s(s(s(0))), s(s(s(s(s(s(0))))))).

:- end_tests(nat).