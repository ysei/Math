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


%% nat(N)
%
% True if N is a Nat. A Nat is a natural number represented as either 0
% or s(N) where N is a Nat.

nat(0).
nat(s(N)) :-
        nat(N).


%% sum(N, M, P)
%
% P is the sum of N and M.

sum(0, M, M) :-
        nat(M).
sum(s(N), M, s(P)) :-
        sum(N, M, P).


%% prod(N, M, P)
%
% P is the product of N and M.

prod(0, M, 0) :-
        nat(M).
prod(s(N), M, P) :-
        prod(N, M, Q),
        sum(M, Q, P).


%% pow(N, M, P)
%
% P is M to the power of N.

pow(0, M, s(0)) :-
        nat(M).
pow(s(N), M, P) :-
        pow(N, M, Q),
        prod(M, Q, P).


%% less_eq(N, M)
%
% N is less than or equal to M.

less_eq(N, M) :-
        sum(N, _, M).


%% nat_to_number(+Nat, -Number) is det.
%
% Convert a Nat to a prolog number.

nat_to_number(0, 0).
nat_to_number(s(N), X) :-
        nat_to_number(N, Y),
        X is Y + 1.


%% number_to_nat(+Number, -Nat) is semidet.
%
% Convert a prolog number to a Nat. Fails if Number is not a natural number.

number_to_nat(0, 0) :-
        !.
number_to_nat(X, s(N)) :-
        X > 0,
        Y is X - 1,
        number_to_nat(Y, N).


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