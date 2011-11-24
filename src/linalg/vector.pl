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
:- module(vector, [vec_dot/3]).


%% vec_sum(+V1, +V2, -VR)

vec_sum([], [], []).
vec_sum([X|Xs], [Y|Ys], [Z|Zs]) :-
        Z is X + Y,
        vec_sum(Xs, Ys, Zs).


%% vec_prod(+V1, +V2, -VR)

vec_prod([], [], []).
vec_prod([X|Xs], [Y|Ys], [Z|Zs]) :-
        Z is X * Y,
        vec_prod(Xs, Ys, Zs).


%% vec_dot(+V1, +V2, -S)

vec_dot(X, Y, Z) :-
        vec_dot(X, Y, 0, Z).

vec_dot([], [], Z, Z).
vec_dot([X|Xs], [Y|Ys], S0, Z) :-
        S is X * Y + S0,
        vec_dot(Xs, Ys, S, Z).


:- begin_tests(vector).

test(vec_sum) :-
        vec_sum([1,2,3], [1,2,3], [2,4,6]).

test(vec_prod) :-
        vec_prod([1,2,3], [1,2,3], [1,4,9]).

test(vec_dot) :-
        vec_dot([1,2,3], [1,2,3], 14).

:- end_tests(vector).
