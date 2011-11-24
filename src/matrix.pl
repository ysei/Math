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
:- module(matrix, []).


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


%% mat_mult(+A, +B, -C)

mat_mult(A, B, C) :-
        transpose(B, BT),
        mat_mult1(A, BT, C).

mat_mult1([], _, []).
mat_mult1([V|Vs], B, [X|Xs]) :-
        mat_mult2(B, V, X),
        mat_mult1(Vs, B, Xs).

mat_mult2([], _, []).
mat_mult2([V|Vs], Y, [Z|Zs]) :-
        vec_dot(V, Y, Z),
        mat_mult2(Vs, Y, Zs).


%% transpose(+Matrix, -Transpose)

transpose([], []).
transpose([Row|Rows], Cols) :-
        transpose(Row, [Row|Rows], Cols).

transpose([], _, []).
transpose([_|Elems], Rows0, [Col|Cols]) :-
        tr_first(Rows0, Col, Rows1),
        transpose(Elems, Rows1, Cols).

tr_first([], [], []).
tr_first([Row0|Rows0], [Elem|Elems], [Row1|Rows1]) :-
        Row0 = [Elem|Row1],
        tr_first(Rows0, Elems, Rows1).


:- begin_tests(matrix).

test(vec_sum) :-
        vec_sum([1,2,3], [1,2,3], [2,4,6]).

test(vec_prod) :-
        vec_prod([1,2,3], [1,2,3], [1,4,9]).

test(vec_dot) :-
        vec_dot([1,2,3], [1,2,3], 14).

test(transpose) :-
        transpose([[1,2,3], [a,b,c]], [[1,a], [2,b], [3,c]]).

:- end_tests(matrix).