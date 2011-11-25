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
:- module(vector,
          [ sum/3,              % +X:vector, +Y:vector, -Z:vector
            prod/3,             % +X:vector, +Y:vector, -Z:vector
            dot/3               % +X:vector, +Y:vector, -Z:scalar
          ]).


%% sum(+X:vector, +Y:vector, -Z:vector)

sum([], [], []).
sum([X|Xs], [Y|Ys], [Z|Zs]) :-
        Z is X + Y,
        sum(Xs, Ys, Zs).


%% prod(+X:vector, +Y:vector, -Z:vector)

prod([], [], []).
prod([X|Xs], [Y|Ys], [Z|Zs]) :-
        Z is X * Y,
        prod(Xs, Ys, Zs).


%% dot(+X:vector, +Y:vector, -Z:scalar)

dot(X, Y, Z) :-
        dot(X, Y, 0, Z).

dot([], [], Z, Z).
dot([X|Xs], [Y|Ys], Z0, Z) :-
        Z1 is X * Y + Z0,
        dot(Xs, Ys, Z1, Z).


:- begin_tests(vector).

test(sum) :-
        sum([1,2,3], [1,2,3], [2,4,6]).

test(prod) :-
        prod([1,2,3], [1,2,3], [1,4,9]).

test(dot) :-
        dot([1,2,3], [1,2,3], 14).

:- end_tests(vector).
