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
:- module(matrix,
          [ prod/3,             % +A:matrix, +B:matrix, -C:matrix
            transpose/2         % +M:matrix, -T:matrix
          ]).

:- use_module('vector.pl',
              [ dot/3 as vec_dot
              ]).


%% prod(+A:matrix, +B:matrix, -C:matrix)

prod(A, B, C) :-
        transpose(B, BT),
        prod1(A, BT, C).

prod1([], _, []).
prod1([Ar|Ars], BT, [Cr|Crs]) :-
        prod2(BT, Ar, Cr),
        prod1(Ars, BT, Crs).

prod2([], _, []).
prod2([Bc|Bcs], Ar, [Cre|Cres]) :-
        vec_dot(Bc, Ar, Cre),
        prod2(Bcs, Ar, Cres).


%% transpose(+M:matrix, -T:matrix)

transpose([], []).
transpose([Row|Rows], Cols) :-
        transpose(Row, [Row|Rows], Cols).

transpose([], _, []).
transpose([_|Elems], Rows0, [Col|Cols]) :-
        split_first_column(Rows0, Col, Rows1),
        transpose(Elems, Rows1, Cols).

split_first_column([], [], []).
split_first_column([Row0|Rows0], [Elem|Elems], [Row1|Rows1]) :-
        Row0 = [Elem|Row1],
        split_first_column(Rows0, Elems, Rows1).


:- begin_tests(matrix).

test(transpose) :-
        transpose([[1,2,3], [a,b,c]], [[1,a], [2,b], [3,c]]).

:- end_tests(matrix).