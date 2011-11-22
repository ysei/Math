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

test(transpose) :-
        transpose([[1,2,3], [a,b,c]], [[1,a], [2,b], [3,c]]).

:- end_tests(matrix).