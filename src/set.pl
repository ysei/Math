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
:- module(set, []).


%% subset(Set, Subset)
%
% Subset is a subset of Set.
%
% Notes:
% The (+Set, +Subset) mode only works for ordered sets.
% Clause order in the predicate is significant for the (-Set, +Subset) mode:
% ([], []) goes first to avoid infinite recursion.

subset([], []).
subset([X|Y], [X|Z]) :-
        subset(Y, Z).
subset([_|Y], Z) :-
        subset(Y, Z).


%% powerset(+Set, -Powerset)
%
% Powerset is the powerset of Set.

powerset(Set, Powerset) :-
        setof(Subset, subset(Set, Subset), Powerset).
