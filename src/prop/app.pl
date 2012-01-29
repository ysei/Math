/*
  Copyright 2012 Jose Sebastian Reguera Candal

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
:- use_module(library('http/thread_httpd')).
:- use_module(library('http/html_write')).
:- use_module(library('http/http_parameters')).

server :-
        Port = 8000,
        http_server(reply, [port(Port)]).

reply(Request) :-
        format('Content-type: text/html~n~n', []),
        http_parameters(Request, [query(Data, [default(0)])]),
        atom_to_term(Data, Prop, _),
        phrase(generate_reply(Request, Data, Prop), Html_Tokens),
        print_html(Html_Tokens).

generate_reply(Request, Data, Prop) -->
        page([title(['Prop'])],
             [h2(['Prop']),
              table(\request_rows(Request)),
              p(Data),
              \query_form,
              math([mrow(\prop(Prop))])]).

query_form -->
              html(form([method(get), action('prop')],
                        [input([type(text), name('query')], []),
                         input([type(submit)], [])])).

request_rows([]) -->
        [].
request_rows([H|T]) -->
        { term_to_atom(H, A) },
        html(tr([td(A)])),
        request_rows(T).

prop(eq(P, Q)) -->
        prop(P),
        html(mo(&(harr))),
        prop(Q).
prop(imp(P, Q)) -->
        prop(P),
        html(mo(&(rarr))),
        prop(Q).
prop(and(P, Q)) -->
        prop(P),
        html(mo(&(0x2227))),
        prop(Q).
prop(or(P, Q)) -->
        prop(P),
        html(mo(&(0x2228))),
        prop(Q).
prop(not(P)) -->
        html(mo(&(0xAC))),
        prop(P).
prop(var(P)) -->
        html(mi([P])).
prop(0) -->
        html(mi([0])).
prop(1) -->
        html(mi([1])).
