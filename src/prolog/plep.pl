% plep.pl
% This file is part of plep -- grep for Prolog terms
% Copyright (C) 2012 Kilian Evang

% This program is free software; you can redistribute it and/or
% modify it under the terms of the GNU Lesser General Public
% License as published by the Free Software Foundation; either
% version 3 of the License, or (at your option) any later version.

% This program is distributed in the hope that it will be useful,
% but WITHOUT ANY WARRANTY; without even the implied warranty of
% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
% Lesser General Public License for more details.

% You should have received a copy of the GNU Lesser General
% Public License along with this program; if not, write to the
% Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
% Boston, MA 02110-1301 USA

:- module(plep,[main/0]).

:- use_module(library(optparse),[opt_parse/5]).
:- use_module(library(charsio),[read_term_from_chars/3]).

main :-
  current_prolog_flag(argv,Argv),
  opt_parse([[opt(module),
                  type(atom),
                  meta('MODULE'),
                  help('A module specification to use for operators, in order to be able to parse the term and the files to search in.'),
                  shortflags([m]),
                  longflags([module])]],
                Argv,
                Options,
                PositionalArguments),
  process_options(Options,ReadTermOptionsList),
  process_positional_arguments(PositionalArguments,ReadTermOptionsList,Term,Files),
  plep(Files,Term,ReadTermOptionsList),
  halt.
main :-
  halt(1).

process_options([],[]).
process_options([module(Module)|_],[module(Module)]) :-
  nonvar(Module),
  !.
process_options([_|Rest],ReadTermOptionsList) :-
  process_options(Rest,ReadTermOptionsList).

process_positional_arguments([_,TermAtom|Files],ReadTermOptionsList,Term,Files) :-
  !,
  atom_chars(TermAtom,TermCodes),
  read_term_from_chars(TermCodes,Term,ReadTermOptionsList).
process_positional_arguments(_,_,_,_) :-
  format(user_error,'ERROR: At least one argument is required: TERM [FILE...]~n',[]),
  fail.

plep([],_,_).
plep([File|Files],Term,RTOL) :-
  plep_file(File,Term,RTOL),
  plep(Files,Term,RTOL).

plep_file(File,Term,RTOL) :-
  open(File,read,Stream), % TODO handle file non-existence, maybe by trying to suffix filename with .pl?
  plep_stream(Stream,Term,RTOL),
  close(Stream).

plep_stream(Stream,SearchTerm,RTOL) :-
  read_term(Stream,ReadTerm,RTOL),
  plep_term(ReadTerm,SearchTerm),
  plep_stream_continue(ReadTerm,Stream,SearchTerm,RTOL).

plep_stream_continue(end_of_file,_,_,_) :-
  !.
plep_stream_continue(_,Stream,SearchTerm,RTOL) :-
  plep_stream(Stream,SearchTerm,RTOL).

% If the read term can unify with the search term, print it, otherwise recurse.
plep_term(ReadTerm,SearchTerm) :-
  \+ \+ (ReadTerm = SearchTerm),
  !,
  write(ReadTerm), % TODO prefix with filename if requested
  nl.
plep_term(ReadTerm,SearchTerm) :-
  ReadTerm =.. [_|Terms],
  plep_terms(Terms,SearchTerm).

plep_terms([],_).
plep_terms([Term|Terms],SearchTerm) :-
  plep_term(Term,SearchTerm),
  plep_terms(Terms,SearchTerm).
