:- module(plep,[main/0]).

:- use_module(library(optparse),[opt_parse/5]).
:- use_module(library(charsio),[read_term_from_chars/3]).
:- use_module(library(dialect/sicstus),[use_module/3]). % Wat.

:- dynamic option/1.
:- dynamic operator_module/1.

main :-
  current_prolog_flag(argv,Argv),
  opt_parse([[opt(module),
              type(atom),
              meta('MODULE'),
              help('A module specification to use for operators, in order to be able to parse the term and the files to search in.'),
              shortflags([m]),
              longflags([module])],
             [opt(period),
              type(boolean),
              help('If this option is given, each term will be suffixed by a period so the output can be read in as Prolog source.'),
              shortflags([p]),
              longflags([period])],
             [opt(encoding),
              type(atom),
              meta('ENCODING'),
              help('The character encoding of the input file, such as latin1 or utf8.'),
              shortflags([e]),
              longflags([encoding])]],
                Argv,
                Options,
                PositionalArguments),
  assert_options(Options),
  use_operator_module,
  process_positional_arguments(PositionalArguments,Term,Files),
  plep(Files,Term),
  halt.
main :-
  halt(1).

assert_options([]).
assert_options([Option|Options]) :-
  assert(option(Option)),
  assert_options(Options).

process_positional_arguments([_,TermAtom|Files],Term,Files) :-
  !,
  atom_chars(TermAtom,TermCodes),
  read_term_options_list(RTOL),
  read_term_from_chars(TermCodes,Term,RTOL).
process_positional_arguments(_,_,_) :-
  format(user_error,'ERROR: At least one argument is required: TERM [FILE...]~n',[]),
  fail.

plep([],_).
plep([File|Files],Term) :-
  plep_file(File,Term),
  plep(Files,Term).

plep_file(File,Term) :-
  open_options_list(Options),
  open(File,read,Stream,Options), % TODO handle file non-existence, maybe by trying to suffix filename with .pl?
  plep_stream(Stream,Term),
  close(Stream).

plep_stream(Stream,SearchTerm) :-
  read_term_options_list(RTOL),
  read_term(Stream,ReadTerm,RTOL),
  plep_term(ReadTerm,SearchTerm),
  plep_stream_continue(ReadTerm,Stream,SearchTerm).

read_term_options_list([module(Module)]) :-
  operator_module(Module),
  !.
read_term_options_list([]).

open_options_list([encoding(Encoding)]) :-
  option(encoding(Encoding)),
  !.
open_options_list([]).

use_operator_module :-
  option(module(File)),
  nonvar(File),
  !,
  use_module(Module,File,[]),
  assert(operator_module(Module)).
use_operator_module.

plep_stream_continue(end_of_file,_,_) :-
  !.
plep_stream_continue(_,Stream,SearchTerm) :-
  plep_stream(Stream,SearchTerm).

% If the read term can unify with the search term, print it, otherwise recurse.
plep_term(ReadTerm,SearchTerm) :-
  \+ \+ (ReadTerm = SearchTerm),
  !,
  write_term(ReadTerm,[quoted(true)]), % TODO prefix with filename if requested
  write_period_if_requested,
  nl.
plep_term(ReadTerm,SearchTerm) :-
  ReadTerm =.. [_|Terms],
  plep_terms(Terms,SearchTerm).

write_period_if_requested :-
  option(period(true)),
  !,
  write('.').
write_period_if_requested.

plep_terms([],_).
plep_terms([Term|Terms],SearchTerm) :-
  plep_term(Term,SearchTerm),
  plep_terms(Terms,SearchTerm).
