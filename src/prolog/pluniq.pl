% TODO This shares A LOT of code with plep. Factor all the command-line
% processing stuff out to a separate module! The trickiest part will be
% the specific handling of positional arguments.

:- module(pluniq,[main/0]).

:- use_module(library(optparse),[opt_parse/5]).
:- use_module(library(charsio),[read_term_from_chars/3]).
:- use_module(library(dialect/sicstus),[use_module/3]). % Wat.
:- use_module(termproc,[process_files/4]).

:- dynamic option/1.
:- dynamic operator_module/1.
:- dynamic current_term/1.

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
  process_positional_arguments(PositionalArguments,Files),
  read_term_options_list(ReadTermOptions),
  open_options_list(OpenOptions),
  append(ReadTermOptions,OpenOptions,TermprocOptions),
  process_files(Files,pluniq_term(Arg),Arg,TermprocOptions),
  output_current_term_if_any,
  halt.
main :-
  halt(1).

assert_options([]).
assert_options([Option|Options]) :-
  assert(option(Option)),
  assert_options(Options).

process_positional_arguments([_|Files],Files).

read_term_options_list([module(Module)]) :-
  operator_module(Module),
  !.
read_term_options_list([]).

open_options_list([encoding(Encoding)]) :-
  option(encoding(Encoding)),
  nonvar(Encoding),
  !.
open_options_list([]).

use_operator_module :-
  option(module(File)),
  nonvar(File),
  !,
  use_module(Module,File,[]),
  assert(operator_module(Module)).
use_operator_module.

pluniq_term(NewTerm) :-
  current_term(OldTerm),
  subsumes_term(OldTerm,NewTerm),
  !.
pluniq_term(NewTerm) :-
  current_term(OldTerm),
  subsumes_term(NewTerm,OldTerm),
  !,
  retract(current_term(_)),
  assert(current_term(NewTerm)).
pluniq_term(_) :-
  output_current_term_if_any,
  fail.
pluniq_term(NewTerm) :-
  assert(current_term(NewTerm)).

output_current_term_if_any :-
  retract(current_term(Term)),
  !,
  numbervars(Term,0,_),
  write_term(Term,[quoted(true),numbervars(true)]),
  write_period_if_requested,
  nl.
output_current_term_if_any.

write_period_if_requested :-
  option(period(true)),
  !,
  write('.').
write_period_if_requested.
