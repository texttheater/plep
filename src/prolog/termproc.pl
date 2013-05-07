% Library for processing Prolog files term by term.
% Usage:
% :- use_module(strmprc,[strmprc/2]).
% :- strmprc(Callback,Arg)
% where Callback is a goal that will be called for each Prolog term read from
% the file specified as the last command-line argument, after binding Arg to
% the term. Only UTF-8 input is currently supported. Processing ends when end of
% file is reached or Callback fails.

:- module(termproc,[
    process_files/3,
    process_files/4,
    process_file/3,
    process_file/4]).

:- meta_predicate
    process_files(+,0,-),
    process_files(+,0,-,+),
    process_file(+,0,-),
    process_file(+,0,-,+).

process_files(Files,Callback,Arg) :-
  process_files(Files,Callback,Arg,[]).

process_files([],_,_,_).
process_files([File|Files],Callback,Arg,Options) :-
  process_file(File,Callback,Arg,Options),
  process_files(Files,Callback,Arg,Options).

process_file(File,Callback,Arg) :-
  process_file(File,Callback,Arg,[]).

process_file(File,Callback,Arg,Options) :-
  open(File,read,Stream,Options),
  process_stream(Stream,Callback,Arg,Options),
  close(Stream).

process_stream(Stream,Callback,Arg,Options) :-
  read_term(Stream,Term,Options),
  continue(Term,Stream,Callback,Arg,Options).

continue(Term,_,_,_,_) :-
  Term == end_of_file,
  !.
continue(Term,Stream,Callback,Arg,Options) :-
  \+ \+ (Term=Arg, Callback),
  process_stream(Stream,Callback,Arg,Options).
