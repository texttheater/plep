PROLOG=swipl

bin/plep: src/prolog/plep.pl
	mkdir -p bin
	$(PROLOG) -g "['src/prolog/plep'], qsave_program('bin/plep',[goal=main,stand_alone=true]), halt."

