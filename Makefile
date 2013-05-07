PROLOG=swipl

bin/plep: src/prolog/plep.pl src/prolog/termproc.pl
	mkdir -p bin
	$(PROLOG) -g "['src/prolog/plep'], qsave_program('bin/plep',[goal=main,stand_alone=true]), halt."

# TODO document
bin/pluniq: src/prolog/pluniq.pl src/prolog/termproc.pl
	mkdir -p bin
	$(PROLOG) -g "['src/prolog/pluniq'], qsave_program('bin/pluniq',[goal=main,stand_alone=true]), halt."

