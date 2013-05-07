# Makefile
# This file is part of plep -- grep for Prolog terms
# Copyright (C) 2012 Kilian Evang

# This program is free software; you can redistribute it and/or
# modify it under the terms of the GNU Lesser General Public
# License as published by the Free Software Foundation; either
# version 3 of the License, or (at your option) any later version.

# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
# Lesser General Public License for more details.

# You should have received a copy of the GNU Lesser General
# Public License along with this program; if not, write to the
# Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
# Boston, MA 02110-1301 USA

PROLOG=swipl

bin/plep: src/prolog/plep.pl src/prolog/termproc.pl
	mkdir -p bin
	$(PROLOG) -g "['src/prolog/plep'], qsave_program('bin/plep',[goal=main,stand_alone=true]), halt."

