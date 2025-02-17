##########################################################################
#  ocaml-gettext: a library to translate messages                        #
#                                                                        #
#  Copyright (C) 2003-2008 Sylvain Le Gall <sylvain@le-gall.net>         #
#                                                                        #
#  This library is free software; you can redistribute it and/or         #
#  modify it under the terms of the GNU Lesser General Public            #
#  License as published by the Free Software Foundation; either          #
#  version 2.1 of the License, or (at your option) any later version;    #
#  with the OCaml static compilation exception.                          #
#                                                                        #
#  This library is distributed in the hope that it will be useful,       #
#  but WITHOUT ANY WARRANTY; without even the implied warranty of        #
#  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU     #
#  Lesser General Public License for more details.                       #
#                                                                        #
#  You should have received a copy of the GNU Lesser General Public      #
#  License along with this library; if not, write to the Free Software   #
#  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307   #
#  USA                                                                   #
##########################################################################

default: build test

build:
	dune build @install

doc:
	dune build @doc

test:
	dune runtest

all:
	dune build @all

install:
	dune install

uninstall:
	dune uninstall

clean:
	dune clean

fmt:
	dune fmt

lint:
	opam-dune-lint
	dune build @fmt

bench:
	dune exec --workspace dune-workspace.dev \
		test/bench/bench.exe -- --test_dir ./test/testdata

headache: distclean
	headache -h .header \
		-c .headache.config \
		`find $(CURDIR)/ -type d -name .svn -prune -false -o -type f`

git-pre-commit-hook: test lint

deploy: doc test
	dune-release lint
	dune-release tag
	git push --all
	git push --tag
	dune-release

eol:
	find ./ -name _build -prune -false -or -name "*.ml" | xargs grep -r -e "  *$$"


export OCAML_GETTEXT=$(readlink -f _build/default/src/bin/ocaml-gettext/OCamlGettext.exe)
export OCAML_XGETTEXT=$(readlink -f _build/default/src/bin/ocaml-xgettext/xgettext.exe)
update-po: build
	$(MAKE) -C examples/po all
	$(MAKE) -C po all

.PHONY: build doc test all uninstall clean install bench deploy lint fmt update-po

