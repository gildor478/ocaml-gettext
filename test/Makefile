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

REQUIRES   = fileutils gettext.extension gettext-camomile gettext-stub
TESTDIR    = tests
CMO        = common.cmo 
ifeq ($(BUILD_BENCH),yes)
NAME       = bench
CMO       += bench.cmo
REQUIRES  += benchmark
else 
ifeq ($(BUILD_TEST),yes)
NAME       = test
CMO       += test.cmo
REQUIRES  += oUnit
endif
endif

include ../ConfMakefile

ifneq ($(DONT_RECURSE),yes)
all:
	$(MAKE) all BUILD_BENCH=$(BUILD_BENCH) DONT_RECURSE=yes
	$(MAKE) all BUILD_TEST=$(BUILD_TEST) DONT_RECURSE=yes

install:
	$(MAKE) install BUILD_BENCH=$(BUILD_BENCH) DONT_RECURSE=yes
	$(MAKE) install BUILD_TEST=$(BUILD_TEST) DONT_RECURSE=yes

uninstall: 
	$(MAKE) uninstall BUILD_BENCH=$(BUILD_BENCH) DONT_RECURSE=yes
	$(MAKE) uninstall BUILD_TEST=$(BUILD_TEST) DONT_RECURSE=yes

clean:: 
	-$(MAKE) clean BUILD_BENCH=$(BUILD_BENCH) DONT_RECURSE=yes
	-$(MAKE) clean BUILD_TEST=$(BUILD_TEST) DONT_RECURSE=yes
	-$(RM) -r $(TESTDIR)

distclean::

endif
	
include ../TopMakefile
