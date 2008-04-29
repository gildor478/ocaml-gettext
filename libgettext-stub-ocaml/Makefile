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

include ../ConfMakefile

ifeq ($(BUILD_STUB),yes)

NAME       = gettext-stub
CMO        =       
LIBRARY    = true
REQUIRES   = gettext.extension
PREDICATES =
INSTALLIB  = META
INCLUDES   = $(STUB_CFLAGS)

all:

##############################
# Stub library               #
##############################

CMO_STUB =                 \
 gettextStubCompat_stubs.o \
 gettextStubCompat.cmo     \
 gettextStub.cmo 

INSTALLIB +=               \
 gettextStub.cmxa          \
 gettextStub.cma           \
 gettextStub.a             \
 dllgettextStub.so         \
 libgettextStub.a          \
 gettextStub.cmi           \
 gettextStubCompat.cmi     

INSTALLIB += $(CMO_STUB:.cmo=.cmx)

gettextStub.cma: $(CMO_STUB)
gettextStub.cmxa: $(CMO_STUB:.cmo=.cmx)
gettextStub.cma gettextStub.cmxa: STUBSOBJS = yes 
gettextStub.cma gettextStub.cmxa: OCAMLMKLIB_FLAGS += $(STUB_LDFLAGS)

clean::
	-$(RM) gettextStub.mli
	-$(RM) gettextStubCompat.mli

endif

include ../TopMakefile
