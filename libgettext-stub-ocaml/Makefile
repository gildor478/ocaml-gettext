##########################################################################
#  Ocaml-gettext : a library to translate messages                       #
#                                                                        #
#  Copyright (C) 2003, 2004, 2005 Sylvain Le Gall <sylvain@le-gall.net>  #
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
#                                                                        #
#  Contact: sylvain@le-gall.net                                          #
##########################################################################

NAME       = gettext-stub
CMO        =       
LIBRARY    = true
REQUIRES   = gettext.extension
PREDICATES =
INSTALLIB  = META

all:

distclean::
	-$(RM) META

##############################
# Stub library               #
##############################

INSTALLIB +=               \
 gettextStub.cmxa          \
 gettextStub.cma           \
 gettextStub.a             \
 dllgettextStub.so         \
 libgettextStub.a          \
 gettextStub.cmi           \
 gettextStubCompat.cmi     

CMO_STUB =                 \
 gettextStubCompat_stubs.o \
 gettextStubCompat.cmo     \
 gettextStub.cmo 

gettextStubCompat.cmo: gettextStubCompat.cmi
gettextStubCompat.cmx: gettextStubCompat.cmi
gettextStub.cma: $(CMO_STUB)
gettextStub.cmxa: $(CMO_STUB:.cmo=.cmx)
gettextStub.cma gettextStub.cmxa: STUBSOBJS = -L$(OCAMLLIB) -lcamlidl

clean::
	-$(RM) gettextStubCompat.ml gettextStubCompat.mli 
	-$(RM) gettextStubCompat.h gettextStubCompat_stubs.c 

include ../TopMakefile
