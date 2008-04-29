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

ifeq ($(BUILD_CAMOMILE),yes)

NAME       = gettext-camomile
CMO        =       
LIBRARY    = true
REQUIRES   = gettext.extension camomile
PREDICATES =
INSTALLIB  = META

all:

##########################
# Ocaml Gettext Camomile #
##########################

REQUIRES += camomile

CMO_CAMOMILE=         \
 gettextCamomile.cmo

INSTALLIB +=          \
 gettextCamomile.cma  \
 gettextCamomile.cmxa \
 gettextCamomile.a    \
 gettextCamomile.cmi

INSTALLIB += $(CMO_CAMOMILE:.cmo=.cmx)

gettextCamomile.cmo: gettextCamomile.cmi
gettextCamomile.cmx: gettextCamomile.cmi
gettextCamomile.cma: $(CMO_CAMOMILE)
gettextCamomile.cmxa: $(CMO_CAMOMILE:.cmo=.cmx)

endif

include ../TopMakefile
