# $Id: Makefile,v 1.1 2003/02/26 23:43:35 steve Exp $
#
#	Development Makefile for the faux product queue implementation
#
include ../macros.make

XML_INC		=
INCLUDES	= -I../config -I. -I../pq -I../misc -I../ulog -I../protocol \
		  $(XML_INC)
LDLIBS		= -lm
LIBRARY		= ../libldm.a
MANUAL		= fauxPq.3
LIB_CSRCS	= \
	fauxPq.c \
	fauxPqConfigFile.c
PACKING_LIST = \
	$(LIB_CSRCS) \
	depends \
	fauxPqConfigFile.h \
	Makefile \
	pq.xml

all:	archived_files

install: 	$(ETCDIR)/pq.xml

$(ETCDIR)/pq.xml: $(ETCDIR) pq.xml
	-@test -f $@ || ($(INSTALL) pq.xml $@ && echo 'updated $@')

include ../rules.make
include depends
