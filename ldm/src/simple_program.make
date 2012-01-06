# $Id: simple_program.make,v 1.2.18.2 2005/10/05 16:59:09 steve Exp $

INCLUDES	= -I../config -I../misc -I../ulog -I../protocol -I../pq
LDLIBS	= -lm
C_PROGRAMS	= $(PROGRAM)
MANUALS	= $(PROGRAM).1
PACKING_LIST	= \
	$(PROGRAM).1 \
	$(PROGRAM).c \
	depends \
	Makefile

all:	$(PROGRAM)

install: installed_programs installed_manuals

$(PROGRAM): $(PROGRAM).o $(LIBRARY)
	$(CC) -o $@ $(CFLAGS) $(PROGRAM).o $(LIBS)

lint:	$(PROGRAM).ln
	$(LINT) -x $(LINTFLAGS) $(CPPFLAGS) $(PROGRAM).ln \
	    -L../protocol -lprotocol -L../misc -lmisc
