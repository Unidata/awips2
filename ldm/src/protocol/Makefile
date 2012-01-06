# $Id: Makefile,v 5.3.12.7 2008/09/22 16:31:20 steve Exp $
#
#	Makefile for ldm ONC RPC protocol objects

include ../macros.make

INCLUDES = -I../config -I. -I../misc -I../ulog -I../protocol -I../pq -I../server

LIBRARY		= ../libldm.a
LINT_LIBRARY	= protocol
HEADERS		= \
	atofeedt.h \
	autoshift.h \
	globals.h \
	h_clnt.h \
	ldm.h \
	ldm5_clnt.h \
	ldmprint.h \
	ldm_clnt.h \
	ldm_xlen.h \
	md5.h \
	prod_class.h \
	savedInfo.h \
	timestamp.h \
	xdr_data.h

LIB_CSRCS	= \
	atofeedt.c \
	autoshift.c \
	globals.c \
	h_clnt.c \
	ldm4_svc.c \
	ldm5_svc.c \
	ldm_xdr.c \
	ldm_xlen.c \
	ldm5_clnt.c \
	ldm6_clnt.c \
	ldmprint.c \
	ldm_clnt.c \
	md5c.c \
	one_svc_run.c \
	prod_info.c \
	prod_class.c \
	savedInfo.c \
	timestamp.c \
	xdr_data.c

PACKING_LIST	= \
	atofeedt.c \
	atofeedt.h \
	autoshift.c \
	autoshift.h \
	depends \
	fix_clnt.pl \
	gen.mk \
	globals.c \
	globals.h \
	h_clnt.c \
	h_clnt.h \
	ldm.h \
	ldm.x \
	ldm4.h \
	ldm4_svc.c \
	ldm5.h \
	ldm5_clnt.c \
	ldm5_clnt.h \
	ldm6_clnt.c \
	ldm5_svc.c \
	ldmprint.c \
	ldmprint.h \
	ldm_clnt.c \
	ldm_clnt.h \
	ldm_xdr.c \
	ldm_xlen.c \
	ldm_xlen.h \
	Makefile \
	md5.h \
	md5c.c \
	one_svc_run.c \
	prod_info.c \
	prod_info.h \
	prod_class.c \
	prod_class.h \
	rsaglobal.h \
	savedInfo.c \
	savedInfo.h \
	timestamp.c \
	timestamp.h \
	xdr_data.c \
	xdr_data.h

TAG_SRCS	= \
	../server/*.c ../server/*.h \
	../pq/*.c ../pq/*.h \
	../ulog/*.c ../ulog/*.h \
	../misc/*.c ../misc/*.h

all:	ldm_xdr.c archived_files

install:	installed_headers

include ../rules.make

# The "-C" option in the following causes Standard C function declarations.
# This allow the compiler to catch improper use.
#
ldm.h: ldm.x
	$(RPCGEN) -h ldm.x | \
	sed 's/typedef *char *signaturet/typedef unsigned char signaturet/' > $@

ldm_xdr.c: ldm.x
	(echo '#include "ldmconfig.h"'; $(RPCGEN) -c ldm.x) | \
	sed \
	  -e 's/xdr_opaque *( *xdrs, *objp, *16)/xdr_opaque(xdrs, (char*)objp, 16)/' \
	  -e '/#if defined(_LP64)/,/#endif/d' > $@

# The client-side code created by rpcgen(1) must be modified to ensure that
# certain functions use batched RPC by having: 
#   1) a NULL XDR function for the return value;
#   2) a zero timeout; and (just to make sure)
#   3) a NULL pointer for the return value.
# #3 isn't necessary according to our RPC 4.0 source code
# <file:///opt/rpcsrc_40/rpc/clnt_tcp.c> but SUN's ONC+ Developer's Guide
# <http://docs.sun.com/db?p=/doc/802-1997> shows this in its examples (of
# course, it also uses "xdr_void" rather than NULL for the return-value
# XDR-routine (sheesh!)).
#
ldm6_clnt.c: ldm.x
	(echo '#include "ldmconfig.h"'; $(RPCGEN) -l ldm.x) > $@.tmp || \
	    rm $@.tmp
	perl fix_clnt.pl < $@.tmp > $@ && rm $@.tmp       # fix stuff

lint:	$(GARBAGE) llib-l$(LINT_LIBRARY).ln

include depends
