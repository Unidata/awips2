SHELL=/bin/sh

#  Make sure one of the following options appears in your CFLAGS
#  variable to indicate which system you are on.  Used to call
#  "C" routines from Fortran.
#  -DLINUX, -DLINUXG95, -DSGI, -DHP, -DCRAY90, -DAIX, -DLINUXF90, -DVPP5000

#  If you want to enable support for PNG or JPEG2000 encoding/decoding,
#  you must specify -DUSE_PNG and/or -DUSE_JPEG2000 in the FDEFS variable
#  for the Fortran pre-processor
#  -DUSE_PNG requires libpng.a and libz.a
#                     ( and png.h pngconf.h zconf.h zlib.h include files).
#  -DUSE_JPEG2000 requires libjasper.a
#                     ( and all the jasper/*.h include files).
#
#  In addition, INCDIR must include all directories where the above
#  mentioned include files can be found.
FDEFS=-DUSE_PNG -DUSE_JPEG2000
INCDIR=-I/home/brockwoo/workspace/ncepLib/org.ncep.grib/include

LIB=libg2.a

#--------------------------------------
# The following was used for XLF on AIX
#DEFS=-DAIX -DHAVE_SYS_TYPES_H=1
#FC=xlf
#CC=xlc
#CPP=/usr/ccs/lib/cpp -P
#MODDIR=../g2mod
#FFLAGS=-O3 -g -qnosave -qarch=auto -qmoddir=$(MODDIR) -I $(MODDIR)
#CFLAGS=-O3 -q64 -g -qcpluscmt -qarch=auto $(DEFS) $(INCDIR)
#ARFLAGS=-X64
#--------------------------------------
# The following was used for G95 on LINUX
#
DEFS=-DLINUX
FC=gfortran
CC=gcc
CPP=cpp -P -C
MODDIR=.
FFLAGS=-O2 -g -I $(MODDIR)
#
#----- used with 32-bit machine  ---
#
CFLAGS=-O3 $(DEFS) $(INCDIR)
#
#----- used with 64-bit machine  ---
#
# CFLAGS=-O3 $(DEFS) $(INCDIR) -D__64BIT__
#
ARFLAGS=
#--------------------------------------

.SUFFIXES: .a .f .F .c

$(LIB):	$(LIB)(gridtemplates.o) \
	$(LIB)(pdstemplates.o) \
	$(LIB)(drstemplates.o) \
	$(LIB)(gribmod.o) \
	$(LIB)(realloc.o) \
	$(LIB)(addfield.o) \
	$(LIB)(addgrid.o) \
	$(LIB)(addlocal.o) \
	$(LIB)(getfield.o) \
	$(LIB)(gb_info.o) \
	$(LIB)(gf_getfld.o) \
	$(LIB)(gf_free.o) \
	$(LIB)(gf_unpack1.o) \
	$(LIB)(gf_unpack2.o) \
	$(LIB)(gf_unpack3.o) \
	$(LIB)(gf_unpack4.o) \
	$(LIB)(gf_unpack5.o) \
	$(LIB)(gf_unpack6.o) \
	$(LIB)(gf_unpack7.o) \
	$(LIB)(gettemplates.o) \
	$(LIB)(getlocal.o) \
	$(LIB)(getdim.o) \
	$(LIB)(getpoly.o) \
	$(LIB)(gribcreate.o) \
	$(LIB)(gribend.o) \
	$(LIB)(gribinfo.o) \
	$(LIB)(mkieee.o) \
	$(LIB)(rdieee.o) \
	$(LIB)(simpack.o) \
	$(LIB)(simunpack.o) \
	$(LIB)(cmplxpack.o) \
	$(LIB)(compack.o) \
	$(LIB)(misspack.o) \
	$(LIB)(pack_gp.o) \
	$(LIB)(reduce.o) \
	$(LIB)(comunpack.o) \
	$(LIB)(specpack.o) \
	$(LIB)(specunpack.o) \
	$(LIB)(jpcpack.o) \
	$(LIB)(jpcunpack.o) \
	$(LIB)(enc_jpeg2000.o) \
	$(LIB)(dec_jpeg2000.o) \
	$(LIB)(pngpack.o) \
	$(LIB)(pngunpack.o) \
	$(LIB)(enc_png.o) \
	$(LIB)(dec_png.o) \
        $(LIB)(mova2i.o) \
	$(LIB)(gbytesc.o) \
	$(LIB)(skgb.o) \
	$(LIB)(ixgb2.o) \
	$(LIB)(getg2i.o) \
	$(LIB)(getg2ir.o) \
	$(LIB)(getgb2s.o) \
	$(LIB)(getgb2r.o) \
	$(LIB)(getgb2l.o) \
	$(LIB)(getgb2.o) \
	$(LIB)(getgb2p.o) \
	$(LIB)(getgb2rp.o) \
	$(LIB)(putgb2.o) \
	$(LIB)(g2grids.o) \
	$(LIB)(params.o) \
        $(LIB)(params_ecmwf.o) \
	$(LIB)(getidx.o) \
	$(LIB)(gdt2gds.o)

.F.f:
	$(CPP) $(FDEFS) $*.F $*.f

.f.a:
	$(FC) -c $(FFLAGS) $<
	ar $(ARFLAGS) -ruc $@ $*.o
	rm -f $*.o

.c.a:
	$(CC) -c $(CFLAGS) $<
	ar $(ARFLAGS) -ruc $@ $*.o
	rm -f $*.o

