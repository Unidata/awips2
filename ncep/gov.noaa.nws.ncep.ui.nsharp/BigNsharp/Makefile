#
# Makefile for bigsharp.
# Modified for NAWIPS version 5.11.2+, tested with 5.11.4
# Jay Liang, Fri Dec 12 20:56:00 UTC 2008
#
# CC and FC flags can be defined with -g for extra debugging info, however,
# do NOT use -O flag for optimization as it dumps core.
#

PROG	= nsharp.exe
DEBUG	= 0
CC	= gcc
FC	= g77
#debug	CC	= gcc -g -DDEBUG_JL
#debug	FC	= g77 -g -DDEBUG_JL


FFLAGS	= -fno-second-underscore -I$(OS_INC) -I$(GEMINC)
CFLAGS_G = -DUNDERSCORE -DLinux
CFLAGS	= $(CFLAGS_G) -I. -I./Sndglib -I$(OS_INC) -I$(GEMINC) \
	-I$(NWX) -I$(NSHARP) \
	-I/usr/include/X11R6 -I/usr/X11R6/include/Xm \
        -I/awips2/python/include/python2.7 

LFLAGS	=$(LFLAGS_G) $(LFLAGS_R)

XLIBS	= -L/usr/X11R6/lib -lXm -lXt -lX11

#CHIN change "-lnetcdf" to "$(OS_LIB)/libnetcdf.a"
LIBS	= -L./Sndglib -lsndg \
	$(OS_LIB)/ginitp_alt.o $(OS_LIB)/gendp_alt.o \
	-L$(OS_LIB) -lsnlist -lsnlib -lsflist -lsflib -lnxmlib -ldiaglib \
	-lgemlib -lprmcnvlib -lgridlib -lgplt -lgridlib -lcgemlib \
	-ldevice -lxwp -lxw -lps -lgn -lgemlib $(OS_LIB)/libnetcdf.a \
	-ltextlib -lxml2 -lxslt -liconv -lbz2 \
	$(XLIBS) -lz -lm -L/awips2/python/lib -lpython2.7

NOBJS	= xinitd.o xwvid1.o xwvid3.o xwvid6.o xwvid2.o \
	xwvid5.o decoder.o readdata.o sharp95.o \
	get_gem_times.o get_mdl_time.o get_mdl_snd.o get_gem_stns.o \
	mapinit.o mapmark.o mapdraw.o mapw.o draw.o \
	calwxt.o xvgifwr.o getsfc.o fonts.o getsndg.o config.o \
	get_mdl_stns.o globfiles.o textwin.o textsave.o hpgl.o \
	parameterization.o nhail1.o sars.o sup_nsharp.o \
	snrdatjh.o snmergjh.o mruadtjh.o

SNDGLIB = ./Sndglib/libsndg.a

all::	$(PROG)

$(PROG)::
	@echo Compiling and Linking \`$@\` 
	@$(MAKE) _nsharp

_nsharp:: $(NOBJS) sndglib
	$(FC) -o $(PROG) $(NOBJS) $(LIBS)
	

sndglib:
	cd ./Sndglib; $(MAKE)

links:
	@-$(GEMPAK)/install/setup_links

clean:
	rm -f $(PROG) $(NOBJS)
	cd ./Sndglib; $(MAKE) clean
