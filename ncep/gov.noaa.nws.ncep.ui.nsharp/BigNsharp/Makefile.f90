PROG	= nsharp.exe
DEBUG	=0
CC	= cc
# for Linux
#FC	= g77
FC	= 
# for HPUX 
FFLAGS  = -I$(GEMINC) +DAportable -g
# for linux compiles
#FFLAGS  = -fno-second-underscore -I$(GEMINC)/Linux -I$(GEMINC)
#CFLAGS_G = -Ae -DHPTP -D_HPUX_SOURCE +DAportable -g
#CFLAGS_G = -Ae -D_HPUX_SOURCE +DAportable -g
# for HPUX
CFLAGS_G = -Ae +DAportable -g
# for linux compiles
#CFLAGS_G = -DUNDERSCORE -DLinux 
#CFLAGS	=$(CFLAGS_G) -U__cplusplus -I. -I./Sndglib -I$(GEMINC) -I/usr/include
CFLAGS	=$(CFLAGS_G) -I. -I./Sndglib -I$(GEMINC) -I/usr/include \
	 -I$(NAWIPS)/include -I$(NAWIPS)/nprogs/nwx/source \
	 -I/usr/include/X11R6 -I/usr/include/Motif1.2_R6 \
	 -I/ops_users_nfssrv1/nascrpts/projects/nsharp/new3/Sndglib/dmalloc-3.0.5 
LFLAGS	=$(LFLAGS_G) $(LFLAGS_R)


LOCLIB  = $(GEMOLB)/xwp.a $(GEMOLB)/xw.a $(GEMOLB)/ps.a $(GEMOLB)/gn.a


LIBS =	-L./Sndglib -lsndg \
	-L/ops_users_nfssrv1/nascrpts/projects/nsharp/new3/Sndglib/dmalloc-3.0.5 -ldmalloc \
	$(GEMOLB)/nmaplib.a \
	$(GEMOLB)/ginitp_alt.o $(GEMOLB)/gendp_alt.o \
	$(GEMOLB)/sfmap.a $(GEMOLB)/snmap.a \
	$(GEMOLB)/gdplot2.a $(GEMOLB)/gdmap.a \
	$(GEMOLB)/gdstream.a $(GEMOLB)/grphgd.a \
	$(GEMOLB)/gdcfil.a $(GEMOLB)/Nxmlib.a \
	$(GEMLIB) $(CGEMLIB) $(GPLT) $(DEVICE) \
	$(LOCLIB) $(GEMLIB) $(CGEMLIB) \
	$(GEMOLB)/libnetcdf.a $(GEMOLB)/libz.a \
	-lz -lm



#LIBS =  $(NAWIPS)/lib/linux2/ginitp_alt.o \
#	$(NAWIPS)/lib/linux2/gendp_alt.o \
#	$(NAWIPS)/lib/linux2/Nxmlib.a \
#	$(NAWIPS)/lib/linux2/device.a \
#	$(NAWIPS)/lib/linux2/xw.a \
#	$(NAWIPS)/lib/linux2/gn.a \
#	$(NAWIPS)/lib/linux2/cgemlib.a \
#	$(NAWIPS)/lib/linux2/gemlib.a \
#	$(NAWIPS)/lib/linux2/gplt.a \
#	-L$(GEMOLB) -L./Sndglib -L/home/gregg/projects/nsharp/new3/Sndglib/dmalloc-3.0.5 \
#	-lz -lnetcdf -ldmalloc -lsndg -lm

XLIBS  = -L/usr/X11R6/lib -lXm -lXt -lX11

NOBJS = xinitd.o xwvid1.o xwvid3.o xwvid6.o xwvid2.o \
	xwvid5.o decoder.o draw.o readdata.o sharp95.o \
	get_gem_times.o get_mdl_time.o get_mdl_snd.o get_gem_stns.o \
	mapinit.o mapmark.o mapdraw.o mapw.o calwxt.o \
	xvgifwr.o getsfc.o fonts.o getsndg.o config.o \
	get_mdl_stns.o globfiles.o textwin.o textsave.o hpgl.o \
	parameterization.o

SNDGLIB = ./Sndglib/libsndg.a

# Libraries needed for some GEMPAK library calls in the code
# This can be removed from the Makefile
GEMLIBS=$(GEMLIB) $(APPL) $(SYSLIB) $(GPLT) $(GEMLIB)
DEVLIBS=$NAWIPS/lib/linux2/device.a \
	$NAWIPS/lib/linux2/xw.a \
	$NAWIPS/lib/linux2/gn.a

GPLT_ALT=$/NAWIPS/lib/linux2/ginitp_alt.o \
         $/NAWIPS/lib/linux2/gendp_alt.o
MYLIBS=$(GEMOLB)/snlist.a \
	$(GEMOLB)/ginitp_alt.o \
	$(GEMOLB)/gendp_alt.o \
	$(GEMOLB)/gemlib.a \
	$(GEMOLB)/gplt.a \
	$(GEMOLB)/device.a \
	$(GEMOLB)/gn.a \
	$(GEMOLB)/cgemlib.a \
	$(GEMOLB)/gemlib.a -lm

all::	$(PROG)

$(PROG)::
	@echo Compiling and Linking \`$@\` 
	@$(MAKE) _nsharp

_nsharp:: $(NOBJS) sndglib
	$(FC) -o $(PROG) $(NOBJS) $(LIBS) $(XLIBS)

sndglib:
	cd Sndglib; $(MAKE)

links:
	@-$(GEMPAK)/install/setup_links

xwvid1.o:	xwvid1.c $(H)
xwvid2.o:	xwvid2.c $(H)
xwvid3.o:	xwvid3.c $(H)
hpgl.o:		hpgl.c $(H)
readdata.o:	readdata.c $(H)
sharp95.o:	sharp95.c $(H)
decoder.o:	decoder.c $(H)
xwvid5.o:	xwvid5.c $(H)
#mapw.o:		gui.h $(H)
#draw.o:		gui.h $(H)
xwvid6.o:       xwvid6.c $(H)
winter.o:	winter.c $(H)
parameterization.o:	parameterization.c $(H)

getsndg: getsndg.o get_mdl_snd.o
	$(FC) -o $@ getsndg.o get_mdl_snd.o $(MYLIBS)

config: config.o globfiles.o
	$(CC) -o $@ config.o globfiles.o

#config.o: config.c
#	$(CC) $(CFLAGS) -DZORRO -c config.c

clean:
	rm -f $(PROG) $(NOBJS)

OBJS=dummy2.o get_mdl_snd.o config.o globfiles.o get_mdl_stns.o get_mdl_time.o
dummy2: $(OBJS)
	$(FC) -o $@ $(OBJS) $(LIBS) $(XLIBS)

dummy: dummy.o
	$(CC) -o $@ dummy.o -L/home/gregg/projects/nsharp/new3/Sndglib/dmalloc-3.0.5 -ldmalloc
