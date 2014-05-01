# Makefile for the sounding library
#
# -mkay (mkay@spc.noaa.gov)
# 12/16/99

# Define where dmalloc is located
ifeq ($(OS), Linux)
#DMALLOC = /users/gregg/projects/nsharp/new3/dmalloc-3.0.5-linux
DMALLOC = /NAWIPS/nascrpts/production/nsnsharp_new2/linux/dmalloc-3.0.5-linux
endif 

ifeq ($(OS), HPUX)
DMALLOC = /NAWIPS/nascrpts/production/nsnsharp_new2/hpux/dmalloc-3.0.5-hpux
endif 


ifeq ($(OS), Linux)
	CC=gcc
	FC=g77
	#debug	CC=gcc -g -DDEBUG_JL
	#debug	FC=g77 -g -DDEBUG_JL
	CFLAGS=-DUNDERSCORE -fPIC
	FFLAGS=	
endif

ifeq ($(OS), HPUX)
	cc=cc
	FC=$FC
	CFLAGS=-Ae +DAportable -g -fPIC -I$(DMALLOC)
	FFLAGS=	
endif

AR=/usr/bin/ar
ARFLAGS=rv
RM=/bin/rm

TARGET=libsndg.a

OBJS=basics.o blayer.o pinomoore.o thermo.o winds.o setsndg.o skparams.o \
computeparms.o profile.o parms.o how.o newhistory.o list.o winter.o

all: $(TARGET)

$(TARGET): $(OBJS)
	$(AR) $(ARFLAGS) $@ $(OBJS)

clean:
	$(RM) -f $(TARGET) $(OBJS)

LIBS =  $NAWIPS/lib/linux/ginitp_alt.o \
        $NAWIPS/lib/linux/gendp_alt.o \
        $NAWIPS/lib/linux/gemlib.a \
        $NAWIPS/lib/linux/Nxmlib.a \
        $NAWIPS/lib/linux/gplt.a \
        $NAWIPS/lib/linux/device.a \
        $NAWIPS/lib/linux/xw.a \
        $NAWIPS/lib/linux/gn.a \
        $NAWIPS/lib/linux/cgemlib.a \
        $NAWIPS/lib/linux/gemlib.a \
        -L. -lsndg -lm

#JL#XLIBS  =-L/usr/lib/Motif1.2_R6 -L/usr/lib/X11R6 -lXm -lXt -lX11
XLIBS  =-L/usr/X11R6/lib64 -lXm -lXt -lX11

duh: how.o ../getsndg.o ../get_mdl_snd.o
	$(FC) -o duh how.o ../getsndg.o ../get_mdl_snd.o $(LIBS) $(XLIBS)

TMPOBJS=basics.o blayer.o pinomoore.o thermo.o winds.o setsndg.o \
skparams.o computeparms.o profile.o parms.o

JUNK = -L. -lm
#duh: how.o $(TMPOBJS)
#	$(CC) -o duh how.o $(TMPOBJS) $(JUNK)

mem: mem.o
	$(CC) -o $@ mem.o $(JUNK)

gunk: gunk.o
	$(CC) -o $@ gunk.o $(JUNK)

newhistory: newhistory.o list.o
	$(CC) -o $@ newhistory.o list.o
