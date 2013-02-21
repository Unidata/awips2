#################################################################
#		PLEASE DO NOT MODIFY THIS FILE.			#
#								#
#			libdepends.mk				#
#								#
#################################################################
#
#  Add search path for X libraries.
#
ifneq "$(MACHTYPE)" "x86_64"
VPATH += /usr/lib   /usr/X11R6/lib
else
VPATH += /usr/lib64 /usr/X11R6/lib64
endif

#
#  external libraries
#
ifneq "$(DIRNAM)" "GAMET"
-lGAMET:
	@$(MAKE) -C $(NAWIPS)/extlibs/GAMET
endif

ifneq "$(DIRNAM)" "GPC"
-lGPC:
	@$(MAKE) -C $(NAWIPS)/extlibs/GPC
endif

#
#  core gempak libraries
#
ifneq "$(DIRNAM)" "appl" 
-lappl:
	@$(MAKE) -C $(GEMPAK)/source/appl
endif

ifneq "$(DIRNAM)" "awlib"
-lawlib:
	@$(MAKE) -C $(GEMPAK)/source/awlib
endif

ifneq "$(DIRNAM)" "bridge"
-lbridge:
	@$(MAKE) -C $(GEMPAK)/source/bridge
endif

ifneq "$(DIRNAM)" "cgemlib"
-lcgemlib:
	@$(MAKE) -C $(GEMPAK)/source/cgemlib
endif

ifneq "$(DIRNAM)" "device"
-ldevice:
	@$(MAKE) -C $(GEMPAK)/source/device
endif

ifneq "$(DIRNAM)" "diaglib"
-ldiaglib:
	@$(MAKE) -C $(GEMPAK)/source/diaglib
endif

ifneq "$(DIRNAM)" "driver"
-ldriver:
	@$(MAKE) -C $(GEMPAK)/source/driver
endif

ifneq "$(DIRNAM)" "gemlib"
-lgemlib:
	@$(MAKE) -C $(GEMPAK)/source/gemlib
endif

-lgplt:
	@$(MAKE) -C $(GEMPAK)/source/gplt

-lgpltdev:
	@$(MAKE) -C $(GEMPAK)/source/gpltdev

ifneq "$(DIRNAM)" "griblib"
-lgriblib:
	@$(MAKE) -C $(GEMPAK)/source/griblib
endif

ifneq "$(DIRNAM)" "gridlib"
-lgridlib:
	@$(MAKE) -C $(GEMPAK)/source/gridlib
endif

ifneq "$(DIRNAM)" "jwblib"
-ljwblib:
	@$(MAKE) -C $(GEMPAK)/source/jwblib
endif

ifneq "$(DIRNAM)" "ncepUT"
-lncepUT:
	@$(MAKE) -C $(GEMPAK)/source/ncepUT
endif

ifneq "$(DIRNAM)" "nmaplib"
-lnmaplib:
	@$(MAKE) -C $(GEMPAK)/source/nmaplib
endif

ifneq "$(DIRNAM)" "nxmlib"
-lnxmlib:
	@$(MAKE) -C $(GEMPAK)/source/nxmlib
endif

ifneq "$(DIRNAM)" "oalib"
-loalib:
	@$(MAKE) -C $(GEMPAK)/source/oalib
endif

ifneq "$(DIRNAM)" "prmcnvlib"
-lprmcnvlib:
	@$(MAKE) -C $(GEMPAK)/source/prmcnvlib
endif

ifneq "$(DIRNAM)" "sflib"
-lsflib:
	@$(MAKE) -C $(GEMPAK)/source/sflib
endif

ifneq "$(DIRNAM)" "snlib"
-lsnlib:
	@$(MAKE) -C $(GEMPAK)/source/snlib
endif

ifneq "$(DIRNAM)" "syslib"
-lsyslib:
	@$(MAKE) -C $(GEMPAK)/source/syslib
endif

ifneq "$(DIRNAM)" "textlib"
-ltextlib:
	@$(MAKE) -C $(GEMPAK)/source/textlib
endif

ifneq "$(DIRNAM)" "vflib"
-lvflib:
	@$(MAKE) -C $(GEMPAK)/source/vflib
endif

#
#  driver libraries
#
ifneq "$(DIRNAM)" "fax"
-lfax:
	@$(MAKE) -C $(GEMPAK)/source/driver/active/fax
endif

ifneq "$(DIRNAM)" "gf"
-lgf:
	@$(MAKE) -C $(GEMPAK)/source/driver/active/gf
endif

ifneq "$(DIRNAM)" "gif"
-lgif:
	@$(MAKE) -C $(GEMPAK)/source/driver/active/gif
endif

ifneq "$(DIRNAM)" "gn"
-lgn:
	@$(MAKE) -C $(GEMPAK)/source/driver/active/gn
endif

ifneq "$(DIRNAM)" "nc"
-lnc:
	@$(MAKE) -C $(GEMPAK)/source/driver/active/nc
endif

ifneq "$(DIRNAM)" "ps"
-lps:
	@$(MAKE) -C $(GEMPAK)/source/driver/active/ps
endif

ifneq "$(DIRNAM)" "rbk"
-lrbk:
	@$(MAKE) -C $(GEMPAK)/source/driver/active/rbk
endif

ifneq "$(DIRNAM)" "tiff"
-ltiff:
	@$(MAKE) -C $(GEMPAK)/source/driver/active/tiff
endif

ifneq "$(DIRNAM)" "utf"
-lutf:
	@$(MAKE) -C $(GEMPAK)/source/driver/active/utf
endif

ifneq "$(DIRNAM)" "vg"
-lvg:
	@$(MAKE) -C $(GEMPAK)/source/driver/active/vg
endif

ifneq "$(DIRNAM)" "xw"
-lxw:
	@$(MAKE) -C $(GEMPAK)/source/driver/active/xw
endif

ifneq "$(DIRNAM)" "xwp"
-lxwp:
	@$(MAKE) -C $(GEMPAK)/source/driver/active/xwp
endif

$(LIBDIR)/ginitp_alt.o $(LIBDIR)/gendp_alt.o:
	@$(MAKE) -C $(GEMPAK)/source/gplt/access/alt

#
#  program libraries
#
-lairmet_share:
	@$(MAKE) -C $(GEMPAK)/source/contrib/awc/airmet_share

ifneq "$(DIRNAM)" "gdcfil"
-lgdcfil:
	@$(MAKE) -C $(GEMPAK)/source/programs/gd/gdcfil
endif

ifneq "$(DIRNAM)" "gdcntr"
-lgdcntr:
	@$(MAKE) -C $(GEMPAK)/source/programs/gd/gdcntr
endif

ifneq "$(DIRNAM)" "gdmap"
-lgdmap:
	@$(MAKE) -C $(GEMPAK)/source/programs/gd/gdmap
endif

ifneq "$(DIRNAM)" "gdplot2"
-lgdplot2:
	@$(MAKE) -C $(GEMPAK)/source/programs/gd/gdplot2
endif

ifneq "$(DIRNAM)" "gdptpdf"
-lgdptpdf:
	@$(MAKE) -C $(GEMPAK)/source/programs/gd/gdptpdf
endif

ifneq "$(DIRNAM)" "gdstream"
-lgdstream:
	@$(MAKE) -C $(GEMPAK)/source/programs/gd/gdstream
endif

ifneq "$(DIRNAM)" "gdthgt"
-lgdthgt:
	@$(MAKE) -C $(GEMPAK)/source/programs/gd/gdthgt
endif

ifneq "$(DIRNAM)" "grphgd"
-lgrphgd:
	@$(MAKE) -C $(GEMPAK)/source/programs/gd/grphgd
endif

ifneq "$(DIRNAM)" "oabsfc"
-loabsfc:
	@$(MAKE) -C $(GEMPAK)/source/programs/oa/oabsfc
endif

ifneq "$(DIRNAM)" "sfmap"
-lsfmap:
	@$(MAKE) -C $(GEMPAK)/source/programs/sf/sfmap
endif

ifneq "$(DIRNAM)" "shp"
-lshp:
	@$(MAKE) -C $(GEMPAK)/source/programs/maps/shp
endif

ifneq "$(DIRNAM)" "snmap"
-lsnmap:
	@$(MAKE) -C $(GEMPAK)/source/programs/sn/snmap
endif

#
#  End of libdepends.mk
#
