#****************************************************************************
#* NCSA HDF                                                                 *
#* National Comptational Science Alliance                                   *
#* University of Illinois at Urbana-Champaign                               *
#* 605 E. Springfield, Champaign IL 61820                                   *
#*                                                                          *
#* For conditions of distribution and use, see the accompanying             *
#* COPYING file.                                                            *
#*                                                                          *
#****************************************************************************/

all:
	(cd object; $(MAKE);)
	(cd unittests; $(MAKE);)

check:
	(cd unittests; $(MAKE) check;)
	(cd object; $(MAKE) check;)

clean:
	(cd object; $(MAKE) clean;)
	(cd unittests; $(MAKE) clean;)
