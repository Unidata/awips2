# Generated automatically from Makefile.in by configure.
#/****************************************************************************
# * NCSA HDF                                                                 *
# * National Comptational Science Alliance                                   *
# * University of Illinois at Urbana-Champaign                               *
# * 605 E. Springfield, Champaign IL 61820                                   *
# *                                                                          *
# * For conditions of distribution and use, see the accompanying             *
# * hdf-java/COPYING file.                                                   *
# *                                                                          *
# ****************************************************************************/


JAVAC=/common/awips/jdk1.6.0_05/bin/javac
RM=/bin/rm

CLASSPATH=/common/awips/jdk1.6.0_05/jre/lib/rt.jar:/home/chammack/hdf5java-beta/hdf-java-2.6   

.SUFFIXES: .java .class

.java.class:
	$(JAVAC) -classpath $(CLASSPATH) $<

all: testh4file testh5file
	chmod u+x runtests.sh

testh4file: TestH4File.class
	chmod u+x testh4file.sh

testh5file: TestH5File.class
	chmod u+x testh5file.sh

runtests: all
	chmod u+x runtests.sh

check: all
#	./runtests.sh
	./TestH5Object.sh
	./TestH5MemoryLeak.sh

clean:
	$(RM) -f *.class
	$(RM) -f *.hdf
	$(RM) -f *.h5
