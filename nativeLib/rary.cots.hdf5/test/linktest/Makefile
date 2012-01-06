#/****************************************************************************
# * NCSA HDF                                                                 *
# * National Comptational Science Alliance                                   *
# * University of Illinois at Urbana-Champaign                               *
# * 605 E. Springfield, Champaign IL 61820                                   *
# *                                                                          *
# * For conditions of distribution and use, see the accompanying             *
# * java-hdf5/COPYING file.                                                  *
# *                                                                          *
# ****************************************************************************/


JAVAC           = /common/awips/jdk1.6.0_05/bin/javac
RM           = /bin/rm

CLASSPATH=/common/awips/jdk1.6.0_05/jre/lib/rt.jar:/home/chammack/hdf5java-beta/hdf-java-2.6                                                         

.SUFFIXES: .java .class

.java.class:
	$(JAVAC) -classpath $(CLASSPATH) $<

all: TestHDF5Link TestHDF4Link

TestHDF5Link: TestHDF5Link.class
	chmod u+x testlink

TestHDF4Link: TestHDF4Link.class
	chmod u+x testh4link

clean:
	$(RM) *.class
