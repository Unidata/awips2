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

CLASSPATH=/common/awips/jdk1.6.0_05/jre/lib/rt.jar:/home/chammack/hdf5java-beta/hdf-java-2.6:../../lib/junit.jar

.SUFFIXES: .java .class

.java.class:
	$(JAVAC) -classpath $(CLASSPATH) $<

all:    AllH5ObjectTests.class \
        AttributeTest.class \
        CompoundDSTest.class \
        DatasetTest.class \
        DatatypeTest.class \
        FileFormatTest.class \
        GroupTest.class \
        H5BugFixTest.class \
        H5CompoundDSTest.class \
        H5DatatypeTest.class \
        H5FileTest.class \
        H5GroupTest.class \
        H5ScalarDSTest.class \
        H5TestFile.class \
        HObjectTest.class \
        ScalarDSTest.class


check: all
	chmod u+x unittests.sh; ./unittests.sh

clean:
	$(RM) *.class
	$(RM) -f *.hdf
	$(RM) -f *.h5

