/**
 * 
 */
package test.unittests;

import junit.framework.Test;
import junit.framework.TestSuite;

/**
 * Test suite for all unit tests of HDF5 objects.
 * 
 * @author xcao
 *
 */
public class AllH5ObjectTests {
    public static Test suite() {
        final TestSuite suite = new TestSuite("Test for test.unittests");
        //$JUnit-BEGIN$
        
        // ncsa.hdf.object.h5 package
        suite.addTestSuite(H5CompoundDSTest.class);
        suite.addTestSuite(H5BugFixTest.class);
        suite.addTestSuite(H5ScalarDSTest.class);
        suite.addTestSuite(H5GroupTest.class);
        suite.addTestSuite(H5DatatypeTest.class);
        suite.addTestSuite(H5FileTest.class);

        // ncsa.hdf.object package
        suite.addTestSuite(CompoundDSTest.class);
        suite.addTestSuite(DatasetTest.class);
        suite.addTestSuite(ScalarDSTest.class);
        suite.addTestSuite(AttributeTest.class);
        suite.addTestSuite(DatatypeTest.class);
        suite.addTestSuite(FileFormatTest.class);
        suite.addTestSuite(GroupTest.class);
        suite.addTestSuite(HObjectTest.class);
        suite.addTestSuite(H5CompoundDSTest.class);
        
        //$JUnit-END$
        return suite;
    }

    public static void main(final String[] args) {
        
        try { H5TestFile.createTestFile(null); } 
        catch (final Exception ex) {
            System.out.println("*** Unable to create HDF5 test file. "+ex);
            System.exit(-1);
        }
        
        junit.textui.TestRunner.run(suite());
    }
}
