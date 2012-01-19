/**
 * 
 */
package test.unittests;

import java.util.List;

import ncsa.hdf.hdf5lib.H5;
import ncsa.hdf.hdf5lib.HDF5Constants;
import ncsa.hdf.object.Attribute;
import ncsa.hdf.object.FileFormat;
import ncsa.hdf.object.Metadata;
import ncsa.hdf.object.h5.H5File;
import ncsa.hdf.object.h5.H5Group;
import junit.framework.TestCase;

/**
 * @author Rishi R. Sinha
 * This has to be removed because both the methods tested here are actually abstract methods and should be 
 * tested elsewhere.
 *
 */
public class MetadataTest extends TestCase {
	private static final H5File H5FILE = new H5File();
	
	private H5File testFile = null;
   	private H5Group testGroup = null;
   	private Metadata strAttr = null;
   	private Metadata arrayIntAttr = null;
   	
	/**
	 * @param arg0
	 */
	public MetadataTest(String arg0) {
		super(arg0);
	}

	/* (non-Javadoc)
	 * @see junit.framework.TestCase#setUp()
	 */
	protected void setUp() throws Exception {
		super.setUp();
		testFile = (H5File)H5FILE.open(H5TestFile.NAME_FILE_H5, FileFormat.WRITE);
	    assertNotNull(testFile);
	    testGroup = (H5Group) testFile.get(H5TestFile.NAME_GROUP_ATTR);
	    assertNotNull(testGroup);
		List testAttrs = testGroup.getMetadata();
		assertNotNull(testAttrs);
		strAttr = (Attribute) testAttrs.get(0);
		assertNotNull(strAttr);
		arrayIntAttr = (Attribute) testAttrs.get(1);
		assertNotNull(arrayIntAttr);
	}

	/* (non-Javadoc)
	 * @see junit.framework.TestCase#tearDown()
	 */
	protected void tearDown() throws Exception {
		super.tearDown();
//		 make sure all objects are closed
        final int fid = testFile.getFID();
        if (fid > 0) {
            int nObjs = 0;
            try { nObjs = H5.H5Fget_obj_count(fid, HDF5Constants.H5F_OBJ_ALL); }
            catch (final Exception ex) { fail("H5.H5Fget_obj_count() failed. "+ ex);   }
            assertEquals(1, nObjs); // file id should be the only one left open
         }
        
        if (testFile != null) {
            try { testFile.close(); } catch (final Exception ex) {}
            testFile = null;
        }
	}

	/**
	 * Test method for {@link ncsa.hdf.object.Metadata#getValue()}.
	 */
	public final void testGetValue() {
		String[] value = (String[]) strAttr.getValue();
		if (!value[0].equals("String attribute.")) {
            fail("getValue() fails.");
        }
		
		int[] intValue = (int[]) arrayIntAttr.getValue();
		
		for (int i = 0; i < 10; i++) {
			if (intValue[i] != i+1) {
                fail("getValue() fails");
            }
		}
	}

	/**
	 * Test method for {@link ncsa.hdf.object.Metadata#setValue(java.lang.Object)}.
	 */
	public final void testSetValue() {
		String[] tempValue = {"Temp String Value"};
		String[] prevValue = (String[]) strAttr.getValue();
		strAttr.setValue(tempValue);
		String[] value = (String[]) strAttr.getValue();
		if (!value[0].equals("Temp String Value")) {
            fail("setValue() fails.");
        }
		strAttr.setValue(prevValue);
		
		int[] tempIntArray = {0, 1, 2, 3, 4, 5, 6, 7, 8, 9};
		int[] intPrevValue = (int[]) arrayIntAttr.getValue();
		arrayIntAttr.setValue(tempIntArray);
		
		int[] intValue = (int[]) arrayIntAttr.getValue();
		
		for (int i = 0; i < 10; i++) {
			if (intValue[i] != i) {
                fail("getValue() fails");
            }
		}
		arrayIntAttr.setValue(intPrevValue);
	}

}
