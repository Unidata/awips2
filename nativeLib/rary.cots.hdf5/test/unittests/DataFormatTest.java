/**
 * 
 */
package test.unittests;

import ncsa.hdf.hdf5lib.H5;
import ncsa.hdf.hdf5lib.HDF5Constants;
import ncsa.hdf.object.Attribute;
import ncsa.hdf.object.DataFormat;
import ncsa.hdf.object.Datatype;
import ncsa.hdf.object.FileFormat;
import ncsa.hdf.object.h5.H5Datatype;
import ncsa.hdf.object.h5.H5File;
import junit.framework.TestCase;
import java.util.List;

/**
 * @author rsinha
 *
 */
public class DataFormatTest extends TestCase {
	  private static final H5File H5FILE = new H5File();
	    
	  private H5File testFile = null;
	  private DataFormat testGroup = null;
	/**
	 * @param arg0
	 */
	public DataFormatTest(String arg0) {
		super(arg0);
	}

	/* (non-Javadoc)
	 * @see junit.framework.TestCase#setUp()
	 */
	protected void setUp() throws Exception {
		super.setUp();
		testFile = (H5File)H5FILE.open(H5TestFile.NAME_FILE_H5, FileFormat.WRITE);
        assertNotNull(testFile);
        testGroup = testFile.get(H5TestFile.NAME_GROUP_ATTR);
        assertNotNull(testGroup);
	}

	/* (non-Javadoc)
	 * @see junit.framework.TestCase#tearDown()
	 */
	protected void tearDown() throws Exception {
		super.tearDown();
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
	 * Test method for {@link ncsa.hdf.object.DataFormat#getFile()}.
     * <ul>
     *     <li>Test if the file name is correct
     * </ul>
	 */
	public final void testGetFile() {
		if (!testGroup.getFile().equals(H5TestFile.NAME_FILE_H5)) {
            fail("getFile() fails.");
        }
	}

	/**
	 * Test method for {@link ncsa.hdf.object.DataFormat#getMetadata()}.
     * <ul>
     *     <li> Reading the attributes
     *     <li> Checking the values of attributes
     * </ul>
	 */
	public final void testGetMetadata() {
		Attribute strAttr = null;
		Attribute arrayIntAttr = null;
		List mdataList = null;
		try {
			mdataList = testGroup.getMetadata();
		}
		catch (final Exception ex) { 
			fail("getMetadata() failed. " + ex );   
		}
		strAttr = (Attribute) mdataList.get(0);
		arrayIntAttr = (Attribute) mdataList.get(1);
		String[] value = (String[]) strAttr.getValue();
		if (!value[0].equals("String attribute.")) {
            fail("getMdata() failed.");
        }

		int[] intValue = (int[]) arrayIntAttr.getValue();
		long[] dims = arrayIntAttr.getDataDims();

		for (int i = 0; i < dims[0]; i++) {
			if (intValue[i] != i+1) {
                fail("getValue() failed");
            }
		}
	}

	/**
	 * Test method for {@link ncsa.hdf.object.DataFormat#writeMetadata(java.lang.Object)}.
     * <ul>
     *     <li> Writing new attributes
     *     <li> Checking that the new attributes are written in file
     * </ul>
	 */
	public final void testWriteMetadata() {
		long[] attrDims = {1};
		String attrName = "CLASS";
		String[] classValue = {"IMAGE"};
		Datatype attrType = new H5Datatype(Datatype.CLASS_STRING, classValue[0].length()+1, -1, -1);
		Attribute attr = new Attribute(attrName, attrType, attrDims);
		assertNotNull(testGroup);
		assertNotNull(attr);
		attr.setValue(classValue);
		try {
			testGroup.writeMetadata(attr);
		} catch (Exception ex) {
			fail("writeMetadata() failed " + ex.getMessage());	
		}
		
		List mdataList = null;
		try {
			mdataList = testGroup.getMetadata();
		}
		catch (final Exception ex) { 
			fail("getMetadata() failed. " + ex );   
		}
	
		assertEquals(3, mdataList.size());
		
		Attribute strAttr = null;
		Attribute arrayIntAttr = null;

		strAttr = (Attribute) mdataList.get(0);
		arrayIntAttr = (Attribute) mdataList.get(1);
		String[] value = (String[]) strAttr.getValue();
		

		if (!value[0].equals("String attribute.")) {
            fail("writeMdata() failed.");
        }

		int[] intValue = (int[]) arrayIntAttr.getValue();
		long[] dims = arrayIntAttr.getDataDims();

		for (int i = 0; i < dims[0]; i++) {
			if (intValue[i] != i+1) {
                fail("writeValue() failed");
            }
		}
		strAttr = (Attribute) mdataList.get(2);
		value = (String[]) strAttr.getValue();
		if (!value[0].equals("IMAGE")) {
            fail("writeMetadata() failed.");
        }
	}

	/**
	 * Test method for {@link ncsa.hdf.object.DataFormat#removeMetadata(java.lang.Object)}.
     * <ul>
     *     <li> Remove an attribute
     * </ul>
	 */
	public final void testRemoveMetadata() {
		List mdataList = null;
		try {
			mdataList = testGroup.getMetadata();
		}
		catch (final Exception ex) { 
			fail("getMetadata() failed. " + ex.getMessage());   
		}
		
		Attribute strAttr = (Attribute) mdataList.get(2);
		try {
		testGroup.removeMetadata(strAttr);
		} catch (Exception e) {
			fail("removeMetadata() failed " + e.getMessage());
		}
		assertEquals(2, mdataList.size());
	}

}
