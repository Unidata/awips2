/**
 * 
 */
package test.unittests;

import ncsa.hdf.hdf5lib.H5;
import ncsa.hdf.hdf5lib.HDF5Constants;
import ncsa.hdf.object.FileFormat;
import ncsa.hdf.object.h5.H5File;
import junit.framework.TestCase;
import ncsa.hdf.object.HObject;

/**
 * @author Rishi R. Sinha
 *
 */
public class HObjectTest extends TestCase {
	private static final H5File H5FILE = new H5File();
    private static final String GNAME = H5TestFile.NAME_GROUP;
 
    private H5File testFile = null;
    private HObject testObj = null;
    private long testOID;
    
	/**
	 * @param arg0
	 */
	public HObjectTest(String arg0) {
		super(arg0);
	}

	/* (non-Javadoc)
	 * @see junit.framework.TestCase#setUp()
	 */
	protected void setUp() throws Exception {
		super.setUp();
		
        testFile = (H5File)H5FILE.open(H5TestFile.NAME_FILE_H5, FileFormat.WRITE);
        assertNotNull(testFile);
        testObj = testFile.get(GNAME);
        assertNotNull(testObj);
        testOID = testObj.getOID()[0];
	}

	/* (non-Javadoc)
	 * @see junit.framework.TestCase#tearDown()
	 */
	protected void tearDown() throws Exception {
		super.tearDown();
		
        // make sure all objects are closed
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
         * Test method for {@link ncsa.hdf.object.HObject#getFile()}.
         * <p>
         * What to test:
         * <ul>
         *   <li> Make sure file name in object yields same file as filename
         * </ul>
         */
        public final void testGetFile() {
            String fullFileName = testObj.getFile();
            if ( ! fullFileName.endsWith( H5TestFile.NAME_FILE_H5 ) ) {
                fail("Wrong File");
            }
        }


	/**
	 * Test method for {@link ncsa.hdf.object.HObject#getName()}.
	 * <p>
	 * What to test:
	 * <ul>
	 * 	 <li> For the base group, find the name of the group and test it against the standard.
	 * </ul>
	 */
	public final void testGetName() {
		if (!testObj.getName().equals(GNAME.substring(1))) {
            fail("GetName returns wrong name");
        }
	}

	/**
	 * Test method for {@link ncsa.hdf.object.HObject#getFullName()}.
	 * <p>
	 * What to test:
	 * <ul>
	 * 	 <li> For the base group, find the full name of the group and test it against the standard.
	 * </ul>
	 */
	public final void testGetFullName() {
		if (!testObj.getFullName().equals(GNAME)) {
            fail("GetFullName returns wrong name");
        }
	}

	/**
	 * Test method for {@link ncsa.hdf.object.HObject#getPath()}.
	 * * <p>
	 * What to test:
	 * <ul>
	 * 	 <li> For the base group, find the path of the group and test it against the standard.
	 * </ul>
	 */
	public final void testGetPath() {
		if (!testObj.getPath().equals("/")) {
            fail("GetPath returns wrong path");
        }
	}

	/**
	 * Test method for {@link ncsa.hdf.object.HObject#setName(java.lang.String)}.
	 * <p>
	 * What to test:
	 * <ul>
	 * 	 <li> Test setting the name to null. It should not be set.
	 * 	 <li> Test setting the name to another existing name in the same group.
	 *   <li> Test setting the name to a new name.
	 * </ul>
	 */
	public final void testSetName() {
        final String newName = "tmpName";

        // test set name to null
        try {
            testObj.setName(null);
        } catch (final Exception ex) { 
            ; // Expected - intentional
        }
       
        // set to an existing name
        try {
            testObj.setName(H5TestFile.NAME_DATASET_FLOAT);
        } catch (final Exception ex) { 
            ; // Expected - intentional
        }

        try { 
            testObj.setName(newName); 
        } catch (final Exception ex) { 
            fail("setName() failed. "+ ex);
        }
 
        // close the file and reopen it
        try {
            testFile.close();
            testFile.open();
            testObj = testFile.get(newName);
        } catch (final Exception ex) { 
            fail("setName() failed. "+ ex);
        }
       
        HObject tmpObj;
        // test the old name
        try {
            tmpObj = testFile.get(GNAME);
         } catch (final Exception ex) { 
             tmpObj = null; // Expected - intentional
        }
        assertNull("The dataset should be null because it has been renamed", tmpObj);

        // set back the original name
        try { 
            testObj.setName(GNAME); 
        } catch (final Exception ex) { 
            fail("setName() failed. "+ ex);
        }
        
        // make sure the dataset is OK
        try {
            testObj = testFile.get(GNAME);
        } catch (final Exception ex) { 
            fail("setName() failed. "+ ex);
        }
        assertNotNull(testObj);
	}

	/**
	 * Test method for {@link ncsa.hdf.object.HObject#setPath(java.lang.String)}.
	 * <p>
	 * What to test:
	 * <ul>
	 * 	 <li> Test setting the path to null. It should not be set.
	 * 	 <li> Test setting the path to another existing name in the same group.
	 *   <li> Test setting the path to a new name.
	 * </ul>
	 */
	public final void testSetPath() {
		String path = testObj.getPath();
		try {
			testObj.setPath(null);
		} catch (Exception e) {;}
        
		if (!path.equals(testObj.getPath())) {
			fail("testPath changed the path name even though null was passed to it.");
		}
        
		try {
			testObj.setPath("testPath");
		} catch (Exception e) {
			fail("testPath failed when trying to set it to testPath");
		}
		if (!testObj.getPath().equals("testPath")) {
			fail("testPath failed when trying to set it to testPath");
		}
		try  {
			testObj.setPath(path);
		} catch (Exception e) {
			fail("testPath failed when trying to reset the path to " + path);
		}
	}

	/**
	 * Test method for {@link ncsa.hdf.object.HObject#open()}.
	 *  <p>
	 * What to test:
	 * <ul>
	 * 	 <li> Open the Group and check that the gid returned is less than 1.
	 * </ul>
	 */
	public final void testOpen() {
        int gid=-1;

        for (int loop=0; loop < 15; loop++) {
            gid=-1;
            try {
                gid = testObj.open();
            } catch (final Exception ex) { 
                fail("open() failed. "+ ex);
            }
            assertTrue(gid > 0);
            testObj.close(gid);
         }
	}

	/**
	 * Test method for {@link ncsa.hdf.object.HObject#close(int)}.
	 * <p>
	 * What to test:
	 * <ul>
	 * 	 <li> Run the tests for opening the group.
	 * </ul>
	 */
	public final void testClose() {
		testOpen();
	}

	/**
	 * Test method for {@link ncsa.hdf.object.HObject#getFID()}.
	 * <p>
	 * What to test:
	 * <ul>
	 * 	 <li> get the FID for the group and make sure that it is the same as the FID for the file.
	 * </ul>
	 */
	public final void testGetFID() {
		assertEquals(testObj.getFID(), testFile.getFID());
	}

	/**
	 * Test method for {@link ncsa.hdf.object.HObject#equalsOID(long[])}.
	 * <p>
	 * What to test:
	 * <ul>
	 * 	 <li> Check against null. It should fail.
	 * 	 <li> Check against the OID that we have already extraced.
	 * </ul>
	 */
	public final void testEqualsOID() {
		assertNotNull(testObj);
		assertTrue(testObj.equalsOID(new long[] {testOID}));
	}

	/**
	 * Test method for {@link ncsa.hdf.object.HObject#getFileFormat()}.
	 * <p>
	 * What to test:
	 * <ul>
	 * 	 <li> For the group, check against null. 
	 *   <li> For the group, check against the testFile.
	 * </ul>
	 */
	public final void testGetFileFormat() {
		assertNotNull(testObj.getFileFormat());
		assertEquals(testObj.getFileFormat(), testFile);
	}

	/**
	 * Test method for {@link ncsa.hdf.object.HObject#getOID()}.
	 * <p>
	 * What to test:
	 * <ul>
	 *   <li> Check that OIDlist is not null.
	 *   <li> Check that OID[0] is correct. 
	 * </ul>
	 */
	public final void testGetOID() {
		assertNotNull(testObj.getOID());
		assertEquals(testObj.getOID()[0], testOID);
	}

	/**
	 * Test method for {@link ncsa.hdf.object.HObject#hasAttribute()}.
	 * <p>
	 * What to test:
	 * <ul>
	 *   <li> Check for Image dataset which has an attribute.
	 *   <li> Check for base group which has no attributes.
	 * </ul>
	 */
	public final void testHasAttribute() {
		try {
			assertTrue(testFile.get(H5TestFile.NAME_DATASET_IMAGE).hasAttribute());
		} catch (Exception e) {
			fail("get() fails.");
		}
		assertFalse(testObj.hasAttribute());
	}

	/**
	 * Test method for {@link ncsa.hdf.object.HObject#toString()}.
	 * <p>
	 * What to test:
	 * <ul>
	 *   <li> Check for the group.
	 * </ul>  
	 */
	public final void testToString() {
		assertEquals(testObj.toString(), GNAME.substring(1));
	}

}
