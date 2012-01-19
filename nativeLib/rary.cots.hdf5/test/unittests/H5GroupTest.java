/**
 * 
 */
package test.unittests;

import java.util.Vector;

import ncsa.hdf.hdf5lib.H5;
import ncsa.hdf.hdf5lib.HDF5Constants;
import ncsa.hdf.hdf5lib.HDFNativeData;
import ncsa.hdf.hdf5lib.exceptions.HDF5Exception;
import ncsa.hdf.object.Attribute;
import ncsa.hdf.object.Datatype;
import ncsa.hdf.object.FileFormat;
import ncsa.hdf.object.Group;
import ncsa.hdf.object.h5.H5Group;
import ncsa.hdf.object.h5.H5Datatype;
import ncsa.hdf.object.h5.H5File;
import junit.framework.TestCase;

/**
 * @author xcao
 *
 */
public class H5GroupTest extends TestCase {
    private static final H5File H5FILE = new H5File();
    private static final int NLOOPS = 5;
    private static final int TEST_VALUE_INT = Integer.MAX_VALUE;
    private static final float TEST_VALUE_FLOAT = Float.MAX_VALUE;
    private static final String TEST_VALUE_STR = "H5GroupTest";
    private static final String GNAME = H5TestFile.NAME_GROUP_ATTR;
    private static final String GNAME_SUB = H5TestFile.NAME_GROUP_SUB;
    
    private H5Datatype typeInt = null;
    private H5Datatype typeFloat = null;
    private H5Datatype typeStr = null;
    private H5File testFile = null;
    private H5Group testGroup = null;

    /**
     * @param arg0
     */
    public H5GroupTest(final String arg0) {
        super(arg0);
    }

    /* (non-Javadoc)
     * @see junit.framework.TestCase#setUp()
     */
    protected void setUp() throws Exception {
        super.setUp();
        
        testFile = (H5File)H5FILE.open(H5TestFile.NAME_FILE_H5, FileFormat.WRITE);
        assertNotNull(testFile);

        typeInt = new H5Datatype(Datatype.CLASS_INTEGER, H5TestFile.DATATYPE_SIZE, -1, -1);
        typeFloat = new H5Datatype(Datatype.CLASS_FLOAT, H5TestFile.DATATYPE_SIZE, -1, -1);
        typeStr = new H5Datatype(Datatype.CLASS_STRING, H5TestFile.STR_LEN, -1, -1);

        testGroup = (H5Group)testFile.get(GNAME);
        assertNotNull(testGroup);
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
     * Test method for {@link ncsa.hdf.object.h5.H5Group#setName(java.lang.String)}.
     * <p>
     * What to test:
     * <ul> 
     *   <li> Test for boundary conditions
     *   <ul>
     *     <li> Set name to null
     *   </ul>
     *   <li> Test for failure
     *   <ul>
     *     <li> Set a name that already exists in file.
     *   </ul>
     *   <li> Test for general functionality
     *   <ul>
     *     <li> change the dataset name
     *     <li> close/re-open the file
     *     <li> get the dataset with the new name
     *     <li> failure test: get the dataset with the original name
     *     <li> set the name back to the original name
     *   </ul>
     * </ul>
     */
    public final void testSetName() {
        final String newName = "tmpName";

        // test set name to null
        try {
            testGroup.setName(null);
        } catch (final Exception ex) { 
            ; // Expected - intentional
        }
       
        // set to an existing name
        try {
            testGroup.setName(H5TestFile.NAME_DATASET_FLOAT);
        } catch (final Exception ex) { 
            ; // Expected - intentional
        }

        try { 
            testGroup.setName(newName); 
        } catch (final Exception ex) { 
            fail("setName() failed. "+ ex);
        }
 
        // close the file and reopen it
        try {
            testFile.close();
            testFile.open();
            testGroup = (H5Group)testFile.get(newName);
        } catch (final Exception ex) { 
            fail("setName() failed. "+ ex);
        }
       
        // test the old name
        H5Group tmpDset = null;
        try {
            tmpDset = (H5Group)testFile.get(GNAME);
         } catch (final Exception ex) { 
             tmpDset = null; // Expected - intentional
        }
        assertNull("The dataset should be null because it has been renamed", tmpDset);

        // set back the original name
        try { 
            testGroup.setName(GNAME); 
        } catch (final Exception ex) { 
            fail("setName() failed. "+ ex);
        }
        
        // make sure the dataset is OK
        try {
            testGroup = (H5Group)testFile.get(GNAME);
        } catch (final Exception ex) { 
            fail("setName() failed. "+ ex);
        }
        assertNotNull(testGroup);
    }

    /**
     * Test method for {@link ncsa.hdf.object.h5.H5Group#setPath(java.lang.String)}.
     */
    public final void testSetPath() {
        final String newPath = "tmpName";

        try {
            testGroup.setPath(newPath);
        } catch (final Exception ex) { 
            fail("setPath() failed. "+ ex);
        }
    }

    /**
     * Test method for {@link ncsa.hdf.object.h5.H5Group#open()}.
     * <p>
     * What to test:
     *   <ul>
     *     <li> open a group identifier
     *     <li> Check if gid is valid
     *     <li> Close the group
     *     <li> Repeat all above
     *   </ul>
     */
    public final void testOpen() {
        int gid=-1;

        for (int loop=0; loop<NLOOPS; loop++) {
            gid=-1;
            try {
                gid = testGroup.open();
            } catch (final Exception ex) { 
                fail("open() failed. "+ ex);
            }
            
            assertTrue(gid > 0);
            
            testGroup.close(gid);
         }
     }

    /**
     * Test method for {@link ncsa.hdf.object.h5.H5Group#close(int)}.
     * <p>
     * What to test:
     *   <ul>
     *     <li> open a group identifier
     *     <li> Check if gid is valid
     *     <li> Close the group
     *     <li> Repeat all above
     *   </ul>
     */
    public final void testClose() {
        testOpen();
    }

    /**
     * Test method for {@link ncsa.hdf.object.h5.H5Group#clear()}.
     * <p>
     * What to test:
     * <ul>
     *   <li> Read attributes from file
     *   <li> clear the group
     *   <li> make sure that the attribute list is empty 
     * </ul>
     */
    public final void testClear() {
        Vector attrs = null;
        try { 
            attrs = (Vector)testGroup.getMetadata();
        } catch (final Exception ex) { 
            fail("clear() failed. "+ ex);
        }
        assertTrue(attrs.size()>0);

        // clear up the dataset
        testGroup.clear();
        
        // attribute is empty
        try { 
            attrs = (Vector)testGroup.getMetadata();
        } catch (final Exception ex) { 
            fail("clear() failed. "+ ex);
        }
        assertTrue(attrs.size() <= 0);
    }

    /**
     * Test method for {@link ncsa.hdf.object.h5.H5Group#H5Group(ncsa.hdf.object.FileFormat, java.lang.String, java.lang.String, ncsa.hdf.object.Group)}.
      * <p>
     * What to test:
     * <ul>
     *   <li> Construct an H5Group object that exits in file
     *   <ul>
     *     <li> new H5Group (file, null, fullpath, pgroup)
     *     <li> new H5Group (file, fullname, null, pgroup)
     *     <li> new H5Group (file, name, path, pgroup)
     *   </ul>
     *   <li> Construct an H5Group object that does not exist in file
     * </ul>
    */
    public final void testH5GroupFileFormatStringStringGroup() {
        Group pgroup = null;
        final String[] names = {null, GNAME_SUB, GNAME_SUB.substring(4)};
        final String[] paths = {GNAME_SUB, null, H5TestFile.NAME_GROUP};

        
        final H5File file = (H5File)testGroup.getFileFormat();
        assertNotNull(file);
        
        try {
            pgroup = (Group)testFile.get(H5TestFile.NAME_GROUP);
        } catch (final Exception ex) {
            fail("testFile.get() failed. "+ex);
        }
        assertNotNull(pgroup);
        
        for (int idx=0; idx<names.length; idx++) {
            final H5Group grp = new H5Group(file, names[idx], paths[idx], pgroup);
            final int gid = grp.open();
            assertTrue(gid>0);
            grp.close(gid);
        }
        
        final H5Group grp = new H5Group(file, "NO_SUCH_DATASET", "NO_SUCH_PATH", pgroup);
        final int gid = grp.open();
        assertTrue(gid <=0);
     }

    /**
     * Test method for {@link ncsa.hdf.object.h5.H5Group#H5Group(ncsa.hdf.object.FileFormat, java.lang.String, java.lang.String, ncsa.hdf.object.Group, long[])}.
      * <p>
     * What to test:
     * <ul>
     *   <li> Construct an H5Group object that exits in file
     *   <ul>
     *     <li> new H5Group (file, null, fullpath, pgroup, oid)
     *     <li> new H5Group (file, fullname, null, pgroup, oid)
     *     <li> new H5Group (file, name, path, pgroup, oid)
     *   </ul>
     *   <li> Construct an H5Group object that does not exist in file
     * </ul>
     */
    public final void testH5GroupFileFormatStringStringGroupLongArray() {
    	// RISHI SINHA Why are we testing a deprecated API.
        Group pgroup = null;
        final String[] names = {null, GNAME_SUB, GNAME_SUB.substring(4)};
        final String[] paths = {GNAME_SUB, null, H5TestFile.NAME_GROUP};

        
        final H5File file = (H5File)testGroup.getFileFormat(); // RISHI SINHA Why recreating these objects as we have these objects in this class already.
        assertNotNull(file);
        
        try {
            pgroup = (Group) testFile.get(H5TestFile.NAME_GROUP);
        } catch (final Exception ex) {
            fail("testFile.get() failed. "+ex);
        }
        assertNotNull(pgroup);
        
        long[] oid = null;
        for (int idx=0; idx<names.length; idx++) {
            
            try
            {
                final byte[] ref_buf = H5.H5Rcreate(file.getFID(), GNAME_SUB, HDF5Constants.H5R_OBJECT, -1);
                final long l = HDFNativeData.byteToLong(ref_buf, 0);
                oid = new long[1];
                oid[0] = l; // save the object ID
            } catch (final HDF5Exception ex) { 
                fail("H5.H5Rcreate() failed. "+ ex);
            }
            
            assertNotNull(oid);
            
            final H5Group grp = new H5Group(file, names[idx], paths[idx], pgroup, oid);
            final int gid = grp.open();
            assertTrue(gid>0);
            grp.close(gid);
        }
        
        // test a non-existing dataset
        final H5Group grp = new H5Group(file, "NO_SUCH_DATASET", "NO_SUCH_PATH", pgroup, null);
        final int gid = grp.open();
        assertTrue(gid <=0);
    }

    /**
     * Test method for {@link ncsa.hdf.object.h5.H5Group#getMetadata()}.
     * <p>
     * Cases tested:
     * <ul>
     *   <li> Get all the attributes
     *   <li> Check the content of the attributes
     * </ul>
     */
    public final void testGetMetadata() {
        Vector attrs = null;
        
        try {
            attrs = (Vector) testGroup.getMetadata();
        } catch (final Exception ex) { 
            fail("getMetadata() failed. "+ ex);
        }
        assertNotNull(attrs);
        assertTrue(attrs.size() > 0);
        
        final int n = attrs.size();
        for (int i=0; i<n; i++) {
            final Attribute attr = (Attribute) attrs.get(i);
            final H5Datatype dtype = (H5Datatype)attr.getType();
            if (dtype.getDatatypeClass() == Datatype.CLASS_STRING) {
                assertTrue(H5TestFile.ATTRIBUTE_STR.getName().equals(attr.getName()));
                assertTrue(((String[]) H5TestFile.ATTRIBUTE_STR.getValue())[0].equals(((String[])attr.getValue())[0]));
            } else if (dtype.getDatatypeClass() == Datatype.CLASS_INTEGER) {
                assertTrue(H5TestFile.ATTRIBUTE_INT_ARRAY.getName().equals(attr.getName()));
                final int[] expected = (int [])H5TestFile.ATTRIBUTE_INT_ARRAY.getValue();
                assertNotNull(expected);
                final int[] ints = (int[]) attr.getValue();
                assertNotNull(ints);
                for (int j =0; j<expected.length; j++) {
                    assertEquals(expected[j], ints[j]);
                }
            }
        } //for (int i=0; i<n; i++) {
    }

    /**
     * Test method for {@link ncsa.hdf.object.h5.H5Group#writeMetadata(java.lang.Object)}.
     * <p>
     * What to test:
     * <ul>
     *   <li> Update the value of an existing attribute
     *   <li> Attach a new attribute
     *   <li> Close and re-open file to check if the change is made in file
     *   <li> Restore to the orginal state
     * </ul>
     */
    public final void testWriteMetadata() {
        Vector attrs = null;
        Attribute attr = null;
        
        try {
            attrs = (Vector) testGroup.getMetadata();
        } catch (final Exception ex) { 
            fail("getMetadata() failed. "+ ex);
        }
        assertNotNull(attrs);
        assertTrue(attrs.size() > 0);
        
        // update existing attribute
        int n = attrs.size();
        for (int i=0; i<n; i++) {
            attr = (Attribute) attrs.get(i);
            final H5Datatype dtype = (H5Datatype)attr.getType();
            if (dtype.getDatatypeClass() == Datatype.CLASS_STRING) {
                final String[] strs = (String[]) attr.getValue();
                strs[0] = TEST_VALUE_STR;
            } else if (dtype.getDatatypeClass() == Datatype.CLASS_INTEGER) {
                final int[] ints = (int[]) attr.getValue();
                assertNotNull(ints);
                for (int j =0; j<ints.length; j++) {
                    ints[j] = TEST_VALUE_INT;
                }
            }
            try  {
                testGroup.writeMetadata(attr);
            } catch (final Exception ex) { 
                fail("writeMetadata() failed. "+ ex);
            }
        } //for (int i=0; i<n; i++) {
        
        // attache a new attribute
        attr = new Attribute(
                "float attribute", 
                typeFloat, new long[] {1},
                new float[] {TEST_VALUE_FLOAT});
        try  {
            testGroup.writeMetadata(attr);
        } catch (final Exception ex) { 
            fail("writeMetadata() failed. "+ ex);
        }

        // close the file and reopen it
        try {
            testGroup.clear();
            testFile.close();
            testFile.open();
            testGroup = (H5Group)testFile.get(GNAME);
        } catch (final Exception ex) { 
            fail("write() failed. "+ ex);
        }
        
        // check the change in file
        try {
            attrs = (Vector) testGroup.getMetadata();
        } catch (final Exception ex) { 
            fail("getMetadata() failed. "+ ex);
        }
        assertNotNull(attrs);
        assertTrue(attrs.size() > 0);
        
        n = attrs.size();
        Attribute newAttr = null;
        for (int i=0; i<n; i++) {
            attr = (Attribute) attrs.get(i);
            final H5Datatype dtype = (H5Datatype)attr.getType();
            if (dtype.getDatatypeClass() == Datatype.CLASS_STRING) {
                assertTrue(H5TestFile.ATTRIBUTE_STR.getName().equals(attr.getName()));
                assertTrue(TEST_VALUE_STR.equals(((String[])attr.getValue())[0]));
            } else if (dtype.getDatatypeClass() == Datatype.CLASS_INTEGER) {
                assertTrue(H5TestFile.ATTRIBUTE_INT_ARRAY.getName().equals(attr.getName()));
                final int[] ints = (int[]) attr.getValue();
                assertNotNull(ints);
                for (int j =0; j<ints.length; j++) {
                    assertEquals(TEST_VALUE_INT, ints[j]);
                }
            } else if (dtype.getDatatypeClass() == Datatype.CLASS_FLOAT) {
                newAttr = attr;
                final float[] floats = (float[]) attr.getValue();
                assertEquals(TEST_VALUE_FLOAT, floats[0], Float.MIN_VALUE);
            }
        } //for (int i=0; i<n; i++) {

        // remove the new attribute
        try {
            testGroup.removeMetadata(newAttr);
        } catch (final Exception ex) { 
            fail("removeMetadata() failed. "+ ex);
        }
        
        // set the value to original
        n = attrs.size();
        for (int i=0; i<n; i++) {
            attr = (Attribute) attrs.get(i);
            final H5Datatype dtype = (H5Datatype)attr.getType();
            if (dtype.getDatatypeClass() == Datatype.CLASS_STRING) {
                final String[] strs = (String[]) attr.getValue();
                strs[0] = ((String[]) H5TestFile.ATTRIBUTE_STR.getValue())[0];
            } else if (dtype.getDatatypeClass() == Datatype.CLASS_INTEGER) {
                final int[] ints = (int[]) attr.getValue();
                assertNotNull(ints);
                for (int j =0; j<ints.length; j++) {
                    final int[] expected = (int [])H5TestFile.ATTRIBUTE_INT_ARRAY.getValue();
                    ints[j] = expected[j];
                }
            }
            try  {
                testGroup.writeMetadata(attr);
            } catch (final Exception ex) { 
                fail("writeMetadata() failed. "+ ex);
            }
        } //for (int i=0; i<n; i++) {
    }

    /**
     * Test method for {@link ncsa.hdf.object.h5.H5Group#removeMetadata(java.lang.Object)}.
     * <p>
     * What to test:
     * <ul>
     *   <li> Remove all existing attributes
     *   <li> Close and reopen file to check if all attribute are removed from file
     *   <li> Restore to the orginal state
     * </ul>
     */
    public final void testRemoveMetadata() {
        Vector attrs = null;
        try {
            attrs = (Vector) testGroup.getMetadata();
        } catch (final Exception ex) { 
            fail("getMetadata() failed. "+ ex);
        }
        assertNotNull(attrs);
        assertTrue(attrs.size() > 0);
        
        // remove all attributes
        final int n = attrs.size();
        final Object[] arrayAttr = attrs.toArray();
        for (int i=0; i<n; i++) {
            try {
                testGroup.removeMetadata(arrayAttr[i]);
            } catch (final Exception ex) { 
                fail("removeMetadata() failed. "+ ex);
            }
         }
        
        // close the file and reopen it
        try {
            testGroup.clear();
            testFile.close();
            testFile.open();
            testGroup = (H5Group)testFile.get(GNAME);
        } catch (final Exception ex) { 
            fail("write() failed. "+ ex);
        }
        attrs = null;
        
        try {
            attrs = (Vector) testGroup.getMetadata();
        } catch (final Exception ex) { 
            fail("getMetadata() failed. "+ ex);
        }
        assertNotNull(attrs);
        assertFalse(attrs.size() > 0);

        // restor to the original
        try  {
            testGroup.writeMetadata(H5TestFile.ATTRIBUTE_STR);
            testGroup.writeMetadata(H5TestFile.ATTRIBUTE_INT_ARRAY);
        } catch (final Exception ex) { 
            fail("writeMetadata() failed. "+ ex);
        }
    }

    /**
     * Test method for {@link ncsa.hdf.object.h5.H5Group#create(java.lang.String, ncsa.hdf.object.Group)}.
     * <p>
     * What to test:
     * <ul>
     *   <li> Create a new group
     *   <li> Close and reopen the file
     *   <li> Check the new group
     *   <li> Restore to the orginal file (remove the new group)
     * </ul>
     */
    public final void testCreate() {
        Group grp = null;
        final String nameNew = "/tmpH5Group";
        try {
            final Group rootGrp = (Group)testFile.get("/");
            grp = H5Group.create(nameNew, rootGrp);
        } catch (final Exception ex) { 
            fail("H5Group.create failed. "+ ex);
        }
        assertNotNull(grp);
        
        try {
            testFile.close();
            testFile.open();
        } catch (final Exception ex) { 
            fail("testFile.get() failed. "+ ex);
        }

        try {
            grp = (Group)testFile.get(nameNew);
        } catch (final Exception ex) { 
            fail("testFile.get() failed. "+ ex);
        }
        assertNotNull(grp);
        
        try {
            testFile.delete(grp); // delete the new datast
         }  catch (final Exception ex) { 
            fail("testFile.delete() failed. "+ ex);
        }
        
        try {
            testFile.close();
            testFile.open();
        } catch (final Exception ex) { 
            fail("testFile.get() failed. "+ ex);
        }

        grp = null;
        try {
            grp = (Group)testFile.get(nameNew);
        } catch (final Exception ex) { 
            ; // Expected - intentional
        }
        assertNull(grp);
    }
    
}
