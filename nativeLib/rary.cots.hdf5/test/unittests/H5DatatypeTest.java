/**
 * 
 */
package test.unittests;

import java.util.Vector;

import ncsa.hdf.hdf5lib.H5;
import ncsa.hdf.hdf5lib.HDF5Constants;
import ncsa.hdf.object.Attribute;
import ncsa.hdf.object.Datatype;
import ncsa.hdf.object.FileFormat;
import ncsa.hdf.object.h5.H5Datatype;
import ncsa.hdf.object.h5.H5File;
import junit.framework.TestCase;

/**
 * TestCase for H5Datatype.
 * <p>
 * This class tests all the public methods in H5ScalarDS class.
 * <p>
 * The test file contains the following objects. 
 * <pre>
 * 
        /dataset_byte            Dataset {50, 10}
        /dataset_comp            Dataset {50, 10}
        /dataset_enum            Dataset {50, 10}
        /dataset_float           Dataset {50, 10}
        /dataset_image           Dataset {50, 10}
        /dataset_int             Dataset {50, 10}
        /dataset_str             Dataset {50, 10}
        /g0                      Group
        /g0/dataset_comp         Dataset {50, 10}
        /g0/dataset_int          Dataset {50, 10}
        /g0/datatype_float       Type
        /g0/datatype_int         Type
        /g0/datatype_str         Type
        /g0/g00                  Group
        /g0/g00/dataset_float    Dataset {50, 10}
        /g0_attr                 Group
 * </pre>
 * <p>
 * We use the following template to test all the methods:
 * <p>
     * What to test:
     * <ul> 
     *   <li> Test for boundary conditions
     *   <ul>
     *     <li>
     *   </ul>
     *   <li> Test for failure
     *   <ul>
     *     <li>
     *   </ul>
     *   <li> Test for success on general functionality
     *   <ul>
     *     <li> 
     *   </ul>
     * </ul>
 * 
 * @author Peter Cao, The HDF Group
 */
public class H5DatatypeTest extends TestCase {
    private static final H5File H5FILE = new H5File();
    private static final int NLOOPS = 10;
    private static final int TEST_VALUE_INT = Integer.MAX_VALUE;
    private static final float TEST_VALUE_FLOAT = Float.MAX_VALUE;
    private static final String TEST_VALUE_STR = "Test";
    private static final String TNAME = H5TestFile.NAME_DATATYPE_INT;
     
    private H5Datatype typeInt = null;
    private H5Datatype typeUInt = null;
    private H5Datatype typeFloat = null;
    private H5Datatype typeStr = null;
    private H5File testFile = null;
    private H5Datatype testDatatype = null;

    /**
     * @param arg0
     */
    public H5DatatypeTest(final String arg0) {
        super(arg0);
    }

    /* (non-Javadoc)
     * @see junit.framework.TestCase#setUp()
     */
    protected void setUp() throws Exception {
        super.setUp();

        typeInt = new H5Datatype(Datatype.CLASS_INTEGER, H5TestFile.DATATYPE_SIZE, -1, -1);
        typeUInt = new H5Datatype(Datatype.CLASS_INTEGER, H5TestFile.DATATYPE_SIZE, -1, Datatype.SIGN_NONE);
        typeFloat = new H5Datatype(Datatype.CLASS_FLOAT, H5TestFile.DATATYPE_SIZE, -1, -1);
        typeStr = new H5Datatype(Datatype.CLASS_STRING, H5TestFile.STR_LEN, -1, -1);

        testFile = (H5File)H5FILE.open(H5TestFile.NAME_FILE_H5, FileFormat.WRITE);
        assertNotNull(testFile);

        testFile.open();

        testDatatype = (H5Datatype)testFile.get(TNAME);
        assertNotNull(testDatatype);
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
     * Test method for {@link ncsa.hdf.object.h5.H5Datatype#open()}.
     * <p>
     * What to test:
     *   <ul>
     *     <li> Open a datatype identifier
     *     <li> Check the class and size of the datatype
     *     <li> Close the datatype
     *     <li> Repeat all above
     *   </ul>
     */
    public final void testOpen() {
        int tid=-1, tclass=-1, tsize=-1;

        for (int loop=0; loop<NLOOPS; loop++) {
            tid = tclass = tsize = -1;
            
            try {
                tid = testDatatype.open();
            } catch (final Exception ex) { 
                fail("open() failed. "+ ex);
            }
            assertTrue(tid > 0);
            
            try {
                tclass = H5.H5Tget_class(tid);
                tsize = H5.H5Tget_size(tid);
            } catch (final Exception ex) { 
                fail("open() failed. "+ ex);
            }
           
            assertEquals(4, tsize);
            assertEquals(HDF5Constants.H5T_INTEGER, tclass);
             
            try { H5.H5Tclose(tid); } catch (final Exception ex) {}
         }
     }

    /**
     * Test method for {@link ncsa.hdf.object.h5.H5Datatype#close(int)}.
     * <p>
     * What to test:
     *   <ul>
     *     <li> Open a datatype identifier
     *     <li> Check the class and size of the datatype
     *     <li> Close the datatype
     *     <li> Access the closed datatype (expect to fail)
     *     <li> Repeat all above
     *   </ul>
    */
    public final void testClose() {
        int tid=-1, tclass=-1, tsize=-1;

        for (int loop=0; loop<NLOOPS; loop++) {
            tid = tclass = tsize = -1;
            
            try {
                tid = testDatatype.open();
            } catch (final Exception ex) { 
                fail("open() failed. "+ ex);
            }
            assertTrue(tid > 0);
            
            try {
                tclass = H5.H5Tget_class(tid);
                tsize = H5.H5Tget_size(tid);
            } catch (final Exception ex) { 
                fail("open() failed. "+ ex);
            }
           
            assertEquals(4, tsize);
            assertEquals(HDF5Constants.H5T_INTEGER, tclass);
             
            try { H5.H5Tclose(tid); } catch (final Exception ex) {}
            
            try {
                tclass = H5.H5Tget_class(tid);
            }  catch (final Exception ex) { 
                ; // Expected - intentional
            }
         }
    }

    /**
     * Test method for {@link ncsa.hdf.object.h5.H5Datatype#toNative()}.
     * <p>
     * What to test:
     *   <ul>
     *     <li> Create integer, float and string datatypes in memory
     *     <li> Call toNative() to get datatype identifiers
     *     <li> Check the classes and sizes of the datatypes from toNative()
     *   </ul>
     */
    public final void testToNative() {
        int tid=-1, tclass=-1, tsize=-1;

        // test integer datatype
        try {
            tid = typeInt.toNative();
        } catch (final Exception ex) { 
            fail("testToNative() failed. "+ ex);
        }
        try {
            tclass = H5.H5Tget_class(tid);
            tsize = H5.H5Tget_size(tid);
        } catch (final Exception ex) { 
            fail("open() failed. "+ ex);
        }
        assertEquals(4, tsize);
        assertEquals(HDF5Constants.H5T_INTEGER, tclass);
        try { H5.H5Tclose(tid); } catch (final Exception ex) {}
        
        // test float datatype
        try {
            tid = typeFloat.toNative();
        } catch (final Exception ex) { 
            fail("testToNative() failed. "+ ex);
        }
        try {
            tclass = H5.H5Tget_class(tid);
            tsize = H5.H5Tget_size(tid);
        } catch (final Exception ex) { 
            fail("open() failed. "+ ex);
        }
        assertEquals(4, tsize);
        assertEquals(HDF5Constants.H5T_FLOAT, tclass);
        try { H5.H5Tclose(tid); } catch (final Exception ex) {}
       
        // test String datatype
        try {
            tid = typeStr.toNative();
        } catch (final Exception ex) { 
            fail("testToNative() failed. "+ ex);
        }
        try {
            tclass = H5.H5Tget_class(tid);
            tsize = H5.H5Tget_size(tid);
        } catch (final Exception ex) { 
            fail("open() failed. "+ ex);
        }
        assertEquals(H5TestFile.STR_LEN, tsize);
        assertEquals(HDF5Constants.H5T_STRING, tclass);
        
        try { H5.H5Tclose(tid); } catch (final Exception ex) {}
    }

    /**
     * Test method for {@link ncsa.hdf.object.h5.H5Datatype#fromNative(int)}.
     * <p>
     * What to test:
     *   <ul>
     *     <li> Create empty datatype object in memory
     *     <li> Test faillure on the class and size of the datatype
     *     <li> Open datatype identifier from file
     *     <li> Call fromNative(int tid) to fill the datatype object
     *     <li> Check the class and size of the datatype
     *   </ul>
     */
    public final void testFromNative() {
        int tid=-1;
        final H5Datatype type = new H5Datatype(-1);
        
        assertFalse(Datatype.CLASS_INTEGER==type.getDatatypeClass());
        assertFalse(type.getDatatypeSize()==4);
        
        try
        {
            tid = H5.H5Topen(testFile.getFID(), TNAME);
        } catch (final Exception ex) { 
            fail("H5Topen() failed. "+ ex);
        }
        assertTrue(tid>0);
        
        type.fromNative(tid);
        
        assertTrue(Datatype.CLASS_INTEGER==type.getDatatypeClass());
        assertTrue(type.getDatatypeSize()==4);
        
        try { H5.H5Tclose(tid); } catch (final Exception ex) {}
    }

    /**
     * Test method for {@link ncsa.hdf.object.h5.H5Datatype#isUnsigned()}.
      * <p>
     * What to test:
     *   <ul>
     *     <li> Check unsigned integer datatype
     *     <li> check signed integer datatype
     *     <li> Check non-integer datatype
     *   </ul>
    */
    public final void testIsUnsigned() {
        assertFalse(typeInt.isUnsigned());
        assertFalse(typeFloat.isUnsigned());
        assertFalse(typeStr.isUnsigned());
        assertTrue(typeUInt.isUnsigned());
    }

    /**
     * Test method for {@link ncsa.hdf.object.h5.H5Datatype#getMetadata()}.
     * <p>
     * What to test:
     * <ul>
     *   <li> Get all the attributes
     *   <li> Check the content of the attributes
     * </ul>
      */
    public final void testGetMetadata() {
        Vector attrs = null;
        
        try {
            attrs = (Vector) testDatatype.getMetadata();
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
     * Test method for {@link ncsa.hdf.object.h5.H5Datatype#writeMetadata(java.lang.Object)}.
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
            attrs = (Vector) testDatatype.getMetadata();
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
                testDatatype.writeMetadata(attr);
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
            testDatatype.writeMetadata(attr);
        } catch (final Exception ex) { 
            fail("writeMetadata() failed. "+ ex);
        }

        // close the file and reopen it
        try {
            testFile.close();
            testFile.open();
            testDatatype = (H5Datatype)testFile.get(TNAME);
        } catch (final Exception ex) { 
            fail("write() failed. "+ ex);
        }
        
        // check the change in file
        try {
            attrs = (Vector) testDatatype.getMetadata();
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
            testDatatype.removeMetadata(newAttr);
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
                testDatatype.writeMetadata(attr);
            } catch (final Exception ex) { 
                fail("writeMetadata() failed. "+ ex);
            }
        } //for (int i=0; i<n; i++) {
    }

    /**
     * Test method for {@link ncsa.hdf.object.h5.H5Datatype#removeMetadata(java.lang.Object)}.
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
            attrs = (Vector) testDatatype.getMetadata();
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
                testDatatype.removeMetadata(arrayAttr[i]);
            } catch (final Exception ex) { 
                fail("removeMetadata() failed. "+ ex);
            }
         }
        
        // close the file and reopen it
        try {
            testFile.close();
            testFile.open();
            testDatatype = (H5Datatype)testFile.get(TNAME);
        } catch (final Exception ex) { 
            fail("write() failed. "+ ex);
        }
        attrs = null;
        
        try {
            attrs = (Vector) testDatatype.getMetadata();
        } catch (final Exception ex) { 
            fail("getMetadata() failed. "+ ex);
        }
        assertNotNull(attrs);
        assertFalse(attrs.size() > 0);

        // restor to the original
        try  {
            testDatatype.writeMetadata(H5TestFile.ATTRIBUTE_STR);
            testDatatype.writeMetadata(H5TestFile.ATTRIBUTE_INT_ARRAY);
        } catch (final Exception ex) { 
            fail("writeMetadata() failed. "+ ex);
        }
    }
}
