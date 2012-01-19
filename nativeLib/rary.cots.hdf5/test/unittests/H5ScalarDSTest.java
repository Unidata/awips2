/**
 * 
 */
package test.unittests;

import java.util.Vector;
import java.lang.reflect.Array;
import ncsa.hdf.hdf5lib.H5;
import ncsa.hdf.hdf5lib.HDF5Constants;
import ncsa.hdf.hdf5lib.HDFNativeData;
import ncsa.hdf.hdf5lib.exceptions.HDF5Exception;
import ncsa.hdf.object.Attribute;
import ncsa.hdf.object.Dataset;
import ncsa.hdf.object.Datatype;
import ncsa.hdf.object.FileFormat;
import ncsa.hdf.object.Group;
import ncsa.hdf.object.ScalarDS;
import ncsa.hdf.object.h5.H5ScalarDS;
import ncsa.hdf.object.h5.H5Datatype;
import ncsa.hdf.object.h5.H5File;
import junit.framework.TestCase;

/**
 * TestCase for H5ScalarDS.
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
public class H5ScalarDSTest extends TestCase {
    private static final H5File H5FILE = new H5File();
    private static final int NLOOPS = 10;
    private static final int TEST_VALUE_INT = Integer.MAX_VALUE;
    private static final float TEST_VALUE_FLOAT = Float.MAX_VALUE;
    private static final String TEST_VALUE_STR = "H5ScalarDSTest";
    private static final String DNAME = H5TestFile.NAME_DATASET_INT;
    private static final String DNAME_SUB = H5TestFile.NAME_DATASET_INT_SUB;
    
    private H5Datatype typeInt = null;
    private H5Datatype typeFloat = null;
    private H5Datatype typeStr = null;
    private H5File testFile = null;
    private H5ScalarDS testDataset = null;

    /**
     * @param arg0
     */
    public H5ScalarDSTest(final String arg0) {
        super(arg0);
    }

    /* (non-Javadoc)
     * @see junit.framework.TestCase#setUp()
     */
    protected void setUp() throws Exception {
        super.setUp();

        typeInt = new H5Datatype(Datatype.CLASS_INTEGER, H5TestFile.DATATYPE_SIZE, -1, -1);
        typeFloat = new H5Datatype(Datatype.CLASS_FLOAT, H5TestFile.DATATYPE_SIZE, -1, -1);
        typeStr = new H5Datatype(Datatype.CLASS_STRING, H5TestFile.STR_LEN, -1, -1);

        testFile = (H5File)H5FILE.open(H5TestFile.NAME_FILE_H5, FileFormat.WRITE);
        assertNotNull(testFile);

        testFile.open();

        testDataset = (H5ScalarDS)testFile.get(DNAME);
        assertNotNull(testDataset);
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
     * Test method for {@link ncsa.hdf.object.h5.H5ScalarDS#setName(java.lang.String)}.
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
            testDataset.setName(null);
        } catch (final Exception ex) { 
            ; // Expected - intentional
        }
       
        // set to an existing name
        try {
            testDataset.setName(H5TestFile.NAME_DATASET_FLOAT);
        } catch (final Exception ex) { 
            ; // Expected - intentional
        }

        try { 
            testDataset.setName(newName); 
        } catch (final Exception ex) { 
            fail("setName() failed. "+ ex);
        }
 
        // close the file and reopen it
        try {
            testFile.close();
            testFile.open();
            testDataset = (H5ScalarDS)testFile.get(newName);
        } catch (final Exception ex) { 
            fail("setName() failed. "+ ex);
        }
       
        // test the old name
        H5ScalarDS tmpDset = null;
        try {
            tmpDset = (H5ScalarDS)testFile.get(DNAME);
         } catch (final Exception ex) { 
             tmpDset = null; // Expected - intentional
        }
        assertNull("The dataset should be null because it has been renamed", tmpDset);

        // set back the original name
        try { 
            testDataset.setName(DNAME); 
        } catch (final Exception ex) { 
            fail("setName() failed. "+ ex);
        }
        
        // make sure the dataset is OK
        try {
            testDataset = (H5ScalarDS)testFile.get(DNAME);
        } catch (final Exception ex) { 
            fail("setName() failed. "+ ex);
        }
        assertNotNull(testDataset);
    }
    
    /**
     * Test method for {@link ncsa.hdf.object.h5.H5ScalarDS#open()}.
     * <p>
     * What to test:
     *   <ul>
     *     <li> open a dataset identifier
     *     <li> get datatype and dataspace identifier for the dataset
     *     <li> Repeat all above
     *   </ul>
     */
    public final void testOpen() {
        int did=-1, tid=-1, sid=-1;

        for (int loop=0; loop<NLOOPS; loop++) {
            did= tid= sid=-1;
            try {
                did = testDataset.open();
                tid = H5.H5Dget_type(did);
                sid = H5.H5Dget_space(did);
            } catch (final Exception ex) { 
                fail("open() failed. "+ ex);
            }
            
            assertTrue(did > 0);
            assertTrue(tid > 0);
            assertTrue(sid > 0);
            
            try {
                H5.H5Tclose(tid);
            } catch (final Exception ex) {}
            try {
                H5.H5Sclose(sid);
            } catch (final Exception ex) {}
            try {
                H5.H5Dclose(did);
            } catch (final Exception ex) {}
         }
     }
    
    /**
     * Test method for {@link ncsa.hdf.object.h5.H5ScalarDS#close(int)}.
     * <p>
     * What to test:
     * <ul>
     *   <li> open a dataset identifier
     *   <li> get datatype and dataspace identifier for the dataset
     *   <li> close dataset
     *   <li> failure test for the closed did 
     *   <li> Repeat all above
     * </ul>
     */
    public final void testClose() {
        int did=-1, tid=-1, sid=-1;
        
        for (int loop=0; loop<NLOOPS; loop++) {
            did= tid= sid=-1;
            try {
                did = testDataset.open();
                tid = H5.H5Dget_type(did);
                sid = H5.H5Dget_space(did);
            } catch (final Exception ex) { 
                fail("open() failed. "+ ex);
            }
            
            assertTrue(did > 0);
            assertTrue(tid > 0);
            assertTrue(sid > 0);
            
            try {
                H5.H5Tclose(tid);
            } catch (final Exception ex) {}
            try {
                H5.H5Sclose(sid);
            } catch (final Exception ex) {}
            
            try { 
                testDataset.close(did);
            } catch (final Exception ex) { 
                fail("close() failed. "+ ex);
            }
            
            // dataset is closed, expect to fail
            try {
                tid = H5.H5Dget_type(did);
            } catch (final Exception ex) { 
                tid = -1; // Expected - intentional
            }
            assertTrue(tid < 0);
            
            try {
                sid = H5.H5Dget_space(did);
            } catch (final Exception ex) { 
                sid = -1; // Expected - intentional
            }
            assertTrue(sid < 0);
        }
    }

    /**
     * Test method for {@link ncsa.hdf.object.h5.H5ScalarDS#clear()}.
     * <p>
     * What to test:
     * <ul>
     *   <li> Read data/attributes from file
     *   <li> clear the dataet
     *   <li> make sure that the data is empty
     *   <li> make sure that the attribute list is empty 
     * </ul>
     */
    public final void testClear() {
        Object data = null;
        
        try {
            data = testDataset.getData();
        } catch (final Exception ex) { 
            fail("getData() failed. "+ ex);
        }
        assertNotNull(data);
        assertTrue(Array.getLength(data) > 0);

        Vector attrs = null;
        try { 
            attrs = (Vector)testDataset.getMetadata();
        } catch (final Exception ex) { 
            fail("clear() failed. "+ ex);
        }

        // clear up the dataset
        testDataset.clear();
        
        // attribute is empty
        try { 
            attrs = (Vector)testDataset.getMetadata();
        } catch (final Exception ex) { 
            fail("clear() failed. "+ ex);
        }
        assertTrue(attrs.size() <= 0);
    }

    /**
     * Test method for {@link ncsa.hdf.object.h5.H5ScalarDS#init()}.
     * <p>
     * What to test:
     * <ul>
     *   <li> call init()
     *   <li> Select a subset
     *   <li> Repeat all above
     * </ul>
     */
    public final void testInit() {
        for (int loop=0; loop<NLOOPS; loop++) {

            try { testFile.close(); } catch (final Exception ex) { }
           
            try {
                testFile.open();
                
                testDataset = (H5ScalarDS)testFile.get(DNAME);
            } catch (final Exception ex) { 
                fail("setName() failed. "+ ex);
            }
           
            testDataset.init();

            // test the rank
            final int rank = testDataset.getRank();
            assertEquals(H5TestFile.RANK, rank);
            
            // test the dimesin sizes
            final long[] dims = testDataset.getDims();
            assertNotNull(dims);
            for (int i=0; i<rank; i++) {
                assertEquals(H5TestFile.DIMs[i], dims[i]);
            }
            
            // start at 0
            final long[] start = testDataset.getStartDims();
            assertNotNull(start);
            for (int i=0; i<rank; i++) {
                assertEquals(0, start[i]);
            }
           
            // test selection
            final long[] selectedDims = testDataset.getSelectedDims();
            final int[] selectedIndex = testDataset.getSelectedIndex();
            assertNotNull(selectedDims);
            assertNotNull(selectedIndex);
            if (rank == 1)
            {
                assertEquals(0, selectedIndex[0]);
                assertEquals(dims[0], selectedDims[0]);
            }
            else if (rank == 2)
            {
                assertEquals(0, selectedIndex[0]);
                assertEquals(1, selectedIndex[1]);
                assertEquals(dims[0], selectedDims[0]);
                assertEquals(dims[1], selectedDims[1]);
            }
            else if (rank > 2)
            {
                assertEquals(rank-2, selectedIndex[0]); // columns
                assertEquals(rank-1, selectedIndex[1]); // rows
                assertEquals(rank-3, selectedIndex[2]);
                assertEquals(dims[rank-1], selectedDims[rank-1]);
                assertEquals(dims[rank-2], selectedDims[rank-2]);
            }
            
            int nObjs = 0;
            try { nObjs = H5.H5Fget_obj_count(testFile.getFID(), HDF5Constants.H5F_OBJ_ALL); }
            catch (final Exception ex) { fail("H5.H5Fget_obj_count() failed. "+ ex);   }
            assertEquals(1, nObjs); // file id should be the only one left open
            
            try { testFile.close(); } catch (final Exception ex) {}
            
         } //for (int loop=0; loop<NLOOPS; loop++)
    } //public final void testInit() {
    

    /**
     * Test method for {@link ncsa.hdf.object.h5.H5ScalarDS#read()}.
     * <p>
     * What to test:
     * <ul>
     *   <li> Read the whole dataset of the test dataset
     *   <li> read a subset of the test dataset
     *   <li> Repeat all above
     *   <li> Read all types scalar datasets
     *   
     * </ul>
     */
    public final void testRead() 
    {
        for (int loop=0; loop<NLOOPS; loop++) 
        {
            testDataset.init();
            int[] ints = null;
            
            // read the whole dataset
            try {
                ints = (int[])testDataset.getData();
            } catch (final Exception ex) {
                fail("testDataset.getData() failed. "+ex);
            }
            assertNotNull(ints);
            
            // check the data content
            for (int i=0; i<ints.length; i++) {
                assertEquals(H5TestFile.DATA_INT[i], ints[i]);
            }
        } // for (int loop=0; loop<NLOOPS; loop++)
        
        try { 
            testFile.close();
        } catch (final Exception ex) {}
        
        // read all types of scalar datasets
        Dataset dset =null;
        final String dnames[] = {
                H5TestFile.NAME_DATASET_CHAR, H5TestFile.NAME_DATASET_ENUM,
                H5TestFile.NAME_DATASET_FLOAT, H5TestFile.NAME_DATASET_IMAGE,
                H5TestFile.NAME_DATASET_INT, H5TestFile.NAME_DATASET_STR,
                H5TestFile.NAME_DATASET_INT_SUB, H5TestFile.NAME_DATASET_FLOAT_SUB_SUB};
        
        for (int i=0; i<NLOOPS; i++)
        {
            final H5File file = new H5File(H5TestFile.NAME_FILE_H5, FileFormat.READ);
            
            try {
                // datasets
                for (int j=0; j<dnames.length; j++) {
                    dset = (Dataset)file.get(dnames[j]);
                    final Object data = dset.getData();
                }
            } catch (final Exception ex) { 
                 fail("file.get() failed. "+ ex);
            }
            
            int nObjs = 0;
            try { nObjs = H5.H5Fget_obj_count(file.getFID(), HDF5Constants.H5F_OBJ_ALL); }
            catch (final Exception ex) { fail("H5.H5Fget_obj_count() failed. "+ ex);   }
            assertEquals(1, nObjs); // file id should be the only one left open
            
            try {            
                file.close();
            } catch (final Exception ex) { 
                 fail("file.close() failed. "+ ex);
            }
        } // for (int i=0; i<NLOOPS; i++)
    }

    /**
     * Test method for {@link ncsa.hdf.object.h5.H5ScalarDS#read()}.
     * <p>
     * What to test:
     * <ul>
     *   <li> Read data row by row
     *   <li> Repeat all above
     * </ul>
     */
    public final void testReadByRow() {
        int[] data = null;

        for (int loop=0; loop<NLOOPS; loop++) {
            testDataset.init();

            // read data row by row
            final int nrows = testDataset.getHeight();
            for (int i=0; i<nrows; i++) {
                testDataset.clearData();
                testDataset.init();
                
                final int rank = testDataset.getRank();
                final long[] start = testDataset.getStartDims();
                final long[] count = testDataset.getSelectedDims();
               
                // select one row only
                for (int j=0; j<rank; j++) {
                    count[j] = 1;
                }
                
                // select different rows
                start[0] = i;
                
                try {
                    data = (int[])testDataset.getData();
                } catch (final Exception ex) { 
                    fail("getData() failed. "+ ex);
                }
                assertNotNull(data);
                
                final int idx = (int)H5TestFile.DIM2*i;
                assertEquals(H5TestFile.DATA_INT[idx], data[0]);
             } // for (int i=0; i<nrows; i++) {
        } //for (int loop=0; loop<NLOOPS; loop++) {
    }
    
    /**
     * Test method for {@link ncsa.hdf.object.h5.H5ScalarDS#readBytes()}.
     * <p>
     * What to test:
     * <ul>
     *   <li> Read the whole dataset in a byte buffer
     *   <li> check the data size
     * </ul>
     */
    public final void testReadBytes() {
        byte[] data = null;
        
        try {
            data = testDataset.readBytes();
        } catch (final Exception ex) { 
            fail("readBytes() failed. "+ ex);
        }
        assertNotNull(data);
        
        final int n = Array.getLength(data);
        final int expected = H5TestFile.DIM_SIZE * 4;
        
        assertEquals(expected, n);
    }

    /**
     * Test method for {@link ncsa.hdf.object.h5.H5ScalarDS#write(java.lang.Object)}.
     * <p>
     * What to test:
     * <ul>
     *   <li> Read/write the whole dataset
     *   <li> Repeat all above
     *   <li> write the original data back to file
     * </ul>
     */
    public final void testWriteObject() {
        int[] data = null;

        for (int loop=0; loop<NLOOPS; loop++) {
            // read the whole dataset by default
            testDataset.init();

            try {
                data = (int[])testDataset.getData();
            } catch (final Exception ex) { 
                fail("getData() failed. "+ ex);
            }
            assertNotNull(data);
            assertEquals(H5TestFile.DIM_SIZE, Array.getLength(data));
            
            // change the data value
            for (int i=0; i<H5TestFile.DIM_SIZE; i++) {
                data[i] = TEST_VALUE_INT;
            }
            
            // write the data to file
            try { 
                testDataset.write(data);
            }  catch (final Exception ex) { 
                fail("write() failed. "+ ex);
            }
            
            // close the file and reopen it
            try {
                testFile.close();
                testFile.open();
                testDataset = (H5ScalarDS)testFile.get(DNAME);
            } catch (final Exception ex) { 
                fail("write() failed. "+ ex);
            }
            
            // read the data into memory to make sure the data is correct
            testDataset.init();
            testDataset.clearData();
            
            try {
                data = (int[])testDataset.getData();
            } catch (final Exception ex) { 
                fail("getData() failed. "+ ex);
            }
            assertNotNull(data);
            assertEquals(H5TestFile.DIM_SIZE, Array.getLength(data));
            
            // check the data values
            for (int i=0; i<H5TestFile.DIM_SIZE; i++) {
                assertEquals(TEST_VALUE_INT, data[i]);
            }
            
            // write the original data into file
            try { 
                testDataset.write(H5TestFile.DATA_INT);
            }  catch (final Exception ex) { 
                fail("write() failed. "+ ex);
            }
        } //for (int loop=0; loop<NLOOPS; loop++) {
     }

    /**
     * Test method for {@link ncsa.hdf.object.h5.H5ScalarDS#write(java.lang.Object)}.
     * <p>
     * What to test:
     * <ul>
     *   <li> Read/write a subset of dataset
     *   <li> Repeat all above
     *   <li> write the original data back to file
     * </ul>
     */
    public final void testWriteSubset() {
        int[] data=null;

        for (int loop=0; loop<NLOOPS; loop++) {
            // read the whole dataset by default
            testDataset.init();

            // write a subset: the first half of the dataset
            final int rank = testDataset.getRank();
            final long[] dims = testDataset.getDims();
            long[] count = testDataset.getSelectedDims();
            
            // select the first 1/2 of the datast
            long size = 1;
            for (int j=0; j<rank; j++) {
                count[j] = dims[j]/2;
                size *= count[j];
            }
            
            data = new int[(int)size];
            for (int j=0; j<size; j++) {
                data[j] = TEST_VALUE_INT;
            }

            // write the data to file
            try { 
                testDataset.write(data);
            }  catch (final Exception ex) { 
                fail("write() failed. "+ ex);
            }
            
            // close the file and reopen it
            try {
                testFile.close();
                testFile.open();
                testDataset = (H5ScalarDS)testFile.get(DNAME);
            } catch (final Exception ex) { 
                fail("write() failed. "+ ex);
            }
            
            // read the data into memory to make sure the data is correct
            testDataset.init();
            testDataset.clearData();
            
            // select the first 1/2 of the datast
            count = testDataset.getSelectedDims();
            for (int j=0; j<rank; j++) {
                count[j] = dims[j]/2;
            }
            
            try {
                data = (int[])testDataset.getData();
            } catch (final Exception ex) { 
                fail("getData() failed. "+ ex);
            }
            assertNotNull(data);
            assertEquals(size, Array.getLength(data));
            
            // check the data values
            for (int i=0; i<size; i++) {
                assertEquals(TEST_VALUE_INT, data[i]);
            }
            
            // write the original data into file
            for (int j=0; j<rank; j++) {
                count[j] = dims[j];
            }
            try { 
                testDataset.write(H5TestFile.DATA_INT);
            }  catch (final Exception ex) { 
                fail("write() failed. "+ ex);
            }
        } //for (int loop=0; loop<NLOOPS; loop++) {
     }
    
    /**
     * Test method for {@link ncsa.hdf.object.h5.H5ScalarDS#write(java.lang.Object)}.
     * <p>
     * What to test:
     * <ul>
     *   <li> Read/write a subset of null strings
     *   <li> Repeat all above
     *   <li> write the original data back to file
     * </ul>
     */
    public final void testReadWriteNullStr() {
        String[] data = null;
        String[] nullStrs = null;
        H5ScalarDS dset = null;
        
        try {
            dset = (H5ScalarDS)testFile.get(H5TestFile.NAME_DATASET_STR);
            dset.init();
        } catch (Exception ex) { dset = null;}
        assertNotNull(dset);
        
        try {
            data = (String[]) dset.getData();
        } catch (Exception ex) {data = null; }
        assertNotNull(data);
        assertTrue(data.length>0);
        
        nullStrs = new String[data.length];
        
        for (int i=0; i<data.length; i++)
            nullStrs[i] = null;
        
        // write null strings
        try { 
            dset.write(nullStrs);
        } catch (Exception ex) {
            fail("Write null strings failed. "+ex);
        }
        
        // read null strings
        try { 
            dset.clearData();
            nullStrs = (String[]) dset.read();
        } catch (Exception ex) {
            fail("Read null strings failed. "+ex);
            nullStrs = null;
        }
        assertNotNull(nullStrs);
        
        // make sure all the strings are empty
        for (int i=0; i<data.length; i++) {
            assertNotNull(nullStrs[i]);
            assertTrue(nullStrs[i].length()==0);
        }

        // restore to the original state
        try { 
            dset.write(data);
        } catch (Exception ex) {
            fail("Write null strings failed. "+ex);
        }
     
        // read data back and check it is to the original state
        try { 
            dset.clearData();
            nullStrs = (String[]) dset.read();
        } catch (Exception ex) {
            fail("Read null strings failed. "+ex);
            nullStrs = null;
        }
        assertNotNull(nullStrs);
        for (int i=0; i<data.length; i++)
            assertTrue(data[i].equals(nullStrs[i]));
     }
    
    /**
     * Test method for {@link ncsa.hdf.object.h5.H5ScalarDS#copy(ncsa.hdf.object.Group, java.lang.String, long[], java.lang.Object)}.
     * <p>
     * What to test:
     * <ul>
     *   <li> Copy all scalar datasets to a new file
     *   <li> Check the content of new datasts
     *   <li> Repeat all above
      * </ul>
     */
    public final void testCopy() {
        int nObjs=0;
        Dataset dset=null, dsetNew=null;
        H5File tmpFile = null;
        
        final String DNAMES[] = {
                H5TestFile.NAME_DATASET_CHAR, H5TestFile.NAME_DATASET_ENUM,
                H5TestFile.NAME_DATASET_FLOAT, H5TestFile.NAME_DATASET_IMAGE,
                H5TestFile.NAME_DATASET_INT, H5TestFile.NAME_DATASET_STR};
       
        try { testFile.close(); } catch (final Exception ex) { ; }
        
        for (int loop=0; loop<NLOOPS; loop++)
        {
            tmpFile = new H5File("H5ScalarDS_testCopy.h5", FileFormat.CREATE);

            try {
                // test two open options: open full tree or open individual object only
                 for (int openOption=0; openOption<2; openOption++)
                {
                    nObjs = 0;
                    if (openOption == 0) {
                        try { 
                            testFile.open(); // opent the full tree
                        } catch (final Exception ex) { 
                            System.err.println("file.open(). "+ ex);
                        }
                    }
                      
                    try {
                        final Group rootGrp = (Group) tmpFile.get("/");
                        
                        // datasets
                        for (int j=0; j<DNAMES.length; j++) {
                            dset = (Dataset)testFile.get(DNAMES[j]);
                            dset.init();
                            final Object data = dset.getData();
                            dset.write(data);
                            dset.getMetadata();
                            
                            // copy data into a new datast
                            if (dset instanceof ScalarDS) {
                                dsetNew = dset.copy(rootGrp, DNAMES[j]+"_copy"+openOption, H5TestFile.DIMs, data);
                                assertNotNull(dsetNew);
                                final Object dataCopy = dsetNew.getData();
                                final int size = Array.getLength(data);
                                for (int k=0; k<size; k++) {
                                    assertEquals(Array.get(data, k), Array.get(dataCopy, k));
                                }
                            }
                        }
                    } catch (final Exception ex) { 
                         fail("file.get(). "+ ex);
                    }
         
                    nObjs = 0;
                    try { nObjs = H5.H5Fget_obj_count(tmpFile.getFID(), HDF5Constants.H5F_OBJ_ALL); }
                    catch (final Exception ex) { ; }
                    if (nObjs > 1) {
                        fail("Possible memory leak. Some objects are still open.");
                    }
                    
                    try {            
                        tmpFile.close();
                    } catch (final Exception ex) { 
                         System.err.println("file.close() failed. "+ ex);
                    }
                } // for (int openOption=0; openOption<2; openOption++)
            } finally {
                // delete the testing file
                if (tmpFile != null) {
                    tmpFile.delete();
                }
            }
        } //for (int loop=0; loop<NLOOPS; loop++) {
    }

    /**
     * Test method for {@link ncsa.hdf.object.h5.H5ScalarDS#getDatatype()}.
     * <p>
     * What to test:
     * <ul>
     *   <li> Get datatype
     *   <li> Check the class and size of the datatype
     * </ul>
     */
    public final void testGetDatatype() {
        H5Datatype dtype = null;
        
        try {
            dtype = (H5Datatype) testDataset.getDatatype();
        } catch (final Exception ex) {
            fail("testDataset.getDatatype() failed. "+ex);
        }
        
        assertNotNull(dtype);
        assertEquals(Datatype.CLASS_INTEGER, dtype.getDatatypeClass());
        assertEquals(H5TestFile.DATATYPE_SIZE, dtype.getDatatypeSize());
    }

    /**
     * Test method for {@link ncsa.hdf.object.h5.H5ScalarDS#getPalette()}.
     * <p>
     * What to test:
     * <ul>
     *   <li> Get the palette from an image
     *   <li> Check the content of the palette
     * </ul>
     */
    public final void testGetPalette() {
        ScalarDS img = null;
        
        try {
            img = (ScalarDS)testFile.get(H5TestFile.NAME_DATASET_IMAGE);
        } catch (final Exception ex) {
            fail("testFile.get failed. "+ex);
        }
        assertNotNull(img);
        
        final byte[][] pal = img.getPalette();
        assertNotNull(pal);
        
        for (int i=0; i<256; i++) {
            assertEquals(H5TestFile.DATA_PALETTE[i*3],   pal[0][i]);
            assertEquals(H5TestFile.DATA_PALETTE[i*3+1], pal[1][i]);
            assertEquals(H5TestFile.DATA_PALETTE[i*3+2], pal[2][i]);
        }
     }

    /**
     * Test method for {@link ncsa.hdf.object.h5.H5ScalarDS#readPalette(int)}.
     * <p>
     * What to test:
     * <ul>
     *   <li> Get the palette from an image
     *   <li> Check the content of the palette
     * </ul>
     */
    public final void testReadPalette() {
        ScalarDS img = null;
        
        try {
            img = (ScalarDS)testFile.get(H5TestFile.NAME_DATASET_IMAGE);
        } catch (final Exception ex) {
            fail("testFile.get failed. "+ex);
        }
        assertNotNull(img);
        
        final byte[][] pal = img.readPalette(0);
        assertNotNull(pal);
        
        for (int i=0; i<256; i++) {
            assertEquals(H5TestFile.DATA_PALETTE[i*3],   pal[0][i]);
            assertEquals(H5TestFile.DATA_PALETTE[i*3+1], pal[1][i]);
            assertEquals(H5TestFile.DATA_PALETTE[i*3+2], pal[2][i]);
        }
    }

    /**
     * Test method for {@link ncsa.hdf.object.h5.H5ScalarDS#getPaletteRefs()}.
     * <p>
     * What to test:
     * <ul>
     *   <li> Get an array of palette references from an image
     *   <li> Check the content of the palette references
     * </ul>
     */
    public final void testGetPaletteRefs() {
        ScalarDS img = null;
        
        try {
            img = (ScalarDS)testFile.get(H5TestFile.NAME_DATASET_IMAGE);
        } catch (final Exception ex) {
            fail("testFile.get failed. "+ex);
        }
        assertNotNull(img);
        
        final byte[] refs = img.getPaletteRefs();
        assertNotNull(refs);
    }

    /**
     * Test method for {@link ncsa.hdf.object.h5.H5ScalarDS#H5ScalarDS(ncsa.hdf.object.FileFormat, java.lang.String, java.lang.String)}.
     * <p>
     * What to test:
     * <ul>
     *   <li> Construct an H5ScalarDS object that exits in file
     *   <ul>
     *     <li> new H5ScalarDS (file, null, fullpath)
     *     <li> new H5ScalarDS (file, fullname, null)
     *     <li> new H5ScalarDS (file, name, path)
     *   </ul>
     *   <li> Construct an H5ScalarDS object that does not exist in file
     * </ul>
     */
    public final void testH5ScalarDSFileFormatStringString() {
        int[] data = null;
        final String[] names = {null, DNAME_SUB, DNAME.substring(1)};
        final String[] paths = {DNAME_SUB, null, H5TestFile.NAME_GROUP};

        final H5File file = (H5File)testDataset.getFileFormat();
        assertNotNull(file);
        
        // test existing dataset in file
        for (int idx=0; idx<names.length; idx++) {
            H5ScalarDS dset = new H5ScalarDS(file, names[idx], paths[idx]);
            assertNotNull(dset);
            
            // make sure that the data content is correct
            try {
                data = (int[])dset.getData();
            } catch (final Exception ex) { 
                fail("getData() failed. "+ ex);
            }
            assertNotNull(data);
            
            for (int i=0; i<H5TestFile.DIM_SIZE; i++) {
                assertEquals(H5TestFile.DATA_INT[i], data[i]);
            }
            
            // check the name and path
            assertTrue(DNAME_SUB.equals(dset.getFullName()));
            assertTrue(DNAME_SUB.equals(dset.getPath()+dset.getName()));
            
            dset.clear();
            dset = null;
        }
        
        // test a non-existing dataset
        final H5ScalarDS dset = new H5ScalarDS(file, "NO_SUCH_DATASET", "NO_SUCH_PATH");
        dset.init();
        dset.clearData();
        data = null;
        try {
            data = (int[])dset.getData();
        } catch (final Exception ex) { 
            data = null; // Expected - intentional
        }
        assertNull(data);
   }

    /**
     * Test method for {@link ncsa.hdf.object.h5.H5ScalarDS#H5ScalarDS(ncsa.hdf.object.FileFormat, java.lang.String, java.lang.String, long[])}.
     * <p>
     * What to test:
     * <ul>
     *   <li> Construct an H5ScalarDS object that exits in file
     *   <ul>
     *     <li> new H5ScalarDS (file, null, fullpath, oid)
     *     <li> new H5ScalarDS (file, fullname, null, oid)
     *     <li> new H5ScalarDS (file, name, path, oid)
     *   </ul>
     *   <li> Construct an H5ScalarDS object that does not exist in file
     * </ul>
     */
    public final void testH5ScalarDSFileFormatStringStringLongArray() {
        int[] data = null;
        final String[] names = {null, DNAME_SUB, DNAME.substring(1)};
        final String[] paths = {DNAME_SUB, null, H5TestFile.NAME_GROUP};

        final H5File file = (H5File)testDataset.getFileFormat();
        assertNotNull(file);
        
        // test existing dataset in file
        long[] oid = null;
        for (int idx=0; idx<names.length; idx++) {
            
            try
            {
                final byte[] ref_buf = H5.H5Rcreate(file.getFID(), DNAME_SUB, HDF5Constants.H5R_OBJECT, -1);
                final long l = HDFNativeData.byteToLong(ref_buf, 0);
                oid = new long[1];
                oid[0] = l; // save the object ID
            } catch (final HDF5Exception ex) { 
                fail("H5.H5Rcreate() failed. "+ ex);
            }
            assertNotNull(oid);

            H5ScalarDS dset = new H5ScalarDS(file, names[idx], paths[idx], oid);
            assertNotNull(dset);
            
            // make sure that the data content is correct
            try {
                data = (int[])dset.getData();
            } catch (final Exception ex) { 
                fail("getData() failed. "+ ex);
            }
            assertNotNull(data);
            
            for (int i=0; i<H5TestFile.DIM_SIZE; i++) {
                assertEquals(H5TestFile.DATA_INT[i], data[i]);
            }
            
            // check the name and path
            assertTrue(DNAME_SUB.equals(dset.getFullName()));
            assertTrue(DNAME_SUB.equals(dset.getPath()+dset.getName()));
            
            dset.clear();
            dset = null;
        }
        
        // test a non-existing dataset
        final H5ScalarDS dset = new H5ScalarDS(file, "NO_SUCH_DATASET", "NO_SUCH_PATH", null);
        dset.init();
        dset.clearData();
        data = null;
        try {
            data = (int[])dset.getData();
        } catch (final Exception ex) { 
            data = null; // Expected - intentional
        }
        assertNull(data);
   }

    /**
     * Test method for {@link ncsa.hdf.object.h5.H5ScalarDS#getMetadata()}.
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
            attrs = (Vector) testDataset.getMetadata();
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
     * Test method for {@link ncsa.hdf.object.h5.H5ScalarDS#writeMetadata(java.lang.Object)}.
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
            attrs = (Vector) testDataset.getMetadata();
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
                testDataset.writeMetadata(attr);
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
            testDataset.writeMetadata(attr);
        } catch (final Exception ex) { 
            fail("writeMetadata() failed. "+ ex);
        }

        // close the file and reopen it
        try {
            testDataset.clear();
            testFile.close();
            testFile.open();
            testDataset = (H5ScalarDS)testFile.get(DNAME);
        } catch (final Exception ex) { 
            fail("write() failed. "+ ex);
        }
        
        // check the change in file
        try {
            attrs = (Vector) testDataset.getMetadata();
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
            testDataset.removeMetadata(newAttr);
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
                testDataset.writeMetadata(attr);
            } catch (final Exception ex) { 
                fail("writeMetadata() failed. "+ ex);
            }
        } //for (int i=0; i<n; i++) {
    }

    /**
     * Test method for {@link ncsa.hdf.object.h5.H5ScalarDS#removeMetadata(java.lang.Object)}.
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
            attrs = (Vector) testDataset.getMetadata();
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
                testDataset.removeMetadata(arrayAttr[i]);
            } catch (final Exception ex) { 
                fail("removeMetadata() failed. "+ ex);
            }
         }
        
        // close the file and reopen it
        try {
            testDataset.clear();
            testFile.close();
            testFile.open();
            testDataset = (H5ScalarDS)testFile.get(DNAME);
        } catch (final Exception ex) { 
            fail("write() failed. "+ ex);
        }
        attrs = null;
        
        try {
            attrs = (Vector) testDataset.getMetadata();
        } catch (final Exception ex) { 
            fail("getMetadata() failed. "+ ex);
        }
        assertNotNull(attrs);
        assertFalse(attrs.size() > 0);

        // restor to the original
        try  {
            testDataset.writeMetadata(H5TestFile.ATTRIBUTE_STR);
            testDataset.writeMetadata(H5TestFile.ATTRIBUTE_INT_ARRAY);
        } catch (final Exception ex) { 
            fail("writeMetadata() failed. "+ ex);
        }
     }

    /**
     * Test method for {@link ncsa.hdf.object.h5.H5ScalarDS#create(java.lang.String, ncsa.hdf.object.Group, ncsa.hdf.object.Datatype, long[], long[], long[], int, java.lang.Object)}.
     * <p>
     * What to test:
     * <ul>
     *   <li> Create a new dataset of 32-bit float with level-9 gzip compression
     *   <li> Close and reopen the file
     *   <li> Check the content of the new dataset
     *   <li> Restore to the orginal file (remove the new dataset)
     * </ul>
     */
    public final void testCreate() {
        ScalarDS dset = null;
        final String nameNew = "/tmpH5ScalarDS";
        float[] data = null;
        
        final H5Datatype typeFloat = new H5Datatype(Datatype.CLASS_FLOAT, 4, -1, -1);
        
        try {
            final Group rootGrp = (Group)testFile.get("/");
            dset = H5ScalarDS.create(nameNew, rootGrp, typeFloat, H5TestFile.DIMs, null, H5TestFile.CHUNKs, 9, H5TestFile.DATA_FLOAT);
        } catch (final Exception ex) { 
            fail("H5ScalarDS.create() failed. "+ ex);
        }
        
        // check the data content
        try {
            data = (float[])dset.getData();
        } catch (final Exception ex) { 
            fail("dset.getData() failed. "+ ex);
        }
        assertNotNull(data);
         for (int i=0; i<H5TestFile.DIM_SIZE; i++) {
            assertEquals(H5TestFile.DATA_FLOAT[i], data[i], Float.MIN_VALUE);
        }
        
         try {
             testFile.delete(dset); // delete the new datast
         }  catch (final Exception ex) { 
             fail("testFile.delete failed. "+ ex);
         }
    }
    
}