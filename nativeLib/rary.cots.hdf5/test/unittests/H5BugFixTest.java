/**
 * 
 */
package test.unittests;

import java.util.Vector;

import ncsa.hdf.hdf5lib.H5;
import ncsa.hdf.hdf5lib.HDF5Constants;
import ncsa.hdf.object.*;
import ncsa.hdf.object.h5.*;
import junit.framework.TestCase;

/**
 * TestCase for bug fixes.
 * <p>
 * This class tests all the public methods in H5CompoundDS class.
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
 *
 *@author Peter Cao, The HDF Group
 */
public class H5BugFixTest extends TestCase {
    private static final int NLOOPS = 10;
    private static final H5File H5FILE = new H5File();
    private H5File testFile = null;
    

    /**
     * @param arg0
     */
    public H5BugFixTest(final String arg0) {
        super(arg0);
    }

    private static void collectGarbage() {
        try {
            System.gc();
            Thread.sleep(100);
            System.runFinalization();
            Thread.sleep(100);
        }
        catch (final Exception ex){
            ex.printStackTrace();
        }
    }
    
    /* (non-Javadoc)
     * @see junit.framework.TestCase#setUp()
     */
    protected void setUp() throws Exception {
        super.setUp();
        testFile = (H5File)H5FILE.open(H5TestFile.NAME_FILE_H5, FileFormat.WRITE);
        assertNotNull(testFile);
    }

    /* (non-Javadoc)
     * @see junit.framework.TestCase#tearDown()
     */
    protected void tearDown() throws Exception {
        super.tearDown();
        
        if (testFile != null) {
            try { testFile.close(); } catch (final Exception ex) {}
            testFile = null;
        }
    }
    
    /**
     * The following program fails because dataset.init() does not reset the selection of dataspace.
     * <p>
     * The bug appears on hdf-java 2.4 beta04 or earlier version. It is fixed at later version.
     *  
     * <pre>
          1)  read the table cell (using dataset selection to select only that row of the table)
          2)  re-initialize the Dataset
          3)  call 'Dataset.clearData()'
          4)  call 'Dataset.getData()'
          5)  change the correct column/row **
          6)  call 'Dataset.write()'
          7)  close the file
          8)  reopen the file and read the table cell as in step 1
          9)  assert that the value has been changed and is correct
          This sequence of actions works correctly on the hdf-java library built for
          64-bit solaris that we received in August 2006.  On the latest (beta-d), This
          fails when attempting to change the value of the 1st and 4th rows (however, it
          works for the 0th row).
     * </pre> 
     * <p>
     */
    public final void testBug847() throws Exception {
        Vector data = null;
        
        final H5CompoundDS dset = (H5CompoundDS)testFile.get(H5TestFile.NAME_DATASET_COMPOUND);
        assertNotNull(dset);

        for (int loop=0; loop<NLOOPS; loop++) {
            // read the whole dataset by default
            dset.init();

            try {
                data = (Vector)dset.getData();
            } catch (final Exception ex) { 
                fail("getData() failed. "+ ex);
            }
            assertNotNull(data);
            assertTrue(data.size() > 0);
            
            // check the data values
            int[] ints = (int[])data.get(0);
            float[] floats = (float[])data.get(1);
            String[] strs = (String[])data.get(2);
            assertNotNull(ints);
            assertNotNull(floats);
            assertNotNull(strs);
            for (int i=0; i<H5TestFile.DIM_SIZE; i++) {
                assertEquals(H5TestFile.DATA_INT[i], ints[i]);
                assertEquals(H5TestFile.DATA_FLOAT[i], floats[i], Float.MIN_VALUE);
                assertTrue(H5TestFile.DATA_STR[i].equals(strs[i]));
            }
            
            final int rank = dset.getRank();
            final long[] dims = dset.getDims();
            final long[] start = dset.getStartDims();
            final long[] count = dset.getSelectedDims();
            final int[] selectedIndex = dset.getSelectedIndex();
            
            // read data row by row
            for (int i=0; i<rank; i++) {
                start[i] = 0;
                count[i] = 1;
            }
            final int nrows = dset.getHeight();
            for (int i=0; i<nrows; i++) {
                dset.clearData();
                dset.init();
                
                // select one row only
                for (int j=0; j<rank; j++) {
                    count[j] = 1;
                }
                
                // select different rows
                start[0] = i;
                
                try {
                    data = (Vector)dset.getData();
                } catch (final Exception ex) { 
                    fail("getData() failed. "+ ex);
                }
                
                ints = (int[])data.get(0);
                floats = (float[])data.get(1);
                strs = (String[])data.get(2);
                assertNotNull(ints);
                assertNotNull(floats);
                assertNotNull(strs);
                assertEquals(H5TestFile.DATA_INT[i], ints[0]);
                assertEquals(H5TestFile.DATA_FLOAT[i], floats[0], Float.MIN_VALUE);
                assertTrue(H5TestFile.DATA_STR[i].equals(strs[0]));
            } // for (int i=0; i<nrows; i++) {
            
            // read field by field
            final int nmembers = dset.getMemberCount();
            for (int i=0; i<nmembers; i++) {
                dset.clearData();
                dset.init();
                
                dset.setMemberSelection(false);
                dset.selectMember(i);
                
                try {
                    data = (Vector)dset.getData();
                } catch (final Exception ex) { 
                    fail("getData() failed. "+ ex);
                }
                assertNotNull(data);
                assertTrue(data.size()==1);
                
                switch (i) {
                    case 0:
                        ints = (int[])data.get(0);
                        assertNotNull(ints);
                        for (int j=0; j<H5TestFile.DIM_SIZE; j++) {
                            assertEquals(H5TestFile.DATA_INT[j], ints[j]);
                        }
                        break;
                    case 1:
                        floats = (float[])data.get(0);
                        assertNotNull(floats);
                        for (int j=0; j<H5TestFile.DIM_SIZE; j++) {
                            assertEquals(H5TestFile.DATA_FLOAT[j], floats[j], Float.MIN_VALUE);
                        }
                        break;
                    case 2:
                        strs = (String[])data.get(0);
                        assertNotNull(strs);
                        for (int j=0; j<H5TestFile.DIM_SIZE; j++) {
                            assertTrue(H5TestFile.DATA_STR[j].equals(strs[j]));
                        }
                        break;
                }
            } // for (int i=0; i<nmembers; i++) {
        } //for (int loop=0; loop<NLOOPS; loop++) {
    }
    
    /**
     * The following operation causes memory leak because a group is left open at file.get().
     * <p>
     * The bug appears on hdf-java 2.4 beta05 or earlier version. It is fixed at later version.
     *  
     * <pre>
        while (true)
        {
            H5File file = new H5File(H5TestFile.NAME_FILE_H5, H5File.READ);
            //file.open();
            file.get("/Table0");
            file.get("/Group0");   
            
            int n = H5.H5Fget_obj_count(file.getFID(), HDF5Constants.H5F_OBJ_ALL);
            if (n>1)
                System.out.println("*** Possible memory leak!!!");
            
            file.close();
        }
     * </pre> 
     */
    public final void testBug863() throws Exception {
        int nObjs = 0; // number of object left open
        Dataset dset =null;
        final String dnames[] = {
                H5TestFile.NAME_DATASET_CHAR, H5TestFile.NAME_DATASET_COMPOUND,
                H5TestFile.NAME_DATASET_COMPOUND_SUB, H5TestFile.NAME_DATASET_ENUM,
                H5TestFile.NAME_DATASET_FLOAT, H5TestFile.NAME_DATASET_IMAGE,
                H5TestFile.NAME_DATASET_INT, H5TestFile.NAME_DATASET_STR,
                H5TestFile.NAME_DATASET_INT_SUB, H5TestFile.NAME_DATASET_FLOAT_SUB_SUB};
         
        // test two open options: open full tree or open individual object only
        for (int openOption=0; openOption<2; openOption++){
            for (int i=0; i<NLOOPS; i++)
            {
                nObjs = 0;
                final H5File file = new H5File(H5TestFile.NAME_FILE_H5, FileFormat.WRITE);
                
                if (openOption == 0) {
                    try { 
                        file.open(); // opent the full tree
                    } catch (final Exception ex) { 
                        fail("file.open() failed. "+ ex);
                    }
                }
                  
                try {
                    // datasets
                    for (int j=0; j<dnames.length; j++) {
                        dset = (Dataset)file.get(dnames[j]);
                        dset.init();
                        final Object data = dset.getData();
                        dset.write(data);
                        dset.getMetadata();
                    }
                    
                    // groups
                    file.get(H5TestFile.NAME_GROUP);
                    file.get(H5TestFile.NAME_GROUP_ATTR);
                    file.get(H5TestFile.NAME_GROUP_SUB);
                    
                    // datatypes
                    file.get(H5TestFile.NAME_DATATYPE_INT);
                    file.get(H5TestFile.NAME_DATATYPE_FLOAT);
                    file.get(H5TestFile.NAME_DATATYPE_STR);

                } catch (final Exception ex) { 
                     fail("file.get() failed. "+ ex);
                }
     
                try {
                    nObjs = H5.H5Fget_obj_count(file.getFID(), HDF5Constants.H5F_OBJ_ALL);
                } catch (final Exception ex) { 
                     fail("H5.H5Fget_obj_count() failed. "+ ex);
                }
                
                try {            
                    file.close();
                } catch (final Exception ex) { 
                     fail("file.close() failed. "+ ex);
                }
   
                assertTrue(nObjs <=1); // file id should be the only this left open
            } //for (int i=0; i<NLOOPS; i++)
        } // for (int openOption=0; openOption<2; openOption++)
    }
}
