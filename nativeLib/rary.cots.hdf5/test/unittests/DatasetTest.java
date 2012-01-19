/**
 * 
 */
package test.unittests;

import java.util.Arrays;

import ncsa.hdf.hdf5lib.H5;
import ncsa.hdf.hdf5lib.HDF5Constants;
import ncsa.hdf.object.FileFormat;
import ncsa.hdf.object.Dataset;
import ncsa.hdf.object.h5.H5File;
import junit.framework.TestCase;

/**
 * @author rsinha
 *
 */
public class DatasetTest extends TestCase {
	private static final H5File H5FILE = new H5File();

	private H5File testFile = null;
	String[] dsetNames = {H5TestFile.NAME_DATASET_INT, H5TestFile.NAME_DATASET_FLOAT, H5TestFile.NAME_DATASET_CHAR, 
			H5TestFile.NAME_DATASET_STR, H5TestFile.NAME_DATASET_ENUM, H5TestFile.NAME_DATASET_IMAGE, 
			H5TestFile.NAME_DATASET_COMPOUND};
	private Dataset[] dSets = new Dataset[dsetNames.length];
	/**
	 * @param arg0
	 */
	public DatasetTest(String arg0) {
		super(arg0);
	}

	/* (non-Javadoc)
	 * @see junit.framework.TestCase#setUp()
	 */
	protected void setUp() throws Exception {
		super.setUp();
		testFile = (H5File)H5FILE.open(H5TestFile.NAME_FILE_H5, FileFormat.READ);
		assertNotNull(testFile);
		testFile.open();
		for (int i = 0; i < dSets.length; i++) {
			dSets[i] = (Dataset) testFile.get(dsetNames[i]);
			dSets[i].init();
			assertNotNull(dSets[i]);
		}
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
	 * For each dataset in the file we are:
	 * <ul>
	 * 	 <li> checking the chunk size for the datasets.
	 * 	 <li> checking details about compression.
	 *   <li> cheking whether the byte array is to be converted to string.
	 *   <li> checking the dimension names.
	 *   <li> checking the dimension sizes.
	 *   <li> checking the rank.
	 *   <li> checking the selected dimensions.
	 *   <li> checking the selected indexes.
	 *   <li> checking the startdims.
	 *   <li> checking the stride.
	 *   <li> checking the width.
	 *  </ul>
	 */
	public final void testMetadataAssociatedWithDataset() {
		for (int i =0; i < dsetNames.length; i++) {
			assertNull(dSets[i].getChunkSize());
			assertTrue(dSets[i].getCompression().equals("NONE"));
			assertTrue(dSets[i].getConvertByteToString()); // by default, strings are converted
			assertNull(dSets[i].getDimNames());
			assertTrue(Arrays.equals(dSets[i].getDims(), H5TestFile.DIMs));
			if (H5TestFile.NAME_DATASET_STR.equals("/" + dSets[i].getName())) {
                assertEquals(dSets[i].getHeight(), H5TestFile.DIM2);
            } else {
                assertEquals(dSets[i].getHeight(), H5TestFile.DIM1);
            }
			assertEquals(dSets[i].getRank(), H5TestFile.RANK);
			long[] array = new long[2];
			if (H5TestFile.NAME_DATASET_STR.equals("/" + dSets[i].getName())) {
				array[0] = 1;
				array[1] = 10;
			}
			else {
				array[0] = 50;
				array[1] = 10;
			}
			assertTrue(Arrays.equals(dSets[i].getSelectedDims(), array));
			int[] arrayInt = new int[3];
			if (H5TestFile.NAME_DATASET_STR.equals("/" + dSets[i].getName())) {
				arrayInt[0] = 1; arrayInt[1] = 0; arrayInt[2] = 2;
			}
			else {
				arrayInt[0] = 0; arrayInt[1] = 1; arrayInt[2] = 2;
			}
			assertTrue(Arrays.equals(dSets[i].getSelectedIndex(), arrayInt));
			array[0] = 0; array[1] = 0;
			assertTrue(Arrays.equals(dSets[i].getStartDims(), array));
			array[0] = 1; array[1] = 1;
			assertTrue(Arrays.equals(dSets[i].getStride(), array));
			if (H5TestFile.NAME_DATASET_STR.equals("/" + dSets[i].getName())) {
                assertEquals(dSets[i].getWidth(), 1);
            } else {
                assertEquals(dSets[i].getWidth(), H5TestFile.DIM2);
            }
		}
	}
	
    /**
     *   Test converting the following unsigned values to signed values.
     *   <ul>
     *     <li> byte[] int8 = { - 1, - 128, 127, 0};
     *     <li> short[] int16 = { - 1, - 32768, 32767, 0};
     *     <li> int[] int32 = { - 1, - 2147483648, 2147483647, 0};
     * </ul>
     * Expected values
     *   <ul>
     *     <li> short[] uint8 = {255, 128, 127, 0};
     *     <li> int[] uint16 = {65535, 32768, 32767, 0};
     *     <li> long[] uint32 = {4294967295L, 2147483648L, 2147483647, 0};
     * </ul>
     */
    public final void testConvertFromUnsignedC() {
        byte [] int8 = {-1, -128, 127, 0};
        short [] int16 = {-1, -32768, 32767, 0};
        int [] int32 = {-1, -2147483648, 2147483647, 0};
        
        short [] uint8 = {255, 128, 127, 0};
        int [] uint16 = {65535, 32768, 32767, 0};
        long [] uint32 = {4294967295L, 2147483648L, 2147483647, 0};

        short [] expected8 = (short [])Dataset. convertFromUnsignedC(int8, null );
        assertTrue(Arrays.equals(expected8, uint8));

        int [] expected16 = (int [])Dataset. convertFromUnsignedC(int16, null );
        assertTrue(Arrays.equals(expected16, uint16));

        long [] expected32 = (long [])Dataset. convertFromUnsignedC(int32, null );
        assertTrue(Arrays.equals(expected32, uint32));
    }
}
