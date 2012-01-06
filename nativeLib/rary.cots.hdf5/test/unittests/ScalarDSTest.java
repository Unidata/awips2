/**
 * 
 */
package test.unittests;

import ncsa.hdf.hdf5lib.H5;
import ncsa.hdf.hdf5lib.HDF5Constants;
import ncsa.hdf.object.FileFormat;
import ncsa.hdf.object.h5.H5File;
import ncsa.hdf.object.h5.H5Group;
import junit.framework.TestCase;
import ncsa.hdf.object.ScalarDS;

/**
 * @author rsinha
 *
 */
public class ScalarDSTest extends TestCase {
	private static final H5File H5FILE = new H5File();

	private H5File testFile = null;
	private H5Group testGroup = null;
	private ScalarDS intDset = null;
	private ScalarDS floatDset = null;
	private ScalarDS charDset = null;
	private ScalarDS strDset = null;
	private ScalarDS enumDset = null;
	private ScalarDS imageDset = null;
	private ScalarDS imagePalete = null;
	private ScalarDS ORDset = null;
	/**
	 * @param arg0
	 */
	public ScalarDSTest(String arg0) {
		super(arg0);
	}

	/* (non-Javadoc)
	 * @see junit.framework.TestCase#setUp()
	 */
	protected void setUp() throws Exception {		
		super.setUp();
		testFile = (H5File)H5FILE.open(H5TestFile.NAME_FILE_H5, FileFormat.READ);
		assertNotNull(testFile);
		testGroup = (H5Group) testFile.get(H5TestFile.NAME_GROUP_ATTR);
		assertNotNull(testGroup);

		intDset = (ScalarDS) testFile.get(H5TestFile.NAME_DATASET_INT);
		assertNotNull(intDset);
		intDset.init();
		floatDset = (ScalarDS) testFile.get(H5TestFile.NAME_DATASET_FLOAT);
		assertNotNull(floatDset);
		floatDset.init();
		charDset = (ScalarDS) testFile.get(H5TestFile.NAME_DATASET_CHAR);
		assertNotNull(charDset);
		charDset.init();
		strDset = (ScalarDS) testFile.get(H5TestFile.NAME_DATASET_STR);
		assertNotNull(strDset);
		strDset.init();
		enumDset = (ScalarDS) testFile.get(H5TestFile.NAME_DATASET_ENUM);
		assertNotNull(enumDset);
		enumDset.init();
		imageDset = (ScalarDS) testFile.get(H5TestFile.NAME_DATASET_IMAGE);
		assertNotNull(imageDset);
		imageDset.init();
		ORDset = (ScalarDS) testFile.get(H5TestFile.NAME_DATASET_OBJ_REF);
		assertNotNull(ORDset);
		ORDset.init();
		imagePalete = (ScalarDS) testFile.get(H5TestFile.NAME_DATASET_IMAGE_PALETTE);
		assertNotNull(imagePalete);
		imagePalete.init();
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
	 * 
	 * What to test:
     * <ul> 
     *   <li> Test for general functionality
     *   <ul>
     *     <li> Check for all datasets whether it is an image or not (only image should return true).
     *     <li> Check for all datasets whether imageDisplay is set (only image should return true).
     *     <li> Get Image Data Range from image dataset and check if it is valid.
     *     <li> Check the interlace value for the image dataset.
     *     <li> Check if the image is unsigned byte.
     *     <li> Check setting the image to unsigned byte.
     *     <li> Check true color for the image dataset.
     *   </ul>
     * </ul>
     */ 
	public final void testImageFunctionality() {
        assertTrue(imageDset.hasAttribute());
		assertTrue(imageDset.isImage());
		assertTrue(imageDset.isImageDisplay());
	
		assertEquals(imageDset.getInterlace(), -1);
		
		assertFalse(imageDset.isTrueColor());
		
		assertFalse(intDset.isImage());
		assertFalse(floatDset.isImage());
		assertFalse(charDset.isImage());
		assertFalse(enumDset.isImage());
		assertFalse(imagePalete.isImage());
		assertFalse(ORDset.isImage());
	
		assertFalse(intDset.isImageDisplay());
		assertFalse(floatDset.isImageDisplay());
		assertFalse(charDset.isImageDisplay());
		assertFalse(enumDset.isImageDisplay());
		assertFalse(imagePalete.isImageDisplay());
		assertFalse(ORDset.isImageDisplay());
	
		intDset.setIsImageDisplay(true);
		assertTrue(intDset.isImageDisplay());
		intDset.setIsImageDisplay(false);
		assertFalse(intDset.isImageDisplay());
	}
	
	 /** What to test:
	  * <ul> 
	  *   <li> Test for general functionality
	  *   <ul>
	  *     <li> For all datasets in the file check if it is text. Only string dataset should return true.
	  *   </ul>
	  * </ul>
	  */ 
	public final void testIsText() {
		assertTrue(strDset.isText());
		assertFalse(imageDset.isText());
		assertFalse(intDset.isText());
		assertFalse(floatDset.isText());
		assertFalse(charDset.isText());
		assertFalse(enumDset.isText());
		assertFalse(ORDset.isText());
		assertFalse(imagePalete.isText());
	}
}
