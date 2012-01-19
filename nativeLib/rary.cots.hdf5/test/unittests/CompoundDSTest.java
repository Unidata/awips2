/**
 * 
 */
package test.unittests;

import ncsa.hdf.hdf5lib.H5;
import ncsa.hdf.hdf5lib.HDF5Constants;
import ncsa.hdf.object.Datatype;
import ncsa.hdf.object.FileFormat;
import ncsa.hdf.object.h5.H5File;
import junit.framework.TestCase;
import ncsa.hdf.object.CompoundDS;

/**
 * @author rsinha
 *
 */
public class CompoundDSTest extends TestCase {
	private static final H5File H5FILE = new H5File();

	private H5File testFile = null;
	private CompoundDS testDS = null;
	/**
	 * @param arg0
	 */
	public CompoundDSTest(String arg0) {
		super(arg0);
	}

	/* (non-Javadoc)
	 * @see junit.framework.TestCase#setUp()
	 */
	protected void setUp() throws Exception {
		super.setUp();
		testFile = (H5File)H5FILE.open(H5TestFile.NAME_FILE_H5, FileFormat.WRITE);
		assertNotNull(testFile);
		testDS = (CompoundDS) testFile.get(H5TestFile.NAME_DATASET_COMPOUND);
		assertNotNull(testDS);
		testDS.init();
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
	 * For the Compund Dataset in the Test File, we are checking
	 * <ul>
	 *		<li> geting the memberCount.
	 *		<li> the names of each member in the dataset.
	 *		<li> the types of each member in the dataset.
	 *		<li> the orders of each member in the dataset.
	 *		<li> the dims of each member in the dataset.
	 * </ul>
	 */
	public final void testFieldsHaveCorrectNameTypeOrderAndDims() {
		int correctMemberCount = H5TestFile.COMPOUND_MEMBER_NAMES.length;
		assertEquals(testDS.getMemberCount(), correctMemberCount);
		String[] names = testDS.getMemberNames();
		for (int i = 0; i < correctMemberCount; i++) {
			if (!names[i].equals(H5TestFile.COMPOUND_MEMBER_NAMES[i])) {
                fail("Member Name at position " + i + "should be " + H5TestFile.COMPOUND_MEMBER_NAMES[i] + 
						", while getMemberNames returns " + names[i]);
            }
		}
		Datatype[] types = testDS.getMemberTypes();
		for (int i = 0; i < correctMemberCount; i++) {
			if (!types[i].getDatatypeDescription().equals(H5TestFile.COMPOUND_MEMBER_DATATYPES[i].getDatatypeDescription())) {
                fail("Member Type at position " + i + "should be " + 
						H5TestFile.COMPOUND_MEMBER_DATATYPES[i].getDatatypeDescription() + 
						", while getMemberTypes returns " + types[i].getDatatypeDescription());
            }
		}
		int[] orders = testDS.getMemberOrders();
		for (int i = 0; i < correctMemberCount; i++) {
			if (orders[i] != 1) {
                fail("Member Order at position " + i + "should be " + 1 + ", while getMemberOrders returns " + orders[i]);
            }
		}
		for (int i = 0; i < correctMemberCount; i++) {
			assertNull(testDS.getMemeberDims(i)); // all scalar data
		}
	}
	
	/**
	 * For the Compund Dataset in the Test File, we are checking
	 * <ul>
	 *		<li> Geting the selectMemberCount method on the default selection.
	 *		<li> setting ths member selection so that no member is selected.
	 *		<li> setting the member selection so that all members are exlplicitly selected.
	 *		<li> Adding one member at a time and checking if the addition is working properly.
	 * </ul>
	 */
	public final void testSelectionDeselectionCountWorks() {
		if (testDS.getSelectedMemberCount() != H5TestFile.COMPOUND_MEMBER_NAMES.length) {
            fail("Right after init getSelectedMemberCount returns" + testDS.getSelectedMemberCount() 
					+ ", when it should return " + H5TestFile.COMPOUND_MEMBER_NAMES.length);
        }
		
		testDS.setMemberSelection(false);
		assertEquals(testDS.getSelectedMemberCount(), 0);
		testDS.setMemberSelection(true);
		assertEquals(testDS.getSelectedMemberCount(), H5TestFile.COMPOUND_MEMBER_NAMES.length);
		testDS.setMemberSelection(false);
		assertEquals(testDS.getSelectedMemberCount(), 0);
		
		for (int i = 0; i < testDS.getMemberCount(); i++) {
			testDS.selectMember(i);
			int[] orders = testDS.getSelectedMemberOrders();
			Datatype[] types = testDS.getMemberTypes();
			for (int j = 0; j <= i; j++) {
				if (!testDS.isMemberSelected(j)) {
                    fail("Member " + j + "was selected while isMemberSelected says it wasnt.");
                }
				if (orders[j] != 1) {
                    fail("Member Order at position " + j + "should be " + 1 + ", while getMemberOrders returns " + orders[j]);
                }
				if (!types[j].getDatatypeDescription().equals(H5TestFile.COMPOUND_MEMBER_DATATYPES[j].getDatatypeDescription())) {
                    fail("Member Type at position " + i + "should be " + 
							H5TestFile.COMPOUND_MEMBER_DATATYPES[j].getDatatypeDescription() + 
							", while getMemberTypes returns " + types[j].getDatatypeDescription());
                }
			}
		}
	}
}
