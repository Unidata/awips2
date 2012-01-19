/**
 * 
 */
package test.unittests;

import ncsa.hdf.hdf5lib.H5;
import ncsa.hdf.hdf5lib.HDF5Constants;
import ncsa.hdf.object.h5.H5Datatype;
import ncsa.hdf.object.h5.H5Group;
import ncsa.hdf.object.Attribute;
import ncsa.hdf.object.Datatype;
import ncsa.hdf.object.FileFormat;
import ncsa.hdf.object.h5.H5File;
import junit.framework.TestCase;

import java.util.Arrays;
import java.util.List;
/**
 * @author Rishi R. Sinha
 *
 */
public class AttributeTest extends TestCase {
	private static final H5File H5FILE = new H5File();
	    
    private H5File testFile = null;
   	private H5Group testGroup = null;
   	private Attribute strAttr = null;
   	private Attribute arrayIntAttr = null;

   	/**
	 * @param arg0
	 */
	public AttributeTest(String arg0) {
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
	 * Test method for {@link ncsa.hdf.object.Attribute#Attribute(java.lang.String, ncsa.hdf.object.Datatype, long[])}.
	 * <p>
	 * Here we test:
	 * 	<ul>
	 * 		<li> Creating a new attribute with no value.
	 * 		<li> Setting the attribute value.
	 * 	</ul>
	 * 
	 */
	public final void testAttributeStringDatatypeLongArray() {
		long[] attrDims = {1};
        String attrName = "CLASS";
        String[] classValue = {"IMAGE"};
        Datatype attrType = new H5Datatype(Datatype.CLASS_STRING, classValue[0].length()+1, -1, -1);
        Attribute attr = new Attribute(attrName, attrType, attrDims);
        attr.setValue(classValue);
        assertNotNull(attr);
        assertEquals(classValue[0], attr.toString("|"));
	}

	/**
	 * Test method for {@link ncsa.hdf.object.Attribute#Attribute(java.lang.String, ncsa.hdf.object.Datatype, long[], java.lang.Object)}.
	 * <p>
	 * Here we test:
	 * 	<ul>
	 * 		<li> Creating a new attribute with a value.
	 * 	</ul>
	 */
	public final void testAttributeStringDatatypeLongArrayObject() {
		long[] attrDims = {1};
        String attrName = "CLASS";
        String[] classValue = {"IMAGE"};
        Datatype attrType = new H5Datatype(Datatype.CLASS_STRING, classValue[0].length()+1, -1, -1);
        Attribute attr = new Attribute(attrName, attrType, attrDims, classValue);
        assertNotNull(attr);
        assertEquals(classValue[0], attr.toString("|"));
	}

	/**
	 * Test method for {@link ncsa.hdf.object.Attribute#getValue()}.
	 * 
	 * Here we test:
	 * 	<ul>
	 * 		<li> Getting the value for the two attributes (the string attribute and the int array attribute).
	 * 	</ul>
	 */
	public final void testGetValue() {
		assertEquals(((String[]) strAttr.getValue())[0], "String attribute.");
		assertTrue(Arrays.equals((int []) arrayIntAttr.getValue(), new int[] {1, 2, 3, 4, 5, 6, 7, 8, 9, 10}));
	}

	/**
	 * Test method for {@link ncsa.hdf.object.Attribute#setValue(java.lang.Object)}.
	 * <p>
	 *  Here we test:
	 * 	<ul>
	 * 		<li> Setting new value for the two attributes (the string attribute and the int array attribute).
	 * 	</ul>
	 */
	public final void testSetValue() {
		String[] prevValue = (String[]) strAttr.getValue();
		strAttr.setValue("Temp String Value");
		assertEquals(((String) strAttr.getValue()), "Temp String Value");
		strAttr.setValue(prevValue);
		
		int[] intPrevValue = (int[]) arrayIntAttr.getValue();
		arrayIntAttr.setValue(new int[] {0, 1, 2, 3, 4, 5, 6, 7, 8, 9});
		assertTrue(Arrays.equals((int [])arrayIntAttr.getValue(), new int[] {0, 1, 2, 3, 4, 5, 6, 7, 8, 9}));
		arrayIntAttr.setValue(intPrevValue);
	}

	/**
	 * Test method for {@link ncsa.hdf.object.Attribute#getName()}.
	 * <p>
	 * Here we test:
	 * 	<ul>
	 * 		<li> Getting the names of the two attributes (the string attribute and the int array attribute).
	 * 	</ul>
	 */
	public final void testGetName() {
		assertTrue(strAttr.getName().equals("strAttr"));
		assertTrue(arrayIntAttr.getName().equals("arrayInt"));
	}

	/**
	 * Test method for {@link ncsa.hdf.object.Attribute#getRank()}.
	 * <p>
	 * Here we test:
	 * 	<ul>
	 * 		<li> Getting the rank for the two attributes (the string attribute and the int array attribute).
	 * 	</ul>
	 */
	public final void testGetRank() {
		assertEquals(strAttr.getRank(), 1);
		assertEquals(arrayIntAttr.getRank(), 1);
	}

	/**
	 * Test method for {@link ncsa.hdf.object.Attribute#getDataDims()}.
	 * <p>
	 * Here we test:
	 * 	<ul>
	 * 		<li> Getting the dimensionalities for the two attributes (the string attribute and the int array attribute).
	 * 	</ul>
	 */
	public final void testGetDataDims() {
		assertEquals(strAttr.getDataDims()[0], 1);
		assertEquals(arrayIntAttr.getDataDims()[0], 10);
	}

	/**
	 * Test method for {@link ncsa.hdf.object.Attribute#getType()}.
	 * <p>
	 * Here we test:
	 * 	<ul>
	 * 		<li> Getting the value for the two attributes (the string attribute and the int array attribute).
	 * 	</ul>
	 */
	public final void testGetType() {
		assertTrue(strAttr.getType().getDatatypeDescription().equals("String, length = 20"));
		assertTrue(arrayIntAttr.getType().getDatatypeDescription().equals("32-bit integer"));
	}

	/**
	 * Test method for {@link ncsa.hdf.object.Attribute#isUnsigned()}.
	 * <p>
	 * Here we test:
	 * 	<ul>
	 * 		<li> Check if the two attributes (the string attribute and the int array attribute) are unsigned.
	 * 	</ul>
	 */
	public final void testIsUnsigned() {
		assertFalse(strAttr.isUnsigned());
		assertFalse(arrayIntAttr.isUnsigned());
	}

	/**
	 * Test method for {@link ncsa.hdf.object.Attribute#toString(java.lang.String)}.
	 * <p>
	 * Here we test:
	 * 	<ul>
	 * 		<li> the toString method for the two attributes (the string attribute and the int array attribute).
	 * 	</ul>
	 */
	public final void testToStringString() {
		assertTrue(strAttr.toString(",").equals("String attribute."));
		assertTrue(arrayIntAttr.toString(",").equals("1,2,3,4,5,6,7,8,9,10"));
	}

}
