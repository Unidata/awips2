/**
 * 
 */
package test.unittests;

import junit.framework.TestCase;
import ncsa.hdf.object.Datatype;
import ncsa.hdf.object.h5.H5Datatype;

/**
 * @author rsinha
 *
 */
public class DatatypeTest extends TestCase {
	
	private Datatype[] baseTypes = null;
	private int[] classes = {Datatype.CLASS_BITFIELD, Datatype.CLASS_CHAR, Datatype.CLASS_COMPOUND, 
			Datatype.CLASS_ENUM, Datatype.CLASS_FLOAT, Datatype.CLASS_INTEGER, Datatype.CLASS_NO_CLASS,
			Datatype.CLASS_OPAQUE, Datatype.CLASS_REFERENCE, Datatype.CLASS_STRING, 
			Datatype.CLASS_VLEN};
	private int[] signs = {Datatype.SIGN_2, Datatype.SIGN_NONE, Datatype.NSGN};
	private int[] orders = {Datatype.ORDER_BE, Datatype.ORDER_LE, Datatype.ORDER_NONE, Datatype.ORDER_VAX};
	private int n_classes = 11;
	private int n_signs = 3;
	private int n_orders = 4;
	private int[] sizes = {32, 64, 8, 16};
	private String[] descriptions = {"Unknown",	"Unknown", "Unknown",
	"Unknown", "Unknown", "Unknown", "Unknown",	"Unknown", "Unknown",
	"Unknown", "Unknown", "Unknown", "8-bit character",	"8-bit unsigned character",
	"8-bit character", "8-bit character", "8-bit unsigned character", "8-bit character",
	"8-bit character", "8-bit unsigned character", "8-bit character", "8-bit character",
	"8-bit unsigned character", "8-bit character", "Unknown", "Unknown", "Unknown", "Unknown",
	"Unknown", "Unknown", "Unknown", "Unknown", "Unknown", "Unknown", "Unknown",
	"Unknown", "enum (0=1  1=2  )", "enum (0=1  1=2  )", "enum (0=1  1=2  )",
	"enum (0=1  1=2  )", "enum (0=1  1=2  )", "enum (0=1  1=2  )", 
	"enum (0=1  1=2  )", "enum (0=1  1=2  )", "enum (0=1  1=2  )", "enum (0=1  1=2  )", 
	"enum (0=1  1=2  )", "enum (0=1  1=2  )", "32-bit floating-point",
	"32-bit floating-point", "32-bit floating-point", "32-bit floating-point",
	"32-bit floating-point", "32-bit floating-point", "32-bit floating-point",
	"32-bit floating-point", "32-bit floating-point", "32-bit floating-point",
	"32-bit floating-point", "32-bit floating-point", "32-bit floating-point",
	"32-bit floating-point", "32-bit floating-point", "32-bit floating-point",
	"32-bit floating-point", "32-bit floating-point", "32-bit floating-point",
	"32-bit floating-point", "32-bit floating-point", "32-bit floating-point",
	"32-bit floating-point", "32-bit floating-point", "32-bit integer",
	"32-bit integer", "64-bit integer", "32-bit integer", "32-bit unsigned integer",
	"32-bit unsigned integer", "64-bit unsigned integer", "32-bit unsigned integer",
	"32-bit integer", "32-bit integer", "64-bit integer", "32-bit integer", "32-bit integer",
	"32-bit integer", "64-bit integer", "32-bit integer", "32-bit unsigned integer",
	"32-bit unsigned integer", "64-bit unsigned integer", "32-bit unsigned integer",
	"32-bit integer", "32-bit integer", "64-bit integer", "32-bit integer", "32-bit integer",
	"32-bit integer", "64-bit integer", "32-bit integer", "32-bit unsigned integer",
	"32-bit unsigned integer", "64-bit unsigned integer", "32-bit unsigned integer",
	"32-bit integer", "32-bit integer", "64-bit integer", "32-bit integer", "32-bit integer",
	"32-bit integer", "64-bit integer", "32-bit integer", "32-bit unsigned integer",
	"32-bit unsigned integer", "64-bit unsigned integer", "32-bit unsigned integer",
	"32-bit integer", "32-bit integer", "64-bit integer", "32-bit integer",
	"Unknown", "Unknown", "Unknown", "Unknown", "Unknown", "Unknown", "Unknown", "Unknown", "Unknown",
	"Unknown", "Unknown", "Unknown", "Unknown", "Unknown", "Unknown", "Unknown", "Unknown", "Unknown",
	"Unknown", "Unknown", "Unknown", "Unknown", "Unknown", "Unknown", "Object reference", "Object reference",
	"Object reference", "Object reference", "Object reference", "Object reference", "Object reference",
	"Object reference", "Object reference", "Object reference", "Object reference", "Object reference",
	"String, length = 32", "String, length = 32", "String, length = 32", "String, length = 32",
	"String, length = 32", "String, length = 32", "String, length = 32", "String, length = 32",
	"String, length = 32", "String, length = 32", "String, length = 32", "String, length = 32",
	"Unknown", "Unknown", "Unknown", "Unknown", "Unknown", "Unknown", "Unknown", "Unknown", "Unknown",
	"Unknown", "Unknown", "Unknown"};
	
	/**
	 * @param arg0
	 */
	public DatatypeTest(String arg0) {
		super(arg0);
	}

	/* (non-Javadoc)
	 * @see junit.framework.TestCase#setUp()
	 */
	protected void setUp() throws Exception {
		super.setUp();
		baseTypes = new Datatype[n_orders*n_signs*(n_classes+4)];
		int counter = 0;
		for (int i = 0; i < n_classes; i++) {
			for (int j = 0; j < n_orders; j++) {
				for (int k = 0; k < n_signs; k++) {
					int n_sizes;
					switch(classes[i]) {
					case Datatype.CLASS_INTEGER:
						n_sizes = 4;
						break;
					case Datatype.CLASS_FLOAT:
						n_sizes = 2;
						break;
					default:
						n_sizes = 1;
						break;
					}
					for (int l = 0; l < n_sizes; l++) {
						baseTypes[counter++] = new H5Datatype(classes[i], sizes[l], 
								orders[j], signs[k]);
						assertNotNull(baseTypes[i]);
					}
				}
			}
		}
	}

	/* (non-Javadoc)
	 * @see junit.framework.TestCase#tearDown()
	 */
	protected void tearDown() throws Exception {
		super.tearDown();
	}

	/**
	 * Test method for {@link ncsa.hdf.object.Datatype#getDatatypeClass()}.
	 * <p>
	 * We test for every combination of class, size and possible signs.
	 */
	public final void testGetDatatypeClass() {
		int counter = 0;
		for (int i = 0; i < n_classes; i++) {
			for (int j = 0; j < n_orders; j++) {
				for (int k = 0; k < n_signs; k++) {
					int n_sizes;
					switch(classes[i]) {
					case Datatype.CLASS_INTEGER:
						n_sizes = 4;
						break;
					case Datatype.CLASS_FLOAT:
						n_sizes = 2;
						break;
					default:
						n_sizes = 1;
						break;
					}
					for (int l = 0; l < n_sizes; l++) {
						assertEquals(baseTypes[counter++].getDatatypeClass(), classes[i]);
					}
				}
			}
		}
	}

	/**
	 * Test method for {@link ncsa.hdf.object.Datatype#getDatatypeSize()}.
	 * <p>
	 * We test for every combination of class, size and possible signs.
	 */
	public final void testGetDatatypeSize() {
		int counter = 0;
		for (int i = 0; i < n_classes; i++) {
			for (int j = 0; j < n_orders; j++) {
				for (int k = 0; k < n_signs; k++) {
					int n_sizes;
					switch(classes[i]) {
					case Datatype.CLASS_INTEGER:
						n_sizes = 4;
						break;
					case Datatype.CLASS_FLOAT:
						n_sizes = 2;
						break;
					default:
						n_sizes = 1;
						break;
					}
					for (int l = 0; l < n_sizes; l++) {
						assertEquals(baseTypes[counter++].getDatatypeSize(), sizes[l]);
					}
				}
			}
		}
	}

	/**
	 * Test method for {@link ncsa.hdf.object.Datatype#getDatatypeOrder()}.
	 * <p>
	 * We test for every combination of class, size and possible signs.
	 */
	public final void testGetDatatypeOrder() {
		int counter = 0;
		for (int i = 0; i < n_classes; i++) {
			for (int j = 0; j < n_orders; j++) {
				for (int k = 0; k < n_signs; k++) {
					int n_sizes;
					switch(classes[i]) {
					case Datatype.CLASS_INTEGER:
						n_sizes = 4;
						break;
					case Datatype.CLASS_FLOAT:
						n_sizes = 2;
						break;
					default:
						n_sizes = 1;
						break;
					}
					for (int l = 0; l < n_sizes; l++) {
						assertEquals(baseTypes[counter++].getDatatypeOrder(), orders[j]);
					}
				}
			}
		}
	}

	/**
	 * Test method for {@link ncsa.hdf.object.Datatype#getDatatypeSign()}.
	 * <p>
	 * We test for every combination of class, size and possible signs.
	 */
	public final void testGetDatatypeSign() {
		int counter = 0;
		for (int i = 0; i < n_classes; i++) {
			for (int j = 0; j < n_orders; j++) {
				for (int k = 0; k < n_signs; k++) {
					int n_sizes;
					switch(classes[i]) {
					case Datatype.CLASS_INTEGER:
						n_sizes = 4;
						break;
					case Datatype.CLASS_FLOAT:
						n_sizes = 2;
						break;
					default:
						n_sizes = 1;
						break;
					}
					for (int l = 0; l < n_sizes; l++) {
						assertEquals(baseTypes[counter++].getDatatypeSign(), signs[k]);
					}
				}
			}
		}
	}

	/**
	 * Test method for {@link ncsa.hdf.object.Datatype#setEnumMembers(java.lang.String)}.
	 * <p>
	 * create a new enum data type set it to two different values and check it.
	 */
	public final void testSetEnumMembers() {
		Datatype ed = new H5Datatype(Datatype.CLASS_ENUM, 2, Datatype.ORDER_NONE, Datatype.NSGN);
		ed.setEnumMembers("low=20, high=40");
		assertEquals(ed.getEnumMembers(), "low=20, high=40");
	}

	/**
	 * Test method for {@link ncsa.hdf.object.Datatype#getEnumMembers()}.
	 * <p>
	 * look at {@link ncsa.hdf.object.Datatype#setEnumMembers(java.lang.String)}.
	 */
	public final void testGetEnumMembers() {
		testSetEnumMembers();
	}

	/**
	 * Test method for {@link ncsa.hdf.object.Datatype#getDatatypeDescription()}.
	 * RISHI SINHA - THE METHOD CALLED IS ONE FOR H5 WHICH OVERRIDES THE BASE CALL.
	 * <p>
	 * We test for every combination of class, size and possible signs.
	 */
	public final void testGetDatatypeDescription() {
		int counter = 0;
		for (int i = 0; i < n_classes; i++) {
			for (int j = 0; j < n_orders; j++) {
				for (int k = 0; k < n_signs; k++) {
					int n_sizes;
					switch(classes[i]) {
					case Datatype.CLASS_INTEGER:
						n_sizes = 4;
						break;
					case Datatype.CLASS_FLOAT:
						n_sizes = 2;
						break;
					default:
						n_sizes = 1;
						break;
					}
					for (int l = 0; l < n_sizes; l++) {
						assertEquals(baseTypes[counter].getDatatypeDescription(), descriptions[counter]);
					}
				}
			}
		}
	}
	/** ABSTRACT METHOD
	 * Test method for {@link ncsa.hdf.object.Datatype#isUnsigned()}.
	 * <p>
	 * We test for every combination of class, size and possible signs.
	 */
	public final void testIsUnsigned() {
		int counter = 0;
		for (int i = 0; i < n_classes; i++) {
			for (int j = 0; j < n_orders; j++) {
				for (int k = 0; k < n_signs; k++) {
					int n_sizes;
					switch(classes[i]) {
					case Datatype.CLASS_INTEGER:
						n_sizes = 4;
						break;
					case Datatype.CLASS_FLOAT:
						n_sizes = 2;
						break;
					default:
						n_sizes = 1;
						break;
					}
					for (int l = 0; l < n_sizes; l++) {
						 boolean isSigned = baseTypes[counter++].isUnsigned();
						 if (isSigned && (signs[k] != Datatype.SIGN_NONE)) {
                            fail("isUnsigned Failed.");
                        }
					}
				}
			}
		}
	}
}
