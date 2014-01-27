/**
 * 
 */
package gov.noaa.nws.ncep.ui.pgen.elements;

import org.junit.After;
import org.junit.AfterClass;
import org.junit.Before;
import org.junit.BeforeClass;
import org.junit.Test;

/**
 * JUnit test for Class "ProductRelationship".
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date       	Ticket#		Engineer	Description
 * ------------	----------	-----------	--------------------------
 * 10/22/08					J. Wu   	Initial Creation.
 *
 * </pre>
 * 
 * @author	J. Wu
 * @version	0.0.1
 *
 */
public class ProductRelationshipTest {

	/**
	 * @throws java.lang.Exception
	 */
	@BeforeClass
	public static void setUpBeforeClass() throws Exception {
	}

	/**
	 * @throws java.lang.Exception
	 */
	@AfterClass
	public static void tearDownAfterClass() throws Exception {
	}

	/**
	 * @throws java.lang.Exception
	 */
	@Before
	public void setUp() throws Exception {
	}

	/**
	 * @throws java.lang.Exception
	 */
	@After
	public void tearDown() throws Exception {
	}

	/**
	 * Test method for {@link nmap.pgen.product.ProductRelationship#toString()}.
	 */
	@Test
	public final void testToString() {
		System.out.println ( "\nContent in ProductRelationship Class\n" );
		for ( ProductRelationship pr : ProductRelationship.values() ) {
			System.out.println ( pr + ":\t" + pr.ordinal() );
		}
	}

}
