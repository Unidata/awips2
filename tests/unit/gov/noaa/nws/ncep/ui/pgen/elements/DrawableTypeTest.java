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
 * Junit test code for Class "DrawableType" - printing out all defined DrawableTypes
 * to the console.
 * 
 * @author jwu
 *
 */
public class DrawableTypeTest {

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
	 * Test method for {@link nmap.pgen.drawable.DrawableType#toString()}.
	 */
	@Test
	public final void testToString() {
		System.out.println ( "\nContent in DrawableType Class\n" );
		for ( DrawableType dt : DrawableType.values() ) {
			System.out.println ( dt + ":\t" + dt.ordinal() );
		}
	}

}
