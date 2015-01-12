/**
 * 
 */
package gov.noaa.nws.ncep.ui.pgen.elements;

import java.util.HashMap;

import static org.junit.Assert.*;

import org.junit.After;
import org.junit.AfterClass;
import org.junit.Before;
import org.junit.BeforeClass;
import org.junit.Test;

/**
 * @author jwu
 *
 */
public class ProductInfoTest {
	
	ProductInfo info;
	HashMap<String, String> tmap;
	
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
		tmap = new HashMap<String, String>();
		tmap.put( "Center", "AWC" );
		info = new ProductInfo( tmap );		
    }

	/**
	 * @throws java.lang.Exception
	 */
	@After
	public void tearDown() throws Exception {
	}
	
	@Test
	public void testTmap() {
		assertNotNull(tmap);
	}

}
