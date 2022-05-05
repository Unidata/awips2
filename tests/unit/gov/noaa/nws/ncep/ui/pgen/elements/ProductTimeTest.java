/**
 * 
 */
package gov.noaa.nws.ncep.ui.pgen.elements;

import java.util.Calendar;

import static org.junit.Assert.*;

import org.junit.After;
import org.junit.AfterClass;
import org.junit.Before;
import org.junit.BeforeClass;
import org.junit.Test;

/**
 * JUnit test for class "ProductTime".
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
public class ProductTimeTest {

	private Calendar tcalendar;	
	
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
	 * @throws java.lang.ExcepstartTimetion
	 */
	@Before
	public void setUp() throws Exception {
		tcalendar = (Calendar)Calendar.getInstance();
	}

	/**
	 * @throws java.lang.Exception
	 */
	@After
	public void tearDown() throws Exception {
	}

	/**
	 * Test method for {@link nmap.pgen.product.ProductTime#compare(nmap.pgen.product.ProductTime)}.
	 */
	@Test
	public final void testCompare() {
	    ProductTime ninfo = new ProductTime( tcalendar );
	    ProductTime ninfo1 = new ProductTime( tcalendar );	    
	    assertTrue( ninfo.compare( ninfo1 ) );	
	}

	/**
	 * Test method for {@link nmap.pgen.product.ProductTime#getRange()}.
	 */
	@Test
	public final void testGetRange() {
		Calendar ncalendar = (Calendar)Calendar.getInstance();
		ProductTime ninfo = new ProductTime( tcalendar );
		ninfo.setEndTime( ncalendar );
		assertTrue( ninfo.getRange() == 
			        (ncalendar.getTimeInMillis() - tcalendar.getTimeInMillis()) );
	}
}
