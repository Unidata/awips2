/**
 * This Java class is the JUnit test for the AwwFips.
 *
 * <pre>
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    	Engineer    Description
 * ------------ ---------- 	----------- --------------------------
 * 10/2008		 38			L. Lin		Creation
 * 04/2009		128			T. Lee		Append ^r^n to testMndLine
 * </pre>
 * 
 * @author L. Lin
 * @version 1.0       
 */
package gov.noaa.nws.ncep.edex.plugin.aww.common;

import static org.junit.Assert.*;
import java.util.Calendar;
import gov.noaa.nws.ncep.edex.tools.decoder.MndTime;
import gov.noaa.nws.ncep.edex.plugin.aww.util.AwwParser;
import gov.noaa.nws.ncep.common.dataplugin.aww.AwwFips;
import gov.noaa.nws.ncep.common.dataplugin.aww.AwwUgc;

import java.util.Iterator;
import java.util.ArrayList;
import java.util.TimeZone;

import org.junit.Test;

public class AwwFipsTest {
	
	private final String testMndLine = "1005 AM EDT TUE SEP 16 2008\r\r\n";
	private final String testUgcLine = "NDZ031-076-MIC094-162205-";
	
	MndTime mt = new MndTime(testMndLine.getBytes());
	Calendar mndTime = mt.getMndTime();
	
	AwwUgc testUgc = new AwwUgc();
	
	AwwFips af = new AwwFips();
	
	public void setUp() throws Exception {
	}
	
	@Test
	public void testProcessFips() {
				
		ArrayList<String> cfipsList = new ArrayList<String>();
		System.out.println("mndtime -->"+ mndTime);

		
		cfipsList.add("NDZ031");
		cfipsList.add("NDZ076");
		cfipsList.add("MIC094");
		
		AwwParser.processFips(testUgcLine, testUgc, mndTime);
		
		/*
		 * test the product purge date
		 */
		Calendar cal = Calendar.getInstance(TimeZone.getTimeZone("GMT"));
		cal.set(Calendar.YEAR, 2008);
		cal.set(Calendar.MONTH, 8);
		cal.set(Calendar.DATE, 16);
		cal.set(Calendar.HOUR_OF_DAY, 22);
		cal.set(Calendar.MINUTE, 5);
		cal.set(Calendar.SECOND, 0);
		cal.set(Calendar.MILLISECOND, 0);
		Calendar cc = testUgc.getProdPurgeTime();
		assertEquals(cal,cc);
		
		// test the county fips
		if (testUgc.getAwwFIPS() !=null && testUgc.getAwwFIPS().size() >0) {
			for (Iterator<AwwFips> iter = testUgc.getAwwFIPS().iterator(); iter.hasNext();) {
	            AwwFips cond = iter.next();
	            String fips = cond.getFips();
	            assertTrue(cfipsList.contains(fips));
	            
	         }
		}
		
	}

}
