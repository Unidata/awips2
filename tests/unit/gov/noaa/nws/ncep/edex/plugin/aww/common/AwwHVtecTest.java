/**
 * This Java class is the JUnit test for the AwwHVtec.
 *
 * <pre>
 *
 * L. Lin       04/09   Creation
 * </pre>
 *
 */
package gov.noaa.nws.ncep.edex.plugin.aww.common;

import static org.junit.Assert.*;
import gov.noaa.nws.ncep.common.dataplugin.aww.AwwHVtec;
import gov.noaa.nws.ncep.common.dataplugin.aww.AwwVtec;
import gov.noaa.nws.ncep.edex.plugin.aww.util.AwwParser;
import java.util.Calendar;
import org.junit.Test;
import java.util.Iterator;


public class AwwHVtecTest {

	private final String testVtecLine = "/O.EXT.KGRR.FL.W.0020.080914T2157Z-080915T1800Z/\r\r\n";
	private final String testHVtecLine = "/JACM4.2.ER.080914T2157Z.080915T0000Z.080915T0600Z.NR/";
	
	private static final String testSegment = "MIC075-151705-\r\r\n"+
    "/O.EXT.KGRR.FL.W.0020.080914T2157Z-080915T1800Z/\n\n\r"+
    "/JACM4.2.ER.080914T2157Z.080915T0000Z.080915T0600Z.NR/\r\r\n"+
    "1105 AM EDT SUN SEP 14 2008\r\r\n" +
	"ATTN...WFO...BMX...HUN...JAN...MEG...OHX...\r\r\n"+
	"\r\r\n";

	AwwHVtec hvtec = new AwwHVtec();
	AwwVtec vtec = new AwwVtec();

	public void setUp() throws Exception {
	}

	@Test
	public void testProcessHVtec() {
		
		vtec = AwwParser.processVtec(testVtecLine, testSegment);
		
		if(vtec.getAwwHVtecLine() != null && vtec.getAwwHVtecLine().size() > 0)
	      {
	         for (Iterator<AwwHVtec> iter = vtec.getAwwHVtecLine().iterator(); iter.hasNext();) {
	            hvtec = iter.next();
	      
	            // Compare the differences
	    		String hvtecLine = hvtec.getHvtecLine();
	    		assertEquals(hvtecLine, testHVtecLine);
	    		
	    		String floodSeverity = hvtec.getFloodSeverity();
	    		assertEquals(floodSeverity, "2");
	    		
	    		String immediateCause = hvtec.getImmediateCause();
	    		assertEquals(immediateCause, "ER");
	    		
	    		Calendar startTime = hvtec.getEventStartTime();
	    		Calendar calstart = Calendar.getInstance();
	    		calstart.set(Calendar.YEAR, 2008);
	    		calstart.set(Calendar.MONTH, 8);
	    		calstart.set(Calendar.DATE, 14);
	    		calstart.set(Calendar.HOUR_OF_DAY, 21);
	    		calstart.set(Calendar.MINUTE, 57);
	    		calstart.set(Calendar.SECOND, 0);
	    		calstart.set(Calendar.MILLISECOND, 0);
	    		assertEquals(calstart, startTime);
	    		
	    		Calendar crestTime = hvtec.getEventCrestTime();
	    		Calendar calcrest = Calendar.getInstance();
	    		calcrest.set(Calendar.YEAR, 2008);
	    		calcrest.set(Calendar.MONTH, 8);
	    		calcrest.set(Calendar.DATE, 15);
	    		calcrest.set(Calendar.HOUR_OF_DAY, 00);
	    		calcrest.set(Calendar.MINUTE, 00);
	    		calcrest.set(Calendar.SECOND, 0);
	    		calcrest.set(Calendar.MILLISECOND, 0);
	    		assertEquals(calcrest, crestTime);
	    		
	    		Calendar endTime = hvtec.getEventEndTime();
	    		Calendar calend = Calendar.getInstance();
	    		calend.set(Calendar.YEAR, 2008);
	    		calend.set(Calendar.MONTH, 8);
	    		calend.set(Calendar.DATE, 15);
	    		calend.set(Calendar.HOUR_OF_DAY, 06);
	    		calend.set(Calendar.MINUTE, 00);
	    		calend.set(Calendar.SECOND, 0);
	    		calend.set(Calendar.MILLISECOND, 0);
	    		assertEquals(calend, endTime);
	    		
	         }
	      }
		
	}

}
