/**
 * This Java class is the JUnit test for the AwwVtec.
 *
 * <pre>
 *
 * L. Lin       04/09   Creation
 * </pre>
 *
 */
package gov.noaa.nws.ncep.edex.plugin.aww.common;

import static org.junit.Assert.*;
import gov.noaa.nws.ncep.edex.plugin.aww.util.AwwParser;
import gov.noaa.nws.ncep.common.dataplugin.aww.AwwUgc;
import gov.noaa.nws.ncep.common.dataplugin.aww.AwwVtec;
import java.util.Calendar;
import org.junit.Test;


public class AwwVtecTest {
	
	private final String testVtecLine = "/O.EXT.KGRR.FL.W.0020.080914T2157Z-080915T1800Z/\r\r\n";
	
	private static final String testSegment = "MIC075-151705-\r\r\n"+
    "/O.EXT.KGRR.FL.W.0020.080914T2157Z-080915T1800Z/\n\n\r"+
    "/JACM4.2.ER.080914T2157Z.080915T0000Z.080915T0600Z.NR/\r\r\n"+
    "1105 AM EDT SUN SEP 14 2008\r\r\n" +
	"ATTN...WFO...BMX...HUN...JAN...MEG...OHX...\r\r\n"+
	"\r\r\n";

	AwwUgc testUgc = new AwwUgc();
	AwwVtec vtec = new AwwVtec();
	
	public void setUp() throws Exception {
	}

	@Test
	public void testProcessVtec() {
		
		vtec = AwwParser.processVtec(testVtecLine, testSegment);
		
		// Compare the differences
		String vtecLine = vtec.getVtecLine();
		assertEquals(vtecLine, testVtecLine);
		
		String prodClass = vtec.getProductClass();
		assertEquals(prodClass, "O");
		
		String action = vtec.getAction();
		assertEquals(action, "EXT");
		
		String officeID = vtec.getOfficeID();
		assertEquals(officeID, "KGRR");
		
		String phenomena = vtec.getPhenomena();
		assertEquals(phenomena, "FL");
		
		String significance = vtec.getSignificance();
		assertEquals(significance, "W");
		
		String eventTrackingNumber = vtec.getEventTrackingNumber();
		assertEquals(eventTrackingNumber, "0020");
		
		Calendar startTime = vtec.getEventStartTime();
		Calendar calstart = Calendar.getInstance();
		calstart.set(Calendar.YEAR, 2008);
		calstart.set(Calendar.MONTH, 8);
		calstart.set(Calendar.DATE, 14);
		calstart.set(Calendar.HOUR_OF_DAY, 21);
		calstart.set(Calendar.MINUTE, 57);
		calstart.set(Calendar.SECOND, 0);
		calstart.set(Calendar.MILLISECOND, 0);
		assertEquals(calstart, startTime);
		
		Calendar endTime = vtec.getEventEndTime();
		Calendar calend = Calendar.getInstance();
		calend.set(Calendar.YEAR, 2008);
		calend.set(Calendar.MONTH, 8);
		calend.set(Calendar.DATE, 15);
		calend.set(Calendar.HOUR_OF_DAY, 18);
		calend.set(Calendar.MINUTE, 00);
		calend.set(Calendar.SECOND, 0);
		calend.set(Calendar.MILLISECOND, 0);
		assertEquals(calend, endTime);
		
		
	}

}
