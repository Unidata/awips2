/**
 * This Java class is the JUnit test for the AwwUgc.
 *
 * <pre>
 *
 * L. Lin       04/09   Creation
 * </pre>
 *
 */
package gov.noaa.nws.ncep.edex.plugin.aww.common;

import static org.junit.Assert.*;
import gov.noaa.nws.ncep.common.dataplugin.aww.AwwUgc;
import gov.noaa.nws.ncep.edex.plugin.aww.util.AwwParser;
import java.util.Calendar;
import org.junit.Test;
import java.util.ArrayList;


public class AwwUgcTest {

	private final String testUgcLine = "MIC075-151705-\r\r\n";
	private static final String testSegment = "MIC075-151705-\r\r\n"+
    "/O.EXT.KGRR.FL.W.0020.080914T2157Z-080915T1800Z/\n\n\r"+
    "/JACM4.2.ER.080914T2157Z.080915T0000Z.080915T0600Z.NR/\r\r\n"+
    "1105 AM EDT SUN SEP 14 2008\r\r\n" +
	"ATTN...WFO...BMX...HUN...JAN...MEG...OHX...\r\r\n"+
	"\r\r\n";
                                            
	AwwUgc ugc = new AwwUgc();

	public void setUp() throws Exception {
	}
	
	@Test
	public void testProcessUgc() {
		
		ArrayList<String> watchesList = new ArrayList<String>();
		Calendar mndTime = null;
		
		AwwUgc ugc=AwwParser.processUgc(testUgcLine, testSegment, mndTime, watchesList);
		
		String ugcLine=ugc.getUgc();
		assertEquals(ugcLine, testUgcLine);
		
	}

}
