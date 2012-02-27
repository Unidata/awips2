/**
 * 
 * NcPafmParserTest
 * 
 * This class contains the JUnit testing for NcPafmParser
 * 
 * <pre>
 *      
 * SOFTWARE HISTORY
 *      
 * Date       	Ticket#		Engineer	Description
 * ------------	----------	-----------	--------------------------
 * 08/21/2009   126         F. J. Yen   Initial Creation
 * 12/11/2009	126			F. J. Yen	Migrated from to11d3 to to11d6
 *       
 * </pre>
 * 
 * @author Fee Jing Yen, SIB
 * @version 1
 */
package gov.noaa.nws.ncep.edex.plugin.ncpafm.util;

import static org.junit.Assert.*;
import gov.noaa.nws.ncep.common.dataplugin.ncpafm.NcPafmBulletin;
import gov.noaa.nws.ncep.edex.plugin.ncpafm.util.NcPafmParser;
import gov.noaa.nws.ncep.edex.tools.decoder.MndTime;
import gov.noaa.nws.ncep.common.tools.IDecoderConstantsN;

import java.util.Calendar;
import java.util.regex.Pattern;

import org.junit.After;
import org.junit.AfterClass;
import org.junit.Before;
import org.junit.BeforeClass;
import org.junit.Test;

public class NcPafmParserTest {

	private static final float RMISSD = IDecoderConstantsN.FLOAT_MISSING;
	@BeforeClass
	public static void setUpBeforeClass() throws Exception {

	}

	@AfterClass
	public static void tearDownAfterClass() throws Exception {
	}

	@Before
	public void setUp() throws Exception {

	}

	@After
	public void tearDown() throws Exception {
	}

	@Test
	public void testProcessWMO() {
		 Calendar mndTime = null;
         NcPafmBulletin record;
         final String wmoHeader = "WOUS64";
         final String testBull = "WOUS64 KWNS 190404\n\n\r"+
         "/O.EXT.KGRR.FL.W.0020.080914T2157Z-080915T1800Z/\n\n\r"+
         "/JACM4.2.ER.080914T2157Z.080915T0000Z.080915T0600Z.NR/\r\r\n"+
         "1105 AM EDT SUN SEP 14 2008\r\r\n" +
         "ATTN...WFO...BMX...HUN...JAN...MEG...OHX...\r\r\n"+
         "\r\r\n";
         
      // Set MND (Mass News Disseminator) time string and convert it into Calendar object
     	MndTime mt = new MndTime(testBull.getBytes());
     	mndTime = mt.getMndTime();

         record = new NcPafmBulletin();

         record = NcPafmParser.processWMO(testBull, mndTime);
         String wmo=record.getWmoHeader();
         assertEquals(wmo, wmoHeader);
	}


	@Test
	public void testParseFlParmRow() {
		final int maxNmHr = 48;

		int[] locStNdx = {13, 16, 19, 22, 25, 28, 31, 34, 37, 40, 43, 46, 49, 52, 55, 58, 61, 64, 67, 70, 73, 76};
		int[] locEnNdx = {16, 19, 22, 25, 28, 31, 34, 37, 40, 43, 46, 49, 52, 55, 58, 61, 64, 67, 70, 73, 76, 79};
		final Float[] tempAr = new Float[maxNmHr];
		Float [] tempExp = {RMISSD, RMISSD, RMISSD, 85f, 87f, 84f, 77f, 74f, 72f, 71f, 80f, 86f, 88f, 102f, 103f, 
				79f, -4f,-10f, 87f, 93f, 96f, 93f,
				RMISSD, RMISSD, RMISSD, RMISSD,RMISSD, RMISSD, RMISSD, RMISSD,RMISSD, RMISSD, RMISSD, RMISSD,
				RMISSD, RMISSD, RMISSD, RMISSD, RMISSD, RMISSD, RMISSD, RMISSD,RMISSD, RMISSD, RMISSD, RMISSD,
				RMISSD, RMISSD};
		int ndxSt = 0;
		int ndxLast = 22;
		
		String row = "TEMP                   85 87 84 77 74 72 71 80 86 88102103 79 -4-10 87 93 96 93\r\r\n";
			
		NcPafmParser.parseFlParmRow(row, ndxSt, ndxLast, locStNdx, locEnNdx, tempAr);
		assertEquals (tempExp, tempAr);
	}
	
	@Test
	public void testParseSnowRangeParmRow() {
		final int maxNmHr = 48;
		final String snow12Val = "( MM( |\\r))|( T( |\\r))|( 00-00( |\\r))|( \\d{1,2}( |\\r))|( \\d{1,2}-\\d{1,2}( |\\r))";
        final Pattern snow12ValPattern = Pattern.compile(snow12Val);

		int[] locStNdx = {13, 16, 19, 22, 25, 28, 31, 34, 37, 40, 43, 46, 49, 52, 55, 58, 61, 64, 67, 70, 73, 76};
		int[] locEnNdx = {16, 19, 22, 25, 28, 31, 34, 37, 40, 43, 46, 49, 52, 55, 58, 61, 64, 67, 70, 73, 76, 79};
		final Float[] tempMnAr = new Float[maxNmHr];
		final Float[] tempMxAr = new Float[maxNmHr];
		Float [] tempMnExp = {RMISSD, RMISSD, RMISSD, RMISSD, RMISSD, 0f, RMISSD, RMISSD, RMISSD, 1f, RMISSD,
				RMISSD,RMISSD, RMISSD, RMISSD, RMISSD, RMISSD, RMISSD, RMISSD,RMISSD, RMISSD, RMISSD,
				RMISSD, RMISSD, RMISSD, RMISSD,RMISSD, RMISSD, RMISSD, RMISSD,RMISSD, RMISSD, RMISSD, RMISSD,
				RMISSD, RMISSD, RMISSD, RMISSD, RMISSD, RMISSD, RMISSD, RMISSD,RMISSD, RMISSD, RMISSD, RMISSD,
				RMISSD, RMISSD};
		Float [] tempMxExp = {RMISSD, RMISSD, RMISSD, RMISSD, RMISSD, 0f, RMISSD, RMISSD, RMISSD, 3f, RMISSD,
		RMISSD,RMISSD, RMISSD, RMISSD, RMISSD, RMISSD, RMISSD, RMISSD,RMISSD, RMISSD, RMISSD,
		RMISSD, RMISSD, RMISSD, RMISSD,RMISSD, RMISSD, RMISSD, RMISSD,RMISSD, RMISSD, RMISSD, RMISSD,
		RMISSD, RMISSD, RMISSD, RMISSD, RMISSD, RMISSD, RMISSD, RMISSD,RMISSD, RMISSD, RMISSD, RMISSD,
		RMISSD, RMISSD};
		int ndxSt = 0;
		int ndxLast = 22;
		
		String row = "SNOW                      00-00       01-03                                    \r\r\n";

		NcPafmParser.parseSnowRangeParmRow(row, snow12ValPattern, ndxSt, ndxLast, locStNdx, locEnNdx, tempMnAr, tempMxAr);
		System.out.println ("tempMxAr[9]=" + tempMxAr[9] + " elev=" + tempMxAr[11]);
		System.out.println ("tempMnAr[9] =" + tempMnAr[9] + " twel=" + tempMnAr[12]);
		assertEquals (tempMxExp[5], tempMxAr[5]);
		assertEquals (tempMnExp[5], tempMnAr[5]);
		assertEquals (tempMxExp[9], tempMxAr[9]);
		assertEquals (tempMnExp[9], tempMnAr[9]);
		
	}

	@SuppressWarnings("deprecation")
	@Test
	public void testParseStrParmRow() {
		final int maxNmHr = 48;

		int[] locStNdx = {13, 16, 19, 22, 25, 28, 31, 34, 37, 40, 43, 46, 49, 52, 55, 58, 61, 64, 67, 70, 73, 76};
		int[] locEnNdx = {16, 19, 22, 25, 28, 31, 34, 37, 40, 43, 46, 49, 52, 55, 58, 61, 64, 67, 70, 73, 76, 79};
		final String[] cloudsAr = new String[maxNmHr];
		String [] cloudsExp = {"   ", " FW", " FW", " FW", " FW", " SC", " SC", " SC", " B1", " B1"," B1", " SC", " SC", " SC", 
				" SC", " SC", " SC", " SC", " SC", " FW", " FW", " FW",
				"   ", "   ", "   ", "   ","   ", "   ", "   ", "   ","   ", "   ", "   ", "   ",
				"   ", "   ", "   ", "   ","   ", "   ", "   ", "   ","   ", "   ", "   ", "   ",
				"   ", "   "};
		int ndxSt = 0;
		int ndxLast = 22;
		
		String row = "CLOUDS           FW FW FW FW SC SC SC B1 B1 B1 SC SC SC SC SC SC SC SC FW FW FW\r\r\n";
		
		NcPafmParser.parseStrParmRow(row, ndxSt, ndxLast, locStNdx, locEnNdx, cloudsAr);
		assertEquals (cloudsExp, cloudsAr);
	}

}
