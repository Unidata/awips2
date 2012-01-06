/**
 * 
 * PRLibraryTest
 * 
 * This class contains the JUnit testing for PRLibrary
 * 
 * * <pre>
 *      
 * SOFTWARE HISTORY
 *      
 * Date       	Ticket#		Engineer	Description
 * ------------	----------	-----------	--------------------------
 * 04 Apr 2011	398		F. J. Yen   Initial Creation (Not complete. Not final.)
 *       
 * </pre>
 * 
 * @author Fee Jing Yen, SIB
 * @version 1
 * 
 */
package gov.noaa.nws.ncep.gempak.parameterConversionLibrary;

import static org.junit.Assert.*;
import gov.noaa.nws.ncep.gempak.parameterconversionlibrary.GempakConstants;
import gov.noaa.nws.ncep.gempak.parameterconversionlibrary.PRLibrary;

import org.junit.Test;

/**
 * @author fjyen
 *
 */
public class PRLibraryTest {
	private static int testCaseNum = 0;

//	/**
//	 * Test method for {@link gov.noaa.nws.ncep.gempak.parameterconversionlibrary.PRLibrary#getRZLLInstance()}.
//	 */
//	@Test
//	public void testGetRZLLInstance() {
//		fail("Not yet implemented");
//	}

	/**
	 * Test method for {@link gov.noaa.nws.ncep.gempak.parameterconversionlibrary.PRLibrary#prAlti(float)}.
	 */
	@Test
	public void testPrAlti() {
		testCaseNum++;
		float altInch = PRLibrary.prAlti(850f);
		System.out.println("testPrAlti testCaseNum=" + testCaseNum +
				"a:  altimeter input in millibars= 850. altimeter in inches=" + altInch);
		assertEquals (25.1002712f, altInch);
		altInch = PRLibrary.prAlti(1000f);
		System.out.println("testPrAlti testCaseNum=" + testCaseNum +
				"b: altimeter input in millibars= 1000. altimeter in inches=" + altInch);
		assertEquals (29.5297298f, altInch);
		altInch = PRLibrary.prAlti(2000f);
		System.out.println("testPrAlti testCaseNum=" + testCaseNum +
				"c:  altimeter input in millibars= 2000. altimeter in inches=" + altInch);
		assertEquals (59.0594597f, altInch);
		altInch = PRLibrary.prAlti(10920f);
		System.out.println("testPrAlti testCaseNum=" + testCaseNum +
				"d:  altimeter input in millibars= 10920. altimeter in inches=" + altInch);
		assertEquals (322.464661f, altInch);
		altInch = PRLibrary.prAlti(-20f);
		System.out.println("testPrAlti testCaseNum=" + testCaseNum +
				"e:  altimeter input in millibars= -20. altimeter in inches=" + altInch);
		assertEquals (-0.59059459f, altInch);
	}

	/**
	 * Test method for {@link gov.noaa.nws.ncep.gempak.parameterconversionlibrary.PRLibrary#prAltm(float)}.
	 */
	@Test
	public void testPrAltm() {
		testCaseNum++;
		float altMb = PRLibrary.prAltm(29.921f);
		System.out.println("testPrAltm testCaseNum=" + testCaseNum +
				"a:  altimeter input in inches= 29.921 altimeter in millibars=" + altMb);
		assertEquals (1013.25f, altMb);
		altMb = PRLibrary.prAltm(59.06f);
		System.out.println("testPrAltm testCaseNum=" + testCaseNum +
				"a:  altimeter input in inches= 2000.0183159 altimeter in millibars=" + altMb);
		assertEquals (2000.01831f, altMb);
		altMb = PRLibrary.prAltm(59.05945f);
		System.out.println("testPrAltm testCaseNum=" + testCaseNum +
				"b:  altimeter input in inche=59.05945 altimeter in millibars=" + altMb);
		assertEquals (1999.99951f, altMb);
		altMb = PRLibrary.prAltm(59.059459f);
		System.out.println("testPrAltm testCaseNum=" + testCaseNum +
				"c:  altimeter input in inches=59.059459  altimeter in millibars=" + altMb);
		assertEquals (2000f, altMb, .0005f);
		altMb = PRLibrary.prAltm(29f);
		System.out.println("testPrAltm testCaseNum=" + testCaseNum +
				"d:  altimeter input in inche=29. altimeter in millibars=" + altMb);
		assertEquals (982.061096f, altMb);
		altMb = PRLibrary.prAltm(59.0594597f);
		System.out.println("testPrAltm testCaseNum=" + testCaseNum +
				"e:  altimeter input in inches= 59.0594597. altimeter in millibars=" + altMb);
		assertEquals (2000f, altMb, .0005f);
	}

	/**
	 * Test method for {@link gov.noaa.nws.ncep.gempak.parameterconversionlibrary.PRLibrary#prAltp(float, float)}.
	 */
	@Test
	public void testPrAltp() {
		testCaseNum++;
		float altP = PRLibrary.prAltp(1020f, 2000f);
		System.out.println("testPrAltp testCaseNum=" + testCaseNum +
				"a:  input pressure= 1020. sea elev=  2000.   output: altimeter in inches=" + altP);
		assertEquals (38.3914757f, altP, .0005f);
		altP = PRLibrary.prAltp(500f, 10000f);
		System.out.println("testPrAltp testCaseNum=" + testCaseNum +
				"b:  input pressure=  500. sea elev= 10000.   output: altimeter in inches=" + altP);
		assertEquals (56.590477f, altP, .0005f);
		altP = PRLibrary.prAltp(230f, 0f);
		System.out.println("testPrAltp testCaseNum=" + testCaseNum +
				"c:  input pressure=  230. sea elev=     0.   output: altimeter in inches=" + altP);
		assertEquals (6.79183817f, altP, .0005f);
		altP = PRLibrary.prAltp(250f, -100f);
		System.out.println("testPrAltp testCaseNum=" + testCaseNum +
				"d:  input pressure=  250. sea elev=  -100.   output: altimeter in inches=" + altP);
		assertEquals (7.29552078, altP, .0005f);
	}

	/**
	 * Test method for {@link gov.noaa.nws.ncep.gempak.parameterconversionlibrary.PRLibrary#prTpwn(float, float, float)}.
	 */
	@Test
	public void testPrTpwn() {
		testCaseNum++;
		float tpwn = PRLibrary.prTpwn(20f, 49f, 30f);
		System.out.println("testPrTpwn testCaseNum=" + testCaseNum +
				"a:  input twnm= 20. vwnm=  49. pprb=30  output: temporary wx code twnm=" + tpwn);
		assertEquals (20f, tpwn, .0005f);
		tpwn = PRLibrary.prTpwn(30.0001f, 25.02f, 28.03f);
		System.out.println("testPrTpwn testCaseNum=" + testCaseNum +
				"b:  input twnm= 30. vwnm=  25.02. pprb=30.0001  output: temporary wx code twnm=" + tpwn);
		assertEquals (25.0200005f, tpwn, .0005f);
		tpwn = PRLibrary.prTpwn(30.0001f, 25.02f, 29.9f);
		System.out.println("testPrTpwn testCaseNum=" + testCaseNum +
				"c:  input twnm= 30. vwnm=  25.02. pprb=30.0001  output: temporary wx code twnm=" + tpwn);
		assertEquals (30.0000992f, tpwn, .0005f);
		tpwn = PRLibrary.prTpwn(GempakConstants.RMISSD, 25.02f, 29.9f);
		System.out.println("testPrTpwn testCaseNum=" + testCaseNum +
				"d:  input twnm= GempakConstants.RMISSD vwnm=  25.9. pprb=30.0001  output: temporary wx code twnm=" + tpwn);
		assertEquals (25.0200005f, tpwn, .0005f);
		tpwn = PRLibrary.prTpwn(GempakConstants.RMISSD, GempakConstants.RMISSD, 30.f);
		System.out.println("testPrTpwn testCaseNum=" + testCaseNum +
				"e:  input twnm= GempakConstants.RMISSD vwnm=  GempakConstants.RMISSD pprb=30.  output: temporary wx code twnm=" + tpwn);
		assertEquals (GempakConstants.RMISSD, tpwn, .0005f);		
	}

	/**
	 * Test method for {@link gov.noaa.nws.ncep.gempak.parameterconversionlibrary.PRLibrary#prAwnm(float, float, float, float)}.
	 */
	@Test
	public void testPrAwnm() {
		testCaseNum++;
		float awnm = PRLibrary.prAwnm(45f, 55f, 65f, 30f);
		System.out.println("testPrAwnm testCaseNum=" + testCaseNum +
				"a:  input wnum=45 twnm= 55. vwnm=  65. pprb=30  output: all wx numeric code awnm=" + awnm);
		assertEquals (55f, awnm, .0005f);
		awnm = PRLibrary.prAwnm(20f, 30f, 40f, 28f);
		System.out.println("testPrAwnm testCaseNum=" + testCaseNum +
				"b:  input wnum=20 twnm= 30. vwnm=  40. pprb=28  output: all wx numeric code awnm=" + awnm);
		assertEquals (40f, awnm, .0005f);
		awnm = PRLibrary.prAwnm(20f, 30f, GempakConstants.RMISSD, 28f);
		System.out.println("testPrAwnm testCaseNum=" + testCaseNum +
				"c:  input wnum=20 twnm= 30. vwnm=  GempakConstants.RMISSD pprb=28  output: all wx numeric code awnm=" + awnm);
		assertEquals (30f, awnm, .0005f);
		awnm = PRLibrary.prAwnm(20f, GempakConstants.RMISSD, 40f, 30f);
		System.out.println("testPrAwnm testCaseNum=" + testCaseNum +
				"d:  input wnum=20 twnm= GempakConstants.RMISSD vwnm=  40. pprb=30  output: all wx numeric code awnm=" + awnm);
		assertEquals (40f, awnm, .0005f);
		awnm = PRLibrary.prAwnm(20f, GempakConstants.RMISSD, 40f, 31f);
		System.out.println("testPrAwnm testCaseNum=" + testCaseNum +
				"e:  input wnum=20 twnm= GempakConstants.RMISSD vwnm=  40. pprb=31  output: all wx numeric code awnm=" + awnm);
		assertEquals (40f, awnm, .0005f);
		awnm = PRLibrary.prAwnm(20f, GempakConstants.RMISSD, 40f, 29.6f);
		System.out.println("testPrAwnm testCaseNum=" + testCaseNum +
				"f:  input wnum=20 twnm= GempakConstants.RMISSD vwnm=  40. pprb=29.6  output: all wx numeric code awnm=" + awnm);
		assertEquals (40f, awnm, .0005f);
		awnm = PRLibrary.prAwnm(10f, GempakConstants.RMISSD, GempakConstants.RMISSD, 30f);
		System.out.println("testPrAwnm testCaseNum=" + testCaseNum +
				"g:  input wnum=10 twnm= GempakConstants.RMISSD vwnm=  GempakConstants.RMISSD pprb=30  output: all wx numeric code awnm=" + awnm);
		assertEquals (10f, awnm, .0005f);
	}

	/**
	 * Test method for {@link gov.noaa.nws.ncep.gempak.parameterconversionlibrary.PRLibrary#prAmsl(float)}.
	 */
	@Test
	public void testPrAmsl() {
		testCaseNum++;
		float amsl = PRLibrary.prAmsl(650f);
		System.out.println("testPrAlti testCaseNum=" + testCaseNum +
				"a:  pressure input in millibars= 850. 3-digit display of pressure=" + amsl);
		assertEquals (500f, amsl);
		amsl = PRLibrary.prAmsl(820.34f);
		System.out.println("testPrAlti testCaseNum=" + testCaseNum +
				"b:  pressure input in millibars= 850. 3-digit display of pressure=" + amsl);
		assertEquals (203f, amsl);
		amsl = PRLibrary.prAmsl(1275.59f);
		System.out.println("testPrAlti testCaseNum=" + testCaseNum +
				"c:  pressure input in millibars= 1275. 3-digit display of pressure=" + amsl);
		assertEquals (756, amsl);
	}

	/**
	 * Test method for {@link gov.noaa.nws.ncep.gempak.parameterconversionlibrary.PRLibrary#prCfct(float)}.
	 */
	@Test
	public void testPrCfct() {
		testCaseNum++;
		float prcfct = PRLibrary.prCfct(1f);
		System.out.println("testPrCfct testCaseNum=" + testCaseNum +
				"a:  Input Numeric total cloud cover= 1.  Return WMO fractional cloud cover=" + prcfct);
		assertEquals (6f, prcfct);
		prcfct = PRLibrary.prCfct(0f);
		System.out.println("testPrCfct testCaseNum=" + testCaseNum +
				"b:  Input Numeric total cloud cover= 0.  Return WMO fractional cloud cover=" + prcfct);
		assertEquals (1f, prcfct);
		prcfct = PRLibrary.prCfct(2f);
		System.out.println("testPrCfct testCaseNum=" + testCaseNum +
				"c:  Input Numeric total cloud cover= 2.  Return WMO fractional cloud cover=" + prcfct);
		assertEquals (6f, prcfct);
		prcfct = PRLibrary.prCfct(3f);
		System.out.println("testPrCfct testCaseNum=" + testCaseNum +
				"d:  Input Numeric total cloud cover= 3.  Return WMO fractional cloud cover=" + prcfct);
		assertEquals (2f, prcfct);
		prcfct = PRLibrary.prCfct(5f);
		System.out.println("testPrCfct testCaseNum=" + testCaseNum +
				"e:  Input Numeric total cloud cover= 5.  Return WMO fractional cloud cover=" + prcfct);
		prcfct = PRLibrary.prCfct(8f);
		System.out.println("testPrCfct testCaseNum=" + testCaseNum +
				"f:  Input Numeric total cloud cover= 8.  Return WMO fractional cloud cover=" + prcfct);
		assertEquals (4f, prcfct);
		prcfct = PRLibrary.prCfct(9f);
		System.out.println("testPrCfct testCaseNum=" + testCaseNum +
				"g:  Input Numeric total cloud cover= 9.  Return WMO fractional cloud cover=" + prcfct);
		assertEquals (5f, prcfct);
		prcfct = PRLibrary.prCfct(10f);
		System.out.println("testPrCfct testCaseNum=" + testCaseNum +
				"h:  Input Numeric total cloud cover=10.  Return WMO fractional cloud cover=" + prcfct);
		assertEquals (GempakConstants.RMISSD, prcfct);
		prcfct = PRLibrary.prCfct(-1f);
		System.out.println("testPrCfct testCaseNum=" + testCaseNum +
				"i:  Input Numeric total cloud cover=-1.  Return WMO fractional cloud cover=" + prcfct);
		assertEquals (GempakConstants.RMISSD, prcfct);
	}

	/**
	 * Test method for {@link gov.noaa.nws.ncep.gempak.parameterconversionlibrary.PRLibrary#prClct(float, float, float)}.
	 */
	@Test
	public void testPrClct() {
		testCaseNum++;
		float prClct = PRLibrary.prClct(5f, 5f, 5f);
		System.out.println("testPrClct testCaseNum=" + testCaseNum +
				"a:  Input clcl, clcm, clch= 5, 5, 5  Return Max cloud cover=" + prClct);
		assertEquals (5f, prClct);
		prClct = PRLibrary.prClct(3f, 4f, 5f);
		System.out.println("testPrClct testCaseNum=" + testCaseNum +
				"b:  Input clcl, clcm, clch= 3, 4, 5  Return Max cloud cover=" + prClct);
		assertEquals (5f, prClct);
		prClct = PRLibrary.prClct(2f, 7f, 9f);
		System.out.println("testPrClct testCaseNum=" + testCaseNum +
				"c:  Input clcl, clcm, clch= 2, 7, 9  Return Max cloud cover=" + prClct);
		assertEquals (9f, prClct);
		prClct = PRLibrary.prClct(2f, 9f, 7f);
		System.out.println("testPrClct testCaseNum=" + testCaseNum +
				"d:  Input clcl, clcm, clch= 2, 9, 7  Return Max cloud cover=" + prClct);
		assertEquals (9f, prClct);
		prClct = PRLibrary.prClct(8f, 7f, 9f);
		System.out.println("testPrClct testCaseNum=" + testCaseNum +
				"e:  Input clcl, clcm, clch= 8, 7, 9  Return Max cloud cover=" + prClct);
		assertEquals (9f, prClct);
		prClct = PRLibrary.prClct(9f, 0f, 4f);
		System.out.println("testPrClct testCaseNum=" + testCaseNum +
				"f:  Input clcl, clcm, clch= 9, 0, 4  Return Max cloud cover=" + prClct);
		assertEquals (4f, prClct);
		prClct = PRLibrary.prClct(9f, 9f, 9f);
		System.out.println("testPrClct testCaseNum=" + testCaseNum +
				"g:  Input clcl, clcm, clch= 2, 9, 7  Return Max cloud cover=" + prClct);
		assertEquals (9f, prClct);
		prClct = PRLibrary.prClct(7f, 7f, 7f);
		System.out.println("testPrClct testCaseNum=" + testCaseNum +
				"h:  Input clcl, clcm, clch= 7, 7, 7  Return Max cloud cover=" + prClct);
		assertEquals (7f, prClct);
		prClct = PRLibrary.prClct(9f, 9f, 6f);
		System.out.println("testPrClct testCaseNum=" + testCaseNum +
				"h:  Input clcl, clcm, clch= 9, 9, 6  Return Max cloud cover=" + prClct);
		assertEquals (9f, prClct);
		prClct = PRLibrary.prClct(9f, 3f, 4f);
		System.out.println("testPrClct testCaseNum=" + testCaseNum +
				"h:  Input clcl, clcm, clch= 9, 3, 4  Return Max cloud cover=" + prClct);
		assertEquals (4f, prClct);
		prClct = PRLibrary.prClct(GempakConstants.RMISSD, GempakConstants.RMISSD, GempakConstants.RMISSD);
		System.out.println("testPrClct testCaseNum=" + testCaseNum +
				"i:  Input clcl, clcm, clch=GempakConstants.RMISSD, GempakConstants.RMISSD, GempakConstants.RMISSD  Return Max cloud cover=" + prClct);
		assertEquals (GempakConstants.RMISSD, prClct);
		/* 88888888 but legacy has 0   888 */
		prClct = PRLibrary.prClct(GempakConstants.RMISSD, 9f, 7f);
		System.out.println("testPrClct testCaseNum=" + testCaseNum +
				"j:  Input clcl, clcm, clch=GempakConstants.RMISSD, 9., 7.  Return Max cloud cover=" + prClct);
		assertEquals (GempakConstants.RMISSD, prClct);
		/* 88888888 but legacy has 9   888 */
		prClct = PRLibrary.prClct(9f, 7f, GempakConstants.RMISSD);
		System.out.println("testPrClct testCaseNum=" + testCaseNum +
				"j:  Input clcl, clcm, clch=9., 7., GempakConstants.RMISSD  Return Max cloud cover=" + prClct);
		assertEquals (GempakConstants.RMISSD, prClct);
		/* 88888888 but legacy has 7    888 */
	}

	/**
	 * Test method for {@link gov.noaa.nws.ncep.gempak.parameterconversionlibrary.PRLibrary#prClcx(float)}.
	 */
	@Test
	public void testPrClcx() {
		testCaseNum++;
		float prClcx = PRLibrary.prClcx(389);
		System.out.println("testPrClcx testCaseNum=" + testCaseNum +
				"a:  Input comx= 389  Return Numeric cloud coverage code CLCx=" + prClcx);
		assertEquals (9f, prClcx);
		prClcx = PRLibrary.prClcx(3472);
		System.out.println("testPrClcx testCaseNum=" + testCaseNum +
				"b:  Input comx= 3472  Return Numeric cloud coverage code CLCx=" + prClcx);
		assertEquals (2f, prClcx);
		prClcx = PRLibrary.prClcx(GempakConstants.RMISSD);
		System.out.println("testPrClcx testCaseNum=" + testCaseNum +
				"c  Input comx= 3472  Return Numeric cloud coverage code CLCx=" + prClcx);
		assertEquals (GempakConstants.RMISSD, prClcx);
		prClcx = PRLibrary.prClcx(0f);
		System.out.println("testPrClcx testCaseNum=" + testCaseNum +
				"d  Input comx= 0  Return Numeric cloud coverage code CLCx=" + prClcx);
		assertEquals (0, prClcx);
		prClcx = PRLibrary.prClcx(-25f);
		System.out.println("testPrClcx testCaseNum=" + testCaseNum +
				"e  Input comx= -25  Return Numeric cloud coverage code CLCx=" + prClcx);
		assertEquals (-5, prClcx);
		prClcx = PRLibrary.prClcx(9876);
		System.out.println("testPrClcx testCaseNum=" + testCaseNum +
				"f  Input comx= 9876  Return Numeric cloud coverage code CLCx=" + prClcx);
		assertEquals (6, prClcx);
	}

	/**
	 * Test method for {@link gov.noaa.nws.ncep.gempak.parameterconversionlibrary.PRLibrary#prCldb(float, float, float, float)}.
	 */
	@Test
	public void testPrCldb() {
		testCaseNum++;
		float prCldb = PRLibrary.prCldb(25f, 221f, 807f, 254f);
		System.out.println("testPrCldb testCaseNum=" + testCaseNum +
				"a:  Input ceil, chc1, chc2, chc3 = 2450., 222., 807., 2504.  Return Lowest ceiling cldb=" + prCldb);
		assertEquals (254f, prCldb);
		prCldb = PRLibrary.prCldb(GempakConstants.RMISSD, 223f, 807f, 254f);
		System.out.println("testPrCldb testCaseNum=" + testCaseNum +
				"b:  Input ceil, chc1, chc2, chc3 = GempakConstants.RMISSD, 223., 807., 2504.  Return Lowest ceiling cldb=" + prCldb);
		assertEquals (223f, prCldb);
		prCldb = PRLibrary.prCldb(20f, 224f, 203f, 258f);
		System.out.println("testPrCldb testCaseNum=" + testCaseNum +
				"c:  Input ceil, chc1, chc2, chc3 = 22., 224., 201., 258.  Return Lowest ceiling cldb=" + prCldb);
		assertEquals (203f, prCldb);
		prCldb = PRLibrary.prCldb(25f, 248f, GempakConstants.RMISSD, 254f);
		System.out.println("testPrCldb testCaseNum=" + testCaseNum +
				"d:  Input ceil, chc1, chc2, chc3 = 25., 248., GempakConstants.RMISSD, 254.  Return Lowest ceiling cldb=" + prCldb);
		assertEquals (254f, prCldb);
		prCldb = PRLibrary.prCldb(GempakConstants.RMISSD, 223f, GempakConstants.RMISSD, 254f);
		System.out.println("testPrCldb testCaseNum=" + testCaseNum +
				"e:  Input ceil, chc1, chc2, chc3 = GempakConstants.RMISSD, 223., GempakConstants.RMISSD, 2504.  Return Lowest ceiling cldb=" + prCldb);
		assertEquals (223f, prCldb);
		prCldb = PRLibrary.prCldb(GempakConstants.RMISSD, 263f, GempakConstants.RMISSD, 254f);
		System.out.println("testPrCldb testCaseNum=" + testCaseNum +
				"f:  Input ceil, chc1, chc2, chc3 = GempakConstants.RMISSD, 263., GempakConstants.RMISSD, 2504.  Return Lowest ceiling cldb=" + prCldb);
		assertEquals (254f, prCldb);
		prCldb = PRLibrary.prCldb(GempakConstants.RMISSD, GempakConstants.RMISSD, 213f, 254f);
		System.out.println("testPrCldb testCaseNum=" + testCaseNum +
				"g:  Input ceil, chc1, chc2, chc3 = GempakConstants.RMISSD, GempakConstants.RMISSD, 213, 254.  Return Lowest ceiling cldb=" + prCldb);
		assertEquals (213f, prCldb);
		prCldb = PRLibrary.prCldb(GempakConstants.RMISSD, GempakConstants.RMISSD, GempakConstants.RMISSD, 254f);
		System.out.println("testPrCldb testCaseNum=" + testCaseNum +
				"h:  Input ceil, chc1, chc2, chc3 = GempakConstants.RMISSD, GempakConstants.RMISSD, GempakConstants.RMISSD, 254.  Return Lowest ceiling cldb=" + prCldb);
		assertEquals (254f, prCldb);
		prCldb = PRLibrary.prCldb(GempakConstants.RMISSD, 263f, 234f, GempakConstants.RMISSD);
		System.out.println("testPrCldb testCaseNum=" + testCaseNum +
				"i:  Input ceil, chc1, chc2, chc3 = GempakConstants.RMISSD, 263., 234., GempakConstants.RMISSD,   Return Lowest ceiling cldb=" + prCldb);
		assertEquals (234f, prCldb);
		prCldb = PRLibrary.prCldb(GempakConstants.RMISSD, 263f, 234f, GempakConstants.RMISSD);
		System.out.println("testPrCldb testCaseNum=" + testCaseNum +
				"j:  Input ceil, chc1, chc2, chc3 = GempakConstants.RMISSD, 213., 234., GempakConstants.RMISSD,   Return Lowest ceiling cldb=" + prCldb);
		assertEquals (234f, prCldb);
		prCldb = PRLibrary.prCldb(GempakConstants.RMISSD, 263f, 234f, 227f);
		System.out.println("testPrCldb testCaseNum=" + testCaseNum +
				"k:  Input ceil, chc1, chc2, chc3 = GempakConstants.RMISSD, 263., 234., 227.,   Return Lowest ceiling cldb=" + prCldb);
		assertEquals (227f, prCldb);
		prCldb = PRLibrary.prCldb(GempakConstants.RMISSD, 213f, 234f, 227f);
		System.out.println("testPrCldb testCaseNum=" + testCaseNum +
				"l:  Input ceil, chc1, chc2, chc3 = GempakConstants.RMISSD, 213., 234., 227.,   Return Lowest ceiling cldb=" + prCldb);
		assertEquals (213f, prCldb);
		prCldb = PRLibrary.prCldb(GempakConstants.RMISSD, GempakConstants.RMISSD, 213f, GempakConstants.RMISSD);
		System.out.println("testPrCldb testCaseNum=" + testCaseNum +
				"m:  Input ceil, chc1, chc2, chc3 = GempakConstants.RMISSD, GempakConstants.RMISSD, 213, GempakConstants.RMISSD  Return Lowest ceiling cldb=" + prCldb);
		assertEquals (213f, prCldb);
	}

	/**
	 * Test method for {@link gov.noaa.nws.ncep.gempak.parameterconversionlibrary.PRLibrary#prClhx(float)}.
	 */
	@Test
	public void testPrClhx() {
		testCaseNum++;
		float prClhx = PRLibrary.prClhx(9f);
		System.out.println("testPrClhx testCaseNum=" + testCaseNum +
				"a:  Input comx= 9.  Return Cloud height in hundreds of feet CLHx=" + prClhx);
		assertEquals (GempakConstants.RMISSD, prClhx);		
		prClhx = PRLibrary.prClhx(10053f);
		System.out.println("testPrClhx testCaseNum=" + testCaseNum +
				"b:  Input comx= 10053.  Return Cloud height in hundreds of feet CLHx=" + prClhx);
		assertEquals (5f, prClhx);
		prClhx = PRLibrary.prClhx(2758f);
		System.out.println("testPrClhx testCaseNum=" + testCaseNum +
				"c:  Input comx= 2758.  Return Cloud height in hundreds of feet CLHx=" + prClhx);
		assertEquals (275f, prClhx);
		prClhx = PRLibrary.prClhx(100f);
		System.out.println("testPrClhx testCaseNum=" + testCaseNum +
				"d:  Input comx= 100.  Return Cloud height in hundreds of feet CLHx=" + prClhx);
		assertEquals (10f, prClhx);
		prClhx = PRLibrary.prClhx(10f);
		System.out.println("testPrClhx testCaseNum=" + testCaseNum +
				"e:  Input comx= 10.  Return Cloud height in hundreds of feet CLHx=" + prClhx);
		assertEquals (1f, prClhx);
		prClhx = PRLibrary.prClhx(15483f);
		System.out.println("testPrClhx testCaseNum=" + testCaseNum +
				"f:  Input comx= 15483.  Return Cloud height in hundreds of feet CLHx=" + prClhx);
		assertEquals (548f, prClhx);
	}

	/**
	 * Test method for {@link gov.noaa.nws.ncep.gempak.parameterconversionlibrary.PRLibrary#prCloa(float)}.
	 */
	@Test
	public void testPrCloa() {
		testCaseNum++;
		float prCloa = PRLibrary.prCloa(2f);
		System.out.println("testPrCloa testCaseNum=" + testCaseNum +
				"a:  Input comx= 2.  Return Cloud height in hundreds of feet Cloa=" + prCloa);
		assertEquals (.40f, prCloa, .0005);
		prCloa = PRLibrary.prCloa(6f);
		System.out.println("testPrCloa testCaseNum=" + testCaseNum +
				"a:  Input comx= 6.  Return Cloud height in hundreds of feet Cloa=" + prCloa);
		assertEquals (.25f, prCloa, .0005);
		prCloa = PRLibrary.prCloa(8f);
		System.out.println("testPrCloa testCaseNum=" + testCaseNum +
				"a:  Input comx= 8.  Return Cloud height in hundreds of feet Cloa=" + prCloa);
		assertEquals (.9f, prCloa, .0005);
		prCloa = PRLibrary.prCloa(9f);
		System.out.println("testPrCloa testCaseNum=" + testCaseNum +
				"b:  Input comx= 9.  Return Cloud height in hundreds of feet Cloa=" + prCloa);
		assertEquals (0f, prCloa, .0005);
		prCloa = PRLibrary.prCloa(0f);
		System.out.println("testPrCloa testCaseNum=" + testCaseNum +
				"c:  Input comx= 0.  Return Cloud height in hundreds of feet Cloa=" + prCloa);
		assertEquals (0f, prCloa, .0005);
		prCloa = PRLibrary.prCloa(7f);
		System.out.println("testPrCloa testCaseNum=" + testCaseNum +
				"d:  Input comx= 7.  Return Cloud height in hundreds of feet Cloa=" + prCloa);
		assertEquals (.6f, prCloa, .0005);
		prCloa = PRLibrary.prCloa(11f);
		System.out.println("testPrCloa testCaseNum=" + testCaseNum +
				"e:  Input comx= 11.  Return Cloud height in hundreds of feet Cloa=" + prCloa);
		assertEquals (0f, prCloa, .0005);
		prCloa = PRLibrary.prCloa(-1f);
		System.out.println("testPrCloa testCaseNum=" + testCaseNum +
				"f:  Input comx= -1.  Return Cloud height in hundreds of feet Cloa=" + prCloa);
		assertEquals (0f, prCloa, .0005);
		prCloa = PRLibrary.prCloa(GempakConstants.RMISSD);
		System.out.println("testPrCloa testCaseNum=" + testCaseNum +
				"g:  Input comx= GempakConstants.RMISSD  Return Cloud height in hundreds of feet Cloa=" + prCloa);
//   Legacy does this:  assertEquals (0f, prCloa, .0005);
		assertEquals (GempakConstants.RMISSD, prCloa, .0005);
	}

	/**
	 * Test method for {@link gov.noaa.nws.ncep.gempak.parameterconversionlibrary.PRLibrary#prCmbc(float, float, float)}.
	 */
	@Test
	public void testPrCmbc() {
		testCaseNum++;
		float prCmbc = PRLibrary.prCmbc(2f, 10f, 3f);
		System.out.println("testPrCmbc testCaseNum=" + testCaseNum +
				"a:  Input clcl, clcm, clch= 2f, 10f, 3f  Return Combined low, min, and high cloud coverage=" + prCmbc);
		assertEquals (203f, prCmbc);
		prCmbc = PRLibrary.prCmbc(2f, 5f, 3f);
		System.out.println("testPrCmbc testCaseNum=" + testCaseNum +
				"b:  Input clcl, clcm, clch= 2., 5., 3.  Return Combined low, min, and high cloud coverage=" + prCmbc);
		assertEquals (253f, prCmbc);
		prCmbc = PRLibrary.prCmbc(10f, 3f, 4f);
		System.out.println("testPrCmbc testCaseNum=" + testCaseNum +
				"c:  Input clcl, clcm, clch= 10., 3., 4.  Return Combined low, min, and high cloud coverage=" + prCmbc);
		assertEquals (34f, prCmbc);
		prCmbc = PRLibrary.prCmbc(4f, 10f, 3f);
		System.out.println("testPrCmbc testCaseNum=" + testCaseNum +
				"d:  Input clcl, clcm, clch= 4., 10., 3.  Return Combined low, min, and high cloud coverage=" + prCmbc);		
		assertEquals (403f, prCmbc);
		prCmbc = PRLibrary.prCmbc(10f, GempakConstants.RMISSD, 3f);
		System.out.println("testPrCmbc testCaseNum=" + testCaseNum +
				"e:  Input clcl, clcm, clch= GempakConstants.RMISSD, 10., 3.  Return Combined low, min, and high cloud coverage=" + prCmbc);
		assertEquals (3f, prCmbc);
		prCmbc = PRLibrary.prCmbc(GempakConstants.RMISSD, 10f, 3f);
		System.out.println("testPrCmbc testCaseNum=" + testCaseNum +
				"f:  Input clcl, clcm, clch= GempakConstants.RMISSD, 10., 3.  Return Combined low, min, and high cloud coverage=" + prCmbc);	
		assertEquals (3f, prCmbc);
		prCmbc = PRLibrary.prCmbc(4f, 6f, 10f);
		System.out.println("testPrCmbc testCaseNum=" + testCaseNum +
				"g:  Input clcl, clcm, clch= 4., 6., 10.  Return Combined low, min, and high cloud coverage=" + prCmbc);
		assertEquals (460f, prCmbc);
	}

	/**
	 * Test method for {@link gov.noaa.nws.ncep.gempak.parameterconversionlibrary.PRLibrary#prCmsl(float, float)}.
	 */
	@Test
	public void testPrCmsl() {
		testCaseNum++;
		float cmsl = PRLibrary.prCmsl(2300f, 400f);
		System.out.println("testPrCmsl testCaseNum=" + testCaseNum +
				"a:  input ceiling in hundreds of ft= 2300. stn elev in meters=  400." +
				" output: Ceiling converted to MSL in 100's of ft=" + cmsl);
		assertEquals (2313.12012, cmsl, .0001f);
		cmsl = PRLibrary.prCmsl(4380f, 799f);
		System.out.println("testPrCmsl testCaseNum=" + testCaseNum +
				"b:  input ceiling in hundreds of ft= 4380. Stn elev in meters=  799." +
				" output: Ceiling converted to MSL in 100's of ft=" + cmsl);
		assertEquals (4406.20996f, cmsl, .0001f);
		cmsl = PRLibrary.prCmsl(10300f, 12000f);
		System.out.println("testPrCmsl testCaseNum=" + testCaseNum +
				"c:  input ceiling in hundreds of ft= 10300. Stn elev in meters=  12000." +
				" output: Ceiling converted to MSL in 100's of ft=" + cmsl);
		assertEquals (10693.7002f, cmsl, .0001f);
		cmsl = PRLibrary.prCmsl(930f, -200f);
		System.out.println("testPrCmsl testCaseNum=" + testCaseNum +
				"c:  input ceiling in hundreds of ft= 930.. Stn elev in meters=  -200." +
				" output: Ceiling converted to MSL in 100's of ft=" + cmsl);
		assertEquals (923.440002f, cmsl, .0001f);
		cmsl = PRLibrary.prCmsl(GempakConstants.RMISSD, 4500f);
		System.out.println("testPrCmsl testCaseNum=" + testCaseNum +
				"c:  input ceiling in hundreds of ft= GempakConstants.RMISSD Stn elev in meters=  4500." +
				" output: Ceiling converted to MSL in 100's of ft=" + cmsl);
		assertEquals (GempakConstants.RMISSD, cmsl, .0001f);
		cmsl = PRLibrary.prCmsl(2235, GempakConstants.RMISSD);
		System.out.println("testPrCmsl testCaseNum=" + testCaseNum +
				"c:  input ceiling in hundreds of ft= 2235.. Stn elev in meters=  GempakConstants.RMISSD" +
				" output: Ceiling converted to MSL in 100's of ft=" + cmsl);
		assertEquals (GempakConstants.RMISSD, cmsl, .0001f);
	}

	/**
	 * Test method for {@link gov.noaa.nws.ncep.gempak.parameterconversionlibrary.PRLibrary#prComh(float, float, float)}.
	 */
	@Test
	public void testPrComh() {
		fail("Not yet implemented");
	}

	/**
	 * Test method for {@link gov.noaa.nws.ncep.gempak.parameterconversionlibrary.PRLibrary#prComl(float, float, float)}.
	 */
	@Test
	public void testPrComl() {
		fail("Not yet implemented");
	}

	/**
	 * Test method for {@link gov.noaa.nws.ncep.gempak.parameterconversionlibrary.PRLibrary#prComm(float, float, float)}.
	 */
	@Test
	public void testPrComm() {
		fail("Not yet implemented");
	}

	/**
	 * Test method for {@link gov.noaa.nws.ncep.gempak.parameterconversionlibrary.PRLibrary#prComt(float, float, float)}.
	 */
	@Test
	public void testPrComt() {
		fail("Not yet implemented");
	}

	/**
	 * Test method for {@link gov.noaa.nws.ncep.gempak.parameterconversionlibrary.PRLibrary#prComx(float, float)}.
	 */
	@Test
	public void testPrComx() {
		fail("Not yet implemented");
	}

	/**
	 * Test method for {@link gov.noaa.nws.ncep.gempak.parameterconversionlibrary.PRLibrary#prCsyh(float)}.
	 */
	@Test
	public void testPrCsyh() {
		testCaseNum++;
		float csyh = PRLibrary.prCsyh(8f);
		System.out.println("testPrCsyh testCaseNum=" + testCaseNum +
				"a:  Input:Synoptic code for high clouds ctyh= 8   Output:Cloud type symbol number=" + csyh);
		assertEquals (28f, csyh);
		csyh = PRLibrary.prCsyh(3f);
		System.out.println("testPrCsyh testCaseNum=" + testCaseNum +
				"b:  Input:Synoptic code for high clouds ctyh= 3   Output:Cloud type symbol number=" + csyh);
		assertEquals (23f, csyh);
		csyh = PRLibrary.prCsyh(1f);
		System.out.println("testPrCsyh testCaseNum=" + testCaseNum +
				"c:  Input:Synoptic code for high clouds ctyh= 1   Output:Cloud type symbol number=" + csyh);
		assertEquals (21f, csyh);
		csyh = PRLibrary.prCsyh(0f);
		System.out.println("testPrCsyh testCaseNum=" + testCaseNum +
				"d:  Input:Synoptic code for high clouds ctyh= 0   Output:Cloud type symbol number=" + csyh);
		assertEquals (0f, csyh);
		csyh = PRLibrary.prCsyh(-1f);
		System.out.println("testPrCsyh testCaseNum=" + testCaseNum +
				"e:  Input:Synoptic code for high clouds ctyh= -1   Output:Cloud type symbol number=" + csyh);
		assertEquals (0f, csyh);
		csyh = PRLibrary.prCsyh(.1f);
		System.out.println("testPrCsyh testCaseNum=" + testCaseNum +
				"f:  Input:Synoptic code for high clouds ctyh= .1   Output:Cloud type symbol number=" + csyh);
		assertEquals (0f, csyh);
		csyh = PRLibrary.prCsyh(9f);
		System.out.println("testPrCsyh testCaseNum=" + testCaseNum +
				"g:  Input:Synoptic code for high clouds ctyh= 9   Output:Cloud type symbol number=" + csyh);
		assertEquals (29f, csyh);
		csyh = PRLibrary.prCsyh(9.01f);
		System.out.println("testPrCsyh testCaseNum=" + testCaseNum +
				"h:  Input:Synoptic code for high clouds ctyh= 9.01   Output:Cloud type symbol number=" + csyh);
		assertEquals (0f, csyh);
	}

	/**
	 * Test method for {@link gov.noaa.nws.ncep.gempak.parameterconversionlibrary.PRLibrary#prCsyl(float)}.
	 */
	@Test
	public void testPrCsyl() {
		testCaseNum++;
		float csyl = PRLibrary.prCsyl(8f);
		System.out.println("testPrCsyl testCaseNum=" + testCaseNum +
				"a:  Input:Synoptic code for low clouds ctyh= 8   Output:Cloud type symbol number=" + csyl);
		assertEquals (8f, csyl);
		csyl = PRLibrary.prCsyl(3f);
		System.out.println("testPrCsyl testCaseNum=" + testCaseNum +
				"b:  Input:Synoptic code for low clouds ctyh= 3   Output:Cloud type symbol number=" + csyl);
		assertEquals (3f, csyl);
		csyl = PRLibrary.prCsyl(1f);
		System.out.println("testPrCsyl testCaseNum=" + testCaseNum +
				"c:  Input:Synoptic code for low clouds ctyh= 1   Output:Cloud type symbol number=" + csyl);
		assertEquals (1f, csyl);
		csyl = PRLibrary.prCsyl(0f);
		System.out.println("testPrCsyl testCaseNum=" + testCaseNum +
				"d:  Input:Synoptic code for low clouds ctyh= 0   Output:Cloud type symbol number=" + csyl);
		assertEquals (0f, csyl);
		csyl = PRLibrary.prCsyl(-1f);
		System.out.println("testPrCsyl testCaseNum=" + testCaseNum +
				"e:  Input:Synoptic code for low clouds ctyh= -1   Output:Cloud type symbol number=" + csyl);
		assertEquals (0f, csyl);
		csyl = PRLibrary.prCsyl(.1f);
		System.out.println("testPrCsyl testCaseNum=" + testCaseNum +
				"f:  Input:Synoptic code for low clouds ctyh= .1   Output:Cloud type symbol number=" + csyl);
		assertEquals (0f, csyl);
		csyl = PRLibrary.prCsyl(9f);
		System.out.println("testPrCsyl testCaseNum=" + testCaseNum +
				"g:  Input:Synoptic code for low clouds ctyh= 9   Output:Cloud type symbol number=" + csyl);
		assertEquals (9f, csyl);
		csyl = PRLibrary.prCsyl(9.01f);
		System.out.println("testPrCsyl testCaseNum=" + testCaseNum +
				"h:  Input:Synoptic code for low clouds ctyh= 9.01   Output:Cloud type symbol number=" + csyl);
		assertEquals (0f, csyl);
		csyl = PRLibrary.prCsyl(5f);
		System.out.println("testPrCsyl testCaseNum=" + testCaseNum +
				"i:  Input:Synoptic code for low clouds ctyh= 5   Output:Cloud type symbol number=" + csyl);
		assertEquals (5f, csyl);
	}

	/**
	 * Test method for {@link gov.noaa.nws.ncep.gempak.parameterconversionlibrary.PRLibrary#prCsym(float)}.
	 */
	@Test
	public void testPrCsym() {
		testCaseNum++;
		float csym = PRLibrary.prCsym(8f);
		System.out.println("testPrCsym testCaseNum=" + testCaseNum +
				"a:  Input:Synoptic code for mid clouds ctyh= 8   Output:Cloud type symbol number=" + csym);
		assertEquals (18f, csym);
		csym = PRLibrary.prCsym(3f);
		System.out.println("testPrCsym testCaseNum=" + testCaseNum +
				"b:  Input:Synoptic code for mid clouds ctyh= 3   Output:Cloud type symbol number=" + csym);
		assertEquals (13f, csym);
		csym = PRLibrary.prCsym(1f);
		System.out.println("testPrCsym testCaseNum=" + testCaseNum +
				"c:  Input:Synoptic code for mid clouds ctyh= 1   Output:Cloud type symbol number=" + csym);
		assertEquals (11f, csym);
		csym = PRLibrary.prCsym(0f);
		System.out.println("testPrCsym testCaseNum=" + testCaseNum +
				"d:  Input:Synoptic code for mid clouds ctyh= 0   Output:Cloud type symbol number=" + csym);
		assertEquals (0f, csym);
		csym = PRLibrary.prCsym(-1f);
		System.out.println("testPrCsym testCaseNum=" + testCaseNum +
				"e:  Input:Synoptic code for mid clouds ctyh= -1   Output:Cloud type symbol number=" + csym);
		assertEquals (0f, csym);
		csym = PRLibrary.prCsym(.1f);
		System.out.println("testPrCsym testCaseNum=" + testCaseNum +
				"f:  Input:Synoptic code for mid clouds ctyh= .1   Output:Cloud type symbol number=" + csym);
		assertEquals (0f, csym);
		csym = PRLibrary.prCsym(9f);
		System.out.println("testPrCsym testCaseNum=" + testCaseNum +
				"g:  Input:Synoptic code for mid clouds ctyh= 9   Output:Cloud type symbol number=" + csym);
		assertEquals (19f, csym);
		csym = PRLibrary.prCsym(9.01f);
		System.out.println("testPrCsym testCaseNum=" + testCaseNum +
				"i:  Input:Synoptic code for mid clouds ctyh= 9.01   Output:Cloud type symbol number=" + csym);
		assertEquals (0f, csym);
		csym = PRLibrary.prCsym(4f);
		System.out.println("testPrCsym testCaseNum=" + testCaseNum +
				"j:  Input:Synoptic code for mid clouds ctyh= 4   Output:Cloud type symbol number=" + csym);
		assertEquals (14f, csym);
	}

	/**
	 * Test method for {@link gov.noaa.nws.ncep.gempak.parameterconversionlibrary.PRLibrary#prCsyt(float, float, float)}.
	 */
	@Test
	public void testPrCsyt() {
		testCaseNum++;
		float csyt = PRLibrary.prCsyt(8f, 3f, 9f);
		System.out.println("testPrCsyt testCaseNum=" + testCaseNum +
				"a:  Input:ctyl, ctym, ctyh= 8, 3, 9   Output:Cloud type symbol number for first reported level=" + csyt);
		assertEquals (8f, csyt);
		csyt = PRLibrary.prCsyt(3f, 7f, 1f);
		System.out.println("testPrCsyt testCaseNum=" + testCaseNum +
				"b:  Input:ctyl, ctym, ctyh= 3, 7, 1  Output:Cloud type symbol number for first reported level=" + csyt);
		assertEquals (3f, csyt);
		csyt = PRLibrary.prCsyt(1f, 2f, 3f);
		System.out.println("testPrCsyt testCaseNum=" + testCaseNum +
				"c:  Input:ctyl, ctym, ctyh= 1, 2, 3   Output:Cloud type symbol number for first reported level=" + csyt);
		assertEquals (1f, csyt);
		csyt = PRLibrary.prCsyt(0f, 7f, 8f);
		System.out.println("testPrCsyt testCaseNum=" + testCaseNum +
				"d:  Input:ctyl, ctym, ctyh= 0, 7, 8   Output:Cloud type symbol number for first reported level=" + csyt);
		assertEquals (17f, csyt);
		csyt = PRLibrary.prCsyt(-1f, 5f, 3f);
		System.out.println("testPrCsyt testCaseNum=" + testCaseNum +
				"e:  Input:ctyl, ctym, ctyh= -1   Output:Cloud type symbol number for first reported level=" + csyt);
		assertEquals (15f, csyt);
		csyt = PRLibrary.prCsyt(.1f, 0f, 5f);
		System.out.println("testPrCsyt testCaseNum=" + testCaseNum +
				"f:  Input:ctyl, ctym, ctyh= .1, 0, 5  Output:Cloud type symbol number for first reported level=" + csyt);
		assertEquals (25f, csyt);
		csyt = PRLibrary.prCsyt(9f, 3f, 7f);
		System.out.println("testPrCsyt testCaseNum=" + testCaseNum +
				"g:  Input:ctyl, ctym, ctyh= 9, 3, 7   Output:Cloud type symbol number for first reported level=" + csyt);
		assertEquals (9f, csyt);
		csyt = PRLibrary.prCsyt(9.01f, 10f, 6f);
		System.out.println("testPrCsyt testCaseNum=" + testCaseNum +
				"h:  Input:ctyl, ctym, ctyh= 9.01, 10, 6   Output:Cloud type symbol number for first reported level=" + csyt);
		assertEquals (26f, csyt);
		csyt = PRLibrary.prCsyt(GempakConstants.RMISSD, -1f, 3f);
		System.out.println("testPrCsyt testCaseNum=" + testCaseNum +
				"i:  Input:ctyl, ctym, ctyh= 4   Output:Cloud type symbol number for first reported level=" + csyt);
		assertEquals (23f, csyt);
		csyt = PRLibrary.prCsyt(0f, 10f, GempakConstants.RMISSD);
		System.out.println("testPrCsyt testCaseNum=" + testCaseNum +
				"j:  Input:ctyl, ctym, ctyh= 4   Output:Cloud type symbol number for first reported level=" + csyt);
		assertEquals (0f, csyt);
	}

	/**
	 * Test method for {@link gov.noaa.nws.ncep.gempak.parameterconversionlibrary.PRLibrary#prCtcc(float, float, float)}.
	 */
	@Test
	public void testPrCtcc() {
		testCaseNum++;
		float ctcc = PRLibrary.prCtcc(123f, 234f, 456f);
		System.out.println("testPrCsyt testCaseNum=" + testCaseNum +
				"a:  Input:chc1, chc2, chc3= 123, 234, 456   Output:Maximum cloud coverage=" + ctcc);
		assertEquals (4f, ctcc);
		ctcc = PRLibrary.prCtcc(258f, 789f, 388f);
		System.out.println("testPrCsyt testCaseNum=" + testCaseNum +
				"b:  Input:chc1, chc2, chc3= 258, 2789 388   Output:Maximum cloud coverage=" + ctcc);
		assertEquals (9f, ctcc);
		ctcc = PRLibrary.prCtcc(369f, 589f, 389f);
		System.out.println("testPrCsyt testCaseNum=" + testCaseNum +
				"c:  Input:chc1, chc2, chc3= 369, 589, 389   Output:Maximum cloud coverage=" + ctcc);
		assertEquals (9f, ctcc);
		ctcc = PRLibrary.prCtcc(355f, 353f, 637f);
		System.out.println("testPrCsyt testCaseNum=" + testCaseNum +
				"d:  Input:chc1, chc2, chc3= 355, 353, 637   Output:Maximum cloud coverage=" + ctcc);
		assertEquals (5f, ctcc);
		ctcc = PRLibrary.prCtcc(789f, 736f, 688f);
		System.out.println("testPrCsyt testCaseNum=" + testCaseNum +
				"d:  Input:chc1, chc2, chc3= 789, 736, 688   Output:Maximum cloud coverage=" + ctcc);
		assertEquals (8f, ctcc);
		ctcc = PRLibrary.prCtcc(GempakConstants.RMISSD, 736f, 688f);
		System.out.println("testPrCsyt testCaseNum=" + testCaseNum +
				"e:  Input:chc1, chc2, chc3=GempakConstants.RMISSD, 736, 688   Output:Maximum cloud coverage=" + ctcc);
		//assertEquals (8f, ctcc);
		/*888888888 Legacy has 8f but this has missing 888*/
		ctcc = PRLibrary.prCtcc(736f, GempakConstants.RMISSD, 687f);
		System.out.println("testPrCsyt testCaseNum=" + testCaseNum +
				"f:  Input:chc1, chc2, chc3=736, GempakConstants.RMISSD, 687   Output:Maximum cloud coverage=" + ctcc);
		//assertEquals (7f, ctcc);
		/*888888888 Legacy has 8f but this has missing 888*/
		ctcc = PRLibrary.prCtcc(GempakConstants.RMISSD, GempakConstants.RMISSD, GempakConstants.RMISSD);
		System.out.println("testPrCsyt testCaseNum=" + testCaseNum +
				"g:  Input:chc1, chc2, chc3=GempakConstants.RMISSD, GempakConstants.RMISSD, GempakConstants.RMISSD   Output:Maximum cloud coverage=" + ctcc);
		assertEquals (0f, ctcc);
		/*888888888 Legacy has 0f but this has missing 888*/
		
	}

	/**
	 * Test method for {@link gov.noaa.nws.ncep.gempak.parameterconversionlibrary.PRLibrary#prCtcf(float)}.
	 */
	@Test
	public void testPrCtcf() {
		testCaseNum++;
		float ctcf = PRLibrary.prCtcf(8f);
		System.out.println("testprCtcf testCaseNum=" + testCaseNum +
				"a:  Input:Numeric total cloud cover clct= 8   Output:WMO fractional cloud cover table=" + ctcf);
		assertEquals (7f, ctcf);
		ctcf = PRLibrary.prCtcf(1f);
		System.out.println("testprCtcf testCaseNum=" + testCaseNum +
				"b:  Input:Numeric total cloud cover clct= 1  Output:WMO fractional cloud cover table=" + ctcf);
		assertEquals (0f, ctcf);
		ctcf = PRLibrary.prCtcf(2f);
		System.out.println("testprCtcf testCaseNum=" + testCaseNum +
				"c:  Input:Numeric total cloud cover clct= 2  Output:WMO fractional cloud cover table=" + ctcf);
		assertEquals (3f, ctcf);
		ctcf = PRLibrary.prCtcf(5f);
		System.out.println("testprCtcf testCaseNum=" + testCaseNum +
				"d:  Input:Numeric total cloud cover clct= 5  Output:WMO fractional cloud cover table=" + ctcf);
		assertEquals (9f, ctcf);
		ctcf = PRLibrary.prCtcf(8f);
		System.out.println("testprCtcf testCaseNum=" + testCaseNum +
				"e:  Input:Numeric total cloud cover clct= 8  Output:WMO fractional cloud cover table=" + ctcf);
		assertEquals (7f, ctcf);
		ctcf = PRLibrary.prCtcf(9f);
		System.out.println("testprCtcf testCaseNum=" + testCaseNum +
				"f:  Input:Numeric total cloud cover clct= 9  Output:WMO fractional cloud cover table=" + ctcf);
		assertEquals (0f, ctcf);
		ctcf = PRLibrary.prCtcf(0f);
		System.out.println("testprCtcf testCaseNum=" + testCaseNum +
				"g:  Input:Numeric total cloud cover clct= 0  Output:WMO fractional cloud cover table=" + ctcf);
		assertEquals (GempakConstants.RMISSD, ctcf);
		ctcf = PRLibrary.prCtcf(10f);
		System.out.println("testprCtcf testCaseNum=" + testCaseNum +
				"g:  Input:Numeric total cloud cover clct= 10  Output:WMO fractional cloud cover table=" + ctcf);
		assertEquals (GempakConstants.RMISSD, ctcf);
	}

	/**
	 * Test method for {@link gov.noaa.nws.ncep.gempak.parameterconversionlibrary.PRLibrary#prD100(float)}.
	 */
	@Test
	public void testPrD100() {
		testCaseNum++;
		float d100 = PRLibrary.prD100(83468.3f);
		System.out.println("testPrD100 testCaseNum=" + testCaseNum +
				"a:  Input:A real value=83468.3    Output:value divided by 100 d100=" + d100);
		assertEquals (834.683f, d100);
		d100 = PRLibrary.prD100(-27894.23f);
		System.out.println("testPrD100 testCaseNum=" + testCaseNum +
				"b:  Input:A real value= -27894.23   Output:value divided by 100 d100=" + d100);
		assertEquals (-278.9423f, d100);
		d100 = PRLibrary.prD100(2879012f);
		System.out.println("testPrD100 testCaseNum=" + testCaseNum +
				"c:  Input:A real value= 2879012   Output:value divided by 100 d100=" + d100);
		assertEquals (28790.12f, d100);
		d100 = PRLibrary.prD100(.2879012f);
		System.out.println("testPrD100 testCaseNum=" + testCaseNum +
				"d:  Input:A real value= .2879012   Output:value divided by 100 d100=" + d100);
		assertEquals (.002879012f, d100);
		d100 = PRLibrary.prD100(GempakConstants.RMISSD);
		System.out.println("testPrD100 testCaseNum=" + testCaseNum +
				"e:  Input:A real value= GempakConstants.RMISSD   Output:value divided by 100 d100=" + d100);
		assertEquals (GempakConstants.RMISSD, d100);
	}

	/**
	 * Test method for {@link gov.noaa.nws.ncep.gempak.parameterconversionlibrary.PRLibrary#prDden(float, float)}.
	 */
	@Test
	public void testPrDden() {
		testCaseNum++;
		float dden = PRLibrary.prDden(2879f, 130f);
		System.out.println("testPrDden testCaseNum=" + testCaseNum +
				"a:  Input:presMb, tmpc= 2879. 130.   Output:Density of dry air=" + dden);
		assertEquals (2.487898f, dden);
		dden = PRLibrary.prDden(GempakConstants.RMISSD, 130f);
		System.out.println("testPrDden testCaseNum=" + testCaseNum +
				"b:  Input:presMb, tmpc=GempakConstants.RMISSD, 130.   Output:Density of dry air=" + dden);
		assertEquals (GempakConstants.RMISSD, dden);
		dden = PRLibrary.prDden(GempakConstants.RMISSD, GempakConstants.RMISSD);
		System.out.println("testPrDden testCaseNum=" + testCaseNum +
				"c:  Input:presMb, tmpc= GempakConstants.RMISSD, GempakConstants.RMISSD    Output:Density of dry air=" + dden);
		assertEquals (GempakConstants.RMISSD, dden);
		dden = PRLibrary.prDden(23881f, GempakConstants.RMISSD);
		System.out.println("testPrDden testCaseNum=" + testCaseNum +
				"d:  Input:presMb, tmpc= 23881, GempakConstants.RMISSD    Output:Density of dry air=" + dden);
		assertEquals (GempakConstants.RMISSD, dden);
		dden = PRLibrary.prDden(0, 57f);
		System.out.println("testPrDden testCaseNum=" + testCaseNum +
				"e:  Input:presMb, tmpc= 28971.1f, 0   Output:Density of dry air=" + dden);
		assertEquals (0f, dden);
		dden = PRLibrary.prDden(89000.f, -3f);
		System.out.println("testPrDden testCaseNum=" + testCaseNum +
				"f:  Input:presMb, tmpc= 28971.1f, 0   Output:Density of dry air=" + dden);
		assertEquals (114.773758f, dden);
		dden = PRLibrary.prDden(28971.1f, 0f);
		System.out.println("testPrDden testCaseNum=" + testCaseNum +
				"g:  Input:presMb, tmpc= 28971.1f, 0   Output:Density of dry air=" + dden);
		assertEquals (36.950588f, dden);
	}

	/**
	 * Test method for {@link gov.noaa.nws.ncep.gempak.parameterconversionlibrary.PRLibrary#prDdep(float, float)}.
	 */
	@Test
	public void testPrDdep() {
		testCaseNum++;
		float ddep = PRLibrary.prDdep(158f, 140f);
		System.out.println("testPrDdep testCaseNum=" + testCaseNum +
				"a:  Input:tmpx, dwpx= 158. 140.   Output:Dewpoint Depression DPDX=" + ddep);
		assertEquals (18f, ddep);
		ddep = PRLibrary.prDdep(-38f, -40f);
		System.out.println("testPrDdep testCaseNum=" + testCaseNum +
				"b:  Input:tmpx, dwpx= -38, -40.   Output:Dewpoint Depression DPDX=" + ddep);
		assertEquals (2f, ddep);
		ddep = PRLibrary.prDdep(-45f, -20f);
		System.out.println("testPrDdep testCaseNum=" + testCaseNum +
				"c:  Input:tmpx, dwpx= -45, -20.   Output:Dewpoint Depression DPDX=" + ddep);
		assertEquals (GempakConstants.RMISSD, ddep);
		ddep = PRLibrary.prDdep(GempakConstants.RMISSD, GempakConstants.RMISSD);
		System.out.println("testPrDdep testCaseNum=" + testCaseNum +
				"d:  Input:tmpx, dwpx= GempakConstants.RMISSD, GempakConstants.RMISSD.   Output:Dewpoint Depression DPDX=" + ddep);
		assertEquals (GempakConstants.RMISSD, ddep);
		ddep = PRLibrary.prDdep(78.3f, GempakConstants.RMISSD);
		System.out.println("testPrDdep testCaseNum=" + testCaseNum +
				"e:  Input:tmpx, dwpx= 78.3, GempakConstants.RMISSD.   Output:Dewpoint Depression DPDX=" + ddep);
		assertEquals (GempakConstants.RMISSD, ddep);
		ddep = PRLibrary.prDdep(78.3f, 79.52f);
		System.out.println("testPrDdep testCaseNum=" + testCaseNum +
				"f:  Input:tmpx, dwpx= 78.3, 79.52.   Output:Dewpoint Depression DPDX=" + ddep);
		assertEquals (GempakConstants.RMISSD, ddep);
		ddep = PRLibrary.prDdep(79.5f, 78.3f);
		System.out.println("testPrDdep testCaseNum=" + testCaseNum +
				"g:  Input:tmpx, dwpx= 79.5, 78.3   Output:Dewpoint Depression DPDX=" + ddep);
		assertEquals (1.2999359, ddep);
		ddep = PRLibrary.prDdep(20f, 0f);
		System.out.println("testPrDdep testCaseNum=" + testCaseNum +
				"h:  Input:tmpx, dwpx= 20. 0.   Output:Dewpoint Depression DPDX=" + ddep);
		assertEquals (20f, ddep);
	}

	/**
	 * Test method for {@link gov.noaa.nws.ncep.gempak.parameterconversionlibrary.PRLibrary#prDmax(float, float, float)}.
	 */
	@Test
	public void testPrDmax() {
		testCaseNum++;
		float dmax = PRLibrary.prDmax(158f, 140f, 130f);
		System.out.println("testPrDmax testCaseNum=" + testCaseNum +
				"a:  Input:t00x t06x tdxc in Celsius= 158. 140.   Output:Max temp of the 3 in Fahrenheit=" + dmax);
		assertEquals (316.399994f, dmax);
		dmax = PRLibrary.prDmax(78f, 73f, 68f);
		System.out.println("testPrDmax testCaseNum=" + testCaseNum +
				"b:  Input:t00x t06x tdxc in Celsius= 78 73 68   Output:Max temp of the 3 in Fahrenheit=" + dmax);
		assertEquals (172.399994f, dmax);
		dmax = PRLibrary.prDmax(GempakConstants.RMISSD, 73f, 68f);
		System.out.println("testPrDmax testCaseNum=" + testCaseNum +
				"c:  Input:t00x t06x tdxc in Celsius= GempakConstants.RMISSD 73 68   Output:Max temp of the 3 in Fahrenheit=" + dmax);
		assertEquals (GempakConstants.RMISSD, dmax);
		dmax = PRLibrary.prDmax(208f, GempakConstants.RMISSD, 68f);
		System.out.println("testPrDmax testCaseNum=" + testCaseNum +
				"d:  Input:t00x t06x tdxc in Celsius= 208 GempakConstants.RMISSD 68   Output:Max temp of the 3 in Fahrenheit=" + dmax);
		assertEquals (GempakConstants.RMISSD, dmax);
		dmax = PRLibrary.prDmax(208f, 231f, GempakConstants.RMISSD);
		System.out.println("testPrDmax testCaseNum=" + testCaseNum +
				"e:  Input:t00x t06x tdxc in Celsius= 208 231 GempakConstants.RMISSD   Output:Max temp of the 3 in Fahrenheit=" + dmax);
		assertEquals (447.799988f, dmax);
		dmax = PRLibrary.prDmax(40f, 55f, 30f);
		System.out.println("testPrDmax testCaseNum=" + testCaseNum +
				"f:  Input:t00x t06x tdxc in Celsius= 40 55 30   Output:Max temp of the 3 in Fahrenheit=" + dmax);
		assertEquals (131f, dmax);
		dmax = PRLibrary.prDmax(-30f, -6f, -10f);
		System.out.println("testPrDmax testCaseNum=" + testCaseNum +
				"g:  Inh:  Input:t00x t06x tdxc in Celsius= -30 0 -.1   Output:Max temp of the 3 in Fahrenheit=" + dmax);
		assertEquals (21.2f, dmax);
		dmax = PRLibrary.prDmax(20f, 19f, 22f);
		System.out.println("testPrDmax testCaseNum=" + testCaseNum +
				"i:  Input:t00x t06x tdxc in Celsius= 20 19 22   Output:Max temp of the 3 in Fahrenheit=" + dmax);
		assertEquals (71.6f, dmax);
		dmax = PRLibrary.prDmax(0f, -.1f, -2f);
		System.out.println("testPrDmax testCaseNum=" + testCaseNum +
				"j:  Input:t00x t06x tdxc in Celsius= 0 -.1 -2   Output:Max temp of the 3 in Fahrenheit=" + dmax);
		assertEquals (32f, dmax);
	}

	/**
	 * Test method for {@link gov.noaa.nws.ncep.gempak.parameterconversionlibrary.PRLibrary#prTmcf(float)}.
	 */
	@Test
	public void testPrTmcf() {
		testCaseNum++;
		float tmcf = PRLibrary.prTmcf(0f);
		System.out.println("testPrTmfc testCaseNum=" + testCaseNum +
				"a:  Input:Temperature in Celsius= 0   Output:Temperature in Fahrenheit=" + tmcf);
		assertEquals (32f, tmcf);
		tmcf = PRLibrary.prTmcf(100f);
		System.out.println("testPrTmfc testCaseNum=" + testCaseNum +
				"b:  Input:Temperature in Celsius= 100   Output:Temperature in Fahrenheit=" + tmcf);
		assertEquals (212f, tmcf);
		tmcf = PRLibrary.prTmcf(-10f);
		System.out.println("testPrTmfc testCaseNum=" + testCaseNum +
				"c:  Input:Temperature in Celsius= -10   Output:Temperature in Fahrenheit=" + tmcf);
		assertEquals (14f, tmcf);
		tmcf = PRLibrary.prTmcf(20f);
		System.out.println("testPrTmfc testCaseNum=" + testCaseNum +
				"d:  Input:Temperature in Celsius= 20   Output:Temperature in Fahrenheit=" + tmcf);
		assertEquals (68f, tmcf);
		tmcf = PRLibrary.prTmcf(GempakConstants.RMISSD);
		System.out.println("testPrTmfc testCaseNum=" + testCaseNum +
				"e:  Input:Temperature in Celsius= GempakConstants.RMISSD   Output:Temperature in Fahrenheit=" + tmcf);
		assertEquals (GempakConstants.RMISSD, tmcf);
		tmcf = PRLibrary.prTmcf(130f);
		System.out.println("testPrTmfc testCaseNum=" + testCaseNum +
				"f:  Input:Temperature in Celsius= 130   Output:Temperature in Fahrenheit=" + tmcf);
		assertEquals (266f, tmcf);
		tmcf = PRLibrary.prTmcf(200f);
		System.out.println("testPrTmfc testCaseNum=" + testCaseNum +
				"g:  Input:Temperature in Celsius= 200   Output:Temperature in Fahrenheit=" + tmcf);
		assertEquals (392f, tmcf);
		tmcf = PRLibrary.prTmcf(-50f);
		System.out.println("testPrTmfc testCaseNum=" + testCaseNum +
				"h:  Input:Temperature in Celsius= -50   Output:Temperature in Fahrenheit=" + tmcf);
		assertEquals (-57.999996f, tmcf, .00001);
	}

	/**
	 * Test method for {@link gov.noaa.nws.ncep.gempak.parameterconversionlibrary.PRLibrary#prTmfc(float)}.
	 */
	@Test
	public void testPrTmfc() {
		testCaseNum++;
		float tmfc = PRLibrary.prTmfc(0f);
		System.out.println("testPrTmfc testCaseNum=" + testCaseNum +
				"a:  Input:Temperature in Fahrenheit= 0   Output:Temperature in Celsius=" + tmfc);
		assertEquals (-17.777779f, tmfc);
		tmfc = PRLibrary.prTmfc(32f);
		System.out.println("testPrTmfc testCaseNum=" + testCaseNum +
				"b:  Input:Temperature in Fahrenheit= 32   Output:Temperature in Celsius=" + tmfc);
		assertEquals (0f, tmfc);
		tmfc = PRLibrary.prTmfc(212f);
		System.out.println("testPrTmfc testCaseNum=" + testCaseNum +
				"c:  Input:Temperature in Fahrenheit= 212   Output:Temperature in Celsius=" + tmfc);
		assertEquals (100f, tmfc);
		tmfc = PRLibrary.prTmfc(85f);
		System.out.println("testPrTmfc testCaseNum=" + testCaseNum +
				"d:  Input:Temperature in Fahrenheit= 85   Output:Temperature in Celsius=" + tmfc);
		assertEquals (29.44445f, tmfc);
		tmfc = PRLibrary.prTmfc(GempakConstants.RMISSD);
		System.out.println("testPrTmfc testCaseNum=" + testCaseNum +
				"e:  Input:Temperature in Fahrenheit= GempakConstants.RMISSD   Output:Temperature in Celsius=" + tmfc);
		assertEquals (GempakConstants.RMISSD, tmfc);
		tmfc = PRLibrary.prTmfc(105f);
		System.out.println("testPrTmfc testCaseNum=" + testCaseNum +
				"f:  Input:Temperature in Fahrenheit= 105   Output:Temperature in Celsius=" + tmfc);
		assertEquals (40.55556f, tmfc);
		tmfc = PRLibrary.prTmfc(250f);
		System.out.println("testPrTmfc testCaseNum=" + testCaseNum +
				"g  Input:Temperature in Fahrenheit= 250   Output:Temperature in Celsius=" + tmfc);
		assertEquals (121.11112f, tmfc);
	}

	/**
	 * Test method for {@link gov.noaa.nws.ncep.gempak.parameterconversionlibrary.PRLibrary#prTmfk(float)}.
	 */
	@Test
	public void testPrTmfk() {
		testCaseNum++;
		float tmfk = PRLibrary.prTmfk(0f);
		System.out.println("testPrTmfk testCaseNum=" + testCaseNum +
				"a:  Input:Temperature in Fahrenheit= 0   Output:Temperature in Kelvin=" + tmfk);
		assertEquals (255.37222f, tmfk);
		tmfk = PRLibrary.prTmfk(212f);
		System.out.println("testPrTmfk testCaseNum=" + testCaseNum +
				"b:  Input:Temperature in Fahrenheit= 212   Output:Temperature in Kelvin=" + tmfk);
		assertEquals (373.14999f, tmfk);
		tmfk = PRLibrary.prTmfk(GempakConstants.RMISSD);
		System.out.println("testPrTmfk testCaseNum=" + testCaseNum +
				"c:  Input:Temperature in Fahrenheit= GempakConstants.RMISSD   Output:Temperature in Kelvin=" + tmfk);
		assertEquals (GempakConstants.RMISSD, tmfk);
		tmfk = PRLibrary.prTmfk(-25f);
		System.out.println("testPrTmfk testCaseNum=" + testCaseNum +
				"d:  Input:Temperature in Fahrenheit= -25   Output:Temperature in Kelvin=" + tmfk);
		assertEquals (241.4833f, tmfk);
		tmfk = PRLibrary.prTmfk(180f);
		System.out.println("testPrTmfk testCaseNum=" + testCaseNum +
				"e:  Input:Temperature in Fahrenheit= 180   Output:Temperature in Kelvin=" + tmfk);
		assertEquals (355.37222f, tmfk);
		tmfk = PRLibrary.prTmfk(32f);
		System.out.println("testPrTmfk testCaseNum=" + testCaseNum +
				"f:  Input:Temperature in Fahrenheit= 32   Output:Temperature in Kelvin=" + tmfk);
		assertEquals (273.14999f, tmfk);
		tmfk = PRLibrary.prTmfk(250f);
		System.out.println("testPrTmfk testCaseNum=" + testCaseNum +
				"g:  Input:Temperature in Fahrenheit= 250   Output:Temperature in Kelvin=" + tmfk);
		assertEquals (394.2611f, tmfk);
	}

	/**
	 * Test method for {@link gov.noaa.nws.ncep.gempak.parameterconversionlibrary.PRLibrary#prTmkf(float)}.
	 */
	@Test
	public void testPrTmkf() {
		testCaseNum++;
		float tmkf = PRLibrary.prTmkf(0f);
		System.out.println("testPrTmkf testCaseNum=" + testCaseNum +
				"a:  Input:Temperature in Kelvin= 0   Output:Temperature in Fahrenheit=" + tmkf);
		assertEquals (-459.66998f, tmkf);
		tmkf = PRLibrary.prTmkf(273.15f);
		System.out.println("testPrTmkf testCaseNum=" + testCaseNum +
				"b:  Input:Temperature in Kelvin= 273.15   Output:Temperature in Fahrenheit=" + tmkf);
		assertEquals (32f, tmkf);
		tmkf = PRLibrary.prTmkf(GempakConstants.RMISSD);
		System.out.println("testPrTmkf testCaseNum=" + testCaseNum +
				"c:  Input:Temperature in Kelvin= GempakConstants.RMISSD   Output:Temperature in Fahrenheit=" + tmkf);
		assertEquals (GempakConstants.RMISSD, tmkf);
		tmkf = PRLibrary.prTmkf(250f);
		System.out.println("testPrTmkf testCaseNum=" + testCaseNum +
				"d:  Input:Temperature in Kelvin= 250   Output:Temperature in Fahrenheit=" + tmkf);
		assertEquals (-9.67f, tmkf);
		tmkf = PRLibrary.prTmkf(400f);
		System.out.println("testPrTmkf testCaseNum=" + testCaseNum +
				"e:  Input:Temperature in Kelvin= 400   Output:Temperature in Fahrenheit=" + tmkf);
		assertEquals (260.33f, tmkf);
		tmkf = PRLibrary.prTmkf(-20f);
		System.out.println("testPrTmkf testCaseNum=" + testCaseNum +
				"f:  Input:Temperature in Kelvin= -20   Output:Temperature in Fahrenheit=" + tmkf);
		assertEquals (-495.67f, tmkf);

	}

	/**
	 * Test method for {@link gov.noaa.nws.ncep.gempak.parameterconversionlibrary.PRLibrary#prTmck(float)}.
	 */
	@Test
	public void testPrTmck() {
		testCaseNum++;
		float tmck = PRLibrary.prTmck(0f);
		System.out.println("testPrTmck testCaseNum=" + testCaseNum +
				"a:  Input:Temperature in Celsius= 0   Output:Temperature in Kelvin=" + tmck);
		assertEquals (273.149994f, tmck);
		tmck = PRLibrary.prTmck(100f);
		System.out.println("testPrTmck testCaseNum=" + testCaseNum +
				"b:  Input:Temperature in Celsius= 100   Output:Temperature in Kelvin=" + tmck);
		assertEquals (373.149994f, tmck);
		tmck = PRLibrary.prTmck(GempakConstants.RMISSD);
		System.out.println("testPrTmck testCaseNum=" + testCaseNum +
				"c:  Input:Temperature in Celsius= GempakConstants.RMISSD   Output:Temperature in Kelvin=" + tmck);
		assertEquals (GempakConstants.RMISSD, tmck);
		tmck = PRLibrary.prTmck(-50f);
		System.out.println("testPrTmck testCaseNum=" + testCaseNum +
				"d:  Input:Temperature in Celsius= -50  Output:Temperature in Kelvin=" + tmck);
		assertEquals (223.149994, tmck);
		tmck = PRLibrary.prTmck(-250f);
		System.out.println("testPrTmck testCaseNum=" + testCaseNum +
				"e:  Input:Temperature in Celsius= -250  Output:Temperature in Kelvin=" + tmck);
		assertEquals (23.14999f, tmck);
		tmck = PRLibrary.prTmck(250f);
		System.out.println("testPrTmck testCaseNum=" + testCaseNum +
				"f:  Input:Temperature in Celsius= 250  Output:Temperature in Kelvin=" + tmck);
		assertEquals (523.150024f, tmck);
		tmck = PRLibrary.prTmck(273.15f);
		System.out.println("testPrTmck testCaseNum=" + testCaseNum +
				"g:  Input:Temperature in Celsius= 273.15  Output:Temperature in Kelvin=" + tmck);
		assertEquals (546.30f, tmck);
		tmck = PRLibrary.prTmck(-273.15f);
		System.out.println("testPrTmck testCaseNum=" + testCaseNum +
				"h:  Input:Temperature in Celsius= -273.15  Output:Temperature in Kelvin=" + tmck);
		assertEquals (0f, tmck);
	}

	/**
	 * Test method for {@link gov.noaa.nws.ncep.gempak.parameterconversionlibrary.PRLibrary#prTmkc(float)}.
	 */
	@Test
	public void testPrTmkc() {
		testCaseNum++;
		float tmkc = PRLibrary.prTmkc(0f);
		System.out.println("testPrTmkc testCaseNum=" + testCaseNum +
				"a:  Input:Temperature in Kelvin= 0   Output:Temperature in Celsius=" + tmkc);
		assertEquals (-273.149994f, tmkc);
		tmkc = PRLibrary.prTmkc(100f);
		System.out.println("testPrTmkc testCaseNum=" + testCaseNum +
				"b:  Input:Temperature in Kelvin= 100   Output:Temperature in Celsius=" + tmkc);
		assertEquals (-173.149994f, tmkc);
		tmkc = PRLibrary.prTmkc(500f);
		System.out.println("testPrTmkc testCaseNum=" + testCaseNum +
				"c:  Input:Temperature in Kelvin= 500   Output:Temperature in Celsius=" + tmkc);
		assertEquals (226.85f, tmkc);
		tmkc = PRLibrary.prTmkc(273.15f);
		System.out.println("testPrTmkc testCaseNum=" + testCaseNum +
				"d:  Input:Temperature in Kelvin= 273.15   Output:Temperature in Celsius=" + tmkc);
		assertEquals (0f, tmkc);
		tmkc = PRLibrary.prTmkc(GempakConstants.RMISSD);
		System.out.println("testPrTmkc testCaseNum=" + testCaseNum +
				"e:  Input:Temperature in Kelvin= GempakConstants.RMISSD   Output:Temperature in Celsius=" + tmkc);
		assertEquals (GempakConstants.RMISSD, tmkc);
	}

	/**
	 * Test method for {@link gov.noaa.nws.ncep.gempak.parameterconversionlibrary.PRLibrary#prDmin(float, float)}.
	 */
	@Test
	public void testPrDmin() {
		testCaseNum++;
		float dmin = PRLibrary.prDmin(158f, 140f);
		System.out.println("testPrDmin testCaseNum=" + testCaseNum +
				"a:  Input:t12n, t18n in Celsius= 158. 140.   Output:Min temp of the 2 in Fahrenheit=" + dmin);
		assertEquals (284f, dmin);
		dmin = PRLibrary.prDmin(0f, -3f);
		System.out.println("testPrDmin testCaseNum=" + testCaseNum +
				"b:  Input:t12n, t18n in Celsius= 0 -3   Output:Min temp of the 2 in Fahrenheit=" + dmin);
		assertEquals (26.6f, dmin);
		dmin = PRLibrary.prDmin(0f, 2f);
		System.out.println("testPrDmin testCaseNum=" + testCaseNum +
				"c:  Input:t12n, t18n in Celsius= 0 2   Output:Min temp of the 2 in Fahrenheit=" + dmin);
		assertEquals (32f, dmin);
		dmin = PRLibrary.prDmin(100f, 102f);
		System.out.println("testPrDmin testCaseNum=" + testCaseNum +
				"d:  Input:t12n, t18n in Celsius= 100 102   Output:Min temp of the 2 in Fahrenheit=" + dmin);
		assertEquals (212f, dmin);
		dmin = PRLibrary.prDmin(GempakConstants.RMISSD, GempakConstants.RMISSD);
		System.out.println("testPrDmin testCaseNum=" + testCaseNum +
				"e:  Input:t12n, t18n in Celsius= GempakConstants.RMISSD GempakConstants.RMISSD   Output:Min temp of the 2 in Fahrenheit=" + dmin);
		assertEquals (GempakConstants.RMISSD, dmin);
		dmin = PRLibrary.prDmin(GempakConstants.RMISSD, 0f);
		System.out.println("testPrDmin testCaseNum=" + testCaseNum +
				"f:  Input:t12n, t18n in Celsius= GempakConstants.RMISSD 0   Output:Min temp of the 2 in Fahrenheit=" + dmin);
		assertEquals (GempakConstants.RMISSD, dmin);
		dmin = PRLibrary.prDmin(100f, GempakConstants.RMISSD);
		System.out.println("testPrDmin testCaseNum=" + testCaseNum +
				"g:  Input:t12n, t18n in Celsius= 100. GempakConstants.RMISSD   Output:Min temp of the 2 in Fahrenheit=" + dmin);
		assertEquals (GempakConstants.RMISSD, dmin);
	}

	/**
	 * Test method for {@link gov.noaa.nws.ncep.gempak.parameterconversionlibrary.PRLibrary#prDrct(float, float)}.
	 */
	@Test
	public void testPrDrct() {
		testCaseNum++;
		float drct = PRLibrary.prDrct(35f, 40f);
		System.out.println("testPrDrct testCaseNum=" + testCaseNum +
				"a:  Input:Velocity components in same units ux, vx= 35, 40" +
				 " Output:Wind direction in degrees=" + drct);
		assertEquals (221.1859f, drct);
		drct = PRLibrary.prDrct(0f, 0f);
		System.out.println("testPrDrct testCaseNum=" + testCaseNum +
				"b:  Input:Velocity components in same units ux, vx= 0, 0" +
				 " Output:Wind direction in degrees=" + drct);
		assertEquals (0f, drct);
		drct = PRLibrary.prDrct(0f, 50f);
		System.out.println("testPrDrct testCaseNum=" + testCaseNum +
				"c:  Input:Velocity components in same units ux, vx= 0, 50" +
				 " Output:Wind direction in degrees=" + drct);
		assertEquals (180f, drct);
		drct = PRLibrary.prDrct(0f, 100f);
		System.out.println("testPrDrct testCaseNum=" + testCaseNum +
				"d:  Input:Velocity components in same units ux, vx= 0, 100" +
				 " Output:Wind direction in degrees=" + drct);
		assertEquals (180f, drct);
		drct = PRLibrary.prDrct(0f, -100f);
		System.out.println("testPrDrct testCaseNum=" + testCaseNum +
				"e:  Input:Velocity components in same units ux, vx= 0, -100" +
				 " Output:Wind direction in degrees=" + drct);
		assertEquals (360f, drct);
		drct = PRLibrary.prDrct(-1f, 0f);
		System.out.println("testPrDrct testCaseNum=" + testCaseNum +
				"f:  Input:Velocity components in same units ux, vx= -1, 0" +
				 " Output:Wind direction in degrees=" + drct);
		assertEquals (90f, drct);
		drct = PRLibrary.prDrct(1f, 0f);
		System.out.println("testPrDrct testCaseNum=" + testCaseNum +
				"c:  Input:Velocity components in same units ux, vx= 1, 0" +
				 " Output:Wind direction in degrees=" + drct);
		assertEquals (270f, drct);
		drct = PRLibrary.prDrct(1f, 1f);
		System.out.println("testPrDrct testCaseNum=" + testCaseNum +
				"d:  Input:Velocity components in same units ux, vx= 1, 1" +
				 " Output:Wind direction in degrees=" + drct);
		assertEquals (225f, drct);
		drct = PRLibrary.prDrct(-1f, 1f);
		System.out.println("testPrDrct testCaseNum=" + testCaseNum +
				"e:  Input:Velocity components in same units ux, vx= -1, 1" +
				 " Output:Wind direction in degrees=" + drct);
		assertEquals (135f, drct);
		drct = PRLibrary.prDrct(10f, -10f);
		System.out.println("testPrDrct testCaseNum=" + testCaseNum +
				"f:  Input:Velocity components in same units ux, vx= 10, -10" +
				 " Output:Wind direction in degrees=" + drct);
		assertEquals (315f, drct);
		drct = PRLibrary.prDrct(GempakConstants.RMISSD, GempakConstants.RMISSD);
		System.out.println("testPrDrct testCaseNum=" + testCaseNum +
				"g:  Input:Velocity components in same units ux, vx= GempakConstants.RMISSD GempakConstants.RMISSD" +
				 " Output:Wind direction in degrees=" + drct);
		assertEquals (GempakConstants.RMISSD, drct);
		drct = PRLibrary.prDrct(1f, GempakConstants.RMISSD);
		System.out.println("testPrDrct testCaseNum=" + testCaseNum +
				"h:  Input:Velocity components in same units ux, vx= 1f GempakConstants.RMISSD" +
				 " Output:Wind direction in degrees=" + drct);
		assertEquals (GempakConstants.RMISSD, drct);
		drct = PRLibrary.prDrct(GempakConstants.RMISSD, 1f);
		System.out.println("testPrDrct testCaseNum=" + testCaseNum +
				"i:  Input:Velocity components in same units ux, vx= GempakConstants.RMISSD 1f" +
				 " Output:Wind direction in degrees=" + drct);
		assertEquals (GempakConstants.RMISSD, drct);
	}

	/**
	 * Test method for {@link gov.noaa.nws.ncep.gempak.parameterconversionlibrary.PRLibrary#prDwdp(float, float)}.
	 */
	@Test
	public void testPrDwdp() {
		testCaseNum++;
		float dwdp = PRLibrary.prDwdp(158f, 140f);
		System.out.println("testPrDwdp testCaseNum=" + testCaseNum +
				"a:  Input:tmpx, dpdx= 158. 140.   Output:Dewpoint dwdp=" + dwdp);
		assertEquals (18f, dwdp);
		dwdp = PRLibrary.prDwdp(23f, 21f);
		System.out.println("testPrDwdp testCaseNum=" + testCaseNum +
				"b:  Input:tmpx, dpdx= 23. 21.   Output:Dewpoint dwdp=" + dwdp);
		assertEquals (2f, dwdp);
		dwdp = PRLibrary.prDwdp(50.3f, 31.1f);
		System.out.println("testPrDwdp testCaseNum=" + testCaseNum +
				"c:  Input:tmpx, dpdx= 50.3 31.1   Output:Dewpoint dwdp=" + dwdp);
		assertEquals (19.1999989, dwdp);
		dwdp = PRLibrary.prDwdp(GempakConstants.RMISSD, GempakConstants.RMISSD);
		System.out.println("testPrDwdp testCaseNum=" + testCaseNum +
				"d:  Input:tmpx, dpdx=GempakConstants.RMISSD, GempakConstants.RMISSD   Output:Dewpoint dwdp=" + dwdp);
		assertEquals (GempakConstants.RMISSD, dwdp);
		dwdp = PRLibrary.prDwdp(GempakConstants.RMISSD, 34f);
		System.out.println("testPrDwdp testCaseNum=" + testCaseNum +
				"e:  Input:tmpx, dpdx=GempakConstants.RMISSD, 35.   Output:Dewpoint dwdp=" + dwdp);
		assertEquals (GempakConstants.RMISSD, dwdp);
		dwdp = PRLibrary.prDwdp(-23f, -21f);
		System.out.println("testPrDwdp testCaseNum=" + testCaseNum +
				"f:  Input:tmpx, dpdx= -23. -21   Output:Dewpoint dwdp=" + dwdp);
		assertEquals (-2f, dwdp);
	}

	/**
	 * Test method for {@link gov.noaa.nws.ncep.gempak.parameterconversionlibrary.PRLibrary#prDwpt(float, float)}.
	 */
	@Test
	public void testPrDwpt() {
		testCaseNum++;
		float dwpt = PRLibrary.prDwpt(80f, 930f);
		System.out.println("testPrDwpt testCaseNum=" + testCaseNum +
				"a:  Input:rmix, pres= 158. 140.   Output:Dewpoint dwpt=" + dwpt);
		assertEquals (46.8061447f, dwpt);
		dwpt = PRLibrary.prDwpt(0f, 950f);
		System.out.println("testPrDwpt testCaseNum=" + testCaseNum +
				"b:  Input:rmix, pres= 0. 950.   Output:Dewpoint dwpt=" + dwpt);
		assertEquals (GempakConstants.RMISSD, dwpt);
		dwpt = PRLibrary.prDwpt(1f, 928f);
		System.out.println("testPrDwpt testCaseNum=" + testCaseNum +
				"c:  Input:rmix, pres= 1. 928.   Output:Dewpoint dwpt=" + dwpt);
		assertEquals (-18.0634995, dwpt);
		dwpt = PRLibrary.prDwpt(100f, 1050f);
		System.out.println("testPrDwpt testCaseNum=" + testCaseNum +
				"d:  Input:rmix, pres= 1. 928.   Output:Dewpoint dwpt=" + dwpt);
		assertEquals (53.1297989, dwpt);
		dwpt = PRLibrary.prDwpt(1000f, 1050f);
		System.out.println("testPrDwpt testCaseNum=" + testCaseNum +
				"e:  Input:rmix, pres= 1. 928.   Output:Dewpoint dwpt=" + dwpt);
		assertEquals (87.1692734, dwpt);
		dwpt = PRLibrary.prDwpt(1000f, 920f);
		System.out.println("testPrDwpt testCaseNum=" + testCaseNum +
				"e:  Input:rmix, pres= 1000 920   Output:Dewpoint dwpt=" + dwpt);
		assertEquals (83.8563766, dwpt);
		dwpt = PRLibrary.prDwpt(GempakConstants.RMISSD, 930f);
		System.out.println("testPrDwpt testCaseNum=" + testCaseNum +
				"e:  Input:rmix, pres= GempakConstants.RMISSD 140.   Output:Dewpoint dwpt=" + dwpt);
		assertEquals (GempakConstants.RMISSD, dwpt);
		dwpt = PRLibrary.prDwpt(GempakConstants.RMISSD, GempakConstants.RMISSD);
		System.out.println("testPrDwpt testCaseNum=" + testCaseNum +
				"a:  Input:rmix, pres= GempakConstants.RMISSD GempakConstants.RMISSD.   Output:Dewpoint dwpt=" + dwpt);
		assertEquals (GempakConstants.RMISSD, dwpt);
		dwpt = PRLibrary.prDwpt(60f, GempakConstants.RMISSD);
		System.out.println("testPrDwpt testCaseNum=" + testCaseNum +
				"a:  Input:rmix, pres= 60f GempakConstants.RMISSD   Output:Dewpoint dwpt=" + dwpt);
		assertEquals (GempakConstants.RMISSD, dwpt);
	}

	/**
	 * Test method for {@link gov.noaa.nws.ncep.gempak.parameterconversionlibrary.PRLibrary#prFosb(float, float, float)}.
	 */
	@Test
	public void testPrFosb() {
		testCaseNum++;
		float fosb = PRLibrary.prFosb(70f, 65f, 50f);
		System.out.println("testPrFosb testCaseNum=" + testCaseNum +
				"a:  Input:tmpc, relh, sped= 70 65 50   Output:Fosberg Index=" + fosb);
		assertEquals (185.56917f, fosb);
		fosb = PRLibrary.prFosb(75f, 50f, 25f);
		System.out.println("testPrFosb testCaseNum=" + testCaseNum +
				"b:  Input:tmpc, relh, sped= 70 50 25   Output:Fosberg Index=" + fosb);
		assertEquals (106.909836f, fosb);
		fosb = PRLibrary.prFosb(65f, 10f, 15f);
		System.out.println("testPrFosb testCaseNum=" + testCaseNum +
				"c:  Input:tmpc, relh, sped= 65 10 15   Output:Fosberg Index=" + fosb);
		assertEquals (97.6963425f, fosb);
		fosb = PRLibrary.prFosb(60f, 10f, 17f);
		System.out.println("testPrFosb testCaseNum=" + testCaseNum +
				"d:  Input:tmpc, relh, sped= 65 10 15   Output:Fosberg Index=" + fosb);
		assertEquals (110.315086f, fosb);
		fosb = PRLibrary.prFosb(60f, 9f, 8.94f);
		System.out.println("testPrFosb testCaseNum=" + testCaseNum +
				"e:  Input:tmpc, relh, sped= 60 9 8.94   Output:Fosberg Index=" + fosb);
		assertEquals (58.8714027f, fosb);
		fosb = PRLibrary.prFosb(12f, 8f, 23f);
		System.out.println("testPrFosb testCaseNum=" + testCaseNum +
				"f:  Input:tmpc, relh, sped= 12 8 23   Output:Fosberg Index=" + fosb);
		assertEquals (149.234543f, fosb);
		fosb = PRLibrary.prFosb(12f, 10f, 0f);
		System.out.println("testPrFosb testCaseNum=" + testCaseNum +
				"g:  Input:tmpc, relh, sped= 12 10 0   Output:Fosberg Index=" + fosb);
		assertEquals (2.80317616f, fosb);
		fosb = PRLibrary.prFosb(12f, 10f, 1f);
		System.out.println("testPrFosb testCaseNum=" + testCaseNum +
				"h:  Input:tmpc, relh, sped= 12 10 0   Output:Fosberg Index=" + fosb);
		assertEquals (6.86461592f, fosb);
		fosb = PRLibrary.prFosb(GempakConstants.RMISSD, 60f, 18f);
		System.out.println("testPrFosb testCaseNum=" + testCaseNum +
				"i:  Input:tmpc, relh, sped= GempakConstants.RMISSD 60 18   Output:Fosberg Index=" + fosb);
		assertEquals (GempakConstants.RMISSD, fosb);
		fosb = PRLibrary.prFosb(GempakConstants.RMISSD, GempakConstants.RMISSD, GempakConstants.RMISSD);
		System.out.println("testPrFosb testCaseNum=" + testCaseNum +
				"j:  Input:tmpc, relh, sped= GempakConstants.RMISSD GempakConstants.RMISSD GempakConstants.RMISSD   Output:Fosberg Index=" + fosb);
		assertEquals (GempakConstants.RMISSD, fosb);
		fosb = PRLibrary.prFosb(29f, GempakConstants.RMISSD, GempakConstants.RMISSD);
		System.out.println("testPrFosb testCaseNum=" + testCaseNum +
				"k:  Input:tmpc, relh, sped= 29 GempakConstants.RMISSD GempakConstants.RMISSD   Output:Fosberg Index=" + fosb);
		assertEquals (GempakConstants.RMISSD, fosb);
		fosb = PRLibrary.prFosb(29f, GempakConstants.RMISSD, 23f);
		System.out.println("testPrFosb testCaseNum=" + testCaseNum +
				"l:  Input:tmpc, relh, sped= 29 GempakConstants.RMISSD 23   Output:Fosberg Index=" + fosb);
		assertEquals (GempakConstants.RMISSD, fosb);
		fosb = PRLibrary.prFosb(29f, 90f, GempakConstants.RMISSD);
		System.out.println("testPrFosb testCaseNum=" + testCaseNum +
				"m:  Input:tmpc, relh, sped= 29 90 GempakConstants.RMISSD   Output:Fosberg Index=" + fosb);
		assertEquals (GempakConstants.RMISSD, fosb);
		fosb = PRLibrary.prFosb(29f, 100f, 18f);
		System.out.println("testPrFosb testCaseNum=" + testCaseNum +
				"n:  Input:tmpc, relh, sped= 29 100 18   Output:Fosberg Index=" + fosb);
		assertEquals (10.4163122f, fosb);		
	}

	/**
	 * Test method for {@link gov.noaa.nws.ncep.gempak.parameterconversionlibrary.PRLibrary#prMskn(float)}.
	 */
	@Test
	public void testPrMskn() {
		testCaseNum++;
		float sknt = PRLibrary.prMskn(100f);
		System.out.println("testPrMskn testCaseNum=" + testCaseNum +
				"a:  Input:wind speed in m/s sped= 100.  Output:wind speed in knots=" + sknt);
		assertEquals (194.25f, sknt);
		sknt = PRLibrary.prMskn(175f);
		System.out.println("testPrMskn testCaseNum=" + testCaseNum +
				"b:  Input:wind speed in m/s sped= 175.  Output:wind speed in knots=" + sknt);
		assertEquals (339.9375f, sknt);
		sknt = PRLibrary.prMskn(10f);
		System.out.println("testPrMskn testCaseNum=" + testCaseNum +
				"c:  Input:wind speed in m/s sped= 10.  Output:wind speed in knots=" + sknt);
		assertEquals (19.4249992f, sknt);
		sknt = PRLibrary.prMskn(55f);
		System.out.println("testPrMskn testCaseNum=" + testCaseNum +
				"d:  Input:wind speed in m/s sped= 55.  Output:wind speed in knots=" + sknt);
		assertEquals (106.837502f, sknt);
		sknt = PRLibrary.prMskn(0f);
		System.out.println("testPrMskn testCaseNum=" + testCaseNum +
				"e:  Input:wind speed in m/s sped= 0.  Output:wind speed in knots=" + sknt);
		assertEquals (0f, sknt);
		sknt = PRLibrary.prMskn(23f);
		System.out.println("testPrMskn testCaseNum=" + testCaseNum +
				"f:  Input:wind speed in m/s sped= 23.  Output:wind speed in knots=" + sknt);
		assertEquals (44.6775017f, sknt);
		sknt = PRLibrary.prMskn(122f);
		System.out.println("testPrMskn testCaseNum=" + testCaseNum +
				"g:  Input:wind speed in m/s sped= 122.  Output:wind speed in knots=" + sknt);
		assertEquals (236.985001f, sknt);
		sknt = PRLibrary.prMskn(GempakConstants.RMISSD);
		System.out.println("testPrMskn testCaseNum=" + testCaseNum +
				"h:  Input:wind speed in m/s sped= GempakConstants.RMISSD  Output:wind speed in knots=" + sknt);
		assertEquals (GempakConstants.RMISSD, sknt);
	}

	/**
	 * Test method for {@link gov.noaa.nws.ncep.gempak.parameterconversionlibrary.PRLibrary#prKnms(float)}.
	 */
	@Test
	public void testPrKnms() {
		testCaseNum++;
		float sped = PRLibrary.prKnms(100f);
		System.out.println("testPrKnms testCaseNum=" + testCaseNum +
				"a:  Input:wind speed in knots= 100.  Output:wind speed in meters/sec=" + sped);
		assertEquals (51.4800529f, sped);
		sped = PRLibrary.prKnms(290f);
		System.out.println("testPrKnms testCaseNum=" + testCaseNum +
				"b:  Input:wind speed in knots= 290.  Output:wind speed in meters/sec=" + sped);
		assertEquals (149.292145f, sped);
		sped = PRLibrary.prKnms(0f);
		System.out.println("testPrKnms testCaseNum=" + testCaseNum +
				"c:  Input:wind speed in knots= 0.  Output:wind speed in meters/sec=" + sped);
		assertEquals (0f, sped);
		sped = PRLibrary.prKnms(10f);
		System.out.println("testPrKnms testCaseNum=" + testCaseNum +
				"d:  Input:wind speed in knots= 10.  Output:wind speed in meters/sec=" + sped);
		assertEquals (5.14800501f, sped);
		sped = PRLibrary.prKnms(395f);
		System.out.println("testPrKnms testCaseNum=" + testCaseNum +
				"e:  Input:wind speed in knots= 395.  Output:wind speed in meters/sec=" + sped);
		assertEquals (203.346207f, sped);
		sped = PRLibrary.prKnms(GempakConstants.RMISSD);
		System.out.println("testPrKnms testCaseNum=" + testCaseNum +
				"f:  Input:wind speed in knots= GempakConstants.RMISSD  Output:wind speed in meters/sec=" + sped);
		assertEquals (GempakConstants.RMISSD, sped);
	}

	/**
	 * Test method for {@link gov.noaa.nws.ncep.gempak.parameterconversionlibrary.PRLibrary#prKnmh(float)}.
	 */
	@Test
	public void testPrKnmh() {
		testCaseNum++;
		float smph = PRLibrary.prKnmh(100f);
		System.out.println("testPrKnmh testCaseNum=" + testCaseNum +
				"a:  Input:wind speed in knots= 100.  Output:wind speed in miles/hour=" + smph);
		assertEquals (115.07798f, smph);
		smph = PRLibrary.prKnmh(15f);
		System.out.println("testPrKnmh testCaseNum=" + testCaseNum +
				"b:  Input:wind speed in knots= 15.  Output:wind speed in miles/hour=" + smph);
		assertEquals (17.2616959f, smph);
		smph = PRLibrary.prKnmh(255f);
		System.out.println("testPrKnmh testCaseNum=" + testCaseNum +
				"c:  Input:wind speed in knots= 255.  Output:wind speed in miles/hour=" + smph);
		assertEquals (293.448853f, smph);
		smph = PRLibrary.prKnmh(350f);
		System.out.println("testPrKnmh testCaseNum=" + testCaseNum +
				"d:  Input:wind speed in knots= 350.  Output:wind speed in miles/hour=" + smph);
		assertEquals (402.772919f, smph);
		smph = PRLibrary.prKnmh(GempakConstants.RMISSD);
		System.out.println("testPrKnmh testCaseNum=" + testCaseNum +
				"e:  Input:wind speed in knots= GempakConstants.RMISSD  Output:wind speed in miles/hour=" + smph);
		assertEquals (GempakConstants.RMISSD, smph);
		smph = PRLibrary.prKnmh(30f);
		System.out.println("testPrKnmh testCaseNum=" + testCaseNum +
				"f:  Input:wind speed in knots= 30  Output:wind speed in miles/hour=" + smph);
		assertEquals (34.5233917, smph);
	}

	/**
	 * Test method for {@link gov.noaa.nws.ncep.gempak.parameterconversionlibrary.PRLibrary#prMhkn(float)}.
	 */
	@Test
	public void testPrMhkn() {
		testCaseNum++;
		float knts = PRLibrary.prMhkn(100f);
		System.out.println("testPrMhkn testCaseNum=" + testCaseNum +
				"a:  Input:wind speed in miles/hour= 100.  Output:wind speed in knots=" + knts);
		assertEquals (86.8975983f, knts);
		knts = PRLibrary.prMhkn(0f);
		System.out.println("testPrMhkn testCaseNum=" + testCaseNum +
				"b:  Input:wind speed in miles/hour= 0.  Output:wind speed in knots=" + knts);
		assertEquals (0f, knts);
		knts = PRLibrary.prMhkn(250f);
		System.out.println("testPrMhkn testCaseNum=" + testCaseNum +
				"c:  Input:wind speed in miles/hour= 250.  Output:wind speed in knots=" + knts);
		assertEquals (217.244003f, knts);
		knts = PRLibrary.prMhkn(410f);
		System.out.println("testPrMhkn testCaseNum=" + testCaseNum +
				"d:  Input:wind speed in miles/hour= 410.  Output:wind speed in knots=" + knts);
		assertEquals (356.280151f, knts);
		knts = PRLibrary.prMhkn(21f);
		System.out.println("testPrMhkn testCaseNum=" + testCaseNum +
				"e:  Input:wind speed in miles/hour= 21.  Output:wind speed in knots=" + knts);
		assertEquals (18.2484951f, knts);
		knts = PRLibrary.prMhkn(GempakConstants.RMISSD);
		System.out.println("testPrMhkn testCaseNum=" + testCaseNum +
				"f:  Input:wind speed in miles/hour= GempakConstants.RMISSD  Output:wind speed in knots=" + knts);
		assertEquals (GempakConstants.RMISSD, knts);
	}
	

	/**
	 * Test method for {@link gov.noaa.nws.ncep.gempak.parameterconversionlibrary.PRLibrary#prHcdm(java.lang.String)}.
	 */
	@Test
	public void testPrHcdm() {
		testCaseNum++;
		float hcdm = PRLibrary.prHcdm("00");
		System.out.println("testPrHcdm testCaseNum=" + testCaseNum +
				"a:  Input:code figure from code table 1677= 00  Output:height in meters=" + hcdm);
		assertEquals (0f, hcdm);
		hcdm = PRLibrary.prHcdm("34");
		System.out.println("testPrHcdm testCaseNum=" + testCaseNum +
				"b:  Input:code figure from code table 1677= 34  Output:height in meters=" + hcdm);
		assertEquals (1020f, hcdm);
		hcdm = PRLibrary.prHcdm("49");
		System.out.println("testPrHcdm testCaseNum=" + testCaseNum +
				"c:  Input:code figure from code table 1677= 49  Output:height in meters=" + hcdm);
		assertEquals (1470f, hcdm);
		hcdm = PRLibrary.prHcdm("50");
		System.out.println("testPrHcdm testCaseNum=" + testCaseNum +
				"d:  Input:code figure from code table 1677= 50  Output:height in meters=" + hcdm);
		assertEquals (1500f, hcdm);
		hcdm = PRLibrary.prHcdm("51");
		System.out.println("testPrHcdm testCaseNum=" + testCaseNum +
				"e:  Input:code figure from code table 1677= 51  Output:height in meters=" + hcdm);
		assertEquals (GempakConstants.RMISSD, hcdm);
		hcdm = PRLibrary.prHcdm("52");
		System.out.println("testPrHcdm testCaseNum=" + testCaseNum +
				"f:  Input:code figure from code table 1677= 52  Output:height in meters=" + hcdm);
		assertEquals (GempakConstants.RMISSD, hcdm);
		hcdm = PRLibrary.prHcdm("55");
		System.out.println("testPrHcdm testCaseNum=" + testCaseNum +
				"g:  Input:code figure from code table 1677= 55  Output:height in meters=" + hcdm);
		assertEquals (GempakConstants.RMISSD, hcdm);
		hcdm = PRLibrary.prHcdm("56");
		System.out.println("testPrHcdm testCaseNum=" + testCaseNum +
				"h:  Input:code figure from code table 1677= 56  Output:height in meters=" + hcdm);
		assertEquals (1800f, hcdm);
		hcdm = PRLibrary.prHcdm("77");
		System.out.println("testPrHcdm testCaseNum=" + testCaseNum +
				"i:  Input:code figure from code table 1677= 77  Output:height in meters=" + hcdm);
		assertEquals (8100f, hcdm);
		hcdm = PRLibrary.prHcdm("88");
		System.out.println("testPrHcdm testCaseNum=" + testCaseNum +
				"j:  Input:code figure from code table 1677= 88  Output:height in meters=" + hcdm);
		assertEquals (21000f, hcdm);
		hcdm = PRLibrary.prHcdm("89");
		System.out.println("testPrHcdm testCaseNum=" + testCaseNum +
				"k:  Input:code figure from code table 1677= 89  Output:height in meters=" + hcdm);
		assertEquals (22500f, hcdm);
		hcdm = PRLibrary.prHcdm("90");
		System.out.println("testPrHcdm testCaseNum=" + testCaseNum +
				"l:  Input:code figure from code table 1677= 90  Output:height in meters=" + hcdm);
		assertEquals (GempakConstants.RMISSD, hcdm);
		hcdm = PRLibrary.prHcdm("99");
		System.out.println("testPrHcdm testCaseNum=" + testCaseNum +
				"m:  Input:code figure from code table 1677= 99  Output:height in meters=" + hcdm);
		assertEquals (GempakConstants.RMISSD, hcdm);
		hcdm = PRLibrary.prHcdm("100");
		System.out.println("testPrHcdm testCaseNum=" + testCaseNum +
				"n:  Input:code figure from code table 1677= 100  Output:height in meters=" + hcdm);
		assertEquals (GempakConstants.RMISSD, hcdm);
		hcdm = PRLibrary.prHcdm("101");
		System.out.println("testPrHcdm testCaseNum=" + testCaseNum +
				"o:  Input:code figure from code table 1677= 101  Output:height in meters=" + hcdm);
		assertEquals (GempakConstants.RMISSD, hcdm);
		hcdm = PRLibrary.prHcdm("80");
		System.out.println("testPrHcdm testCaseNum=" + testCaseNum +
				"p:  Input:code figure from code table 1677= 80  Output:height in meters=" + hcdm);
		assertEquals (9000f, hcdm);
		hcdm = PRLibrary.prHcdm("81");
		System.out.println("testPrHcdm testCaseNum=" + testCaseNum +
				"q:  Input:code figure from code table 1677= 81  Output:height in meters=" + hcdm);
		assertEquals (10500f, hcdm);
		hcdm = PRLibrary.prHcdm("3");
		System.out.println("testPrHcdm testCaseNum=" + testCaseNum +
				"r:  Input:code figure from code table 1677= 3  Output:height in meters=" + hcdm);
		assertEquals (GempakConstants.RMISSD, hcdm);
		hcdm = PRLibrary.prHcdm("0");
		System.out.println("testPrHcdm testCaseNum=" + testCaseNum +
				"s:  Input:code figure from code table 1677= 0  Output:height in meters=" + hcdm);
		assertEquals (GempakConstants.RMISSD, hcdm);
		hcdm = PRLibrary.prHcdm("207");
		System.out.println("testPrHcdm testCaseNum=" + testCaseNum +
				"t:  Input:code figure from code table 1677= 207  Output:height in meters=" + hcdm);
		assertEquals (GempakConstants.RMISSD, hcdm);
	}

	/**
	 * Test method for {@link gov.noaa.nws.ncep.gempak.parameterconversionlibrary.PRLibrary#prHeat(float, float)}.
	 */
	@Test
	public void testPrHeat() {
		testCaseNum++;
		float heat = PRLibrary.prHeat(40f, 50f);
		System.out.println("testPrHeat testCaseNum=" + testCaseNum +
				"a:  Input:tmpf, relh= 158. 140.   Output:Heat index heat=" + heat);
		assertEquals (40f, heat);
		heat = PRLibrary.prHeat(79f, 85f);
		System.out.println("testPrHeat testCaseNum=" + testCaseNum +
				"b:  Input:tmpf, relh= 79. 85..   Output:Heat index heat=" + heat);
		assertEquals (82.5797958f, heat);
		heat = PRLibrary.prHeat(50f, 90f);
		System.out.println("testPrHeat testCaseNum=" + testCaseNum +
				"c:  Input:tmpf, relh= 50. 90.   Output:Heat index heat=" + heat);
		assertEquals (48.9300003f, heat);
		heat = PRLibrary.prHeat(86f, 90f);
		System.out.println("testPrHeat testCaseNum=" + testCaseNum +
				"d:  Input:tmpf, relh= 86.. 90.   Output:Heat index heat=" + heat);
		assertEquals (105.394394f, heat);
		heat = PRLibrary.prHeat(105f, 100f);
		System.out.println("testPrHeat testCaseNum=" + testCaseNum +
				"e:  Input:tmpf, relh= 105. 100.   Output:Heat index heat=" + heat);
		assertEquals (234.359528f, heat);
		heat = PRLibrary.prHeat(95f, 99f);
		System.out.println("testPrHeat testCaseNum=" + testCaseNum +
				"f:  Input:tmpf, relh= 95. 99.   Output:Heat index heat=" + heat);
		assertEquals (159.549545f, heat);
		heat = PRLibrary.prHeat(95f, 99f);
		System.out.println("testPrHeat testCaseNum=" + testCaseNum +
				"g:  Input:tmpf, relh= 95. 99.   Output:Heat index heat=" + heat);
		assertEquals (159.549545f, heat);
		heat = PRLibrary.prHeat(85f, 20f);
		System.out.println("testPrHeat testCaseNum=" + testCaseNum +
				"h:  Input:tmpf, relh= 95. 99.   Output:Heat index heat=" + heat);
		assertEquals (82.0372543f, heat);
		heat = PRLibrary.prHeat(85f, 98f);
		System.out.println("testPrHeat testCaseNum=" + testCaseNum +
				"i:  Input:tmpf, relh= 95. 99.   Output:Heat index heat=" + heat);
		assertEquals (106.390327f, heat);
		heat = PRLibrary.prHeat(GempakConstants.RMISSD, 98f);
		System.out.println("testPrHeat testCaseNum=" + testCaseNum +
				"j:  Input:tmpf, relh= GempakConstants.RMISSD. 99.   Output:Heat index heat=" + heat);
		assertEquals (GempakConstants.RMISSD, heat);
		heat = PRLibrary.prHeat(GempakConstants.RMISSD, GempakConstants.RMISSD);
		System.out.println("testPrHeat testCaseNum=" + testCaseNum +
				"k:  Input:tmpf, relh= GempakConstants.RMISSD. GempakConstants.RMISSD.   Output:Heat index heat=" + heat);
		assertEquals (GempakConstants.RMISSD, heat);
		heat = PRLibrary.prHeat(86f, GempakConstants.RMISSD);
		System.out.println("testPrHeat testCaseNum=" + testCaseNum +
				"l:  Input:tmpf, relh= 86f. GempakConstants.RMISSD.   Output:Heat index heat=" + heat);
		assertEquals (GempakConstants.RMISSD, heat);
		heat = PRLibrary.prHeat(89f, 10f);
		System.out.println("testPrHeat testCaseNum=" + testCaseNum +
				"m:  Input:tmpf, relh= 89. 10.   Output:Heat index heat=" + heat);
		assertEquals (84.4792023, heat);
		heat = PRLibrary.prHeat(90f, 5f);
		System.out.println("testPrHeat testCaseNum=" + testCaseNum +
				"n:  Input:tmpf, relh= 90. 5.   Output:Heat index heat=" + heat);
		assertEquals (84.4517517, heat);
		heat = PRLibrary.prHeat(100f, 20f);
		System.out.println("testPrHeat testCaseNum=" + testCaseNum +
				"o:  Input:tmpf, relh= 100. 20..   Output:Heat index heat=" + heat);
		assertEquals (97.4739685f, heat);
	}

	/**
	 * Test method for {@link gov.noaa.nws.ncep.gempak.parameterconversionlibrary.PRLibrary#prHgmf(float)}.
	 */
	@Test
	public void testPrHgmf() {
		testCaseNum++;
		float hgft = PRLibrary.prHgmf(0f);
		System.out.println("testPrHgmf testCaseNum=" + testCaseNum +
				"a:  Input:Height in meters= 0   Output:Height in feet=" + hgft);
		assertEquals (0f, hgft);
		hgft = PRLibrary.prHgmf(235f);
		System.out.println("testPrHgmf testCaseNum=" + testCaseNum +
				"b:  Input:Height in meters= 235   Output:Height in feet=" + hgft);
		assertEquals (771f, hgft);
		hgft = PRLibrary.prHgmf(236f);
		System.out.println("testPrHgmf testCaseNum=" + testCaseNum +
				"c:  Input:Height in meters= 236   Output:Height in feet=" + hgft);
		assertEquals (774f, hgft);
		hgft = PRLibrary.prHgmf(237f);
		System.out.println("testPrHgmf testCaseNum=" + testCaseNum +
				"d:  Input:Height in meters= 237   Output:Height in feet=" + hgft);
		assertEquals (778f, hgft);
		hgft = PRLibrary.prHgmf(237f);
		System.out.println("testPrHgmf testCaseNum=" + testCaseNum +
				"e:  Input:Height in meters= 237   Output:Height in feet=" + hgft);
		assertEquals (778f, hgft);
		hgft = PRLibrary.prHgmf(GempakConstants.RMISSD);
		System.out.println("testPrHgmf testCaseNum=" + testCaseNum +
				"f:  Input:Height in meters= GempakConstants.RMISSD   Output:Height in feet=" + hgft);
		assertEquals (GempakConstants.RMISSD, hgft);
		hgft = PRLibrary.prHgmf(100000);
		System.out.println("testPrHgmf testCaseNum=" + testCaseNum +
				"f:  Input:Height in meters= 100000   Output:Height in feet=" + hgft);
		assertEquals (328084, hgft);
	}

	/**
	 * Test method for {@link gov.noaa.nws.ncep.gempak.parameterconversionlibrary.PRLibrary#prHgfm(float)}.
	 */
	@Test
	public void testPrHgfm() {
		testCaseNum++;
		float hght = PRLibrary.prHgfm(0f);
		System.out.println("testPrHgfm testCaseNum=" + testCaseNum +
				"a:  Input:Height in feet= 0   Output:Height in meters=" + hght);
		assertEquals (0f, hght);
		hght = PRLibrary.prHgfm(100000f);
		System.out.println("testPrHgfm testCaseNum=" + testCaseNum +
				"b:  Input:Height in feet= 100000   Output:Height in meters=" + hght);
		assertEquals (30480f, hght);
		hght = PRLibrary.prHgfm(5897f);
		System.out.println("testPrHgfm testCaseNum=" + testCaseNum +
				"c:  Input:Height in feet= 5897   Output:Height in meters=" + hght);
		assertEquals (1797.40564f, hght);
		hght = PRLibrary.prHgfm(GempakConstants.RMISSD);
		System.out.println("testPrHgfm testCaseNum=" + testCaseNum +
				"d:  Input:Height in feet= GempakConstants.RMISSD   Output:Height in meters=" + hght);
		assertEquals (GempakConstants.RMISSD, hght);
	}

	/**
	 * Test method for {@link gov.noaa.nws.ncep.gempak.parameterconversionlibrary.PRLibrary#prHgfs(float)}.
	 */
	@Test
	public void testPrHgfs() {
		testCaseNum++;
		float hgml = PRLibrary.prHgfs(0f);
		System.out.println("testPrHgfs testCaseNum=" + testCaseNum +
				"a:  Input:Height in feet= 0   Output:Height in miles=" + hgml);
		assertEquals (0f, hgml);
		hgml = PRLibrary.prHgfs(30500f);
		System.out.println("testPrHgfs testCaseNum=" + testCaseNum +
				"b:  Input:Height in feet= 30500   Output:Height in miles=" + hgml);
		assertEquals (5.77651501f, hgml);
		hgml = PRLibrary.prHgfs(125500f);
		System.out.println("testPrHgfs testCaseNum=" + testCaseNum +
				"c:  Input:Height in feet= 125500   Output:Height in miles=" + hgml);
		assertEquals (23.7689381f, hgml);
		hgml = PRLibrary.prHgfs(300500f);
		System.out.println("testPrHgfs testCaseNum=" + testCaseNum +
				"d:  Input:Height in feet= 300500   Output:Height in miles=" + hgml);
		assertEquals (56.9128761f, hgml);
		hgml = PRLibrary.prHgfs(GempakConstants.RMISSD);
		System.out.println("testPrHgfs testCaseNum=" + testCaseNum +
				"e:  Input:Height in feet= GempakConstants.RMISSD   Output:Height in miles=" + hgml);
		assertEquals (GempakConstants.RMISSD, hgml);
	}

	/**
	 * Test method for {@link gov.noaa.nws.ncep.gempak.parameterconversionlibrary.PRLibrary#prHgsf(float)}.
	 */
	@Test
	public void testPrHgsf() {
		testCaseNum++;
		float hgft = PRLibrary.prHgsf(12345f);
		System.out.println("testPrHgsf testCaseNum=" + testCaseNum +
				"a:  input height in feet= 12345.  Output:height in miles=" + hgft);
		assertEquals (65181600f, hgft);
		hgft = PRLibrary.prHgsf(100f);
		System.out.println("testPrHgsf testCaseNum=" + testCaseNum +
				"b:  input height in feet= 100.  Output:height in miles=" + hgft);
		assertEquals (528000f, hgft);
		hgft = PRLibrary.prHgsf(.5f);
		System.out.println("testPrHgsf testCaseNum=" + testCaseNum +
				"c:  input height in feet= .5   Output:height in miles=" + hgft);
		assertEquals (2640f, hgft);
		hgft = PRLibrary.prHgsf(1.1f);
		System.out.println("testPrHgsf testCaseNum=" + testCaseNum +
				"d:  input height in feet= 1.1   Output:height in miles=" + hgft);
		assertEquals (5808f, hgft);
		hgft = PRLibrary.prHgsf(GempakConstants.RMISSD);
		System.out.println("testPrHgsf testCaseNum=" + testCaseNum +
				"e:  input height in feet= GempakConstants.RMISSD   Output:height in miles=" + hgft);
		assertEquals (GempakConstants.RMISSD, hgft);
	}

	/**
	 * Test method for {@link gov.noaa.nws.ncep.gempak.parameterconversionlibrary.PRLibrary#prHgmk(float)}.
	 */
	@Test
	public void testPrHgmk() {
		testCaseNum++;
		float hgmk = PRLibrary.prHgmk(12345f);
		System.out.println("testPrHgmk testCaseNum=" + testCaseNum +
				"a:  input height in meters= 12345.  Output:height in kilometers=" + hgmk);
		assertEquals (12.345f, hgmk);
		hgmk = PRLibrary.prHgmk(-298751f);
		System.out.println("testPrHgmk testCaseNum=" + testCaseNum +
				"b:  input height in meters= -298751.  Output:height in kilometers=" + hgmk);
		assertEquals (-298.751f, hgmk);
		hgmk = PRLibrary.prHgmk(0f);
		System.out.println("testPrHgmk testCaseNum=" + testCaseNum +
				"c:  input height in meters= 0.  Output:height in kilometers=" + hgmk);
		assertEquals (0f, hgmk);
		hgmk = PRLibrary.prHgmk(345.987f);
		System.out.println("testPrHgmk testCaseNum=" + testCaseNum +
				"d:  input height in meters= 345.987   Output:height in kilometers=" + hgmk);
		assertEquals (.345987f, hgmk);
		hgmk = PRLibrary.prHgmk(GempakConstants.RMISSD);
		System.out.println("testPrHgmk testCaseNum=" + testCaseNum +
				"e:  input height in meters= GempakConstants.RMISSD  Output:height in kilometers=" + hgmk);
		assertEquals (GempakConstants.RMISSD, hgmk);
	}

	/**
	 * Test method for {@link gov.noaa.nws.ncep.gempak.parameterconversionlibrary.PRLibrary#prHgkm(float)}.
	 */
	@Test
	public void testPrHgkm() {
		testCaseNum++;
		float hgkm = PRLibrary.prHgkm(12345f);
		System.out.println("testPrHgkm testCaseNum=" + testCaseNum +
				"a:  input height in meters= 12345.  Output:height in kilometers=" + hgkm);
		assertEquals (12345000f, hgkm);
		hgkm = PRLibrary.prHgkm(340f);
		System.out.println("testPrHgkm testCaseNum=" + testCaseNum +
				"b:  input height in meters= 350.  Output:height in kilometers=" + hgkm);
		assertEquals (340000f, hgkm);
		hgkm = PRLibrary.prHgkm(.879f);
		System.out.println("testPrHgkm testCaseNum=" + testCaseNum +
				"c:  input height in meters= .879  Output:height in kilometers=" + hgkm);
		assertEquals (879f, hgkm);
		hgkm = PRLibrary.prHgkm(25.34f);
		System.out.println("testPrHgkm testCaseNum=" + testCaseNum +
				"d:  input height in meters= 25.34  Output:height in kilometers=" + hgkm);
		assertEquals (25340f, hgkm);
		hgkm = PRLibrary.prHgkm(GempakConstants.RMISSD);
		System.out.println("testPrHgkm testCaseNum=" + testCaseNum +
				"e:  input height in meters= GempakConstants.RMISSD   Output:height in kilometers=" + hgkm);
		assertEquals (GempakConstants.RMISSD, hgkm);
		hgkm = PRLibrary.prHgkm(0f);
		System.out.println("testPrHgkm testCaseNum=" + testCaseNum +
				"f:  input height in meters= 0   Output:height in kilometers=" + hgkm);
		assertEquals (0f, hgkm);
	}

	/**
	 * Test method for {@link gov.noaa.nws.ncep.gempak.parameterconversionlibrary.PRLibrary#prHgmd(float)}.
	 */
	@Test
	public void testPrHgmd() {
		testCaseNum++;
		float hgmd= PRLibrary.prHgmd(23f);
		System.out.println("testPrHgmd testCaseNum=" + testCaseNum +
				"a:  input height in meters= 23.  Output:height in decameters=" + hgmd);
		assertEquals (2.3f, hgmd);
		hgmd= PRLibrary.prHgmd(0f);
		System.out.println("testPrHgmd testCaseNum=" + testCaseNum +
				"b:  input height in meters= 0.  Output:height in decameters=" + hgmd);
		assertEquals (0f, hgmd);
		hgmd= PRLibrary.prHgmd(89730f);
		System.out.println("testPrHgmd testCaseNum=" + testCaseNum +
				"c:  input height in meters= 89730.  Output:height in decameters=" + hgmd);
		assertEquals (8973f, hgmd);
		hgmd= PRLibrary.prHgmd(786.54f);
		System.out.println("testPrHgmd testCaseNum=" + testCaseNum +
				"d:  input height in meters= 786.54  Output:height in decameters=" + hgmd);
		assertEquals (78.654f, hgmd);
		hgmd= PRLibrary.prHgmd(GempakConstants.RMISSD);
		System.out.println("testPrHgmd testCaseNum=" + testCaseNum +
				"e:  input height in meters= GempakConstants.RMISSD   Output:height in decameters=" + hgmd);
		assertEquals (GempakConstants.RMISSD, hgmd);
	}

	/**
	 * Test method for {@link gov.noaa.nws.ncep.gempak.parameterconversionlibrary.PRLibrary#prHgnm(float)}.
	 */
	@Test
	public void testPrHgnm() {
		testCaseNum++;
		float hgnm= PRLibrary.prHgnm(3450f);
		System.out.println("testPrHgnm testCaseNum=" + testCaseNum +
				"a:  input height in nautical miles= 3450. Output:height in meters=" + hgnm);
		assertEquals (6389400f, hgnm);
		hgnm= PRLibrary.prHgnm(0f);
		System.out.println("testPrHgnm testCaseNum=" + testCaseNum +
				"b:  input height in nautical miles= 0. Output:height in meters=" + hgnm);
		assertEquals (0f, hgnm);
		hgnm= PRLibrary.prHgnm(1f);
		System.out.println("testPrHgnm testCaseNum=" + testCaseNum +
				"c:  input height in nautical miles= 1. Output:height in meters=" + hgnm);
		assertEquals (1852f, hgnm);
		hgnm= PRLibrary.prHgnm(55f);
		System.out.println("testPrHgnm testCaseNum=" + testCaseNum +
				"d:  input height in nautical miles= 55. Output:height in meters=" + hgnm);
		assertEquals (101860f, hgnm);
		hgnm= PRLibrary.prHgnm(GempakConstants.RMISSD);
		System.out.println("testPrHgnm testCaseNum=" + testCaseNum +
				"e:  input height in nautical miles= GempakConstants.RMISSD  Output:height in meters=" + hgnm);
		assertEquals (GempakConstants.RMISSD, hgnm);
	}

	/**
	 * Test method for {@link gov.noaa.nws.ncep.gempak.parameterconversionlibrary.PRLibrary#prHmtr(float, float)}.
	 */
	@Test
	public void testPrHmtr() {
		testCaseNum++;
		float hmtr= PRLibrary.prHmtr(GempakConstants.RMISSD, GempakConstants.RMISSD);
		System.out.println("testPrHmtr testCaseNum=" + testCaseNum +
				"a:  input tmpf, dwpf= GempakConstants.RMISSD GempakConstants.RMISSD  Output:humiture index hmtr=" + hmtr);
		assertEquals (GempakConstants.RMISSD, hmtr);
		hmtr= PRLibrary.prHmtr(GempakConstants.RMISSD, 78f);
		System.out.println("testPrHmtr testCaseNum=" + testCaseNum +
				"b:  input tmpf, dwpf= GempakConstants.RMISSD 78.  Output:humiture index hmtr=" + hmtr);
		assertEquals (GempakConstants.RMISSD, hmtr);
		hmtr= PRLibrary.prHmtr(80f, GempakConstants.RMISSD);
		System.out.println("testPrHmtr testCaseNum=" + testCaseNum +
				"c:  input tmpf, dwpf= 80. GempakConstants.RMISSD  Output:humiture index hmtr=" + hmtr);
		assertEquals (GempakConstants.RMISSD, hmtr);
		hmtr= PRLibrary.prHmtr(80f, 78f);
		System.out.println("testPrHmtr testCaseNum=" + testCaseNum +
				"d:  input tmpf, dwpf= 80. 78.  Output:humiture index hmtr=" + hmtr);
		assertEquals (91.73988, hmtr);
		hmtr= PRLibrary.prHmtr(100f, 88f);
		System.out.println("testPrHmtr testCaseNum=" + testCaseNum +
				"e:  input tmpf, dwpf= 100. 88.  Output:humiture index hmtr=" + hmtr);
		assertEquals (124.246048, hmtr);
	}

	/**
	 * Test method for {@link gov.noaa.nws.ncep.gempak.parameterconversionlibrary.PRLibrary#prVapr(float)}.
	 */
	@Test
	public void testPrVapr() {
		testCaseNum++;
		float vapr= PRLibrary.prVapr(GempakConstants.RMISSD);
		System.out.println("testPrVapr testCaseNum=" + testCaseNum +
				"a:  input dwpc= GempakConstants.RMISSD  Output:Vapor pressure in mb vapr=" + vapr);
		assertEquals (GempakConstants.RMISSD, vapr);
		vapr= PRLibrary.prVapr(50f);
		System.out.println("testPrVapr testCaseNum=" + testCaseNum +
				"b:  input dwpc= 50f  Output:Vapor pressure in mb vapr=" + vapr);
		assertEquals (124.024048f, vapr);
		vapr= PRLibrary.prVapr(0f);
		System.out.println("testPrVapr testCaseNum=" + testCaseNum +
				"c:  input dwpc= 0f  Output:Vapor pressure in mb vapr=" + vapr);
		assertEquals (6.112f, vapr);
		vapr= PRLibrary.prVapr(10f);
		System.out.println("testPrVapr testCaseNum=" + testCaseNum +
				"d:  input dwpc= 10f  Output:Vapor pressure in mb vapr=" + vapr);
		assertEquals (12.2716961f, vapr);
		vapr= PRLibrary.prVapr(66f);
		System.out.println("testPrVapr testCaseNum=" + testCaseNum +
				"e:  input dwpc= 66f  Output:Vapor pressure in mb vapr=" + vapr);
		assertEquals (264.62973f, vapr);
		vapr= PRLibrary.prVapr(53.78f);
		System.out.println("testPrVapr testCaseNum=" + testCaseNum +
				"f:  input dwpc= 53.78f  Output:Vapor pressure in mb vapr=" + vapr);
		assertEquals (149.437576f, vapr);
	}

	/**
	 * Test method for {@link gov.noaa.nws.ncep.gempak.parameterconversionlibrary.PRLibrary#prIgro(float, float, float)}.
	 */
	@Test
	public void testPrIgro() {
		testCaseNum++;
		float igro = PRLibrary.prIgro(-10f, 0f, 10f);
		System.out.println("testPrIgro testCaseNum=" + testCaseNum +
				"a:  input tmpc, sstc, sped= -10. 0. 10.   output:Ice growth igro=" + igro);
		assertEquals (2.6934f, igro);
		igro = PRLibrary.prIgro(-20f, -1.7f, 1f);
		System.out.println("testPrIgro testCaseNum=" + testCaseNum +
				"b:  input tmpc, sstc, sped= -20f, -1.7 1.   output:Ice growth igro=" + igro);
		assertEquals (.718486011f, igro);
		igro = PRLibrary.prIgro(-5f, 5f, 50f);
		System.out.println("testPrIgro testCaseNum=" + testCaseNum +
				"c:  input tmpc, sstc, sped= -5. 5. 50.   output:Ice growth igro=" + igro);
		assertEquals (2.33257294f, igro);
		igro = PRLibrary.prIgro(-10f, 5f, 40f);
		System.out.println("testPrIgro testCaseNum=" + testCaseNum +
				"d:  input tmpc, sstc, sped= -10., 5., 40.   output:Ice growth igro=" + igro);
		assertEquals (7.30219698f, igro);
		igro = PRLibrary.prIgro(-10f, 5f, 51f);
		System.out.println("testPrIgro testCaseNum=" + testCaseNum +
				"e:  input tmpc, sstc, sped= -10., 5., 51.   output:Ice growth igro=" + igro);
		assertEquals (GempakConstants.RMISSD, igro);
		igro = PRLibrary.prIgro(1f, 5f, 50f);
		System.out.println("testPrIgro testCaseNum=" + testCaseNum +
				"f:  input tmpc, sstc, sped= -10., 5., 51.   output:Ice growth igro=" + igro);
		assertEquals (GempakConstants.RMISSD, igro);
		igro = PRLibrary.prIgro(0f, 13f, 50f);
		System.out.println("testPrIgro testCaseNum=" + testCaseNum +
				"g:  input tmpc, sstc, sped= 0., 13., 50.   output:Ice growth igro=" + igro);
		assertEquals (GempakConstants.RMISSD, igro);
		igro = PRLibrary.prIgro(GempakConstants.RMISSD, 13f, 50f);
		System.out.println("testPrIgro testCaseNum=" + testCaseNum +
				"h:  input tmpc, sstc, sped= GempakConstants.RMISSD, 13., 50.   output:Ice growth igro=" + igro);
		assertEquals (GempakConstants.RMISSD, igro);
		igro = PRLibrary.prIgro(-20f, GempakConstants.RMISSD, 50f);
		System.out.println("testPrIgro testCaseNum=" + testCaseNum +
				"h:  input tmpc, sstc, sped= -20.GempakConstants.RMISSD, 50.   output:Ice growth igro=" + igro);
		assertEquals (GempakConstants.RMISSD, igro);
		igro = PRLibrary.prIgro(-20f, -1f, GempakConstants.RMISSD);
		System.out.println("testPrIgro testCaseNum=" + testCaseNum +
				"h:  input tmpc, sstc, sped= -20. -1. GempakConstants.RMISSD   output:Ice growth igro=" + igro);
		assertEquals (GempakConstants.RMISSD, igro);
	}

	/**
	 * Test method for {@link gov.noaa.nws.ncep.gempak.parameterconversionlibrary.PRLibrary#prInmm(float)}.
	 */
	@Test
	public void testPrInmm() {
		testCaseNum++;
		float inmm= PRLibrary.prInmm(3f);
		System.out.println("testPrInmm testCaseNum=" + testCaseNum +
				"a:  Input in inches= 3. Output in millimeters=" + inmm);
		assertEquals (76.1999969f, inmm);
		inmm= PRLibrary.prInmm(250f);
		System.out.println("testPrInmm testCaseNum=" + testCaseNum +
				"b:  Input in inches= 250. Output in millimeters=" + inmm);
		assertEquals (6350f, inmm);
		inmm= PRLibrary.prInmm(8000f);
		System.out.println("testPrInmm testCaseNum=" + testCaseNum +
				"c:  Input in inches= 8000. Output in millimeters=" + inmm);
		assertEquals (203200f, inmm);
		inmm= PRLibrary.prInmm(90f);
		System.out.println("testPrInmm testCaseNum=" + testCaseNum +
				"d:  Input in inches= 90. Output in millimeters=" + inmm);
		assertEquals (2286f, inmm);
		inmm= PRLibrary.prInmm(0f);
		System.out.println("testPrInmm testCaseNum=" + testCaseNum +
				"e:  Input in inches= 0. Output in millimeters=" + inmm);
		assertEquals (0f, inmm);
		inmm= PRLibrary.prInmm(GempakConstants.RMISSD);
		System.out.println("testPrInmm testCaseNum=" + testCaseNum +
				"f:  Input in inches= GempakConstants.RMISSD Output in millimeters=" + inmm);
		assertEquals (GempakConstants.RMISSD, inmm);
	}

	/**
	 * Test method for {@link gov.noaa.nws.ncep.gempak.parameterconversionlibrary.PRLibrary#prMmin(float)}.
	 */
	@Test
	public void testPrMmin() {
		testCaseNum++;
		float mmin= PRLibrary.prMmin(3450f);
		System.out.println("testPrMmin testCaseNum=" + testCaseNum +
				"a:  Input in millimeters= 3450. Output in inches=" + mmin);
		assertEquals (135.826843f, mmin);
		mmin= PRLibrary.prMmin(7200f);
		System.out.println("testPrMmin testCaseNum=" + testCaseNum +
				"b:  Input in millimeters= 7200. Output in inches=" + mmin);
		assertEquals (283.464722f, mmin);
		mmin= PRLibrary.prMmin(9271f);
		System.out.println("testPrMmin testCaseNum=" + testCaseNum +
				"c:  Input in millimeters= 9271. Output in inches=" + mmin);
		assertEquals (365.000214f, mmin);
		mmin= PRLibrary.prMmin(0f);
		System.out.println("testPrMmin testCaseNum=" + testCaseNum +
				"d:  Input in millimeters= 0. Output in inches=" + mmin);
		assertEquals (0f, mmin);
		mmin= PRLibrary.prMmin(GempakConstants.RMISSD);
		System.out.println("testPrMmin testCaseNum=" + testCaseNum +
				"e:  Input in millimeters= GempakConstants.RMISSD Output in inches=" + mmin);
		assertEquals (GempakConstants.RMISSD, mmin);
	}

	/**
	 * Test method for {@link gov.noaa.nws.ncep.gempak.parameterconversionlibrary.PRLibrary#prMobs(float, float)}.
	 */
	@Test
	public void testPrMobs() {
		testCaseNum++;
		float mobs= PRLibrary.prMobs(400f, 390f);
		System.out.println("testPrMobs testCaseNum=" + testCaseNum +
				"a:Input:MSL CEIL, Mtn obsc threshold in 100's of ft= 400. 390."
				+ "Output: indicator that mountain obscuration threshold is met=" + mobs);
		assertEquals (0f, mobs);
		mobs= PRLibrary.prMobs(400f, 390f);
		System.out.println("testPrMobs testCaseNum=" + testCaseNum +
				"b:Input:MSL CEIL, Mtn obsc threshold in 100's of ft= 400. 390."
				+ "Output: indicator that mountain obscuration threshold is met=" + mobs);
		assertEquals (1f, mobs);
		mobs= PRLibrary.prMobs(400f, 400f);
		System.out.println("testPrMobs testCaseNum=" + testCaseNum +
				"c:Input:MSL CEIL, Mtn obsc threshold in 100's of ft= 400. 400."
				+ "Output: indicator that mountain obscuration threshold is met=" + mobs);
		assertEquals (0f, mobs);
		mobs= PRLibrary.prMobs(900f, 901f);
		System.out.println("testPrMobs testCaseNum=" + testCaseNum +
				"d:Input:MSL CEIL, Mtn obsc threshold in 100's of ft= 900. 901."
				+ "Output: indicator that mountain obscuration threshold is met=" + mobs);
		assertEquals (1f, mobs);
		mobs= PRLibrary.prMobs(GempakConstants.RMISSD, 901f);
		System.out.println("testPrMobs testCaseNum=" + testCaseNum +
				"e:Input:MSL CEIL, Mtn obsc threshold in 100's of ft= GempakConstants.RMISSD 901."
				+ "Output: indicator that mountain obscuration threshold is met=" + mobs);
		assertEquals (GempakConstants.RMISSD, mobs);
		mobs= PRLibrary.prMobs(589f, GempakConstants.RMISSD);
		System.out.println("testPrMobs testCaseNum=" + testCaseNum +
				"f:Input:MSL CEIL, Mtn obsc threshold in 100's of ft= 589. GempakConstants.RMISSD"
				+ "Output: indicator that mountain obscuration threshold is met=" + mobs);
		assertEquals (GempakConstants.RMISSD, mobs);
	}

	/**
	 * Test method for {@link gov.noaa.nws.ncep.gempak.parameterconversionlibrary.PRLibrary#prLati(float, float, float, float, float)}.
	 */
	@Test
	public void testPrLati() {
		testCaseNum++;
		float lati= PRLibrary.prLati(23.4f, 103.5f, 20f, 10f, 200f);
		System.out.println("testPrLati testCaseNum=" + testCaseNum +
				"a:Input: lat, lon, range, azim, selv= 23.4 103.5 20 10 200"
				+ "  Output: Actual latitude lati=" + lati);
		assertEquals (23.2491379f, lati);
		PRLibrary.getRZLLInstance().prRzll(10.5f, .1f, 30f, 15f, 19050f);
		lati = PRLibrary.getRZLLInstance().getXlat();
		System.out.println("testPrRzll testCaseNum=" + testCaseNum +
				"b:Input: lat, lon, range, azim, selv= 10.5 .1 30. 15. 19050."
				+ "  Output: Actual latitude lati=" + lati);
		assertEquals (10.2952042f, lati);
		PRLibrary.getRZLLInstance().prRzll(0f, -20f, 40f, 20f, 400f);
		lati = PRLibrary.getRZLLInstance().getXlat();
		System.out.println("testPrLati testCaseNum=" + testCaseNum +
				"c:Input: lat, lon, range, azim, selv= 0. -20. 40. 20. 400."
				+ "  Output: Actual latitude lati=" + lati);
		assertEquals (0.146628499f, lati);
		PRLibrary.getRZLLInstance().prRzll(-55f, -123f, 75f, 660f, 4200f);
		lati = PRLibrary.getRZLLInstance().getXlat();
		System.out.println("testPrRzll testCaseNum=" + testCaseNum +
				"d:Input: lat, lon, range, azim, selv= -55 -123 75 660 4200"
				+ "  Output: Actual latitude lati=" + lati);
		assertEquals (-54.3480339f, lati);
		PRLibrary.getRZLLInstance().prRzll(-40f, 0f, GempakConstants.RMISSD, 80f, 1700f);
		lati = PRLibrary.getRZLLInstance().getXlat();
		System.out.println("testPrRzll testCaseNum=" + testCaseNum +
				"e:Input: lat, lon, range, azim, selv= -40 0 GempakConstants.RMISSD 80 1700."
				+ "  Output: Actual latitude lati=" + lati);
		assertEquals (GempakConstants.RMISSD, lati);
		PRLibrary.getRZLLInstance().prRzll(-40f, 0f, 95f, GempakConstants.RMISSD, 1700f);
		lati = PRLibrary.getRZLLInstance().getXlat();
		System.out.println("testPrRzll testCaseNum=" + testCaseNum +
				"f:Input: lat, lon, range, azim, selv= -40 0 95 GempakConstants.RMISSD 1700."
				+ "  Output: Actual latitude lati=" + lati);
		assertEquals (GempakConstants.RMISSD, lati);
		PRLibrary.getRZLLInstance().prRzll(0f, 0f, 95f, 500f, GempakConstants.RMISSD);
		lati = PRLibrary.getRZLLInstance().getXlat();
		System.out.println("testPrRzll testCaseNum=" + testCaseNum +
				"g:Input: lat, lon, range, azim, selv= 0 0 95 500 GempakConstants.RMISSD"
				+ "  Output: Actual latitude lati=" + lati);
		assertEquals (GempakConstants.RMISSD, lati);
		PRLibrary.getRZLLInstance().prRzll(GempakConstants.RMISSD, 0f, 95f, 500f, 3500f);
		lati = PRLibrary.getRZLLInstance().getXlat();
		System.out.println("testPrRzll testCaseNum=" + testCaseNum +
				"h:Input: lat, lon, range, azim, selv= GempakConstants.RMISSD 0 95 500 3500"
				+ "  Output: Actual latitude lati=" + lati);
		assertEquals (GempakConstants.RMISSD, lati);
	}

	/**
	 * Test method for {@link gov.noaa.nws.ncep.gempak.parameterconversionlibrary.PRLibrary#prLoni(float, float, float, float, float)}.
	 */
	@Test
	public void testPrLoni() {
		testCaseNum++;
		float loni= PRLibrary.prLoni(23.4f, 103.5f, 20f, 10f, 200f);
		System.out.println("testPrLoni testCaseNum=" + testCaseNum +
				"a:Input: lat, lon, range, azim, selv= 23.4 103.5 20 10 200"
				+ "  Output: Actual latitude lati=" + loni);
		assertEquals (103.39357f, loni);
		PRLibrary.getRZLLInstance().prRzll(10.5f, .1f, 30f, 15f, 19050f);
		loni = PRLibrary.getRZLLInstance().getXlon();
		System.out.println("testPrRzll testCaseNum=" + testCaseNum +
				"b:Input: lat, lon, range, azim, selv= 10.5 .1 30. 15. 19050."
				+ "  Actual longitude loni=" + loni);
		assertEquals (0.278129399f, loni);
		PRLibrary.getRZLLInstance().prRzll(0f, -20f, 40f, 20f, 400f);
		loni = PRLibrary.getRZLLInstance().getXlon();
		System.out.println("testPrRzll testCaseNum=" + testCaseNum +
				"c:Input: lat, lon, range, azim, selv= 0. -20. 40. 20. 400."
				+ "  Actual longitude loni=" + loni);
		assertEquals (-19.6719666f, loni);
		PRLibrary.getRZLLInstance().prRzll(-55f, -123f, 75f, 660f, 4200f);
		loni = PRLibrary.getRZLLInstance().getXlon();
		System.out.println("testPrRzll testCaseNum=" + testCaseNum +
				"d:Input: lat, lon, range, azim, selv= -40 -80 60 50 1200."
				+ "  Actual longitude loni=" + loni);
		assertEquals (-122.695969f, loni);		
		PRLibrary.getRZLLInstance().prRzll(0f, 23f, GempakConstants.RMISSD, 600f, 2500f);
		loni = PRLibrary.getRZLLInstance().getXlon();
		System.out.println("testPrRzll testCaseNum=" + testCaseNum +
				"e:Input: lat, lon, range, azim, selv= 0 23 GempakConstants.RMISSD 600 2500"
				+ "  Actual longitude loni=" + loni);
		assertEquals (GempakConstants.RMISSD, loni);
		PRLibrary.getRZLLInstance().prRzll(-40f, 0f, 95f, GempakConstants.RMISSD, 1700f);
		loni = PRLibrary.getRZLLInstance().getXlon();
		System.out.println("testPrRzll testCaseNum=" + testCaseNum +
				"f:Input: lat, lon, range, azim, selv= -40 0 95 GempakConstants.RMISSD 1700."
				+ "  Actual longitude loni=" + loni);
		assertEquals (GempakConstants.RMISSD, loni);
		PRLibrary.getRZLLInstance().prRzll(-40f, 0f, 95f, 500f, GempakConstants.RMISSD);
		loni = PRLibrary.getRZLLInstance().getXlon();
		System.out.println("testPrRzll testCaseNum=" + testCaseNum +
				"g:Input: lat, lon, range, azim, selv= -40 0 95 500 GempakConstants.RMISSD"
				+ "  Actual longitude loni=" + loni);
		assertEquals (GempakConstants.RMISSD, loni);
		PRLibrary.getRZLLInstance().prRzll(-35f, GempakConstants.RMISSD, 95f, 500f, 3500f);
		loni = PRLibrary.getRZLLInstance().getXlon();
		System.out.println("testPrRzll testCaseNum=" + testCaseNum +
				"h:Input: lat, lon, range, azim, selv= 35 GempakConstants.RMISSD 95 500 3500"
				+ "  Actual longitude loni=" + loni);
		assertEquals (GempakConstants.RMISSD, loni);
	}

	/**
	 * Test method for {@link gov.noaa.nws.ncep.gempak.parameterconversionlibrary.PRLibrary#prLoni(float, float, float, float, float)}.
	 */
	@Test
	public void testPrRzll() {
		testCaseNum++;
		PRLibrary.getRZLLInstance().prRzll(23.4f, 103.5f, 20f, 10f, 200f);
		float lati = PRLibrary.getRZLLInstance().getXlat();
		float loni = PRLibrary.getRZLLInstance().getXlon();
		System.out.println("testPrRzll testCaseNum=" + testCaseNum +
				"a:Input: lat, lon, range, azim, selv= 23.4 103.5 20 10 200"
				+ "  Output: Actual latitude lati=" + lati
				+ "  Actual longitude loni=" + loni);
		assertEquals (23.2491379f, lati);
		assertEquals (103.39357f, loni);
		PRLibrary.getRZLLInstance().prRzll(10.5f, 70.25f, 30f, 15f, 300f);
		lati = PRLibrary.getRZLLInstance().getXlat();
		loni = PRLibrary.getRZLLInstance().getXlon();
		System.out.println("testPrRzll testCaseNum=" + testCaseNum +
				"b:Input: lat, lon, range, azim, selv= 10.5 70.25 30. 15. 300."
				+ "  Output: Actual latitude lati=" + lati
				+ "  Actual longitude loni=" + loni);
		assertEquals (10.2952042f, lati);
		assertEquals (70.4281235f, loni);
		PRLibrary.getRZLLInstance().prRzll(.5f, -20f, 40f, 20f, 400f);
		lati = PRLibrary.getRZLLInstance().getXlat();
		loni = PRLibrary.getRZLLInstance().getXlon();
		System.out.println("testPrRzll testCaseNum=" + testCaseNum +
				"c1:Input: lat, lon, range, azim, selv= .5 -20. 40. 20. 400."
				+ "  Output: Actual latitude lati=" + lati
				+ "  Actual longitude loni=" + loni);
		assertEquals (0.646620274f, lati);
		assertEquals (-19.6719456f, loni);
		PRLibrary.getRZLLInstance().prRzll(.0001f, -20f, 40f, 20f, 400f);
		lati = PRLibrary.getRZLLInstance().getXlat();
		loni = PRLibrary.getRZLLInstance().getXlon();
		System.out.println("testPrRzll testCaseNum=" + testCaseNum +
				"c2:Input: lat, lon, range, azim, selv= .0001 -20. 40. 20. 400."
				+ "  Output: Actual latitude lati=" + lati
				+ "  Actual longitude loni=" + loni);
		assertEquals (0.146728486f, lati);
		assertEquals (-19.6719666f, loni);
		PRLibrary.getRZLLInstance().prRzll(0f, -20f, 40f, 20f, 400f);
		lati = PRLibrary.getRZLLInstance().getXlat();
		loni = PRLibrary.getRZLLInstance().getXlon();
		System.out.println("testPrRzll testCaseNum=" + testCaseNum +
				"c:Input: lat, lon, range, azim, selv= 0. -20. 40. 20. 400."
				+ "  Output: Actual latitude lati=" + lati
				+ "  Actual longitude loni=" + loni);
		assertEquals (0.146628499f, lati);
		assertEquals (-19.6719666f, loni);
		PRLibrary.getRZLLInstance().prRzll(25f, 175f, 50f, 35f, 500f);
		lati = PRLibrary.getRZLLInstance().getXlat();
		loni = PRLibrary.getRZLLInstance().getXlon();
		System.out.println("testPrRzll testCaseNum=" + testCaseNum +
				"d:Input: lat, lon, range, azim, selv= 25. 175. 50. 35. 500."
				+ "  Output: Actual latitude lati=" + lati
				+ "  Actual longitude loni=" + loni);
		assertEquals (24.5937195f, lati);
		assertEquals (174.788361f, loni);
		PRLibrary.getRZLLInstance().prRzll(-40f, -80f, 60f, 50f, 1200f);
		lati = PRLibrary.getRZLLInstance().getXlat();
		loni = PRLibrary.getRZLLInstance().getXlon();
		System.out.println("testPrRzll testCaseNum=" + testCaseNum +
				"e:Input: lat, lon, range, azim, selv= -40 -80 60 50 1200."
				+ "  Output: Actual latitude lati=" + lati
				+ "  Actual longitude loni=" + loni);
		assertEquals (-39.4790382f, lati);
		assertEquals (-80.1834564f, loni);
		PRLibrary.getRZLLInstance().prRzll(-40f, 0f, 90f, 80f, 1700f);
		lati = PRLibrary.getRZLLInstance().getXlat();
		loni = PRLibrary.getRZLLInstance().getXlon();
		System.out.println("testPrRzll testCaseNum=" + testCaseNum +
				"f:Input: lat, lon, range, azim, selv= -40 0 90 80 1700."
				+ "  Output: Actual latitude lati=" + lati
				+ "  Actual longitude loni=" + loni);
		assertEquals (-40.0846214f, lati);
		assertEquals (-1.05167794f, loni);
		PRLibrary.getRZLLInstance().prRzll(-40f, 0f, GempakConstants.RMISSD, 80f, 1700f);
		lati = PRLibrary.getRZLLInstance().getXlat();
		loni = PRLibrary.getRZLLInstance().getXlon();
		System.out.println("testPrRzll testCaseNum=" + testCaseNum +
				"g:Input: lat, lon, range, azim, selv= -40 0 GempakConstants.RMISSD 80 1700."
				+ "  Output: Actual latitude lati=" + lati
				+ "  Actual longitude loni=" + loni);
		assertEquals (GempakConstants.RMISSD, lati);
		assertEquals (GempakConstants.RMISSD, loni);
		PRLibrary.getRZLLInstance().prRzll(-40f, 0f, 95f, GempakConstants.RMISSD, 1700f);
		lati = PRLibrary.getRZLLInstance().getXlat();
		loni = PRLibrary.getRZLLInstance().getXlon();
		System.out.println("testPrRzll testCaseNum=" + testCaseNum +
				"h:Input: lat, lon, range, azim, selv= -40 0 95 GempakConstants.RMISSD 1700."
				+ "  Output: Actual latitude lati=" + lati
				+ "  Actual longitude loni=" + loni);
		assertEquals (GempakConstants.RMISSD, lati);
		assertEquals (GempakConstants.RMISSD, loni);
		PRLibrary.getRZLLInstance().prRzll(-40f, 0f, 95f, 500f, GempakConstants.RMISSD);
		lati = PRLibrary.getRZLLInstance().getXlat();
		loni = PRLibrary.getRZLLInstance().getXlon();
		System.out.println("testPrRzll testCaseNum=" + testCaseNum +
				"i:Input: lat, lon, range, azim, selv= -40 0 95 500 GempakConstants.RMISSD"
				+ "  Output: Actual latitude lati=" + lati
				+ "  Actual longitude loni=" + loni);
		assertEquals (GempakConstants.RMISSD, lati);
		assertEquals (GempakConstants.RMISSD, loni);
		PRLibrary.getRZLLInstance().prRzll(GempakConstants.RMISSD, 0f, 95f, 500f, 3500f);
		lati = PRLibrary.getRZLLInstance().getXlat();
		loni = PRLibrary.getRZLLInstance().getXlon();
		System.out.println("testPrRzll testCaseNum=" + testCaseNum +
				"j:Input: lat, lon, range, azim, selv= GempakConstants.RMISSD 0 95 500 3500"
				+ "  Output: Actual latitude lati=" + lati
				+ "  Actual longitude loni=" + loni);
		assertEquals (GempakConstants.RMISSD, lati);
		assertEquals (GempakConstants.RMISSD, loni);
		PRLibrary.getRZLLInstance().prRzll(-35f, GempakConstants.RMISSD, 95f, 500f, 3500f);
		lati = PRLibrary.getRZLLInstance().getXlat();
		loni = PRLibrary.getRZLLInstance().getXlon();
		System.out.println("testPrRzll testCaseNum=" + testCaseNum +
				"k:Input: lat, lon, range, azim, selv= 35 GempakConstants.RMISSD 95 500 3500"
				+ "  Output: Actual latitude lati=" + lati
				+ "  Actual longitude loni=" + loni);
		assertEquals (GempakConstants.RMISSD, lati);
		assertEquals (GempakConstants.RMISSD, loni);
	}
	
	/**
	 * Test method for {@link gov.noaa.nws.ncep.gempak.parameterconversionlibrary.PRLibrary#prLhvp(float)}.
	 */
	@Test
	public void testPrLhvp() {
		testCaseNum++;
		float lhvp= PRLibrary.prLhvp(30f);
		System.out.println("testPrLhvp testCaseNum=" + testCaseNum +
				"a: Input:tmpc= 30   Output:Latent heat lhvp in J/kg=" + lhvp);
		assertEquals (2428900f, lhvp);
		lhvp= PRLibrary.prLhvp(0f);
		System.out.println("testPrLhvp testCaseNum=" + testCaseNum +
				"b: Input:tmpc= 0   Output:Latent heat lhvp in J/kg=" + lhvp);
		assertEquals (2500000f, lhvp);
		lhvp= PRLibrary.prLhvp(-5f);
		System.out.println("testPrLhvp testCaseNum=" + testCaseNum +
				"c: Input:tmpc= -5   Output:Latent heat lhvp in J/kg=" + lhvp);
		assertEquals (2511850f, lhvp);
		lhvp= PRLibrary.prLhvp(85f);
		System.out.println("testPrLhvp testCaseNum=" + testCaseNum +
				"d: Input:tmpc= 85   Output:Latent heat lhvp in J/kg=" + lhvp);
		assertEquals (2298550f, lhvp);
		lhvp= PRLibrary.prLhvp(135f);
		System.out.println("testPrLhvp testCaseNum=" + testCaseNum +
				"e: Input:tmpc= 135   Output:Latent heat lhvp in J/kg=" + lhvp);
		assertEquals (2180050f, lhvp);
		lhvp= PRLibrary.prLhvp(GempakConstants.RMISSD);
		System.out.println("testPrLhvp testCaseNum=" + testCaseNum +
				"f: Input:tmpc= GempakConstants.RMISSD   Output:Latent heat lhvp in J/kg=" + lhvp);
		assertEquals (GempakConstants.RMISSD, lhvp);
	}

	/**
	 * Test method for {@link gov.noaa.nws.ncep.gempak.parameterconversionlibrary.PRLibrary#prLtmp(float, float, float)}.
	 */
	@Test
	public void testPrLtmp() {
		/* 88888888 Not tested yet since no GEMPAK test 88888888888 */
		testCaseNum++;
		float ltmp= PRLibrary.prLtmp(30f, 35f, 250f);
		System.out.println("testPrLtmp testCaseNum=" + testCaseNum +
				"a: Input:Potential temp in K thta, equiv potential temp in K thte, " +
				"lifted pressure= 30 35 250  " + " Output:Lifted temp in C ltmp=" + ltmp);
//		assertEquals (   , ltmp);
	}

	/**
	 * Test method for {@link gov.noaa.nws.ncep.gempak.parameterconversionlibrary.PRLibrary#prM100(float)}.
	 */
	@Test
	public void testPrM100() {
		testCaseNum++;
		float m100= PRLibrary.prM100(30f);
		System.out.println("testPrM100 testCaseNum=" + testCaseNum +
				"a: Input:value= 30   Output:value * 100=" + m100);
		assertEquals (3000f, m100);
		m100= PRLibrary.prM100(.4383f);
		System.out.println("testPrM100 testCaseNum=" + testCaseNum +
				"b: Input:value= .4683   Output:value * 100=" + m100);
		assertEquals (43.83f, m100);
		m100= PRLibrary.prM100(GempakConstants.RMISSD);
		System.out.println("testPrM100 testCaseNum=" + testCaseNum +
				"c: Input:value= GempakConstants.RMISSD   Output:value * 100=" + m100);
		assertEquals (GempakConstants.RMISSD, m100);
		m100= PRLibrary.prM100(378563f);
		System.out.println("testPrM100 testCaseNum=" + testCaseNum +
				"d: Input:value= 378563.   Output:value * 100=" + m100);
		assertEquals (37856300f, m100);
	}

	/**
	 * Test method for {@link gov.noaa.nws.ncep.gempak.parameterconversionlibrary.PRLibrary#prMhgt(float, float, float, float)}.
	 */
	@Test
	public void testPrMhgt() {
		testCaseNum++;
		float mhgt= PRLibrary.prMhgt(1000f, 995f, 990f, 340f);
		System.out.println("testPrMhgt testCaseNum=" + testCaseNum +
				"a: Input:bottom ht, bottom pressure, top pressure, scale height= 1000 995 990 340  " +
				" Output:Moist hydrostatic height=" + mhgt);
		assertEquals (1001.71283f, mhgt);
		mhgt= PRLibrary.prMhgt(2030f, 1020f, 1050f, 457f);
		System.out.println("testPrMhgt testCaseNum=" + testCaseNum +
				"b: Input:bottom ht, bottom pressure, top pressure, scale height= 2030 1020 1050 457" +
				" Output:Moist hydrostatic height=" + mhgt);
		assertEquals (2016.75269f, mhgt);
		mhgt= PRLibrary.prMhgt(458f, 970f, 1000f, 430f);
		System.out.println("testPrMhgt testCaseNum=" + testCaseNum +
				"c: Input:bottom ht, bottom pressure, top pressure, scale height= 458 970 1000 430" +
				" Output:Moist hydrostatic height=" + mhgt);
		assertEquals (444.902527f, mhgt);
		mhgt= PRLibrary.prMhgt(GempakConstants.RMISSD, 970f, 1000f, 430f);
		System.out.println("testPrMhgt testCaseNum=" + testCaseNum +
				"c: Input:bottom ht, bottom pressure, top pressure, scale height= GempakConstants.RMISSD 970 1000 430" +
				" Output:Moist hydrostatic height=" + mhgt);
		assertEquals (GempakConstants.RMISSD, mhgt);
		mhgt= PRLibrary.prMhgt(630f, GempakConstants.RMISSD, 1000f, 430f);
		System.out.println("testPrMhgt testCaseNum=" + testCaseNum +
				"d: Input:bottom ht, bottom pressure, top pressure, scale height= 630 GempakConstants.RMISSD 1000 430" +
				" Output:Moist hydrostatic height=" + mhgt);
		assertEquals (GempakConstants.RMISSD, mhgt);
		mhgt= PRLibrary.prMhgt(630f, 1030f, GempakConstants.RMISSD, 430f);
		System.out.println("testPrMhgt testCaseNum=" + testCaseNum +
				"e: Input:bottom ht, bottom pressure, top pressure, scale height= 630 1030 GempakConstants.RMISSD 430" +
				" Output:Moist hydrostatic height=" + mhgt);
		assertEquals (GempakConstants.RMISSD, mhgt);
		mhgt= PRLibrary.prMhgt(630f, 1030f, 1010f, GempakConstants.RMISSD);
		System.out.println("testPrMhgt testCaseNum=" + testCaseNum +
				"f: Input:bottom ht, bottom pressure, top pressure, scale height= 630 1030 1010 GempakConstants.RMISSD" +
				" Output:Moist hydrostatic height=" + mhgt);
		assertEquals (GempakConstants.RMISSD, mhgt);
	}

	/**
	 * Test method for {@link gov.noaa.nws.ncep.gempak.parameterconversionlibrary.PRLibrary#prP03c(float)}.
	 */
	@Test
	public void testPrP03c() {
		testCaseNum++;
		float p03c= PRLibrary.prP03c(1000f);
		System.out.println("testPrP03c testCaseNum=" + testCaseNum +
				"a: Input:Pressure tendency information= 1000" +
				" Output:pressure change in mb=" + p03c);
		assertEquals (0f, p03c);
		p03c= PRLibrary.prP03c(950f);
		System.out.println("testPrP03c testCaseNum=" + testCaseNum +
				"b: Input:Pressure tendency information= 950" +
				" Output:pressure change in mb=" + p03c);
		assertEquals (95f, p03c);
		p03c= PRLibrary.prP03c(1053f);
		System.out.println("testPrP03c testCaseNum=" + testCaseNum +
				"c: Input:Pressure tendency information= 1053" +
				" Output:pressure change in mb=" + p03c);
		assertEquals (5.3f, p03c);
		p03c= PRLibrary.prP03c(1234f);
		System.out.println("testPrP03c testCaseNum=" + testCaseNum +
				"d: Input:Pressure tendency information= 1234" +
				" Output:pressure change in mb=" + p03c);
		assertEquals (23.399996f, p03c);
		p03c= PRLibrary.prP03c(4234f);
		System.out.println("testPrP03c testCaseNum=" + testCaseNum +
				"e: Input:Pressure tendency information= 4234" +
				" Output:pressure change in mb=" + p03c);
		assertEquals (0f, p03c);
		p03c= PRLibrary.prP03c(5389f);
		System.out.println("testPrP03c testCaseNum=" + testCaseNum +
				"f: Input:Pressure tendency information= 5389" +
				" Output:pressure change in mb=" + p03c);
		assertEquals (-38.90f, p03c);
		p03c= PRLibrary.prP03c(8785f);
		System.out.println("testPrP03c testCaseNum=" + testCaseNum +
				"g: Input:Pressure tendency information= 8785" +
				" Output:pressure change in mb=" + p03c);
		assertEquals (-78.5f, p03c);
		p03c= PRLibrary.prP03c(9537f);
		System.out.println("testPrP03c testCaseNum=" + testCaseNum +
				"h: Input:Pressure tendency information= 9537" +
				" Output:pressure change in mb=" + p03c);
		assertEquals (GempakConstants.RMISSD, p03c);
		p03c= PRLibrary.prP03c(GempakConstants.RMISSD);
		System.out.println("testPrP03c testCaseNum=" + testCaseNum +
				"i: Input:Pressure tendency information= GempakConstants.RMISSD" +
				" Output:pressure change in mb=" + p03c);
		assertEquals (GempakConstants.RMISSD, p03c);
		p03c= PRLibrary.prP03c(3895f);
		System.out.println("testPrP03c testCaseNum=" + testCaseNum +
				"f: Input:Pressure tendency information= 3895" +
				" Output:pressure change in mb=" + p03c);
		assertEquals (89.5f, p03c);
	}

	/**
	 * Test method for {@link gov.noaa.nws.ncep.gempak.parameterconversionlibrary.PRLibrary#prP30d(java.lang.String)}.
	 */
	@Test
	public void testPrP03d() {
		testCaseNum++;
		float p03d= PRLibrary.prP03d("1000");
		System.out.println("testPrP03d testCaseNum=" + testCaseNum +
				"a: Input:WMO pressure tendency code= 1000" +
				" Output:pressure change information=" + p03d);
		assertEquals (1000f, p03d);
		p03d= PRLibrary.prP03d("0238");
		System.out.println("testPrP03d testCaseNum=" + testCaseNum +
				"b: Input:WMO pressure tendency code= 0238" +
				" Output:pressure change information=" + p03d);
		assertEquals (238f, p03d);
		p03d= PRLibrary.prP03d("1jkd");
		System.out.println("testPrP03d testCaseNum=" + testCaseNum +
				"c: Input:WMO pressure tendency code= 1jkd" +
				" Output:pressure change information=" + p03d);
		assertEquals (GempakConstants.RMISSD, p03d);
		p03d= PRLibrary.prP03d("GempakConstants.RMISSD");
		System.out.println("testPrP03d testCaseNum=" + testCaseNum +
				"d: Input:WMO pressure tendency code= GempakConstants.RMISSD" +
				" Output:pressure change information=" + p03d);
		assertEquals (GempakConstants.RMISSD, p03d);
		p03d= PRLibrary.prP03d("8523");
		System.out.println("testPrP03d testCaseNum=" + testCaseNum +
				"e: Input:WMO pressure tendency code= 8523" +
				" Output:pressure change information=" + p03d);
		assertEquals (8523f, p03d);
		p03d= PRLibrary.prP03d("9787");
		System.out.println("testPrP03d testCaseNum=" + testCaseNum +
				"f: Input:WMO pressure tendency code= 9787" +
				" Output:pressure change information=" + p03d);
		assertEquals (GempakConstants.RMISSD, p03d);
		p03d= PRLibrary.prP03d("999");
		System.out.println("testPrP03d testCaseNum=" + testCaseNum +
				"f: Input:WMO pressure tendency code= 999" +
				" Output:pressure change information=" + p03d);
		assertEquals (GempakConstants.RMISSD, p03d);
		p03d= PRLibrary.prP03d("23");
		System.out.println("testPrP03d testCaseNum=" + testCaseNum +
				"f: Input:WMO pressure tendency code= 23" +
				" Output:pressure change information=" + p03d);
		assertEquals (23f, p03d);
		p03d= PRLibrary.prP03d("124523");
		System.out.println("testPrP03d testCaseNum=" + testCaseNum +
				"f: Input:WMO pressure tendency code= 124523" +
				" Output:pressure change information=" + p03d);
		assertEquals (1245f, p03d);
	}

	/**
	 * Test method for {@link gov.noaa.nws.ncep.gempak.parameterconversionlibrary.PRLibrary#prPalt(float, float)}.
	 */
	@Test
	public void testPrPalt() {
		testCaseNum++;
		float palt= PRLibrary.prPalt(1000f, 2350f);
		System.out.println("testPrPalt testCaseNum=" + testCaseNum +
				"a: Input:Altimeter (mb), station elevation (m)= 1000 2350" +
				" Output:Station pressure (mb)=" + palt);
		assertEquals (751.057983f, palt);
		palt= PRLibrary.prPalt(475f, 1200f);
		System.out.println("testPrPalt testCaseNum=" + testCaseNum +
				"b: Input:Altimeter (mb), station elevation (m)= 475 1200" +
				" Output:Station pressure (mb)=" + palt);
		assertEquals (411.200867f, palt);
		palt= PRLibrary.prPalt(0f, 3200f);
		System.out.println("testPrPalt testCaseNum=" + testCaseNum +
				"c: Input:Altimeter (mb), station elevation (m)= 0 3200" +
				" Output:Station pressure (mb)=" + palt);
		assertEquals (0f, palt);
		palt= PRLibrary.prPalt(189f, 0f);
		System.out.println("testPrPalt testCaseNum=" + testCaseNum +
				"d: Input:Altimeter (mb), station elevation (m)= 189 0" +
				" Output:Station pressure (mb)=" + palt);
		assertEquals (189f, palt);
		palt= PRLibrary.prPalt(590f, 3200f);
		System.out.println("testPrPalt testCaseNum=" + testCaseNum +
				"e: Input:Altimeter (mb), station elevation (m)= 5990 3200" +
				" Output:Station pressure (mb)=" + palt);
		assertEquals (397.955719f, palt);
		palt= PRLibrary.prPalt(3500f, 2300f);
		System.out.println("testPrPalt testCaseNum=" + testCaseNum +
				"f: Input:Altimeter (mb), station elevation (m)= 3500 2300" +
				" Output:Station pressure (mb)=" + palt);
		assertEquals (2645.19995f, palt);
		palt= PRLibrary.prPalt(GempakConstants.RMISSD, 2300f);
		System.out.println("testPrPalt testCaseNum=" + testCaseNum +
				"g: Input:Altimeter (mb), station elevation (m)= GempakConstants.RMISSD 2300" +
				" Output:Station pressure (mb)=" + palt);
		assertEquals (GempakConstants.RMISSD, palt);
		palt= PRLibrary.prPalt(3400f, GempakConstants.RMISSD);
		System.out.println("testPrPalt testCaseNum=" + testCaseNum +
				"h: Input:Altimeter (mb), station elevation (m)= 3400 GempakConstants.RMISSD" +
				" Output:Station pressure (mb)=" + palt);
		assertEquals (GempakConstants.RMISSD, palt);
	}

	/**
	 * Test method for {@link gov.noaa.nws.ncep.gempak.parameterconversionlibrary.PRLibrary#prThte(float, float, float)}.
	 */
	@Test
	public void testPrThte() {
		testCaseNum++;
		float thte= PRLibrary.prThte(1000f, 80f, 70f);
		System.out.println("testPrThte testCaseNum=" + testCaseNum +
				"a: Input:PRES (mb), TMPC, DWPC= 1000 80 70" +
				" Output:Equivalent potential temp in K=" + thte);
		assertEquals (4924.67652f, thte);
		thte= PRLibrary.prThte(960f, 50f, 45f);
		System.out.println("testPrThte testCaseNum=" + testCaseNum +
				"b: Input:PRES (mb), TMPC, DWPC= 960 50 45" +
				" Output:Equivalent potential temp in K=" + thte);
		assertEquals (593.602417f, thte);
		thte= PRLibrary.prThte(980f, 30f, 28f);
		System.out.println("testPrThte testCaseNum=" + testCaseNum +
				"c: Input:PRES (mb), TMPC, DWPC= 980 30 28" +
				" Output:Equivalent potential temp in K=" + thte);
		assertEquals (380.783081f, thte);
		thte= PRLibrary.prThte(1040f, 30f, 30f);
		System.out.println("testPrThte testCaseNum=" + testCaseNum +
				"d: Input:PRES (mb), TMPC, DWPC= 1040 30 30" +
				" Output:Equivalent potential temp in K=" + thte);
		assertEquals (378.671082f, thte);
		thte= PRLibrary.prThte(1040f, 30f, GempakConstants.RMISSD);
		System.out.println("testPrThte testCaseNum=" + testCaseNum +
				"e: Input:PRES (mb), TMPC, DWPC= 1040 30 GempakConstants.RMISSD" +
				" Output:Equivalent potential temp in K=" + thte);
		assertEquals (GempakConstants.RMISSD, thte);
		thte= PRLibrary.prThte(1040f, GempakConstants.RMISSD, 30f);
		System.out.println("testPrThte testCaseNum=" + testCaseNum +
				"f: Input:PRES (mb), TMPC, DWPC= 1040 GempakConstants.RMISSD 30" +
				" Output:Equivalent potential temp in K=" + thte);
		assertEquals (GempakConstants.RMISSD, thte);
		thte= PRLibrary.prThte(GempakConstants.RMISSD, 45f, 43f);
		System.out.println("testPrThte testCaseNum=" + testCaseNum +
				"g: Input:PRES (mb), TMPC, DWPC= GempakConstants.RMISSD 45 43" +
				" Output:Equivalent potential temp in K=" + thte);
		assertEquals (GempakConstants.RMISSD, thte);
	}

	/**
	 * Test method for {@link gov.noaa.nws.ncep.gempak.parameterconversionlibrary.PRLibrary#prTlcl(float, float)}.
	 */
	@Test
	public void testPrTlcl() {
		testCaseNum++;
		float tlcl= PRLibrary.prTlcl(-300f, -238f);
		System.out.println("testPrTlcl testCaseNum=" + testCaseNum +
				"a: Input:temperature in C, dewpoint in C= -300 -238" +
				" Output:Temperature (K) at the lifted Condensation Level LCL=" + tlcl);
		assertEquals (GempakConstants.RMISSD, tlcl);
		tlcl= PRLibrary.prTlcl(-230f, -280f);
		System.out.println("testPrTlcl testCaseNum=" + testCaseNum +
				"b: Input:temperature in C, dewpoint in C= -230 -280" +
				" Output:Temperature (K) at the lifted Condensation Level LCL=" + tlcl);
		assertEquals (GempakConstants.RMISSD, tlcl);
		tlcl= PRLibrary.prTlcl(GempakConstants.RMISSD, 280f);
		System.out.println("testPrTlcl testCaseNum=" + testCaseNum +
				"c: Input:temperature in C, dewpoint in C= -GempakConstants.RMISSD -238" +
				" Output:Temperature (K) at the lifted Condensation Level LCL=" + tlcl);
		assertEquals (GempakConstants.RMISSD, tlcl);
		tlcl= PRLibrary.prTlcl(223f, GempakConstants.RMISSD);
		System.out.println("testPrTlcl testCaseNum=" + testCaseNum +
				"d: Input:temperature in C, dewpoint in C= 223 GempakConstants.RMISSD" +
				" Output:Temperature (K) at the lifted Condensation Level LCL=" + tlcl);
		assertEquals (GempakConstants.RMISSD, tlcl);
		tlcl= PRLibrary.prTlcl(140f, 130f);
		System.out.println("testPrTlcl testCaseNum=" + testCaseNum +
				"e: Input:temperature in C, dewpoint in C= 140 130" +
				" Output:Temperature (K) at the lifted Condensation Level LCL=" + tlcl);
		assertEquals (399.497803f, tlcl);
		tlcl= PRLibrary.prTlcl(130f, 140f);
		System.out.println("testPrTlcl testCaseNum=" + testCaseNum +
				"f: Input:temperature in C, dewpoint in C= 130 140" +
				" Output:Temperature (K) at the lifted Condensation Level LCL=" + tlcl);
		assertEquals (417.899945f, tlcl);
		tlcl= PRLibrary.prTlcl(-259f, -230f);
		System.out.println("testPrTlcl testCaseNum=" + testCaseNum +
				"g: Input:temperature in C, dewpoint in C= -259 -230" +
				" Output:Temperature (K) at the lifted Condensation Level LCL=" + tlcl);
		assertEquals (43.3760796f, tlcl);
		tlcl= PRLibrary.prTlcl(-271f, -274f);
		System.out.println("testPrTlcl testCaseNum=" + testCaseNum +
				"h: Input:temperature in C, dewpoint in C= -271 -274" +
				" Output:Temperature (K) at the lifted Condensation Level LCL=" + tlcl);
		assertEquals (GempakConstants.RMISSD, tlcl);
		tlcl= PRLibrary.prTlcl(-274f, -273f);
		System.out.println("testPrTlcl testCaseNum=" + testCaseNum +
				"i: Input:temperature in C, dewpoint in C= -274 -273" +
				" Output:Temperature (K) at the lifted Condensation Level LCL=" + tlcl);
		assertEquals (GempakConstants.RMISSD, tlcl);
		tlcl= PRLibrary.prTlcl(98f, 95f);
		System.out.println("testPrTlcl testCaseNum=" + testCaseNum +
				"j: Input:temperature in C, dewpoint in C= -98 95" +
				" Output:Temperature (K) at the lifted Condensation Level LCL=" + tlcl);
		assertEquals (367.164642, tlcl);
		tlcl= PRLibrary.prTlcl(0f, 1f);
		System.out.println("testPrTlcl testCaseNum=" + testCaseNum +
				"k: Input:temperature in C, dewpoint in C= 0 1" +
				" Output:Temperature (K) at the lifted Condensation Level LCL=" + tlcl);
		assertEquals (274.367615f, tlcl);
	}

	/**
	 * Test method for {@link gov.noaa.nws.ncep.gempak.parameterconversionlibrary.PRLibrary#prTmpk(float, float)}.
	 */
	@Test
	public void testPrTmpk() {
		testCaseNum++;
		float tmpk= PRLibrary.prTmpk(950f, 208f);
		System.out.println("testPrTmpk testCaseNum=" + testCaseNum +
				"a: Input:Pressure (mb), potential temperature (K)= 950 298" +
				" Output:Temperature (K)=" + tmpk);
		assertEquals (204.973938f, tmpk);
		tmpk= PRLibrary.prTmpk(1020f, -237f);
		System.out.println("testPrTmpk testCaseNum=" + testCaseNum +
				"b: Input:Pressure (mb), potential temperature (K)= 1020 -237" +
				" Output:Temperature (K)=" + tmpk);
		assertEquals (-238.344711f, tmpk);
		tmpk= PRLibrary.prTmpk(1035f, 0f);
		System.out.println("testPrTmpk testCaseNum=" + testCaseNum +
				"c: Input:Pressure (mb), potential temperature (K)= 1035 0" +
				" Output:Temperature (K)=" + tmpk);
		assertEquals (0f, tmpk);
		tmpk= PRLibrary.prTmpk(1000f, 135f);
		System.out.println("testPrTmpk testCaseNum=" + testCaseNum +
				"d: Input:Pressure (mb), potential temperature (K)= 1000 135" +
				" Output:Temperature (K)=" + tmpk);
		assertEquals (135f, tmpk);
		tmpk= PRLibrary.prTmpk(1000f, GempakConstants.RMISSD);
		System.out.println("testPrTmpk testCaseNum=" + testCaseNum +
				"d: Input:Pressure (mb), potential temperature (K)= 1000 GempakConstants.RMISSD" +
				" Output:Temperature (K)=" + tmpk);
		assertEquals (GempakConstants.RMISSD, tmpk);
		tmpk= PRLibrary.prTmpk(GempakConstants.RMISSD, 100f);
		System.out.println("testPrTmpk testCaseNum=" + testCaseNum +
				"d: Input:Pressure (mb), potential temperature (K)= GempakConstants.RMISSD 100" +
				" Output:Temperature (K)=" + tmpk);
		assertEquals (GempakConstants.RMISSD, tmpk);
	}

	/**
	 * Test method for {@link gov.noaa.nws.ncep.gempak.parameterconversionlibrary.PRLibrary#prTmst(float, float, float)}.
	 */
	@Test
	public void testPrTmst() {
		testCaseNum++;
		float tmst= PRLibrary.prTmst(220f, 1060f, 0f);
		System.out.println("testPrTmst testCaseNum=" + testCaseNum +
				"a: Input:Pressure (mb), potential temperature (K)= 220 1060 0" +
				" Output:Temperature (K)=" + tmst);
		assertEquals (223.582626f, tmst);
		tmst= PRLibrary.prTmst(200f, 980f, 195f);
		System.out.println("testPrTmst testCaseNum=" + testCaseNum +
				"b: Input:Pressure (mb), potential temperature (K)= 200 980 195" +
				" Output:Temperature (K)=" + tmst);
		assertEquals (198.844116f, tmst);
		tmst= PRLibrary.prTmst(10f, 950f, 10f);
		System.out.println("testPrTmst testCaseNum=" + testCaseNum +
				"c: Input:Pressure (mb), potential temperature (K)= 0 950 10" +
				" Output:Temperature (K)=" + tmst);
		assertEquals (GempakConstants.RMISSD, tmst);
		tmst= PRLibrary.prTmst(34f, 950f, 30f);
		System.out.println("testPrTmst testCaseNum=" + testCaseNum +
				"d: Input:Pressure (mb), potential temperature (K)= 34 950 30" +
				" Output:Temperature (K)=" + tmst);
		assertEquals (GempakConstants.RMISSD, tmst);
		tmst= PRLibrary.prTmst(34f, 920f, 0f);
		System.out.println("testPrTmst testCaseNum=" + testCaseNum +
				"e: Input:Pressure (mb), potential temperature (K)= 34 950 30" +
				" Output:Temperature (K)=" + tmst);
		assertEquals (33.199585f, tmst);
		tmst= PRLibrary.prTmst(34f, 1050f, 34f);
		System.out.println("testPrTmst testCaseNum=" + testCaseNum +
				"f: Input:Pressure (mb), potential temperature (K)= 34 1050 34" +
				" Output:Temperature (K)=" + tmst);
		assertEquals (34.4772797f, tmst);
		tmst= PRLibrary.prTmst(34f, 1050f, 34f);
		System.out.println("testPrTmst testCaseNum=" + testCaseNum +
				"g: Input:Pressure (mb), potential temperature (K)= 34 1050 34" +
				" Output:Temperature (K)=" + tmst);
		assertEquals (34.4772797f, tmst);
		tmst= PRLibrary.prTmst(33.35f, 980f, 0f);
		System.out.println("testPrTmst testCaseNum=" + testCaseNum +
				"h: Input:Pressure (mb), potential temperature (K)= 33.35 980 0" +
				" Output:Temperature (K)=" + tmst);
		assertEquals (33.1580505f, tmst);
		tmst= PRLibrary.prTmst(33.34f, 980f, 33f);
		System.out.println("testPrTmst testCaseNum=" + testCaseNum +
				"i: Input:Pressure (mb), potential temperature (K)= 33.34 980 33" +
				" Output:Temperature (K)=" + tmst);
		assertEquals (GempakConstants.RMISSD, tmst);
		tmst= PRLibrary.prTmst(75f, 995f, -1f);
		System.out.println("testPrTmst testCaseNum=" + testCaseNum +
				"j: Input:Pressure (mb), potential temperature (K)= 75 995 -1" +
				" Output:Temperature (K)=" + tmst);
		assertEquals (GempakConstants.RMISSD, tmst);
		tmst= PRLibrary.prTmst(75f, 995f, 0f);
		System.out.println("testPrTmst testCaseNum=" + testCaseNum +
				"k: Input:Pressure (mb), potential temperature (K)= 75 995 0" +
				" Output:Temperature (K)=" + tmst);
		assertEquals (74.8926697f, tmst);
		tmst= PRLibrary.prTmst(GempakConstants.RMISSD, 995f, -1f);
		System.out.println("testPrTmst testCaseNum=" + testCaseNum +
				"l: Input:Pressure (mb), potential temperature (K)= GempakConstants.RMISSD 995 -1" +
				" Output:Temperature (K)=" + tmst);
		assertEquals (GempakConstants.RMISSD, tmst);
		tmst= PRLibrary.prTmst(78f, GempakConstants.RMISSD,-1f);
		System.out.println("testPrTmst testCaseNum=" + testCaseNum +
				"m: Input:Pressure (mb), potential temperature (K)= 78 GempakConstants.RMISSD -1" +
				" Output:Temperature (K)=" + tmst);
		assertEquals (GempakConstants.RMISSD, tmst);
		tmst= PRLibrary.prTmst(78f,1000f, GempakConstants.RMISSD);
		System.out.println("testPrTmst testCaseNum=" + testCaseNum +
				"n: Input:Pressure (mb), potential temperature (K)= 78 100 GempakConstants.RMISSD" +
				" Output:Temperature (K)=" + tmst);
		assertEquals (GempakConstants.RMISSD, tmst);
	}

	/**
	 * Test method for {@link gov.noaa.nws.ncep.gempak.parameterconversionlibrary.PRLibrary#prMixr(float, float)}.
	 */
	@Test
	public void testPrMixr() {
		testCaseNum++;
		float mixr= PRLibrary.prMixr(40f, 950f);
		System.out.println("testPrMixr testCaseNum=" + testCaseNum +
				"a: Input:dewpoint (C), Pressure (mb)= 40 950" +
				" Output:Mixing ratio (g/kg)=" + mixr);
		assertEquals (52.7414246f, mixr);
		mixr= PRLibrary.prMixr(80f, 950f);
		System.out.println("testPrMixr testCaseNum=" + testCaseNum +
				"b: Input:dewpoint (C), Pressure (mb)= 80 950" +
				" Output:Mixing ratio (g/kg)=" + mixr);
		assertEquals (GempakConstants.RMISSD, mixr);
		mixr= PRLibrary.prMixr(90f, 1090f);
		System.out.println("testPrMixr testCaseNum=" + testCaseNum +
				"c: Input:dewpoint (C), Pressure (mb)= 90 1090" +
				" Output:Mixing ratio (g/kg)=" + mixr);
		assertEquals (GempakConstants.RMISSD, mixr);
		mixr= PRLibrary.prMixr(60f, 950);
		System.out.println("testPrMixr testCaseNum=" + testCaseNum +
				"d: Input:dewpoint (C), Pressure (mb)= 60 950" +
				" Output:Mixing ratio (g/kg)=" + mixr);
		assertEquals (167.844528f, mixr);
		mixr= PRLibrary.prMixr(60f, 1050);
		System.out.println("testPrMixr testCaseNum=" + testCaseNum +
				"e: Input:dewpoint (C), Pressure (mb)= 60 1050" +
				" Output:Mixing ratio (g/kg)=" + mixr);
		assertEquals (148.123169f, mixr);
		mixr= PRLibrary.prMixr(GempakConstants.RMISSD, 950f);
		System.out.println("testPrMixr testCaseNum=" + testCaseNum +
				"f: Input:dewpoint (C), Pressure (mb)= GempakConstants.RMISSD 950" +
				" Output:Mixing ratio (g/kg)=" + mixr);
		assertEquals (GempakConstants.RMISSD, mixr);
		mixr= PRLibrary.prMixr(GempakConstants.RMISSD, GempakConstants.RMISSD);
		System.out.println("testPrMixr testCaseNum=" + testCaseNum +
				"g: Input:dewpoint (C), Pressure (mb)= GempakConstants.RMISSD GempakConstants.RMISSD" +
				" Output:Mixing ratio (g/kg)=" + mixr);
		assertEquals (GempakConstants.RMISSD, mixr);
		mixr= PRLibrary.prMixr(60f, GempakConstants.RMISSD);
		System.out.println("testPrMixr testCaseNum=" + testCaseNum +
				"h: Input:dewpoint (C), Pressure (mb)= 60 GempakConstants.RMISSD" +
				" Output:Mixing ratio (g/kg)=" + mixr);
		assertEquals (GempakConstants.RMISSD, mixr);
	}

	/**
	 * Test method for {@link gov.noaa.nws.ncep.gempak.parameterconversionlibrary.PRLibrary#prPany(float, float)}.
	 */
	@Test
	public void testPrPany() {
		testCaseNum++;
		float pany= PRLibrary.prPany(950f, 920f);
		System.out.println("testPrPany testCaseNum=" + testCaseNum +
				"a: Input:Altimeter (mb), Sea LevelPressure (mb)= 950 920" +
				" Output:PMSL or ALTM (K)=" + pany);
		assertEquals (920f, pany);
		pany= PRLibrary.prPany(980f, GempakConstants.RMISSD);
		System.out.println("testPrPany testCaseNum=" + testCaseNum +
				"b: Input:Altimeter (mb), Sea LevelPressure (mb)= 980 GempakConstants.RMISSD" +
				" Output:PMSL or ALTM (K)=" + pany);
		assertEquals (980f, pany);
		pany= PRLibrary.prPany(GempakConstants.RMISSD, GempakConstants.RMISSD);
		System.out.println("testPrPany testCaseNum=" + testCaseNum +
				"c: Input:Altimeter (mb), Sea LevelPressure (mb)= GempakConstants.RMISSD GempakConstants.RMISSD" +
				" Output:PMSL or ALTM (K)=" + pany);
		assertEquals (GempakConstants.RMISSD, pany);
		pany= PRLibrary.prPany(GempakConstants.RMISSD, 1010f);
		System.out.println("testPrPany testCaseNum=" + testCaseNum +
				"c: Input:Altimeter (mb), Sea LevelPressure (mb)= GempakConstants.RMISSD 1010f" +
				" Output:PMSL or ALTM (K)=" + pany);
		assertEquals (1010f, pany);
	}

	/**
	 * Test method for {@link gov.noaa.nws.ncep.gempak.parameterconversionlibrary.PRLibrary#prPkdd(float)}.
	 */
	@Test
	public void testPrPkdd() {
		testCaseNum++;
		float pkdd= PRLibrary.prPkdd(950f);
		System.out.println("testPrPkdd testCaseNum=" + testCaseNum +
				"a: Input:Packed speed and direction= 950" +
				" Output:Wind Direction in degrees=" + pkdd);
		assertEquals (5f, pkdd);
		pkdd= PRLibrary.prPkdd(10500f);
		System.out.println("testPrPkdd testCaseNum=" + testCaseNum +
				"b Input:Packed speed and direction= 10500" +
				" Output:Wind Direction in degrees=" + pkdd);
		assertEquals (105f, pkdd);
		pkdd=PRLibrary.prPkdd(78921f);
		System.out.println("testPrPkdd testCaseNum=" + testCaseNum +
				"c: Input:Packed speed and direction= 78921" +
				" Output:Wind Direction in degrees=" + pkdd);
		assertEquals (425f, pkdd);
		pkdd=PRLibrary.prPkdd(38967f);
		System.out.println("testPrPkdd testCaseNum=" + testCaseNum +
				"d: Input:Packed speed and direction= 38967" +
				" Output:Wind Direction in degrees=" + pkdd);
		assertEquals (25f, pkdd);
		pkdd=PRLibrary.prPkdd(23452f);
		System.out.println("testPrPkdd testCaseNum=" + testCaseNum +
				"e: Input:Packed speed and direction= 23452" +
				" Output:Wind Direction in degrees=" + pkdd);
		assertEquals (230f, pkdd);
		pkdd=PRLibrary.prPkdd(GempakConstants.RMISSD);
		System.out.println("testPrPkdd testCaseNum=" + testCaseNum +
				"e: Input:Packed speed and direction= GempakConstants.RMISSD" +
				" Output:Wind Direction in degrees=" + pkdd);
		assertEquals (GempakConstants.RMISSD, pkdd);
	}

	/**
	 * Test method for {@link gov.noaa.nws.ncep.gempak.parameterconversionlibrary.PRLibrary#prPkss(float)}.
	 */
	@Test
	public void testPrPkss() {
		testCaseNum++;
		float pkss= PRLibrary.prPkss(18678f);
		System.out.println("testPrPkss testCaseNum=" + testCaseNum +
				"a: Input:Packed speed and direction DDFFF= 18678" +
				" Output:Wind Speed in knots=" + pkss);
		assertEquals (178f, pkss);
		pkss= PRLibrary.prPkss(678f);
		System.out.println("testPrPkss testCaseNum=" + testCaseNum +
				"b: Input:Packed speed and direction DDFFF= 678" +
				" Output:Wind Speed in knots=" + pkss);
		assertEquals (178f, pkss);
		pkss= PRLibrary.prPkss(678f);
		System.out.println("testPrPkss testCaseNum=" + testCaseNum +
				"c: Input:Packed speed and direction DDFFF= 678" +
				" Output:Wind Speed in knots=" + pkss);
		assertEquals (178f, pkss);
		pkss= PRLibrary.prPkss(35999f);
		System.out.println("testPrPkss testCaseNum=" + testCaseNum +
				"d: Input:Packed speed and direction DDFFF= 35999" +
				" Output:Wind Speed in knots=" + pkss);
		assertEquals (499f, pkss);
		pkss= PRLibrary.prPkss(GempakConstants.RMISSD);
		System.out.println("testPrPkss testCaseNum=" + testCaseNum +
				"e: Input:Packed speed and direction DDFFF= GempakConstants.RMISSD" +
				" Output:Wind Speed in knots=" + pkss);
		assertEquals (GempakConstants.RMISSD, pkss);
	}

	/**
	 * Test method for {@link gov.noaa.nws.ncep.gempak.parameterconversionlibrary.PRLibrary#prPlcl(float, float, float)}.
	 *
	@Test
	public void testPrPlcl() {
		testCaseNum++;
		float plcl= PRLibrary.prPlcl(35f, 970f, 230f);
		System.out.println("testPrPlcl testCaseNum=" + testCaseNum +
				"a: Input:TMPC, PRES,(mb), TLCL (K)= 35 970 230" +
				" Output: LCL pressure in mb=" + plcl);
		assertEquals (348.458956f, plcl);
		plcl= PRLibrary.prPlcl(90f, 1030f, 285f);
		System.out.println("testPrPlcl testCaseNum=" + testCaseNum +
				"b: Input:TMPC, PRES,(mb), TLCL (K)= 90 1030 285" +
				" Output: LCL pressure in mb=" + plcl);
		assertEquals (441.055359f, plcl);
		plcl= PRLibrary.prPlcl(55f, 1020f, 240f);
		System.out.println("testPrPlcl testCaseNum=" + testCaseNum +
				"c: Input:TMPC, PRES,(mb), TLCL (K)= 55 1020 240" +
				" Output: LCL pressure in mb=" + plcl);
		assertEquals (341.26059f, plcl);
		plcl= PRLibrary.prPlcl(55f, 1020f, 240f);
		System.out.println("testPrPlcl testCaseNum=" + testCaseNum +
				"d: Input:TMPC, PRES,(mb), TLCL (K)= 55 1020 240" +
				" Output: LCL pressure in mb=" + plcl);
		assertEquals (341.26059f, plcl);
		plcl= PRLibrary.prPlcl(20f, 1090f, 5f);
		System.out.println("testPrPlcl testCaseNum=" + testCaseNum +
				"e: Input:TMPC, PRES,(mb), TLCL (K)= 20 1090 5" +
				" Output: LCL pressure in mb=" + plcl);
		assertEquals (.000706329418f, plcl);
		plcl= PRLibrary.prPlcl(0f, 1025f, 260f);
		System.out.println("testPrPlcl testCaseNum=" + testCaseNum +
				"f: Input:TMPC, PRES,(mb), TLCL (K)= 0 1025 260" +
				" Output: LCL pressure in mb=" + plcl);
		assertEquals (862.435059f, plcl);
		plcl= PRLibrary.prPlcl(GempakConstants.RMISSD, 1025f, 260f);
		System.out.println("testPrPlcl testCaseNum=" + testCaseNum +
				"f: Input:TMPC, PRES,(mb), TLCL (K)= GempakConstants.RMISSD 1025 260" +
				" Output: LCL pressure in mb=" + plcl);
		assertEquals (GempakConstants.RMISSD, plcl);
		plcl= PRLibrary.prPlcl(35f, GempakConstants.RMISSD, 260f);
		System.out.println("testPrPlcl testCaseNum=" + testCaseNum +
				"g: Input:TMPC, PRES,(mb), TLCL (K)= 35 GempakConstants.RMISSD 260" +
				" Output: LCL pressure in mb=" + plcl);
		assertEquals (GempakConstants.RMISSD, plcl);
		plcl= PRLibrary.prPlcl(35f, 1020f, GempakConstants.RMISSD);
		System.out.println("testPrPlcl testCaseNum=" + testCaseNum +
				"h: Input:TMPC, PRES,(mb), TLCL (K)= 35 1020 GempakConstants.RMISSD" +
				" Output: LCL pressure in mb=" + plcl);
		assertEquals (GempakConstants.RMISSD, plcl);
	}

	/**
	 * Test method for {@link gov.noaa.nws.ncep.gempak.parameterconversionlibrary.PRLibrary#prpmsl(float, float, float, float)}.
	 */
	@Test
	public void testPrPmsl() {
		testCaseNum++;
		float pmsl= PRLibrary.prPmsl(990f, 35f, 34f, 400f);
		System.out.println("testPrPmsl testCaseNum=" + testCaseNum +
				"a: Input:PRES (mb), TMPC,  DWPC, SELV (m)= 990 35 34 400" +
				" Output:Mean sea level pressure in mb=" + pmsl);
		assertEquals (1033.76868f, pmsl);
		pmsl= PRLibrary.prPmsl(990f, 35f, 34f, 4000f);
		System.out.println("testPrPmsl testCaseNum=" + testCaseNum +
				"b: Input:PRES (mb), TMPC,  DWPC, SELV (m)= 990 35 34 4000" +
				" Output:Mean sea level pressure in mb=" + pmsl);
		assertEquals (1502.4718f, pmsl);
		pmsl= PRLibrary.prPmsl(990f, 35f, 34f, 4000f);
		System.out.println("testPrPmsl testCaseNum=" + testCaseNum +
				"c: Input:PRES (mb), TMPC,  DWPC, SELV (m)= 990 35 34 4000" +
				" Output:Mean sea level pressure in mb=" + pmsl);
		assertEquals (1502.4718f, pmsl);
		pmsl= PRLibrary.prPmsl(985f, 75f, 75f, 0f);
		System.out.println("testPrPmsl testCaseNum=" + testCaseNum +
				"d: Input:PRES (mb), TMPC,  DWPC, SELV (m)= 985 75 75 0" +
				" Output:Mean sea level pressure in mb=" + pmsl);
		assertEquals (985f, pmsl);
		pmsl= PRLibrary.prPmsl(995f, 0f, 10f, 120f);
		System.out.println("testPrPmsl testCaseNum=" + testCaseNum +
				"e: Input:PRES (mb), TMPC,  DWPC, SELV (m)= 995 0 10 120" +
				" Output:Mean sea level pressure in mb=" + pmsl);
		assertEquals (1009.95367f, pmsl);
		pmsl= PRLibrary.prPmsl(1010f, 65f, 65f, 120f);
		System.out.println("testPrPmsl testCaseNum=" + testCaseNum +
				"f: Input:PRES (mb), TMPC,  DWPC, SELV (m)= 1010 65 65 120" +
				" Output:Mean sea level pressure in mb=" + pmsl);
		assertEquals (1021.12952f, pmsl);
		pmsl= PRLibrary.prPmsl(1010f, 65f, 60f, 120f);
		System.out.println("testPrPmsl testCaseNum=" + testCaseNum +
				"g: Input:PRES (mb), TMPC,  DWPC, SELV (m)= 1010 65 60 120" +
				" Output:Mean sea level pressure in mb=" + pmsl);
		assertEquals (1021.37073f, pmsl);
		pmsl= PRLibrary.prPmsl(1010f, 65f, 60f, 900f);
		System.out.println("testPrPmsl testCaseNum=" + testCaseNum +
				"h: Input:PRES (mb), TMPC,  DWPC, SELV (m)= 1010 65 60 900" +
				" Output:Mean sea level pressure in mb=" + pmsl);
		assertEquals (1097.83228f, pmsl);
		pmsl= PRLibrary.prPmsl(GempakConstants.RMISSD, 65f, 60f, 900f);
		System.out.println("testPrPmsl testCaseNum=" + testCaseNum +
				"i: Input:PRES (mb), TMPC,  DWPC, SELV (m)= GempakConstants.RMISSD 65 60 900" +
				" Output:Mean sea level pressure in mb=" + pmsl);
		assertEquals (GempakConstants.RMISSD, pmsl);
		pmsl= PRLibrary.prPmsl(1015f, GempakConstants.RMISSD, 60f, 900f);
		System.out.println("testPrPmsl testCaseNum=" + testCaseNum +
				"j: Input:PRES (mb), TMPC,  DWPC, SELV (m)= 1015 GempakConstants.RMISSD 60 900" +
				" Output:Mean sea level pressure in mb=" + pmsl);
		assertEquals (GempakConstants.RMISSD, pmsl);
		pmsl= PRLibrary.prPmsl(1015f, 61f, GempakConstants.RMISSD, 900f);
		System.out.println("testPrPmsl testCaseNum=" + testCaseNum +
				"k: Input:PRES (mb), TMPC,  DWPC, SELV (m)= 1015 61 GempakConstants.RMISSD 900" +
				" Output:Mean sea level pressure in mb=" + pmsl);
		assertEquals (GempakConstants.RMISSD, pmsl);
		pmsl= PRLibrary.prPmsl(1015f, 61f, 62f, GempakConstants.RMISSD);
		System.out.println("testPrPmsl testCaseNum=" + testCaseNum +
				"l: Input:PRES (mb), TMPC,  DWPC, SELV (m)= 1015 61 62 GempakConstants.RMISSD" +
				" Output:Mean sea level pressure in mb=" + pmsl);
		assertEquals (GempakConstants.RMISSD, pmsl);
	}

	/**
	 * Test method for {@link gov.noaa.nws.ncep.gempak.parameterconversionlibrary.PRLibrary#prPmst(float, float)}.
	 */
	@Test
	public void testPrPmst() {
		testCaseNum++;
		float pmst= PRLibrary.prPmst(300f, 315f);
		System.out.println("testPrPmst testCaseNum=" + testCaseNum +
				"a: Input:Equivalent potential temp (K), Parcel temperature (K)= 300 315" +
				" Output:Pressure (mb)=" + pmst);
		assertEquals (2322.68823f, pmst);
		pmst= PRLibrary.prPmst(200f, 205f);
		System.out.println("testPrPmst testCaseNum=" + testCaseNum +
				"b: Input:Equivalent potential temp (K), Parcel temperature (K)= 200 205" +
				" Output:Pressure (mb)=" + pmst);
		assertEquals (1090.46277f, pmst);
		pmst= PRLibrary.prPmst(375f, 1f);
		System.out.println("testPrPmst testCaseNum=" + testCaseNum +
				"c: Input:Equivalent potential temp (K), Parcel temperature (K)= 375 1" +
				" Output:Pressure (mb)=" + pmst);
		assertEquals (GempakConstants.RMISSD, pmst);
		pmst= PRLibrary.prPmst(430f, -100f);
		System.out.println("testPrPmst testCaseNum=" + testCaseNum +
				"d: Input:Equivalent potential temp (K), Parcel temperature (K)= 430 -100" +
				" Output:Pressure (mb)=" + pmst);
		assertEquals (GempakConstants.RMISSD, pmst);
		pmst= PRLibrary.prPmst(-200f, 500f);
		System.out.println("testPrPmst testCaseNum=" + testCaseNum +
				"d: Input:Equivalent potential temp (K), Parcel temperature (K)= -200 500" +
				" Output:Pressure (mb)=" + pmst);
		assertEquals (GempakConstants.RMISSD, pmst);
		pmst= PRLibrary.prPmst(0f, 10f);
		System.out.println("testPrPmst testCaseNum=" + testCaseNum +
				"d: Input:Equivalent potential temp (K), Parcel temperature (K)= 0 10" +
				" Output:Pressure (mb)=" + pmst);
		assertEquals (GempakConstants.RMISSD, pmst);
	}

	/**
	 * Test method for {@link gov.noaa.nws.ncep.gempak.parameterconversionlibrary.PRLibrary#prPr24(float, float, float, float)}.
	 */
	@Test
	public void testPrPr24() {
		testCaseNum++;
		float pr24= PRLibrary.prPr24(-2f, -1f, 0f, -2f);
		System.out.println("testPrPr24 testCaseNum=" + testCaseNum +
				"a: Input:p01, p02, p03, p04= -2 -1 0 -2" +
				" Output:PR24 the 24-hour precipitation=" + pr24);
		assertEquals (0f, pr24);
		pr24= PRLibrary.prPr24(-2f, -1f, -3f, -2f);
		System.out.println("testPrPr24 testCaseNum=" + testCaseNum +
				"b: Input:p01, p02, p03, p04= -2 -1 -3 -2" +
				" Output:PR24 the 24-hour precipitation=" + pr24);
		assertEquals (GempakConstants.RMISSD, pr24);
		pr24= PRLibrary.prPr24(-1f, 0f, 3f, 2f);
		System.out.println("testPrPr24 testCaseNum=" + testCaseNum +
				"c: Input:p01, p02, p03, p04= -1 0 3 2" +
				" Output:PR24 the 24-hour precipitation=" + pr24);
		assertEquals (5f, pr24);
		pr24= PRLibrary.prPr24(GempakConstants.RMISSD, 1f, 2f, 3f);
		System.out.println("testPrPr24 testCaseNum=" + testCaseNum +
				"d: Input:p01, p02, p03, p04= GempakConstants.RMISSD 1 2 3" +
				" Output:PR24 the 24-hour precipitation=" + pr24);
		assertEquals (6f, pr24);
		pr24= PRLibrary.prPr24(0f, 1f, 3f, 2f);
		System.out.println("testPrPr24 testCaseNum=" + testCaseNum +
				"e: Input:p01, p02, p03, p04= 0 1 3 2" +
				" Output:PR24 the 24-hour precipitation=" + pr24);
		assertEquals (6f, pr24);
	}

	/**
	 * Test method for {@link gov.noaa.nws.ncep.gempak.parameterconversionlibrary.PRLibrary#prPr6x(float, float, float, float)}.
	 */
	@Test
	public void testPrPr6x() {
		testCaseNum++;
		float pr6x= PRLibrary.prPr6x(-2f, -1f, 0f, -2f);
		System.out.println("testPrPr6x testCaseNum=" + testCaseNum +
				"a: Input:p01, p02, p03, p04= -2 -1 0 -2" +
				" Output:PR6x the max precip amount=" + pr6x);
		assertEquals (0f, pr6x);
		pr6x= PRLibrary.prPr6x(2f, 1f, 0f, 2f);
		System.out.println("testPrPr6x testCaseNum=" + testCaseNum +
				"b: Input:p01, p02, p03, p04= 2 1 0 2" +
				" Output:PR6x the max precip amount=" + pr6x);
		assertEquals (2f, pr6x);
		pr6x= PRLibrary.prPr6x(3f, 5f, 5f, 5f);
		System.out.println("testPrPr6x testCaseNum=" + testCaseNum +
				"c: Input:p01, p02, p03, p04= 3 5 5 5" +
				" Output:PR6x the max precip amount=" + pr6x);
		assertEquals (5f, pr6x);
		pr6x= PRLibrary.prPr6x(3.5f, 3.7f, -1f, 1f);
		System.out.println("testPrPr6x testCaseNum=" + testCaseNum +
				"d: Input:p01, p02, p03, p04= 3.5 3.7 -1 1" +
				" Output:PR6x the max precip amount=" + pr6x);
		assertEquals (3.7f, pr6x);
		pr6x= PRLibrary.prPr6x(2f, 5f, 1f, 4f);
		System.out.println("testPrPr6x testCaseNum=" + testCaseNum +
				"e: Input:p01, p02, p03, p04= 2 5 1 4" +
				" Output:PR6x the max precip amount=" + pr6x);
		assertEquals (5f, pr6x);
		pr6x= PRLibrary.prPr6x(GempakConstants.RMISSD, GempakConstants.RMISSD, GempakConstants.RMISSD, GempakConstants.RMISSD);
		System.out.println("testPrPr6x testCaseNum=" + testCaseNum +
				"e: Input:p01, p02, p03, p04= GempakConstants.RMISSD, GempakConstants.RMISSD, GempakConstants.RMISSD, GempakConstants.RMISSD" +
				" Output:PR6x the max precip amount=" + pr6x);
		assertEquals (GempakConstants.RMISSD, pr6x);
		pr6x= PRLibrary.prPr6x(2.75f, GempakConstants.RMISSD, GempakConstants.RMISSD, GempakConstants.RMISSD);
		System.out.println("testPrPr6x testCaseNum=" + testCaseNum +
				"f: Input:p01, p02, p03, p04= 2.75f, GempakConstants.RMISSD, GempakConstants.RMISSD, GempakConstants.RMISSD" +
				" Output:PR6x the max precip amount=" + pr6x);
		assertEquals (2.75f, pr6x);
	}

	/**
	 * Test method for {@link gov.noaa.nws.ncep.gempak.parameterconversionlibrary.PRLibrary#prPres(float, float)}.
	 */
	@Test
	public void testPrPres() {
		testCaseNum++;
		float pres= PRLibrary.prPres(30f, 285f);
		System.out.println("testPrPres testCaseNum=" + testCaseNum +
				"a: Input:TMPC, THTA (K)= 30 285" +
				" Output:Pres the max precip amount=" + pres);
		assertEquals (1241.20776f, pres);
		pres= PRLibrary.prPres(40f, 310f);
		System.out.println("testPrPres testCaseNum=" + testCaseNum +
				"b: Input:TMPC, THTA (K)= 40 310" +
				" Output:Pres the max precip amount=" + pres);
		assertEquals (1036.01843f, pres);
		pres= PRLibrary.prPres(GempakConstants.RMISSD, 310f);
		System.out.println("testPrPres testCaseNum=" + testCaseNum +
				"c: Input:TMPC, THTA (K)= GempakConstants.RMISSD 310" +
				" Output:Pres the max precip amount=" + pres);
		assertEquals (GempakConstants.RMISSD, pres);
		pres= PRLibrary.prPres(25f, GempakConstants.RMISSD);
		System.out.println("testPrPres testCaseNum=" + testCaseNum +
				"d: Input:TMPC, THTA (K)= 25 GempakConstants.RMISSD" +
				" Output:Pres the max precip amount=" + pres);
		assertEquals (GempakConstants.RMISSD, pres);
		pres= PRLibrary.prPres(50f, 320f);
		System.out.println("testPrPres testCaseNum=" + testCaseNum +
				"a: Input:TMPC, THTA (K)= 50 320" +
				" Output:Pres the max precip amount=" + pres);
		assertEquals (1034.87903f, pres);
	}

	/**
	 * Test method for {@link gov.noaa.nws.ncep.gempak.parameterconversionlibrary.PRLibrary#prPspd(float, float)}.
	 */
	@Test
	public void testPrPspd() {
		testCaseNum++;
		float pspd= PRLibrary.prPspd(182f, 45f);
		System.out.println("testPrPspd testCaseNum=" + testCaseNum +
				"a: Input:DRCT, SPED= 182 45" +
				" Output:Packed wind speed and direction in the form DDFFF=" + pspd);
		assertEquals (18045f, pspd);
		pspd= PRLibrary.prPspd(183f, 45f);
		System.out.println("testPrPspd testCaseNum=" + testCaseNum +
				"b: Input:DRCT, SPED= 183 45" +
				" Output:Packed wind speed and direction in the form DDFFF=" + pspd);
		assertEquals (18545f, pspd);
		pspd= PRLibrary.prPspd(187f, 45f);
		System.out.println("testPrPspd testCaseNum=" + testCaseNum +
				"c: Input:DRCT, SPED= 187 45" +
				" Output:Packed wind speed and direction in the form DDFFF=" + pspd);
		assertEquals (18545f, pspd);
		pspd= PRLibrary.prPspd(188f, 45f);
		System.out.println("testPrPspd testCaseNum=" + testCaseNum +
				"d: Input:DRCT, SPED= 188 45" +
				" Output:Packed wind speed and direction in the form DDFFF=" + pspd);
		assertEquals (19045f, pspd);
		pspd= PRLibrary.prPspd(243f, 37f);
		System.out.println("testPrPspd testCaseNum=" + testCaseNum +
				"e: Input:DRCT, SPED= 243 37" +
				" Output:Packed wind speed and direction in the form DDFFF=" + pspd);
		assertEquals (24537f, pspd);
		pspd= PRLibrary.prPspd(242f, 37f);
		System.out.println("testPrPspd testCaseNum=" + testCaseNum +
				"f: Input:DRCT, SPED= 242 37" +
				" Output:Packed wind speed and direction in the form DDFFF=" + pspd);
		assertEquals (24037f, pspd);
		pspd= PRLibrary.prPspd(237f, 159f);
		System.out.println("testPrPspd testCaseNum=" + testCaseNum +
				"g: Input:DRCT, SPED= 237 159" +
				" Output:Packed wind speed and direction in the form DDFFF=" + pspd);
		assertEquals (23659f, pspd);
		pspd= PRLibrary.prPspd(238f, 159f);
		System.out.println("testPrPspd testCaseNum=" + testCaseNum +
				"h: Input:DRCT, SPED= 238 159" +
				" Output:Packed wind speed and direction in the form DDFFF=" + pspd);
		assertEquals (23159f, pspd);
		pspd= PRLibrary.prPspd(3f, 56f);
		System.out.println("testPrPspd testCaseNum=" + testCaseNum +
				"h: Input:DRCT, SPED= 3 56" +
				" Output:Packed wind speed and direction in the form DDFFF=" + pspd);
		assertEquals (556f, pspd);
		pspd= PRLibrary.prPspd(1f, 52f);
		System.out.println("testPrPspd testCaseNum=" + testCaseNum +
				"i: Input:DRCT, SPED= 1 52" +
				" Output:Packed wind speed and direction in the form DDFFF=" + pspd);
		assertEquals (52f, pspd);
		pspd= PRLibrary.prPspd(GempakConstants.RMISSD, 52f);
		System.out.println("testPrPspd testCaseNum=" + testCaseNum +
				"j: Input:DRCT, SPED= GempakConstants.RMISSD 52" +
				" Output:Packed wind speed and direction in the form DDFFF=" + pspd);
		assertEquals (GempakConstants.RMISSD, pspd);
		pspd= PRLibrary.prPspd(94f, GempakConstants.RMISSD);
		System.out.println("testPrPspd testCaseNum=" + testCaseNum +
				"k: Input:DRCT, SPED= 94 GempakConstants.RMISSD" +
				" Output:Packed wind speed and direction in the form DDFFF=" + pspd);
		assertEquals (GempakConstants.RMISSD, pspd);
	}

	/**
	 * Test method for {@link gov.noaa.nws.ncep.gempak.parameterconversionlibrary.PRLibrary#prPtsy(float)}.
	 */
	@Test
	public void testPrPtsy() {
		testCaseNum++;
		float ptsy= PRLibrary.prPtsy(832f);
		System.out.println("testPrPtsy testCaseNum=" + testCaseNum +
				"a: Input:Pressure tendency info P03D= 832" +
				" Output:Pressure tendency symbol code=" + ptsy);
		assertEquals (999f, ptsy);
		ptsy= PRLibrary.prPtsy(8325f);
		System.out.println("testPrPtsy testCaseNum=" + testCaseNum +
				"b: Input:Pressure tendency info P03D= 8325" +
				" Output:Pressure tendency symbol code=" + ptsy);
		assertEquals (8999f, ptsy);
		ptsy= PRLibrary.prPtsy(9001f);
		System.out.println("testPrPtsy testCaseNum=" + testCaseNum +
				"c: Input:Pressure tendency info P03D= 9001" +
				" Output:Pressure tendency symbol code=" + ptsy);
		assertEquals (GempakConstants.RMISSD, ptsy);
		ptsy= PRLibrary.prPtsy(7985f);
		System.out.println("testPrPtsy testCaseNum=" + testCaseNum +
				"d: Input:Pressure tendency info P03D= 7985" +
				" Output:Pressure tendency symbol code=" + ptsy);
		assertEquals (7999f, ptsy);
		ptsy= PRLibrary.prPtsy(2087f);
		System.out.println("testPrPtsy testCaseNum=" + testCaseNum +
				"e: Input:Pressure tendency info P03D= 2087" +
				" Output:Pressure tendency symbol code=" + ptsy);
		assertEquals (2999f, ptsy);
		ptsy= PRLibrary.prPtsy(1840f);
		System.out.println("testPrPtsy testCaseNum=" + testCaseNum +
				"f: Input:Pressure tendency info P03D= 1840" +
				" Output:Pressure tendency symbol code=" + ptsy);
		assertEquals (1999f, ptsy);
		ptsy= PRLibrary.prPtsy(0f);
		System.out.println("testPrPtsy testCaseNum=" + testCaseNum +
				"g: Input:Pressure tendency info P03D= 0" +
				" Output:Pressure tendency symbol code=" + ptsy);
		assertEquals (999f, ptsy);
		ptsy= PRLibrary.prPtsy(-GempakConstants.RMISSD);
		System.out.println("testPrPtsy testCaseNum=" + testCaseNum +
				"h: Input:Pressure tendency info P03D= GempakConstants.RMISSD" +
				" Output:Pressure tendency symbol code=" + ptsy);
		assertEquals (GempakConstants.RMISSD, ptsy);
		
	}

	/**
	 * Test method for {@link gov.noaa.nws.ncep.gempak.parameterconversionlibrary.PRLibrary#prPwao(float)}.
	 */
	@Test
	public void testPrPwao() {
		testCaseNum++;
		float pwao= PRLibrary.prPwao(2f);
		System.out.println("testPrPwao testCaseNum=" + testCaseNum +
				"a: Input:Auto station past weather code (WMO code table 4531)= 2" +
				" Output:Manned station past code (WMO code table 4561)=" + pwao);
		assertEquals (3f, pwao);
		pwao= PRLibrary.prPwao(9f);
		System.out.println("testPrPwao testCaseNum=" + testCaseNum +
				"b: Input:Auto station past weather code (WMO code table 4531)= 9" +
				" Output:Manned station past code (WMO code table 4561)=" + pwao);
		assertEquals (9f, pwao);
		pwao= PRLibrary.prPwao(3f);
		System.out.println("testPrPwao testCaseNum=" + testCaseNum +
				"c: Input:Auto station past weather code (WMO code table 4531)= 3" +
				" Output:Manned station past code (WMO code table 4561)=" + pwao);
		assertEquals (4f, pwao);
		pwao= PRLibrary.prPwao(1f);
		System.out.println("testPrPwao testCaseNum=" + testCaseNum +
				"d: Input:Auto station past weather code (WMO code table 4531)= 1" +
				" Output:Manned station past code (WMO code table 4561)=" + pwao);
		assertEquals (GempakConstants.RMISSD, pwao);
		pwao= PRLibrary.prPwao(0f);
		System.out.println("testPrPwao testCaseNum=" + testCaseNum +
				"e: Input:Auto station past weather code (WMO code table 4531)= 0" +
				" Output:Manned station past code (WMO code table 4561)=" + pwao);
		assertEquals (GempakConstants.RMISSD, pwao);
		pwao= PRLibrary.prPwao(10f);
		System.out.println("testPrPwao testCaseNum=" + testCaseNum +
				"f: Input:Auto station past weather code (WMO code table 4531)= 10" +
				" Output:Manned station past code (WMO code table 4561)=" + pwao);
		assertEquals (GempakConstants.RMISSD, pwao);
		pwao= PRLibrary.prPwao(-1f);
		System.out.println("testPrPwao testCaseNum=" + testCaseNum +
				"f: Input:Auto station past weather code (WMO code table 4531)= -1" +
				" Output:Manned station past code (WMO code table 4561)=" + pwao);
		assertEquals (GempakConstants.RMISSD, pwao);
		pwao= PRLibrary.prPwao(1090f);
		System.out.println("testPrPwao testCaseNum=" + testCaseNum +
				"f: Input:Auto station past weather code (WMO code table 4531)= 1090" +
				" Output:Manned station past code (WMO code table 4561)=" + pwao);
		assertEquals (GempakConstants.RMISSD, pwao);
		pwao= PRLibrary.prPwao(GempakConstants.RMISSD);
		System.out.println("testPrPwao testCaseNum=" + testCaseNum +
				"f: Input:Auto station past weather code (WMO code table 4531)= GempakConstants.RMISSD" +
				" Output:Manned station past code (WMO code table 4561)=" + pwao);
		assertEquals (GempakConstants.RMISSD, pwao);
	}

	/**
	 * Test method for {@link gov.noaa.nws.ncep.gempak.parameterconversionlibrary.PRLibrary#prQuot(float, float)}.
	 */
	@Test
	public void testPrQuot() {
		testCaseNum++;
		float quot= PRLibrary.prQuot(2f, 5f);
		System.out.println("testPrQuot testCaseNum=" + testCaseNum +
				"a: Input:Numerator= 2 5" +
				" Output:Denominator=" + quot);
		assertEquals (.4f, quot);
		quot= PRLibrary.prQuot(0f, 1f);
		System.out.println("testPrQuot testCaseNum=" + testCaseNum +
				"b: Input:Numerator)= 0 1" +
				" Output:Denominator=" + quot);
		assertEquals (0f, quot);
		quot= PRLibrary.prQuot(130f, 4f);
		System.out.println("testPrQuot testCaseNum=" + testCaseNum +
				"c: Input:Numerator)= 130 4" +
				" Output:Denominator=" + quot);
		assertEquals (32.5f, quot);
		quot= PRLibrary.prQuot(1230f, 0f);
		System.out.println("testPrQuot testCaseNum=" + testCaseNum +
				"d: Input:Numerator)= 1230 0" +
				" Output:Denominator=" + quot);
		assertEquals (GempakConstants.RMISSD, quot);
		quot= PRLibrary.prQuot(1230f, GempakConstants.RMISSD);
		System.out.println("testPrQuot testCaseNum=" + testCaseNum +
				"e: Input:Numerator)= 1230 GempakConstants.RMISSD" +
				" Output:Denominator=" + quot);
		assertEquals (GempakConstants.RMISSD, quot);
		quot= PRLibrary.prQuot(GempakConstants.RMISSD, 2f);
		System.out.println("testPrQuot testCaseNum=" + testCaseNum +
				"f: Input:Numerator)= 1GempakConstants.RMISSD 2" +
				" Output:Denominator=" + quot);
		assertEquals (GempakConstants.RMISSD, quot);
	}

	/**
	 * Test method for {@link gov.noaa.nws.ncep.gempak.parameterconversionlibrary.PRLibrary#prRelh(float, float)}.
	 */
	@Test
	public void testPrRelh() {
		testCaseNum++;
		float relh= PRLibrary.prRelh(50f, 50f);
		System.out.println("testPrRelh testCaseNum=" + testCaseNum +
				"a: Input:TMPC, DWPC= 50 50" +
				" Output:RELH=" + relh);
		assertEquals (100f, relh);
		relh= PRLibrary.prRelh(50f, 45f);
		System.out.println("testPrRelh testCaseNum=" + testCaseNum +
				"b: Input:TMPC, DWPC= 50 45" +
				" Output:RELH=" + relh);
		assertEquals (77.5638351f, relh);
		relh= PRLibrary.prRelh(34f, 32f);
		System.out.println("testPrRelh testCaseNum=" + testCaseNum +
				"c: Input:TMPC, DWPC= 34 32" +
				" Output:RELH=" + relh);
		assertEquals (89.3544464f, relh);
		relh= PRLibrary.prRelh(68f, 50f);
		System.out.println("testPrRelh testCaseNum=" + testCaseNum +
				"d: Input:TMPC, DWPC= 68 50" +
				" Output:RELH=" + relh);
		assertEquals (42.8650246f, relh);
		relh= PRLibrary.prRelh(70f, 73f);
		System.out.println("testPrRelh testCaseNum=" + testCaseNum +
				"e: Input:TMPC, DWPC= 70 73" +
				" Output:RELH=" + relh);
		assertEquals (113.893143f, relh);
		relh= PRLibrary.prRelh(GempakConstants.RMISSD, 73f);
		System.out.println("testPrRelh testCaseNum=" + testCaseNum +
				"f: Input:TMPC, DWPC= GempakConstants.RMISSD 73" +
				" Output:RELH=" + relh);
		assertEquals (GempakConstants.RMISSD, relh);
		relh= PRLibrary.prRelh(41f, GempakConstants.RMISSD);
		System.out.println("testPrRelh testCaseNum=" + testCaseNum +
				"g: Input:TMPC, DWPC= 41 GempakConstants.RMISSD" +
				" Output:RELH=" + relh);
		assertEquals (GempakConstants.RMISSD, relh);
	}

	/**
	 * Test method for {@link gov.noaa.nws.ncep.gempak.parameterconversionlibrary.PRLibrary#prRhdp(float, float)}.
	 */
	@Test
	public void testPrRhdp() {
		testCaseNum++;
		float rhdp= PRLibrary.prRhdp(30f, 100f);
		System.out.println("testPrRhdp testCaseNum=" + testCaseNum +
				"a: Input:TMPC, RELH= 30 100" +
				" Output:DWPC=" + rhdp);
		assertEquals (30f, rhdp);
		rhdp= PRLibrary.prRhdp(35f, 95f);
		System.out.println("testPrRhdp testCaseNum=" + testCaseNum +
				"b: Input:TMPC, RELH= 35 95" +
				" Output:DWPC=" + rhdp);
		assertEquals (34.0784149f, rhdp);
		rhdp= PRLibrary.prRhdp(62f, 80f);
		System.out.println("testPrRhdp testCaseNum=" + testCaseNum +
				"c: Input:TMPC, RELH= 62 80" +
				" Output:DWPC=" + rhdp);
		assertEquals (57.2352066f, rhdp);
		rhdp= PRLibrary.prRhdp(-10f, 60f);
		System.out.println("testPrRhdp testCaseNum=" + testCaseNum +
				"d: Input:TMPC, RELH= -10 60" +
				" Output:DWPC=" + rhdp);
		assertEquals (-16.2984734f, rhdp);
		rhdp= PRLibrary.prRhdp(77f, 75f);
		System.out.println("testPrRhdp testCaseNum=" + testCaseNum +
				"e: Input:TMPC, RELH= 77 75" +
				" Output:DWPC=" + rhdp);
		assertEquals (70.2760391f, rhdp);
		rhdp= PRLibrary.prRhdp(GempakConstants.RMISSD, 75f);
		System.out.println("testPrRhdp testCaseNum=" + testCaseNum +
				"f: Input:TMPC, RELH= GempakConstants.RMISSD 75" +
				" Output:DWPC=" + rhdp);
		assertEquals (GempakConstants.RMISSD, rhdp);
		rhdp= PRLibrary.prRhdp(34f, GempakConstants.RMISSD);
		System.out.println("testPrRhdp testCaseNum=" + testCaseNum +
				"g: Input:TMPC, RELH= 34 GempakConstants.RMISSD" +
				" Output:DWPC=" + rhdp);
		assertEquals (GempakConstants.RMISSD, rhdp);
	}

	/**
	 * Test method for {@link gov.noaa.nws.ncep.gempak.parameterconversionlibrary.PRLibrary#prSali(float)}.
	 */
	@Test
	public void testPrSali() {
		testCaseNum++;
		float sali= PRLibrary.prSali(12.47f);
		System.out.println("testPrSali testCaseNum=" + testCaseNum +
				"a: Input:Altimeter setting in inches ALTI= 12.47" +
				" Output:Abbreviated standard altimeter code in inches SALI=" + sali);
		assertEquals (247f, sali);
		sali= PRLibrary.prSali(37.55f);
		System.out.println("testPrSali testCaseNum=" + testCaseNum +
				"b: Input:Altimeter setting in inches ALTI= 37.55" +
				" Output:Abbreviated standard altimeter code in inches SALI=" + sali);
		assertEquals (755f, sali);
		sali= PRLibrary.prSali(9.375001f);
		System.out.println("testPrSali testCaseNum=" + testCaseNum +
				"c: Input:Altimeter setting in inches ALTI= 9.375001" +
				" Output:Abbreviated standard altimeter code in inches SALI=" + sali);
		assertEquals (938f, sali);
		sali= PRLibrary.prSali(18.41999f);
		System.out.println("testPrSali testCaseNum=" + testCaseNum +
				"d: Input:Altimeter setting in inches ALTI= 18.41999" +
				" Output:Abbreviated standard altimeter code in inches SALI=" + sali);
		assertEquals (842f, sali);
		sali= PRLibrary.prSali(GempakConstants.RMISSD);
		System.out.println("testPrSali testCaseNum=" + testCaseNum +
				"e: Input:Altimeter setting in inches ALTI= GempakConstants.RMISSD" +
				" Output:Abbreviated standard altimeter code in inches SALI=" + sali);
		assertEquals (GempakConstants.RMISSD, sali);
		sali= PRLibrary.prSali(-17.3212f);
		System.out.println("testPrSali testCaseNum=" + testCaseNum +
				"f: Input:Altimeter setting in inches ALTI= -17.3212" +
				" Output:Abbreviated standard altimeter code in inches SALI=" + sali);
		assertEquals (-732f, sali);
		sali= PRLibrary.prSali(0);
		System.out.println("testPrSali testCaseNum=" + testCaseNum +
				"g: Input:Altimeter setting in inches ALTI= 0" +
				" Output:Abbreviated standard altimeter code in inches SALI=" + sali);
		assertEquals (0f, sali);
		sali= PRLibrary.prSali(.105001f);
		System.out.println("testPrSali testCaseNum=" + testCaseNum +
				"h: Input:Altimeter setting in inches ALTI= .105001" +
				" Output:Abbreviated standard altimeter code in inches SALI=" + sali);
		assertEquals (11f, sali);
	}

	/**
	 * Test method for {@link gov.noaa.nws.ncep.gempak.parameterconversionlibrary.PRLibrary#prSalt(float)}.
	 */
	@Test
	public void testPrSalt() {
		testCaseNum++;
		float salt= PRLibrary.prSalt(12.47f);
		System.out.println("testPrSalt testCaseNum=" + testCaseNum +
				"a: Input:Altimeter setting in inches ALTI= 12.47" +
				" Output:Abbreviated standard altimeter code in mb SALT=" + salt);
		assertEquals (222f, salt);
		salt= PRLibrary.prSalt(35.2f);
		System.out.println("testPrSalt testCaseNum=" + testCaseNum +
				"b: Input:Altimeter setting in inches ALTI= 35.2" +
				" Output:Abbreviated standard altimeter code in mb SALT=" + salt);
		assertEquals (920f, salt);
		salt= PRLibrary.prSalt(9.3f);
		System.out.println("testPrSalt testCaseNum=" + testCaseNum +
				"c: Input:Altimeter setting in inches ALTI= 9.3" +
				" Output:Abbreviated standard altimeter code in mb SALT=" + salt);
		assertEquals (149f, salt);
		salt= PRLibrary.prSalt(29.78f);
		System.out.println("testPrSalt testCaseNum=" + testCaseNum +
				"d: Input:Altimeter setting in inches ALTI= 29.78" +
				" Output:Abbreviated standard altimeter code in mb SALT=" + salt);
		assertEquals (84f, salt);
		salt= PRLibrary.prSalt(GempakConstants.RMISSD);
		System.out.println("testPrSalt testCaseNum=" + testCaseNum +
				"d: Input:Altimeter setting in inches ALTI= GempakConstants.RMISSD" +
				" Output:Abbreviated standard altimeter code in mb SALT=" + salt);
		assertEquals (GempakConstants.RMISSD, salt);
	}

	/**
	 * Test method for {@link gov.noaa.nws.ncep.gempak.parameterconversionlibrary.PRLibrary#prSclh(float, float, float, float, float, float)}.
	 */
	@Test
	public void testPrSclh() {
		testCaseNum++;
		float sclh= PRLibrary.prSclh(10f, 25f, 22f, 25f, 955f, 980f);
		System.out.println("testPrSclh testCaseNum=" + testCaseNum +
				"a: Input:tb, tt, tdb, tdt, pb, pt= " +
				" Output:Scale height in meters=" + sclh);
		assertEquals (8753.85352f, sclh);
	}

	/**
	 * Test method for {@link gov.noaa.nws.ncep.gempak.parameterconversionlibrary.PRLibrary#prSkyx(float, float, float)}.
	 */
	@Test
	public void testPrSkyx() {
		fail("Not yet implemented");
	}

	/**
	 * Test method for {@link gov.noaa.nws.ncep.gempak.parameterconversionlibrary.PRLibrary#prSped(float, float)}.
	 */
	@Test
	public void testPrSped() {
		fail("Not yet implemented");
	}

	/**
	 * Test method for {@link gov.noaa.nws.ncep.gempak.parameterconversionlibrary.PRLibrary#prStdz(float, float)}.
	 */
	@Test
	public void testPrStdz() {
		fail("Not yet implemented");
	}

	/**
	 * Test method for {@link gov.noaa.nws.ncep.gempak.parameterconversionlibrary.PRLibrary#prThta(float, float)}.
	 */
	@Test
	public void testPrThta() {
		fail("Not yet implemented");
	}

	/**
	 * Test method for {@link gov.noaa.nws.ncep.gempak.parameterconversionlibrary.PRLibrary#prThwc(float, float, float)}.
	 */
	@Test
	public void testPrThwc() {
		fail("Not yet implemented");
	}

	/**
	 * Test method for {@link gov.noaa.nws.ncep.gempak.parameterconversionlibrary.PRLibrary#prTmwb(float, float, float)}.
	 */
	@Test
	public void testPrTmwb() {
		fail("Not yet implemented");
	}

	/**
	 * Test method for {@link gov.noaa.nws.ncep.gempak.parameterconversionlibrary.PRLibrary#prTpfr(float, float, float)}.
	 */
	@Test
	public void testPrTpfr() {
		fail("Not yet implemented");
	}

	/**
	 * Test method for {@link gov.noaa.nws.ncep.gempak.parameterconversionlibrary.PRLibrary#prTvrk(float, float, float)}.
	 */
	@Test
	public void testPrTvrk() {
		testCaseNum++;
		float tvrk= PRLibrary.prTvrk(35f, 35f, 1010f);
		System.out.println("testPrTvrk testCaseNum=" + testCaseNum +
				"a: Input:PRES (mb), TMPC,  DWPC, SELV (m)= 35 35 1010" +
				" Output:Mean sea level pressure in mb=" + tvrk);
		assertEquals (314.814697f, tvrk);
		tvrk= PRLibrary.prTvrk(78f, 75f, 1090f);
		System.out.println("testPrTvrk testCaseNum=" + testCaseNum +
				"b: Input:PRES (mb), TMPC,  DWPC, SELV (m)= 78 75 1090" +
				" Output:Mean sea level pressure in mb=" + tvrk);
		assertEquals (406.69812f, tvrk);
		tvrk= PRLibrary.prTvrk(64f, 64f, 1200f);
		System.out.println("testPrTvrk testCaseNum=" + testCaseNum +
				"c: Input:PRES (mb), TMPC,  DWPC, SELV (m)= 64 64 1200" +
				" Output:Mean sea level pressure in mb=" + tvrk);
		assertEquals (365.09848f, tvrk);
		tvrk= PRLibrary.prTvrk(0f, 1f, 945f);
		System.out.println("testPrTvrk testCaseNum=" + testCaseNum +
				"d: Input:PRES (mb), TMPC,  DWPC, SELV (m)= 0 1 945" +
				" Output:Mean sea level pressure in mb=" + tvrk);
		assertEquals (273.872803f, tvrk);
		tvrk= PRLibrary.prTvrk(GempakConstants.RMISSD, 1f, 945f);
		System.out.println("testPrTvrk testCaseNum=" + testCaseNum +
				"e: Input:PRES (mb), TMPC,  DWPC, SELV (m)= GempakConstants.RMISSD 1 945" +
				" Output:Mean sea level pressure in mb=" + tvrk);
		assertEquals (GempakConstants.RMISSD, tvrk);
		tvrk= PRLibrary.prTvrk(18f, GempakConstants.RMISSD, 945f);
		System.out.println("testPrTvrk testCaseNum=" + testCaseNum +
				"f: Input:PRES (mb), TMPC,  DWPC, SELV (m)= 18 GempakConstants.RMISSD 945" +
				" Output:Mean sea level pressure in mb=" + tvrk);
		assertEquals (291.149994f, tvrk);
		tvrk= PRLibrary.prTvrk(18f, 18f, GempakConstants.RMISSD);
		System.out.println("testPrTvrk testCaseNum=" + testCaseNum +
				"g: Input:PRES (mb), TMPC,  DWPC, SELV (m)= 18 18 GempakConstants.RMISSD" +
				" Output:Mean sea level pressure in mb=" + tvrk);
		assertEquals (GempakConstants.RMISSD, tvrk);
		tvrk= PRLibrary.prTvrk(18f, GempakConstants.RMISSD, GempakConstants.RMISSD);
		System.out.println("testPrTvrk testCaseNum=" + testCaseNum +
				"h: Input:PRES (mb), TMPC,  DWPC, SELV (m)= 18 GempakConstants.RMISSD GempakConstants.RMISSD" +
				" Output:Mean sea level pressure in mb=" + tvrk);
		assertEquals (GempakConstants.RMISSD, tvrk);
	}

	/**
	 * Test method for {@link gov.noaa.nws.ncep.gempak.parameterconversionlibrary.PRLibrary#prUwnd(float, float)}.
	 */
	@Test
	public void testPrUwnd() {
		fail("Not yet implemented");
	}

	/**
	 * Test method for {@link gov.noaa.nws.ncep.gempak.parameterconversionlibrary.PRLibrary#prVwnd(float, float)}.
	 */
	@Test
	public void testPrVwnd() {
		fail("Not yet implemented");
	}

	/**
	 * Test method for {@link gov.noaa.nws.ncep.gempak.parameterconversionlibrary.PRLibrary#prVskn(float)}.
	 */
	@Test
	public void testPrVskn() {
		fail("Not yet implemented");
	}

	/**
	 * Test method for {@link gov.noaa.nws.ncep.gempak.parameterconversionlibrary.PRLibrary#prWccv(float)}.
	 */
	@Test
	public void testPrWccv() {
		fail("Not yet implemented");
	}

	/**
	 * Test method for {@link gov.noaa.nws.ncep.gempak.parameterconversionlibrary.PRLibrary#prWceq(float, float)}.
	 */
	@Test
	public void testPrWceq() {
		fail("Not yet implemented");
	}

	/**
	 * Test method for {@link gov.noaa.nws.ncep.gempak.parameterconversionlibrary.PRLibrary#prWcht(float, float)}.
	 */
	@Test
	public void testPrWcht() {
		fail("Not yet implemented");
	}

	/**
	 * Test method for {@link gov.noaa.nws.ncep.gempak.parameterconversionlibrary.PRLibrary#prWcmp(float, float, float)}.
	 */
	@Test
	public void testPrWcmp() {
		fail("Not yet implemented");
	}

	/**
	 * Test method for {@link gov.noaa.nws.ncep.gempak.parameterconversionlibrary.PRLibrary#prWcms(float, float)}.
	 */
	@Test
	public void testPrWcms() {
		fail("Not yet implemented");
	}

	/**
	 * Test method for {@link gov.noaa.nws.ncep.gempak.parameterconversionlibrary.PRLibrary#prWind(float, float)}.
	 */
	@Test
	public void testPrWind() {
		fail("Not yet implemented");
	}

	/**
	 * Test method for {@link gov.noaa.nws.ncep.gempak.parameterconversionlibrary.PRLibrary#prWmao(float)}.
	 */
	@Test
	public void testPrWmao() {
		fail("Not yet implemented");
	}

	/**
	 * Test method for {@link gov.noaa.nws.ncep.gempak.parameterconversionlibrary.PRLibrary#prWnml(float, float, float)}.
	 */
	@Test
	public void testPrWnml() {
		fail("Not yet implemented");
	}

	/**
	 * Test method for {@link gov.noaa.nws.ncep.gempak.parameterconversionlibrary.PRLibrary#prWphf(float, float, float)}.
	 */
	@Test
	public void testPrWphf() {
		fail("Not yet implemented");
	}

	/**
	 * Test method for {@link gov.noaa.nws.ncep.gempak.parameterconversionlibrary.PRLibrary#prWvph(float, float, float)}.
	 */
	@Test
	public void testPrWvph() {
		fail("Not yet implemented");
	}

	/**
	 * Test method for {@link gov.noaa.nws.ncep.gempak.parameterconversionlibrary.PRLibrary#prWphm(float, float, float, float)}.
	 */
	@Test
	public void testPrWphm() {
		fail("Not yet implemented");
	}

	/**
	 * Test method for {@link gov.noaa.nws.ncep.gempak.parameterconversionlibrary.PRLibrary#prWtnd(java.lang.String)}.
	 */
	@Test
	public void testPrWtnd() {
		fail("Not yet implemented");
	}

	/**
	 * Test method for {@link gov.noaa.nws.ncep.gempak.parameterconversionlibrary.PRLibrary#prWvdd(float, float)}.
	 */
	@Test
	public void testPrWvdd() {
		fail("Not yet implemented");
	}

	/**
	 * Test method for {@link gov.noaa.nws.ncep.gempak.parameterconversionlibrary.PRLibrary#prWvis(float)}.
	 */
	@Test
	public void testPrWvis() {
		fail("Not yet implemented");
	}

	/**
	 * Test method for {@link gov.noaa.nws.ncep.gempak.parameterconversionlibrary.PRLibrary#prWvsw(float, float, float)}.
	 */
	@Test
	public void testPrWvsw() {
		fail("Not yet implemented");
	}

	/**
	 * Test method for {@link gov.noaa.nws.ncep.gempak.parameterconversionlibrary.PRLibrary#prWxvf(float, float)}.
	 */
	@Test
	public void testPrWxvf() {
		fail("Not yet implemented");
	}

	/**
	 * Test method for {@link gov.noaa.nws.ncep.gempak.parameterconversionlibrary.PRLibrary#prWcfr(float, float, float, float)}.
	 */
	@Test
	public void testPrWcfr() {
		fail("Not yet implemented");
	}

	/**
	 * Test method for {@link gov.noaa.nws.ncep.gempak.parameterconversionlibrary.PRLibrary#prXvfr(float, float)}.
	 */
	@Test
	public void testPrXvfr() {
		fail("Not yet implemented");
	}

	/**
	 * Test method for {@link gov.noaa.nws.ncep.gempak.parameterconversionlibrary.PRLibrary#prZalt(float, float)}.
	 */
	@Test
	public void testPrZalt() {
		fail("Not yet implemented");
	}

	/**
	 * Test method for {@link gov.noaa.nws.ncep.gempak.parameterconversionlibrary.PRLibrary#prNsym(float)}.
	 */
	@Test
	public void testPrNsym() {
		fail("Not yet implemented");
	}

}
