package gov.noaa.nws.ncep.edex.plugin.h5scd.util;

import static org.junit.Assert.*;
import org.junit.Before;
import org.junit.Test;
import com.raytheon.edex.util.Util;
import java.util.Calendar;
import java.util.TimeZone;
import gov.noaa.nws.ncep.common.dataplugin.h5scd.H5ScdRecord;
import gov.noaa.nws.ncep.edex.plugin.h5scd.util.H5ScdParser;
import gov.noaa.nws.ncep.edex.util.UtilN;
import gov.noaa.nws.ncep.common.tools.IDecoderConstantsN;

public class H5ScdParserTest {
	final double EPSILON = 0.01;
	String good_report_case1;
	String good_report_case2;
	String good_report_case3;
	String bad_report;
	String issueTime_case1;
	String issueTime_case2;
	H5ScdRecord record;
	Calendar cal;
	Integer month;

	@Before 	
	public void initialize () {
		good_report_case1 = "KDCA SCD 0045 8123456 70041 400500024\r\r\n";
		good_report_case2 = "KDCA SCD COR 0550 -SHRA FZRA FZDZ SHPL 888/3// 931022 933003\r\r\n" 
			+ "4/030 60012 98122 24/931043 70041 400501094\r\r\n";
		good_report_case3 = "KDCA SCD 2355 410511094\r\r\n";
		bad_report = "KDCA SCD XXX 2355 -RA\r\r\n";
		issueTime_case1 = "280050";
		/* 
		 * Find month based on the date of the issuance time
		 */
		Calendar cdr = UtilN.findDataTime(issueTime_case1, (Calendar)null);
		month = cdr.get(Calendar.MONTH);
		record = new H5ScdRecord();
		cal = Calendar.getInstance(TimeZone.getTimeZone("GMT"));
		try {
			record.setIssueTime(Util.findCurrentTime(issueTime_case1));
		} catch (Exception e) {
			// no statement block
		}
	}

	@Test
	public void test1ProcessH5Scd() {
		
		/* 
		 * Case I: Good report 
		 */
		H5ScdParser.processH5Scd(good_report_case1, record);
		assertEquals("KDCA",record.getStationID());
		assertEquals("REG",record.getCorIndicator());
		assertEquals(1L,(long)record.getCFRT());
		assertEquals(2L,(long)record.getCFRL());
		assertEquals(3L,(long)record.getCTYL());
		assertEquals(4L,(long)record.getCBAS());
		assertEquals(5L,(long)record.getCTYM());
		assertEquals(6L,(long)record.getCTYH());
		assertEquals(5.0D,(double)record.getTDXC(),EPSILON);
		assertEquals(2.4D,(double)record.getTDNC(),EPSILON);
		assertEquals(0.41D,(double)record.getP24I(),EPSILON);

		/*
		 * Test observation time for the 1st case.
		 */
		cal.set(Calendar.MONTH,month);
		cal.set(Calendar.DAY_OF_MONTH,28);
		cal.set(Calendar.HOUR_OF_DAY, 0);
		cal.set(Calendar.MINUTE,45);
		cal.set(Calendar.SECOND,0);
		cal.set(Calendar.MILLISECOND,0);
		assertEquals(cal,record.getObsTime());
		assertEquals(record.getSuspectTimeFlag(), "false");
	}

	@Test
	public void test2ProcessH5Scd() {	
		
		/* 
		 * Case II: Good report with COR, two liners and lengthy weather report
		 */
		H5ScdParser.processH5Scd(good_report_case2, record);
		assertEquals("COR",record.getCorIndicator());
		assertEquals(8L,(long)record.getCFRT());
		assertEquals(8L,(long)record.getCFRL());
		assertEquals(IDecoderConstantsN.INTEGER_MISSING.longValue(),(long)record.getCTYL());
		assertEquals(IDecoderConstantsN.INTEGER_MISSING.longValue(),(long)record.getCTYM());
		assertEquals(IDecoderConstantsN.INTEGER_MISSING.longValue(),(long)record.getCTYH());
		assertEquals(3L,(long)record.getCBAS());
		assertEquals(122L,(long)record.getMSUN());
		assertEquals(2.2D,(double)record.getSNEW(),EPSILON);
		assertEquals(0.3D,(double)record.getWEQS(),EPSILON);
		assertEquals(3.0D,(double)record.getSNOW(),EPSILON);		
		assertEquals(4.3D,(double)record.getS24I(),EPSILON);
		assertEquals(5.0D,(double)record.getTDXC(),EPSILON);
		assertEquals(-9.4D,(double)record.getTDNC(),EPSILON);
		assertEquals(0.12D,(double)record.getP06I(),EPSILON);
		assertEquals(0.41D,(double)record.getP24I(),EPSILON);
		assertEquals("-SHRA FZRA FZDZ SHPL",record.getWTHR());

		/*
		 * Test observation time 2nd case.
		 * 	Issue Time: 250050
		 *  Obs Time: 0550
		 */
		cal.set(Calendar.MONTH,month);
		cal.set(Calendar.DAY_OF_MONTH,27);
		cal.set(Calendar.HOUR_OF_DAY, 5);
		cal.set(Calendar.MINUTE,50);
		cal.set(Calendar.SECOND,0);
		cal.set(Calendar.MILLISECOND,0);
		assertEquals(cal,record.getObsTime());
		assertEquals(record.getSuspectTimeFlag(), "true");	
	}

	@Test
	public void test3ProcessH5Scd() {

		/* 
		 * Case III: Good report with date change
		 */
		H5ScdParser.processH5Scd(good_report_case3, record);
		assertEquals(-5.1D,(double)record.getTDXC(),EPSILON);
		assertEquals(-9.4D,(double)record.getTDNC(),EPSILON);
		
		/*
		 * Test observation time for the 3rd case.
		 * 	Issue time: 250050.
		 * 	Obs time: 2355.
		 */
		cal.set(Calendar.MONTH,month);
		cal.set(Calendar.DAY_OF_MONTH,27);
		cal.set(Calendar.HOUR_OF_DAY, 23);
		cal.set(Calendar.MINUTE,55);
		cal.set(Calendar.SECOND,0);
		cal.set(Calendar.MILLISECOND,0);
		assertEquals(cal,record.getObsTime());
		assertEquals(record.getSuspectTimeFlag(), "false");
	}

	@Test
	public void test4ProcessH5Scd() {	
		
		/* 
		 * Case IV: Bad report with "XXX" instead of "COR"
		 */
		H5ScdParser.processH5Scd(bad_report, record);	 
		assertEquals("XXX",record.getCorIndicator());
	}
}
