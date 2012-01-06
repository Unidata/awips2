/**
 * IdftParserTest.java
 * 
 * Junit test for IdftParser
 * 
 * <pre>
 * SOFTWARE HISTORY
 * 
 * Date       	Ticket#		Engineer	Description
 * ------------	----------	-----------	--------------------------
 * 02Jun2009		 100	F. J. Yen	Initial creation
 * 27May2010		 100	F. J. Yen	Migrated from to11dr3 to to11dr11
 * 
 * </pre>
 * 
 * @author Fee Jing Yen, SIB
 * @version 1
 * 
 */
package gov.noaa.nws.ncep.edex.plugin.idft.util;

import static org.junit.Assert.*;

import java.util.Calendar;
import java.util.List;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import org.junit.Test;

import gov.noaa.nws.ncep.common.dataplugin.idft.IdftRecord;

public class IdftParserTest {
	IdftRecord record = new IdftRecord();
	/*
	 * Unable to test method readIdftLocs due to edex localization methods used 
	 * and junit test does not run edex.  Unable to test for itype = 0 for the
	 * same reason since the idftLocs.xml table is needed to obtain the lat/lon.
	 */

	@Test
	public void testProcessIdft(){
		final String IDFT_DATALN2 = "(\\d{1,4}) +(\\d{0,2})\\.(\\d)(N|S) +(\\d{0,3})\\.(\\d)(W|E) +(\\d{1,3}) +(\\d{1,4})\\.(\\d)\\r\\r\\n";
		final Pattern dataLnPattern2 = Pattern.compile(IDFT_DATALN2);
		String thePntRec2 = "  410   66.8S   73.3W  253    2.4\r\r\n";
		Matcher m2 = dataLnPattern2.matcher(thePntRec2);
		if (m2.find()) {
			IdftParser.processIdft(m2, 6, record);	
		    assertEquals (410, record.getPointNum());
			assertEquals (-66.8, record.getLat());
			assertEquals (-73.3, record.getLon());
			assertEquals (253.0F, record.getDirection());
		    assertEquals (2.4F, record.getDistanceNm());
		}
		thePntRec2 = "  390   50.5N   10.3E  180    4.5\r\r\n";
		m2 = dataLnPattern2.matcher(thePntRec2);
		if (m2.find()) {
			IdftParser.processIdft(m2, 6, record);	
		    assertEquals (390, record.getPointNum());
			assertEquals (50.5, record.getLat());
			assertEquals (10.3, record.getLon());
			assertEquals (180.0F, record.getDirection());
		    assertEquals (4.5F, record.getDistanceNm());
		}
	}
}