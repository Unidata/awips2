package gov.noaa.nws.ncep.edex.plugin.h5uair.util;

import static org.junit.Assert.*;
import gov.noaa.nws.ncep.edex.plugin.h5uair.util.TimeGroup;
import gov.noaa.nws.ncep.edex.util.UtilN;
import gov.noaa.nws.ncep.common.tools.IDecoderConstantsN;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import java.util.Calendar;
import java.util.Iterator;
import java.util.Set;

import org.junit.After;
import org.junit.Before;
import org.junit.Test;

public class TimeGroupTest {
	
	public void setUp() throws Exception {
	}

	@Test
	public void testTimeField() {
		String report = "USUS41 KLWX 190000\r\r\n" + 
		                "72403 TTAA  69001 72403 99002 12424 36004 00104 12223 00507 \r\r\n" + 
		                "92752 07200 02010 85448 08833 31513 70035 00631 26515 50566 \r\r\n" + 
		                "14941 26524 40731 26758 25535 30932 43966 25541 25052 50967 \r\r\n" + 
		                "25059 20195 54969 24559 15379 57376 27049 10634 57982 24530 \r\r\n" +
		                "88215 54568 25061 77237 24565 42207 31313 58708 82303 51515 \r\r\n" + 
		                "10164 00005 10194 35511 28014= \r\r\n";
		
		TimeGroup.TimeField(report);
		String obsUTC = TimeGroup.getObsUTC();
		String UTC = TimeGroup.getUTC();
		int topwind = TimeGroup.getTopwind();
		Boolean windKnot = TimeGroup.getWindKnot();
		assertEquals("00",obsUTC);
		assertEquals("00",UTC);
		assertEquals(1,topwind);
		assertEquals(true,windKnot);
	}

}