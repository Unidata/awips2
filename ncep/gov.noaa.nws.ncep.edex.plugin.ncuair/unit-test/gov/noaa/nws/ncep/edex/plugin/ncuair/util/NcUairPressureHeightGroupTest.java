package gov.noaa.nws.ncep.edex.plugin.ncuair.util;

import static org.junit.Assert.*;
import gov.noaa.nws.ncep.edex.plugin.ncuair.util.NcUairPressureHeightGroup;
import gov.noaa.nws.ncep.edex.util.UtilN;
import gov.noaa.nws.ncep.common.dataplugin.ncuair.NcUairRecord;
import gov.noaa.nws.ncep.common.tools.IDecoderConstantsN;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import java.util.Calendar;
import java.util.Iterator;
import java.util.Set;

import org.junit.After;
import org.junit.Before;
import org.junit.Test;

public class NcUairPressureHeightGroupTest {
	
	public void setUp() throws Exception {
	}

	@Test
	public void testPressHeightField() {
		
		String presgroup = "00104";
		Boolean above = false;
		int level = 1;
		String stationNumber = "72403";
		String dataType = "TTAA";
		NcUairRecord record = null;
		NcUairPressureHeightGroup.PressureHeightField(presgroup, above, level, stationNumber, dataType, record);
		float height = NcUairPressureHeightGroup.getHeight();
		float pres = NcUairPressureHeightGroup.getPressure();		
		assertEquals(104.0, height);
		assertEquals(1000.0, pres);
	}

}