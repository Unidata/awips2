package gov.noaa.nws.ncep.edex.plugin.ncuair.util;

import static org.junit.Assert.*;
import gov.noaa.nws.ncep.edex.plugin.ncuair.util.NcUairWindGroup;
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

public class NcUairWindGroupTest {
	
	public void setUp() throws Exception {
	}

	@Test
	public void testWindField() {		
		String windgroup = "27049";		
		NcUairWindGroup.WindField(windgroup, false);
		float windSpeed = NcUairWindGroup.getSped();
		float winddir = NcUairWindGroup.getDrct();
		assertEquals(49.0,windSpeed);
		assertEquals(270.0,winddir);
	}
}