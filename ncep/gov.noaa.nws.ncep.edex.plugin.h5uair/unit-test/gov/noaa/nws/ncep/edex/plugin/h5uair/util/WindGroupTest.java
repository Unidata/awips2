package gov.noaa.nws.ncep.edex.plugin.h5uair.util;

import static org.junit.Assert.*;
import gov.noaa.nws.ncep.edex.plugin.h5uair.util.WindGroup;
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

public class WindGroupTest {
	
	public void setUp() throws Exception {
	}

	@Test
	public void testWindField() {		
		String windgroup = "27049";		
		WindGroup.WindField(windgroup, false);
		float windSpeed = WindGroup.getSped();
		float winddir = WindGroup.getDrct();
		assertEquals(49.0,windSpeed);
		assertEquals(270.0,winddir);
	}
}