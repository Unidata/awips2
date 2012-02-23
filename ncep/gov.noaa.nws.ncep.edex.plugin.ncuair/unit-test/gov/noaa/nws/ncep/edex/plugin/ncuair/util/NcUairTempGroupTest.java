package gov.noaa.nws.ncep.edex.plugin.ncuair.util;

import static org.junit.Assert.*;
import gov.noaa.nws.ncep.edex.plugin.ncuair.util.NcUairTempGroup;

import org.junit.Test;

public class NcUairTempGroupTest {
	
	public void setUp() throws Exception {
	}

	@Test
	public void testTempField() {
		String tempgroup = "12424";
		NcUairTempGroup.TempField(tempgroup);
		float temp = NcUairTempGroup.getTemperature();
		float dt = NcUairTempGroup.getDewpointTemp();
		assertEquals(12.4,temp);
		assertEquals(10.0,dt);
	}

}