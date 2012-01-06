package gov.noaa.nws.ncep.edex.plugin.h5uair.util;

import static org.junit.Assert.*;
import gov.noaa.nws.ncep.edex.plugin.h5uair.util.TempGroup;
import org.junit.Test;

public class TempGroupTest {
	
	public void setUp() throws Exception {
	}

	@Test
	public void testTempField() {
		String tempgroup = "12424";
		TempGroup.TempField(tempgroup);
		float temp = TempGroup.getTemperature();
		float dt = TempGroup.getDewpointTemp();
		assertEquals(12.4,temp);
		assertEquals(10.0,dt);
	}

}