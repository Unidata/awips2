package gov.noaa.nws.ncep.gempak.parameters.colors;

import gov.noaa.nws.ncep.gempak.parameters.colors.COLORS;

import org.eclipse.swt.graphics.RGB;
import org.junit.Test;
import static org.junit.Assert.*;

public class COLORSTest {

	@Test
	public void testCOLORSNormalCase() {
		COLORS colors = new COLORS("1;2;4");
		
		assertEquals(colors.getFirstColor(), new RGB(255, 228, 220));
		assertEquals(colors.getColors()[1], new RGB(255, 0, 0));
		assertEquals(colors.getColors()[0], new RGB(255, 228, 220));
		assertEquals(colors.getColors()[2], new RGB(0, 0, 255));
	}
}
