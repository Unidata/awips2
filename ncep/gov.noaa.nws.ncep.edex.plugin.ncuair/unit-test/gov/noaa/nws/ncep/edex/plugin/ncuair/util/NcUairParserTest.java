/**
 * This Java class is the JUnit test for the UairParser.
 *
 * <pre>
 *
 * L. Lin       04/09   Creation
 * S. Gurung    09/11  Renamed H5 to Nc and h5 to nc
 * D. Johnson   08/12  Upgrade to JUnit 4.10.
 * </pre>
 *
 */
package gov.noaa.nws.ncep.edex.plugin.ncuair.util;

import static org.junit.Assert.assertEquals;

import org.junit.Before;
import org.junit.Test;

public class NcUairParserTest {
	String report = "USUS41 KLWX 190000\r\r\n" + 
	"72403 TTAA  69001 72403 99002 12424 36004 00104 12223 00507 \r\r\n" + 
	"92752 07200 02010 85448 08833 31513 70035 00631 26515 50566 \r\r\n" + 
	"14941 26524 40731 26758 25535 30932 43966 25541 25052 50967 \r\r\n" + 
	"25059 20195 54969 24559 15379 57376 27049 10634 57982 24530 \r\r\n" +
	"88215 54568 25061 77237 24565 42207 31313 58708 82303 51515 \r\r\n" + 
	"10164 00005 10194 35511 28014= \r\r\n";

	@Before
	public void initialize () {
	}


	@Test
	public void testGetDataType() {
		String dataType = NcUairParser.getDataType(report);
		assertEquals("TTAA",dataType);
	}

	@Test
	public void testGetStationNumber() {
		String station = NcUairParser.getStationNumber(report);
		assertEquals("72403",station);
	}
	
	@Test
	public void testFindCorIndicator() {
		String nul = null;
		String cor = NcUairParser.findCorIndicator(report);
		assertEquals(nul, cor);
	}
	
	@Test
	public void testGetUairType() {
		Integer uairType = NcUairParser.getUairType("TTAA");
        assertEquals(1, uairType.intValue());
	}
}

