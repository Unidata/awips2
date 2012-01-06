package gov.noaa.nws.ncep.viz.rsc.ffg;

import static org.junit.Assert.*;

import gov.noaa.nws.ncep.viz.rsc.ffg.CountyZoneInfo;
import gov.noaa.nws.ncep.viz.rsc.ffg.CountyZoneInfo.ZoneEntry;

import java.io.FileNotFoundException;
import java.io.IOException;

import org.junit.After;
import org.junit.AfterClass;
import org.junit.Before;
import org.junit.BeforeClass;
import org.junit.Test;

public class CountyZoneInfoTest {

	private CountyZoneInfo czInfo = null;
	private CountyZoneInfo badCzInfo = null;
	private String czFile = "/usr1/ghull/junit_tests/Task83/ffgCountyZones.stn";
	private String badCzFile = "fileNotFound.xxx";
	
	
	@BeforeClass
	public static void setUpBeforeClass() throws Exception {
	}

	@AfterClass
	public static void tearDownAfterClass() throws Exception {
	}

	@Before
	public void setUp() throws Exception {
		czInfo = new CountyZoneInfo( czFile );
		badCzInfo = new CountyZoneInfo( badCzFile );
	}

	@After
	public void tearDown() throws Exception {
	}

	@Test
	public void testCountyZoneInfo() {
		assertNotNull( czInfo );
	}

	@Test
	public void testGetZone() {
		try {
			assertTrue( czInfo.loadCounties() );
		}
		catch( Exception e ) {
			fail("Error loadCounties:"+czInfo.getFileName() );
		}

		String zoneid = "FLZ045"; // good zone
		ZoneEntry zone = czInfo.getZone( zoneid );
		
		assertNotNull( zone );
		assertEquals( zone.getZoneId(), zoneid );
		assertTrue( zone.getLat() < 29.00 );
		assertTrue( zone.getLat() > 28.00 );
		assertTrue( zone.getLon() < -81.30 );
		assertTrue( zone.getLon() > -81.40 );
		
		zoneid = "XXXX"; // bad zone
		zone = czInfo.getZone( zoneid );
		assertNull( zone );
	}

	@Test
	public void testLoadCounties() {
		// 
		try {
			assertTrue( czInfo.loadCounties() );
		}
		catch( Exception e ) {
			fail("Error loadCounties:"+czInfo.getFileName() );
		}
		
		czInfo = new CountyZoneInfo( "fileNotFound.xxx");

		try {
			czInfo.loadCounties();
			fail("returned true in FileNotFound case");
		}
		catch( FileNotFoundException e ) {
			assertTrue(true);
		}
		catch( IOException e ) {
			fail("IOException in fileNotFound case");
		}
	}
}
