package gov.noaa.nws.ncep.edex.plugin.stormtrack.util;

import static org.junit.Assert.assertEquals;
import gov.noaa.nws.ncep.common.dataplugin.stormtrack.StormTrackRecord;

import java.util.Calendar;

import org.junit.BeforeClass;
import org.junit.Test;

public class StormTrackParserTest {

	@BeforeClass
	public static void setUpBeforeClass() throws Exception {
	}

	@Test
	public void testProcessLatLon() {
		Float latLon;
		latLon = StormTrackParser.processLatLon("975W");
        assertEquals(-97.5, latLon.doubleValue());
		latLon = StormTrackParser.processLatLon("1605E");
        assertEquals(160.5, latLon.doubleValue());
		latLon = StormTrackParser.processLatLon("1605W");
        assertEquals(-160.5, latLon.doubleValue());
		latLon = StormTrackParser.processLatLon("623E");
        assertEquals(62.3, latLon.doubleValue());
		latLon = StormTrackParser.processLatLon("62N");
        assertEquals(6.2, latLon.doubleValue());
		latLon = StormTrackParser.processLatLon("847S");
        assertEquals(-84.7, latLon.doubleValue());
		latLon = StormTrackParser.processLatLon("847G");
        assertEquals(999999., latLon.doubleValue());
		latLon = StormTrackParser.processLatLon("");
        assertEquals(999999., latLon.doubleValue());
		latLon = StormTrackParser.processLatLon(" ");
        assertEquals(999999., latLon.doubleValue());
	}

	@Test
	public void testProcessWarnTime() {
		Calendar warnTime;

		warnTime = StormTrackParser.processWarnTime("2010061706");
		assertEquals (2010, warnTime.get(Calendar.YEAR));
		assertEquals (5, warnTime.get(Calendar.MONTH));
		assertEquals (17, warnTime.get(Calendar.DAY_OF_MONTH));
		assertEquals (6, warnTime.get(Calendar.HOUR_OF_DAY));
		
		warnTime = StormTrackParser.processWarnTime("2010062006");
		assertEquals (2010, warnTime.get(Calendar.YEAR));
		assertEquals (5, warnTime.get(Calendar.MONTH));
		assertEquals (20, warnTime.get(Calendar.DAY_OF_MONTH));
		assertEquals (6, warnTime.get(Calendar.HOUR_OF_DAY));
		
		warnTime = StormTrackParser.processWarnTime("2010062022");
		assertEquals (2010, warnTime.get(Calendar.YEAR));
		assertEquals (5, warnTime.get(Calendar.MONTH));
		assertEquals (20, warnTime.get(Calendar.DAY_OF_MONTH));
		assertEquals (22, warnTime.get(Calendar.HOUR_OF_DAY));
		
		warnTime = StormTrackParser.processWarnTime("2010123021");
		assertEquals (2010, warnTime.get(Calendar.YEAR));
		assertEquals (11, warnTime.get(Calendar.MONTH));
		assertEquals (30, warnTime.get(Calendar.DAY_OF_MONTH));
		assertEquals (21, warnTime.get(Calendar.HOUR_OF_DAY));
		
		warnTime = StormTrackParser.processWarnTime("2010101012");
		assertEquals (2010, warnTime.get(Calendar.YEAR));
		assertEquals (9, warnTime.get(Calendar.MONTH));
		assertEquals (10, warnTime.get(Calendar.DAY_OF_MONTH));
		assertEquals (12, warnTime.get(Calendar.HOUR_OF_DAY));
		
		warnTime = StormTrackParser.processWarnTime(" ");
		Calendar warnTimeD = Calendar.getInstance();
		assertEquals (warnTimeD.get(Calendar.YEAR), warnTime.get(Calendar.YEAR));
		assertEquals (warnTimeD.get(Calendar.MONTH), warnTime.get(Calendar.MONTH));
		assertEquals (warnTimeD.get(Calendar.DAY_OF_MONTH), warnTime.get(Calendar.DAY_OF_MONTH));
		assertEquals (warnTimeD.get(Calendar.HOUR_OF_DAY), warnTime.get(Calendar.HOUR_OF_DAY));
		
	}
	@Test
	public void testProcessFields() {
		StormTrackRecord record;
		String theBulletin;
		Calendar warnDateTime;
		theBulletin = "EP, 20, 2009101518,  2, ZGFS,   0, 116M,  966W,  25, 1008, XX,  34, NEQ,  100,   85,   70,   85,\n";
		record = StormTrackParser.processFields(theBulletin);
		assertEquals ("EP", record.getBasin());
		assertEquals (20, record.getCycloneNum());
		warnDateTime = record.getWarnTime();
		assertEquals (2009, warnDateTime.get(Calendar.YEAR));
		assertEquals (9, warnDateTime.get(Calendar.MONTH));
		assertEquals (15, warnDateTime.get(Calendar.DAY_OF_MONTH));
		assertEquals (18, warnDateTime.get(Calendar.HOUR_OF_DAY));
		assertEquals (2, record.getTechniqueNum());
		assertEquals ("ZGFS", record.getModel());
		assertEquals (0, record.getFcstHour());
		assertEquals (999999., record.getClat());
		assertEquals (-96.6, record.getClon());
		assertEquals (25., record.getWindMax());
		assertEquals (1008., record.getMslp());
		assertEquals ("XX", record.getStormType());
		assertEquals (34., record.getWindCategory());
		assertEquals ("NEQ", record.getWindCode());
		assertEquals (100., record.getQuad1WindRad());
		assertEquals (85., record.getQuad2WindRad());
		assertEquals (70., record.getQuad3WindRad());
		assertEquals (85., record.getQuad4WindRad());
		assertEquals (999999., record.getClosedP());
		assertEquals (999999., record.getRadClosedP());
		//
		theBulletin = "CP, 85, 2010053012, 03, OFCL,   3, 242N, 1568W, 100,  960, HU,  34, NEQ,  130,  132,  134,  135,    0,    0,   0, 120,  20,   C,   0, RMT,  25,   9,   HURCNAME,  , 12, NEQ, 130, 131, 132, 133\n";
		record = StormTrackParser.processFields(theBulletin);
		assertEquals ("CP", record.getBasin());
		assertEquals (85, record.getCycloneNum());
		warnDateTime = record.getWarnTime();
		assertEquals (2010, warnDateTime.get(Calendar.YEAR));
		assertEquals (4, warnDateTime.get(Calendar.MONTH));
		assertEquals (30, warnDateTime.get(Calendar.DAY_OF_MONTH));
		assertEquals (12, warnDateTime.get(Calendar.HOUR_OF_DAY));
		assertEquals (3, record.getTechniqueNum());
		assertEquals ("OFCL", record.getModel());
		assertEquals (3, record.getFcstHour());
		assertEquals (24.2, record.getClat());
		assertEquals (-156.8, record.getClon());
		assertEquals (100., record.getWindMax());
		assertEquals (960., record.getMslp());
		assertEquals ("HU", record.getStormType());
		assertEquals (34., record.getWindCategory());
		assertEquals ("NEQ", record.getWindCode());
		assertEquals (130., record.getQuad1WindRad());
		assertEquals (132., record.getQuad2WindRad());
		assertEquals (134., record.getQuad3WindRad());
		assertEquals (135., record.getQuad4WindRad());
		assertEquals (0., record.getClosedP());
		assertEquals (0., record.getRadClosedP());
		assertEquals (0., record.getMaxWindRad());
		assertEquals (120., record.getGust());
		assertEquals (20., record.getEyeSize());
		assertEquals ("C", record.getSubRegion());
		assertEquals (0., record.getMaxSeas());
		assertEquals ("RMT", record.getForecaster());
		assertEquals (25., record.getStormDrct());
		assertEquals (9., record.getStormSped());
		assertEquals ("HURCNAME", record.getStormName());
		assertEquals (" ", record.getStormDepth());
		assertEquals (12., record.getWaveHght());
		assertEquals ("NEQ", record.getWaveCode());
		assertEquals (130., record.getQuad1WaveRad());
		assertEquals (131., record.getQuad2WaveRad());
		assertEquals (132., record.getQuad3WaveRad());
		assertEquals (133., record.getQuad4WaveRad());		
	}

}
