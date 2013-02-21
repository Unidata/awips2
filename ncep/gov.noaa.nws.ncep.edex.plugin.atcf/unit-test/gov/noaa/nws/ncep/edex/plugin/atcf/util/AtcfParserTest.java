package gov.noaa.nws.ncep.edex.plugin.atcf.util;

import static org.junit.Assert.assertEquals;
import gov.noaa.nws.ncep.common.dataplugin.atcf.AtcfRecord;

import java.util.Calendar;

import org.junit.Test;

public class AtcfParserTest {

    private static final double ALLOWABLE_DOUBLE_DELTA = 0.00001;

	@Test
	public void testProcessLatLon() {
        float latLon = AtcfParser.processLatLon("975W");
        assertEquals(-97.5F, latLon, ALLOWABLE_DOUBLE_DELTA);
		latLon = AtcfParser.processLatLon("1605E");
        assertEquals(160.5, latLon, ALLOWABLE_DOUBLE_DELTA);
		latLon = AtcfParser.processLatLon("1605W");
        assertEquals(-160.5, latLon, ALLOWABLE_DOUBLE_DELTA);
		latLon = AtcfParser.processLatLon("623E");
        assertEquals(62.3, latLon, ALLOWABLE_DOUBLE_DELTA);
		latLon = AtcfParser.processLatLon("62N");
        assertEquals(6.2, latLon, ALLOWABLE_DOUBLE_DELTA);
		latLon = AtcfParser.processLatLon("847S");
        assertEquals(-84.7, latLon, ALLOWABLE_DOUBLE_DELTA);
		latLon = AtcfParser.processLatLon("847G");
        assertEquals(999999., latLon, ALLOWABLE_DOUBLE_DELTA);
		latLon = AtcfParser.processLatLon("");
        assertEquals(999999., latLon, ALLOWABLE_DOUBLE_DELTA);
		latLon = AtcfParser.processLatLon(" ");
        assertEquals(999999., latLon, ALLOWABLE_DOUBLE_DELTA);
	}

	@Test
	public void testProcessWarnTime() {
		Calendar warnTime;

		warnTime = AtcfParser.processWarnTime("2010061706");
		assertEquals (2010, warnTime.get(Calendar.YEAR));
		assertEquals (5, warnTime.get(Calendar.MONTH));
		assertEquals (17, warnTime.get(Calendar.DAY_OF_MONTH));
		assertEquals (6, warnTime.get(Calendar.HOUR_OF_DAY));
		
		warnTime = AtcfParser.processWarnTime("2010062006");
		assertEquals (2010, warnTime.get(Calendar.YEAR));
		assertEquals (5, warnTime.get(Calendar.MONTH));
		assertEquals (20, warnTime.get(Calendar.DAY_OF_MONTH));
		assertEquals (6, warnTime.get(Calendar.HOUR_OF_DAY));
		
		warnTime = AtcfParser.processWarnTime("2010062022");
		assertEquals (2010, warnTime.get(Calendar.YEAR));
		assertEquals (5, warnTime.get(Calendar.MONTH));
		assertEquals (20, warnTime.get(Calendar.DAY_OF_MONTH));
		assertEquals (22, warnTime.get(Calendar.HOUR_OF_DAY));
		
		warnTime = AtcfParser.processWarnTime("2010123021");
		assertEquals (2010, warnTime.get(Calendar.YEAR));
		assertEquals (11, warnTime.get(Calendar.MONTH));
		assertEquals (30, warnTime.get(Calendar.DAY_OF_MONTH));
		assertEquals (21, warnTime.get(Calendar.HOUR_OF_DAY));
		
		warnTime = AtcfParser.processWarnTime("2010101012");
		assertEquals (2010, warnTime.get(Calendar.YEAR));
		assertEquals (9, warnTime.get(Calendar.MONTH));
		assertEquals (10, warnTime.get(Calendar.DAY_OF_MONTH));
		assertEquals (12, warnTime.get(Calendar.HOUR_OF_DAY));
		
		warnTime = AtcfParser.processWarnTime(" ");
		Calendar warnTimeD = Calendar.getInstance();
		assertEquals (warnTimeD.get(Calendar.YEAR), warnTime.get(Calendar.YEAR));
		assertEquals (warnTimeD.get(Calendar.MONTH), warnTime.get(Calendar.MONTH));
		assertEquals (warnTimeD.get(Calendar.DAY_OF_MONTH), warnTime.get(Calendar.DAY_OF_MONTH));
		assertEquals (warnTimeD.get(Calendar.HOUR_OF_DAY), warnTime.get(Calendar.HOUR_OF_DAY));
		
	}
	@Test
	public void testProcessFields() {
		AtcfRecord record;
		String theBulletin;
		Calendar warnDateTime;
		theBulletin = "EP, 20, 2009101518,  2, ZGFS,   0, 116M,  966W,  25, 1008, XX,  34, NEQ,  100,   85,   70,   85,\n";
		record = AtcfParser.processFields(theBulletin);
		assertEquals ("EP", record.getBasin());
		assertEquals (20, record.getCycloneNum());
		warnDateTime = record.getWarnTime();
		assertEquals (2009, warnDateTime.get(Calendar.YEAR));
		assertEquals (9, warnDateTime.get(Calendar.MONTH));
		assertEquals (15, warnDateTime.get(Calendar.DAY_OF_MONTH));
		assertEquals (18, warnDateTime.get(Calendar.HOUR_OF_DAY));
		assertEquals (2, record.getTechniqueNum());
		assertEquals ("ZGFS", record.getTechnique());
		assertEquals (0, record.getFcstHour());
        assertEquals(999999., record.getClat(), ALLOWABLE_DOUBLE_DELTA);
        assertEquals(-96.6, record.getClon(), ALLOWABLE_DOUBLE_DELTA);
        assertEquals(25., record.getWindMax(), ALLOWABLE_DOUBLE_DELTA);
        assertEquals(1008., record.getMslp(), ALLOWABLE_DOUBLE_DELTA);
		assertEquals ("XX", record.getIntensity());
        assertEquals(34., record.getRadWind(), ALLOWABLE_DOUBLE_DELTA);
		assertEquals ("NEQ", record.getRadWindQuad());
        assertEquals(100., record.getQuad1WindRad(), ALLOWABLE_DOUBLE_DELTA);
        assertEquals(85., record.getQuad2WindRad(), ALLOWABLE_DOUBLE_DELTA);
        assertEquals(70., record.getQuad3WindRad(), ALLOWABLE_DOUBLE_DELTA);
        assertEquals(85., record.getQuad4WindRad(), ALLOWABLE_DOUBLE_DELTA);
        assertEquals(999999., record.getClosedP(), ALLOWABLE_DOUBLE_DELTA);
        assertEquals(999999., record.getRadClosedP(), ALLOWABLE_DOUBLE_DELTA);
		//
		theBulletin = "CP, 85, 2010053012, 03, OFCL,   3, 242N, 1568W, 100,  960, HU,  34, NEQ,  130,  132,  134,  135,    0,    0,   0, 120,  20,   C,   0, RMT,  25,   9,   HURCNAME,  , 12, NEQ, 130, 131, 132, 133\n";
		record = AtcfParser.processFields(theBulletin);
		assertEquals ("CP", record.getBasin());
		assertEquals (85, record.getCycloneNum());
		warnDateTime = record.getWarnTime();
		assertEquals (2010, warnDateTime.get(Calendar.YEAR));
		assertEquals (4, warnDateTime.get(Calendar.MONTH));
		assertEquals (30, warnDateTime.get(Calendar.DAY_OF_MONTH));
		assertEquals (12, warnDateTime.get(Calendar.HOUR_OF_DAY));
		assertEquals (3, record.getTechniqueNum());
		assertEquals ("OFCL", record.getTechnique());
		assertEquals (3, record.getFcstHour());
        assertEquals(24.2, record.getClat(), ALLOWABLE_DOUBLE_DELTA);
        assertEquals(-156.8, record.getClon(), ALLOWABLE_DOUBLE_DELTA);
        assertEquals(100., record.getWindMax(), ALLOWABLE_DOUBLE_DELTA);
        assertEquals(960., record.getMslp(), ALLOWABLE_DOUBLE_DELTA);
		assertEquals ("HU", record.getIntensity());
        assertEquals(34., record.getRadWind(), ALLOWABLE_DOUBLE_DELTA);
		assertEquals ("NEQ", record.getRadWindQuad());
        assertEquals(130., record.getQuad1WindRad(), ALLOWABLE_DOUBLE_DELTA);
        assertEquals(132., record.getQuad2WindRad(), ALLOWABLE_DOUBLE_DELTA);
        assertEquals(134., record.getQuad3WindRad(), ALLOWABLE_DOUBLE_DELTA);
        assertEquals(135., record.getQuad4WindRad(), ALLOWABLE_DOUBLE_DELTA);
        assertEquals(0., record.getClosedP(), ALLOWABLE_DOUBLE_DELTA);
        assertEquals(0., record.getRadClosedP(), ALLOWABLE_DOUBLE_DELTA);
        assertEquals(0., record.getMaxWindRad(), ALLOWABLE_DOUBLE_DELTA);
        assertEquals(120., record.getGust(), ALLOWABLE_DOUBLE_DELTA);
        assertEquals(20., record.getEyeSize(), ALLOWABLE_DOUBLE_DELTA);
		assertEquals ("C", record.getSubRegion());
        assertEquals(0., record.getMaxSeas(), ALLOWABLE_DOUBLE_DELTA);
		assertEquals ("RMT", record.getForecaster());
        assertEquals(25., record.getStormDrct(), ALLOWABLE_DOUBLE_DELTA);
        assertEquals(9., record.getStormSped(), ALLOWABLE_DOUBLE_DELTA);
		assertEquals ("HURCNAME", record.getStormName());
		assertEquals (" ", record.getStormDepth());
        assertEquals(12., record.getRadWave(), ALLOWABLE_DOUBLE_DELTA);
		assertEquals ("NEQ", record.getRadWaveQuad());
        assertEquals(130., record.getQuad1WaveRad(), ALLOWABLE_DOUBLE_DELTA);
        assertEquals(131., record.getQuad2WaveRad(), ALLOWABLE_DOUBLE_DELTA);
        assertEquals(132., record.getQuad3WaveRad(), ALLOWABLE_DOUBLE_DELTA);
        assertEquals(133., record.getQuad4WaveRad(), ALLOWABLE_DOUBLE_DELTA);
	}

}
