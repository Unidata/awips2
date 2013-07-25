/**
 * This Java class is the JUnit test for the AwwParser.
 *
 * <pre>
 *
 * L. Lin       04/09   Creation
 * </pre>
 *
 */
package gov.noaa.nws.ncep.edex.plugin.aww.util;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;
import gov.noaa.nws.ncep.common.dataplugin.aww.AwwFips;
import gov.noaa.nws.ncep.common.dataplugin.aww.AwwHVtec;
import gov.noaa.nws.ncep.common.dataplugin.aww.AwwLatlons;
import gov.noaa.nws.ncep.common.dataplugin.aww.AwwRecord;
import gov.noaa.nws.ncep.common.dataplugin.aww.AwwUgc;
import gov.noaa.nws.ncep.common.dataplugin.aww.AwwVtec;
import gov.noaa.nws.ncep.edex.tools.decoder.MndTime;

import java.util.ArrayList;
import java.util.Calendar;
import java.util.Iterator;
import java.util.TimeZone;

import org.junit.Before;
import org.junit.Ignore;
import org.junit.Test;

//TODO fix?
@Ignore
public class AwwParserTest {

    String ddhhmm;

    @Before
    public void initialize() {
        ddhhmm = "041540";
    }

    @Test
    public void testTransformTime() {
        final String zeroTime = "000000T0000";
        final String timeString = "080914T2157";

        /*
         * test the transform time case 1 - null
         */
        Calendar timeGroup = null;
        timeGroup = AwwParser.findEventTime(zeroTime);
        assertEquals(timeGroup, null);

        timeGroup = AwwParser.findEventTime(timeString);
        /*
         * test the transform time case 2 - normal
         */
        Calendar cal = Calendar.getInstance();
        cal.set(Calendar.YEAR, 2008);
        cal.set(Calendar.MONTH, 8);
        cal.set(Calendar.DATE, 14);
        cal.set(Calendar.HOUR_OF_DAY, 21);
        cal.set(Calendar.MINUTE, 57);
        cal.set(Calendar.SECOND, 0);
        cal.set(Calendar.MILLISECOND, 0);
        assertEquals(cal, timeGroup);

    }

    @Test
    public void testProcessATTN() {
        final String testBull = "MIC075-151705-\r\r\n"
                + "/O.EXT.KGRR.FL.W.0020.080914T2157Z-080915T1800Z/\r\r\n"
                + "/JACM4.2.ER.080914T2157Z.080915T0000Z.080915T0600Z.NR/\r\r\n"
                + "1105 AM EDT SUN SEP 14 2008\r\r\n"
                + "ATTN...WFO...BMX...HUN...JAN...MEG...OHX...\r\r\n"
                + "\r\r\n";
        final String attnLine = "BMX;HUN;JAN;MEG;OHX";

        String attention = AwwParser.processATTN(testBull);
        assertEquals(attention, attnLine);
    }

    @Test
    public void testProcessWMO() {
        Calendar mndTime = null;
        AwwRecord record;
        final String wmoHeader = "WOUS64";
        final String testBull = "WOUS64 KWNS 190404\n\n\r"
                + "/O.EXT.KGRR.FL.W.0020.080914T2157Z-080915T1800Z/\n\n\r"
                + "/JACM4.2.ER.080914T2157Z.080915T0000Z.080915T0600Z.NR/\r\r\n"
                + "1105 AM EDT SUN SEP 14 2008\r\r\n"
                + "ATTN...WFO...BMX...HUN...JAN...MEG...OHX...\r\r\n"
                + "\r\r\n";

        // Set MND (Mass News Disseminator) time string and convert it into
        // Calendar object
        MndTime mt = new MndTime(testBull.getBytes());
        mndTime = mt.getMndTime();

        record = new AwwRecord();

        record = AwwParser.processWMO(testBull, mndTime);
        String wmo = record.getWmoHeader();
        assertEquals(wmo, wmoHeader);
    }

    @Test
    public void testGetReportType() {

        // Case A
        final String testBull = "WOUS64 KWNS 190404\n\n\r" + "FLWDTX\n\n\r"
                + "SEVERE THUNDERSTORM OUTLINE UPDATE"
                + "NATIONAL WEATHER SERVICE DETROIT/PONTIAC MI" + "\r\r\n";

        String reportType = "SEVERE THUNDERSTORM OUTLINE UPDATE";
        String retType = AwwParser.getReportType(testBull);
        assertEquals(reportType, retType);

        // Case B
        final String bullB = "WOUS64 KWNS 190404\n\n\r" + "FLWDTX\n\n\r"
                + "FLOOD WARNING"
                + "NATIONAL WEATHER SERVICE DETROIT/PONTIAC MI" + "\r\r\n";

        reportType = "FLOOD WARNING";
        retType = AwwParser.getReportType(bullB);
        assertEquals(reportType, retType);

        // Case C
        final String bullC = "WOUS64 KWNS 190404\n\n\r" + "FLWDTX\n\n\r"
                + "TORNADO WATCH"
                + "NATIONAL WEATHER SERVICE DETROIT/PONTIAC MI" + "\r\r\n";

        reportType = "TORNADO WATCH";
        retType = AwwParser.getReportType(bullC);
        assertEquals(reportType, retType);

        // Case D
        final String bullD = "WOUS64 KWNS 190404\n\n\r" + "FLWDTX\n\n\r"
                + "FLASH FLOOD WATCH"
                + "NATIONAL WEATHER SERVICE DETROIT/PONTIAC MI" + "\r\r\n";

        reportType = "FLASH FLOOD WATCH";
        retType = AwwParser.getReportType(bullD);
        assertEquals(reportType, retType);

        // Case E
        final String bullE = "WOUS64 KWNS 190404\n\n\r" + "FLWDTX\n\n\r"
                + "WINTER STORM WARNING"
                + "NATIONAL WEATHER SERVICE DETROIT/PONTIAC MI" + "\r\r\n";

        reportType = "WINTER STORM WARNING";
        retType = AwwParser.getReportType(bullE);
        assertEquals(reportType, retType);

        // Case F
        final String bullF = "WOUS64 KWNS 190404\n\n\r" + "FLWDTX\n\n\r"
                + "WATCH COUNTY NOTIFICATION"
                + "NATIONAL WEATHER SERVICE DETROIT/PONTIAC MI" + "\r\r\n";

        reportType = "WATCH COUNTY NOTIFICATION";
        retType = AwwParser.getReportType(bullF);
        assertEquals(reportType, retType);

        // Case G
        final String bullG = "WOUS64 KWNS 190404\n\n\r" + "FLWDTX\n\n\r"
                + "DENSE FOG ADVISORY"
                + "NATIONAL WEATHER SERVICE DETROIT/PONTIAC MI" + "\r\r\n";

        reportType = "FOG ADVISORY";
        retType = AwwParser.getReportType(bullG);
        assertEquals(reportType, retType);

        // Case H
        final String bullH = "WOUS64 KWNS 190404\n\n\r" + "FLWDTX\n\n\r"
                + "HIGH WIND WARNING"
                + "NATIONAL WEATHER SERVICE DETROIT/PONTIAC MI" + "\r\r\n";

        reportType = "HIGH WIND WARNING";
        retType = AwwParser.getReportType(bullH);
        assertEquals(reportType, retType);

    }

    @Test
    public void testProcessFips() {

        final String testMndLine = "1005 AM EDT TUE SEP 16 2008\r\r\n";
        final String testUgcLine = "NDZ031-076-MIC094-162205-";

        MndTime mt = new MndTime(testMndLine.getBytes());
        Calendar mndTime = mt.getMndTime();

        AwwUgc testUgc = new AwwUgc();
        ArrayList<String> cfipsList = new ArrayList<String>();

        cfipsList.add("NDZ031");
        cfipsList.add("NDZ076");
        cfipsList.add("MIC094");

        AwwParser.processFips(testUgcLine, testUgc, mndTime);

        /*
         * test the product purge date
         */
        // Calendar cal = Calendar.getInstance();
        Calendar cal = Calendar.getInstance(TimeZone.getTimeZone("GMT"));
        cal.set(Calendar.YEAR, 2008);
        cal.set(Calendar.MONTH, 8);
        cal.set(Calendar.DATE, 16);
        cal.set(Calendar.HOUR_OF_DAY, 22);
        cal.set(Calendar.MINUTE, 5);
        cal.set(Calendar.SECOND, 0);
        cal.set(Calendar.MILLISECOND, 0);
        Calendar cc = testUgc.getProdPurgeTime();
        assertEquals(cal, cc);

        // test the county fips
        if (testUgc.getAwwFIPS() != null && testUgc.getAwwFIPS().size() > 0) {
            for (Iterator<AwwFips> iter = testUgc.getAwwFIPS().iterator(); iter
                    .hasNext();) {
                AwwFips cond = iter.next();
                String fips = cond.getFips();
                assertTrue(cfipsList.contains(fips));

            }
        }
    }

    @Test
    public void testProcessUgc() {

        final String testUgcLine = "MIC075-151705-\r\r\n";
        final String testSegment = "MIC075-151705-\r\r\n"
                + "/O.EXT.KGRR.FL.W.0020.080914T2157Z-080915T1800Z/\n\n\r"
                + "/JACM4.2.ER.080914T2157Z.080915T0000Z.080915T0600Z.NR/\r\r\n"
                + "1105 AM EDT SUN SEP 14 2008\r\r\n"
                + "ATTN...WFO...BMX...HUN...JAN...MEG...OHX...\r\r\n"
                + "\r\r\n";

        AwwUgc ugc = new AwwUgc();

        ArrayList<String> watchesList = new ArrayList<String>();
        Calendar mndTime = null;

        ugc = AwwParser.processUgc(testUgcLine, testSegment, mndTime,
                watchesList);

        String ugcLine = ugc.getUgc();
        assertEquals(ugcLine, testUgcLine);
    }

    @Test
    public void testProcessVtec() {

        final String testVtecLine = "/O.EXT.KGRR.FL.W.0020.080914T2157Z-080915T1800Z/\r\r\n";

        final String testSegment = "MIC075-151705-\r\r\n"
                + "/O.EXT.KGRR.FL.W.0020.080914T2157Z-080915T1800Z/\n\n\r"
                + "/JACM4.2.ER.080914T2157Z.080915T0000Z.080915T0600Z.NR/\r\r\n"
                + "1105 AM EDT SUN SEP 14 2008\r\r\n"
                + "ATTN...WFO...BMX...HUN...JAN...MEG...OHX...\r\r\n"
                + "\r\r\n";

        AwwVtec vtec = new AwwVtec();
        vtec = AwwParser.processVtec(testVtecLine, testSegment);

        // Compare the differences
        String vtecLine = vtec.getVtecLine();
        assertEquals(vtecLine, testVtecLine);

        String prodClass = vtec.getProductClass();
        assertEquals(prodClass, "O");

        String action = vtec.getAction();
        assertEquals(action, "EXT");

        String officeID = vtec.getOfficeID();
        assertEquals(officeID, "KGRR");

        String phenomena = vtec.getPhenomena();
        assertEquals(phenomena, "FL");

        String significance = vtec.getSignificance();
        assertEquals(significance, "W");

        String eventTrackingNumber = vtec.getEventTrackingNumber();
        assertEquals(eventTrackingNumber, "0020");

        Calendar startTime = vtec.getEventStartTime();
        Calendar calstart = Calendar.getInstance();
        calstart.set(Calendar.YEAR, 2008);
        calstart.set(Calendar.MONTH, 8);
        calstart.set(Calendar.DATE, 14);
        calstart.set(Calendar.HOUR_OF_DAY, 21);
        calstart.set(Calendar.MINUTE, 57);
        calstart.set(Calendar.SECOND, 0);
        calstart.set(Calendar.MILLISECOND, 0);
        assertEquals(calstart, startTime);

        Calendar endTime = vtec.getEventEndTime();
        Calendar calend = Calendar.getInstance();
        calend.set(Calendar.YEAR, 2008);
        calend.set(Calendar.MONTH, 8);
        calend.set(Calendar.DATE, 15);
        calend.set(Calendar.HOUR_OF_DAY, 18);
        calend.set(Calendar.MINUTE, 00);
        calend.set(Calendar.SECOND, 0);
        calend.set(Calendar.MILLISECOND, 0);
        assertEquals(calend, endTime);
    }

    @Test
    public void testProcessLatlons() {

        final String testLatlons = "LAT...LON 4257 8255 4255 8265 4264 8269 4265 8268";

        AwwUgc testUgc = new AwwUgc();
        ArrayList<Float> flatList = new ArrayList<Float>();
        ArrayList<Float> flonList = new ArrayList<Float>();

        int[] latlonIndex = new int[1];

        latlonIndex[0] = 0;

        flatList.add((float) (4257 / 100.0));
        flonList.add((float) (-8255 / 100.0));
        flatList.add((float) (4255 / 100.0));
        flonList.add((float) (-8265 / 100.0));
        flatList.add((float) (4264 / 100.0));
        flonList.add((float) (-8269 / 100.0));
        flatList.add((float) (4265 / 100.0));
        flonList.add((float) (-8268 / 100.0));

        AwwParser.processLatlons(testLatlons, testUgc, latlonIndex);

        // test the county fips
        if (testUgc.getAwwLatLon() != null && testUgc.getAwwLatLon().size() > 0) {
            for (Iterator<AwwLatlons> iter = testUgc.getAwwLatLon().iterator(); iter
                    .hasNext();) {
                AwwLatlons cond = iter.next();
                assertTrue(flatList.contains(cond.getLat()));
                assertTrue(flonList.contains(cond.getLon()));
            }
        }
    }

    @Test
    public void testProcessHVtec() {
        final String testVtecLine = "/O.EXT.KGRR.FL.W.0020.080914T2157Z-080915T1800Z/\r\r\n";
        final String testHVtecLine = "/JACM4.2.ER.080914T2157Z.080915T0000Z.080915T0600Z.NR/";
        final String testSegment = "MIC075-151705-\r\r\n"
                + "/O.EXT.KGRR.FL.W.0020.080914T2157Z-080915T1800Z/\n\n\r"
                + "/JACM4.2.ER.080914T2157Z.080915T0000Z.080915T0600Z.NR/\r\r\n"
                + "1105 AM EDT SUN SEP 14 2008\r\r\n"
                + "ATTN...WFO...BMX...HUN...JAN...MEG...OHX...\r\r\n"
                + "\r\r\n";

        AwwHVtec hvtec = new AwwHVtec();
        AwwVtec vtec = new AwwVtec();

        vtec = AwwParser.processVtec(testVtecLine, testSegment);

        if (vtec.getAwwHVtecLine() != null && vtec.getAwwHVtecLine().size() > 0) {
            for (Iterator<AwwHVtec> iter = vtec.getAwwHVtecLine().iterator(); iter
                    .hasNext();) {
                hvtec = iter.next();

                // Compare the differences
                String hvtecLine = hvtec.getHvtecLine();
                assertEquals(hvtecLine, testHVtecLine);

                String floodSeverity = hvtec.getFloodSeverity();
                assertEquals(floodSeverity, "2");

                String immediateCause = hvtec.getImmediateCause();
                assertEquals(immediateCause, "ER");

                Calendar startTime = hvtec.getEventStartTime();
                Calendar calstart = Calendar.getInstance();
                calstart.set(Calendar.YEAR, 2008);
                calstart.set(Calendar.MONTH, 8);
                calstart.set(Calendar.DATE, 14);
                calstart.set(Calendar.HOUR_OF_DAY, 21);
                calstart.set(Calendar.MINUTE, 57);
                calstart.set(Calendar.SECOND, 0);
                calstart.set(Calendar.MILLISECOND, 0);
                assertEquals(calstart, startTime);

                Calendar crestTime = hvtec.getEventCrestTime();
                Calendar calcrest = Calendar.getInstance();
                calcrest.set(Calendar.YEAR, 2008);
                calcrest.set(Calendar.MONTH, 8);
                calcrest.set(Calendar.DATE, 15);
                calcrest.set(Calendar.HOUR_OF_DAY, 00);
                calcrest.set(Calendar.MINUTE, 00);
                calcrest.set(Calendar.SECOND, 0);
                calcrest.set(Calendar.MILLISECOND, 0);
                assertEquals(calcrest, crestTime);

                Calendar endTime = hvtec.getEventEndTime();
                Calendar calend = Calendar.getInstance();
                calend.set(Calendar.YEAR, 2008);
                calend.set(Calendar.MONTH, 8);
                calend.set(Calendar.DATE, 15);
                calend.set(Calendar.HOUR_OF_DAY, 06);
                calend.set(Calendar.MINUTE, 00);
                calend.set(Calendar.SECOND, 0);
                calend.set(Calendar.MILLISECOND, 0);
                assertEquals(calend, endTime);

            }
        }
    }

}
