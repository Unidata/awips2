/**
 * This software was developed and / or modified by Raytheon Company,
 * pursuant to Contract DG133W-05-CQ-1067 with the US Government.
 * 
 * U.S. EXPORT CONTROLLED TECHNICAL DATA
 * This software product contains export-restricted data whose
 * export/transfer/disclosure is restricted by U.S. law. Dissemination
 * to non-U.S. persons whether in the United States or abroad requires
 * an export license or other authorization.
 * 
 * Contractor Name:        Raytheon Company
 * Contractor Address:     6825 Pine Street, Suite 340
 *                         Mail Stop B8
 *                         Omaha, NE 68106
 *                         402.291.0100
 * 
 * See the AWIPS II Master Rights File ("Master Rights File.pdf") for
 * further licensing information.
 **/
package com.raytheon.edex.transform.shef;

import static org.junit.Assert.assertTrue;
import static org.mockito.Mockito.doNothing;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

import java.io.IOException;
import java.util.ArrayList;
import java.util.Calendar;
import java.util.List;
import java.util.TimeZone;

import javax.xml.transform.TransformerException;

import org.junit.Before;
import org.junit.Test;
import org.mockito.Mockito;

import com.raytheon.edex.esb.Headers;
import com.raytheon.edex.transform.shef.obs.MetarToShefConfigReader;
import com.raytheon.edex.transform.shef.obs.Utilities;
import com.raytheon.uf.common.dataplugin.obs.metar.MetarRecord;
import com.raytheon.uf.common.ohd.AppsDefaults;
import com.raytheon.uf.common.pointdata.spatial.SurfaceObsLocation;
import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.common.wmo.WMOHeader;

/**
 * JUnit test for Metar To Shef Transformer.
 * 
 * <pre>
 *
 * SOFTWARE HISTORY
 * 
 * Date          Ticket#  Engineer  Description
 * ------------- -------- --------- --------------------------------------------
 * May 30, 2018  6843     mduff     Initial creation
 * Jul 10, 2019  6843     randerso  Remove AppsDefaultWrapper. Changes for
 *                                  parseConfig signature change.
 * 
 * </pre>
 *
 * @author mpduff
 */

public class MetarToShefTransformerTest {

    private static final String TRACE_ID = "traceId";

    private static final int METAR_ID = 5;

    private AppsDefaults appsDefaults = mock(AppsDefaults.class);

    private MetarToShefConfigReader configReader = Mockito
            .spy(new MetarToShefConfigReader());

    @Before
    public void setup() throws IOException {
        doNothing().when(configReader)
                .readConfig(MetarToShefConfigReader.METAR_CFG);
        when(appsDefaults.getToken(AbstractShefTransformer.OPT_SHEF_ARC_DIR))
                .thenReturn("/tmp");

        configReader.parseConfig(getFileContents().iterator());
    }

    /*
     * Default parameters " -a -b -p1 -y2k -salias -p6 -p24 -round -w -strip "
     */

    @Test
    public void testTransformMetarOptionP1NoPrecip()
            throws TransformerException {
        Headers headers = new Headers();
        headers.put(TRACE_ID, TRACE_ID);
        headers.put(WMOHeader.INGEST_FILE_NAME,
                "SAUS70_KWBC_302000_26943118.2018053019");
        MetarRecord metar = getMetarRecordNoPrecip();

        MetarToShefTransformer transformer = new MetarToShefTransformer("-p1",
                appsDefaults, configReader);
        byte[] result = transformer.transformMetar(metar, headers);
        String answer = new String(result);
        assertTrue(answer.contains("TAIRZZZ  72"));
        assertTrue(answer.contains("TDIRZZZ  64"));
        assertTrue(answer.contains("PAIRZZZ  297.40"));
        assertTrue(answer.contains("PPHRZZZ  0.00"));
    }

    @Test
    public void testTransformMetarOptionPctNoPrecip()
            throws TransformerException {
        Headers headers = new Headers();
        headers.put(TRACE_ID, TRACE_ID);
        headers.put(WMOHeader.INGEST_FILE_NAME,
                "SAUS70_KWBC_302000_26943118.2018053019");
        MetarRecord metar = getMetarRecordNoPrecip();

        MetarToShefTransformer transformer = new MetarToShefTransformer(
                "-pct 8", appsDefaults, configReader);
        byte[] result = transformer.transformMetar(metar, headers);
        String answer = new String(result);
        assertTrue(answer.contains("TAIRZZZ  72"));
        assertTrue(answer.contains("TDIRZZZ  64"));
        assertTrue(answer.contains("PAIRZZZ  297.40"));
    }

    @Test
    public void testTransformMetarOptionPctAndP1NoPrecip()
            throws TransformerException {
        Headers headers = new Headers();
        headers.put(TRACE_ID, TRACE_ID);
        headers.put(WMOHeader.INGEST_FILE_NAME,
                "SAUS70_KWBC_302000_26943118.2018053019");
        MetarRecord metar = getMetarRecordNoPrecip();

        MetarToShefTransformer transformer = new MetarToShefTransformer(
                "-pct 8 -p1", appsDefaults, configReader);
        byte[] result = transformer.transformMetar(metar, headers);
        String answer = new String(result);
        assertTrue(answer.contains("TAIRZZZ  72"));
        assertTrue(answer.contains("TDIRZZZ  64"));
        assertTrue(answer.contains("PAIRZZZ  297.40"));
        assertTrue(answer.contains("PPHRZZZ  0.00"));
    }

    @Test
    public void testTransformMetarOptionP1() throws TransformerException {
        Headers headers = new Headers();
        headers.put(TRACE_ID, TRACE_ID);
        headers.put(WMOHeader.INGEST_FILE_NAME,
                "SAUS70_KWBC_302000_26943118.2018053019");
        MetarRecord metar = getMetarRecordWithPrecip();

        MetarToShefTransformer transformer = new MetarToShefTransformer("-p1",
                appsDefaults, configReader);
        byte[] result = transformer.transformMetar(metar, headers);
        String answer = new String(result);
        assertTrue(answer.contains("TAIRZZZ  72"));
        assertTrue(answer.contains("TDIRZZZ  64"));
        assertTrue(answer.contains("PAIRZZZ  297.40"));
        assertTrue(answer.contains("PPHRZZZ  1.00"));
        if (Utilities.isIn6HourWindow_1(metar.getTimeObs())) {
            assertTrue(answer.contains("PPQRZZZ   6.00"));
        }
        assertTrue(answer.contains("PPDRZZZ 24.00"));
    }

    @Test
    public void testTransformMetarOptionPct() throws TransformerException {
        Headers headers = new Headers();
        headers.put(TRACE_ID, TRACE_ID);
        headers.put(WMOHeader.INGEST_FILE_NAME,
                "SAUS70_KWBC_302000_26943118.2018053019");
        MetarRecord metar = getMetarRecordWithPrecip();

        MetarToShefTransformer transformer = new MetarToShefTransformer(
                "-pct 8", appsDefaults, configReader);
        byte[] result = transformer.transformMetar(metar, headers);
        String answer = new String(result);
        assertTrue(answer.contains("TAIRZZZ  72"));
        assertTrue(answer.contains("TDIRZZZ  64"));
        assertTrue(answer.contains("PAIRZZZ  297.40"));
        assertTrue(answer.contains("PPHRZZZ  1.00"));
        if (Utilities.isIn6HourWindow_1(metar.getTimeObs())) {
            assertTrue(answer.contains("PPQRZZZ   6.00"));
        }
        assertTrue(answer.contains("PPDRZZZ 24.00"));
    }

    @Test
    public void testTransformMetarOptionPctAndP1() throws TransformerException {
        Headers headers = new Headers();
        headers.put(TRACE_ID, TRACE_ID);
        headers.put(WMOHeader.INGEST_FILE_NAME,
                "SAUS70_KWBC_302000_26943118.2018053019");
        MetarRecord metar = getMetarRecordWithPrecip();

        MetarToShefTransformer transformer = new MetarToShefTransformer(
                "-pct 8 -p1", appsDefaults, configReader);
        byte[] result = transformer.transformMetar(metar, headers);
        String answer = new String(result);
        assertTrue(answer.contains("TAIRZZZ  72"));
        assertTrue(answer.contains("TDIRZZZ  64"));
        assertTrue(answer.contains("PAIRZZZ  297.40"));
        assertTrue(answer.contains("PPHRZZZ  1.00"));
        if (Utilities.isIn6HourWindow_1(metar.getTimeObs())) {
            assertTrue(answer.contains("PPQRZZZ   6.00"));
        }
        assertTrue(answer.contains("PPDRZZZ 24.00"));
    }

    private MetarRecord getMetarRecordNoPrecip() {
        String msg = "785\n";
        msg += "SAUS70 KWBC 311300 RRM\n";
        msg += "METAR\n";
        msg += "KOMA 311252Z VRB04KT 10SM FEW250 22/18 A2974 RMK AO2 SLP059 T02220178=\n";

        MetarRecord rec = new MetarRecord();
        rec.setReportType("METAR");
        rec.setId(METAR_ID);
        rec.setAutoStationType("A01");
        SurfaceObsLocation loc = new SurfaceObsLocation();
        loc.setStationId("KOMA");
        rec.setLocation(loc);
        Calendar cal = Calendar.getInstance();
        cal.setTimeZone(TimeZone.getTimeZone("GMT"));
        rec.setTimeObs(cal);
        DataTime dataTime = new DataTime();
        dataTime.setRefTime(cal.getTime());
        rec.setDataTime(dataTime);
        rec.setMessageData(msg);
        rec.setTemperature(22);
        rec.setDewPoint(18);
        rec.setAltimeter(297.4f);
        rec.setVisibility(10f);
        rec.setWindDir("90");
        rec.setWindSpeed(4);

        return rec;
    }

    private MetarRecord getMetarRecordWithPrecip() {
        String msg = "785\n";
        msg += "SAUS70 KWBC 311300 RRM\n";
        msg += "METAR\n";
        msg += "KOMA 311252Z VRB04KT 10SM FEW250 22/18 A2974 RMK AO2 SLP059 T02220178=\n";

        MetarRecord rec = new MetarRecord();
        rec.setReportType("METAR");
        rec.setId(METAR_ID);
        rec.setAutoStationType("A01");
        SurfaceObsLocation loc = new SurfaceObsLocation();
        loc.setStationId("KOMA");
        rec.setLocation(loc);
        Calendar cal = Calendar.getInstance();
        cal.setTimeZone(TimeZone.getTimeZone("GMT"));
        rec.setTimeObs(cal);
        DataTime dataTime = new DataTime();
        dataTime.setRefTime(cal.getTime());
        rec.setDataTime(dataTime);
        rec.setMessageData(msg);
        rec.setTemperature(22);
        rec.setDewPoint(18);
        rec.setAltimeter(297.4f);
        rec.setVisibility(10f);
        rec.setPrecip1Hour(1);
        rec.setPrecip3Hour(3f);
        rec.setPrecip6Hour(6f);
        rec.setPrecip24Hour(24f);
        rec.setWindDir("90");
        rec.setWindSpeed(4);

        return rec;
    }

    private List<String> getFileContents() {
        List<String> lines = new ArrayList<>();
        lines.add("# This is an absolute override file,");
        lines.add("# indicating that a higher priority version of the file ");
        lines.add(
                "# will completely replace a lower priority version of the file.");
        lines.add("");
        lines.add("/tmp/queue/metar/in");
        lines.add("/tmp/queue/metar/out");
        lines.add("/tmp/queue/metar/err");
        lines.add("+SAOOUT");
        lines.add("-ERRORFILE");
        lines.add("-SHEFPASS");
        lines.add(
                "TAIRZZ TD UP SD UD US TX TN PPT PPQ PPH PPD PA TAIRZR TAIRZH TAIRZP TAIRZY PTIR");
        lines.add(".begin_names");
        lines.add(".end_names");
        lines.add(".begin_sm_alias");
        lines.add(" .end_sm_alias");

        return lines;
    }
}
