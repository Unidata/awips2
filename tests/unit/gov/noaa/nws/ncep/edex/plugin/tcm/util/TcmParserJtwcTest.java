package gov.noaa.nws.ncep.edex.plugin.tcm.util;

import static org.junit.Assert.assertEquals;
import gov.noaa.nws.ncep.common.dataplugin.tcm.TcmPositionWinds;
import gov.noaa.nws.ncep.common.dataplugin.tcm.TcmRecord;
import gov.noaa.nws.ncep.common.tools.IDecoderConstantsN;
import gov.noaa.nws.ncep.edex.plugin.tcm.decoder.TcmSeparator;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.IOException;
import java.util.Calendar;
import java.util.Iterator;
import java.util.Set;

import org.apache.log4j.Logger;
import org.junit.After;
import org.junit.Before;
import org.junit.Ignore;
import org.junit.Test;

//TODO fix?
@Ignore
public class TcmParserJtwcTest {
    TcmSeparator sep;

    TcmRecord record = new TcmRecord();

    Set<TcmPositionWinds> tpw_pgtw;

    StringBuffer contents_pgtw = new StringBuffer();

    String data_pgtw = null;

    @Before
    public void initialize() {
        final Logger log = Logger.getLogger(getClass().getName());
        sep = new TcmSeparator();
        File file_pgtw = new File(
                "unit/gov/noaa/nws/ncep/edex/plugin/tcm/decoder/20090316.mar");

        BufferedReader reader = null;

        // Create data file
        try {
            reader = new BufferedReader(new FileReader(file_pgtw));
            String text = null;

            /*
             * Repeat until all lines is read. Add control characters.
             */
            while ((text = reader.readLine()) != null) {
                if (text.length() != 0) {
                    contents_pgtw.append(text).append("\r\r\n");
                }
            }
        } catch (FileNotFoundException e) {
            if (log.isInfoEnabled()) {
                log.info("File is not found");
            }
        } catch (IOException e) {
            if (log.isInfoEnabled()) {
                log.info("I/O Exception");
            }
        } finally {
            try {
                if (reader != null) {
                    reader.close();
                }
            } catch (IOException e) {
                if (log.isInfoEnabled()) {
                    log.info("I/O Exception");
                }
            }
        }
        sep = new TcmSeparator();
        data_pgtw = contents_pgtw.toString();
    }

    @After
    public void tearDown() throws Exception {
        record = null;
        sep = null;
        tpw_pgtw = null;
        contents_pgtw = null;
        data_pgtw = null;
    }

    @Test
    public void testPgtwProcessTcm() {

        TcmParser tp = new TcmParser();
        tp.processTcm(data_pgtw, record);
        assertEquals("WP", record.getBasin());
        assertEquals("KEN", record.getStormName());
        assertEquals("21P", record.getStormNumber());
        assertEquals("003", record.getAdvisoryNumber());
        assertEquals("TROPICAL CYCLONE", record.getStormType());
        assertEquals(null, record.getEyeSize());
        assertEquals(null, record.getMndTime());
        assertEquals(IDecoderConstantsN.INTEGER_MISSING,
                record.getCentralPressure());
        assertEquals(null, record.getNe12ft());
        assertEquals(null, record.getSe12ft());
        assertEquals(null, record.getSw12ft());
        assertEquals(null, record.getNw12ft());
    }

    @Test
    public void testPgtwProcessPositionWinds() {
        TcmParser tp = new TcmParser();
        byte[] data = data_pgtw.getBytes();
        Calendar cal = null;
        tp.processWMO(data, record, cal);
        tp.processTcm(data_pgtw, record);
        tpw_pgtw = record.getTcmPosWinds();
        Iterator<TcmPositionWinds> it = tpw_pgtw.iterator();

        // Check observation time
        cal = record.getObsTime();
        assertEquals(18, cal.get(Calendar.DAY_OF_MONTH));
        assertEquals(12, cal.get(Calendar.HOUR_OF_DAY));
        assertEquals(0, cal.get(Calendar.MINUTE));

        // Current condition
        TcmPositionWinds pw = it.next();
        assertEquals(-23.9, pw.getClat().doubleValue(), 0.0);
        assertEquals(-161.7, pw.getClon().doubleValue(), 0.0);
        assertEquals(35L, pw.getWindMax().longValue());
        assertEquals(45L, (long) pw.getGust().longValue());
        assertEquals(145L, (long) pw.getStormDrct().longValue());
        assertEquals(8L, pw.getStormSped().longValue());
        assertEquals(60L, record.getPositionAccuracy().longValue());
        assertEquals(true, record.getCorr());
        cal = pw.getValidTime();
        assertEquals(18, cal.get(Calendar.DAY_OF_MONTH));
        assertEquals(12, cal.get(Calendar.HOUR_OF_DAY));
        assertEquals(0, cal.get(Calendar.MINUTE));

        // 12 hr forecast
        pw = it.next();
        assertEquals(-25.7, pw.getClat().doubleValue());
        assertEquals(-160.4, pw.getClon().doubleValue());
        assertEquals(35L, pw.getWindMax().longValue());
        assertEquals(45L, pw.getGust().longValue());
        assertEquals(145L, pw.getStormDrct().longValue());
        assertEquals(16L, pw.getStormSped().longValue());
        cal = pw.getValidTime();
        assertEquals(19, cal.get(Calendar.DAY_OF_MONTH));
        assertEquals(0, cal.get(Calendar.HOUR_OF_DAY));
        assertEquals(0, cal.get(Calendar.MINUTE));
    }
}