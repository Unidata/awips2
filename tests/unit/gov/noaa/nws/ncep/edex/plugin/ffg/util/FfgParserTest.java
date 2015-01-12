package gov.noaa.nws.ncep.edex.plugin.ffg.util;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNull;
import gov.noaa.nws.ncep.common.dataplugin.ffg.FfgPrecip;
import gov.noaa.nws.ncep.common.dataplugin.ffg.FfgRecord;
import gov.noaa.nws.ncep.common.tools.IDecoderConstantsN;

import java.util.Calendar;
import java.util.Iterator;
import java.util.Set;

import org.junit.After;
import org.junit.Before;
import org.junit.Test;

public class FfgParserTest {
    FfgRecord record = new FfgRecord();

    Set<FfgPrecip> ppp;

    @Before
    public void setUp() throws Exception {
    }

    @After
    public void tearDown() throws Exception {
        record = null;
    }

    @Test
    public void test1ProcessAWIPSID() {
        /* Case I: Good report with a string of five characters */
        String str_t1 = "FFGMD \r\r\n" + "Dummying Strings";
        String retStr = FfgParser.processAwipsID(str_t1);
        assertEquals("FFGMD", retStr);
    }

    @Test
    public void test2ProcessAWIPSID() {
        /* Case II: Good report with a string of six characters */
        String str_t2 = "FFGNY2\r\r\n" + "Dummying Strings";
        String retStr = FfgParser.processAwipsID(str_t2);
        assertEquals("FFGNY2", retStr);
    }

    @Test
    public void test3ProcessAWIPSID() {
        /* Case III: Return null */
        String str_t3 = "FFGNY2 \r\r\n" + "Dummying Strings";
        String retStr = FfgParser.processAwipsID(str_t3);
        Object nu = null;
        assertNull((String) (nu), retStr);
    }

    @Test
    public void testProcessWMO() {
        String hdr = "FOUS61 KRHA 041504\r\r\n" + "FFGMD \r\r\n";
        byte[] wmo_hdr = hdr.getBytes();
        Calendar cal = null;
        try {
            FfgParser.processWMO(wmo_hdr, record, cal);
            String retHdr = record.getWmoHeader();
            assertEquals("FOUS61 KRHA 041504", retHdr);
            String issue_office = record.getIssueOffice();
            assertEquals("KRHA", issue_office);
            String nu = record.getDesignatorBBB();
            assertEquals("", nu);
        } catch (Exception e) {
            // empty block
        }
    }

    @Test
    public void test1ProcessPrecip() {
        /* Case I: good case. */
        String precip = "DEZ001    3.0/  4.2/  4.5/  4.8 / 6.0 :New Castle Co.\r\r\n";
        FfgParser.processPrecip(precip, record);
        ppp = record.getFfgP();
        Iterator<FfgPrecip> it = ppp.iterator();
        while (it.hasNext()) {
            FfgPrecip pr = it.next();
            assertEquals(3.0, pr.getFf01(), 0.01);
            assertEquals(4.2, pr.getFf03(), 0.01);
            assertEquals(4.5, pr.getFf06(), 0.01);
            assertEquals(4.8, pr.getFf12(), 0.01);
            assertEquals(6.0, pr.getFf24(), 0.01);
        }
    }

    @Test
    public void test2ProcessPrecip() {
        /* Case II: good case with "/" attached at the end of data */
        String precip = "DEZ001    3.0/  4.2/  4.5/  4.8 / 6.0 / :New Castle Co.\r\r\n";
        FfgParser.processPrecip(precip, record);
        ppp = record.getFfgP();
        Iterator<FfgPrecip> it = ppp.iterator();
        while (it.hasNext()) {
            FfgPrecip pr = it.next();
            assertEquals(3.0f, pr.getFf01(), 0.01);
            assertEquals(4.2f, pr.getFf03(), 0.01);
            assertEquals(4.5f, pr.getFf06(), 0.01);
            assertEquals(4.8f, pr.getFf12(), 0.01);
            assertEquals(6.0f, pr.getFf24(), 0.01);
        }
    }

    @Test
    public void test3ProcessPrecip() {
        /* Case III: Bad case with blank report */
        String precip = "DEZ001    3.0/  /  4.5/   / 6.0 /:New Castle Co.\r\r\n";
        FfgParser.processPrecip(precip, record);
        ppp = record.getFfgP();
        Iterator<FfgPrecip> it = ppp.iterator();
        while (it.hasNext()) {
            FfgPrecip pr = it.next();
            assertEquals(3.0, pr.getFf01(), 0.01);
            assertEquals(IDecoderConstantsN.FLOAT_MISSING.doubleValue(),
                    (double) pr.getFf03(), 0.0);
            assertEquals(4.5, pr.getFf06(), 0.01);
            assertEquals(IDecoderConstantsN.FLOAT_MISSING.doubleValue(),
                    (double) pr.getFf12(), 0.0);
            assertEquals(6.0, pr.getFf24(), 0.01);
        }
    }

    @Test
    public void test4ProcessPrecip() {
        /* Case IV: Bad case without "/" */
        String precip = "DEZ001    3.0 :New Castle Co.\r\r\n";
        FfgParser.processPrecip(precip, record);
        ppp = record.getFfgP();
        Iterator<FfgPrecip> it = ppp.iterator();
        while (it.hasNext()) {
            FfgPrecip pr = it.next();
            assertEquals(3.0, pr.getFf01(), 0.01);
            assertEquals(IDecoderConstantsN.FLOAT_MISSING.doubleValue(),
                    (double) pr.getFf03(), 0.0);
            assertEquals(IDecoderConstantsN.FLOAT_MISSING.doubleValue(),
                    (double) pr.getFf06(), 0.0);
            assertEquals(IDecoderConstantsN.FLOAT_MISSING.doubleValue(),
                    (double) pr.getFf12(), 0.0);
            assertEquals(IDecoderConstantsN.FLOAT_MISSING.doubleValue(),
                    (double) pr.getFf24(), 0.0);
        }
    }

    @Test
    public void test5ProcessPrecip() {
        /* Case V: Bad case with no report */
        String precip = "DEZ001   :New Castle Co.\r\r\n";
        FfgParser.processPrecip(precip, record);
        ppp = record.getFfgP();
        Iterator<FfgPrecip> it = ppp.iterator();
        while (it.hasNext()) {
            FfgPrecip pr = it.next();
            assertEquals(IDecoderConstantsN.FLOAT_MISSING.doubleValue(),
                    (double) pr.getFf01(), 0.0);
            assertEquals(IDecoderConstantsN.FLOAT_MISSING.doubleValue(),
                    (double) pr.getFf03(), 0.0);
            assertEquals(IDecoderConstantsN.FLOAT_MISSING.doubleValue(),
                    (double) pr.getFf06(), 0.0);
            assertEquals(IDecoderConstantsN.FLOAT_MISSING.doubleValue(),
                    (double) pr.getFf12(), 0.0);
            assertEquals(IDecoderConstantsN.FLOAT_MISSING.doubleValue(),
                    (double) pr.getFf24(), 0.0);
        }
    }
}