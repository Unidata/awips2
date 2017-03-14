/**

 **/
package gov.noaa.nws.ncep.edex.plugin.ntrans.decoder;

import static org.junit.Assert.assertEquals;

import java.util.Calendar;
import java.util.TimeZone;

import org.junit.After;
import org.junit.AfterClass;
import org.junit.Before;
import org.junit.BeforeClass;
import org.junit.Test;

import com.raytheon.uf.common.time.DataTime;

/**
 * Unit Tests for NtransDecoder Class
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Aug 8, 2014            bhebbard     Initial creation
 * 
 * </pre>
 * 
 * @author bhebbard
 * @version 1.0
 */

public class NtransDecoderTest {

    private NtransDecoder decoder = null;

    private Calendar decodeTime = null;

    private String frameTimeString = null;

    private String metafileName = null;

    DataTime actual, expected;

    /**
     * @throws java.lang.Exception
     */
    @BeforeClass
    public static void setUpBeforeClass() throws Exception {
    }

    /**
     * @throws java.lang.Exception
     */
    @AfterClass
    public static void tearDownAfterClass() throws Exception {
    }

    /**
     * @throws java.lang.Exception
     */
    @Before
    public void setUp() throws Exception {
        decoder = new NtransDecoder();
        decodeTime = Calendar.getInstance(TimeZone.getTimeZone("GMT"));
        decodeTime.setLenient(false); // no nonsense here
        // default simulated decode time: 2014-08-16 23:57:59.327
        decodeTime.set(2014, Calendar.AUGUST, 16, 23, 57, 59);
        decodeTime.set(Calendar.MILLISECOND, 327);
        // set some other defaults
        frameTimeString = "";
        metafileName = "METAFILE_NAME_CONTAINS_NO_TIME_STRING";
    }

    /**
     * @throws java.lang.Exception
     */
    @After
    public void tearDown() throws Exception {
        decoder = null;
        decodeTime = null;
        frameTimeString = null;
        metafileName = null;
    }

    // @formatter:off
    /**
     * Now, the individual test cases!
     * 
     * In the comments which follow...
     *     C = cycle (or initial) time
     *     D = decode (or system) time
     *     V = valid time
     *     F = forecast hour (V - C)
     *     M = month boundary
     *
     */
    // @formatter:on

    /**
     * VALID CASES
     */

    /**
     * A "typical" scenario: C < D < V (Here F = 36)
     * 
     * Test method for
     * {@link gov.noaa.nws.ncep.edex.plugin.ntrans.decoder.NtransDecoder#createDataTime(java.lang.String, java.lang.String, java.util.Calendar)}
     * .
     */
    @Test
    public final void testCreateDataTime_01_C_D_V_F036_A() {
        // default simulated decode time: 2014-08-16 23:57:59.327
        frameTimeString = "18/00V036";
        //
        expected = new DataTime("2014-08-16 12:00:00.0 (36)");
        //
        actual = decoder.createDataTime(frameTimeString, metafileName,
                decodeTime);
        //
        assertEquals(null, decoder.yearFromFileName);
        assertEquals(null, decoder.monthFromFileName);
        assertEquals(null, decoder.dateFromFileName);
        assertEquals(null, decoder.hourFromFileName);
        assertEquals(expected, actual);
    }

    /**
     * C = V < D (F = 0)
     * 
     * Test method for
     * {@link gov.noaa.nws.ncep.edex.plugin.ntrans.decoder.NtransDecoder#createDataTime(java.lang.String, java.lang.String, java.util.Calendar)}
     * .
     */
    @Test
    public final void testCreateDataTime_02_C_V_D_F000_A() {
        decodeTime.set(2014, Calendar.AUGUST, 18, 04, 22, 13);
        frameTimeString = "18/00V000";
        //
        expected = new DataTime("2014-08-18 00:00:00.0 (0)");
        //
        actual = decoder.createDataTime(frameTimeString, metafileName,
                decodeTime);
        //
        assertEquals(null, decoder.yearFromFileName);
        assertEquals(null, decoder.monthFromFileName);
        assertEquals(null, decoder.dateFromFileName);
        assertEquals(null, decoder.hourFromFileName);
        assertEquals(expected, actual);
    }

    /**
     * C < M < V < D (F = 6)
     * 
     * Test method for
     * {@link gov.noaa.nws.ncep.edex.plugin.ntrans.decoder.NtransDecoder#createDataTime(java.lang.String, java.lang.String, java.util.Calendar)}
     * .
     */
    @Test
    public final void testCreateDataTime_03_C_M_V_D_F006_A() {
        decodeTime.set(2014, Calendar.SEPTEMBER, 01, 04, 22, 13);
        frameTimeString = "01/03V006";
        //
        expected = new DataTime("2014-08-31 21:00:00.0 (6)");
        //
        actual = decoder.createDataTime(frameTimeString, metafileName,
                decodeTime);
        //
        assertEquals(null, decoder.yearFromFileName);
        assertEquals(null, decoder.monthFromFileName);
        assertEquals(null, decoder.dateFromFileName);
        assertEquals(null, decoder.hourFromFileName);
        assertEquals(expected, actual);
    }

    /**
     * C < D < M < V (F = 96)
     * 
     * Test method for
     * {@link gov.noaa.nws.ncep.edex.plugin.ntrans.decoder.NtransDecoder#createDataTime(java.lang.String, java.lang.String, java.util.Calendar)}
     * .
     */
    @Test
    public final void testCreateDataTime_04_C_D_M_V_F096_A() {
        decodeTime.set(2014, Calendar.OCTOBER, 31, 04, 22, 13);
        frameTimeString = "03/12V096"; // V = 2014-11-03 12:00:00
        //
        expected = new DataTime("2014-10-30 12:00:00.0 (96)");
        //
        actual = decoder.createDataTime(frameTimeString, metafileName,
                decodeTime);
        //
        assertEquals(null, decoder.yearFromFileName);
        assertEquals(null, decoder.monthFromFileName);
        assertEquals(null, decoder.dateFromFileName);
        assertEquals(null, decoder.hourFromFileName);
        assertEquals(expected, actual);
    }

    /**
     * C < M < D < V (F = 240)
     * 
     * Test method for
     * {@link gov.noaa.nws.ncep.edex.plugin.ntrans.decoder.NtransDecoder#createDataTime(java.lang.String, java.lang.String, java.util.Calendar)}
     * .
     */
    @Test
    public final void testCreateDataTime_05_C_M_D_V_F240_A() {
        decodeTime.set(2017, Calendar.JULY, 01, 04, 22, 13);
        frameTimeString = "02/18V240"; // V = 2017-07-02 18:00:00
        //
        expected = new DataTime("2017-06-22 18:00:00.0 (240)");
        //
        actual = decoder.createDataTime(frameTimeString, metafileName,
                decodeTime);
        //
        assertEquals(null, decoder.yearFromFileName);
        assertEquals(null, decoder.monthFromFileName);
        assertEquals(null, decoder.dateFromFileName);
        assertEquals(null, decoder.hourFromFileName);
        assertEquals(expected, actual);
    }

    /**
     * C < V < M < D (F = 240)
     * 
     * Test method for
     * {@link gov.noaa.nws.ncep.edex.plugin.ntrans.decoder.NtransDecoder#createDataTime(java.lang.String, java.lang.String, java.util.Calendar)}
     * .
     */
    @Test
    public final void testCreateDataTime_06_C_V_M_D_F240_A() {
        decodeTime.set(2019, Calendar.FEBRUARY, 01, 04, 22, 13);
        frameTimeString = "21/18V240"; // V = 2019-01-21 18:00:00
        //
        expected = new DataTime("2019-01-11 18:00:00.0 (240)");
        //
        actual = decoder.createDataTime(frameTimeString, metafileName,
                decodeTime);
        //
        assertEquals(null, decoder.yearFromFileName);
        assertEquals(null, decoder.monthFromFileName);
        assertEquals(null, decoder.dateFromFileName);
        assertEquals(null, decoder.hourFromFileName);
        assertEquals(expected, actual);
    }

    /**
     * Time spec in metafile name: YYYYMMDDHH
     * 
     * Test method for
     * {@link gov.noaa.nws.ncep.edex.plugin.ntrans.decoder.NtransDecoder#createDataTime(java.lang.String, java.lang.String, java.util.Calendar)}
     * .
     */
    @Test
    public final void testCreateDataTime_10_YYYYMMDDHH_A() {
        decodeTime.set(2019, Calendar.FEBRUARY, 01, 04, 22, 13);
        metafileName = "ecens_prob_2013042712_atl";
        frameTimeString = "07/12V240"; // V = 2013-05-07 12:00:00
        //
        expected = new DataTime("2013-04-27 12:00:00.0 (240)");
        //
        actual = decoder.createDataTime(frameTimeString, metafileName,
                decodeTime);
        //
        assertEquals((Integer) 2013, decoder.yearFromFileName);
        assertEquals((Integer) 04, decoder.monthFromFileName);
        assertEquals((Integer) 27, decoder.dateFromFileName);
        assertEquals((Integer) 12, decoder.hourFromFileName);
        assertEquals(expected, actual);
    }

    /**
     * Time spec in metafile name: YYYYMMDD_HH
     * 
     * Test method for
     * {@link gov.noaa.nws.ncep.edex.plugin.ntrans.decoder.NtransDecoder#createDataTime(java.lang.String, java.lang.String, java.util.Calendar)}
     * .
     */
    @Test
    public final void testCreateDataTime_11_YYYYMMDD_HH_A() {
        decodeTime.set(2019, Calendar.FEBRUARY, 01, 04, 22, 13);
        metafileName = "cmc_20130429_00_chi_sta";
        frameTimeString = "09/00V240"; // V = 2013-05-09 00:00:00
        //
        expected = new DataTime("2013-04-29 00:00:00.0 (240)");
        //
        actual = decoder.createDataTime(frameTimeString, metafileName,
                decodeTime);
        //
        assertEquals((Integer) 2013, decoder.yearFromFileName);
        assertEquals((Integer) 04, decoder.monthFromFileName);
        assertEquals((Integer) 29, decoder.dateFromFileName);
        assertEquals((Integer) 00, decoder.hourFromFileName);
        assertEquals(expected, actual);
    }

    /**
     * Time spec in metafile name: YYMMDD_HH
     * 
     * Test method for
     * {@link gov.noaa.nws.ncep.edex.plugin.ntrans.decoder.NtransDecoder#createDataTime(java.lang.String, java.lang.String, java.util.Calendar)}
     * .
     */
    @Test
    public final void testCreateDataTime_12_YYMMDD_HH_A() {
        decodeTime.set(2019, Calendar.FEBRUARY, 01, 04, 22, 13);
        metafileName = "iceaccr_130428_18";
        frameTimeString = "08/18V240"; // V = 2013-05-08 18:00:00
        //
        expected = new DataTime("2013-04-28 18:00:00.0 (240)");
        //
        actual = decoder.createDataTime(frameTimeString, metafileName,
                decodeTime);
        //
        assertEquals((Integer) 2013, decoder.yearFromFileName);
        assertEquals((Integer) 04, decoder.monthFromFileName);
        assertEquals((Integer) 28, decoder.dateFromFileName);
        assertEquals((Integer) 18, decoder.hourFromFileName);
        assertEquals(expected, actual);
    }

    /**
     * Time spec in metafile name: YYYYMMDD
     * 
     * Test method for
     * {@link gov.noaa.nws.ncep.edex.plugin.ntrans.decoder.NtransDecoder#createDataTime(java.lang.String, java.lang.String, java.util.Calendar)}
     * .
     */
    @Test
    public final void testCreateDataTime_13_YYYYMMDD_A() {
        decodeTime.set(2019, Calendar.FEBRUARY, 01, 04, 22, 13);
        metafileName = "cpc_20130419_ecmwf";
        frameTimeString = "04/18V360"; // V = 2013-05-04 18:00:00 - F = 15 days
        //
        expected = new DataTime("2013-04-19 18:00:00.0 (360)");
        //
        actual = decoder.createDataTime(frameTimeString, metafileName,
                decodeTime);
        //
        assertEquals((Integer) 2013, decoder.yearFromFileName);
        assertEquals((Integer) 04, decoder.monthFromFileName);
        assertEquals((Integer) 19, decoder.dateFromFileName);
        assertEquals(null, decoder.hourFromFileName);
        assertEquals(expected, actual);
    }

    /**
     * Time spec in metafile name: YYMMDD
     * 
     * Test method for
     * {@link gov.noaa.nws.ncep.edex.plugin.ntrans.decoder.NtransDecoder#createDataTime(java.lang.String, java.lang.String, java.util.Calendar)}
     * .
     */
    @Test
    public final void testCreateDataTime_14_YYMMDD_A() {
        decodeTime.set(2019, Calendar.FEBRUARY, 01, 04, 22, 13);
        metafileName = "cpc_130419_ecmwf";
        frameTimeString = "04/18V360"; // V = 2013-05-04 18:00:00 - F = 15 days
        //
        expected = new DataTime("2013-04-19 18:00:00.0 (360)");
        //
        actual = decoder.createDataTime(frameTimeString, metafileName,
                decodeTime);
        //
        assertEquals((Integer) 2013, decoder.yearFromFileName);
        assertEquals((Integer) 04, decoder.monthFromFileName);
        assertEquals((Integer) 19, decoder.dateFromFileName);
        assertEquals(null, decoder.hourFromFileName);
        assertEquals(expected, actual);
    }

    /**
     * Time spec in metafile name: Date without hour, followed later by date
     * with hour
     * 
     * Test method for
     * {@link gov.noaa.nws.ncep.edex.plugin.ntrans.decoder.NtransDecoder#createDataTime(java.lang.String, java.lang.String, java.util.Calendar)}
     * .
     */
    @Test
    public final void testCreateDataTime_15_YYYYMMDD_YYYYMMDD_HH_A() {
        decodeTime.set(2019, Calendar.FEBRUARY, 01, 04, 22, 13);
        metafileName = "gfs_gfs.20140829_gfs_20140829_18";
        frameTimeString = "29/18V000"; // V = 2013-05-04 18:00:00 - F = 15 days
        //
        expected = new DataTime("2014-08-29 18:00:00.0 (0)");
        //
        actual = decoder.createDataTime(frameTimeString, metafileName,
                decodeTime);
        //
        assertEquals((Integer) 2014, decoder.yearFromFileName);
        assertEquals((Integer) 8, decoder.monthFromFileName);
        assertEquals((Integer) 29, decoder.dateFromFileName);
        assertEquals((Integer) 18, decoder.hourFromFileName);
        assertEquals(expected, actual);
    }

    /**
     * Time spec in metafile name: Date with hour, followed later by date
     * without hour
     * 
     * Test method for
     * {@link gov.noaa.nws.ncep.edex.plugin.ntrans.decoder.NtransDecoder#createDataTime(java.lang.String, java.lang.String, java.util.Calendar)}
     * .
     */
    @Test
    public final void testCreateDataTime_16_YYYYMMDD_HH_YYYYMMDD_A() {
        decodeTime.set(2019, Calendar.FEBRUARY, 01, 04, 22, 13);
        metafileName = "gfs_gfs.20140829_gfs_20140829_18";
        frameTimeString = "29/18V000"; // V = 2013-05-04 18:00:00 - F = 15 days
        //
        expected = new DataTime("2014-08-29 18:00:00.0 (0)");
        //
        actual = decoder.createDataTime(frameTimeString, metafileName,
                decodeTime);
        //
        assertEquals((Integer) 2014, decoder.yearFromFileName);
        assertEquals((Integer) 8, decoder.monthFromFileName);
        assertEquals((Integer) 29, decoder.dateFromFileName);
        assertEquals((Integer) 18, decoder.hourFromFileName);
        assertEquals(expected, actual);
    }

    /**
     * Test method for
     * {@link gov.noaa.nws.ncep.edex.plugin.ntrans.decoder.NtransDecoder#createDataTime(java.lang.String, java.lang.String)}
     * .
     */
    @Test
    public final void testCreateDataTimeStringString1() {
        DataTime a = decoder.createDataTime("14/22V036",
                "GFS_DUMMY_20140713_10");
        assertEquals((Integer) 2014, decoder.yearFromFileName);
        assertEquals((Integer) 07, decoder.monthFromFileName);
        assertEquals((Integer) 13, decoder.dateFromFileName);
        assertEquals((Integer) 10, decoder.hourFromFileName);
        DataTime x = new DataTime("2014-07-13 10:00:00.0 (36)");
        assertEquals(x, a);
    }

    /**
     * Test method for
     * {@link gov.noaa.nws.ncep.edex.plugin.ntrans.decoder.NtransDecoder#createDataTime(java.lang.String)}
     * .
     */
    @Test
    public final void testCreateDataTimeString1() {
        DataTime a = decoder.createDataTime("20140714/22V036");
        assertEquals(null, decoder.yearFromFileName);
        assertEquals(null, decoder.monthFromFileName);
        assertEquals(null, decoder.dateFromFileName);
        assertEquals(null, decoder.hourFromFileName);
        DataTime x = new DataTime("2014-07-13 10:00:00.0 (36)");
        assertEquals(x, a);

    }

    /**
     * Test method for
     * {@link gov.noaa.nws.ncep.edex.plugin.ntrans.decoder.NtransDecoder#createDataTime(java.lang.String, java.lang.String)}
     * .
     */
    @Test
    public final void testNormalizeFileName01() {
        String a = decoder
                .normalizeMetafileName("gfs_gfs.20140901_gfs_20140901_12_ak");
        String x = "gfs_20140901_12_ak";
        assertEquals(x, a);
    }

    /**
     * Test method for
     * {@link gov.noaa.nws.ncep.edex.plugin.ntrans.decoder.NtransDecoder#createDataTime(java.lang.String, java.lang.String)}
     * .
     */
    @Test
    public final void testNormalizeFileName02() {
        String a = decoder
                .normalizeMetafileName("gdas_gdas.20140901_gdas_20140901_12_na");
        String x = "gdas_20140901_12_na";
        assertEquals(x, a);
    }

    /**
     * Test method for
     * {@link gov.noaa.nws.ncep.edex.plugin.ntrans.decoder.NtransDecoder#createDataTime(java.lang.String, java.lang.String)}
     * .
     */
    @Test
    public final void testNormalizeFileName03() {
        String a = decoder
                .normalizeMetafileName("gefs_gefs.20140831_gefs_20140831_18_spag");
        String x = "gefs_20140831_18_spag";
        assertEquals(x, a);
    }

    /**
     * Test method for
     * {@link gov.noaa.nws.ncep.edex.plugin.ntrans.decoder.NtransDecoder#createDataTime(java.lang.String, java.lang.String)}
     * .
     */
    @Test
    public final void testNormalizeFileName04() {
        String a = decoder
                .normalizeMetafileName("gefs_gefs.20140902_gefs_avgspr_20140902_06_natl");
        String x = "gefs_avgspr_20140902_06_natl";
        assertEquals(x, a);
    }

    /**
     * Test method for
     * {@link gov.noaa.nws.ncep.edex.plugin.ntrans.decoder.NtransDecoder#createDataTime(java.lang.String, java.lang.String)}
     * .
     */
    @Test
    public final void testNormalizeFileName05() {
        String a = decoder
                .normalizeMetafileName("gfs_gfs.20140901_gfs_20140901_00_mar_skewt");
        String x = "gfs_20140901_00_mar_skewt";
        assertEquals(x, a);
    }

    /**
     * Test method for
     * {@link gov.noaa.nws.ncep.edex.plugin.ntrans.decoder.NtransDecoder#createDataTime(java.lang.String, java.lang.String)}
     * .
     */
    @Test
    public final void testNormalizeFileName06() {
        String a = decoder
                .normalizeMetafileName("gfs_gfs.20140901_gfsver_20140901_00");
        String x = "gfsver_20140901_00";
        assertEquals(x, a);
    }

    /**
     * Test method for
     * {@link gov.noaa.nws.ncep.edex.plugin.ntrans.decoder.NtransDecoder#createDataTime(java.lang.String, java.lang.String)}
     * .
     */
    @Test
    public final void testNormalizeFileName07() {
        String a = decoder
                .normalizeMetafileName("gfs_gfs.20140901_gfsver_20140901_18_na_mar");
        String x = "gfsver_20140901_18_na_mar";
        assertEquals(x, a);
    }

    /**
     * Test method for
     * {@link gov.noaa.nws.ncep.edex.plugin.ntrans.decoder.NtransDecoder#createDataTime(java.lang.String, java.lang.String)}
     * .
     */
    @Test
    public final void testNormalizeFileName08() {
        String a = decoder
                .normalizeMetafileName("ghm_ghm.20140901_ghm_20140901_00_invest99l");
        String x = "ghm_20140901_00_invest99l";
        assertEquals(x, a);
    }

    /**
     * Test method for
     * {@link gov.noaa.nws.ncep.edex.plugin.ntrans.decoder.NtransDecoder#createDataTime(java.lang.String, java.lang.String)}
     * .
     */
    @Test
    public final void testNormalizeFileName09() {
        String a = decoder
                .normalizeMetafileName("ghm_ghm.20140902_ghm_20140902_06_dolly05l_nest");
        String x = "ghm_20140902_06_dolly05l_nest";
        assertEquals(x, a);
    }

    /**
     * Test method for
     * {@link gov.noaa.nws.ncep.edex.plugin.ntrans.decoder.NtransDecoder#createDataTime(java.lang.String, java.lang.String)}
     * .
     */
    @Test
    public final void testNormalizeFileName10() {
        String a = decoder
                .normalizeMetafileName("hwrf_hwrf.20140901_hwrf_20140901_06_invest99l_nest");
        String x = "hwrf_20140901_06_invest99l_nest";
        assertEquals(x, a);
    }

    /**
     * Test method for
     * {@link gov.noaa.nws.ncep.edex.plugin.ntrans.decoder.NtransDecoder#createDataTime(java.lang.String, java.lang.String)}
     * .
     */
    @Test
    public final void testNormalizeFileName11() {
        String a = decoder
                .normalizeMetafileName("nam_nam.20140901_nam_20140901_00");
        String x = "nam_20140901_00";
        assertEquals(x, a);
    }

    /**
     * Test method for
     * {@link gov.noaa.nws.ncep.edex.plugin.ntrans.decoder.NtransDecoder#createDataTime(java.lang.String, java.lang.String)}
     * .
     */
    @Test
    public final void testNormalizeFileName12() {
        String a = decoder
                .normalizeMetafileName("nam_nam.20140901_nam_20140901_00_bwx");
        String x = "nam_20140901_00_bwx";
        assertEquals(x, a);
    }

    /**
     * Test method for
     * {@link gov.noaa.nws.ncep.edex.plugin.ntrans.decoder.NtransDecoder#createDataTime(java.lang.String, java.lang.String)}
     * .
     */
    @Test
    public final void testNormalizeFileName13() {
        String a = decoder
                .normalizeMetafileName("nam_nam.20140901_nam_20140901_00_mar_ver");
        String x = "nam_20140901_00_mar_ver";
        assertEquals(x, a);
    }

    /**
     * Test method for
     * {@link gov.noaa.nws.ncep.edex.plugin.ntrans.decoder.NtransDecoder#createDataTime(java.lang.String, java.lang.String)}
     * .
     */
    @Test
    public final void testNormalizeFileName14() {
        String a = decoder
                .normalizeMetafileName("rap_rap.20140831_rap_20140831_23_anlloop");
        String x = "rap_20140831_23_anlloop";
        assertEquals(x, a);
    }

    /**
     * Test method for
     * {@link gov.noaa.nws.ncep.edex.plugin.ntrans.decoder.NtransDecoder#createDataTime(java.lang.String, java.lang.String)}
     * .
     */
    @Test
    public final void testNormalizeFileName15() {
        String a = decoder
                .normalizeMetafileName("rap_rap.20140901_rap_20140901_01");
        String x = "rap_20140901_01";
        assertEquals(x, a);
    }

    /**
     * Test method for
     * {@link gov.noaa.nws.ncep.edex.plugin.ntrans.decoder.NtransDecoder#createDataTime(java.lang.String, java.lang.String)}
     * .
     */
    @Test
    public final void testNormalizeFileName16() {
        String a = decoder
                .normalizeMetafileName("ukmet.2014090_ukmet.2014090._ukmetver_20140901_00");
        // now this one is tricky!
        String x = "ukmetver_20140901_00";
        assertEquals(x, a);
    }

    /**
     * Test method for
     * {@link gov.noaa.nws.ncep.edex.plugin.ntrans.decoder.NtransDecoder#createDataTime(java.lang.String, java.lang.String)}
     * .
     */
    @Test
    public final void testNormalizeFileName17() {
        String a = decoder
                .normalizeMetafileName("ukmet_ukmet.20140902_ukmet_20140902_00_trop");
        String x = "ukmet_20140902_00_trop";
        assertEquals(x, a);
    }

    /**
     * Test method for
     * {@link gov.noaa.nws.ncep.edex.plugin.ntrans.decoder.NtransDecoder#createDataTime(java.lang.String, java.lang.String)}
     * .
     */
    @Test
    public final void testNormalizeFileName18() {
        String a = decoder
                .normalizeMetafileName("wave_wave.20140901_nww3_20140901_12");
        // yes, "wave" disappears here...
        String x = "nww3_20140901_12";
        assertEquals(x, a);
    }

    /**
     * Test method for
     * {@link gov.noaa.nws.ncep.edex.plugin.ntrans.decoder.NtransDecoder#createDataTime(java.lang.String, java.lang.String)}
     * .
     */
    @Test
    public final void testNormalizeFileName19() {
        String a = decoder
                .normalizeMetafileName("wave_wave.20140902_nww3_20140902_00_akw");
        String x = "nww3_20140902_00_akw";
        assertEquals(x, a);
    }

    /**
     * Test method for
     * {@link gov.noaa.nws.ncep.edex.plugin.ntrans.decoder.NtransDecoder#createDataTime(java.lang.String, java.lang.String)}
     * .
     */
    @Test
    public final void testNormalizeFileName20() {
        String a = decoder
                .normalizeMetafileName("opc_ens_20140901_18_gefs_prob_lo_spd");
        // should leave this one alone!
        String x = "opc_ens_20140901_18_gefs_prob_lo_spd";
        assertEquals(x, a);
    }

}
