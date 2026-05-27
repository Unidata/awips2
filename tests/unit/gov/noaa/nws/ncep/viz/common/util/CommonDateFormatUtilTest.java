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
package gov.noaa.nws.ncep.viz.common.util;

import static org.junit.Assert.assertEquals;

import org.junit.Test;

/**
 * TODO Add Description
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Aug 28, 2014            jbernier     Initial creation
 * 
 * </pre>
 * 
 * @author jbernier
 * @version 1.0
 */

public class CommonDateFormatUtilTest {

    private static final int MIN15_IN_SECONDS = 900;

    private static final int HR6_IN_SECONDS = 21600;

    private static final int HR6_15MIN_IN_SECONDS = 22500;

    private static final int HR30_45MIN_IN_SECONDS = 110700;

    private static final int HR36_30MIN_IN_SECONDS = 131400;

    private static final int HR120_IN_SECONDS = 432000;

    private static final int HR136_30MIN_IN_SECONDS = 491400;

    /**
     * Test method for
     * {@link gov.noaa.nws.ncep.viz.common.util.GempakGrid#dbtimeToDattim(java.lang.String)}
     * .
     */
    @Test
    public void testDbTimeToDattim() {
        System.out
                .println("------------------Test-case DbTimeToDattim----------------");
        String dbTime1 = "2014-07-24 12:00:00.0";
        String dattim1 = CommonDateFormatUtil.dbtimeToDattim(dbTime1);
        System.out.println("AWIPS: " + dbTime1 + " to GEMPAK: " + dattim1);
        assertEquals("Actual time did not equal expected time string!",
                "140724/120000", dattim1);

        String dbTime2 = "2014-07-25 12:00:00.0 (6)";
        String dattim2 = CommonDateFormatUtil.dbtimeToDattim(dbTime2);
        System.out.println("AWIPS: " + dbTime2 + " to GEMPAK: " + dattim2);
        assertEquals("Actual time did not equal expected time string!",
                "140725/120000f006", dattim2);

        String dbTime3 = "2014-07-26 12:00:00.0 (36)";
        String dattim3 = CommonDateFormatUtil.dbtimeToDattim(dbTime3);
        System.out.println("AWIPS: " + dbTime3 + " to GEMPAK: " + dattim3);
        assertEquals("Actual time did not equal expected time string!",
                "140726/120000f036", dattim3);

        String dbTime4 = "2014-07-27_12:00:00.0 (120)";
        String dattim4 = CommonDateFormatUtil.dbtimeToDattim(dbTime4);
        System.out.println("AWIPS: " + dbTime4 + " to GEMPAK: " + dattim4);
        assertEquals("Actual time did not equal expected time string!",
                "140727/120000f120", dattim4);

        String dbTime5 = "2014-07-30_12:00:00.0 (0:15)";
        String dattim5 = CommonDateFormatUtil.dbtimeToDattim(dbTime5);
        System.out.println("AWIPS: " + dbTime5 + " to GEMPAK: " + dattim5);
        assertEquals("Actual time did not equal expected time string!",
                "140730/120000f00015", dattim5);

        String dbTime6 = "2014-08-08 12:00:00.0 (30:45)";
        String dattim6 = CommonDateFormatUtil.dbtimeToDattim(dbTime6);
        System.out.println("AWIPS: " + dbTime6 + " to GEMPAK: " + dattim6);
        assertEquals("Actual time did not equal expected time string!",
                "140808/120000f03045", dattim6);

        String dbTime7 = "2014-08-07 12:00:00.0 (036:30)";
        String dattim7 = CommonDateFormatUtil.dbtimeToDattim(dbTime7);
        System.out.println("AWIPS: " + dbTime7 + " to GEMPAK: " + dattim7);
        assertEquals("Actual time did not equal expected time string!",
                "140807/120000f03630", dattim7);
    }

    /**
     * Test method for
     * {@link gov.noaa.nws.ncep.viz.common.util.CommonDateFormatUtil#dattimToDbtime(java.lang.String)}
     * .
     */
    @Test
    public void testDattimToDbTime() {
        System.out
                .println("------------------Test-case DattimToDbTime----------------");
        String dattim1 = "140724/120000";
        String dbTime1 = CommonDateFormatUtil.dattimToDbtime(dattim1);
        System.out.println("GEMPAK: " + dattim1 + " to AWIPS: " + dbTime1);
        assertEquals("Actual time did not equal expected time string!",
                "2014-07-24 12:00:00.0", dbTime1);
        // Note: if there is no forecast time the date time is separated with a
        // space and not an underscore!

        String dattim2 = "140725/120000f006";
        String dbTime2 = CommonDateFormatUtil.dattimToDbtime(dattim2);
        System.out.println("GEMPAK: " + dattim2 + " to AWIPS: " + dbTime2);
        assertEquals("Actual time did not equal expected time string!",
                "2014-07-25_12:00:00.0_(6)", dbTime2);

        String dattim3 = "140726/120000f036";
        String dbTime3 = CommonDateFormatUtil.dattimToDbtime(dattim3);
        System.out.println("GEMPAK: " + dattim3 + " to AWIPS: " + dbTime3);
        assertEquals("Actual time did not equal expected time string!",
                "2014-07-26_12:00:00.0_(36)", dbTime3);

        String dattim4 = "140727/120000f120";
        String dbTime4 = CommonDateFormatUtil.dattimToDbtime(dattim4);
        System.out.println("GEMPAK: " + dattim4 + " to AWIPS: " + dbTime4);
        assertEquals("Actual time did not equal expected time string!",
                "2014-07-27_12:00:00.0_(120)", dbTime4);

        String dattim5 = "140730/120000f00615";
        String dbTime5 = CommonDateFormatUtil.dattimToDbtime(dattim5);
        System.out.println("GEMPAK: " + dattim5 + " to AWIPS: " + dbTime5);
        assertEquals("Actual time did not equal expected time string!",
                "2014-07-30_12:00:00.0_(6:15)", dbTime5);

        String dattim6 = "140808/120000f03045";
        String dbTime6 = CommonDateFormatUtil.dattimToDbtime(dattim6);
        System.out.println("GEMPAK: " + dattim6 + " to AWIPS: " + dbTime6);
        assertEquals("Actual time did not equal expected time string!",
                "2014-08-08_12:00:00.0_(30:45)", dbTime6);

        String dattim7 = "140807/120000f13630";
        String dbTime7 = CommonDateFormatUtil.dattimToDbtime(dattim7);
        System.out.println("GEMPAK: " + dattim7 + " to AWIPS: " + dbTime7);
        assertEquals("Actual time did not equal expected time string!",
                "2014-08-07_12:00:00.0_(136:30)", dbTime7);
    }

    /**
     * Test method for
     * {@link gov.noaa.nws.ncep.viz.common.util.CommonDateFormatUtil#getForecastTimeInSec(java.lang.String)}
     * .
     */
    @Test
    public void testGetForecastTimeInSec() {
        System.out
                .println("------------------Test-case GetForecastTimeInSec----------------");
        String aTime1 = "140724/120000";
        int forecastTimeInSec1 = CommonDateFormatUtil
                .getForecastTimeInSec(aTime1);
        System.out.println("Forecast time: " + aTime1 + " in Seconds: "
                + forecastTimeInSec1);
        assertEquals("Actual time in seconds did not equal expected time!", 0,
                forecastTimeInSec1);
        // Note: if there is no forecast time this method returns 0

        String aTime2 = "2014-07-25_12:00:00.0_(6)";
        int forecastTimeInSec2 = CommonDateFormatUtil
                .getForecastTimeInSec(aTime2);
        System.out.println("Forecast time: " + aTime2 + " in Seconds: "
                + forecastTimeInSec2);
        assertEquals("Actual time in seconds did not equal expected time!",
                HR6_IN_SECONDS, forecastTimeInSec2);

        String aTime3 = "140730/120000f00615";
        int forecastTimeInSec3 = CommonDateFormatUtil
                .getForecastTimeInSec(aTime3);
        System.out.println("Forecast time: " + aTime3 + " in Seconds: "
                + forecastTimeInSec3);
        assertEquals("Actual time in seconds did not equal expected time!",
                HR6_15MIN_IN_SECONDS, forecastTimeInSec3);

        String aTime4 = "2014-08-08_12:00:00.0_(30:45)";
        int forecastTimeInSec4 = CommonDateFormatUtil
                .getForecastTimeInSec(aTime4);
        System.out.println("Forecast time: " + aTime4 + " in Seconds: "
                + forecastTimeInSec4);
        assertEquals("Actual time in seconds did not equal expected time!",
                HR30_45MIN_IN_SECONDS, forecastTimeInSec4);

        String aTime5 = "140807/120000f13630";
        int forecastTimeInSec5 = CommonDateFormatUtil
                .getForecastTimeInSec(aTime5);
        System.out.println("Forecast time: " + aTime5 + " in Seconds: "
                + forecastTimeInSec5);
        assertEquals("Actual time in seconds did not equal expected time!",
                HR136_30MIN_IN_SECONDS, forecastTimeInSec5);
    }

    /**
     * Test method for
     * {@link gov.noaa.nws.ncep.viz.common.util.CommonDateFormatUtil#getForecastTimeString(java.lang.String)}
     * {@link gov.noaa.nws.ncep.viz.common.util.CommonDateFormatUtil#getForecastTimeString(int)}
     * .
     */
    @Test
    public void testGetForecastTimeString() {
        System.out
                .println("------------------Test-case GetForecastTimeString----------------");
        String aTime = "140725/120000f006";
        int secTime = HR6_IN_SECONDS;
        String forecastTimeFromString = CommonDateFormatUtil
                .getForecastTimeString(aTime);
        String forecastTimeFromInt = CommonDateFormatUtil
                .getForecastTimeString(secTime);
        System.out.println("Time: " + aTime + " forecast in seconds: "
                + secTime + " to time string: " + forecastTimeFromInt);
        assertEquals("Actual time did not equal expected time string!", "006",
                forecastTimeFromString);
        assertEquals("Actual time did not equal expected time string!", "006",
                forecastTimeFromInt);

        aTime = "2014-07-27_12:00:00.0 (120)";
        secTime = HR120_IN_SECONDS;
        forecastTimeFromString = CommonDateFormatUtil
                .getForecastTimeString(aTime);
        forecastTimeFromInt = CommonDateFormatUtil
                .getForecastTimeString(secTime);
        System.out.println("Time: " + aTime + " forecast in seconds: "
                + secTime + " to time string: " + forecastTimeFromInt);
        assertEquals("Actual time did not equal expected time string!", "120",
                forecastTimeFromString);
        assertEquals("Actual time did not equal expected time string!", "120",
                forecastTimeFromInt);

        aTime = "140730/120000f00015";
        secTime = MIN15_IN_SECONDS;
        forecastTimeFromString = CommonDateFormatUtil
                .getForecastTimeString(aTime);
        forecastTimeFromInt = CommonDateFormatUtil
                .getForecastTimeString(secTime);
        System.out.println("Time: " + aTime + " forecast in seconds: "
                + secTime + " to time string: " + forecastTimeFromInt);
        assertEquals("Actual time did not equal expected time string!",
                "00015", forecastTimeFromString);
        assertEquals("Actual time did not equal expected time string!",
                "00015", forecastTimeFromInt);

        aTime = "2014-08-07 12:00:00.0 (036:30)";
        secTime = HR36_30MIN_IN_SECONDS;
        forecastTimeFromString = CommonDateFormatUtil
                .getForecastTimeString(aTime);
        forecastTimeFromInt = CommonDateFormatUtil
                .getForecastTimeString(secTime);
        System.out.println("Time: " + aTime + " forecast in seconds: "
                + secTime + " to time string: " + forecastTimeFromInt);
        assertEquals("Actual time did not equal expected time string!",
                "03630", forecastTimeFromString);
        assertEquals("Actual time did not equal expected time string!",
                "03630", forecastTimeFromInt);
    }

    /**
     * Test method for
     * {@link gov.noaa.nws.ncep.viz.common.util.CommonDateFormatUtil#getForecastColonTimeString(java.lang.String)}
     * {@link gov.noaa.nws.ncep.viz.common.util.CommonDateFormatUtil#getForecastColonTimeString(int)}
     * .
     */
    @Test
    public void testGetForecastColonTimeString() {
        System.out
                .println("------------------Test-case GetForecastColonTimeString----------------");
        String aTime = "140725/120000f006";
        int secTime = HR6_IN_SECONDS;
        String forecastTimeFromString = CommonDateFormatUtil
                .getForecastColonTimeString(aTime);
        String forecastTimeFromInt = CommonDateFormatUtil
                .getForecastColonTimeString(secTime);
        System.out.println("Time: " + aTime + " forecast in seconds: "
                + secTime + " to time string: " + forecastTimeFromInt);
        assertEquals("Actual time did not equal expected time string!", "6",
                forecastTimeFromString);
        assertEquals("Actual time did not equal expected time string!", "6",
                forecastTimeFromInt);

        aTime = "2014-07-27_12:00:00.0 (120)";
        secTime = HR120_IN_SECONDS;
        forecastTimeFromString = CommonDateFormatUtil
                .getForecastColonTimeString(aTime);
        forecastTimeFromInt = CommonDateFormatUtil
                .getForecastColonTimeString(secTime);
        System.out.println("Time: " + aTime + " forecast in seconds: "
                + secTime + " to time string: " + forecastTimeFromInt);
        assertEquals("Actual time did not equal expected time string!", "120",
                forecastTimeFromString);
        assertEquals("Actual time did not equal expected time string!", "120",
                forecastTimeFromInt);

        aTime = "140730/120000f00015";
        secTime = MIN15_IN_SECONDS;
        forecastTimeFromString = CommonDateFormatUtil
                .getForecastColonTimeString(aTime);
        forecastTimeFromInt = CommonDateFormatUtil
                .getForecastColonTimeString(secTime);
        System.out.println("Time: " + aTime + " forecast in seconds: "
                + secTime + " to time string: " + forecastTimeFromInt);
        assertEquals("Actual time did not equal expected time string!", "0:15",
                forecastTimeFromString);
        assertEquals("Actual time did not equal expected time string!", "0:15",
                forecastTimeFromInt);

        aTime = "2014-08-07 12:00:00.0 (036:30)";
        secTime = HR36_30MIN_IN_SECONDS;
        forecastTimeFromString = CommonDateFormatUtil
                .getForecastColonTimeString(aTime);
        forecastTimeFromInt = CommonDateFormatUtil
                .getForecastColonTimeString(secTime);
        System.out.println("Time: " + aTime + " forecast in seconds: "
                + secTime + " to time string: " + forecastTimeFromInt);
        assertEquals("Actual time did not equal expected time string!",
                "36:30", forecastTimeFromString);
        assertEquals("Actual time did not equal expected time string!",
                "36:30", forecastTimeFromInt);
    }
}