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
package com.raytheon.uf.edex.decodertools.time;

import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;

import java.text.SimpleDateFormat;
import java.util.Calendar;
import java.util.TimeZone;

import org.junit.BeforeClass;
import org.junit.Test;

import com.raytheon.edex.esb.Headers;
import com.raytheon.uf.edex.decodertools.core.DecoderTools;
/* 08-26-2014 Issue 3365 ICheckAllowArchive class no longer exists.
 * REMOVED import com.raytheon.uf.edex.decodertools.time.TimeTools.ICheckAllowArchive;
 */

/**
 * Test {@link TimeTools}.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Feb 19, 2013 1636       rferrel     Initial creation
 * Aug 26, 2014 3365       ccody       Separate Data Delivery tests out of AWIPS 2 baseline.
 *                                     Required class (ICheckAllowArchive) no longer exists
 *                                     Test functionality that is no longer supported (i.e. will not compile)
 *                                     has been "deactivated" (commented out).
 * 
 * </pre>
 * 
 * @author rferrel
 * @version 1.0
 */

public class TimeToolsAllowArchiveTest {

    @BeforeClass
    public static void classSetup() {
        /* 08-26-2014 Issue 3365 ICheckAllowArchive class no longer exists.
         * REMOVED BEGIN
         *
        TimeTools.checkAllowArchive = new ICheckAllowArchive() {

            @Override
            public boolean allowArchive() {
                return true;
            }
        };
         * 08-26-2014 Issue 3365 ICheckAllowArchive class no longer exists.
         * REMOVED END 
         */
        ITimeService service = new ITimeService() {
            @Override
            public Calendar getCalendar() {
                final Calendar c = Calendar.getInstance();
                c.setTimeZone(TimeZone.getTimeZone("GMT"));
                c.set(Calendar.YEAR, 2011);
                c.set(Calendar.MONTH, Calendar.JULY);
                c.set(Calendar.DAY_OF_MONTH, 15);
                c.set(Calendar.HOUR_OF_DAY, 14);
                c.set(Calendar.MINUTE, 15);
                c.set(Calendar.SECOND, 32);
                c.set(Calendar.MILLISECOND, 268);

                return c;
            }
        };
        TimeTools.setTimeService(service);
    }

    /**
     * End of month, end of year
     * 
     * @return test passed status
     */
    static boolean test(String[] data) {
        String expected = data[3];
        SimpleDateFormat sdf = new SimpleDateFormat("yyyy-MM-dd HH:mm:ss");
        Headers header = new Headers();
        header.put(DecoderTools.INGEST_FILE_NAME, data[1]);

        /* 08-26-2014 Issue 3365 ICheckAllowArchive class no longer exists.
         * REMOVED BEGIN
         *
        sdf.setTimeZone(c.getTimeZone());

        String cs = sdf.format(c.getTime());
        return expected.equals(cs);
         * 08-26-2014 Issue 3365 ICheckAllowArchive class no longer exists.
         * REMOVED END 
         */
        return false;
    }

    /* 08-26-2014 Issue 3365 ICheckAllowArchive class no longer exists.
     * REMOVED BEGIN
     *
    @Test
    public void test() {
        assertTrue(TimeTools.allowArchive());
    }
     * 08-26-2014 Issue 3365 ICheckAllowArchive class no longer exists.
     * REMOVED END 
     */

    @Test
    public void test001() {
        String[] data = new String[] { "test001",
                "UANT01_CWAO_171653_225472224.20110717", "171653",
                "2011-07-17 16:53:00" };
        assertTrue(TimeToolsAllowArchiveTest.test(data));
    }

    @Test
    public void test001H() {
        String[] data = new String[] { "test001H",
                "UANT01_CWAO_171653_225472224.2012091722", "171653",
                "2012-09-17 16:53:00" };
        assertTrue(TimeToolsAllowArchiveTest.test(data));
    }

    @Test
    public void testShortTimestamp() {
        String[] data = new String[] { "testShortTimestamp",
                "UANT01_CWAO_171653_225472224.2011071", "171653",
                "2011-07-17 16:53:00" };
        assertFalse(TimeToolsAllowArchiveTest.test(data));
    }

    @Test
    public void testLongTimestamp() {
        String[] data = new String[] { "testLongTimestamp",
                "UANT01_CWAO_171653_225472224.20110711235", "171653",
                "2011-07-17 16:53:00" };
        assertFalse(TimeToolsAllowArchiveTest.test(data));
    }

    @Test
    public void testLongBadTimestamp() {
        String[] data = new String[] { "testBadTimestamp",
                "UANT01_CWAO_171653_225472224.201107114", "171653",
                "2011-07-17 16:53:00" };
        assertFalse(TimeToolsAllowArchiveTest.test(data));
    }

    @Test
    public void test002() {
        String[] data = new String[] { "test002",
                "UANT01_CWAO_312315_225472224.20110731", "312315",
                "2011-07-31 23:15:00", };
        assertTrue(TimeToolsAllowArchiveTest.test(data));
    }

    @Test
    public void test003() {
        String[] data = new String[] { "test003",
                "UANT01_CWAO_312315_225472224.20110801", "312315",
                "2011-07-31 23:15:00", };
        assertTrue(TimeToolsAllowArchiveTest.test(data));
    }

    @Test
    public void test004() {
        String[] data = new String[] { "test004",
                "UANT01_CWAO_170000_225472224.20110716", "170000",
                "2011-07-17 00:00:00", };
        assertTrue(TimeToolsAllowArchiveTest.test(data));
    }

    @Test
    public void test005() {
        String[] data = new String[] { "test005",
                "UANT01_CWAO_170000_225472224.20110717", "170000",
                "2011-07-17 00:00:00", };
        assertTrue(TimeToolsAllowArchiveTest.test(data));
    }

    @Test
    public void test006() {
        String[] data = new String[] { "test006",
                "UANT01_CWAO_100000_225472224.20111109", "100000",
                "2011-11-10 00:00:00", };
        assertTrue(TimeToolsAllowArchiveTest.test(data));
    }

    @Test
    public void test007() {
        String[] data = new String[] { "test007",
                "UANT01_CWAO_010000_225472224.20111231", "010000",
                "2012-01-01 00:00:00", };
        assertTrue(TimeToolsAllowArchiveTest.test(data));
    }

    @Test
    public void test008() {
        String[] data = new String[] { "test008",
                "UANT01_CWAO_312350_225472224.20120101", "312350",
                "2011-12-31 23:50:00", };
        assertTrue(TimeToolsAllowArchiveTest.test(data));
    }

    @Test
    public void test009() {
        String[] data = new String[] { "test009",
                "UANT01_CWAO_010259_225472224.20111231", "010259",
                "2012-01-01 02:59:00", };
        assertTrue(TimeToolsAllowArchiveTest.test(data));
    }

    @Test
    public void test010() {
        String[] data = new String[] { "test010",
                "UANT01_CWAO_010300_225472224.20111231", "010300",
                "2011-12-01 03:00:00", };
        assertTrue(TimeToolsAllowArchiveTest.test(data));
    }

    @Test
    public void test011() {
        String[] data = new String[] { "test011",
                "UANT01_CWAO_290050_225472224.20120228", "290050",
                "2012-02-29 00:50:00", };
        assertTrue(TimeToolsAllowArchiveTest.test(data));
    }

    @Test
    public void test012() {
        String[] data = new String[] { "test012",
                "UANT01_CWAO_010100_225472224.20120229", "010100",
                "2012-03-01 01:00:00", };
        assertTrue(TimeToolsAllowArchiveTest.test(data));
    }

}
