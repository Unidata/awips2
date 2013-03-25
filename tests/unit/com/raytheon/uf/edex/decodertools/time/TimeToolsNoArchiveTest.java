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

import java.util.Calendar;
import java.util.TimeZone;

import org.junit.BeforeClass;
import org.junit.Test;

import com.raytheon.uf.edex.decodertools.time.TimeTools.ICheckAllowArchive;

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
 * 
 * </pre>
 * 
 * @author rferrel
 * @version 1.0
 */

public class TimeToolsNoArchiveTest {
    @BeforeClass
    public static void classSetup() {
        TimeTools.checkAllowArchive = new ICheckAllowArchive() {

            @Override
            public boolean allowArchive() {
                return false;
            }
        };
        ITimeService service = new ITimeService() {
            @Override
            public Calendar getCalendar() {
                final Calendar c = Calendar.getInstance();
                c.setTimeZone(TimeZone.getTimeZone(TimeTools.ZULU_TIMEZONE));
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

    @Test
    public void test() {
        assertFalse(TimeTools.allowArchive());
    }

    @Test
    public void test001() {
        String[] data = new String[] { "test001",
                "UANT01_CWAO_171653_225472224.20110717", "171653",
                "2011-06-17 16:53:00", };
        assertTrue(TimeToolsAllowArchiveTest.test(data));
    }

    @Test
    public void test002() {
        String[] data = new String[] { "test002",
                "UANT01_CWAO_312315_225472224.20110731", "312315",
                "2011-05-31 23:15:00", };
        assertTrue(TimeToolsAllowArchiveTest.test(data));
    }

    @Test
    public void test011() {
        ITimeService service = new ITimeService() {
            @Override
            public Calendar getCalendar() {
                final Calendar c = Calendar.getInstance();
                c.setTimeZone(TimeZone.getTimeZone(TimeTools.ZULU_TIMEZONE));
                c.set(Calendar.YEAR, 2011);
                c.set(Calendar.MONTH, Calendar.JULY);
                c.set(Calendar.DAY_OF_MONTH, 31);
                c.set(Calendar.HOUR_OF_DAY, 22);
                c.set(Calendar.MINUTE, 45);
                c.set(Calendar.SECOND, 32);
                c.set(Calendar.MILLISECOND, 268);

                return c;
            }
        };
        TimeTools.setTimeService(service);
        String[] data = new String[] { "test011",
                "UANT01_CWAO_312353_225472224.20110717", "312353",
                "2011-07-31 23:53:00", };
        assertTrue(TimeToolsAllowArchiveTest.test(data));
    }
}
