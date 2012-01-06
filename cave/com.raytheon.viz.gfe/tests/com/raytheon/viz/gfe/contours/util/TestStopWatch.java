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
package com.raytheon.viz.gfe.contours.util;

import static org.junit.Assert.*;

import org.junit.After;
import org.junit.Before;
import org.junit.Test;

import com.raytheon.viz.gfe.contours.util.StopWatch;

/**
 * JUnit tests for selected methods in {@link StopWatch}.
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 27March2008  968        MW Fegan    Initial implementation.
 * 
 * </pre>
 *
 * @author mfegan
 * @version 1.0	
 */

public class TestStopWatch {
    StopWatch stopWatch = null;
    /**
     * @throws java.lang.Exception
     */
    @Before
    public void setUp() throws Exception {
        stopWatch = new StopWatch();
    }

    /**
     * @throws java.lang.Exception
     */
    @After
    public void tearDown() throws Exception {
        stopWatch = null;
    }
    /**
     * This is a general test method. The idea is to run a single stop watch
     * through several timing sequences. The total and average times are then
     * checked against independently saved system time values. 
     */
    @Test
    public void testStopWatch() {
        System.out.println("Testing general functionality");
        double total = 0;
        for (int i = 0; i < 4; i++) {
            stopWatch.start();
            long start = System.currentTimeMillis();
            try {
                Thread.sleep(2 * i + 15);
            } catch (InterruptedException e) {
                // just ignore this
            }
            total += (double)(System.currentTimeMillis() - start) / 1000.0;
            stopWatch.stop();
            try {
                Thread.sleep(10);
            } catch (InterruptedException e) {
                // just ignore this
            }
        }
        assertEquals("Verifying total time", total, stopWatch.totalTime, 1.0e-5);
        assertEquals("Verifying number of timings", stopWatch.numTimings, (int)4);
        assertEquals("Verifying average per timing", 
                     total/4.0, stopWatch.getAvgWallClockTime(), 1.0e-5);
    }
}
