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
package com.raytheon.uf.common.time.util;

import static org.junit.Assert.assertEquals;

import org.junit.After;
import org.junit.Before;
import org.junit.Ignore;
import org.junit.Test;

/**
 * Genericized and extracted from {@link TimerImplTest}.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jun 14, 2013 2095       djohnson     Initial creation
 * 
 * </pre>
 * 
 * @author djohnson
 * @version 1.0
 */
@Ignore
public abstract class AbstractTimerTest<TIMER extends ITimer> {

    protected TIMER timer = getTimer();

    @Before
    public void setUp() {
        TimeUtilTest.installMockTimes(new long[] { 100L, 200L });
    }

    @After
    public void cleanUp() {
        TimeUtilTest.resumeTime();
    }

    @Test(expected = IllegalStateException.class)
    public void testTimerImplCantBeStoppedIfHasntBeenStarted() {
        timer.stop();
    }

    @Test
    public void testRestartedTimerWillNotUseIntermediateTime() {
        TimeUtilTest.installMockTimes(new long[] { 50L, 75L, 150L, 250L });

        // Starts at 50L
        timer.start();
        // Stops at 75L: 25L elapsed time
        timer.stop();

        // Restart at 150L: still 25L elapsed time
        timer.start();

        // Stops again at 250L: 100L more elapsed time
        timer.stop();

        // 100L + 25L
        assertEquals(
                "The intermediate time the timer was stopped should not have counted toward the elapsed time!",
                125L, timer.getElapsedTime());
    }

    @Test
    public void testTimerImplReturnsElapsedTime() throws InterruptedException {

        timer.start();
        timer.stop();

        assertEquals("Invalid elapsed time returned!", 100L,
                timer.getElapsedTime());
    }

    @Test
    public void testResetWillAllowTimerToBeReused() {
        // The first difference will be 75-50 = 25L
        // The second difference will be 250-150 = 100L
        TimeUtilTest.installMockTimes(new long[] { 50L, 75L, 150L, 250L });

        timer.start();
        timer.stop();
        assertEquals("Incorrect elapsed time returned!", 25L,
                timer.getElapsedTime());

        timer.reset();

        timer.start();
        timer.stop();
        assertEquals("Incorrect elapsed time returned!", 100L,
                timer.getElapsedTime());
    }

    @Test
    public void testResetDoesNotUsePreviousElapsedTime() {
        TimeUtilTest.installMockTimes(new long[] { 50L, 75L, 150L, 250L });

        timer.start();
        timer.stop();
        assertEquals("Incorrect elapsed time returned!", 25L,
                timer.getElapsedTime());

        timer.reset();

        assertEquals("A reset timer should not have elapsed time!", 0,
                timer.getElapsedTime());
    }

    @Test(expected = IllegalStateException.class)
    public void testStartBeingCalledTwiceThrowsException() {

        timer.start();
        timer.start();
    }

    @Test
    public void testStoppingATimerTwiceDoesNotChangeStopTime() {
        TimeUtilTest.installMockTimes(new long[] { 100L, 200L, 300L });

        timer.start();
        timer.stop();
        // Elapsed time should still be 100L since the stop time should be stuck
        // at 200L
        timer.stop();

        assertEquals(
                "Expected the stop time to not have changed after the second invocation!",
                100L, timer.getElapsedTime());
    }

    @Test
    public void testGetElapsedTimeCanBeCalledOnARunningTimer() {
        TimeUtilTest.installMockTimes(new long[] { 100L, 200L, 300L });

        timer.start();

        // 200L - 100L
        assertEquals("Incorrect amount of time has been elapsed!", 100L,
                timer.getElapsedTime());

        timer.stop();

        // 300L - 100L
        assertEquals("Incorrect amount of time has been elapsed!", 200L,
                timer.getElapsedTime());
    }

    /**
     * Get the implementation under test.
     * 
     * @return the implementation instance
     */
    protected abstract TIMER getTimer();

}
