package com.raytheon.uf.common.time.util;

import static org.junit.Assert.assertEquals;

import org.junit.After;
import org.junit.Before;
import org.junit.Test;

/**
 * 
 * Test {@link TimerImpl}.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Aug 16, 2012  0743      djohnson     Initial creation
 * 
 * </pre>
 * 
 * @author djohnson
 * @version 1.0
 */
public class TimerImplTest {

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
        TimerImpl timer = new TimerImpl();
        timer.stop();
	}

    @Test
    public void testRestartedTimerWillNotUseIntermediateTime() {
        TimeUtilTest.installMockTimes(new long[] { 50L, 75L, 150L, 250L });

        TimerImpl timer = new TimerImpl();
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
        TimerImpl timer = new TimerImpl();
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

        TimerImpl timer = new TimerImpl();
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

        TimerImpl timer = new TimerImpl();
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
        TimerImpl timer = new TimerImpl();
        timer.start();
        timer.start();
	}

	@Test
    public void testStoppingATimerTwiceDoesNotChangeStopTime() {
		TimeUtilTest.installMockTimes(new long[] { 100L, 200L, 300L });

        TimerImpl timer = new TimerImpl();
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
    public void testGetElapsedTimeCanBeCalledOnARunningLock() {
        TimeUtilTest.installMockTimes(new long[] { 100L, 200L, 300L });

        TimerImpl timer = new TimerImpl();
        timer.start();

        // 200L - 100L
        assertEquals("Incorrect amount of time has been elapsed!", 100L,
                timer.getElapsedTime());

        timer.stop();

        // 300L - 100L
        assertEquals("Incorrect amount of time has been elapsed!", 200L,
                timer.getElapsedTime());
    }
}
