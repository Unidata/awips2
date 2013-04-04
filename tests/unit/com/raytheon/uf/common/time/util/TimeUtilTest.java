package com.raytheon.uf.common.time.util;

import static org.hamcrest.CoreMatchers.equalTo;
import static org.hamcrest.CoreMatchers.is;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertSame;
import static org.junit.Assert.assertThat;
import static org.junit.Assert.assertTrue;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

import java.util.Calendar;
import java.util.Date;
import java.util.TimeZone;
import java.util.concurrent.TimeUnit;

import org.junit.After;
import org.junit.Test;

import com.raytheon.edex.util.Util;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.common.time.SimulatedTime;
import com.raytheon.uf.common.util.TestUtil;

/**
 * 
 * Test {@link TimeUtil}. Can also be used to {@link #freezeTime()}. When
 * freezing time in a unit test, you should always have an "@After" annotated
 * method that will resume time (or you can have the call in a try/finally).
 * e.g.:
 * 
 * <pre>
 * {@code
 * 
 * "@After" // without quotes
 * public void afterTest() {
 *    // This is the after each test method of resuming time
 *    TimeUtilTest.resumeTime();
 * }
 * 
 * "@Test" // without quotes
 * public void testSomethingHappensAtFrozenTime() {
 *    TimeUtilTest.freezeTime();
 *    // Perform some testing while time is frozen
 * }
 * 
 * "@Test"
 * public void testSomethingHappensAtFrozenTimeTryFinally() {
 *   try {
 *      TimeUtilTest.freezeTime();
 *      // Perform some testing while time is frozen
 *   } finally {
 *      TimeUtilTest.resumeTime();
 *   }
 * }
 * 
 * }
 * 
 * </pre>
 * 
 * <pre>
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Aug 16, 2012 0743       djohnson     Initial creation
 * Sep 11, 2012 1154       djohnson     Tests for {@link TimeUtil#isNewerDay(Date, Date, TimeZone)}
 * 
 * </pre>
 * 
 * @author djohnson
 * @version 1.0
 */
public class TimeUtilTest {

    /**
     * Allows time to be manipulated so that tests can be run assuming specific
     * time frames.
     * 
     * @author djohnson
     * 
     */
    private static class MockTimeStrategy implements ITimeStrategy {

        private final long[] times;

        private int index;

        public MockTimeStrategy(long[] times) {
            if (times == null || times.length == 0) {
                throw new IllegalArgumentException(
                        "Must specify at least one time!");
            }
            this.times = times;
        }

        @Override
        public long currentTimeMillis() {
            long value = times[index];
            if (index < (times.length - 1)) {
                index++;
            }
            return value;
        }
    }

    /**
     * Mocks the times returned from {@link TimeUtil#currentTimeMillis()} such
     * that each successive call will return the next element in the array,
     * remaining at the last element in the array when reached. This is a
     * low-level call to setup specific time instances to be returned, e.g.:
     * 
     * <pre>
     * {@code 
     * TimeUtilTest.installMockTimes(new long[] 10L, 20L, 30L}); 
     * long time1 = TimeUtil.currentTimeMillis(); // 10L 
     * long time2 = TimeUtil.currentTimeMillis(); // 20L 
     * long time3 = TimeUtil.currentTimeMillis(): // 30L
     * long time4 = TimeUtil.currentTimeMillis(): // 30L, out of new times 
     * }
     * </pre>
     * 
     * @param times
     *            the times to return
     */
    public static void installMockTimes(long[] times) {
        TimeUtil.timeStrategy = new MockTimeStrategy(times);
    }

    /**
     * Mocks the times returned from {@link TimeUtil#currentTimeMillis()} such
     * that the first call will return the current time, and the second call
     * will return the time after the specified duration has elapsed, e.g.:
     * 
     * <pre>
     * {@code 
     * TimeUtilTest.installMockTimes(5, TimeUnit.MINUTES); 
     * long time1 = TimeUtil.currentTimeMillis(); // current time 
     * long time2 = TimeUtil.currentTimeMillis(); // 5 minutes in the future 
     * }
     * </pre>
     * 
     * @param amount
     *            the amount for the specified {@link TimeUnit}
     * @param unit
     *            the {@link TimeUnit}
     */
    public static void installMockTimes(long amount, TimeUnit unit) {
        long start = System.currentTimeMillis();
        long end = start + unit.toMillis(amount);
        
        TimeUtilTest.installMockTimes(new long[] { start, end });
    }

    /**
     * Freezes time so that all successive calls to
     * {@link TimeUtil#currentTimeMillis()} will return the current time. This
     * is useful to remove time sensitive issues from testing, for instance if
     * an operation that exceeds 10 seconds in production code should throw an
     * exception but is allowable in test code, then time should be frozen.
     * 
     * <pre>
     * {@code 
     * TimeUtilTest.freezeTime(); 
     * long time1 = TimeUtil.currentTimeMillis(); // 20000000L for example
     * 
     * Thread.sleep(1000L);
     *  
     * long time2 = TimeUtil.currentTimeMillis(); // 20000000L, still 
     * }
     * </pre>
     */
    public static void freezeTime() {
        freezeTime(System.currentTimeMillis());
    }

    /**
     * Freezes time so that all successive calls to
     * {@link TimeUtil#currentTimeMillis()} will return the specified time. This
     * is useful to test specific scenarios, such as errors that occur on
     * specific dates/times of the year (e.g. the switch to daylight savings
     * time).
     * 
     * <pre>
     * {@code 
     * TimeUtilTest.freezeTime(15151515L); 
     * long time1 = TimeUtil.currentTimeMillis(); // 15151515L
     * 
     * Thread.sleep(1000L);
     *  
     * long time2 = TimeUtil.currentTimeMillis(); // 15151515L, still 
     * }
     * </pre>
     * 
     * @param timeToFreezeAt
     *            the time that the system should be frozen to
     */
    public static void freezeTime(long timeToFreezeAt) {
        SimulatedTime systemTime = SimulatedTime.getSystemTime();
        systemTime.setFrozen(true);
        systemTime.setTime(timeToFreezeAt);
        installMockTimes(new long[] { timeToFreezeAt });
    }

    /**
     * Resumes using the system time, this can be called after time has been
     * frozen to resume noticing time differences when
     * {@link TimeUtil#currentTimeMillis()} is called.
     */
    public static void resumeTime() {
        TimeUtil.timeStrategy = TimeUtil.SYSTEM_TIME_STRATEGY;
        SimulatedTime.getSystemTime().setRealTime();
    }

    @After
    public void cleanUp() {
        TimeUtilTest.resumeTime();
    }

    @Test
    public void testFreezeTimeStopsTime() throws InterruptedException {
        TimeUtilTest.freezeTime();

        long firstTime = TimeUtil.currentTimeMillis();
        Thread.sleep(10L);
        long secondTime = TimeUtil.currentTimeMillis();

        assertEquals("Time should have been frozen!", firstTime, secondTime);
    }

    @Test
    public void testFreezeTimeStopsSimulatedTime() throws InterruptedException {
        TimeUtilTest.freezeTime();

        long firstTime = SimulatedTime.getSystemTime().getMillis();
        Thread.sleep(10L);
        long secondTime = SimulatedTime.getSystemTime().getMillis();

        assertEquals("SimulatedTime should have been frozen!", firstTime,
                secondTime);
    }

    @Test
    public void testResumeTimeWillResumeTime() throws InterruptedException {
        TimeUtilTest.freezeTime();

        long firstTime = TimeUtil.currentTimeMillis();
        TimeUtilTest.resumeTime();
        Thread.sleep(10L);
        long secondTime = TimeUtil.currentTimeMillis();

        TestUtil.assertNotEquals("Time should have resumed!", firstTime,
                secondTime);
    }

    @Test
    public void testResumeTimeWillResumeSimulatedTime() throws InterruptedException {
        TimeUtilTest.freezeTime();

        long firstTime = SimulatedTime.getSystemTime().getMillis();
        TimeUtilTest.resumeTime();
        Thread.sleep(10L);
        long secondTime = SimulatedTime.getSystemTime().getMillis();

        TestUtil.assertNotEquals("Time should have resumed!", firstTime,
                secondTime);
    }

    @Test
    public void testFreezeTimeAtSpecificTimeUsesTheParameter() {
        final long timeToFreezeAt = System.currentTimeMillis() - 20000L;

        TimeUtilTest.freezeTime(timeToFreezeAt);

        assertEquals("Expected time to be frozen at the specified time!",
                timeToFreezeAt, TimeUtil.currentTimeMillis());
    }

    @Test
    public void testFreezeTimeAtSpecificTimeUsesTheParameterForSimulatedTime() {
        final long timeToFreezeAt = System.currentTimeMillis() - 20000L;

        TimeUtilTest.freezeTime(timeToFreezeAt);

        assertEquals("Expected time to be frozen at the specified time!",
                timeToFreezeAt, SimulatedTime.getSystemTime()
                        .getMillis());
    }

    @Test
    public void testInstallMockTimesWithTimeUnitSetsUpCorrectTime() {
        final long twoHoursInMillis = Util.MILLI_PER_HOUR * 2;

        TimeUtilTest.installMockTimes(2, TimeUnit.HOURS);

        long time1 = TimeUtil.currentTimeMillis();
        long time2 = TimeUtil.currentTimeMillis();

        assertEquals(
                "Expected the second time to be 2 hours after the first time!",
                twoHoursInMillis, time2 - time1);
    }

    @Test
    public void testGetPriorityEnabledClockReturnsNullClockWhenPriorityNotEnabled() {
        IUFStatusHandler handler = mock(IUFStatusHandler.class);

        when(handler.isPriorityEnabled(Priority.VERBOSE)).thenReturn(false);

        ITimer clock = TimeUtil.getPriorityEnabledTimer(handler,
                Priority.VERBOSE);

        assertSame("Expected to receive the null clock!", TimeUtil.NULL_CLOCK,
                clock);
    }

    @Test
    public void testGetPriorityEnabledClockReturnsClockImplWhenPriorityEnabled() {
        IUFStatusHandler handler = UFStatus.getHandler(TimeUtilTest.class);
        ITimer clock = TimeUtil.getPriorityEnabledTimer(handler,
                Priority.CRITICAL);

        assertTrue("Expected to receive a real clock!",
                clock instanceof TimerImpl);
    }

    @Test
    public void testNewDateDelegatesToSimulatedTime() {
        long millis = 100L;
        SimulatedTime.getSystemTime().setFrozen(true);
        SimulatedTime.getSystemTime().setTime(millis);
        assertEquals(
                "TimeUtil does not appear to have delegated to SimulatedTime!",
                millis, TimeUtil.newDate().getTime());
    }

    @Test
    public void testNewImmutableDateDelegatesToSimulatedTime() {
        long millis = 100L;
        SimulatedTime.getSystemTime().setFrozen(true);
        SimulatedTime.getSystemTime().setTime(millis);
        assertEquals(
                "TimeUtil does not appear to have delegated to SimulatedTime!",
                millis, TimeUtil.newImmutableDate().getTime());
    }

    @Test
    public void testNewCalendarDelegatesToSimulatedTime() {
        long millis = 100L;
        SimulatedTime.getSystemTime().setFrozen(true);
        SimulatedTime.getSystemTime().setTime(millis);
        assertEquals(
                "TimeUtil does not appear to have delegated to SimulatedTime!",
                millis, TimeUtil.newCalendar().getTime().getTime());
    }

    @Test
    public void testIsNewerDayReturnsTrueForOneMillisecondIntoNewDate() {
        Date earlier = new ImmutableDate(TimeUtil.MILLIS_PER_DAY - 1L);
        Date later = new ImmutableDate(earlier.getTime() + 2L);

        assertTrue(
                "Expected the second date object to be found to be a newer date!",
                TimeUtil.isNewerDay(earlier, later,
                TimeZone.getTimeZone("GMT")));
    }

    @Test
    public void testIsNewerDayReturnsTrueForNewYear() {
        // Jan 11, 1970
        Date earlier = new ImmutableDate(TimeUtil.MILLIS_PER_DAY * 10);
        // Jan 01, 1971
        Date later = new ImmutableDate(TimeUtil.MILLIS_PER_DAY * 366);

        assertTrue(
                "Expected the second date object to be found to be a newer date!",
                TimeUtil.isNewerDay(earlier, later, TimeZone.getTimeZone("GMT")));
    }

    @Test
    public void testMinCalendarFieldsChangesRequestedFields() {
        Calendar cal = TimeUtil.newCalendar();
        cal.set(Calendar.YEAR, 2000);
        cal.set(Calendar.MONTH, 12);
        cal.set(Calendar.DAY_OF_MONTH, 12);
        cal.set(Calendar.HOUR_OF_DAY, 12);
        cal.set(Calendar.MINUTE, 12);
        cal.set(Calendar.SECOND, 12);
        cal.set(Calendar.MILLISECOND, 12);

        TimeUtil.minCalendarFields(cal, Calendar.MONTH, Calendar.HOUR_OF_DAY,
                Calendar.SECOND);

        assertCalendarFieldIsMinimum(cal, Calendar.MONTH);
        assertCalendarFieldIsMinimum(cal, Calendar.HOUR_OF_DAY);
        assertCalendarFieldIsMinimum(cal, Calendar.SECOND);
    }

    @Test
    public void testMinCalendarFieldsDoesNotChangeUnrequestedFields() {
        Calendar cal = TimeUtil.newCalendar();
        cal.set(Calendar.YEAR, 2000);
        cal.set(Calendar.MONTH, 12);
        cal.set(Calendar.DAY_OF_MONTH, 12);
        cal.set(Calendar.HOUR_OF_DAY, 12);
        cal.set(Calendar.MINUTE, 12);
        cal.set(Calendar.SECOND, 12);
        cal.set(Calendar.MILLISECOND, 12);

        TimeUtil.minCalendarFields(cal, Calendar.MONTH, Calendar.HOUR_OF_DAY,
                Calendar.SECOND);

        assertThat(cal.get(Calendar.YEAR), is(equalTo(2000)));
        assertThat(cal.get(Calendar.DAY_OF_MONTH), is(equalTo(12)));
        assertThat(cal.get(Calendar.MINUTE), is(equalTo(12)));
        assertThat(cal.get(Calendar.MILLISECOND), is(equalTo(12)));
    }

    @Test
    public void testMaxCalendarFieldsChangesRequestedFields() {
        Calendar cal = TimeUtil.newCalendar();
        cal.set(Calendar.YEAR, 2000);
        cal.set(Calendar.MONTH, 12);
        cal.set(Calendar.DAY_OF_MONTH, 12);
        cal.set(Calendar.HOUR_OF_DAY, 12);
        cal.set(Calendar.MINUTE, 12);
        cal.set(Calendar.SECOND, 12);
        cal.set(Calendar.MILLISECOND, 12);

        TimeUtil.maxCalendarFields(cal, Calendar.MONTH, Calendar.HOUR_OF_DAY,
                Calendar.SECOND);

        assertCalendarFieldIsMaximum(cal, Calendar.MONTH);
        assertCalendarFieldIsMaximum(cal, Calendar.HOUR_OF_DAY);
        assertCalendarFieldIsMaximum(cal, Calendar.SECOND);
    }

    @Test
    public void testMaxCalendarFieldsDoesNotChangeUnrequestedFields() {
        Calendar cal = TimeUtil.newCalendar();
        cal.set(Calendar.YEAR, 2000);
        cal.set(Calendar.MONTH, 12);
        cal.set(Calendar.DAY_OF_MONTH, 12);
        cal.set(Calendar.HOUR_OF_DAY, 12);
        cal.set(Calendar.MINUTE, 12);
        cal.set(Calendar.SECOND, 12);
        cal.set(Calendar.MILLISECOND, 12);

        TimeUtil.minCalendarFields(cal, Calendar.MONTH, Calendar.HOUR_OF_DAY,
                Calendar.SECOND);

        assertThat(cal.get(Calendar.YEAR), is(equalTo(2000)));
        assertThat(cal.get(Calendar.DAY_OF_MONTH), is(equalTo(12)));
        assertThat(cal.get(Calendar.MINUTE), is(equalTo(12)));
        assertThat(cal.get(Calendar.MILLISECOND), is(equalTo(12)));
    }

    /**
     * Assert a calendar field was set to its minimum allowed value.
     * 
     * @param cal
     *            the calendar
     * @param field
     *            the calendar field
     */
    private static void assertCalendarFieldIsMinimum(Calendar cal, int field) {
        assertThat(cal.get(field), is(equalTo(cal.getActualMinimum(field))));
    }

    /**
     * Assert a calendar field was set to its maximum allowed value.
     * 
     * @param cal
     *            the calendar
     * @param field
     *            the calendar field
     */
    private static void assertCalendarFieldIsMaximum(Calendar cal, int field) {
        assertThat(cal.get(field), is(equalTo(cal.getActualMaximum(field))));
    }
}
