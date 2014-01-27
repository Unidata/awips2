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

import static org.hamcrest.Matchers.is;
import static org.junit.Assert.assertThat;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;

import org.junit.Test;

import com.raytheon.uf.common.status.IPerformanceStatusHandler;
import com.raytheon.uf.common.time.domain.TimePoints;
import com.raytheon.uf.common.time.domain.api.ITimePoint;

/**
 * Test {@link PerformanceTimerImpl}.
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
public class PerformanceTimerImplTest extends
        AbstractTimerTest<PerformanceTimerImpl> {

    @Test
    public void testLapWillReturnTimeSinceStartWhenFirstLap() {
        TimeUtilTest.installMockTimes(new long[] { 100L, 200L, 300L });

        timer.start();

        // 200L - 100L
        assertThat(timer.lap().getMillis(), is(100L));
    }

    @Test
    public void testLapWillReturnTimeSinceLastLapWhenNotFirstLap() {
        TimeUtilTest.installMockTimes(new long[] { 100L, 200L, 500L });

        timer.start();
        timer.lap();

        // 500L - 200L
        assertThat(timer.lap().getMillis(), is(300L));
    }

    @Test
    public void testLapMillisWillReturnTimeSinceStartWhenFirstLap() {
        TimeUtilTest.installMockTimes(new long[] { 100L, 200L, 300L });

        timer.start();

        // 200L - 100L
        assertThat(timer.lapMillis(), is(100L));
    }

    @Test
    public void testLapMillisWillReturnTimeSinceLastLapWhenNotFirstLap() {
        TimeUtilTest.installMockTimes(new long[] { 100L, 200L, 500L });

        timer.start();
        timer.lap();

        // 500L - 200L
        assertThat(timer.lapMillis(), is(300L));
    }

    @Test
    public void testLapMillisByNameWillReturnTimeSinceStartWhenFirstLap() {
        TimeUtilTest.installMockTimes(new long[] { 100L, 200L, 300L });

        timer.start();

        // 200L - 100L
        assertThat(timer.lapMillis("someName"), is(100L));
    }

    @Test
    public void testLapMillisByNameWillReturnTimeSinceLastLapWhenNotFirstLap() {
        TimeUtilTest.installMockTimes(new long[] { 100L, 200L, 500L });

        timer.start();
        timer.lap();

        // 500L - 200L
        assertThat(timer.lapMillis("someName"), is(300L));
    }

    @Test
    public void testMultipleLapsReturnsCorrectValue() {
        TimeUtilTest.installMockTimes(new long[] { 100L, 200L, 400L, 800L });

        timer.start();

        // 200L - 100L
        assertThat(timer.lap().getMillis(), is(100L));
        // 400L - 200L
        assertThat(timer.lap().getMillis(), is(200L));
        // 800L - 400L
        assertThat(timer.lap().getMillis(), is(400L));
    }

    @Test
    public void testLapsAreLoggedByName() {
        IPerformanceStatusHandler log = mock(IPerformanceStatusHandler.class);

        TimeUtilTest.installMockTimes(new long[] { 100L, 200L, 400L, 800L });

        timer.start();

        // 200L - 100L
        timer.lap();
        // 400L - 200L
        timer.lap();
        // 800L - 400L
        timer.lap();

        timer.stop();

        timer.logLaps("testLapsAreLoggedByName()", log);

        verify(log)
                .log("testLapsAreLoggedByName() total [700 ms] lap-1 [100 ms] lap-2 [200 ms] lap-3 [400 ms]");
    }

    @Test
    public void testNoLapsOnlyLogsTotalDuration() {
        IPerformanceStatusHandler log = mock(IPerformanceStatusHandler.class);

        TimeUtilTest.installMockTimes(new long[] { 100L, 200L });

        timer.start();
        timer.stop();

        timer.logLaps("testLapsAreLoggedByName()", log);

        verify(log).log("testLapsAreLoggedByName() total [100 ms]");
    }

    /**
     * {@inheritDoc}
     */
    @Override
    protected PerformanceTimerImpl getTimer() {
        return new PerformanceTimerImpl() {
            @Override
            protected ITimePoint getCurrentTime() {
                // Overridden to return mock times in the test
                return TimePoints.fromMillis(TimeUtil.currentTimeMillis());
            }

        };
    }

}
