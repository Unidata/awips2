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
package com.raytheon.uf.viz.datadelivery.utils;

import static com.google.common.collect.Lists.newArrayList;
import static com.google.common.collect.Sets.newHashSet;
import static org.junit.Assert.assertEquals;

import java.util.List;
import java.util.Set;
import java.util.concurrent.TimeUnit;

import org.junit.Test;

import com.raytheon.uf.common.datadelivery.registry.OpenDapGriddedDataSet;
import com.raytheon.uf.common.datadelivery.registry.OpenDapGriddedDataSetFixture;
import com.raytheon.uf.common.datadelivery.registry.Subscription;
import com.raytheon.uf.common.datadelivery.registry.SubscriptionFixture;
import com.raytheon.uf.common.datadelivery.registry.Time;

/**
 * Test {@link DataDeliveryUtils}.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jan 14, 2013 1286       djohnson     Initial creation
 * Jan 22, 2013 1519       djohnson     Add tests for getMaxLatency calculations.
 * 
 * </pre>
 * 
 * @author djohnson
 * @version 1.0
 */
public class DataDeliveryUtilsTest {

    // These constants are not using TimeUtil to make sure we get a
    // "second opinion" when using TimeUtil in the code under test
    private static final long THREE_HOURS_AS_MINUTES = TimeUnit.HOURS
            .toMinutes(3);

    private static final long MINUTES_PER_DAY = TimeUnit.DAYS.toMinutes(1);

    @Test
    public void maxLatencyReturnsMaxCyclicDifferenceForSubscribedToCycles() {
        List<Integer> cycleTimes = newArrayList();
        cycleTimes.add(0);
        cycleTimes.add(1);
        cycleTimes.add(4);

        Subscription subscription = SubscriptionFixture.INSTANCE.get();
        Time subTime = subscription.getTime();
        subTime.setCycleTimes(cycleTimes);

        assertEquals(THREE_HOURS_AS_MINUTES,
                DataDeliveryUtils.getMaxLatency(subscription));
    }

    @Test
    public void maxLatencyDefaultsToOneDayForSubscriptionWithOneCycle() {
        List<Integer> cycleTimes = newArrayList();
        cycleTimes.add(0);

        Subscription subscription = SubscriptionFixture.INSTANCE.get();
        Time subTime = subscription.getTime();
        subTime.setCycleTimes(cycleTimes);

        assertEquals(MINUTES_PER_DAY,
                DataDeliveryUtils.getMaxLatency(subscription));
    }

    @Test
    public void maxLatencyForDataSetWithOneCycleDefaultsToOneDay() {
        Set<Integer> cycleTimes = newHashSet();
        cycleTimes.add(0);

        OpenDapGriddedDataSet dataset = OpenDapGriddedDataSetFixture.INSTANCE
                .get();
        dataset.setCycles(cycleTimes);

        assertEquals(MINUTES_PER_DAY, DataDeliveryUtils.getMaxLatency(dataset));
    }

    @Test
    public void maxLatencyForDataSetWithMultipleCyclesReturnsMaxCyclicDifference() {
        Set<Integer> cycleTimes = newHashSet();
        cycleTimes.add(0);
        cycleTimes.add(1);
        cycleTimes.add(4);

        OpenDapGriddedDataSet dataset = OpenDapGriddedDataSetFixture.INSTANCE
                .get();
        dataset.setCycles(cycleTimes);

        assertEquals(THREE_HOURS_AS_MINUTES,
                DataDeliveryUtils.getMaxLatency(dataset));
    }

}
