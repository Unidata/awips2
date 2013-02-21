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
package com.raytheon.uf.edex.datadelivery.bandwidth.util;

import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.util.Arrays;
import java.util.Calendar;
import java.util.Collection;
import java.util.Date;
import java.util.List;
import java.util.SortedSet;
import java.util.TreeSet;

import org.junit.After;
import org.junit.Before;
import org.junit.Test;

import com.raytheon.uf.common.datadelivery.registry.Network;
import com.raytheon.uf.common.datadelivery.registry.Subscription;
import com.raytheon.uf.common.datadelivery.registry.SubscriptionBuilder;
import com.raytheon.uf.common.localization.PathManagerFactoryTest;
import com.raytheon.uf.common.time.util.TimeUtil;
import com.raytheon.uf.common.time.util.TimeUtilTest;
import com.raytheon.uf.edex.datadelivery.bandwidth.IntegrationTestBandwidthContextFactory;
import com.raytheon.uf.edex.datadelivery.bandwidth.dao.BandwidthAllocation;
import com.raytheon.uf.edex.datadelivery.bandwidth.dao.BandwidthSubscription;
import com.raytheon.uf.edex.datadelivery.bandwidth.dao.IBandwidthDao;
import com.raytheon.uf.edex.datadelivery.bandwidth.retrieval.BandwidthMap;
import com.raytheon.uf.edex.datadelivery.bandwidth.retrieval.RetrievalManager;
import com.raytheon.uf.edex.datadelivery.bandwidth.retrieval.RetrievalPlan;
import com.raytheon.uf.edex.datadelivery.bandwidth.retrieval.RetrievalStatus;

/**
 * Test {@link BandwidthDaoUtil}.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Oct 24, 2012 1286       djohnson     Initial creation
 * Feb 07, 2013 1543       djohnson     Remove unnecessary test setup methods.
 * Feb 14, 2013 1595       djohnson     Fix retrieval plan/subscription time intersections.
 * 
 * </pre>
 * 
 * @author djohnson
 * @version 1.0
 */

public class BandwidthDaoUtilTest {

    private final IBandwidthDao mockDao = mock(IBandwidthDao.class);

    private final RetrievalManager retrievalManager = mock(RetrievalManager.class);

    private final BandwidthDaoUtil bandwidthDaoUtil = new BandwidthDaoUtil(
            mockDao, retrievalManager);

    private BandwidthMap map;

    private RetrievalPlan plan;

    @Before
    public void setUp() {
        TimeUtilTest.freezeTime(TimeUtil.MILLIS_PER_DAY * 2);

        SimpleAvailablityCalculator dataSetAvailabilityCalculator = new SimpleAvailablityCalculator();
        dataSetAvailabilityCalculator.setDelay(0);

        BandwidthUtil.getInstance().setDataSetAvailabilityCalculator(
                dataSetAvailabilityCalculator);
        PathManagerFactoryTest.initLocalization();

        map = BandwidthMap.load(IntegrationTestBandwidthContextFactory
                .getIntegrationTestBandwidthMapConfigFile());
        plan = new RetrievalPlan(Network.OPSNET, map, mockDao);
    }

    @After
    public void tearDown() {
        TimeUtilTest.resumeTime();
    }

    @Test
    public void testGetRetrievalTimesReturnsBaseReferenceTimesInPlanWindow() {
        // Make sure the subscription is "active" within the plan period
        Subscription subscription = new SubscriptionBuilder()
                .withActivePeriodStart(plan.getPlanStart().getTime())
                .withActivePeriodEnd(plan.getPlanEnd().getTime())
                .withSubscriptionStart(TimeUtil.newImmutableDate()).build();
        subscription.getTime().setCycleTimes(
                Arrays.asList(Integer.valueOf(9), Integer.valueOf(0)));

        TreeSet<Integer> cycles = new TreeSet<Integer>(subscription.getTime()
                .getCycleTimes());

        SortedSet<Calendar> subscriptionTimes = new TreeSet<Calendar>();

        subscriptionTimes = bandwidthDaoUtil.getRetrievalTimes(subscription,
                cycles, plan, subscriptionTimes);

        final List<Integer> daysOfTheYear = Arrays.asList(3, 4);
        verifySubscriptionTimesContainsCyclesForSpecifiedDays(daysOfTheYear,
                cycles, subscriptionTimes);
    }

    @Test
    public void testGetRetrievalTimesUsesSubscriptionStartWhenItIsMostRestrictive() {
        final Date startsTwoDaysIntoPlan = new Date(plan.getPlanStart()
                .getTimeInMillis() + TimeUtil.MILLIS_PER_DAY * 2);

        Subscription subscription = new SubscriptionBuilder()
                .withSubscriptionStart(startsTwoDaysIntoPlan)
                .withSubscriptionEnd(plan.getPlanEnd().getTime()).build();
        subscription.getTime().setCycleTimes(
                Arrays.asList(Integer.valueOf(9), Integer.valueOf(0)));

        TreeSet<Integer> cycles = new TreeSet<Integer>(subscription.getTime()
                .getCycleTimes());

        SortedSet<Calendar> subscriptionTimes = new TreeSet<Calendar>();

        subscriptionTimes = bandwidthDaoUtil.getRetrievalTimes(subscription,
                cycles, plan, subscriptionTimes);

        final List<Integer> daysOfTheYear = Arrays.asList(4);
        verifySubscriptionTimesContainsCyclesForSpecifiedDays(daysOfTheYear,
                cycles, subscriptionTimes);
        final List<Integer> notScheduledDays = Arrays.asList(3);
        verifySubscriptionTimesDoesNotContainCyclesForSpecifiedDays(
                notScheduledDays, cycles, subscriptionTimes);
    }

    @Test
    public void testGetRetrievalTimesUsesSubscriptionEndWhenItIsMostRestrictive() {
        final Date endsOneDayBeforePlan = new Date(plan.getPlanEnd()
                .getTimeInMillis() - TimeUtil.MILLIS_PER_DAY);

        Subscription subscription = new SubscriptionBuilder()
                .withSubscriptionStart(plan.getPlanStart().getTime())
                .withSubscriptionEnd(endsOneDayBeforePlan).build();
        subscription.getTime().setCycleTimes(
                Arrays.asList(Integer.valueOf(9), Integer.valueOf(0)));

        TreeSet<Integer> cycles = new TreeSet<Integer>(subscription.getTime()
                .getCycleTimes());

        SortedSet<Calendar> subscriptionTimes = new TreeSet<Calendar>();

        subscriptionTimes = bandwidthDaoUtil.getRetrievalTimes(subscription,
                cycles, plan, subscriptionTimes);

        final List<Integer> daysOfTheYear = Arrays.asList(3);
        verifySubscriptionTimesContainsCyclesForSpecifiedDays(daysOfTheYear,
                cycles, subscriptionTimes);
        final List<Integer> notScheduledDays = Arrays.asList(4);
        verifySubscriptionTimesDoesNotContainCyclesForSpecifiedDays(
                notScheduledDays, cycles, subscriptionTimes);
    }

    @Test
    public void testRemoveDoesNotRemoveFromRetrievalPlanIfInUnscheduledState() {
        BandwidthSubscription subDao = new BandwidthSubscription();
        subDao.setId(10L);

        BandwidthAllocation alloc1 = new BandwidthAllocation();
        alloc1.setId(1);
        alloc1.setStatus(RetrievalStatus.SCHEDULED);
        BandwidthAllocation alloc2 = new BandwidthAllocation();
        alloc2.setId(2);
        alloc2.setStatus(RetrievalStatus.UNSCHEDULED);

        when(mockDao.getBandwidthAllocations(subDao.getIdentifier()))
                .thenReturn(Arrays.asList(alloc1, alloc2));

        bandwidthDaoUtil.remove(subDao);

        // Remove the scheduled one
        verify(retrievalManager).remove(alloc1);
        // Don't remove the unscheduled one
        verify(retrievalManager, never()).remove(alloc2);
    }

    /**
     * Verifies the subscription times contains the cycles for the specified
     * days.
     * 
     * @param daysOfTheYear
     * @param cycles
     * @param subscriptionTimes
     */
    private void verifySubscriptionTimesContainsCyclesForSpecifiedDays(
            Collection<Integer> daysOfTheYear, Collection<Integer> cycles,
            SortedSet<Calendar> subscriptionTimes) {
        Calendar cal = TimeUtil.newCalendar();
        TimeUtil.minCalendarFields(cal, Calendar.MILLISECOND, Calendar.SECOND,
                Calendar.MINUTE);

        for (Integer dayOfTheYear : daysOfTheYear) {
            cal.set(Calendar.DAY_OF_YEAR, dayOfTheYear);

            for (Integer cycle : cycles) {
                cal.set(Calendar.HOUR_OF_DAY, cycle);
                assertTrue("Expected to find retrieval time of "
                        + BandwidthUtil.format(cal),
                        subscriptionTimes.contains(cal));
            }
        }
    }

    /**
     * Verifies the subscription times do not contain the cycles for the
     * specified days.
     * 
     * @param daysOfTheYear
     * @param cycles
     * @param subscriptionTimes
     */
    private void verifySubscriptionTimesDoesNotContainCyclesForSpecifiedDays(
            Collection<Integer> daysOfTheYear, Collection<Integer> cycles,
            SortedSet<Calendar> subscriptionTimes) {
        Calendar cal = TimeUtil.newCalendar();
        TimeUtil.minCalendarFields(cal, Calendar.MILLISECOND, Calendar.SECOND,
                Calendar.MINUTE);

        for (Integer dayOfTheYear : daysOfTheYear) {
            cal.set(Calendar.DAY_OF_YEAR, dayOfTheYear);

            for (Integer cycle : cycles) {
                cal.set(Calendar.HOUR_OF_DAY, cycle);
                assertFalse("Expected not to find retrieval time of "
                        + BandwidthUtil.format(cal),
                        subscriptionTimes.contains(cal));
            }
        }

    }
}
