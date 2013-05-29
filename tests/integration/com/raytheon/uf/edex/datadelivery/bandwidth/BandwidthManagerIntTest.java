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
package com.raytheon.uf.edex.datadelivery.bandwidth;

import static org.hamcrest.Matchers.empty;
import static org.hamcrest.Matchers.equalTo;
import static org.hamcrest.Matchers.greaterThan;
import static org.hamcrest.Matchers.is;
import static org.hamcrest.Matchers.not;
import static org.hamcrest.Matchers.sameInstance;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertThat;
import static org.junit.Assert.assertTrue;

import java.text.ParseException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Calendar;
import java.util.Collections;
import java.util.Comparator;
import java.util.ConcurrentModificationException;
import java.util.Date;
import java.util.Iterator;
import java.util.List;
import java.util.SortedSet;
import java.util.concurrent.ArrayBlockingQueue;
import java.util.concurrent.BlockingQueue;
import java.util.concurrent.CountDownLatch;

import org.junit.Test;

import com.raytheon.uf.common.datadelivery.registry.DataDeliveryRegistryObjectTypes;
import com.raytheon.uf.common.datadelivery.registry.GriddedDataSetMetaData;
import com.raytheon.uf.common.datadelivery.registry.Network;
import com.raytheon.uf.common.datadelivery.registry.OpenDapGriddedDataSetMetaData;
import com.raytheon.uf.common.datadelivery.registry.OpenDapGriddedDataSetMetaDataFixture;
import com.raytheon.uf.common.datadelivery.registry.ParameterFixture;
import com.raytheon.uf.common.datadelivery.registry.SiteSubscription;
import com.raytheon.uf.common.datadelivery.registry.SiteSubscriptionFixture;
import com.raytheon.uf.common.datadelivery.registry.Subscription;
import com.raytheon.uf.common.datadelivery.registry.Subscription.SubscriptionPriority;
import com.raytheon.uf.common.datadelivery.registry.Time;
import com.raytheon.uf.common.datadelivery.registry.handlers.DataDeliveryHandlers;
import com.raytheon.uf.common.registry.event.RemoveRegistryEvent;
import com.raytheon.uf.common.registry.handler.RegistryHandlerException;
import com.raytheon.uf.common.registry.handler.RegistryObjectHandlersUtil;
import com.raytheon.uf.common.serialization.SerializationException;
import com.raytheon.uf.common.time.util.ImmutableDate;
import com.raytheon.uf.common.time.util.TimeUtil;
import com.raytheon.uf.common.time.util.TimeUtilTest;
import com.raytheon.uf.common.util.TestUtil;
import com.raytheon.uf.edex.datadelivery.bandwidth.dao.BandwidthAllocation;
import com.raytheon.uf.edex.datadelivery.bandwidth.dao.BandwidthSubscription;
import com.raytheon.uf.edex.datadelivery.bandwidth.dao.SubscriptionRetrieval;
import com.raytheon.uf.edex.datadelivery.bandwidth.notification.BandwidthEventBus;
import com.raytheon.uf.edex.datadelivery.bandwidth.retrieval.BandwidthMap;
import com.raytheon.uf.edex.datadelivery.bandwidth.retrieval.RetrievalPlan;
import com.raytheon.uf.edex.datadelivery.bandwidth.retrieval.RetrievalPlan.BandwidthBucket;
import com.raytheon.uf.edex.datadelivery.bandwidth.retrieval.RetrievalPlanTest;
import com.raytheon.uf.edex.datadelivery.bandwidth.retrieval.RetrievalStatus;
import com.raytheon.uf.edex.datadelivery.retrieval.RetrievalManagerNotifyEvent;

/**
 * Integration tests for {@link BandwidthManager}.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Oct 12, 2012 0726       djohnson     Initial creation
 * Oct 23, 2012 1286       djohnson     Create reusable abstract int test.
 * Dec 11, 2012 1286       djohnson     Add test verifying fulfilled retrievals won't cause NPEs when the subscription is updated.
 * Jan 25, 2013 1528       djohnson     Compare priorities as primitive ints.
 * Jan 28, 2013 1530       djohnson     Test that all allocations are unscheduled for subscription that doesn't fully schedule.
 * Jan 30, 2013 1501       djohnson     Fix broken calculations for determining required latency.
 * Feb 14, 2013 1595       djohnson     Fix expired subscription updates that weren't scheduling retrievals.
 * Feb 14, 2013 1596       djohnson     Add test duplicating errors deleting multiple subscriptions for the same provider/dataset.
 * Mar 11, 2013 1645       djohnson     Test configuration file modifications.
 * Mar 28, 2013 1841       djohnson     Subscription is now UserSubscription.
 * Apr 29, 2013 1910       djohnson     Always shutdown bandwidth managers in tests.
 * 
 * </pre>
 * 
 * @author djohnson
 * @version 1.0
 */
public class BandwidthManagerIntTest extends AbstractWfoBandwidthManagerIntTest {

    @Test
    public void testAddingSubscriptionAllocatesOncePerPlanDayForOneCycle()
            throws SerializationException {
        testSubscriptionCyclesAreAllocatedOncePerCyclePerPlanDay(Arrays
                .asList(1));
    }

    @Test
    public void testAddingSubscriptionAllocatesOncePerPlanPerCycle()
            throws SerializationException {
        testSubscriptionCyclesAreAllocatedOncePerCyclePerPlanDay(Arrays.asList(
                1, 2, 3));
    }

    @Test
    public void testDataSetMetaDataUpdateSetsSubscriptionRetrievalsToReady()
            throws SerializationException, ParseException {
        Subscription subscription = SiteSubscriptionFixture.INSTANCE.get();
        bandwidthManager.subscriptionUpdated(subscription);

        OpenDapGriddedDataSetMetaData metadata = OpenDapGriddedDataSetMetaDataFixture.INSTANCE
                .get();
        bandwidthManager.updateDataSetMetaData(metadata);

        Calendar cal = TimeUtil.newCalendar();
        cal.setTime(metadata.getDate());

        List<SubscriptionRetrieval> bandwidthAllocations = bandwidthDao
                .getSubscriptionRetrievals(subscription.getProvider(),
                        subscription.getDataSetName(), cal);
        assertEquals("Didn't find the subscription retrieval as expected!", 1,
                bandwidthAllocations.size());
        assertEquals(RetrievalStatus.READY, bandwidthAllocations.iterator()
                .next().getStatus());
    }

    @Test
    public void testDataSetMetaDataUpdateSetsCorrectTimeOnSubscription()
            throws SerializationException, ParseException {
        Subscription subscription = SiteSubscriptionFixture.INSTANCE.get();
        bandwidthManager.subscriptionUpdated(subscription);

        OpenDapGriddedDataSetMetaData metadata = OpenDapGriddedDataSetMetaDataFixture.INSTANCE
                .get();
        // Make the metadata for one day later than the subscription
        ImmutableDate oneDayLater = new ImmutableDate(metadata.getTime()
                .getStartDate().getTime()
                + TimeUtil.MILLIS_PER_DAY);
        metadata.getTime().setStartDate(oneDayLater);
        metadata.setDate(oneDayLater);

        // Send in the update
        bandwidthManager.updateDataSetMetaData(metadata);

        Calendar cal = TimeUtil.newCalendar();
        cal.setTime(metadata.getDate());

        List<SubscriptionRetrieval> bandwidthAllocations = bandwidthDao
                .getSubscriptionRetrievals(subscription.getProvider(),
                        subscription.getDataSetName(), cal);
        assertEquals("Didn't find the subscription retrieval as expected!", 1,
                bandwidthAllocations.size());
        assertEquals(
                "Didn't find the metadata date on the retrieval's subscription!",
                metadata.getDate(), bandwidthAllocations.iterator().next()
                        .getSubscription().getTime().getStartDate());
    }

    @Test
    public void testDailyProductSubscriptionReceivesTimeAndUrlFromUpdate()
            throws RegistryHandlerException, ParseException,
            SerializationException {
        RegistryObjectHandlersUtil.initMemory();

        // Store the original subscription
        Subscription subscription = SiteSubscriptionFixture.INSTANCE.get();
        DataDeliveryHandlers.getSubscriptionHandler().store(subscription);

        // The dataset metadata update
        OpenDapGriddedDataSetMetaData update = OpenDapGriddedDataSetMetaDataFixture.INSTANCE
                .get();
        update.setUrl("http://testDailyProductSubscriptionReceivesTimeAndUrlFromUpdate");
        update.setCycle(GriddedDataSetMetaData.NO_CYCLE);
        Time dsmdTime = update.getTime();
        dsmdTime.setStartDate(new Date(dsmdTime.getStartDate().getTime()
                + TimeUtil.MILLIS_PER_DAY));

        bandwidthManager.updateDataSetMetaData(update);

        List<SubscriptionRetrieval> retrievals = bandwidthDao
                .getSubscriptionRetrievals(subscription.getProvider(),
                        subscription.getDataSetName());

        assertEquals("Incorrect number of subscription retrievals generated!",
                1, retrievals.size());
        SubscriptionRetrieval retrieval = retrievals.iterator().next();
        Subscription retrievalSub = retrieval.getSubscription();
        assertEquals(
                "The update's url doesn't seem to have been persisted to the retrieval!",
                update.getUrl(), retrievalSub.getUrl());
        assertEquals(
                "The update's time doesn't seem to have been persisted to the retrieval!",
                dsmdTime.getStartDate(), retrievalSub.getTime().getStartDate());
    }

    @Test
    public void testDailyProductSubscriptionIsSetToReadyStatus()
            throws RegistryHandlerException, ParseException {
        RegistryObjectHandlersUtil.initMemory();

        // Store the original subscription
        Subscription subscription = SiteSubscriptionFixture.INSTANCE.get();
        subscription.getTime().setCycleTimes(Collections.<Integer> emptyList());
        DataDeliveryHandlers.getSubscriptionHandler().store(subscription);

        // The dataset metadata update
        OpenDapGriddedDataSetMetaData update = OpenDapGriddedDataSetMetaDataFixture.INSTANCE
                .get();
        update.setUrl("http://testDailyProductSubscriptionReceivesTimeAndUrlFromUpdate");
        update.setCycle(GriddedDataSetMetaData.NO_CYCLE);

        RetrievalPlan plan = retrievalManager.getPlan(subscription.getRoute());
        // Remove any buckets that are before or equal to the time of the
        // update, as if they were removed by the maintenance task
        RetrievalPlanTest.resizePlan(plan, TimeUtil.currentTimeMillis(), plan
                .getPlanEnd().getTimeInMillis());

        bandwidthManager.updateDataSetMetaData(update);

        List<SubscriptionRetrieval> retrievals = bandwidthDao
                .getSubscriptionRetrievals(subscription.getProvider(),
                        subscription.getDataSetName());

        assertEquals("Incorrect number of subscription retrievals generated!",
                1, retrievals.size());
        SubscriptionRetrieval retrieval = retrievals.iterator().next();
        assertEquals(RetrievalStatus.READY, retrieval.getStatus());
    }

    @Test
    public void testSubscriptionLatencyIsPlacedOnSubscriptionDao()
            throws RegistryHandlerException, ParseException {
        RegistryObjectHandlersUtil.initMemory();

        // Store the original subscription
        Subscription subscription = SiteSubscriptionFixture.INSTANCE.get();
        subscription.setLatencyInMinutes(5);
        DataDeliveryHandlers.getSubscriptionHandler().store(subscription);

        // The dataset metadata update
        OpenDapGriddedDataSetMetaData update = OpenDapGriddedDataSetMetaDataFixture.INSTANCE
                .get();
        update.setUrl("http://testDailyProductSubscriptionReceivesTimeAndUrlFromUpdate");
        update.setCycle(GriddedDataSetMetaData.NO_CYCLE);

        RetrievalPlan plan = retrievalManager.getPlan(subscription.getRoute());
        // Remove any buckets that are before or equal to the time of the
        // update, as if they were removed by the maintenance task
        RetrievalPlanTest.resizePlan(plan, TimeUtil.currentTimeMillis(), plan
                .getPlanEnd().getTimeInMillis());

        bandwidthManager.updateDataSetMetaData(update);

        List<SubscriptionRetrieval> retrievals = bandwidthDao
                .getSubscriptionRetrievals(subscription.getProvider(),
                        subscription.getDataSetName());

        assertEquals("Incorrect number of subscription retrievals generated!",
                1, retrievals.size());
        SubscriptionRetrieval retrieval = retrievals.iterator().next();
        assertEquals(RetrievalStatus.READY, retrieval.getStatus());
    }

    @Test
    public void testScheduleSubscriptionUnableToFitReturnsAllocationsUnscheduled() {

        Subscription subscription = createSubscriptionThatFillsUpABucket();
        Subscription subscription2 = createSubscriptionThatFillsUpABucket();

        // subscription2 will not be able to schedule for cycle hour 8
        subscription.getTime().setCycleTimes(
                Arrays.asList(Integer.valueOf(6), Integer.valueOf(8)));
        subscription2.getTime().setCycleTimes(
                Arrays.asList(Integer.valueOf(3), Integer.valueOf(8)));

        List<BandwidthAllocation> unscheduled = bandwidthManager
                .schedule(subscription);
        Collections.sort(unscheduled, new Comparator<BandwidthAllocation>() {
            @Override
            public int compare(BandwidthAllocation o1, BandwidthAllocation o2) {
                return o1.getStartTime().compareTo(o2.getStartTime());
            }
        });
        assertTrue(
                "Should have been able to schedule all cycles for the first subscription!",
                unscheduled.isEmpty());
        unscheduled = bandwidthManager.schedule(subscription2);
        assertEquals(
                "Should have not been able to subscribe for one shared cycle hour for two plan days!",
                2, unscheduled.size());

        Iterator<BandwidthAllocation> iter = unscheduled.iterator();
        BandwidthAllocation hour = iter.next();
        Calendar cal = TimeUtil.newCalendar();
        cal.set(Calendar.HOUR_OF_DAY, 8);
        cal.add(Calendar.DAY_OF_MONTH, 1);
        TestUtil.assertCalEquals(
                "The 8 hour cycle should not have been schedulable!", cal,
                hour.getStartTime());

        cal.add(Calendar.DAY_OF_MONTH, 1);
        hour = iter.next();
        TestUtil.assertCalEquals(
                "The 8 hour cycle should not have been schedulable!", cal,
                hour.getStartTime());
    }

    @Test
    public void testScheduleSubscriptionWithHigherPriorityUnschedulesOther() {

        Subscription subscription = createSubscriptionThatFillsUpABucket();
        Subscription subscription2 = createSubscriptionThatFillsUpABucket();

        // subscription2 will have higher priority
        subscription2.setPriority(SubscriptionPriority.HIGH);

        // they conflict for cycle hour 8
        subscription.getTime().setCycleTimes(
                Arrays.asList(Integer.valueOf(6), Integer.valueOf(8)));
        subscription2.getTime().setCycleTimes(
                Arrays.asList(Integer.valueOf(3), Integer.valueOf(8)));

        List<BandwidthAllocation> unscheduled = bandwidthManager
                .schedule(subscription);
        assertTrue(
                "Should have been able to schedule all cycles for the first subscription!",
                unscheduled.isEmpty());
        unscheduled = bandwidthManager.schedule(subscription2);
        assertEquals(
                "Should have not been able to subscribe for one shared cycle hour for two plan days!",
                2, unscheduled.size());

        Iterator<BandwidthAllocation> iter = unscheduled.iterator();
        BandwidthAllocation unscheduledAllocation = iter.next();
        assertEquals(
                "The first subscription with lower priority should have been the one unscheduled.",
                subscription.getPriority().getPriorityValue(),
                unscheduledAllocation.getPriority(), 0.0);

        unscheduledAllocation = iter.next();
        assertEquals(
                "The first subscription with lower priority should have been the one unscheduled.",
                subscription.getPriority().getPriorityValue(),
                unscheduledAllocation.getPriority(), 0.0);
    }

    @Test
    public void unscheduledSubscriptionUnschedulesAllAllocations() {
        String unscheduledSubDataSetName = "willBeUnscheduled";
        Subscription subscription = createSubscriptionThatFillsUpABucket();
        subscription.setDataSetName(unscheduledSubDataSetName);
        Subscription subscription2 = createSubscriptionThatFillsUpABucket();

        // subscription2 will have higher priority
        subscription2.setPriority(SubscriptionPriority.HIGH);

        // they conflict for cycle hour 8
        subscription.getTime().setCycleTimes(
                Arrays.asList(Integer.valueOf(6), Integer.valueOf(8)));
        subscription2.getTime().setCycleTimes(
                Arrays.asList(Integer.valueOf(3), Integer.valueOf(8)));

        bandwidthManager.schedule(subscription);
        bandwidthManager.schedule(subscription2);

        final List<SubscriptionRetrieval> subscriptionRetrievals = bandwidthDao
                .getSubscriptionRetrievals(subscription.getProvider(),
                        unscheduledSubDataSetName);

        for (SubscriptionRetrieval subscriptionRetrieval : subscriptionRetrievals) {
            assertThat(subscriptionRetrieval.getStatus(),
                    is(equalTo(RetrievalStatus.UNSCHEDULED)));
        }
    }

    /**
     * Redmine #1595: A subscription was created...and then expired. The
     * subscription was updated by extending the Expiration Date only. The
     * subscription, however, was never scheduled (e.g., in the Bandwidth
     * Utilization graph). Data displayed in D2D failed to update. However, the
     * subscription (and data) did begin updating after editing the subscription
     * and adding another parameter.
     */
    @Test
    public void expiredSubscriptionUpdatedToNonExpiredIsScheduled()
            throws Exception {

        final Date yesterday = new Date(TimeUtil.currentTimeMillis()
                - TimeUtil.MILLIS_PER_DAY);
        final Date oneHourAgo = new Date(TimeUtil.currentTimeMillis()
                - TimeUtil.MILLIS_PER_HOUR);

        Subscription subscription = createSubscriptionThatFillsHalfABucket();
        subscription.setSubscriptionStart(yesterday);
        subscription.setSubscriptionEnd(oneHourAgo);

        bandwidthManager.subscriptionUpdated(subscription);

        // Make sure nothing is scheduled when the subscription is expired
        assertThat(
                bandwidthDao.getBandwidthAllocations(subscription.getRoute()),
                is(empty()));

        // No longer expired
        subscription.setSubscriptionEnd(new Date(TimeUtil.currentTimeMillis()
                + TimeUtil.MILLIS_PER_WEEK));

        bandwidthManager.subscriptionUpdated(subscription);

        assertThat(
                bandwidthDao.getBandwidthAllocations(subscription.getRoute()),
                is(not(empty())));
    }

    @Test
    public void subscriptionUpdatedToExpiredHasAllocationsRemoved()
            throws Exception {

        Subscription subscription = createSubscriptionThatFillsHalfABucket();

        bandwidthManager.subscriptionUpdated(subscription);

        assertThat(
                bandwidthDao.getBandwidthAllocations(subscription.getRoute()),
                is(not(empty())));

        final Date yesterday = new Date(TimeUtil.currentTimeMillis()
                - TimeUtil.MILLIS_PER_DAY);
        final Date oneHourAgo = new Date(TimeUtil.currentTimeMillis()
                - TimeUtil.MILLIS_PER_HOUR);

        // Updated to expired
        subscription.setSubscriptionStart(yesterday);
        subscription.setSubscriptionEnd(oneHourAgo);

        bandwidthManager.subscriptionUpdated(subscription);

        // Make sure nothing is scheduled when the subscription is expired
        assertThat(
                bandwidthDao.getBandwidthAllocations(subscription.getRoute()),
                is(empty()));
    }

    @Test
    public void testScheduleSubscriptionWithHigherPrioritySetsOtherAllocationsToUnscheduled() {

        Subscription subscription = createSubscriptionThatFillsUpABucket();
        Subscription subscription2 = createSubscriptionThatFillsUpABucket();

        // subscription2 will have higher priority
        subscription2.setPriority(SubscriptionPriority.HIGH);

        // they conflict for cycle hour 8
        subscription.getTime().setCycleTimes(
                Arrays.asList(Integer.valueOf(6), Integer.valueOf(8)));
        subscription2.getTime().setCycleTimes(
                Arrays.asList(Integer.valueOf(3), Integer.valueOf(8)));

        List<BandwidthAllocation> unscheduled = bandwidthManager
                .schedule(subscription);
        assertTrue(
                "Should have been able to schedule all cycles for the first subscription!",
                unscheduled.isEmpty());
        unscheduled = bandwidthManager.schedule(subscription2);
        assertEquals(
                "Should have not been able to subscribe for one shared cycle hour for two plan days!",
                2, unscheduled.size());

        Iterator<BandwidthAllocation> iter = unscheduled.iterator();
        BandwidthAllocation unscheduledAllocation = iter.next();
        assertEquals(
                "The first subscription should be set to unscheduled status.",
                RetrievalStatus.UNSCHEDULED, unscheduledAllocation.getStatus());

        unscheduledAllocation = iter.next();
        assertEquals(
                "The first subscription should be set to unscheduled status.",
                RetrievalStatus.UNSCHEDULED, unscheduledAllocation.getStatus());
    }

    @Test
    public void testScheduleSubscriptionWithHigherPrioritySetsNewAllocationsToScheduled() {

        Subscription subscription = createSubscriptionThatFillsUpABucket();
        Subscription subscription2 = createSubscriptionThatFillsUpABucket();

        // subscription2 will have higher priority
        subscription2.setPriority(SubscriptionPriority.HIGH);

        // they conflict for cycle hour 8
        subscription.getTime().setCycleTimes(
                Arrays.asList(Integer.valueOf(6), Integer.valueOf(8)));
        subscription2.getTime().setCycleTimes(
                Arrays.asList(Integer.valueOf(3), Integer.valueOf(8)));

        bandwidthManager.schedule(subscription);
        bandwidthManager.schedule(subscription2);

        List<SubscriptionRetrieval> retrievals = bandwidthDao
                .getSubscriptionRetrievals(subscription2.getProvider(),
                        subscription2.getDataSetName());

        assertEquals("Incorrect number of subscription retrievals found.", 4,
                retrievals.size());
        for (SubscriptionRetrieval retrieval : retrievals) {
            assertEquals(
                    "Expected the retrieval to be in the scheduled status!",
                    RetrievalStatus.SCHEDULED, retrieval.getStatus());
        }
    }

    @Test
    public void testTooBigSubscriptionEditedToBeSmallerIsScheduled()
            throws SerializationException {

        // Subscription starts out too big
        Subscription subscription = createSubscriptionThatFillsUpTwoBuckets();
        subscription.getTime().setCycleTimes(Arrays.asList(Integer.valueOf(6)));

        List<BandwidthAllocation> unscheduled = bandwidthManager
                .subscriptionUpdated(subscription);

        assertFalse(
                "Shouldn't have been able to schedule such a large subscription",
                unscheduled.isEmpty());

        // Hey look, this subscription will fit now!
        subscription.setDataSetSize(subscription.getDataSetSize() / 2);
        subscription.setUnscheduled(false);

        unscheduled = bandwidthManager.subscriptionUpdated(subscription);

        assertTrue("Should have been able to schedule the subscription",
                unscheduled.isEmpty());

        List<SubscriptionRetrieval> retrievals = bandwidthDao
                .getSubscriptionRetrievals(subscription.getProvider(),
                        subscription.getDataSetName());

        // One per plan day
        assertEquals("Incorrect number of subscription retrievals found.", 2,
                retrievals.size());
        for (SubscriptionRetrieval retrieval : retrievals) {
            assertEquals(
                    "Expected the retrieval to be in the scheduled status!",
                    RetrievalStatus.SCHEDULED, retrieval.getStatus());
        }
    }

    @Test
    public void testDetermineRequiredLatencyReturnsNecessaryLatency()
            throws SerializationException {

        Subscription subscription = createSubscriptionThatFillsUpTenBuckets();
        subscription.getTime().setCycleTimes(Arrays.asList(Integer.valueOf(0)));
        subscription.setLatencyInMinutes(0);

        int requiredLatency = bandwidthManager
                .determineRequiredLatency(subscription);

        assertEquals("The required latency was calculated incorrectly", 30,
                requiredLatency);
    }

    /**
     * Fulfilled retrievals were causing NPE's when a subscription was updated,
     * because the {@link RetrievalPlan} didn't have a {@link BandwidthBucket}
     * to remove them from (and it shouldn't have). This test prevents that
     * regression.
     * 
     * @throws SerializationException
     *             better not
     */
    @Test
    public void fullfilledSubscriptionDoesNotThrowNullPointerOnUpdate()
            throws SerializationException {

        // Subscription starts out too big
        Subscription subscription = createSubscriptionThatFillsUpABucket();
        subscription.getTime().setCycleTimes(Arrays.asList(Integer.valueOf(0)));
        subscription.setLatencyInMinutes(0);

        bandwidthManager.subscriptionUpdated(subscription);

        final List<SubscriptionRetrieval> subscriptionRetrievals = bandwidthDao
                .getSubscriptionRetrievals(subscription.getProvider(),
                        subscription.getDataSetName());

        for (SubscriptionRetrieval retrieval : subscriptionRetrievals) {
            RetrievalManagerNotifyEvent event = new RetrievalManagerNotifyEvent();
            event.setId(String.valueOf(retrieval.getId()));
            retrievalManager.retrievalCompleted(event);
        }

        // Now let's go forward a couple days in time, and let the old buckets
        // get removed
        TimeUtilTest.freezeTime(TimeUtil.currentTimeMillis()
                + (TimeUtil.MILLIS_PER_DAY * 4));

        // Technically this would be performed by the bandwidth manager
        // maintenance task... is this too much in-depth knowledge of how the
        // system is tied together? How can it be abstracted out, maybe move the
        // maintenance logic into a bandwidth manager method proper?
        retrievalManager.getPlan(subscription.getRoute()).resize();

        subscription.setLatencyInMinutes(1);
        bandwidthManager.subscriptionUpdated(subscription);
    }

    /**
     * Long-running in-memory bandwidth manager proposed schedule operations
     * were causing {@link ConcurrentModificationException}s to occur when
     * receiving events from the {@link BandwidthEventBus}.
     * 
     * @throws Exception
     *             on test failure
     */
    @Test
    public void testInMemoryBandwidthManagerCanReceiveDataSetMetaDataUpdates()
            throws Exception {

        Subscription subscription = createSubscriptionThatFillsUpABucket();
        subscription.getTime().setCycleTimes(Arrays.asList(Integer.valueOf(0)));

        bandwidthManager.schedule(subscription);
        BandwidthManager bwProposed = null;
        try {
            bwProposed = bandwidthManager
                    .startProposedBandwidthManager(BandwidthMap
                            .load(EdexBandwidthContextFactory
                                    .getBandwidthMapConfig()));
            final BandwidthManager proposed = bwProposed;

            final BlockingQueue<Exception> queue = new ArrayBlockingQueue<Exception>(
                    1);

            final int invocationCount = 10;
            final CountDownLatch waitForAllThreadsReadyLatch = new CountDownLatch(
                    invocationCount * 2);
            final CountDownLatch doneLatch = new CountDownLatch(
                    invocationCount * 2);
            for (int i = 0; i < invocationCount; i++) {
                final int current = i;
                Thread thread = new Thread() {
                    @Override
                    public void run() {
                        try {
                            // Wait for all threads to check in, then they all
                            // start
                            // working at once
                            waitForAllThreadsReadyLatch.countDown();
                            waitForAllThreadsReadyLatch.await();
                            proposed.updateDataSetMetaData(OpenDapGriddedDataSetMetaDataFixture.INSTANCE
                                    .get(current));
                        } catch (Exception e) {
                            queue.offer(e);
                        }
                        doneLatch.countDown();
                    }
                };
                thread.start();
            }

            for (int i = 0; i < invocationCount; i++) {
                final int current = i;
                Thread thread = new Thread() {
                    @Override
                    public void run() {
                        try {
                            final Subscription subscription2 = SiteSubscriptionFixture.INSTANCE
                                    .get(current);
                            subscription2
                                    .addParameter(ParameterFixture.INSTANCE
                                            .get(1));
                            subscription2
                                    .addParameter(ParameterFixture.INSTANCE
                                            .get(2));
                            subscription2
                                    .addParameter(ParameterFixture.INSTANCE
                                            .get(3));
                            subscription2.getTime().setCycleTimes(
                                    Arrays.asList(1, 2, 3, 4, 5, 6, 7, 8, 9,
                                            10, 11, 12, 13, 14, 15, 16, 17));
                            subscription2.setLatencyInMinutes(current);
                            // Wait for all threads to check in, then they all
                            // start
                            // working at once
                            waitForAllThreadsReadyLatch.countDown();
                            waitForAllThreadsReadyLatch.await();
                            proposed.schedule(subscription2);
                        } catch (Exception e) {
                            queue.offer(e);
                        }
                        doneLatch.countDown();
                    }
                };
                thread.start();
            }

            // Wait for all threads to finish
            doneLatch.await();

            final Exception exception = queue.poll();
            if (exception != null) {
                throw exception;
            }
        } finally {
            shutdownBandwidthManager(bwProposed);
        }
    }

    /**
     * Subscriptions that are deleted should have all of their bandwidth
     * allocations removed deleted.
     */
    @Test
    public void testDeletedSubscriptionsHaveAllocationsDeletedFromDatabase()
            throws Exception {

        Subscription subscription = createSubscriptionThatFillsUpABucket();
        subscription.getTime().setCycleTimes(Arrays.asList(0, 12));

        bandwidthManager.schedule(subscription);

        final List<BandwidthAllocation> bandwidthAllocations = bandwidthDao
                .getBandwidthAllocations(subscription.getRoute());

        assertEquals("Incorrect number of allocations found.", 4,
                bandwidthAllocations.size());

        sendDeletedSubscriptionEvent(subscription);

        final List<BandwidthAllocation> allocationsAfterDelete = bandwidthDao
                .getBandwidthAllocations(subscription.getRoute());

        assertEquals(
                "Expected all bandwidth allocations to have been deleted.", 0,
                allocationsAfterDelete.size());
    }

    /**
     * Subscriptions that are deleted should have their SubscriptionDaos
     * removed.
     */
    @Test
    public void testDeletedSubscriptionsHaveSubscriptionDaosDeletedFromDatabase()
            throws Exception {

        Subscription subscription = createSubscriptionThatFillsUpABucket();
        subscription.getTime().setCycleTimes(Arrays.asList(0, 12));

        bandwidthManager.schedule(subscription);

        final List<BandwidthSubscription> subscriptionDaos = bandwidthDao
                .getBandwidthSubscription(subscription);

        assertEquals("Incorrect number of subscription daos found.", 4,
                subscriptionDaos.size());

        sendDeletedSubscriptionEvent(subscription);

        final List<BandwidthAllocation> subscriptionDaosAfterDelete = bandwidthDao
                .getBandwidthAllocations(subscription.getRoute());

        assertEquals("Expected all subscription daos to have been deleted.", 0,
                subscriptionDaosAfterDelete.size());
    }

    /**
     * Test case derived from Redmine #1596: There were 5 or 6 subscriptions
     * that were deleted simultaneously. All subscriptions were removed from the
     * Subscription Manager dialog...however, 2 subscriptions remained
     * 'scheduled' or listed on the Bandwidth Utilization graph. After selecting
     * the deleted subscription in the Bandwidth Utilization graph, the view
     * selected subscriptions option was selected. No subscriptions were
     * displayed, as expected...even though the subscription was listed in the
     * Bandwidth Utilization graph.
     */
    @Test
    public void testDeletedSubscriptionsToSameProviderDataSetHaveSubscriptionDaosDeletedFromDatabase()
            throws Exception {

        final int numberOfSubscriptionsWithSameProviderDataSet = 4;

        final SiteSubscription templateSubscription = createSubscriptionThatFillsUpABucket();
        final Network route = templateSubscription.getRoute();
        templateSubscription.setDataSetSize(templateSubscription
                .getDataSetSize()
                / numberOfSubscriptionsWithSameProviderDataSet);

        templateSubscription.getTime().setCycleTimes(Arrays.asList(0, 12));

        int lastKnownNumberOfBandwidthAllocations = 0;
        final Subscription[] subscriptions = new Subscription[numberOfSubscriptionsWithSameProviderDataSet];
        for (int i = 0; i < numberOfSubscriptionsWithSameProviderDataSet; i++) {

            final SiteSubscription currentSubscription = new SiteSubscription(
                    templateSubscription, "ILookLikeTheOtherGuys-" + i);
            subscriptions[i] = currentSubscription;

            bandwidthManager.schedule(currentSubscription);

            // Make sure some data is scheduled for retrieval
            final int currentNumberOfBandwidthAllocations = bandwidthDao
                    .getBandwidthAllocations(route).size();
            assertThat(currentNumberOfBandwidthAllocations,
                    is(greaterThan(lastKnownNumberOfBandwidthAllocations)));

            // Update last known number of bandwidth allocations, so we can
            // continue verifying more is scheduled
            lastKnownNumberOfBandwidthAllocations = currentNumberOfBandwidthAllocations;
        }

        // Schedule two subscription deletions to occur at the same time
        final CountDownLatch waitForAllThreadsToStartLatch = new CountDownLatch(
                numberOfSubscriptionsWithSameProviderDataSet);
        final CountDownLatch deletesFinishedLatch = new CountDownLatch(
                numberOfSubscriptionsWithSameProviderDataSet);

        for (int i = 0; i < numberOfSubscriptionsWithSameProviderDataSet; i++) {
            final int iteration = i;
            final Thread deleteSubscriptionThread = new Thread() {
                @Override
                public void run() {
                    waitForAllThreadsToStartLatch.countDown();
                    try {
                        sendDeletedSubscriptionEvent(subscriptions[iteration]);
                    } finally {
                        deletesFinishedLatch.countDown();
                    }
                }
            };

            // Delete the subscription! Each thread will wait to perform the
            // deletion until all threads are started.
            deleteSubscriptionThread.start();
        }

        // Wait for the deletion threads to finish
        deletesFinishedLatch.await();

        // Better not be any bandwidth subscriptions left, or bandwidth
        // allocations
        assertThat(bandwidthDao.getBandwidthSubscriptions(), is(empty()));
        assertThat(bandwidthDao.getBandwidthAllocations(route), is(empty()));
    }

    /**
     * Subscriptions that are deleted should have all of their bandwidth
     * allocations removed deleted.
     */
    @Test
    public void testDeletedSubscriptionsHaveAllocationsDeletedFromRetrievalManager()
            throws Exception {

        Subscription subscription = createSubscriptionThatFillsUpABucket();
        subscription.getTime().setCycleTimes(Arrays.asList(0, 12));

        final Network network = subscription.getRoute();
        final List<BandwidthAllocation> bandwidthAllocationsOrig = getRetrievalManagerAllocationsForNetwork(network);
        assertEquals("Incorrect number of allocations found.", 0,
                bandwidthAllocationsOrig.size());

        bandwidthManager.schedule(subscription);

        final List<BandwidthAllocation> bandwidthAllocations = getRetrievalManagerAllocationsForNetwork(network);

        assertEquals("Incorrect number of allocations found.", 4,
                bandwidthAllocations.size());

        sendDeletedSubscriptionEvent(subscription);

        final List<BandwidthAllocation> allocationsAfterDelete = getRetrievalManagerAllocationsForNetwork(network);

        assertEquals(
                "Expected all bandwidth allocations to have been deleted.", 0,
                allocationsAfterDelete.size());
    }

    /**
     * Subscriptions that are deleted should have all of their bandwidth
     * allocations removed deleted.
     */
    @Test
    public void testDeletedPartiallyScheduledSubscriptionsHaveAllocationsDeleted()
            throws Exception {

        Subscription subscription = createSubscriptionThatFillsUpTwoBuckets();
        subscription.getTime().setCycleTimes(Arrays.asList(0, 12));
        subscription.setLatencyInMinutes(0);

        final List<BandwidthAllocation> unableToSchedule = bandwidthManager
                .schedule(subscription);
        assertFalse("Shouldn't have been able to fully schedule.",
                unableToSchedule.isEmpty());

        final List<BandwidthAllocation> bandwidthAllocations = bandwidthDao
                .getBandwidthAllocations(subscription.getRoute());

        assertEquals("Incorrect number of allocations found.", 0,
                bandwidthAllocations.size());

        sendDeletedSubscriptionEvent(subscription);

        final List<BandwidthAllocation> allocationsAfterDelete = bandwidthDao
                .getBandwidthAllocations(subscription.getRoute());

        assertEquals(
                "Expected all bandwidth allocations to have been deleted.", 0,
                allocationsAfterDelete.size());
    }

    /**
     * Subscriptions that are deleted should have their SubscriptionDaos
     * removed.
     */
    @Test
    public void testDeletedPartiallyScheduledSubscriptionsHaveSubscriptionDaosDeleted()
            throws Exception {

        Subscription subscription = createSubscriptionThatFillsUpTwoBuckets();
        subscription.getTime().setCycleTimes(Arrays.asList(0, 12));
        subscription.setLatencyInMinutes(0);

        final List<BandwidthAllocation> unableToSchedule = bandwidthManager
                .schedule(subscription);
        assertFalse("Shouldn't have been able to fully schedule.",
                unableToSchedule.isEmpty());

        final List<BandwidthSubscription> subscriptionDaos = bandwidthDao
                .getBandwidthSubscription(subscription);

        assertEquals("Incorrect number of subscription daos found.", 0,
                subscriptionDaos.size());

        sendDeletedSubscriptionEvent(subscription);

        final List<BandwidthAllocation> subscriptionDaosAfterDelete = bandwidthDao
                .getBandwidthAllocations(subscription.getRoute());

        assertEquals("Expected all subscription daos to have been deleted.", 0,
                subscriptionDaosAfterDelete.size());
    }

    @Test
    public void testModifiedConfigurationFileWillReinitializeBandwidthManager() {
        IntegrationTestBandwidthContextFactory
                .getIntegrationTestBandwidthMapConfigFile().setLastModified(
                        Long.MAX_VALUE);
        bandwidthManager.watchForConfigFileChanges.run();

        assertThat(EdexBandwidthContextFactory.getInstance(),
                is(not(sameInstance(bandwidthManager))));
    }

    @Test
    public void testUnmodifiedConfigurationFileWillNotReinitializeBandwidthManager() {
        bandwidthManager.watchForConfigFileChanges.run();

        assertThat(EdexBandwidthContextFactory.getInstance(),
                is(sameInstance(bandwidthManager)));
    }

    private void testSubscriptionCyclesAreAllocatedOncePerCyclePerPlanDay(
            List<Integer> cycles) throws SerializationException {
        Subscription subscription = SiteSubscriptionFixture.INSTANCE.get();
        subscription.getTime().setCycleTimes(cycles);
        try {
            bandwidthManager.subscriptionUpdated(subscription);
        } catch (Throwable t) {
            t.printStackTrace();
        }

        assertEquals("Incorrect number of bandwidth allocations made!",
                retrievalManager.getPlan(subscription.getRoute()).getPlanDays()
                        * cycles.size(),
                bandwidthDao.getBandwidthAllocations(subscription.getRoute())
                        .size());
    }

    private void sendDeletedSubscriptionEvent(Subscription subscription) {
        RemoveRegistryEvent event = new RemoveRegistryEvent(
                subscription.getOwner(), subscription.getId());
        event.setObjectType(DataDeliveryRegistryObjectTypes.SITE_SUBSCRIPTION);
        bandwidthManager.subscriptionRemoved(event);
    }

    /**
     * @param subscription
     * @return
     */
    private List<BandwidthAllocation> getRetrievalManagerAllocationsForNetwork(
            Network network) {
        final SortedSet<BandwidthBucket> buckets = retrievalManager.getPlan(
                network).getBucketsInWindow(Long.MIN_VALUE, Long.MAX_VALUE);
        List<BandwidthAllocation> allocations = new ArrayList<BandwidthAllocation>();
        for (BandwidthBucket bucket : buckets) {
            allocations.addAll(bucket.getRequests());
        }
        return allocations;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    protected Network getRouteToUseForSubscription() {
        return Network.OPSNET;
    }

}
