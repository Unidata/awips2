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
 *                         402.291.0ONE_HUNDRED
 * 
 * See the AWIPS II Master Rights File ("Master Rights File.pdf") for
 * further licensing information.
 **/
package com.raytheon.uf.edex.datadelivery.bandwidth;

import static org.hamcrest.CoreMatchers.equalTo;
import static org.hamcrest.CoreMatchers.is;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotSame;
import static org.junit.Assert.assertThat;
import static org.junit.Assert.assertTrue;

import java.io.File;
import java.util.Arrays;
import java.util.Calendar;
import java.util.Date;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.SortedSet;

import org.junit.Test;

import com.raytheon.uf.common.datadelivery.bandwidth.BandwidthService;
import com.raytheon.uf.common.datadelivery.bandwidth.IBandwidthRequest;
import com.raytheon.uf.common.datadelivery.bandwidth.IProposeScheduleResponse;
import com.raytheon.uf.common.datadelivery.bandwidth.WfoBandwidthService;
import com.raytheon.uf.common.datadelivery.bandwidth.data.BandwidthBucketDescription;
import com.raytheon.uf.common.datadelivery.bandwidth.data.BandwidthGraphData;
import com.raytheon.uf.common.datadelivery.bandwidth.data.BandwidthMap;
import com.raytheon.uf.common.datadelivery.bandwidth.data.SubscriptionStatusSummary;
import com.raytheon.uf.common.datadelivery.bandwidth.data.TimeWindowData;
import com.raytheon.uf.common.datadelivery.registry.AdhocSubscription;
import com.raytheon.uf.common.datadelivery.registry.AdhocSubscriptionFixture;
import com.raytheon.uf.common.datadelivery.registry.Coverage;
import com.raytheon.uf.common.datadelivery.registry.DataType;
import com.raytheon.uf.common.datadelivery.registry.GriddedTime;
import com.raytheon.uf.common.datadelivery.registry.Network;
import com.raytheon.uf.common.datadelivery.registry.Subscription;
import com.raytheon.uf.common.datadelivery.registry.Subscription.SubscriptionPriority;
import com.raytheon.uf.common.datadelivery.registry.Time;
import com.raytheon.uf.common.datadelivery.registry.handlers.DataDeliveryHandlers;
import com.raytheon.uf.common.datadelivery.registry.handlers.ISubscriptionHandler;
import com.raytheon.uf.common.registry.handler.RegistryHandlerException;
import com.raytheon.uf.common.serialization.SerializationUtil;
import com.raytheon.uf.common.time.util.TimeUtil;
import com.raytheon.uf.edex.datadelivery.bandwidth.dao.BandwidthAllocation;
import com.raytheon.uf.edex.datadelivery.bandwidth.dao.BandwidthBucket;
import com.raytheon.uf.edex.datadelivery.bandwidth.dao.SubscriptionRetrieval;
import com.raytheon.uf.edex.datadelivery.bandwidth.retrieval.RetrievalPlan;
import com.raytheon.uf.edex.datadelivery.bandwidth.util.BandwidthUtil;

/**
 * Tests for the thrift service of {@link BandwidthManager}.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Oct 22, 2012 1286       djohnson     Initial creation
 * Nov 20, 2012 1286       djohnson     Add tests for proposeSchedule methods.
 * Dec 06, 2012 1397       djohnson     Add tests for getting bandwidth graph data.
 * Feb 20, 2013 1543       djohnson     Use WFO bandwidth manager.
 * Feb 26, 2013 1643       djohnson     BandwidthService extends reusable class.
 * Feb 27, 2013 1644       djohnson     Bandwidth service is the WFO version.
 * May 20, 2013 1650       djohnson     Add test for returning required dataset size.
 * Jun 12, 2013 2038       djohnson     Add test for returning required dataset size on subscription update.
 * Jun 25, 2013 2106       djohnson     BandwidthBucket is a big boy class now.
 * Jul 11, 2013 2106       djohnson     Use SubscriptionPriority enum.
 * Jul 18, 2013 1653       mpduff       Added test for sub status summary.
 * Sept 25, 2013 1797      dhladky      separated time from gridded time
 * Oct 21, 2013   2292     mpduff       Implement multiple data types
 * </pre>
 * 
 * @author djohnson
 * @version 1.0
 */
public class BandwidthServiceIntTest<T extends Time, C extends Coverage>
        extends AbstractWfoBandwidthManagerIntTest<T, C> {

    private static final int ONE_HUNDRED = 100;

    private final BandwidthService<T, C> service = new WfoBandwidthService<T, C>() {
        @Override
        protected Object getResponseFromServer(IBandwidthRequest<T, C> request)
                throws Exception {
            // Serialize and deserialize each call, this makes sure the dynamic
            // serialize annotations are correct as well
            return SerializationUtil
                    .transformFromThrift(
                            Object.class,
                            SerializationUtil
                                    .transformToThrift(BandwidthServiceIntTest.this.bandwidthManager
                                            .handleRequest(request)));
        }
    };

    @Test
    public void testRetrieveBandwidthFromServer() {
        assertEquals("Incorrect bandwidth amount retrieved!", 768,
                service.getBandwidthForNetworkInKilobytes(Network.OPSNET));
    }

    @Test
    public void testProposeNetworkBandwidthReturnsSubscriptionsUnableToFit()
            throws RegistryHandlerException {

        // Two subscriptions that will fill up a bucket exactly
        Subscription<T, C> subscription = createSubscriptionThatFillsHalfABucket();
        Subscription<T, C> subscription2 = createSubscriptionThatFillsHalfABucket();

        final ISubscriptionHandler subscriptionHandler = DataDeliveryHandlers
                .getSubscriptionHandler();
        subscriptionHandler.store(subscription);
        subscriptionHandler.store(subscription2);

        bandwidthManager.schedule(subscription);
        bandwidthManager.schedule(subscription2);

        // Now we propose dropping the bandwidth by just one kb/s
        Set<String> results = service.proposeBandwidthForNetworkInKilobytes(
                Network.OPSNET, retrievalManager.getPlan(Network.OPSNET)
                        .getDefaultBandwidth() - 1);

        assertEquals(
                "Expected one subscription to not have been able to fit with the new bandwidth!",
                1, results.size());
    }

    @Test
    public void testProposeNetworkBandwidthReturnsNoSubscriptionsWhenAbleToFit()
            throws RegistryHandlerException {

        // Two subscriptions that will fill up only a third of a bucket
        Subscription<T, C> subscription = createSubscriptionThatFillsAThirdOfABucket();
        Subscription<T, C> subscription2 = createSubscriptionThatFillsAThirdOfABucket();

        ISubscriptionHandler subscriptionHandler = DataDeliveryHandlers
                .getSubscriptionHandler();
        subscriptionHandler.store(subscription);
        subscriptionHandler.store(subscription2);

        bandwidthManager.schedule(subscription);
        bandwidthManager.schedule(subscription2);

        // Now we propose dropping the bandwidth by just one kb/s
        Set<String> results = service.proposeBandwidthForNetworkInKilobytes(
                Network.OPSNET, retrievalManager.getPlan(Network.OPSNET)
                        .getDefaultBandwidth() - 1);

        assertTrue(
                "Expected to be able to fit all subscriptions with the new bandwidth!",
                results.isEmpty());
    }

    @Test
    public void testSaveBandwidthToServer() {
        service.setBandwidthForNetworkInKilobytes(Network.OPSNET, ONE_HUNDRED);
        assertEquals(
                "Expected the bandwidth to have been saved to the in memory bandwidth route!",
                ONE_HUNDRED,
                service.getBandwidthForNetworkInKilobytes(Network.OPSNET));
    }

    @Test
    public void testSaveBandwidthToServerUpdatesBandwidthMapFile() {
        service.setBandwidthForNetworkInKilobytes(Network.OPSNET, ONE_HUNDRED);

        File file = IntegrationTestBandwidthContextFactory
                .getIntegrationTestBandwidthMapConfigFile();
        BandwidthMap map = BandwidthMap.load(file);
        assertEquals(
                "Expected the bandwidth to have been saved to the configuration file!",
                ONE_HUNDRED, map.getRoute(Network.OPSNET).getDefaultBandwidth());
    }

    @Test
    public void testSaveBandwidthToServerReinitializesBandwidthMap() {
        service.setBandwidthForNetworkInKilobytes(Network.OPSNET, ONE_HUNDRED);

        // Just check for a bucket with an expected amount of bytes size
        SortedSet<BandwidthBucket> bucketsInWindow = EdexBandwidthContextFactory
                .getInstance().retrievalManager.getPlan(Network.OPSNET)
                .getBucketsInWindow(
                        TimeUtil.currentTimeMillis(),
                        TimeUtil.currentTimeMillis()
                                + (5 * TimeUtil.MILLIS_PER_MINUTE));
        BandwidthBucket bucket = bucketsInWindow.iterator().next();
        assertEquals(
                "The BandwidthManager does not seem to have been reinitialized!",
                BandwidthUtil
                        .convertKilobytesPerSecondToBytesPerSpecifiedMinutes(
                                ONE_HUNDRED, 3), bucket.getBucketSize());
    }

    @Test
    public void testScheduleTwoSubscriptionsThatFitReturnsEmptyList() {
        // Two subscriptions, the sum of which will fill up a bucket exactly
        Subscription<T, C> subscription = createSubscriptionThatFillsHalfABucket();
        Subscription<T, C> subscription2 = createSubscriptionThatFillsHalfABucket();

        // subscription2 will not be able to schedule for cycle hour 8
        ((GriddedTime) subscription.getTime()).setCycleTimes(Arrays.asList(
                Integer.valueOf(6), Integer.valueOf(8)));
        ((GriddedTime) subscription2.getTime()).setCycleTimes(Arrays.asList(
                Integer.valueOf(6), Integer.valueOf(8)));

        @SuppressWarnings("unchecked")
        Set<String> unscheduledSubscriptions = service.schedule(Arrays.asList(
                subscription, subscription2));
        verifyNoSubscriptionsWereUnscheduled(unscheduledSubscriptions);
    }

    @Test
    public void testScheduleTwoSubscriptionsSecondDoesNotFitReturnsSecondsName() {
        Subscription<T, C> subscription = createSubscriptionThatFillsHalfABucket();
        // Requires its own full bucket, so cycle hour 3 will succeed and cycle
        // hour 8 will not
        Subscription<T, C> subscription2 = createSubscriptionThatFillsUpABucket();

        // subscription2 will not be able to schedule for cycle hour 8
        ((GriddedTime) subscription.getTime()).setCycleTimes(Arrays.asList(
                Integer.valueOf(6), Integer.valueOf(8)));
        ((GriddedTime) subscription2.getTime()).setCycleTimes(Arrays.asList(
                Integer.valueOf(3), Integer.valueOf(8)));

        @SuppressWarnings("unchecked")
        Set<String> unscheduledSubscriptions = service.schedule(Arrays.asList(
                subscription, subscription2));
        verifySubscriptionWasNotAbleToBeFullyScheduled(
                unscheduledSubscriptions, subscription2);
    }

    @Test
    public void testScheduleSubscriptionReturnsNamesOfUnscheduledSubscriptions() {

        Subscription<T, C> subscription = createSubscriptionThatFillsUpABucket();
        Subscription<T, C> subscription2 = createSubscriptionThatFillsUpABucket();

        // subscription2 will not be able to schedule for cycle hour 8
        ((GriddedTime) subscription.getTime()).setCycleTimes(Arrays.asList(
                Integer.valueOf(6), Integer.valueOf(8)));
        ((GriddedTime) subscription2.getTime()).setCycleTimes(Arrays.asList(
                Integer.valueOf(3), Integer.valueOf(8)));

        Set<String> unscheduledSubscriptions = service.schedule(subscription);
        verifyNoSubscriptionsWereUnscheduled(unscheduledSubscriptions);

        unscheduledSubscriptions = service.schedule(subscription2);
        verifySubscriptionWasNotAbleToBeFullyScheduled(
                unscheduledSubscriptions, subscription2);
    }

    @Test
    public void testProposeScheduleTwoSubscriptionsThatFitReturnsEmptyList() {

        // Two subscriptions, the sum of which will fill up a bucket exactly
        Subscription<T, C> subscription = createSubscriptionThatFillsHalfABucket();
        Subscription<T, C> subscription2 = createSubscriptionThatFillsHalfABucket();

        // subscription2 will not be able to schedule for cycle hour 8
        ((GriddedTime) subscription.getTime()).setCycleTimes(Arrays.asList(
                Integer.valueOf(6), Integer.valueOf(8)));
        ((GriddedTime) subscription2.getTime()).setCycleTimes(Arrays.asList(
                Integer.valueOf(6), Integer.valueOf(8)));

        @SuppressWarnings("unchecked")
        IProposeScheduleResponse response = service.proposeSchedule(Arrays
                .asList(subscription, subscription2));
        Set<String> unscheduledSubscriptions = response
                .getUnscheduledSubscriptions();
        verifyNoSubscriptionsWereUnscheduled(unscheduledSubscriptions);
    }

    @Test
    public void testProposeScheduleTwoSubscriptionsThatFitReturnsNotSetRequiredLatency() {

        // Two subscriptions, the sum of which will fill up a bucket exactly
        Subscription<T, C> subscription = createSubscriptionThatFillsHalfABucket();
        Subscription<T, C> subscription2 = createSubscriptionThatFillsHalfABucket();

        ((GriddedTime) subscription.getTime()).setCycleTimes(Arrays.asList(
                Integer.valueOf(6), Integer.valueOf(8)));
        ((GriddedTime) subscription2.getTime()).setCycleTimes(Arrays.asList(
                Integer.valueOf(6), Integer.valueOf(8)));

        @SuppressWarnings("unchecked")
        IProposeScheduleResponse response = service.proposeSchedule(Arrays
                .asList(subscription, subscription2));
        assertEquals(
                "Expected the required latency to not be set when the propose schedule fits",
                IProposeScheduleResponse.VALUE_NOT_SET,
                response.getRequiredLatency());
    }

    @Test
    public void testProposeScheduleTwoSubscriptionsThatFitReturnsNotSetRequiredDataSetSize() {

        // Two subscriptions, the sum of which will fill up a bucket exactly
        Subscription<T, C> subscription = createSubscriptionThatFillsHalfABucket();
        Subscription<T, C> subscription2 = createSubscriptionThatFillsHalfABucket();

        ((GriddedTime) subscription.getTime()).setCycleTimes(Arrays.asList(
                Integer.valueOf(6), Integer.valueOf(8)));
        ((GriddedTime) subscription2.getTime()).setCycleTimes(Arrays.asList(
                Integer.valueOf(6), Integer.valueOf(8)));

        @SuppressWarnings("unchecked")
        IProposeScheduleResponse response = service.proposeSchedule(Arrays
                .asList(subscription, subscription2));
        assertEquals(
                "Expected the required latency to not be set when the propose schedule fits",
                IProposeScheduleResponse.VALUE_NOT_SET,
                response.getRequiredDataSetSize());
    }

    @Test
    public void testProposeScheduleTwoSubscriptionsSecondDoesNotFitReturnsSecondsName() {

        // Two subscriptions, the sum of which will fill up a bucket exactly
        Subscription<T, C> subscription = createSubscriptionThatFillsHalfABucket();
        // Requires its own full bucket, so cycle hour 3 will succeed and cycle
        // hour 8 will not
        Subscription<T, C> subscription2 = createSubscriptionThatFillsUpABucket();

        // subscription2 will not be able to schedule for cycle hour 8
        ((GriddedTime) subscription.getTime()).setCycleTimes(Arrays.asList(
                Integer.valueOf(6), Integer.valueOf(8)));
        ((GriddedTime) subscription2.getTime()).setCycleTimes(Arrays.asList(
                Integer.valueOf(3), Integer.valueOf(8)));

        @SuppressWarnings("unchecked")
        Set<String> unscheduledSubscriptions = service.proposeSchedule(
                Arrays.asList(subscription, subscription2))
                .getUnscheduledSubscriptions();
        verifySubscriptionWasNotAbleToBeFullyScheduled(
                unscheduledSubscriptions, subscription2);
    }

    @Test
    public void testProposeScheduleSubscriptionReturnsNamesOfUnscheduledSubscriptions() {

        Subscription<T, C> subscription = createSubscriptionThatFillsUpABucket();
        Subscription<T, C> subscription2 = createSubscriptionThatFillsUpABucket();

        // subscription2 will not be able to schedule for cycle hour 8
        ((GriddedTime) subscription.getTime()).setCycleTimes(Arrays.asList(
                Integer.valueOf(6), Integer.valueOf(8)));
        ((GriddedTime) subscription2.getTime()).setCycleTimes(Arrays.asList(
                Integer.valueOf(3), Integer.valueOf(8)));

        Set<String> unscheduledSubscriptions = service.schedule(subscription);
        verifyNoSubscriptionsWereUnscheduled(unscheduledSubscriptions);

        unscheduledSubscriptions = service.proposeSchedule(subscription2)
                .getUnscheduledSubscriptions();
        verifySubscriptionWasNotAbleToBeFullyScheduled(
                unscheduledSubscriptions, subscription2);
    }

    @Test
    public void testProposeScheduleSubscriptionsSecondDoesntFitReturnsRequiredLatency() {

        // Two subscriptions that will fill up a bucket exactly
        Subscription<T, C> subscription = createSubscriptionThatFillsUpABucket();
        Subscription<T, C> subscription2 = createSubscriptionThatFillsUpABucket();

        // subscription2 will not be able to schedule for cycle hour 8
        ((GriddedTime) subscription.getTime()).setCycleTimes(Arrays.asList(
                Integer.valueOf(6), Integer.valueOf(8)));
        ((GriddedTime) subscription2.getTime()).setCycleTimes(Arrays.asList(
                Integer.valueOf(3), Integer.valueOf(8)));

        Set<String> unscheduledSubscriptions = service.schedule(subscription);
        verifyNoSubscriptionsWereUnscheduled(unscheduledSubscriptions);

        int requiredLatency = service.proposeSchedule(subscription2)
                .getRequiredLatency();
        assertEquals(
                "The required latency should have been returned from propose schedule!",
                6, requiredLatency);
    }

    @Test
    public void testProposeScheduleSubscriptionsSecondDoesntFitReturnsRequiredSize() {

        Subscription<T, C> subscription = createSubscriptionThatFillsHalfABucket();
        Subscription<T, C> subscription2 = createSubscriptionThatFillsUpTenBuckets();

        // subscription2 will not be able to schedule for cycle hour 8
        ((GriddedTime) subscription.getTime()).setCycleTimes(Arrays.asList(
                Integer.valueOf(6), Integer.valueOf(8)));
        ((GriddedTime) subscription2.getTime()).setCycleTimes(Arrays.asList(
                Integer.valueOf(3), Integer.valueOf(8)));

        Set<String> unscheduledSubscriptions = service.schedule(subscription);
        verifyNoSubscriptionsWereUnscheduled(unscheduledSubscriptions);

        // The maximum dataset size that could fit in the bucket is another
        // subscription the same size as the first
        final long expectedRequiredDataSetSize = subscription.getDataSetSize();
        final long requiredDataSetSize = service.proposeSchedule(subscription2)
                .getRequiredDataSetSize();
        assertEquals(
                "The required dataset size should have been returned from propose schedule!",
                expectedRequiredDataSetSize, requiredDataSetSize);
    }

    @Test
    public void testProposeScheduleSubscriptionsSecondDoesntFitReturnsRequiredSizeForSubscriptionUpdate() {

        Subscription<T, C> subscription = createSubscriptionThatFillsHalfABucket();
        ((GriddedTime) subscription.getTime()).setCycleTimes(Arrays.asList(
                Integer.valueOf(6), Integer.valueOf(8)));

        Set<String> unscheduledSubscriptions = service.schedule(subscription);
        verifyNoSubscriptionsWereUnscheduled(unscheduledSubscriptions);

        // We can only fit up to a bucket
        final long expectedRequiredDataSetSize = createSubscriptionThatFillsUpABucket()
                .getDataSetSize();

        // Try to update the subscription to fill two buckets
        subscription.setDataSetSize(createSubscriptionThatFillsUpTwoBuckets()
                .getDataSetSize());

        final long requiredDataSetSize = service.proposeSchedule(subscription)
                .getRequiredDataSetSize();
        assertEquals(
                "The required dataset size should have been returned from propose schedule!",
                expectedRequiredDataSetSize, requiredDataSetSize);
    }

    @Test
    public void testDetermineRequiredSizeReturnsZeroIfUnableToFitAtAll() {

        Subscription<T, C> subscription = createSubscriptionThatFillsUpABucket();
        Subscription<T, C> subscription2 = createSubscriptionThatFillsUpABucket();

        // subscription2 will not be able to schedule for cycle hour 8
        ((GriddedTime) subscription.getTime()).setCycleTimes(Arrays.asList(
                Integer.valueOf(6), Integer.valueOf(8)));
        ((GriddedTime) subscription2.getTime()).setCycleTimes(Arrays.asList(
                Integer.valueOf(3), Integer.valueOf(8)));

        Set<String> unscheduledSubscriptions = service.schedule(subscription);
        verifyNoSubscriptionsWereUnscheduled(unscheduledSubscriptions);

        // The maximum dataset size that could fit in the bucket is another
        // subscription the same size as the first
        final long expectedRequiredDataSetSize = 0;
        final long requiredDataSetSize = service.proposeSchedule(subscription2)
                .getRequiredDataSetSize();
        assertEquals("The required dataset size should have returned 0!",
                expectedRequiredDataSetSize, requiredDataSetSize);
    }

    @Test
    public void testReinitializeStartsNewBandwidthManager() {
        BandwidthManager<T, C> originalBandwidthManager = BandwidthServiceIntTest.this.bandwidthManager;

        service.reinitialize();

        assertNotSame(
                "Expected the BandwidthManager instance to have been replaced",
                originalBandwidthManager,
                EdexBandwidthContextFactory.getInstance());
    }

    @Test
    public void testGetEstimatedCompletionTimeReturnsLastBucketTimeForSubscription() {

        @SuppressWarnings("unchecked")
        AdhocSubscription<T, C> subscription = (AdhocSubscription<T, C>) AdhocSubscriptionFixture.INSTANCE
                .get(DataType.GRID);
        ((GriddedTime) subscription.getTime()).setCycleTimes(Arrays
                .asList(Integer.valueOf(0)));
        subscription.setDataSetSize(createSubscriptionThatFillsUpABucket()
                .getDataSetSize());

        Set<String> unscheduledSubscriptions = service.schedule(subscription);
        verifyNoSubscriptionsWereUnscheduled(unscheduledSubscriptions);

        // Jan 2, 18:00 CST
        Date expected = new Date(172800000L);
        Date actual = service.getEstimatedCompletionTime(subscription);
        assertEquals("Received incorrect estimated completion time!", expected,
                actual);
    }

    @Test
    public void testGetBandwidthGraphDataReturnsCorrectNumberOfSubscriptionNames() {

        // Two subscriptions that will fill up a bucket exactly
        Subscription<T, C> subscription = createSubscriptionThatFillsUpABucket();
        Subscription<T, C> subscription2 = createSubscriptionThatFillsUpABucket();

        // subscription2 will not be able to schedule for cycle hour 8
        ((GriddedTime) subscription.getTime()).setCycleTimes(Arrays.asList(
                Integer.valueOf(6), Integer.valueOf(8)));
        ((GriddedTime) subscription2.getTime()).setCycleTimes(Arrays
                .asList(Integer.valueOf(3)));

        service.schedule(subscription);
        service.schedule(subscription2);

        BandwidthGraphData graphData = service.getBandwidthGraphData();

        assertEquals("Incorrect number of subscriptions returned!", 2,
                graphData.getNumberOfSubscriptions());
    }

    @Test
    public void testGetBandwidthGraphDataReturnsCorrectBinMinutes() {

        // Two subscriptions that will fill up a bucket exactly
        Subscription<T, C> subscription = createSubscriptionThatFillsUpABucket();
        Subscription<T, C> subscription2 = createSubscriptionThatFillsUpABucket();

        // subscription2 will not be able to schedule for cycle hour 8
        ((GriddedTime) subscription.getTime()).setCycleTimes(Arrays.asList(
                Integer.valueOf(6), Integer.valueOf(8)));
        ((GriddedTime) subscription2.getTime()).setCycleTimes(Arrays
                .asList(Integer.valueOf(3)));

        service.schedule(subscription);
        service.schedule(subscription2);

        BandwidthGraphData graphData = service.getBandwidthGraphData();
        RetrievalPlan opsnetPlan = retrievalManager.getPlan(Network.OPSNET);

        assertEquals("Incorrect number of subscriptions returned!",
                opsnetPlan.getBucketMinutes(), graphData.getBinTimeInMinutes());
        SortedSet<BandwidthBucketDescription> descs = graphData
                .getNetworkBucketMap().get(Network.OPSNET);
        long earliestTime = descs.first().getBucketStartTime();
        long latestTime = descs.last().getBucketStartTime();
        assertEquals("Incorrect number of buckets returned", opsnetPlan
                .getBucketsInWindow(earliestTime, latestTime).size(),
                descs.size());
    }

    @Test
    public void testGetBandwidthGraphDataForFragmentedSubscription() {

        Subscription<T, C> subscription = createSubscriptionThatFillsUpTwoBuckets();
        subscription.setLatencyInMinutes(6);
        subscription.setPriority(SubscriptionPriority.HIGH);

        // Reserves a full bucket at 19700103 18:03:00 which fragments the
        // subscription to 19700103 18:00:00 and 18:06:00
        BandwidthAllocation allocation = createAllocationToReserveMiddleBucket(subscription);

        retrievalManager.schedule(Arrays.asList(allocation));

        bandwidthManager.schedule(subscription);

        BandwidthGraphData graphData = service.getBandwidthGraphData();
        final Map<String, List<TimeWindowData>> dataMap = graphData
                .getDataMap();

        final List<TimeWindowData> subscriptionOneTimeWindows = dataMap
                .get(subscription.getName());

        assertEquals(
                "Expected there to be two time windows for this subscription over 2 days",
                2, subscriptionOneTimeWindows.size());
        final TimeWindowData firstTimeWindow = subscriptionOneTimeWindows
                .get(0);
        final TimeWindowData secondTimeWindow = subscriptionOneTimeWindows
                .get(1);

        final List<Long> firstWindowBinStartTimes = firstTimeWindow
                .getBinStartTimes();
        final List<Long> secondWindowBinStartTimes = secondTimeWindow
                .getBinStartTimes();

        assertEquals("Incorrect number of bin start times found.", 2,
                firstWindowBinStartTimes.size());
        assertEquals("Incorrect number of bin start times found.", 2,
                secondWindowBinStartTimes.size());

        final List<SubscriptionRetrieval> subscriptionRetrievals = bandwidthDao
                .getSubscriptionRetrievals(subscription.getProvider(),
                        subscription.getDataSetName());

        final Iterator<SubscriptionRetrieval> iter = subscriptionRetrievals
                .iterator();

        // First retrieval window
        long expectedBinStartTime = iter.next().getStartTime()
                .getTimeInMillis();

        assertEquals(
                "Incorrect first bin start time in the first time window.",
                expectedBinStartTime, firstWindowBinStartTimes.get(0)
                        .longValue());

        expectedBinStartTime += (TimeUtil.MILLIS_PER_MINUTE * 3);
        assertEquals(
                "Incorrect second bin start time in the first time window.",
                expectedBinStartTime, firstWindowBinStartTimes.get(1)
                        .longValue());

        // Second retrieval window
        expectedBinStartTime = iter.next().getStartTime().getTimeInMillis();

        assertEquals(
                "Incorrect first bin start time in the second time window.",
                expectedBinStartTime, secondWindowBinStartTimes.get(0)
                        .longValue());

        // The middle bucket was already reserved, so we went ahead six minutes
        // and used that bucket
        expectedBinStartTime += (TimeUtil.MILLIS_PER_MINUTE * 6);

        assertEquals(
                "Incorrect second bin start time in the second time window.",
                expectedBinStartTime, secondWindowBinStartTimes.get(1)
                        .longValue());
    }

    @Test
    public void testGetBandwidthGraphDataReturnsCorrectTimeWindowsForSubscriptions() {

        // Two subscriptions that will fill up a bucket exactly
        Subscription<T, C> subscription = createSubscriptionThatFillsUpABucket();
        Subscription<T, C> subscription2 = createSubscriptionThatFillsUpABucket();

        // subscription2 will not be able to schedule for cycle hour 8
        ((GriddedTime) subscription.getTime()).setCycleTimes(Arrays.asList(
                Integer.valueOf(6), Integer.valueOf(8)));
        ((GriddedTime) subscription2.getTime()).setCycleTimes(Arrays
                .asList(Integer.valueOf(3)));

        service.schedule(subscription);
        service.schedule(subscription2);

        BandwidthGraphData graphData = service.getBandwidthGraphData();
        final Map<String, List<TimeWindowData>> dataMap = graphData
                .getDataMap();

        final List<TimeWindowData> subscriptionOneTimeWindows = dataMap
                .get(subscription.getName());
        final List<TimeWindowData> subscriptionTwoTimeWindows = dataMap
                .get(subscription2.getName());

        assertEquals(
                "Expected there to be four retrievals for this subscription over 2 days",
                4, subscriptionOneTimeWindows.size());
        assertEquals(
                "Expected there to be two retrievals for this subscription over 2 days",
                2, subscriptionTwoTimeWindows.size());
    }

    @Test
    public void testGetBandwidthGraphDataReturnsCorrectPrioritiesForSubscriptions() {

        // Two subscriptions that will fill up a bucket exactly
        Subscription<T, C> subscription = createSubscriptionThatFillsUpABucket();
        subscription.setPriority(SubscriptionPriority.NORMAL);
        Subscription<T, C> subscription2 = createSubscriptionThatFillsUpABucket();
        subscription.setPriority(SubscriptionPriority.HIGH);

        // subscription2 will not be able to schedule for cycle hour 8
        ((GriddedTime) subscription.getTime()).setCycleTimes(Arrays.asList(
                Integer.valueOf(6), Integer.valueOf(8)));
        ((GriddedTime) subscription2.getTime()).setCycleTimes(Arrays
                .asList(Integer.valueOf(3)));

        service.schedule(subscription);
        service.schedule(subscription2);

        BandwidthGraphData graphData = service.getBandwidthGraphData();
        final Map<String, SubscriptionPriority> priorityMap = graphData
                .getPriorityMap();

        assertThat(priorityMap.get(subscription.getName()),
                is(equalTo(subscription.getPriority())));

        assertThat(priorityMap.get(subscription2.getName()),
                is(equalTo(subscription2.getPriority())));
    }

    @Test
    public void testProposeScheduleSubscriptionsReturnsStatusSummary() {
        Subscription<T, C> subscription = createSubscriptionThatFillsUpTwoBuckets();

        ((GriddedTime) subscription.getTime()).setCycleTimes(Arrays.asList(
                Integer.valueOf(6), Integer.valueOf(8)));
        subscription.setLatencyInMinutes(3);

        IProposeScheduleResponse response = service
                .proposeSchedule(subscription);

        SubscriptionStatusSummary sum = service
                .getSubscriptionStatusSummary(subscription);

        List<BandwidthAllocation> allocationList = bandwidthDao
                .getBandwidthAllocations(Network.OPSNET);
        long actualStartTime = allocationList.get(0).getStartTime()
                .getTimeInMillis();
        long actualEndTime = allocationList.get(0).getEndTime()
                .getTimeInMillis();

        assertEquals("DataSize does not match", subscription.getDataSetSize(),
                sum.getDataSize());
        assertEquals("Latency does not match",
                subscription.getLatencyInMinutes(), sum.getLatency());
        assertEquals("Start time does not match", actualStartTime,
                sum.getStartTime());
        assertEquals("End time does not match", actualEndTime, sum.getEndTime());
    }

    @Test
    public void testProposeScheduleFragmentedSubscriptionReturnsStatusSummary() {
        Subscription<T, C> subscription = createSubscriptionThatFillsUpTwoBuckets();
        subscription.setLatencyInMinutes(6);
        subscription.setPriority(SubscriptionPriority.HIGH);

        // Reserves a full bucket at 19700103 18:03:00 which fragments the
        // subscription to 19700103 18:00:00 and 18:06:00
        BandwidthAllocation allocation = createAllocationToReserveMiddleBucket(subscription);

        retrievalManager.schedule(Arrays.asList(allocation));

        IProposeScheduleResponse response = service
                .proposeSchedule(subscription);

        SubscriptionStatusSummary sum = service
                .getSubscriptionStatusSummary(subscription);

        List<BandwidthAllocation> allocationList = bandwidthDao
                .getBandwidthAllocations(Network.OPSNET);
        for (BandwidthAllocation ba : allocationList) {
            System.out.println(ba);
        }
        long actualStartTime = -1;
        long actualEndTime = -1;

        for (BandwidthAllocation ba : allocationList) {
            if (ba instanceof SubscriptionRetrieval) {
                actualStartTime = ba.getStartTime().getTimeInMillis();
                actualEndTime = ba.getEndTime().getTimeInMillis();
                break;
            }
        }
        assertEquals("DataSize does not match", subscription.getDataSetSize(),
                sum.getDataSize());
        assertEquals("Latency does not match",
                subscription.getLatencyInMinutes(), sum.getLatency());
        assertEquals("Start time does not match", actualStartTime,
                sum.getStartTime());
        assertEquals("End time does not match", actualEndTime, sum.getEndTime());
    }

    /**
     * Creates a BandwidthAllocation that will fill up a bucket and reserve
     * itself for 01/03/1970 18:03:00
     * 
     * @return the allocation
     */
    private BandwidthAllocation createAllocationToReserveMiddleBucket(
            Subscription<T, C> subscription) {
        Calendar cal = TimeUtil.newCalendar();
        cal.set(Calendar.YEAR, 1970);
        cal.set(Calendar.MONTH, Calendar.JANUARY);
        cal.set(Calendar.DAY_OF_MONTH, 3);
        cal.set(Calendar.HOUR_OF_DAY, 18);
        cal.set(Calendar.MINUTE, 3);
        cal.set(Calendar.SECOND, 0);
        cal.set(Calendar.MILLISECOND, 0);

        BandwidthAllocation allocation = new BandwidthAllocation();
        allocation.setStartTime(cal);
        allocation.setEndTime(cal);
        allocation.setNetwork(subscription.getRoute());
        allocation.setPriority(SubscriptionPriority.NORMAL);
        allocation.setAgentType("someAgent");
        allocation.setEstimatedSize(subscription.getDataSetSize() / 2);

        return allocation;
    }

    /**
     * Verify that no subscriptions were unscheduled.
     * 
     * @param unscheduledSubscriptions
     *            the set of subscription names returned from the operation
     */
    private static void verifyNoSubscriptionsWereUnscheduled(
            Set<String> unscheduledSubscriptions) {
        assertTrue("There should not be any unscheduled subscriptions.",
                unscheduledSubscriptions.isEmpty());
    }

    /**
     * Verify that the specific subscription name was returned as unscheduled in
     * the results.
     * 
     * @param unscheduledSubscriptions
     *            the set of unscheduled subscription names
     * @param subscription
     *            the subscription
     */
    private static void verifySubscriptionWasNotAbleToBeFullyScheduled(
            Set<String> unscheduledSubscriptions, @SuppressWarnings("rawtypes")
            Subscription subscription) {
        assertEquals(
                "One and only one subscription should not have been able to fully schedule",
                1, unscheduledSubscriptions.size());
        assertEquals("The wrong subscription name was returned as unscheduled",
                subscription.getName(), unscheduledSubscriptions.iterator()
                        .next());
    }

    /**
     * {@inheritDoc}
     */
    @Override
    protected Network getRouteToUseForSubscription() {
        return Network.OPSNET;
    }
}
