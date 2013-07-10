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

import java.text.ParseException;
import java.util.Calendar;
import java.util.Date;
import java.util.List;
import java.util.SortedSet;
import java.util.concurrent.Executors;
import java.util.concurrent.ScheduledExecutorService;
import java.util.concurrent.ScheduledThreadPoolExecutor;
import java.util.concurrent.TimeUnit;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import com.google.common.annotations.VisibleForTesting;
import com.google.common.eventbus.AllowConcurrentEvents;
import com.google.common.eventbus.Subscribe;
import com.raytheon.uf.common.datadelivery.bandwidth.IBandwidthRequest;
import com.raytheon.uf.common.datadelivery.bandwidth.IBandwidthRequest.RequestType;
import com.raytheon.uf.common.datadelivery.registry.AdhocSubscription;
import com.raytheon.uf.common.datadelivery.registry.DataDeliveryRegistryObjectTypes;
import com.raytheon.uf.common.datadelivery.registry.DataSetMetaData;
import com.raytheon.uf.common.datadelivery.registry.GriddedDataSetMetaData;
import com.raytheon.uf.common.datadelivery.registry.PointDataSetMetaData;
import com.raytheon.uf.common.datadelivery.registry.PointTime;
import com.raytheon.uf.common.datadelivery.registry.SiteSubscription;
import com.raytheon.uf.common.datadelivery.registry.Subscription;
import com.raytheon.uf.common.datadelivery.registry.Time;
import com.raytheon.uf.common.datadelivery.registry.handlers.IDataSetMetaDataHandler;
import com.raytheon.uf.common.datadelivery.registry.handlers.ISubscriptionHandler;
import com.raytheon.uf.common.event.EventBus;
import com.raytheon.uf.common.registry.event.InsertRegistryEvent;
import com.raytheon.uf.common.registry.event.RemoveRegistryEvent;
import com.raytheon.uf.common.registry.handler.IRegistryObjectHandler;
import com.raytheon.uf.common.registry.handler.RegistryHandlerException;
import com.raytheon.uf.common.serialization.SerializationException;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.common.time.util.TimeUtil;
import com.raytheon.uf.common.util.CollectionUtil;
import com.raytheon.uf.common.util.FileUtil;
import com.raytheon.uf.common.util.IFileModifiedWatcher;
import com.raytheon.uf.edex.datadelivery.bandwidth.dao.BandwidthAllocation;
import com.raytheon.uf.edex.datadelivery.bandwidth.dao.BandwidthDataSetUpdate;
import com.raytheon.uf.edex.datadelivery.bandwidth.dao.BandwidthSubscription;
import com.raytheon.uf.edex.datadelivery.bandwidth.dao.IBandwidthDao;
import com.raytheon.uf.edex.datadelivery.bandwidth.dao.IBandwidthDbInit;
import com.raytheon.uf.edex.datadelivery.bandwidth.dao.SubscriptionRetrieval;
import com.raytheon.uf.edex.datadelivery.bandwidth.notification.BandwidthEventBus;
import com.raytheon.uf.edex.datadelivery.bandwidth.retrieval.RetrievalManager;
import com.raytheon.uf.edex.datadelivery.bandwidth.retrieval.RetrievalPlan;
import com.raytheon.uf.edex.datadelivery.bandwidth.retrieval.RetrievalStatus;
import com.raytheon.uf.edex.datadelivery.bandwidth.retrieval.SubscriptionRetrievalFulfilled;
import com.raytheon.uf.edex.datadelivery.bandwidth.util.BandwidthDaoUtil;
import com.raytheon.uf.edex.datadelivery.bandwidth.util.BandwidthUtil;

/**
 * Implementation of {@link BandwidthManager} that isolates EDEX specific
 * functionality. This keeps things out of the {@link InMemoryBandwidthManager}
 * that could interfere with garbage collection/threading concerns.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jul 10, 2013 2106       djohnson     Extracted from {@link BandwidthManager}.
 * 
 * </pre>
 * 
 * @author djohnson
 * @version 1.0
 */
public abstract class EdexBandwidthManager extends BandwidthManager {

    private static final Pattern RAP_PATTERN = Pattern
            .compile(".*rap_f\\d\\d$");

    private final IDataSetMetaDataHandler dataSetMetaDataHandler;

    private final ISubscriptionHandler subscriptionHandler;

    private final ScheduledExecutorService scheduler;

    @VisibleForTesting
    final Runnable watchForConfigFileChanges = new Runnable() {

        private final IFileModifiedWatcher fileModifiedWatcher = FileUtil
                .getFileModifiedWatcher(EdexBandwidthContextFactory
                        .getBandwidthMapConfig());

        @Override
        public void run() {
            if (fileModifiedWatcher.hasBeenModified()) {
                bandwidthMapConfigurationUpdated();
            }
        }
    };

    /**
     * @param dbInit
     * @param bandwidthDao
     * @param retrievalManager
     * @param bandwidthDaoUtil
     */
    public EdexBandwidthManager(IBandwidthDbInit dbInit,
            IBandwidthDao bandwidthDao, RetrievalManager retrievalManager,
            BandwidthDaoUtil bandwidthDaoUtil,
            IDataSetMetaDataHandler dataSetMetaDataHandler,
            ISubscriptionHandler subscriptionHandler) {
        super(dbInit, bandwidthDao, retrievalManager, bandwidthDaoUtil);

        this.dataSetMetaDataHandler = dataSetMetaDataHandler;
        this.subscriptionHandler = subscriptionHandler;

        // schedule maintenance tasks
        scheduler = Executors.newScheduledThreadPool(1);
        // TODO: Uncomment the last line in this comment block when fully
        // switched over to Java 1.7 and remove the finally block in shutdown,
        // that is also marked as TODO
        // This will allow the bandwidth manager to be garbage collected without
        // waiting for all of the delayed tasks to expire, currently they are
        // manually removed in the shutdown method by casting to the
        // implementation and clearing the queue
        // scheduler.setRemoveOnCancelPolicy(true);
        scheduler.scheduleAtFixedRate(watchForConfigFileChanges, 1, 1,
                TimeUnit.MINUTES);
        scheduler.scheduleAtFixedRate(new MaintanenceTask(), 1, 5,
                TimeUnit.MINUTES);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    protected void shutdownInternal() {
        unregisterFromEventBus();
        unregisterFromBandwidthEventBus();

        try {
            scheduler.shutdownNow();
        } catch (Exception e) {
            statusHandler.handle(Priority.WARN,
                    "Unable to shutdown the scheduler.", e);
        } finally {
            // TODO: Remove this finally block when fully switched over to Java
            // 1.7. See TODO comment in the constructor.
            if (scheduler != null
                    && scheduler instanceof ScheduledThreadPoolExecutor) {
                ((ScheduledThreadPoolExecutor) scheduler).getQueue().clear();
            }
        }
        EventBus.unregister(retrievalManager);
    }

    /**
     * Unregister from the {@link EventBus}.
     */
    private void unregisterFromEventBus() {
        EventBus.unregister(this);
    }

    /**
     * Unregister from the {@link BandwidthEventBus}.
     */
    private void unregisterFromBandwidthEventBus() {
        BandwidthEventBus.unregister(this);
    }

    /**
     * The callback method for BandwidthEventBus to use to notify
     * BandwidthManager that retrievalManager has completed the retrievals for a
     * Subscription. The updated BandwidthSubscription Object is placed on the
     * BandwidthEventBus.
     * 
     * @param subscription
     *            The completed subscription.
     */
    @Subscribe
    public void subscriptionFulfilled(
            SubscriptionRetrievalFulfilled subscriptionRetrievalFulfilled) {

        statusHandler.info("subscriptionFullfilled() :: "
                + subscriptionRetrievalFulfilled);

        SubscriptionRetrieval sr = subscriptionRetrievalFulfilled
                .getSubscriptionRetrieval();

        List<SubscriptionRetrieval> subscriptionRetrievals = bandwidthDao
                .querySubscriptionRetrievals(sr.getBandwidthSubscription());

        // Look to see if all the SubscriptionRetrieval's for a subscription are
        // completed.
        boolean complete = true;
        for (SubscriptionRetrieval subscription : subscriptionRetrievals) {
            if (!RetrievalStatus.FULFILLED.equals(subscription.getStatus())) {
                complete = false;
                break;
            }
        }

        if (complete) {
            // Remove the completed SubscriptionRetrieval Objects from the
            // plan..
            RetrievalPlan plan = retrievalManager.getPlan(sr.getNetwork());
            plan.remove(sr);

            // Schedule the next iteration of the subscription
            BandwidthSubscription dao = sr.getBandwidthSubscription();
            Subscription subscription = null;
            try {
                subscription = dao.getSubscription();
            } catch (SerializationException e) {
                statusHandler.error(
                        "Failed to extract Subscription from BandwidthSubscription ["
                                + dao.getIdentifier() + "]", e);
                // No sense in continuing
                return;
            }

            // AdhocSubscriptions are one and done, so don't reschedule.
            if (subscription instanceof AdhocSubscription) {
                return;
            }

            Calendar next = BandwidthUtil.copy(dao.getBaseReferenceTime());
            // See how far into the future the plan goes..
            int days = retrievalManager.getPlan(dao.getRoute()).getPlanDays();

            for (int day = 1; day <= days; day++) {

                next.add(Calendar.DAY_OF_YEAR, 1);
                // Since subscriptions are based on cycles in a day, add one day
                // to the
                // completed BandwidthSubscription to get the next days
                // retrieval.

                // Now check if that BandwidthSubscription has already been
                // scheduled.
                BandwidthSubscription a = bandwidthDao
                        .getBandwidthSubscription(dao.getRegistryId(), next);
                if (a == null) {
                    // Create the new BandwidthSubscription record with the next
                    // time..
                    try {
                        a = bandwidthDao.newBandwidthSubscription(subscription,
                                next);
                    } catch (SerializationException e) {

                        statusHandler.error(
                                "Failed to create new BandwidthSubscription from Subscription ["
                                        + subscription.getId()
                                        + "] baseReferenceTime ["
                                        + BandwidthUtil.format(next) + "]", e);
                    }

                    schedule(a);
                } else {
                    statusHandler
                            .info("Subscription ["
                                    + subscription.getName()
                                    + "] has already been scheduled for baseReferenceTime ["
                                    + BandwidthUtil.format(next) + "]");
                }
            }
        }
    }

    /**
     * When a Subscription is removed from the Registry, a RemoveRegistryEvent
     * is generated and forwarded to this method to remove the necessary
     * BandwidthReservations (and perhaps redefine others).
     * 
     * @param event
     */
    @Subscribe
    @AllowConcurrentEvents
    public void subscriptionRemoved(RemoveRegistryEvent event) {
        String objectType = event.getObjectType();
        if (objectType != null) {
            if (DataDeliveryRegistryObjectTypes.SITE_SUBSCRIPTION
                    .equals(objectType)
                    || DataDeliveryRegistryObjectTypes.SHARED_SUBSCRIPTION
                            .equals(objectType)) {
                statusHandler
                        .info("Recieved Subscription removal notification for Subscription ["
                                + event.getId() + "]");
                // Need to locate and remove all BandwidthReservations for the
                // given subscription..
                List<BandwidthSubscription> l = bandwidthDao
                        .getBandwidthSubscriptionByRegistryId(event.getId());
                if (!l.isEmpty()) {
                    remove(l);
                }
            }
        }
    }

    /**
     * Create a hook into the EDEX Notification sub-system to receive the the
     * necessary InsertRegistryEvents to drive Bandwidth Management.
     * 
     * @param re
     *            The <code>InsertRegistryEvent</code> Object to evaluate.
     */
    @Subscribe
    @AllowConcurrentEvents
    public void registryEventListener(InsertRegistryEvent re) {
        final String objectType = re.getObjectType();
        final String id = re.getId();

        if (DataDeliveryRegistryObjectTypes.DATASETMETADATA.equals(objectType)) {

            DataSetMetaData dsmd = getDataSetMetaData(id);

            if (dsmd != null) {
                // Repost the Object to the BandwidthEventBus to free
                // the notification thread.

                // TODO: A hack to prevent rap_f and rap datasets being
                // Identified as the
                // same dataset...
                Matcher matcher = RAP_PATTERN.matcher(dsmd.getUrl());
                if (matcher.matches()) {
                    statusHandler
                            .info("Found rap_f dataset - updating dataset name from ["
                                    + dsmd.getDataSetName() + "] to [rap_f]");
                    dsmd.setDataSetName("rap_f");
                }
                // TODO: End of hack..

                BandwidthEventBus.publish(dsmd);
            } else {
                statusHandler.error("No DataSetMetaData found for id [" + id
                        + "]");
            }

        }
    }

    private DataSetMetaData getDataSetMetaData(String id) {
        return getRegistryObjectById(dataSetMetaDataHandler, id);
    }

    private static <T> T getRegistryObjectById(
            IRegistryObjectHandler<T> handler, String id) {
        try {
            return handler.getById(id);
        } catch (RegistryHandlerException e) {
            statusHandler.error("Error attempting to retrieve RegistryObject["
                    + id + "] from Registry.", e);
            return null;
        }
    }

    /**
     * Process a {@link GriddedDataSetMetaData} that was received from the event
     * bus.
     * 
     * @param dataSetMetaData
     *            the metadadata
     */
    @Subscribe
    public void updateGriddedDataSetMetaData(
            GriddedDataSetMetaData dataSetMetaData) throws ParseException {
        // Daily/Hourly/Monthly datasets
        if (dataSetMetaData.getCycle() == GriddedDataSetMetaData.NO_CYCLE) {
            updateDataSetMetaDataWithoutCycle(dataSetMetaData);
        }
        // Regular cycle containing datasets
        else {
            updateDataSetMetaDataWithCycle(dataSetMetaData);
        }
    }

    /**
     * Process a {@link PointDataSetMetaData} that was received from the event
     * bus.
     * 
     * @param dataSetMetaData
     *            the metadadata
     */
    @Subscribe
    public void updatePointDataSetMetaData(PointDataSetMetaData dataSetMetaData) {
        // TODO: Change PointDataSetMetaData to only be able to use PointTime
        // objects
        final PointTime time = (PointTime) dataSetMetaData.getTime();
        final String providerName = dataSetMetaData.getProviderName();
        final String dataSetName = dataSetMetaData.getDataSetName();
        final Date pointTimeStart = time.getStartDate();
        final Date pointTimeEnd = time.getEndDate();

        final SortedSet<Integer> allowedRefreshIntervals = PointTime
                .getAllowedRefreshIntervals();
        final long maxAllowedRefreshIntervalInMillis = TimeUtil.MILLIS_PER_MINUTE
                * allowedRefreshIntervals.last();
        final long minAllowedRefreshIntervalInMillis = TimeUtil.MILLIS_PER_MINUTE
                * allowedRefreshIntervals.first();

        // Find any retrievals ranging from those with the minimum refresh
        // interval to the maximum refresh interval
        final Date startDate = new Date(pointTimeStart.getTime()
                + minAllowedRefreshIntervalInMillis);
        final Date endDate = new Date(pointTimeEnd.getTime()
                + maxAllowedRefreshIntervalInMillis);

        final SortedSet<SubscriptionRetrieval> subscriptionRetrievals = bandwidthDao
                .getSubscriptionRetrievals(providerName, dataSetName,
                        RetrievalStatus.SCHEDULED, startDate, endDate);

        if (!CollectionUtil.isNullOrEmpty(subscriptionRetrievals)) {
            for (SubscriptionRetrieval retrieval : subscriptionRetrievals) {
                // Now check and make sure that at least one of the times falls
                // in their retrieval range, their latency is the retrieval
                // interval
                final int retrievalInterval = retrieval
                        .getSubscriptionLatency();

                // This is the latest time on the data we care about, once the
                // retrieval is signaled to go it retrieves everything up to
                // its start time
                final Date latestRetrievalDataTime = retrieval.getStartTime()
                        .getTime();
                // This is the earliest possible time this retrieval cares about
                final Date earliestRetrievalDataTime = new Date(
                        latestRetrievalDataTime.getTime()
                                - (TimeUtil.MILLIS_PER_MINUTE * retrievalInterval));

                // If the end is before any times we care about or the start is
                // after the latest times we care about, skip it
                if (pointTimeEnd.before(earliestRetrievalDataTime)
                        || pointTimeStart.after(latestRetrievalDataTime)) {
                    continue;
                }

                try {
                    // Update the retrieval times on the subscription object
                    // which goes through the retrieval process
                    final Subscription subscription = retrieval
                            .getSubscription();
                    subscription.setUrl(dataSetMetaData.getUrl());
                    subscription.setProvider(dataSetMetaData.getProviderName());

                    if (subscription.getTime() instanceof PointTime) {
                        final PointTime subTime = (PointTime) subscription
                                .getTime();
                        subTime.setRequestStartAsDate(earliestRetrievalDataTime);
                        subTime.setRequestEndAsDate(latestRetrievalDataTime);
                        subTime.setTimes(time.getTimes());
                        // Now update the retrieval to be ready
                        retrieval.setStatus(RetrievalStatus.READY);
                        bandwidthDaoUtil.update(retrieval);
                    } else {
                        throw new IllegalArgumentException(
                                "Subscription time not PointType! "
                                        + subscription.getName());
                    }

                } catch (SerializationException e) {
                    statusHandler.handle(Priority.PROBLEM,
                            e.getLocalizedMessage(), e);
                }
            }
        }
    }

    /**
     * Handles updates for datasets that do not contain cycles.
     * 
     * @param dataSetMetaData
     *            the dataset metadata
     * @throws ParseException
     *             on parsing errors
     */
    private void updateDataSetMetaDataWithoutCycle(
            GriddedDataSetMetaData dataSetMetaData) throws ParseException {
        bandwidthDao.newBandwidthDataSetUpdate(dataSetMetaData);

        // Looking for active subscriptions to the dataset.
        try {
            List<Subscription> subscriptions = subscriptionHandler
                    .getActiveByDataSetAndProvider(
                            dataSetMetaData.getDataSetName(),
                            dataSetMetaData.getProviderName());

            if (subscriptions.isEmpty()) {
                return;
            }

            statusHandler
                    .info(String
                            .format("Found [%s] subscriptions that will have an "
                                    + "adhoc subscription generated and scheduled for url [%s].",
                                    subscriptions.size(),
                                    dataSetMetaData.getUrl()));

            // Create an adhoc for each one, and schedule it
            for (Subscription subscription : subscriptions) {
                Subscription sub = updateSubscriptionWithDataSetMetaData(
                        subscription, dataSetMetaData);

                if (sub instanceof SiteSubscription) {
                    schedule(new AdhocSubscription((SiteSubscription) sub));
                } else {
                    statusHandler
                            .warn("Unable to create adhoc queries for shared subscriptions at this point.  This functionality should be added in the future...");
                }
            }
        } catch (RegistryHandlerException e) {
            statusHandler.handle(Priority.PROBLEM,
                    "Failed to lookup subscriptions.", e);
        }
    }

    /**
     * Handles updates for datasets that contain cycles.
     * 
     * @param dataSetMetaData
     *            the dataset metadata
     * @throws ParseException
     *             on parsing errors
     */
    private void updateDataSetMetaDataWithCycle(
            GriddedDataSetMetaData dataSetMetaData) throws ParseException {
        BandwidthDataSetUpdate dataset = bandwidthDao
                .newBandwidthDataSetUpdate(dataSetMetaData);

        // Looking for active subscriptions to the dataset.
        List<SubscriptionRetrieval> subscriptions = bandwidthDao
                .getSubscriptionRetrievals(dataset.getProviderName(),
                        dataset.getDataSetName(), dataset.getDataSetBaseTime());

        if (!subscriptions.isEmpty()) {
            // Loop through the scheduled SubscriptionRetrievals and mark
            // the scheduled retrievals as ready for retrieval
            for (SubscriptionRetrieval retrieval : subscriptions) {
                // TODO: Evaluate the state changes for receiving multiple
                // dataset update messages. This seems to be happening
                // quite a bit.

                if (RetrievalStatus.SCHEDULED.equals(retrieval.getStatus())) {
                    // Need to update the Subscription Object in the
                    // SubscriptionRetrieval with the current DataSetMetaData
                    // URL and time Object
                    Subscription sub;
                    try {
                        sub = updateSubscriptionWithDataSetMetaData(
                                retrieval.getSubscription(), dataSetMetaData);

                        // Update the SubscriptionRetrieval record with the new
                        // data...
                        retrieval.setSubscription(sub);
                    } catch (SerializationException e) {
                        statusHandler
                                .handle(Priority.PROBLEM,
                                        "Unable to serialize the subscription for the retrieval, skipping...",
                                        e);
                        continue;
                    }

                    retrieval.setStatus(RetrievalStatus.READY);

                    bandwidthDaoUtil.update(retrieval);

                    statusHandler
                            .info(String.format("Updated retrieval [%s] for "
                                    + "subscription [%s] to use "
                                    + "url [%s] and "
                                    + "base reference time [%s]", retrieval
                                    .getIdentifier(), sub.getName(),
                                    dataSetMetaData.getUrl(), BandwidthUtil
                                            .format(sub.getTime()
                                                    .getStartDate())));
                }
            }

            // Notify RetrievalAgentManager of updated RetrievalRequests.
            retrievalManager.wakeAgents();
        } else {
            if (statusHandler.isPriorityEnabled(Priority.DEBUG)) {
                statusHandler
                        .debug("No Subscriptions scheduled for BandwidthDataSetUpdate ["
                                + dataset.getIdentifier()
                                + "] base time ["
                                + BandwidthUtil.format(dataset
                                        .getDataSetBaseTime()) + "]");
            }
        }
    }

    /**
     * Updates a {@link Subscription) to reflect important attributes of the
     * specified {@link DataSetMetaData}.
     * 
     * @param sub
     *            the subscription
     * @param dataSetMetaData
     *            the datasetmetadata update
     * @return the subscription
     */
    private static Subscription updateSubscriptionWithDataSetMetaData(
            Subscription sub, DataSetMetaData dataSetMetaData) {
        final Time dsmdTime = dataSetMetaData.getTime();
        final Time subTime = sub.getTime();
        dsmdTime.setSelectedTimeIndices(subTime.getSelectedTimeIndices());
        dsmdTime.setCycleTimes(subTime.getCycleTimes());
        sub.setTime(dsmdTime);
        sub.setUrl(dataSetMetaData.getUrl());

        return sub;
    }

    /**
     * Signals the bandwidth map localization file is updated, perform a
     * reinitialize operation.
     */
    private void bandwidthMapConfigurationUpdated() {
        IBandwidthRequest request = new IBandwidthRequest();
        request.setRequestType(RequestType.REINITIALIZE);

        try {
            handleRequest(request);
        } catch (Exception e) {
            statusHandler.handle(Priority.PROBLEM,
                    "Error while reinitializing the bandwidth manager.", e);
        }
    }

    /**
     * Private inner work thread used to keep the RetrievalPlans up to date.
     */
    private class MaintanenceTask implements Runnable {

        @Override
        public void run() {
            for (RetrievalPlan plan : retrievalManager.getRetrievalPlans()
                    .values()) {
                plan.resize();
                Calendar newEnd = plan.getPlanEnd();

                // Find DEFERRED Allocations and load them into the plan...
                List<BandwidthAllocation> deferred = bandwidthDao.getDeferred(
                        plan.getNetwork(), newEnd);
                if (!deferred.isEmpty()) {
                    retrievalManager.schedule(deferred);
                }
            }
        }
    }

}
