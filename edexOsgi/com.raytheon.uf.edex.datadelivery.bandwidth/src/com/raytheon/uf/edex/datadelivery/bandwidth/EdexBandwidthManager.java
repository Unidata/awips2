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

import static com.raytheon.uf.common.registry.ebxml.encoder.RegistryEncoders.Type.JAXB;

import java.text.ParseException;
import java.util.ArrayList;
import java.util.Date;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.SortedSet;
import java.util.concurrent.Executors;
import java.util.concurrent.ScheduledExecutorService;
import java.util.concurrent.ScheduledThreadPoolExecutor;
import java.util.concurrent.TimeUnit;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import com.google.common.annotations.VisibleForTesting;
import com.google.common.collect.Lists;
import com.google.common.eventbus.AllowConcurrentEvents;
import com.google.common.eventbus.Subscribe;
import com.raytheon.uf.common.datadelivery.bandwidth.IBandwidthRequest;
import com.raytheon.uf.common.datadelivery.bandwidth.IBandwidthRequest.RequestType;
import com.raytheon.uf.common.datadelivery.bandwidth.ProposeScheduleResponse;
import com.raytheon.uf.common.datadelivery.registry.AdhocSubscription;
import com.raytheon.uf.common.datadelivery.registry.Coverage;
import com.raytheon.uf.common.datadelivery.registry.DataDeliveryRegistryObjectTypes;
import com.raytheon.uf.common.datadelivery.registry.DataSetMetaData;
import com.raytheon.uf.common.datadelivery.registry.GriddedDataSetMetaData;
import com.raytheon.uf.common.datadelivery.registry.Network;
import com.raytheon.uf.common.datadelivery.registry.PointDataSetMetaData;
import com.raytheon.uf.common.datadelivery.registry.PointTime;
import com.raytheon.uf.common.datadelivery.registry.SiteSubscription;
import com.raytheon.uf.common.datadelivery.registry.Subscription;
import com.raytheon.uf.common.datadelivery.registry.Time;
import com.raytheon.uf.common.datadelivery.registry.handlers.IAdhocSubscriptionHandler;
import com.raytheon.uf.common.datadelivery.registry.handlers.IDataSetMetaDataHandler;
import com.raytheon.uf.common.datadelivery.registry.handlers.ISubscriptionHandler;
import com.raytheon.uf.common.datadelivery.service.ISubscriptionNotificationService;
import com.raytheon.uf.common.event.EventBus;
import com.raytheon.uf.common.registry.ebxml.encoder.RegistryEncoders;
import com.raytheon.uf.common.registry.event.InsertRegistryEvent;
import com.raytheon.uf.common.registry.event.RegistryEvent;
import com.raytheon.uf.common.registry.event.RemoveRegistryEvent;
import com.raytheon.uf.common.registry.event.UpdateRegistryEvent;
import com.raytheon.uf.common.registry.handler.IRegistryObjectHandler;
import com.raytheon.uf.common.registry.handler.RegistryHandlerException;
import com.raytheon.uf.common.serialization.SerializationException;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.common.time.util.IPerformanceTimer;
import com.raytheon.uf.common.time.util.TimeUtil;
import com.raytheon.uf.common.util.CollectionUtil;
import com.raytheon.uf.common.util.FileUtil;
import com.raytheon.uf.common.util.IFileModifiedWatcher;
import com.raytheon.uf.edex.datadelivery.bandwidth.dao.BandwidthAllocation;
import com.raytheon.uf.edex.datadelivery.bandwidth.dao.BandwidthDataSetUpdate;
import com.raytheon.uf.edex.datadelivery.bandwidth.dao.IBandwidthDao;
import com.raytheon.uf.edex.datadelivery.bandwidth.dao.IBandwidthDbInit;
import com.raytheon.uf.edex.datadelivery.bandwidth.dao.SubscriptionRetrieval;
import com.raytheon.uf.edex.datadelivery.bandwidth.dao.SubscriptionRetrievalAttributes;
import com.raytheon.uf.edex.datadelivery.bandwidth.hibernate.IFindSubscriptionsForScheduling;
import com.raytheon.uf.edex.datadelivery.bandwidth.notification.BandwidthEventBus;
import com.raytheon.uf.edex.datadelivery.bandwidth.retrieval.RetrievalManager;
import com.raytheon.uf.edex.datadelivery.bandwidth.retrieval.RetrievalPlan;
import com.raytheon.uf.edex.datadelivery.bandwidth.retrieval.RetrievalStatus;
import com.raytheon.uf.edex.datadelivery.bandwidth.retrieval.SubscriptionRetrievalFulfilled;
import com.raytheon.uf.edex.datadelivery.bandwidth.util.BandwidthDaoUtil;
import com.raytheon.uf.edex.datadelivery.bandwidth.util.BandwidthUtil;
import com.raytheon.uf.edex.datadelivery.util.DataDeliveryIdUtil;

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
 * Jul 11, 2013 2106       djohnson     Look up subscription from the handler directly.
 * Jul 19, 2013 2209       dhladky      Fixed un-serialized subscription for pointData.
 * Sep 13, 2013 2267       bgonzale     Check for no subscription retrieval attribute found.
 * Sep 16, 2013 2383       bgonzale     Add exception information for no subscription found.
 *                                      Add throws to updatePointDataSetMetaData.
 * Oct 1 2013   1797       dhladky      Time and GriddedTime separation
 * Oct 10, 2013 1797       bgonzale     Refactored registry Time objects.
 * 10/23/2013   2385       bphillip     Change schedule method to scheduleAdhoc
 * Nov 04, 2013 2506       bgonzale     Added removeBandwidthSubscriptions method.
 *                                      Added subscriptionNotificationService field.
 *                                      Send notifications.
 * Nov 15, 2013 2545       bgonzale     Added check for subscription events before sending
 *                                      notifications.  Republish dataset metadata registry
 *                                      insert and update events as dataset metadata events.
 * Jan 13, 2014 2679       dhladky      Small Point data updates.   
 * Jan 14, 2014 2692       dhladky      AdhocSubscription handler
 * Jan 20, 2013 2398       dhladky      Fixed rescheduling beyond active period/expired window.                                 
 * Jan 24, 2013 2709       bgonzale     Changed parameter to shouldScheduleForTime to a Calendar.
 * Jan 29, 2014 2636       mpduff       Scheduling refactor.
 * Jan 30, 2014 2686       dhladky      refactor of retrieval.
 * Feb 06, 2014 2636       bgonzale     Added initializeScheduling method that uses the in-memory
 *                                      bandwidth manager to perform the scheduling initialization
 *                                      because of efficiency.
 * Feb 11, 2014 2771       bgonzale     Use Data Delivery ID instead of Site.
 * Feb 10, 2014 2636       mpduff       Pass Network map to be scheduled.
 * Feb 21, 2014, 2636      dhladky      Try catch to keep MaintTask from dying.
 * </pre>
 * 
 * @author djohnson
 * @version 1.0
 */
public abstract class EdexBandwidthManager<T extends Time, C extends Coverage>
        extends BandwidthManager<T, C> {

    private static final Pattern RAP_PATTERN = Pattern
            .compile(".*rap_f\\d\\d$");

    private final IDataSetMetaDataHandler dataSetMetaDataHandler;

    private final ISubscriptionHandler subscriptionHandler;

    private final IAdhocSubscriptionHandler adhocSubscriptionHandler;

    private final ScheduledExecutorService scheduler;

    private final ISubscriptionNotificationService subscriptionNotificationService;

    private final IFindSubscriptionsForScheduling findSubscriptionsStrategy;

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
     * @param subscriptionNotificationService
     */
    public EdexBandwidthManager(IBandwidthDbInit dbInit,
            IBandwidthDao<T, C> bandwidthDao,
            RetrievalManager retrievalManager,
            BandwidthDaoUtil<T, C> bandwidthDaoUtil,
            IDataSetMetaDataHandler dataSetMetaDataHandler,
            ISubscriptionHandler subscriptionHandler,
            IAdhocSubscriptionHandler adhocSubscriptionHandler,
            ISubscriptionNotificationService subscriptionNotificationService,
            IFindSubscriptionsForScheduling findSubscriptionsStrategy) {
        super(dbInit, bandwidthDao, retrievalManager, bandwidthDaoUtil);

        this.dataSetMetaDataHandler = dataSetMetaDataHandler;
        this.subscriptionHandler = subscriptionHandler;
        this.subscriptionNotificationService = subscriptionNotificationService;
        this.adhocSubscriptionHandler = adhocSubscriptionHandler;
        this.findSubscriptionsStrategy = findSubscriptionsStrategy;

        // schedule maintenance tasks
        scheduler = Executors.newSingleThreadScheduledExecutor();
    }

    @Override
    public List<String> initializeScheduling(
            Map<Network, List<Subscription>> subMap)
            throws SerializationException {
        List<String> unscheduledNames = new ArrayList<String>(0);

        try {
            for (Network key : subMap.keySet()) {
                List<Subscription<T, C>> subscriptions = new ArrayList<Subscription<T, C>>();
                // this loop is here only because of the generics mess
                for (Subscription s : subMap.get(key)) {
                    subscriptions.add(s);
                }
                ProposeScheduleResponse response = proposeScheduleSubscriptions(subscriptions);
                Set<String> unscheduled = response
                        .getUnscheduledSubscriptions();
                if (!unscheduled.isEmpty()) {
                    // if proposed was unable to schedule some subscriptions it
                    // will schedule nothing. schedule any that can be scheduled
                    // here.
                    List<Subscription<T, C>> subsToSchedule = new ArrayList<Subscription<T, C>>();
                    for (Subscription<T, C> s : subscriptions) {
                        if (!unscheduled.contains(s.getName())) {
                            subsToSchedule.add(s);
                        }
                    }

                    unscheduled.addAll(scheduleSubscriptions(subsToSchedule));

                    unscheduledNames.addAll(unscheduled);
                }
            }
        } finally {
            // TODO: Uncomment the last line in this comment block when fully
            // switched over to Java 1.7 and remove the finally block in
            // shutdown,
            // that is also marked as TODO
            // This will allow the bandwidth manager to be garbage collected
            // without
            // waiting for all of the delayed tasks to expire, currently they
            // are
            // manually removed in the shutdown method by casting to the
            // implementation and clearing the queue
            // scheduler.setRemoveOnCancelPolicy(true);
            scheduler.scheduleAtFixedRate(watchForConfigFileChanges, 1, 1,
                    TimeUnit.MINUTES);
            scheduler.scheduleAtFixedRate(new MaintenanceTask(), 30, 30,
                    TimeUnit.MINUTES);
        }
        return unscheduledNames;
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
    @SuppressWarnings("unchecked")
    @Subscribe
    public void subscriptionFulfilled(
            SubscriptionRetrievalFulfilled subscriptionRetrievalFulfilled) {

        statusHandler.info("subscriptionFulfilled() :: "
                + subscriptionRetrievalFulfilled.getSubscriptionRetrieval());

        SubscriptionRetrieval sr = subscriptionRetrievalFulfilled
                .getSubscriptionRetrieval();

        List<SubscriptionRetrieval> subscriptionRetrievals = bandwidthDao
                .querySubscriptionRetrievals(sr.getBandwidthSubscription());

        List<SubscriptionRetrieval> fulfilledList = new ArrayList<SubscriptionRetrieval>();

        // Look to see if all the SubscriptionRetrieval's for a subscription are
        // completed.
        for (SubscriptionRetrieval subscription : subscriptionRetrievals) {
            if (RetrievalStatus.FULFILLED.equals(subscription.getStatus())) {
                fulfilledList.add(subscription);
            }
        }

        // Remove the completed SubscriptionRetrieval Objects from the
        // plan..
        for (SubscriptionRetrieval fsr : fulfilledList) {
            RetrievalPlan plan = retrievalManager.getPlan(fsr.getNetwork());
            plan.remove(fsr);
            statusHandler.info("Removing fulfilled SubscriptionRetrieval: "
                    + fsr.getId());
        }
    }

    /**
     * When a Subscription is removed from the Registry, a RemoveRegistryEvent
     * is generated and forwarded to this method to remove the necessary
     * BandwidthReservations (and perhaps redefine others).
     * 
     * @param event
     */
    @SuppressWarnings("unchecked")
    @Subscribe
    @AllowConcurrentEvents
    public void subscriptionRemoved(RemoveRegistryEvent event) {
        String objectType = event.getObjectType();
        if (objectType != null) {
            if (DataDeliveryRegistryObjectTypes.isRecurringSubscription(event
                    .getObjectType())) {
                statusHandler
                        .info("Received Subscription removal notification for Subscription ["
                                + event.getId() + "]");
                removeBandwidthSubscriptions(event.getId());

                try {
                    Subscription<T, C> sub = (Subscription<T, C>) RegistryEncoders
                            .ofType(JAXB)
                            .decodeObject(event.getRemovedObject());
                    sendSubscriptionNotificationEvent(event, sub);
                } catch (SerializationException e) {
                    statusHandler
                            .handle(Priority.PROBLEM,
                                    "Failed to retrieve deleted object from RemoveRegistryEvent",
                                    e);
                }
            }
        }
    }

    /**
     * Listen for registry insert events necessary to drive Bandwidth
     * Management.
     * 
     * @param re
     *            The <code>InsertRegistryEvent</code> Object to evaluate.
     */
    @SuppressWarnings("unchecked")
    @Subscribe
    @AllowConcurrentEvents
    public void registryEventListener(InsertRegistryEvent re) {
        final String objectType = re.getObjectType();

        if (DataDeliveryRegistryObjectTypes.DATASETMETADATA.equals(objectType)) {
            publishDataSetMetaDataEvent(re);
        }
        if (DataDeliveryRegistryObjectTypes.isRecurringSubscription(re
                .getObjectType())) {
            Subscription<T, C> sub = getRegistryObjectById(subscriptionHandler,
                    re.getId());
            sendSubscriptionNotificationEvent(re, sub);
        }
    }

    protected void registryEventListener(UpdateRegistryEvent event) {
        final String objectType = event.getObjectType();

        if (DataDeliveryRegistryObjectTypes.DATASETMETADATA.equals(objectType)) {
            publishDataSetMetaDataEvent(event);
        }
    }

    protected void sendSubscriptionNotificationEvent(RegistryEvent event,
            Subscription<T, C> sub) {
        final String objectType = event.getObjectType();

        if (DataDeliveryRegistryObjectTypes.isRecurringSubscription(objectType)) {
            if (sub != null) {
                boolean isApplicableForTheLocalSite = sub.getOfficeIDs()
                        .contains(DataDeliveryIdUtil.getId());
                if (isApplicableForTheLocalSite) {
                    switch (event.getAction()) {
                    case UPDATE:
                        subscriptionNotificationService
                                .sendUpdatedSubscriptionNotification(sub,
                                        sub.getOwner());
                        break;
                    case INSERT:
                        subscriptionNotificationService
                                .sendCreatedSubscriptionNotification(sub,
                                        sub.getOwner());
                        break;
                    case DELETE:
                        subscriptionNotificationService
                                .sendDeletedSubscriptionNotification(sub,
                                        sub.getOwner());
                        break;
                    default:
                        statusHandler.handle(
                                Priority.PROBLEM,
                                "Invalid RegistryEvent action: "
                                        + event.getAction());
                    }
                }
            }
        }
    }

    private void publishDataSetMetaDataEvent(RegistryEvent re) {
        final String id = re.getId();
        DataSetMetaData<T> dsmd = getDataSetMetaData(id);

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
            statusHandler.error("No DataSetMetaData found for id [" + id + "]");
        }
    }

    @SuppressWarnings("unchecked")
    private DataSetMetaData<T> getDataSetMetaData(String id) {
        return getRegistryObjectById(dataSetMetaDataHandler, id);
    }

    protected static <M> M getRegistryObjectById(
            IRegistryObjectHandler<M> handler, String id) {
        try {
            return handler.getById(id);
        } catch (RegistryHandlerException e) {
            statusHandler.error("Error attempting to retrieve RegistryObject["
                    + id + "] from Registry.", e);
            return null;
        }
    }

    /**
     * @return the subscriptionHandler
     */
    public ISubscriptionHandler getSubscriptionHandler() {
        return subscriptionHandler;
    }

    /**
     * Process a {@link GriddedDataSetMetaData} that was received from the event
     * bus.
     * 
     * @param dataSetMetaData
     *            the metadadata
     */
    @SuppressWarnings("unchecked")
    @Subscribe
    public void updateGriddedDataSetMetaData(
            GriddedDataSetMetaData dataSetMetaData) throws ParseException {
        // Daily/Hourly/Monthly datasets
        if (dataSetMetaData.getCycle() == GriddedDataSetMetaData.NO_CYCLE) {
            updateDataSetMetaDataWithoutCycle((DataSetMetaData<T>) dataSetMetaData);
        }
        // Regular cycle containing datasets
        else {
            updateDataSetMetaDataWithCycle((DataSetMetaData<T>) dataSetMetaData);
        }
    }

    /**
     * Process a {@link PointDataSetMetaData} that was received from the event
     * bus.
     * 
     * @param dataSetMetaData
     *            the metadadata
     * @throws ParseException
     */
    @Subscribe
    public void updatePointDataSetMetaData(PointDataSetMetaData dataSetMetaData)
            throws ParseException {

        final PointTime time = dataSetMetaData.getTime();
        final String providerName = dataSetMetaData.getProviderName();
        final String dataSetName = dataSetMetaData.getDataSetName();
        final Date pointTimeStart = time.getStart();
        final Date pointTimeEnd = time.getEnd();

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
                    final SubscriptionRetrievalAttributes<T, C> subscriptionRetrievalAttributes = bandwidthDao
                            .getSubscriptionRetrievalAttributes(retrieval);
                    final Subscription<T, C> subscription = subscriptionRetrievalAttributes
                            .getSubscription();

                    if (subscription.getTime() instanceof PointTime) {
                        final PointTime subTime = (PointTime) subscription
                                .getTime();
                        subscription.setUrl(dataSetMetaData.getUrl());
                        subscription.setProvider(dataSetMetaData
                                .getProviderName());

                        subTime.setRequestStart(earliestRetrievalDataTime);
                        subTime.setRequestEnd(latestRetrievalDataTime);
                        subTime.setTimes(time.getTimes());
                        subscriptionRetrievalAttributes
                                .setSubscription(subscription);

                        bandwidthDao.update(subscriptionRetrievalAttributes);

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
            DataSetMetaData<T> dataSetMetaData) throws ParseException {
        bandwidthDao.newBandwidthDataSetUpdate(dataSetMetaData);

        // Looking for active subscriptions to the dataset.
        try {
            @SuppressWarnings("rawtypes")
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
            for (Subscription<T, C> subscription : subscriptions) {
                @SuppressWarnings("unchecked")
                Subscription<T, C> sub = updateSubscriptionWithDataSetMetaData(
                        subscription, dataSetMetaData);

                if (sub instanceof SiteSubscription) {
                    scheduleAdhoc(new AdhocSubscription<T, C>(
                            (SiteSubscription<T, C>) sub));
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
    @SuppressWarnings("unchecked")
    private void updateDataSetMetaDataWithCycle(
            DataSetMetaData<T> dataSetMetaData) throws ParseException {
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

                    SubscriptionRetrievalAttributes<T, C> attributes = bandwidthDao
                            .getSubscriptionRetrievalAttributes(retrieval);

                    Subscription<T, C> sub;
                    try {
                        sub = updateSubscriptionWithDataSetMetaData(
                                attributes.getSubscription(), dataSetMetaData);

                        // Update the SubscriptionRetrieval record with the new
                        // data...
                        attributes.setSubscription(sub);

                        bandwidthDao.update(attributes);
                    } catch (SerializationException e) {
                        statusHandler
                                .handle(Priority.PROBLEM,
                                        "Unable to serialize the subscription for the retrieval, skipping...",
                                        e);
                        continue;
                    }

                    retrieval.setStatus(RetrievalStatus.READY);

                    bandwidthDaoUtil.update(retrieval);

                    statusHandler.info(String.format(
                            "Updated retrieval [%s] for "
                                    + "subscription [%s] to use "
                                    + "url [%s] and "
                                    + "base reference time [%s]",
                            retrieval.getIdentifier(), sub.getName(),
                            dataSetMetaData.getUrl(),
                            BandwidthUtil.format(sub.getTime().getStart())));
                }
            }

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
     * {@inheritDoc}
     */
    @Override
    protected void unscheduleSubscriptionsForAllocations(
            List<BandwidthAllocation> unscheduled) {
        List<SubscriptionRetrieval> retrievals = Lists.newArrayList();
        for (BandwidthAllocation unscheduledAllocation : unscheduled) {
            if (unscheduledAllocation instanceof SubscriptionRetrieval) {
                SubscriptionRetrieval retrieval = (SubscriptionRetrieval) unscheduledAllocation;
                retrievals.add(retrieval);
            }
        }

        Set<Subscription<T, C>> subscriptions = new HashSet<Subscription<T, C>>();
        for (SubscriptionRetrieval retrieval : retrievals) {
            try {
                final SubscriptionRetrievalAttributes<T, C> sra = bandwidthDao
                        .getSubscriptionRetrievalAttributes(retrieval);
                if (sra != null) {
                    Subscription<T, C> sub = sra.getSubscription();
                    if (sub != null) {
                        subscriptions.add(sub);
                    }
                }
            } catch (SerializationException e) {
                statusHandler.handle(Priority.PROBLEM,
                        "Unable to deserialize a subscription", e);
                continue;
            }
        }

        for (Subscription<T, C> subscription : subscriptions) {
            subscription.setUnscheduled(true);
            subscriptionUpdated(subscription);
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
    @SuppressWarnings({ "rawtypes", "unchecked" })
    private static Subscription updateSubscriptionWithDataSetMetaData(
            Subscription sub, DataSetMetaData dataSetMetaData) {
        // TODO perfect candidate for the factory for time and coverage
        Time dsmdTime = dataSetMetaData.getTime();
        final Time subTime = sub.getTime();
        dsmdTime = handleCyclesAndSequences(subTime, dsmdTime);
        sub.setTime(dsmdTime);
        sub.setUrl(dataSetMetaData.getUrl());

        return sub;
    }

    /**
     * Signals the bandwidth map localization file is updated, perform a
     * reinitialize operation.
     */
    private void bandwidthMapConfigurationUpdated() {
        IBandwidthRequest<T, C> request = new IBandwidthRequest<T, C>();
        request.setRequestType(RequestType.REINITIALIZE);

        try {
            handleRequest(request);
        } catch (Exception e) {
            statusHandler.handle(Priority.PROBLEM,
                    "Error while reinitializing the bandwidth manager.", e);
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.edex.datadelivery.bandwidth.BandwidthManager#
     * getSubscriptionsToSchedule
     * (com.raytheon.uf.common.datadelivery.registry.Network)
     */
    @Override
    protected List<Subscription> getSubscriptionsToSchedule(Network network) {
        List<Subscription> subList = new ArrayList<Subscription>(0);
        try {
            Map<Network, List<Subscription>> activeSubs = findSubscriptionsStrategy
                    .findSubscriptionsToSchedule();
            if (activeSubs.get(network) != null) {
                subList = activeSubs.get(network);
            }
        } catch (Exception e) {
            statusHandler.handle(Priority.PROBLEM,
                    "Error retrieving subscriptions.", e);
        }

        return subList;
    }

    /**
     * Private inner work thread used to keep the RetrievalPlans up to date.
     */
    private class MaintenanceTask implements Runnable {
        @Override
        public void run() {

            try {

                IPerformanceTimer timer = TimeUtil.getPerformanceTimer();
                timer.start();
                statusHandler.info("MaintenanceTask starting...");

                for (RetrievalPlan plan : retrievalManager.getRetrievalPlans()
                        .values()) {
                    if (statusHandler.isPriorityEnabled(Priority.DEBUG)) {
                        statusHandler.info("MaintenanceTask: "
                                + plan.getNetwork());
                        statusHandler.info("MaintenanceTask: planStart: "
                                + plan.getPlanStart().getTime());
                        statusHandler.info("MaintenanceTask: planEnd: "
                                + plan.getPlanEnd().getTime());
                    }
                    plan.resize();
                    if (statusHandler.isPriorityEnabled(Priority.DEBUG)) {
                        statusHandler
                                .info("MaintenanceTask: resized planStart: "
                                        + plan.getPlanStart().getTime());
                        statusHandler.info("MaintenanceTask: resized planEnd: "
                                + plan.getPlanEnd().getTime());
                        statusHandler.info("MaintenanceTask: Update schedule");
                    }
                    // Find DEFERRED Allocations and load them into the plan...
                    List<BandwidthAllocation> deferred = bandwidthDao
                            .getDeferred(plan.getNetwork(), plan.getPlanEnd());
                    if (!deferred.isEmpty()) {
                        retrievalManager.schedule(deferred);
                    }
                }

                int numSubsProcessed = 0;
                for (RetrievalPlan plan : retrievalManager.getRetrievalPlans()
                        .values()) {
                    numSubsProcessed += updateSchedule(plan.getNetwork());
                }
                timer.stop();
                statusHandler.info("MaintenanceTask complete: "
                        + timer.getElapsed() + " - " + numSubsProcessed
                        + " Subscriptions processed.");

            } catch (Throwable t) {
                statusHandler.error("MaintenanceTask: Subscription update scheduling has failed", t);
            }
        }
    }
}
