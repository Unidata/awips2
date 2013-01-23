package com.raytheon.uf.edex.datadelivery.bandwidth;

import java.text.ParseException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Calendar;
import java.util.Collections;
import java.util.Date;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Set;
import java.util.SortedSet;
import java.util.TreeSet;
import java.util.concurrent.Executors;
import java.util.concurrent.ScheduledExecutorService;
import java.util.concurrent.TimeUnit;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import org.springframework.context.support.ClassPathXmlApplicationContext;

import com.google.common.annotations.VisibleForTesting;
import com.google.common.collect.ArrayListMultimap;
import com.google.common.collect.Multimap;
import com.google.common.collect.Sets;
import com.google.common.eventbus.AllowConcurrentEvents;
import com.google.common.eventbus.Subscribe;
import com.raytheon.edex.util.Util;
import com.raytheon.uf.common.auth.exception.AuthorizationException;
import com.raytheon.uf.common.auth.user.IUser;
import com.raytheon.uf.common.datadelivery.bandwidth.IBandwidthRequest;
import com.raytheon.uf.common.datadelivery.bandwidth.IBandwidthRequest.RequestType;
import com.raytheon.uf.common.datadelivery.bandwidth.ProposeScheduleResponse;
import com.raytheon.uf.common.datadelivery.bandwidth.data.BandwidthGraphData;
import com.raytheon.uf.common.datadelivery.registry.AdhocSubscription;
import com.raytheon.uf.common.datadelivery.registry.DataDeliveryRegistryObjectTypes;
import com.raytheon.uf.common.datadelivery.registry.DataSetMetaData;
import com.raytheon.uf.common.datadelivery.registry.GriddedDataSetMetaData;
import com.raytheon.uf.common.datadelivery.registry.Network;
import com.raytheon.uf.common.datadelivery.registry.Subscription;
import com.raytheon.uf.common.datadelivery.registry.Time;
import com.raytheon.uf.common.datadelivery.registry.handlers.DataDeliveryHandlers;
import com.raytheon.uf.common.registry.event.InsertRegistryEvent;
import com.raytheon.uf.common.registry.event.RemoveRegistryEvent;
import com.raytheon.uf.common.registry.handler.IRegistryObjectHandler;
import com.raytheon.uf.common.registry.handler.RegistryHandlerException;
import com.raytheon.uf.common.serialization.SerializationException;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.common.time.util.ITimer;
import com.raytheon.uf.common.time.util.TimeUtil;
import com.raytheon.uf.common.util.LogUtil;
import com.raytheon.uf.common.util.algorithm.AlgorithmUtil;
import com.raytheon.uf.common.util.algorithm.AlgorithmUtil.IBinarySearchResponse;
import com.raytheon.uf.edex.auth.req.AbstractPrivilegedRequestHandler;
import com.raytheon.uf.edex.auth.resp.AuthorizationResponse;
import com.raytheon.uf.edex.core.EDEXUtil;
import com.raytheon.uf.edex.datadelivery.bandwidth.dao.BandwidthAllocation;
import com.raytheon.uf.edex.datadelivery.bandwidth.dao.DataSetMetaDataDao;
import com.raytheon.uf.edex.datadelivery.bandwidth.dao.IBandwidthDao;
import com.raytheon.uf.edex.datadelivery.bandwidth.dao.IBandwidthDbInit;
import com.raytheon.uf.edex.datadelivery.bandwidth.dao.SubscriptionDao;
import com.raytheon.uf.edex.datadelivery.bandwidth.dao.SubscriptionRetrieval;
import com.raytheon.uf.edex.datadelivery.bandwidth.interfaces.BandwidthInitializer;
import com.raytheon.uf.edex.datadelivery.bandwidth.interfaces.ISubscriptionAggregator;
import com.raytheon.uf.edex.datadelivery.bandwidth.notification.BandwidthEventBus;
import com.raytheon.uf.edex.datadelivery.bandwidth.processing.SimpleSubscriptionAggregator;
import com.raytheon.uf.edex.datadelivery.bandwidth.retrieval.BandwidthMap;
import com.raytheon.uf.edex.datadelivery.bandwidth.retrieval.BandwidthRoute;
import com.raytheon.uf.edex.datadelivery.bandwidth.retrieval.RetrievalManager;
import com.raytheon.uf.edex.datadelivery.bandwidth.retrieval.RetrievalPlan;
import com.raytheon.uf.edex.datadelivery.bandwidth.retrieval.RetrievalPlan.BandwidthBucket;
import com.raytheon.uf.edex.datadelivery.bandwidth.retrieval.RetrievalStatus;
import com.raytheon.uf.edex.datadelivery.bandwidth.retrieval.SubscriptionRetrievalFulfilled;
import com.raytheon.uf.edex.datadelivery.bandwidth.util.BandwidthDaoUtil;
import com.raytheon.uf.edex.datadelivery.bandwidth.util.BandwidthUtil;
import com.raytheon.uf.edex.event.EventBus;

/**
 * Abstract {@link IBandwidthManager} implementation which provides core
 * functionality. Intentionally package-private to hide implementation details.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Feb 07, 2012            dhladky      Initial creation
 * Aug 11, 2012 726        jspinks      Modifications for implementing bandwidth management.
 * Oct 10, 2012 0726       djohnson     Changes to support new subscription/adhoc retrievals, use enabled flag,
 *                                      add support for non-cyclic subscriptions.
 * Oct 23, 2012 1286       djohnson     Add more service requests.
 * Nov 20, 2012 1286       djohnson     Add proposing scheduling subscriptions, change some logging to debug.
 * Dec 06, 2012 1397       djohnson     Add ability to get bandwidth graph data.
 * Dec 11, 2012 1403       djohnson     Adhoc subscriptions no longer go to the registry.
 * Dec 12, 2012 1286       djohnson     Remove shutdown hook and finalize().
 * 
 * </pre>
 * 
 * @author dhladky
 * @version 1.0
 */
abstract class BandwidthManager extends
        AbstractPrivilegedRequestHandler<IBandwidthRequest> implements
        IBandwidthManager {

    private static final IUFStatusHandler statusHandler = UFStatus
            .getHandler(BandwidthManager.class);

    private static final Pattern RAP_PATTERN = Pattern
            .compile(".*rap_f\\d\\d$");

    private ISubscriptionAggregator aggregator;

    private BandwidthInitializer initializer;

    private final ScheduledExecutorService scheduler;

    private final IBandwidthDao bandwidthDao;

    private final BandwidthDaoUtil bandwidthDaoUtil;

    private final IBandwidthDbInit dbInit;

    @VisibleForTesting
    final RetrievalManager retrievalManager;

    public BandwidthManager(IBandwidthDbInit dbInit,
            IBandwidthDao bandwidthDao, RetrievalManager retrievalManager,
            BandwidthDaoUtil bandwidthDaoUtil) {
        this.dbInit = dbInit;
        this.bandwidthDao = bandwidthDao;
        this.retrievalManager = retrievalManager;
        this.bandwidthDaoUtil = bandwidthDaoUtil;

        EventBus.getInstance().register(this);
        BandwidthEventBus.register(this);

        // Start a MaintenanceTask
        scheduler = Executors.newScheduledThreadPool(1);
        scheduler.scheduleAtFixedRate(new MaintanenceTask(), 1, 5,
                TimeUnit.MINUTES);
    }

    /**
     * {@inheritDoc}
     */
    @Override
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

        } else if (DataDeliveryRegistryObjectTypes.SUBSCRIPTION
                .equals(objectType)) {

            Subscription subscription = getSubscription(id);

            if (subscription != null) {

                // Make sure the subscriptionId is set to the
                // RegistryObjectId
                subscription.setId(id);
                // Repost the Object to the BandwidthEventBus to free
                // the notification thread.
                BandwidthEventBus.publish(subscription);

            } else {
                statusHandler
                        .error("No Subscription found for id [" + id + "]");
            }
        }
    }

    private static DataSetMetaData getDataSetMetaData(String id) {
        return getRegistryObjectById(
                DataDeliveryHandlers.getDataSetMetaDataHandler(), id);
    }

    private static Subscription getSubscription(String id) {
        return getRegistryObjectById(
                DataDeliveryHandlers.getSubscriptionHandler(), id);
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
     * {@inheritDoc}
     */
    @Override
    @Subscribe
    public void updateDataSetMetaData(DataSetMetaData dataSetMetaData)
            throws ParseException {
        if (dataSetMetaData instanceof GriddedDataSetMetaData) {
            GriddedDataSetMetaData gdsmd = (GriddedDataSetMetaData) dataSetMetaData;

            // Daily/Hourly/Monthly datasets
            if (gdsmd.getCycle() == GriddedDataSetMetaData.NO_CYCLE) {
                updateDataSetMetaDataWithoutCycle(gdsmd);
            }
            // Regular cycle containing datasets
            else {
                updateDataSetMetaDataWithCycle(gdsmd);
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
        bandwidthDao.newDataSetMetaDataDao(dataSetMetaData);

        // Looking for active subscriptions to the dataset.
        try {
            List<Subscription> subscriptions = DataDeliveryHandlers
                    .getSubscriptionHandler().getActiveByDataSetAndProvider(
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

                schedule(new AdhocSubscription(sub));
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
        DataSetMetaDataDao dataset = bandwidthDao
                .newDataSetMetaDataDao(dataSetMetaData);

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
                        .debug("No Subscriptions scheduled for DataSetMetaDataDao ["
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

    private List<BandwidthAllocation> schedule(Subscription subscription,
            SortedSet<Integer> cycles) {
        List<BandwidthAllocation> unscheduled = new ArrayList<BandwidthAllocation>();
        SortedSet<Calendar> retrievalTimes = bandwidthDaoUtil
                .getRetrievalTimes(subscription, cycles);

        if (retrievalTimes.isEmpty()) {
            return unscheduled;
        }

        for (Calendar retrievalTime : retrievalTimes) {

            // Retrieve all the current subscriptions by provider, dataset name
            // and base time.
            List<SubscriptionDao> subscriptions = bandwidthDao
                    .getSubscriptions(subscription.getProvider(),
                            subscription.getDataSetName(), retrievalTime);

            statusHandler.info("schedule() - Scheduling subscription ["
                    + subscription.getName()
                    + String.format(
                            "] baseReferenceTime [%1$tY%1$tm%1$td%1$tH%1$tM",
                            retrievalTime) + "]");

            // Add the current subscription to the ones BandwidthManager already
            // knows about.
            try {
                subscriptions.add(bandwidthDao.newSubscriptionDao(subscription,
                        retrievalTime));
            } catch (SerializationException e) {
                statusHandler.error(
                        "Trapped Exception trying to schedule Subscription["
                                + subscription.getName() + "]", e);
                return null;
            }

            unscheduled.addAll(aggregate(subscriptions));
        }

        return unscheduled;
    }

    private List<BandwidthAllocation> schedule(SubscriptionDao dao) {
        Calendar retrievalTime = dao.getBaseReferenceTime();

        // Retrieve all the current subscriptions by provider, dataset name and
        // base time.
        List<SubscriptionDao> subscriptions = bandwidthDao.getSubscriptions(
                dao.getProvider(), dao.getDataSetName(), retrievalTime);

        statusHandler.info("schedule() - Scheduling subscription ["
                + dao.getName()
                + String.format(
                        "] baseReferenceTime [%1$tY%1$tm%1$td%1$tH%1$tM",
                        retrievalTime));

        return aggregate(subscriptions);
    }

    /**
     * Aggregate subscriptions for a given base time and dataset.
     * 
     * @param subscriptionDaos
     *            A List of SubscriptionDaos that have the same base time and
     *            dataset.
     */
    private List<BandwidthAllocation> aggregate(
            List<SubscriptionDao> subscriptionDaos) {

        List<SubscriptionRetrieval> retrievals = getAggregator().aggregate(
                subscriptionDaos);

        // Create a separate list of BandwidthReservations to schedule
        // as the aggregation process may return all subsumed
        // SubscriptionRetrievals
        // for the specified Subscription.
        List<BandwidthAllocation> reservations = new ArrayList<BandwidthAllocation>();

        for (SubscriptionRetrieval retrieval : retrievals) {

            // New RetrievalRequests will be marked as "PROCESSING"
            // we need to make new BandwidthReservations for these
            // SubscriptionRetrievals.

            // TODO: How to process "rescheduled" RetrievalRequests
            // in the case where subscription aggregation has determined
            // that an existing subscription has now be subsumed or
            // altered to accommodate a new super set of subscriptions...
            //
            if ((retrieval.getStatus().equals(RetrievalStatus.RESCHEDULE) || retrieval
                    .getStatus().equals(RetrievalStatus.PROCESSING))
                    && !retrieval.isSubsumed()) {

                SubscriptionDao dao = retrieval.getSubscriptionDao();
                Calendar retrievalTime = dao.getBaseReferenceTime();
                Calendar startTime = BandwidthUtil.copy(retrievalTime);

                int delayMinutes = retrieval.getDataSetAvailablityDelay();
                int maxLatency = retrieval.getSubscriptionLatency();

                if (statusHandler.isPriorityEnabled(Priority.DEBUG)) {
                    statusHandler.debug("Adding availability minutes of ["
                            + delayMinutes
                            + "] to retrieval start time of "
                            + String.format("[%1$tY%1$tm%1$td%1$tH%1$tM]",
                                    retrievalTime));
                }

                startTime.add(Calendar.MINUTE, delayMinutes);
                retrieval.setStartTime(startTime);

                Calendar endTime = TimeUtil.newCalendar();
                endTime.setTimeInMillis(startTime.getTimeInMillis());

                if (statusHandler.isPriorityEnabled(Priority.DEBUG)) {
                    statusHandler.debug("Adding latency minutes of ["
                            + maxLatency
                            + "] to start time of "
                            + String.format("[%1$tY%1$tm%1$td%1$tH%1$tM]",
                                    startTime));
                }

                endTime.add(Calendar.MINUTE, maxLatency);
                retrieval.setEndTime(endTime);

                // Check to see if the data subscribed to is available..
                // if so, mark the status of the BandwidthReservation as
                // READY.
                List<DataSetMetaDataDao> z = bandwidthDao
                        .getDataSetMetaDataDao(dao.getProvider(),
                                dao.getDataSetName(),
                                dao.getBaseReferenceTime());
                if (z.size() > 0) {
                    retrieval.setStatus(RetrievalStatus.READY);
                }
                bandwidthDao.update(retrieval);

                // Add SubscriptionRetrieval to the list to schedule..
                reservations.add(retrieval);
            }
        }

        if (reservations.isEmpty()) {
            return Collections.emptyList();
        } else {
            // Schedule the Retrievals
            return retrievalManager.schedule(reservations);
        }
    }

    /**
     * {@inheritDoc}
     */
    @Override
    @Subscribe
    @AllowConcurrentEvents
    public void subscriptionRemoved(RemoveRegistryEvent event) {
        String objectType = event.getObjectType();
        if (objectType != null) {
            if (DataDeliveryRegistryObjectTypes.SUBSCRIPTION.equals(objectType)) {
                statusHandler
                        .info("Recieved Subscription removal notification for Subscription ["
                                + event.getId() + "]");
                // Need to locate and remove all BandwidthReservations for the
                // given subscription..
                List<SubscriptionDao> l = bandwidthDao
                        .getSubscriptionDaoByRegistryId(event.getId());
                if (!l.isEmpty()) {
                    remove(l, true);
                }
            }
        }
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public List<BandwidthAllocation> schedule(Subscription subscription) {
        SortedSet<Integer> cycles = new TreeSet<Integer>(subscription.getTime()
                .getCycleTimes());
        List<BandwidthAllocation> unscheduled = schedule(subscription, cycles);

        return unscheduled;
    }

    /**
     * {@inheritDoc}
     * 
     * @return
     */
    @Override
    public List<BandwidthAllocation> schedule(AdhocSubscription subscription) {

        List<SubscriptionDao> subscriptions = new ArrayList<SubscriptionDao>();
        Calendar now = BandwidthUtil.now();
        // Store the AdhocSubscription with a base time of now..
        try {
            subscriptions.add(bandwidthDao
                    .newSubscriptionDao(subscription, now));
        } catch (SerializationException e) {
            statusHandler.error(
                    "Trapped Exception trying to schedule AdhocSubscription["
                            + subscription.getName() + "]", e);
            return Collections.emptyList();
        }

        // Check start time in Time, if it is blank, we need to add the most
        // recent MetaData for the DataSet subscribed to.
        final Time subTime = subscription.getTime();
        if (subTime.getStart() == null) {
            subscription = bandwidthDaoUtil.setAdhocMostRecentUrlAndTime(
                    subscription, true);
        }

        // Use SimpleSubscriptionAggregator (i.e. no aggregation) to generate a
        // SubscriptionRetrieval for this AdhocSubscription
        SimpleSubscriptionAggregator a = new SimpleSubscriptionAggregator(
                bandwidthDao);
        List<BandwidthAllocation> reservations = new ArrayList<BandwidthAllocation>();
        List<SubscriptionRetrieval> retrievals = a.aggregate(subscriptions);

        for (SubscriptionRetrieval retrieval : retrievals) {
            retrieval.setStartTime(now);
            Calendar endTime = BandwidthUtil.copy(now);
            endTime.add(Calendar.MINUTE, retrieval.getSubscriptionLatency());
            retrieval.setEndTime(endTime);
            // Store the SubscriptionRetrieval - retrievalManager expects
            // the BandwidthAllocations to already be stored.
            bandwidthDao.update(retrieval);
            reservations.add(retrieval);
        }

        List<BandwidthAllocation> unscheduled = retrievalManager
                .schedule(reservations);

        // Now iterate the SubscriptionRetrievals and for the
        // Allocations that were scheduled, set the status to READY
        // and notify the retrievalManager.
        for (SubscriptionRetrieval retrieval : retrievals) {

            if (retrieval.getStatus().equals(RetrievalStatus.SCHEDULED)) {
                retrieval.setStatus(RetrievalStatus.READY);
                bandwidthDaoUtil.update(retrieval);
            }
        }

        retrievalManager.wakeAgents();

        return unscheduled;
    }

    /**
     * {@inheritDoc}
     * 
     * @return
     */
    @Override
    public List<BandwidthAllocation> subscriptionUpdated(
            Subscription subscription) throws SerializationException {
        // Since AdhocSubscription extends Subscription it is not possible to
        // separate the processing of those Objects in EventBus. So, handle the
        // case where the updated subscription is actually an AdhocSubscription
        if (subscription instanceof AdhocSubscription) {
            return adhocSubscription((AdhocSubscription) subscription);
        }
        // Dealing with a 'normal' subscription
        else {
            // First see if BandwidthManager has seen the subscription before.
            List<SubscriptionDao> subscriptionDaos = bandwidthDao
                    .getSubscriptionDao(subscription);

            // If BandwidthManager does not know about the subscription, and
            // it's active, attempt to add it..
            if (subscriptionDaos.isEmpty() && subscription.isActive()) {
                final boolean subscribedToCycles = !subscription.getTime()
                        .getCycleTimes().isEmpty();
                final boolean useMostRecentDataSetUpdate = !subscribedToCycles;

                // The subscription has cycles, so we can allocate bandwidth at
                // expected times
                List<BandwidthAllocation> unscheduled = Collections.emptyList();
                if (subscribedToCycles) {
                    unscheduled = schedule(subscription);
                }

                // Create an adhoc subscription based on the new subscription,
                // and set it to retrieve the most recent cycle (or most recent
                // url if a daily product)
                AdhocSubscription adhoc = new AdhocSubscription(subscription);
                adhoc = bandwidthDaoUtil.setAdhocMostRecentUrlAndTime(adhoc,
                        useMostRecentDataSetUpdate);

                if (adhoc == null) {
                    statusHandler
                            .info(String
                                    .format("There wasn't applicable most recent dataset metadata to use for new subscription [%s].  "
                                            + "No adhoc requested.",
                                            subscription.getName()));
                } else {
                    unscheduled = schedule(adhoc);
                }
                return unscheduled;
            } else if (!subscription.isActive()) {
                // See if the subscription was inactivated..
                // Need to remove BandwidthReservations for this
                // subscription.
                return remove(subscriptionDaos, true);
            } else {

                // Compare the 'updated' Subscription with the stored
                // SubscriptionDaos to determine
                // if the changes made to the Subscription would affect
                // BandwidthReservations
                // already in place for this subscription.

                Subscription old = subscriptionDaos.get(0).getSubscription();

                // Check to see if estimated size changed. If there was a change
                // to
                // which parameters or levels or coverage or forecast hours,
                // those
                // don't effect BandwidthReservations so there is no need to
                // change the
                // RetrievalPlan as long as the size stays the same

                if (subscriptionRequiresReschedule(subscription, old)) {

                    // OK, have to remove the old Subscriptions and add the new
                    // ones..
                    List<BandwidthAllocation> unscheduled = remove(
                            subscriptionDaos, false);
                    // No need to check anything else since all the
                    // SubscriptionDao's have been replaced.
                    unscheduled.addAll(schedule(subscription));
                    return unscheduled;
                }

                List<BandwidthAllocation> unscheduled = new ArrayList<BandwidthAllocation>();
                // Check that the cycles in both subscriptions are the same
                SortedSet<Integer> newCycles = new TreeSet<Integer>(
                        subscription.getTime().getCycleTimes());
                SortedSet<Integer> oldCycles = new TreeSet<Integer>(old
                        .getTime().getCycleTimes());

                if (newCycles.size() != oldCycles.size()
                        || !newCycles.containsAll(oldCycles)
                        || !oldCycles.containsAll(newCycles)) {
                    // Cycle times have changed, reschedule.

                    // Create a Set of the common elements..
                    Set<Integer> commonCycles = Sets
                            .union(oldCycles, newCycles);

                    // Remove the common elements from the old cycles, these
                    // need to be removed from the RetrievalPlan..
                    oldCycles.removeAll(commonCycles);

                    // Remove the common elements from the new cycles, these
                    // need to be added to the RetrievalPlan..
                    newCycles.removeAll(commonCycles);

                    // Remove the old cycles, add the new ones...
                    if (oldCycles.size() > 0) {
                        // Create a List of SubscriptionDaos that need to be
                        // removed..
                        List<SubscriptionDao> remove = new ArrayList<SubscriptionDao>();
                        SubscriptionDao dao = null;
                        Iterator<SubscriptionDao> itr = subscriptionDaos
                                .iterator();
                        while (itr.hasNext()) {
                            dao = itr.next();
                            if (oldCycles.contains(dao.getCycle())) {
                                remove.add(dao);
                                itr.remove();
                            }
                        }
                        unscheduled.addAll(remove(remove, true));
                    }

                    if (newCycles.size() > 0) {
                        unscheduled.addAll(schedule(subscription, newCycles));
                    }
                }

                // Update the remaining dao's with the current subscription...
                for (SubscriptionDao dao : subscriptionDaos) {
                    dao.setSubscription(subscription);
                    bandwidthDao.update(dao);
                }

                return unscheduled;
            }
        }
    }

    /**
     * Determines when a subscription requires rescheduling.
     * 
     * @param subscription
     *            the new subscription
     * @param old
     *            the old version
     * @return true if it requires rescheduling
     */
    protected boolean subscriptionRequiresReschedule(Subscription subscription,
            Subscription old) {
        // TODO: Do they have to match EXACTLY... probably not but how
        // much is 'close enough' in bandwidth terms.
        boolean requiresReschedule = (old.getDataSetSize() != subscription
                .getDataSetSize())
        // Priority is different
                || (!old.getPriority().equals(subscription.getPriority()))
                // Latency is different
                || (!(old.getLatencyInMinutes() == subscription
                        .getLatencyInMinutes()));
        return requiresReschedule;
    }

    /**
     * {@inheritDoc}
     * 
     * @return
     */
    @Override
    public List<BandwidthAllocation> adhocSubscription(AdhocSubscription adhoc) {
        statusHandler.info("Scheduling adhoc subscription [" + adhoc.getName()
                + "]");
        return schedule(adhoc);
    }

    /**
     * Remove SubscriptionDao's (and dependent Objects) from any RetrievalPlans
     * they are in and adjust the RetrievalPlans accordingly.
     * 
     * @param subscriptionDaos
     *            The subscriptionDao's to remove.
     * @param reschedule
     * @return
     */
    private List<BandwidthAllocation> remove(
            List<SubscriptionDao> subscriptionDaos, boolean reschedule) {

        List<BandwidthAllocation> unscheduled = new ArrayList<BandwidthAllocation>();

        // If we need to reschedule other bandwidth reservations when we
        // remove the provided SubscriptionDao's then we have to retrieve
        // all the SubscriptionRetrieval records that are scheduled for
        // the same base time.
        if (reschedule) {

            // First create a map of base times to subscriptions
            Multimap<Calendar, SubscriptionDao> map = ArrayListMultimap
                    .create();

            for (SubscriptionDao subscriptionDao : subscriptionDaos) {

                Calendar time = subscriptionDao.getBaseReferenceTime();
                map.put(time, subscriptionDao);
            }

            // Now process each time group by dataset..
            for (Calendar baseTime : map.keySet()) {

                // For each date, get a unique set of provider & dataset name
                Set<String> providerDataSet = new HashSet<String>();
                for (SubscriptionDao dao : map.get(baseTime)) {
                    String key = dao.getProvider() + "::"
                            + dao.getDataSetName();
                    providerDataSet.add(key);
                    bandwidthDaoUtil.remove(dao);
                }

                // Query for and reschedule any SubscriptionRetrieval
                // Objects associated with the Queried SubscriptionDao's
                for (String providerDataSetName : providerDataSet) {
                    String[] key = providerDataSetName.split("::");
                    String provider = key[0];
                    String dataSetName = key[1];
                    List<SubscriptionDao> m = bandwidthDao.getSubscriptions(
                            provider, dataSetName, baseTime);

                    unscheduled.addAll(aggregate(m));
                }
            }

        } else {

            for (SubscriptionDao subscriptionDao : subscriptionDaos) {
                bandwidthDaoUtil.remove(subscriptionDao);
            }
        }

        return unscheduled;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    @Subscribe
    public void subscriptionFulfilled(
            SubscriptionRetrievalFulfilled subscriptionRetrievalFulfilled) {

        statusHandler.info("subscriptionFullfilled() :: "
                + subscriptionRetrievalFulfilled);

        SubscriptionRetrieval sr = subscriptionRetrievalFulfilled
                .getSubscriptionRetrieval();

        List<SubscriptionRetrieval> subscriptionRetrievals = bandwidthDao
                .querySubscriptionRetrievals(sr.getSubscriptionDao());

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
            SubscriptionDao dao = sr.getSubscriptionDao();
            Subscription subscription = null;
            try {
                subscription = dao.getSubscription();
            } catch (SerializationException e) {
                statusHandler.error(
                        "Failed to extract Subscription from SubscriptionDao ["
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
                // completed SubscriptionDao to get the next days retrieval.

                // Now check if that SubscriptionDao has already been scheduled.
                SubscriptionDao a = bandwidthDao.getSubscriptionDao(
                        dao.getRegistryId(), next);
                if (a == null) {
                    // Create the new SubscriptionDao record with the next
                    // time..
                    try {
                        a = bandwidthDao.newSubscriptionDao(subscription, next);
                    } catch (SerializationException e) {

                        statusHandler.error(
                                "Failed to create new SubscriptionDao from Subscription ["
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
     * {@inheritDoc}
     */
    @Override
    public void setAggregator(ISubscriptionAggregator aggregator) {
        this.aggregator = aggregator;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public ISubscriptionAggregator getAggregator() {
        return aggregator;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public Object handleRequest(IBandwidthRequest request) throws Exception {

        ITimer timer = TimeUtil.getTimer();
        timer.start();

        Object response = null;

        final Network requestNetwork = request.getNetwork();
        final int bandwidth = request.getBandwidth();

        final List<Subscription> subscriptions = request.getSubscriptions();
        final RequestType requestType = request.getRequestType();
        switch (requestType) {
        case GET_ESTIMATED_COMPLETION:
            Subscription adhocAsSub = null;
            if (subscriptions.size() != 1
                    || (!((adhocAsSub = subscriptions.get(0)) instanceof AdhocSubscription))) {
                throw new IllegalArgumentException(
                        "Must supply one, and only one, adhoc subscription to get the estimated completion time.");
            }
            response = getEstimatedCompletionTime((AdhocSubscription) adhocAsSub);
            break;
        case REINITIALIZE:
            response = startNewBandwidthManager();
            break;
        case RETRIEVAL_PLAN:
            response = showRetrievalPlan(requestNetwork);
            break;
        case PROPOSE_SCHEDULE_SUBSCRIPTION:
            final ProposeScheduleResponse proposeResponse = proposeSchedule(subscriptions);
            response = proposeResponse;
            Set<String> subscriptionsUnscheduled = proposeResponse
                    .getUnscheduledSubscriptions();
            if (subscriptionsUnscheduled.isEmpty()) {
                statusHandler
                        .info("No subscriptions will be unscheduled by scheduling subscriptions "
                                + subscriptions + ".  Applying...");
                // This is a safe operation as all subscriptions will remain
                // scheduled, just apply
                scheduleSubscriptions(subscriptions);
            } else if (subscriptions.size() == 1) {
                int requiredLatency = determineRequiredLatency(subscriptions
                        .get(0));
                proposeResponse.setRequiredLatency(requiredLatency);
            }
            break;
        case SCHEDULE_SUBSCRIPTION:
            response = scheduleSubscriptions(subscriptions);
            break;
        case GET_BANDWIDTH:
            RetrievalPlan b = retrievalManager.getPlan(requestNetwork);
            if (b != null) {
                response = b.getDefaultBandwidth();
            } else {
                response = null;
            }
            break;
        case PROPOSE_SET_BANDWIDTH:
            Set<Subscription> unscheduledSubscriptions = proposeSetBandwidth(
                    requestNetwork, bandwidth);
            response = unscheduledSubscriptions;
            if (unscheduledSubscriptions.isEmpty()) {
                statusHandler
                        .info("No subscriptions will be unscheduled by changing the bandwidth for network ["
                                + requestNetwork
                                + "] to ["
                                + bandwidth
                                + "].  Applying...");
                // This is a safe operation as all subscriptions will remain
                // scheduled, just apply
                setBandwidth(requestNetwork, bandwidth);
            }

            break;
        case FORCE_SET_BANDWIDTH:
            boolean setBandwidth = setBandwidth(requestNetwork, bandwidth);
            response = setBandwidth;
            break;
        case METADATA_UPDATE:
            DataSetMetaData r = request.getDataSetMetaData();
            updateDataSetMetaData(r);
            break;

        case SHOW_ALLOCATION:

            break;

        case SHOW_BUCKET:

            long bucketId = request.getId();
            RetrievalPlan plan = retrievalManager.getPlan(requestNetwork);
            BandwidthBucket bucket = plan.getBucket(bucketId);
            response = bucket.showReservations();
            break;

        case SHOW_DEFERRED:
            StringBuilder sb = new StringBuilder();
            List<BandwidthAllocation> z = bandwidthDao.getDeferred(
                    requestNetwork, request.getBegin());
            for (BandwidthAllocation allocation : z) {
                sb.append(allocation).append("\n");
            }
            response = sb.toString();
            break;
        case GET_BANDWIDTH_GRAPH_DATA:
            response = getBandwidthGraphData();
            break;
        default:
            throw new IllegalArgumentException(
                    "Dont know how to handle request type [" + requestType
                            + "]");
        }

        // Clean up any in-memory bandwidth configuration files
        InMemoryBandwidthContextFactory.deleteInMemoryBandwidthConfigFile();

        timer.stop();

        statusHandler.info("Processed request of type [" + requestType
                + "] in [" + timer.getElapsedTime() + "] ms");

        return response;
    }

    /**
     * Retrieve the bandwidth graph data.
     * 
     * @return the graph data
     */
    private BandwidthGraphData getBandwidthGraphData() {
        return new BandwidthGraphDataAdapter(retrievalManager)
                .get();
    }

    /**
     * Get the estimated completion time for an adhoc subscription.
     * 
     * @param subscription
     *            the subscription
     * @return the estimated completion time
     */
    private Date getEstimatedCompletionTime(AdhocSubscription subscription) {
        final List<SubscriptionDao> subscriptionDaos = bandwidthDao
                .getSubscriptionDaoByRegistryId(subscription.getId());

        if (subscriptionDaos.isEmpty()) {
            statusHandler
                    .warn("Unable to find subscriptionDaos for subscription ["
                            + subscription + "].  Returning current time.");
            return new Date();
        } else if (subscriptionDaos.size() > 1) {
            statusHandler
                    .warn("Found more than one subscriptionDaos for subscription ["
                            + subscription
                            + "].  Ignoring list and using the first item.");
        }

        SubscriptionDao dao = subscriptionDaos.get(0);
        long id = dao.getId();

        final List<BandwidthAllocation> bandwidthAllocations = bandwidthDao
                .getBandwidthAllocations(id);

        if (bandwidthAllocations.isEmpty()) {
            statusHandler
                    .warn("Unable to find bandwidthAllocations for subscription ["
                            + subscription + "].  Returning current time.");
            return new Date();
        }

        Calendar latest = null;
        for (BandwidthAllocation allocation : bandwidthAllocations) {
            final Calendar endTime = allocation.getEndTime();
            if (latest == null || endTime.after(latest)) {
                latest = endTime;
            }
        }

        return latest.getTime();
    }

    /**
     * Schedule the list of subscriptions.
     * 
     * @param subscriptions
     *            the subscriptions
     * @return the set of subscription names unscheduled
     * @throws SerializationException
     */
    private Set<String> scheduleSubscriptions(List<Subscription> subscriptions)
            throws SerializationException {
        Set<String> unscheduledSubscriptions = new TreeSet<String>();

        Set<BandwidthAllocation> unscheduledAllocations = new HashSet<BandwidthAllocation>();

        for (Subscription subscription : subscriptions) {
            unscheduledAllocations.addAll(subscriptionUpdated(subscription));
        }

        for (BandwidthAllocation allocation : unscheduledAllocations) {
            if (allocation instanceof SubscriptionRetrieval) {
                SubscriptionRetrieval retrieval = (SubscriptionRetrieval) allocation;
                unscheduledSubscriptions.add(retrieval.getSubscription()
                        .getName());
            }
        }
        return unscheduledSubscriptions;
    }

    /**
     * Sets the bandwidth for a network to the specified value.
     * 
     * @param requestNetwork
     *            the network
     * @param bandwidth
     *            the bandwidth
     * @return true on success, false otherwise
     * @throws SerializationException
     *             on error serializing
     */
    private boolean setBandwidth(Network requestNetwork, int bandwidth)
            throws SerializationException {
        RetrievalPlan c = retrievalManager.getPlan(requestNetwork);
        if (c != null) {
            c.setDefaultBandwidth(bandwidth);

            return startNewBandwidthManager();
        }
        return false;
    }

    /**
     * Propose scheduling the subscriptions.
     * 
     * @param subscriptions
     *            the subscriptions
     * @return the response
     * @throws SerializationException
     */
    private ProposeScheduleResponse proposeSchedule(
            List<Subscription> subscriptions) throws SerializationException {
        BandwidthMap copyOfCurrentMap = BandwidthMap
                .load(EdexBandwidthContextFactory.getBandwidthMapConfig());

        BandwidthManager proposedBwManager = startProposedBandwidthManager(copyOfCurrentMap);

        IBandwidthRequest request = new IBandwidthRequest();
        request.setRequestType(RequestType.SCHEDULE_SUBSCRIPTION);
        request.setSubscriptions(subscriptions);

        final Set<String> unscheduled = proposedBwManager
                .scheduleSubscriptions(subscriptions);

        proposedBwManager.shutdown();

        final ProposeScheduleResponse proposeScheduleResponse = new ProposeScheduleResponse();
        proposeScheduleResponse.setUnscheduledSubscriptions(unscheduled);

        return proposeScheduleResponse;
    }

    /**
     * Propose changing a route's bandwidth to the specified amount.
     * 
     * @return the subscriptions that would be unscheduled after setting the
     *         bandwidth
     * 
     * @throws SerializationException
     */
    private Set<Subscription> proposeSetBandwidth(Network requestNetwork,
            int bandwidth) throws SerializationException {
        BandwidthMap copyOfCurrentMap = BandwidthMap
                .load(EdexBandwidthContextFactory.getBandwidthMapConfig());
        BandwidthRoute route = copyOfCurrentMap.getRoute(requestNetwork);
        route.setDefaultBandwidth(bandwidth);

        BandwidthManager proposedBwManager = startProposedBandwidthManager(copyOfCurrentMap);

        if (statusHandler.isPriorityEnabled(Priority.DEBUG)) {
            statusHandler.debug("Current retrieval plan:" + Util.EOL
                    + showRetrievalPlan(requestNetwork) + Util.EOL
                    + "Proposed retrieval plan:" + Util.EOL
                    + proposedBwManager.showRetrievalPlan(requestNetwork));
        }

        List<BandwidthAllocation> unscheduledAllocations = proposedBwManager.bandwidthDao
                .getBandwidthAllocationsInState(RetrievalStatus.UNSCHEDULED);

        if (!unscheduledAllocations.isEmpty()
                && statusHandler.isPriorityEnabled(Priority.DEBUG)) {
            LogUtil.logIterable(
                    statusHandler,
                    Priority.DEBUG,
                    "The following unscheduled allocations would occur with the proposed bandwidth:",
                    unscheduledAllocations);
        }

        Set<Subscription> subscriptions = new HashSet<Subscription>();
        for (BandwidthAllocation allocation : unscheduledAllocations) {
            if (allocation instanceof SubscriptionRetrieval) {
                subscriptions.add(((SubscriptionRetrieval) allocation)
                        .getSubscription());
            }
        }

        if (!subscriptions.isEmpty()
                && statusHandler.isPriorityEnabled(Priority.INFO)) {
            LogUtil.logIterable(
                    statusHandler,
                    Priority.INFO,
                    "The following subscriptions would not be scheduled with the proposed bandwidth:",
                    subscriptions);
        }

        proposedBwManager.shutdown();

        return subscriptions;
    }

    /**
     * Starts the proposed bandwidth manager and returns the reference to it.
     * 
     * @param bandwidthMap
     * 
     * @return the proposed bandwidth manager
     * @throws SerializationException
     */
    @VisibleForTesting
    BandwidthManager startProposedBandwidthManager(BandwidthMap bandwidthMap) {

        InMemoryBandwidthContextFactory
                .setInMemoryBandwidthConfigFile(bandwidthMap);

        return startBandwidthManager(
                InMemoryBandwidthManager.IN_MEMORY_BANDWIDTH_MANAGER_FILES,
                true, "memory");
    }

    /**
     * Starts the new bandwidth manager.
     * 
     * @return true if the new bandwidth manager was started
     */
    private boolean startNewBandwidthManager() {
        BandwidthManager bandwidthManager = startBandwidthManager(
                getSpringFilesForNewInstance(), false, "EDEX");

        final boolean successfullyStarted = bandwidthManager != null;
        if (successfullyStarted) {
            this.shutdown();
        } else {
            statusHandler
                    .error("The new BandwidthManager reference was null, please check the log for errors.");
        }
        return successfullyStarted;
    }

    /**
     * Starts a {@link BandwidthManager} and returns a reference to it.
     * 
     * @param springFiles
     *            the spring files to use
     * @param type
     * @return the reference to the bandwidth manager
     */
    private BandwidthManager startBandwidthManager(final String[] springFiles,
            boolean close, String type) {
        ITimer timer = TimeUtil.getTimer();
        timer.start();

        ClassPathXmlApplicationContext ctx = null;
        try {
            ctx = new ClassPathXmlApplicationContext(springFiles,
                    EDEXUtil.getSpringContext());
            return (BandwidthManager) ctx.getBean("bandwidthManager",
                    BandwidthManager.class);
        } finally {
            if (close) {
                Util.close(ctx);
            }

            timer.stop();
            statusHandler.info("Took [" + timer.getElapsedTime()
                    + "] ms to start a new bandwidth manager of type [" + type
                    + "]");
        }
    }

    /**
     * Return the display of the retrieval plan for the network.
     * 
     * @param network
     *            the network
     * @return the plan
     */
    private String showRetrievalPlan(Network network) {
        RetrievalPlan a = retrievalManager.getPlan(network);
        return (a != null) ? a.showPlan() : "";
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void init() {
        initializer.init(this, dbInit);
    }

    /**
     * Private inner work thread used to keep the RetrievalPlans up to date.
     * 
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

    /**
     * {@inheritDoc}
     */
    @Override
    public void setInitializer(BandwidthInitializer initializer) {
        this.initializer = initializer;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public BandwidthInitializer getInitializer() {
        return initializer;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public AuthorizationResponse authorized(IUser user,
            IBandwidthRequest request) throws AuthorizationException {
        return new AuthorizationResponse(true);
    }

    /**
     * {@inheritDoc}
     */
    public List<BandwidthAllocation> copyState(BandwidthManager copyFrom) {
        IBandwidthDao fromDao = copyFrom.bandwidthDao;

        Set<Subscription> actualSubscriptions = new HashSet<Subscription>();
        for (SubscriptionDao subscription : fromDao.getSubscriptions()) {
            try {
                Subscription actualSubscription = subscription
                        .getSubscription();
                actualSubscriptions.add(actualSubscription);
            } catch (SerializationException e) {
                statusHandler
                        .handle(Priority.PROBLEM,
                                "Unable to deserialize a subscription, results may not be accurate for modeling bandwidth changes.",
                                e);
            }
        }

        // Now for each subscription, attempt to schedule bandwidth
        List<BandwidthAllocation> unscheduled = new ArrayList<BandwidthAllocation>();
        for (Subscription subscription : actualSubscriptions) {
            unscheduled.addAll(this.schedule(subscription));
        }

        return unscheduled;
    }

    /**
     * Performs shutdown necessary for the {@link BandwidthManager} instance.
     */
    @VisibleForTesting
    void shutdown() {
        EventBus.getInstance().unregister(this);
        BandwidthEventBus.unregister(this);
        retrievalManager.shutdown();
        scheduler.shutdownNow();
    }

    /**
     * Get the Spring files used to create a new instance of this
     * {@link BandwidthManager} type.
     * 
     * @return the Spring files
     */
    protected abstract String[] getSpringFilesForNewInstance();

    /**
     * Determine the latency that would be required on the subscription for it
     * to be fully scheduled.
     * 
     * @param subscription
     *            the subscription
     * @return the required latency, in minutes
     */
    @VisibleForTesting
    int determineRequiredLatency(Subscription subscription) {
        ITimer timer = TimeUtil.getTimer();
        timer.start();
        try {
            final Subscription clone = BandwidthUtil.cheapClone(
                    Subscription.class, subscription);

            if (clone.getLatencyInMinutes() < 1) {
                clone.setLatencyInMinutes(1);
            }

            boolean foundLatency = false;
            int latency = clone.getLatencyInMinutes();
            int previousLatency = latency;
            do {
                // Double the latency until we have two values we can binary
                // search between...
                previousLatency = latency;
                latency *= 2;
                clone.setLatencyInMinutes(latency);
                foundLatency = isSchedulableWithoutConflict(clone);
            } while (!foundLatency);

            SortedSet<Integer> possibleLatencies = new TreeSet<Integer>();
            for (int i = previousLatency; i < (latency + 1); i++) {
                possibleLatencies.add(Integer.valueOf(i));
            }

            IBinarySearchResponse<Integer> response = AlgorithmUtil
                    .binarySearch(possibleLatencies, new Comparable<Integer>() {
                        @Override
                        public int compareTo(Integer valueToCheck) {
                            clone.setLatencyInMinutes(valueToCheck);

                            boolean latencyWouldWork = isSchedulableWithoutConflict(clone);

                            // Check if one value less would not work, if so
                            // then this is the required latency, otherwise keep
                            // searching
                            if (latencyWouldWork) {
                                clone.setLatencyInMinutes(clone
                                        .getLatencyInMinutes() - 1);

                                return (isSchedulableWithoutConflict(clone)) ? 1
                                        : 0;
                            } else {
                                // Still too low, stuff would be unscheduled
                                return -1;
                            }
                        }
                    });

            final Integer binarySearchedLatency = response.getItem();
            if (binarySearchedLatency != null) {
                latency = binarySearchedLatency.intValue();

                if (statusHandler.isPriorityEnabled(Priority.DEBUG)) {
                    statusHandler
                            .debug(String
                                    .format("Found required latency of [%s] in [%s] iterations",
                                            binarySearchedLatency,
                                            response.getIterations()));
                }
            } else {
                statusHandler
                        .warn(String
                                .format("Unable to find the required latency with a binary search, using required latency [%s]",
                                        latency));
            }

            timer.stop();

            int bufferRoomInMinutes = retrievalManager.getPlan(
                    subscription.getRoute()).getBucketMinutes();

            final String logMsg = String
                    .format("Determined required latency of [%s] in [%s] ms.  Adding buffer room of [%s] minutes",
                            latency, timer.getElapsedTime(),
                            bufferRoomInMinutes);
            statusHandler.info(logMsg);

            latency += bufferRoomInMinutes;

            return latency;
        } catch (SerializationException e) {
            statusHandler.handle(Priority.PROBLEM,
                    "Unable to serialize a Subscription", e);
            return -1;
        }
    }

    /**
     * Checks whether the subscription, as defined, would be schedulable without
     * conflicting with the current bandwidth or any other subscriptions.
     * 
     * @param subscription
     *            the subscription
     * @return true if able to be cleanly scheduled, false otherwise
     */
    private boolean isSchedulableWithoutConflict(final Subscription subscription) {
        BandwidthMap copyOfCurrentMap = BandwidthMap
                .load(EdexBandwidthContextFactory.getBandwidthMapConfig());

        BandwidthManager proposedBandwidthManager = startProposedBandwidthManager(copyOfCurrentMap);
        try {
            Set<String> unscheduled = proposedBandwidthManager
                    .scheduleSubscriptions(Arrays.asList(subscription));
            proposedBandwidthManager.shutdown();
            return unscheduled.isEmpty();
        } catch (SerializationException e) {
            statusHandler
                    .handle(Priority.PROBLEM,
                            "Serialization error while determining required latency.  Returning true in order to be fault tolerant.",
                            e);
            return true;
        }
    }
}
