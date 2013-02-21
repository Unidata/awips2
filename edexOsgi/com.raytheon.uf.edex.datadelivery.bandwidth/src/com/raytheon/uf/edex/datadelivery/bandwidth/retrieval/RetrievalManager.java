package com.raytheon.uf.edex.datadelivery.bandwidth.retrieval;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.TreeMap;

import com.google.common.eventbus.Subscribe;
import com.raytheon.uf.common.datadelivery.registry.Network;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.time.util.TimeUtil;
import com.raytheon.uf.edex.datadelivery.bandwidth.dao.BandwidthAllocation;
import com.raytheon.uf.edex.datadelivery.bandwidth.dao.IBandwidthDao;
import com.raytheon.uf.edex.datadelivery.bandwidth.dao.SubscriptionRetrieval;
import com.raytheon.uf.edex.datadelivery.bandwidth.notification.BandwidthEventBus;
import com.raytheon.uf.edex.datadelivery.retrieval.RetrievalManagerNotifyEvent;
import com.raytheon.uf.edex.event.EventBus;

/**
 * 
 * Retrieval manager.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Oct 11, 2012 0726       djohnson     Add SW history, check for bandwidth enabled,
 *                                      change the event listener type.
 * Oct 26, 2012 1286       djohnson     Return list of unscheduled allocations.
 * 
 * </pre>
 * 
 * @author djohnson
 * @version 1.0
 */
public class RetrievalManager {

    private static final IUFStatusHandler statusHandler = UFStatus
            .getHandler(RetrievalManager.class);

    // Package-private on purpose so agents have visibility
    static final BandwidthAllocation POISON_PILL = new BandwidthAllocation();

    private final IBandwidthDao bandwidthDao;

    // A Map of the Paths to retrievalPlans
    private Map<Network, RetrievalPlan> retrievalPlans = Collections
            .synchronizedSortedMap(new TreeMap<Network, RetrievalPlan>());

    private final Object notifier;

    private volatile boolean shutdown;

    public RetrievalManager(IBandwidthDao bandwidthDao, Object notifier) {
        this.bandwidthDao = bandwidthDao;
        this.notifier = notifier;

        EventBus.getInstance().register(this);
    }

    public Map<Network, RetrievalPlan> getRetrievalPlans() {
        return retrievalPlans;
    }

    public void setRetrievalPlans(Map<Network, RetrievalPlan> retrievalPlans) {
        this.retrievalPlans = retrievalPlans;
    }

    /**
     * Schedule the allocations.
     * 
     * @param bandwidthAllocations
     *            The BandwidthAllocations to schedule.
     * @return the list of {@link BandwidthAllocation}s that were unable to be
     *         scheduled
     */
    public List<BandwidthAllocation> schedule(
            List<BandwidthAllocation> bandwidthAllocations) {
        List<BandwidthAllocation> unscheduled = new ArrayList<BandwidthAllocation>();

        for (BandwidthAllocation bandwidthAllocation : bandwidthAllocations) {
            Network network = bandwidthAllocation.getNetwork();
            RetrievalPlan plan = getRetrievalPlans().get(network);

            if (plan != null) {

                if (bandwidthAllocation.getStartTime().before(
                        plan.getPlanStart())
                        || bandwidthAllocation.getEndTime().after(
                                plan.getPlanEnd())) {

                    statusHandler
                            .warn("Attempt to schedule bandwidth outside current window. BandwidthAllocation ["
                                    + bandwidthAllocation.getIdentifier()
                                    + "].  The BandwidthAllocation will be deferred.");
                    bandwidthAllocation.setStatus(RetrievalStatus.DEFERRED);
                    bandwidthDao.update(bandwidthAllocation);
                } else {

                    synchronized (plan) {
                        unscheduled.addAll(plan.schedule(bandwidthAllocation));
                        bandwidthDao.update(bandwidthAllocation);
                    }
                }
            } else {
                throw new IllegalArgumentException(String.format(
                        "There is no configuration for network [%s]", network));
            }
        }

        // Update any unscheduled allocations
        for (BandwidthAllocation allocation : unscheduled) {
            allocation.setStatus(RetrievalStatus.UNSCHEDULED);
            bandwidthDao.update(allocation);
        }

        return unscheduled;
    }

    @Subscribe
    public void retrievalCompleted(RetrievalManagerNotifyEvent event) {

        long eventId = Long.parseLong(event.getId());

        if (eventId == -1) {
            statusHandler
                    .error("Unable to mark retrieval complete, received -1 SubscriptionRetrieval id!");
            return;
        }

        SubscriptionRetrieval subscriptionRetrieval = bandwidthDao
                .getSubscriptionRetrieval(eventId);

        // Update the SubscriptionRetrieval in the database since the Retrievals
        // were completed outside the Bandwidth subsystem.
        subscriptionRetrieval.setStatus(RetrievalStatus.FULFILLED);
        subscriptionRetrieval.setActualEnd(TimeUtil.newCalendar());

        bandwidthDao.update(subscriptionRetrieval);

        statusHandler.info("RetrievalComplete:: ["
                + subscriptionRetrieval.getId() + "]");

        // Determine if all the retrievals for the SubscriptionRetrieval are
        // fulfilled.
        List<SubscriptionRetrieval> subscriptionRetrievals = bandwidthDao
                .querySubscriptionRetrievals(subscriptionRetrieval
                        .getSubscriptionDao().getId());

        boolean completed = true;
        // If there is more than one check them all..
        for (SubscriptionRetrieval retrieval : subscriptionRetrievals) {
            completed &= retrieval.getStatus()
                    .equals(RetrievalStatus.FULFILLED);
        }

        if (completed) {
            subscriptionRetrieval.setStatus(RetrievalStatus.FULFILLED);
            bandwidthDao.update(subscriptionRetrieval);
            BandwidthEventBus.publish(new SubscriptionRetrievalFulfilled(
                    subscriptionRetrieval));
        }

    }

    public BandwidthAllocation nextAllocation(Network network, String agentType) {
        if (shutdown) {
            return POISON_PILL;
        }

        RetrievalPlan plan = getRetrievalPlans().get(network);
        if (plan != null) {
            synchronized (plan) {
                return plan.nextAllocation(agentType);
            }
        }
        return null;
    }

    public final RetrievalPlan getPlan(Network network) {
        return getRetrievalPlans().get(network);
    }

    public void remove(BandwidthAllocation allocation) {
        RetrievalPlan plan = getRetrievalPlans().get(allocation.getNetwork());
        if (plan != null) {
            synchronized (plan) {
                plan.remove(allocation);
            }
        }
    }

    public void updateBandwidthAllocation(BandwidthAllocation allocation) {
        RetrievalPlan plan = getRetrievalPlans().get(allocation.getNetwork());
        if (plan != null) {
            synchronized (plan) {
                plan.updateBandwidthReservation(allocation);
            }
        }
    }

    /**
     * Shutdown the retrieval manager.
     */
    public void shutdown() {
        EventBus.getInstance().unregister(this);
        // From this point forward, only return a poison pill for this retrieval
        // manager, which will cause threads attempting to receive bandwidth
        // allocations to die
        this.shutdown = true;
    }

    /**
     * Wake up the AgentManager for the RetrievalManager.
     */
    public void wakeAgents() {
        // This is currently a duplication of wake() in RetrievalAgentManager,
        // because there was a circular dependency in the Spring config files...
        // Can the object graph be made a little cleaner?
        synchronized (notifier) {
            statusHandler.info("Waking up retrieval threads");
            notifier.notifyAll();
        }
    }
}
