package com.raytheon.uf.edex.datadelivery.bandwidth.retrieval;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
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
 * Feb 05, 2013 1580       mpduff       EventBus refactor.
 * Feb 14, 2013 1596       djohnson     Warn log when unable to find a SubscriptionRetrieval.
 * 3/18/2013    1802       bphillip     Event bus registration is now a post-construct operation to ensure proxy is registered with bus
 * 3/13/2013    1802       bphillip     Moved event bus registration from post-construct to spring static method call
 * Jun 13, 2013 2095       djohnson     Can schedule any subclass of BandwidthAllocation.
 * Jun 25, 2013 2106       djohnson     Copy state from another instance, add ability to check for proposed bandwidth throughput changes.
 * Jul 09, 2013 2106       djohnson     Only needs to unregister from the EventBus when used in an EDEX instance, so handled in EdexBandwidthManager.
 * Oct 03, 2013 2267       bgonzale     Added check for no retrieval plan matching in the proposed retrieval plans.
 * Jan 30, 2014   2686     dhladky      refactor of retrieval.
 * Feb 10, 2014  2678      dhladky      Prevent duplicate allocations.
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
    public <T extends BandwidthAllocation> List<BandwidthAllocation> schedule(
            List<T> bandwidthAllocations) {
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
                    bandwidthDao.createOrUpdate(bandwidthAllocation);
                } else {

                    synchronized (plan) {
                        unscheduled.addAll(plan.schedule(bandwidthAllocation));
                        bandwidthDao.createOrUpdate(bandwidthAllocation);
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
            bandwidthDao.createOrUpdate(allocation);
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

        if (subscriptionRetrieval == null) {
            statusHandler.warn("Unable to find SubscriptionRetrieval by id ["
                    + eventId + "]");
            return;
        }

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
                        .getBandwidthSubscription().getId());

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
    
    /***
     * Method used in practice because we need to search for expired allocations.
     * @param network
     * @param agentType
     * @return
     */
    public List<BandwidthAllocation> getRecentAllocations(Network network, String agentType) {
        
        List<BandwidthAllocation> allocations = null;
        
        if (shutdown) {
            allocations = new ArrayList<BandwidthAllocation>(1);
            allocations.add(POISON_PILL);
            return allocations;
        }

        RetrievalPlan plan = getRetrievalPlans().get(network);
        if (plan != null) {
            synchronized (plan) {
                return plan.getRecentAllocations(agentType);
            }
        }
        
        return allocations;
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
        // From this point forward, only return a poison pill for this retrieval
        // manager, which will cause threads attempting to receive bandwidth
        // allocations to die
        this.shutdown = true;
    }

    /**
     * @param fromRetrievalManager
     */
    public void copyState(RetrievalManager fromRetrievalManager) {
        for (Entry<Network, RetrievalPlan> entry : fromRetrievalManager.retrievalPlans
                .entrySet()) {
            final Network network = entry.getKey();
            final RetrievalPlan fromPlan = entry.getValue();
            final RetrievalPlan toPlan = this.retrievalPlans.get(network);

            toPlan.copyState(fromPlan);
        }

    }

    /**
     * Check whether a change in the bandwidth throughput is being proposed.
     * 
     * @param proposedRetrievalManager
     *            the other retrieval manager with any proposed changes
     * @return true if a bandwidth throughput change is being proposed
     */
    public boolean isProposingBandwidthChanges(
            RetrievalManager proposedRetrievalManager) {
        boolean proposingBandwidthChanges = false;

        // If any retrieval plans have a different value for bandwidth, then
        // return true
        for (Entry<Network, RetrievalPlan> entry : this.retrievalPlans
                .entrySet()) {
            final RetrievalPlan proposedRetrievalPlan = proposedRetrievalManager.retrievalPlans
                    .get(entry.getKey());
            if (proposedRetrievalPlan != null && entry.getValue() != null) {
                if (proposedRetrievalPlan.getDefaultBandwidth() != entry
                        .getValue().getDefaultBandwidth()) {
                    proposingBandwidthChanges = true;
                    break;
                }
            } else {
                StringBuilder sb = new StringBuilder(
                        "The ProposedRetrievalPlan, ");
                sb.append(proposedRetrievalPlan);
                sb.append(", or the Existing RetrievalPlan, ");
                sb.append(entry.getKey());
                sb.append(" : ");
                sb.append(entry.getValue());
                sb.append(", is null.  Skipping this check.");
                statusHandler.info(sb.toString());
            }
        }

        return proposingBandwidthChanges;
    }

    /**
     * Initializes the retrieval plans.
     */
    public void initRetrievalPlans() {
        for (RetrievalPlan retrievalPlan : this.getRetrievalPlans().values()) {
            retrievalPlan.init();
        }
    }
}