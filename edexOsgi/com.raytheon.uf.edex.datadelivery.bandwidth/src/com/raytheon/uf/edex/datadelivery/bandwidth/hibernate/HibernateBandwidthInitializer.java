package com.raytheon.uf.edex.datadelivery.bandwidth.hibernate;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Set;

import com.raytheon.uf.common.datadelivery.registry.Subscription;
import com.raytheon.uf.common.registry.ebxml.RegistryUtil;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.edex.datadelivery.bandwidth.IBandwidthManager;
import com.raytheon.uf.edex.datadelivery.bandwidth.dao.BandwidthAllocation;
import com.raytheon.uf.edex.datadelivery.bandwidth.dao.IBandwidthDbInit;
import com.raytheon.uf.edex.datadelivery.bandwidth.interfaces.BandwidthInitializer;

/**
 * 
 * {@link BandwidthInitializer} that uses Hibernate.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Feb 20, 2013 1543       djohnson     Add SW history, separate how to find subscriptions.
 * Apr 30, 2013 1960       djohnson     just call init rather than drop/create tables explicitly.
 * 
 * </pre>
 * 
 * @author djohnson
 * @version 1.0
 */
public class HibernateBandwidthInitializer implements BandwidthInitializer {

    private static final IUFStatusHandler statusHandler = UFStatus
            .getHandler(HibernateBandwidthInitializer.class);

    private final IFindSubscriptionsForScheduling findSubscriptionsStrategy;

    /**
     * @param strategy
     */
    public HibernateBandwidthInitializer(
            IFindSubscriptionsForScheduling findSubscriptionsStrategy) {
        this.findSubscriptionsStrategy = findSubscriptionsStrategy;
    }

    @Override
    public boolean init(IBandwidthManager instance, IBandwidthDbInit dbInit) {

        // TODO: Need to resolve how to load Subscriptions that SHOULD have been
        // fulfilled. In the case were DD has been down for a while
        // BEFORE removing the tables...

        try {
            dbInit.init();
        } catch (Exception e1) {
            throw new RuntimeException(
                    "Error generating bandwidth manager tables", e1);
        }

        Set<Subscription> activeSubscriptions = Collections.emptySet();
        try {
            // Load active subscriptions
            activeSubscriptions = findSubscriptionsStrategy
                    .findSubscriptionsToSchedule();
        } catch (Exception e) {
            statusHandler.error(
                    "Failed to query for subscriptions to schedule", e);
            return false;
        }

        List<BandwidthAllocation> unscheduled = new ArrayList<BandwidthAllocation>();

        for (Subscription subscription : activeSubscriptions) {
            // Make sure the Id is set properly..
            subscription.setId(RegistryUtil.getRegistryObjectKey(subscription));
            statusHandler.info("init() - Loading subscription ["
                    + subscription.getName() + "]");
            unscheduled.addAll(instance.schedule(subscription));

            // TODO: Investigate the various strategies for "filling" in the
            // gaps of
            // data (and how to determine such gaps)

            /*
             * // For coverage purposes, attempt to add an AdhocSubscription for
             * the cycle of // Data previous to the first scheduled cycle of
             * data.
             * 
             * Calendar first = times.first(); int hour =
             * first.get(Calendar.HOUR_OF_DAY); // Find the cycle 'previous' to
             * the first times cycle SortedSet<Integer> cycles = new
             * TreeSet<Integer>(subscription.getTime().getCycleTimes()); if
             * (hour == cycles.first()) { // Have to back up to the previous
             * days last cycle. first.add(Calendar.DAY_OF_YEAR, -1);
             * first.set(Calendar.HOUR_OF_DAY, cycles.last()); } else { // We
             * are somewhere in the current days cycles.. Integer c =
             * cycles.first(); for (Integer cycle : cycles) { // As long as the
             * cycle in less than the hour we are looking for // keep moving
             * forward. if (cycle < hour) c = cycle; else break; } // c should
             * now be pointing to the cycle before the first times cycle...
             * first.set(Calendar.HOUR_OF_DAY, c); }
             */
        }

        // TODO DPJ: Do something more useful other than logging when unable to
        // schedule all subscriptions... maybe send a notification for a ride on
        // the EventBus?
        for (BandwidthAllocation allocation : unscheduled) {
            statusHandler.handle(Priority.PROBLEM,
                    "The following bandwidth allocation is in an unscheduled state:\n   "
                            + allocation);
        }

        return true;
    }
}
