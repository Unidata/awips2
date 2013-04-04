package com.raytheon.uf.edex.datadelivery.bandwidth.hibernate;

import java.sql.SQLException;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

import org.hibernate.cfg.AnnotationConfiguration;

import com.raytheon.uf.common.datadelivery.registry.Subscription;
import com.raytheon.uf.common.datadelivery.registry.handlers.DataDeliveryHandlers;
import com.raytheon.uf.common.registry.ebxml.RegistryUtil;
import com.raytheon.uf.common.registry.handler.RegistryHandlerException;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.edex.datadelivery.bandwidth.IBandwidthManager;
import com.raytheon.uf.edex.datadelivery.bandwidth.dao.BandwidthAllocation;
import com.raytheon.uf.edex.datadelivery.bandwidth.dao.IBandwidthDbInit;
import com.raytheon.uf.edex.datadelivery.bandwidth.interfaces.BandwidthInitializer;

public class HibernateBandwidthInitializer implements BandwidthInitializer {

    private static final IUFStatusHandler statusHandler = UFStatus
            .getHandler(HibernateBandwidthInitializer.class);

    @Override
    public boolean init(IBandwidthManager instance, IBandwidthDbInit dbInit) {

        // TODO: Need to resolve how to load Subscriptions that SHOULD have been
        // fulfilled. In the case were DD has been down for a while
        // BEFORE removing the tables...

        // Empty the bandwidth tables (other than DataSetMetaDataDao) on each
        // start and reload..
        AnnotationConfiguration aConfig = new AnnotationConfiguration();
        aConfig.addAnnotatedClass(com.raytheon.uf.edex.datadelivery.bandwidth.dao.SubscriptionDao.class);
        aConfig.addAnnotatedClass(com.raytheon.uf.edex.datadelivery.bandwidth.dao.SubscriptionRetrieval.class);
        aConfig.addAnnotatedClass(com.raytheon.uf.edex.datadelivery.bandwidth.dao.BandwidthAllocation.class);

        try {
            dbInit.dropTables(aConfig);
            dbInit.createTables(aConfig);
        } catch (SQLException e) {
            // Cannot proceed from here..
            return false;
        }

        List<Subscription> activeSubscriptions = Collections.emptyList();
        try {
            // Load active subscriptions
            activeSubscriptions = DataDeliveryHandlers.getSubscriptionHandler()
                    .getActive();
        } catch (RegistryHandlerException e) {
            statusHandler
                    .error("Failed to query for available active subscriptions");
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
