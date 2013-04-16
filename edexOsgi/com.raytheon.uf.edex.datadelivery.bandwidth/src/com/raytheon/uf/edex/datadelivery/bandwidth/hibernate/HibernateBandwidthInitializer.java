package com.raytheon.uf.edex.datadelivery.bandwidth.hibernate;

import java.sql.SQLException;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Set;

import org.hibernate.cfg.AnnotationConfiguration;

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
 * Apr 16, 2013 1906       djohnson     Implements RegistryInitializedListener.
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

    private IBandwidthManager instance;

    /**
     * @param strategy
     */
    public HibernateBandwidthInitializer(
            IFindSubscriptionsForScheduling findSubscriptionsStrategy) {
        this.findSubscriptionsStrategy = findSubscriptionsStrategy;
    }

    @Override
    public boolean init(IBandwidthManager instance, IBandwidthDbInit dbInit) {

        this.instance = instance;

        // TODO: Need to resolve how to load Subscriptions that SHOULD have been
        // fulfilled. In the case were DD has been down for a while
        // BEFORE removing the tables...

        // Empty the bandwidth tables (other than BandwidthDataSetUpdate) on
        // each start and reload..
        AnnotationConfiguration aConfig = new AnnotationConfiguration();
        aConfig.addAnnotatedClass(com.raytheon.uf.edex.datadelivery.bandwidth.dao.BandwidthSubscription.class);
        aConfig.addAnnotatedClass(com.raytheon.uf.edex.datadelivery.bandwidth.dao.SubscriptionRetrieval.class);
        aConfig.addAnnotatedClass(com.raytheon.uf.edex.datadelivery.bandwidth.dao.BandwidthAllocation.class);

        try {
            dbInit.dropTables(aConfig);
            dbInit.createTables(aConfig);
        } catch (SQLException e) {
            // Cannot proceed from here..
            return false;
        }

        return true;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void executeAfterRegistryInit() {
        Set<Subscription> activeSubscriptions = Collections.emptySet();
        try {
            // Load active subscriptions
            activeSubscriptions = findSubscriptionsStrategy
                    .findSubscriptionsToSchedule();
        } catch (Exception e) {
            statusHandler.error(
                    "Failed to query for subscriptions to schedule", e);
        }

        List<BandwidthAllocation> unscheduled = new ArrayList<BandwidthAllocation>();

        for (Subscription subscription : activeSubscriptions) {
            // Make sure the Id is set properly..
            subscription.setId(RegistryUtil.getRegistryObjectKey(subscription));
            statusHandler.info("init() - Loading subscription ["
                    + subscription.getName() + "]");
            unscheduled.addAll(instance.schedule(subscription));

            for (BandwidthAllocation allocation : unscheduled) {
                statusHandler.handle(Priority.PROBLEM,
                        "The following bandwidth allocation is in an unscheduled state:\n   "
                                + allocation);
            }
        }
    }
}
