package com.raytheon.uf.edex.datadelivery.bandwidth.hibernate;

import java.util.List;
import java.util.Map;

import com.raytheon.uf.common.datadelivery.registry.Network;
import com.raytheon.uf.common.datadelivery.registry.Subscription;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.common.util.StringUtil;
import com.raytheon.uf.edex.datadelivery.bandwidth.IBandwidthManager;
import com.raytheon.uf.edex.datadelivery.bandwidth.dao.IBandwidthDbInit;
import com.raytheon.uf.edex.datadelivery.bandwidth.interfaces.BandwidthInitializer;
import com.raytheon.uf.edex.datadelivery.bandwidth.retrieval.RetrievalManager;
import com.raytheon.uf.edex.datadelivery.util.DataDeliveryIdUtil;

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
 * Apr 30, 2013 1960       djohnson     just call init rather than drop/create tables explicitly.
 * Jun 25, 2013 2106       djohnson     init() now takes a {@link RetrievalManager} as well.
 * Sep 05, 2013 2330       bgonzale     On WFO registry init, only subscribe to local site subscriptions.
 * Sep 06, 2013 2344       bgonzale     Removed attempt to add to immutable empty set.
 * Oct 16, 2013 2267       bgonzale     executeAfterRegistryInit subscribes to all local.  Removed is shared checks.
 * Nov 04, 2013 2506       bgonzale     added site field.  facilitates testing.
 * Nov 19, 2013 2545       bgonzale     Removed programmatic customization for central, client, and dev(monolithic) 
 *                                      registries since the injected FindSubscription handler will be configured now.
 * Jan 29, 2014 2636       mpduff       Scheduling refactor.
 * Feb 06, 2014 2636       bgonzale     Use scheduling initialization method after registry init.
 * Feb 11, 2014 2771       bgonzale     Use Data Delivery ID instead of Site.
 * Feb 14, 2014 2636       mpduff       Clean up logging
 * </pre>
 * 
 * @author djohnson
 * @version 1.0
 */
public class HibernateBandwidthInitializer implements BandwidthInitializer {

    private static final IUFStatusHandler statusHandler = UFStatus
            .getHandler(HibernateBandwidthInitializer.class);

    private final IFindSubscriptionsForScheduling findSubscriptionsStrategy;

    private final String site;

    private IBandwidthManager instance;

    /**
     * @param strategy
     */
    public HibernateBandwidthInitializer(
            IFindSubscriptionsForScheduling findSubscriptionsStrategy) {
        this(findSubscriptionsStrategy, DataDeliveryIdUtil.getId());
    }

    /**
     * @param string
     * @param strategy
     */
    HibernateBandwidthInitializer(
            IFindSubscriptionsForScheduling findSubscriptionsStrategy,
            String site) {
        this.findSubscriptionsStrategy = findSubscriptionsStrategy;
        this.site = site;
    }

    @Override
    public boolean init(IBandwidthManager instance, IBandwidthDbInit dbInit,
            RetrievalManager retrievalManager) {

        this.instance = instance;

        // TODO: Need to resolve how to load Subscriptions that SHOULD have been
        // fulfilled. In the case were DD has been down for a while
        // BEFORE removing the tables...

        try {
            dbInit.init();
        } catch (Exception e1) {
            throw new RuntimeException(
                    "Error generating bandwidth manager tables", e1);
        }

        retrievalManager.initRetrievalPlans();

        return true;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void executeAfterRegistryInit() {
        try {
            Map<Network, List<Subscription>> subMap = findSubscriptionsStrategy
                    .findSubscriptionsToSchedule();

            List<String> unscheduled = instance.initializeScheduling(subMap);

            if (!unscheduled.isEmpty()) {
                StringBuilder sb = new StringBuilder("The following subscriptions could not be scheduled at startup: ");
                sb.append(StringUtil.NEWLINE);
                for (String subscription : unscheduled) {
                    sb.append(subscription).append(" ");
                }
                statusHandler.handle(Priority.INFO, sb.toString());
            }
        } catch (Exception e) {
            statusHandler.error(
                    "Failed to query for subscriptions to schedule", e);
        }
    }
}
