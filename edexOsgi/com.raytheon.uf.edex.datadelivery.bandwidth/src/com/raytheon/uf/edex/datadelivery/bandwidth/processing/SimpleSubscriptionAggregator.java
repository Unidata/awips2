package com.raytheon.uf.edex.datadelivery.bandwidth.processing;

import java.util.ArrayList;
import java.util.List;

import com.raytheon.uf.common.datadelivery.registry.Subscription;
import com.raytheon.uf.common.serialization.SerializationException;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.edex.datadelivery.bandwidth.dao.IBandwidthDao;
import com.raytheon.uf.edex.datadelivery.bandwidth.dao.SubscriptionDao;
import com.raytheon.uf.edex.datadelivery.bandwidth.dao.SubscriptionRetrieval;
import com.raytheon.uf.edex.datadelivery.bandwidth.interfaces.ISubscriptionAggregator;
import com.raytheon.uf.edex.datadelivery.bandwidth.retrieval.RetrievalStatus;
import com.raytheon.uf.edex.datadelivery.bandwidth.retrieval.SubscriptionRetrievalAgent;
import com.raytheon.uf.edex.datadelivery.bandwidth.util.BandwidthUtil;

/**
 * This implementation of ISubscriptionAggregator does no aggregation of
 * subscriptions. It simply creates one SubscriptionRetrieval per subscription,
 * populated with the necessary RetrievalRequests to fulfill the subscription
 * using the existing retrieval engine.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jul 18, 2012 726        jspinks     Initial creation
 * Nov 09, 2012 1286       djohnson    Rename interface to comply with standards.
 * Nov 20, 2012 1286       djohnson    Change some logging to debug.
 * 
 * </pre>
 * 
 * @author jspinks
 * @version 1.0
 */
public class SimpleSubscriptionAggregator implements ISubscriptionAggregator {

    private static final IUFStatusHandler statusHandler = UFStatus
            .getHandler(SimpleSubscriptionAggregator.class);

    private final IBandwidthDao bandwidthDao;

    public SimpleSubscriptionAggregator(IBandwidthDao bandwidthDao) {
        this.bandwidthDao = bandwidthDao;
    }

    @Override
    public List<SubscriptionRetrieval> aggregate(
            List<SubscriptionDao> subscriptions) {

        List<SubscriptionRetrieval> subscriptionRetrievals = new ArrayList<SubscriptionRetrieval>();

        // No aggregation or decomposition of subscriptions, simply create the
        // necessary retrievals without regards to 'sharing' retrievals across
        // subscriptions.

        for (SubscriptionDao subDao : subscriptions) {

            List<SubscriptionRetrieval> t = bandwidthDao
                    .querySubscriptionRetrievals(subDao.getIdentifier());
            // First check to see if the Object already was scheduled
            // (i.e. has SubscriptionRetrievals associated with it) if
            // not, create a SubscriptionRetrieval for the subscription
            if (t.size() == 0) {

                try {
                    SubscriptionRetrieval subscriptionRetrieval = new SubscriptionRetrieval();
                    // Link this SubscriptionRetrieval with the subscription.
                    subscriptionRetrieval.setSubscriptionDao(subDao);
                    subscriptionRetrieval.setNetwork(subDao.getRoute());
                    subscriptionRetrieval
                            .setAgentType(SubscriptionRetrievalAgent.SUBSCRIPTION_AGENT);
                    subscriptionRetrieval.setStatus(RetrievalStatus.PROCESSING);
                    subscriptionRetrieval.setPriority(subDao.getPriority());
                    subscriptionRetrieval.setEstimatedSize(subDao
                            .getEstimatedSize());

                    // Create a Retrieval Object for the Subscription
                    Subscription sub = subDao.getSubscription();

                    subscriptionRetrieval.setSubscriptionLatency(BandwidthUtil
                            .getSubscriptionLatency(sub));
                    subscriptionRetrieval
                            .setDataSetAvailablityDelay(BandwidthUtil
                                    .getDataSetAvailablityDelay(sub));

                    subscriptionRetrieval.setSubscription(sub);
                    subscriptionRetrievals.add(subscriptionRetrieval);

                } catch (SerializationException e) {
                    statusHandler
                            .warn("Trapped SerializationException attempting to process subscription ["
                                    + subDao.getIdentifier()
                                    + "]:  Subscription will not be scheduled.");
                }
            }

            if (statusHandler.isPriorityEnabled(Priority.DEBUG)) {
                statusHandler
                        .debug("Created ["
                                + subscriptionRetrievals.size()
                                + "] SubscriptionRetrieval Objects for SubscriptionDao ["
                                + subDao.getIdentifier() + "]");
            }
        }

        return subscriptionRetrievals;
    }

    @Override
    public List<SubscriptionDao> completeRetrieval(
            List<SubscriptionRetrieval> retrievals) {

        List<SubscriptionDao> daos = new ArrayList<SubscriptionDao>();
        // We know that only one SubscriptionRetrieval was created for each
        // Subscription so there will not be any duplication of subscription
        // ids.
        for (SubscriptionRetrieval retrieval : retrievals) {
            daos.add(bandwidthDao.getSubscriptionDao(retrieval
                    .getSubscriptionDao().getId()));
        }

        StringBuilder sb = new StringBuilder();
        for (SubscriptionDao dao : daos) {
            sb.append("Fulfilled subscription [").append(dao.getIdentifier());
            sb.append("][").append(dao.getRegistryId()).append("]\n");
        }
        if (sb.length() > 0) {
            statusHandler.info(sb.toString());
        }

        return daos;
    }
}
