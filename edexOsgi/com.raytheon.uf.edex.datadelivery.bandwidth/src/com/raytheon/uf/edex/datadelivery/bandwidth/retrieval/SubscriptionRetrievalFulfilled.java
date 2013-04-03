package com.raytheon.uf.edex.datadelivery.bandwidth.retrieval;

import com.raytheon.uf.edex.datadelivery.bandwidth.dao.SubscriptionRetrieval;

public class SubscriptionRetrievalFulfilled {

    private SubscriptionRetrieval subscriptionRetrieval;

    public SubscriptionRetrievalFulfilled(
            SubscriptionRetrieval subscriptionRetrieval) {
        this.setSubscriptionRetrieval(subscriptionRetrieval);
    }

    public void setSubscriptionRetrieval(
            SubscriptionRetrieval subscriptionRetrieval) {
        this.subscriptionRetrieval = subscriptionRetrieval;
    }

    public SubscriptionRetrieval getSubscriptionRetrieval() {
        return subscriptionRetrieval;
    }

}
