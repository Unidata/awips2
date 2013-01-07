package com.raytheon.uf.edex.datadelivery.retrieval;

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

import java.util.List;

import com.raytheon.uf.common.datadelivery.registry.AdhocSubscription;
import com.raytheon.uf.common.datadelivery.registry.PendingSubscription;
import com.raytheon.uf.common.datadelivery.registry.Provider.ServiceType;
import com.raytheon.uf.common.datadelivery.registry.Subscription;
import com.raytheon.uf.common.datadelivery.registry.SubscriptionBundle;
import com.raytheon.uf.common.datadelivery.retrieval.xml.Retrieval;
import com.raytheon.uf.common.datadelivery.retrieval.xml.Retrieval.SubscriptionType;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.edex.datadelivery.retrieval.adapters.RetrievalAdapter;

/**
 * Generate Retrieval
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Feb 07, 2012            dhladky      Initial creation
 * Jul 25, 2012 955        djohnson     Use {@link ServiceTypeFactory}.
 * Nov 19, 2012 1166       djohnson     Clean up JAXB representation of registry objects.
 * Nov 26, 2012 1340       dhladky      Recognize type of subscriptions for statistics.
 * 
 * </pre>
 * 
 * @author dhladky
 * @version 1.0
 */
public abstract class RetrievalGenerator {

    private final ServiceType serviceType;

    private static final IUFStatusHandler statusHandler = UFStatus
            .getHandler(RetrievalGenerator.class);

    public RetrievalGenerator(ServiceType serviceType) {
        this.serviceType = serviceType;
    }

    /**
     * Gets the service adapter.
     * 
     * @param serviceType
     * @return
     */
    protected ServiceType getServiceType() {
        return serviceType;
    }

    /***
     * Build the necessary retrieval objects
     * 
     * @param bundle
     * @return
     * @return
     */
    public abstract List<Retrieval> buildRetrieval(SubscriptionBundle bundle);

    protected abstract RetrievalAdapter getServiceRetrievalAdapter();

    /**
     * Check for duplicates;
     * 
     * @return
     */
    protected abstract Subscription removeDuplicates(Subscription sub);

    /**
     * Gets the type of subscription based on the subscription object type
     * 
     * @param sub
     * @return
     */
    public SubscriptionType getSubscriptionType(Subscription sub) {
        
        if (sub instanceof AdhocSubscription) {
            return SubscriptionType.AD_HOC;
        } else if (sub instanceof Subscription) {
            return SubscriptionType.SUBSCRIBED;
        } else if (sub instanceof PendingSubscription) {
            // I don't really think we'll use this but......
            return SubscriptionType.PENDING;
        } else {
            statusHandler.error("Unknown Type of subscription! "
                    + sub.getName());
            return null;
        }
    }
}
