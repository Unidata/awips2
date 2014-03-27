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
package com.raytheon.uf.edex.datadelivery.service.services.overlap;

import java.util.ArrayList;
import java.util.Collections;
import java.util.HashSet;
import java.util.LinkedList;
import java.util.List;
import java.util.Set;

import com.raytheon.uf.common.datadelivery.registry.AdhocSubscription;
import com.raytheon.uf.common.datadelivery.registry.DataType;
import com.raytheon.uf.common.datadelivery.registry.Subscription;
import com.raytheon.uf.common.datadelivery.registry.handlers.DataDeliveryHandlers;
import com.raytheon.uf.common.datadelivery.registry.handlers.ISubscriptionHandler;
import com.raytheon.uf.common.datadelivery.service.subscription.SubscriptionOverlapRequest;
import com.raytheon.uf.common.datadelivery.service.subscription.SubscriptionOverlapResponse;
import com.raytheon.uf.common.serialization.comm.IRequestHandler;
import com.raytheon.uf.edex.datadelivery.util.DataDeliveryIdUtil;

/**
 * Edex handler for subscription overlap requests.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Oct 24, 2013    2292    mpduff      Initial creation
 * Nov 01, 2013    2292    dhladky     Don't check against yourself for duplication
 * Feb 11, 2014    2771    bgonzale    Use Data Delivery ID instead of Site.
 * 
 * </pre>
 * 
 * @author mpduff
 * @version 1.0
 */

public class SubscriptionOverlapHandler implements
        IRequestHandler<SubscriptionOverlapRequest> {

    /**
     * {@inheritDoc}
     */
    @Override
    public Object handleRequest(SubscriptionOverlapRequest request)
            throws Exception {
        String deliveryId = DataDeliveryIdUtil.getId();
        List<Subscription> subscriptions = request.getSubscriptionList();
        List<String> duplicateList = new LinkedList<String>();
        SubscriptionOverlapResponse response = new SubscriptionOverlapResponse();

        for (Subscription subscription : subscriptions) {
            if (!(subscription instanceof AdhocSubscription)) {
                final ISubscriptionHandler subscriptionHandler = DataDeliveryHandlers
                        .getSubscriptionHandler();
                final List<Subscription> potentialDuplicates = subscriptionHandler
                        .getActiveByDataSetAndProvider(
                                subscription.getDataSetName(),
                                subscription.getProvider());
                DataType dataType = subscription.getDataSetType();
                Set<String> overlappingSubscriptions = new HashSet<String>();
                for (Subscription potentialDuplicate : potentialDuplicates) {
                    // don't check against yourself
                    if (potentialDuplicate.getId().equals(subscription.getId())) {
                        continue;
                    }
                    OverlapData od = OverlapDataFactory.getOverlapData(
                            subscription, potentialDuplicate);

                    if (od.isDuplicate()) {
                        // If the subscription is local then it is flagged
                        // as a duplicate, otherwise it is marked as overlap.
                        if (potentialDuplicate.getOfficeIDs().contains(
                                deliveryId)) {
                            duplicateList.add(potentialDuplicate.getName());
                        } else {
                            overlappingSubscriptions.add(potentialDuplicate
                                    .getName());
                        }
                    }

                    if (od.isOverlapping()) {
                        overlappingSubscriptions.add(potentialDuplicate
                                .getName());
                    }
                }

                if (!overlappingSubscriptions.isEmpty()) {
                    List<String> overlapList = new ArrayList<String>(
                            overlappingSubscriptions);
                    Collections.sort(overlapList);
                    response.setSubscriptionNameList(overlapList);
                }

                response.setDuplicate(!duplicateList.isEmpty());
                response.setOverlap(!overlappingSubscriptions.isEmpty());
            }
        }

        return response;
    }
}
