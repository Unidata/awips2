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
package com.raytheon.uf.common.datadelivery.service.subscription;

import java.util.List;

import com.google.common.collect.Lists;
import com.raytheon.uf.common.datadelivery.registry.Subscription;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;
import com.raytheon.uf.common.serialization.comm.IServerRequest;

/**
 * Subscription overlap request object.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Oct 24, 2013   2292      mpduff     Initial creation
 * 
 * </pre>
 * 
 * @author mpduff
 * @version 1.0
 */
@DynamicSerialize
public class SubscriptionOverlapRequest implements IServerRequest {

    @DynamicSerializeElement
    private List<Subscription> subscriptionList;

    /**
     * Constructor.
     */
    public SubscriptionOverlapRequest() {

    }

    /**
     * Constructor.
     * 
     * @param subscriptionList
     *            - list of subscriptions
     */
    public SubscriptionOverlapRequest(List<Subscription> subscriptionList) {
        this.subscriptionList = subscriptionList;
    }

    /**
     * @return the subscriptionList
     */
    public List<Subscription> getSubscriptionList() {
        if (subscriptionList == null) {
            subscriptionList = Lists.newArrayListWithCapacity(0);
        }
        return subscriptionList;
    }

    /**
     * @param subscriptionList
     *            the subscriptionList to set
     */
    public void setSubscriptionList(List<Subscription> subscriptionList) {
        this.subscriptionList = subscriptionList;
    }
}
