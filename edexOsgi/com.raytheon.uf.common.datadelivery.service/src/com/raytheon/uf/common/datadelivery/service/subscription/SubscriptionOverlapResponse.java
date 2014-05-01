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
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;

/**
 * Subscription overlap response object.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Oct 24, 2013   2292     mpduff      Initial creation
 * 
 * </pre>
 * 
 * @author mpduff
 * @version 1.0
 */
@DynamicSerialize
public class SubscriptionOverlapResponse {
    @DynamicSerializeElement
    private boolean overlap;

    @DynamicSerializeElement
    private boolean duplicate;

    @DynamicSerializeElement
    private List<String> subscriptionNameList;

    /**
     * @return the overlap
     */
    public boolean isOverlap() {
        return overlap;
    }

    /**
     * @param overlap
     *            the overlap to set
     */
    public void setOverlap(boolean overlap) {
        this.overlap = overlap;
    }

    /**
     * @return the duplicate
     */
    public boolean isDuplicate() {
        return duplicate;
    }

    /**
     * @param duplicate
     *            the duplicate to set
     */
    public void setDuplicate(boolean duplicate) {
        this.duplicate = duplicate;
    }

    /**
     * @return the subscriptionNameList
     */
    public List<String> getSubscriptionNameList() {
        if (subscriptionNameList == null) {
            subscriptionNameList = Lists.newArrayListWithCapacity(0);
        }

        return subscriptionNameList;
    }

    /**
     * @param subscriptionNameList
     *            the subscriptionNameList to set
     */
    public void setSubscriptionNameList(List<String> subscriptionNameList) {
        this.subscriptionNameList = subscriptionNameList;
    }

    /**
     * @param name
     */
    public void addSubscriptionName(String name) {
        if (subscriptionNameList == null) {
            subscriptionNameList = Lists.newArrayList();
        }

        this.subscriptionNameList.add(name);
    }
}
