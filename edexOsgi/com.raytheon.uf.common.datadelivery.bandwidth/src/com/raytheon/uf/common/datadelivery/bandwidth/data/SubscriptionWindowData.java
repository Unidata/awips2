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
package com.raytheon.uf.common.datadelivery.bandwidth.data;

import java.util.ArrayList;
import java.util.List;

import com.raytheon.uf.common.datadelivery.registry.Network;
import com.raytheon.uf.common.datadelivery.registry.Subscription.SubscriptionPriority;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;

/**
 * Subscription object holding the download window information.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Nov 21, 2013   2545     mpduff      Initial creation
 * 
 * </pre>
 * 
 * @author mpduff
 * @version 1.0
 */
@DynamicSerialize
public class SubscriptionWindowData {
    /** The network this subscription is associate with */
    @DynamicSerializeElement
    private Network network;

    /** The registry id of the subscription */
    @DynamicSerializeElement
    private String registryId;

    /** The subscription name */
    @DynamicSerializeElement
    private String subscriptionName;

    /** The list of download window data */
    @DynamicSerializeElement
    private List<TimeWindowData> windowDataList;

    /** The subscription's priority */
    @DynamicSerializeElement
    private SubscriptionPriority priority;

    /**
     * Default Constructor.
     */
    public SubscriptionWindowData() {
        windowDataList = new ArrayList<TimeWindowData>();
    }

    /**
     * Constructor.
     * 
     * @param subscriptionName
     *            The subscription's name
     * @param windowsDataList
     *            The list of download window data
     * @param priority
     *            The subscription's priority
     */
    public SubscriptionWindowData(String subscriptionName,
            List<TimeWindowData> windowsDataList, SubscriptionPriority priority) {
        this.subscriptionName = subscriptionName;
        this.windowDataList = windowsDataList;
        this.priority = priority;
    }

    /**
     * @return the subscriptionName
     */
    public String getSubscriptionName() {
        return subscriptionName;
    }

    /**
     * @param subscriptionName
     *            the subscriptionName to set
     */
    public void setSubscriptionName(String subscriptionName) {
        this.subscriptionName = subscriptionName;
    }

    /**
     * @return the windowDataList
     */
    public List<TimeWindowData> getWindowDataList() {
        return windowDataList;
    }

    /**
     * @param windowDataList
     *            the windowDataList to set
     */
    public void setWindowDataList(List<TimeWindowData> windowDataList) {
        this.windowDataList = windowDataList;
    }

    /**
     * Add a TimeWindowData object
     */
    public void addTimeWindow(TimeWindowData windowData) {
        this.windowDataList.add(windowData);
    }

    /**
     * @return the priority
     */
    public SubscriptionPriority getPriority() {
        return priority;
    }

    /**
     * @param priority
     *            the priority to set
     */
    public void setPriority(SubscriptionPriority priority) {
        this.priority = priority;
    }

    /**
     * @return the network
     */
    public Network getNetwork() {
        return network;
    }

    /**
     * @param network
     *            the network to set
     */
    public void setNetwork(Network network) {
        this.network = network;
    }

    /**
     * @return the registryId
     */
    public String getRegistryId() {
        return registryId;
    }

    /**
     * @param registryId
     *            the registryId to set
     */
    public void setRegistryId(String registryId) {
        this.registryId = registryId;
    }

    /*
     * (non-Javadoc)
     * 
     * @see java.lang.Object#hashCode()
     */
    @Override
    public int hashCode() {
        final int prime = 31;
        int result = 1;
        result = prime * result + ((network == null) ? 0 : network.hashCode());
        result = prime * result
                + ((priority == null) ? 0 : priority.hashCode());
        result = prime * result
                + ((registryId == null) ? 0 : registryId.hashCode());
        result = prime
                * result
                + ((subscriptionName == null) ? 0 : subscriptionName.hashCode());
        result = prime * result
                + ((windowDataList == null) ? 0 : windowDataList.hashCode());
        return result;
    }

    /*
     * (non-Javadoc)
     * 
     * @see java.lang.Object#equals(java.lang.Object)
     */
    @Override
    public boolean equals(Object obj) {
        if (this == obj) {
            return true;
        }
        if (obj == null) {
            return false;
        }
        if (!(obj instanceof SubscriptionWindowData)) {
            return false;
        }
        SubscriptionWindowData other = (SubscriptionWindowData) obj;
        if (network != other.network) {
            return false;
        }
        if (priority != other.priority) {
            return false;
        }
        if (registryId == null) {
            if (other.registryId != null) {
                return false;
            }
        } else if (!registryId.equals(other.registryId)) {
            return false;
        }
        if (subscriptionName == null) {
            if (other.subscriptionName != null) {
                return false;
            }
        } else if (!subscriptionName.equals(other.subscriptionName)) {
            return false;
        }
        if (windowDataList == null) {
            if (other.windowDataList != null) {
                return false;
            }
        } else if (!windowDataList.equals(other.windowDataList)) {
            return false;
        }
        return true;
    }
}
