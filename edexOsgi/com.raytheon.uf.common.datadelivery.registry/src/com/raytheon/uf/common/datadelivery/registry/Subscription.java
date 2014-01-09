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
package com.raytheon.uf.common.datadelivery.registry;

import java.util.Calendar;
import java.util.Date;
import java.util.List;
import java.util.Set;

import javax.xml.bind.annotation.XmlEnum;
import javax.xml.bind.annotation.XmlEnumValue;

/**
 * Definition of a subscription.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Mar 25, 2013 1841       djohnson     Extracted from UserSubscription.
 * Apr 08, 2013 1826       djohnson     Remove delivery options.
 * May 15, 2013 1040       mpduff       Changed officeId to a set.
 * Jul 11, 2013 2106       djohnson     SubscriptionPriority allows comparison.
 * Sept 30,2013 1797       dhladky      Abstracted and genericized.
 * Oct 23, 2013 2484       dhladky      Unique ID for subscriptions updated.
 * Nov 14, 2013   2548     mpduff       Add a subscription type information.
 * Jan 08, 2014 2615       bgonzale     Added calculate start and calculate end methods.
 * 
 * </pre>
 * 
 * @author djohnson
 * @version 1.0
 */

public interface Subscription<T extends Time, C extends Coverage> {

    @XmlEnum
    public enum SubscriptionType {
        QUERY, RECURRING;
    }

    /** Enumeration to use for subscription priorities */
    @XmlEnum
    public static enum SubscriptionPriority {
        /** High Priority */
        @XmlEnumValue("High")
        HIGH("High", 1),
        /** Default Priority */
        @XmlEnumValue("Normal")
        NORMAL("Normal", 2),
        /** Low Priority */
        @XmlEnumValue("Low")
        LOW("Low", 3);

        /** Priority Setting */
        private final String priorityName;

        /** Numeric Value of the priority */
        private final int priorityValue;

        private SubscriptionPriority(String priorityName, Integer priorityValue) {
            this.priorityName = priorityName;
            this.priorityValue = priorityValue;
        }

        /**
         * Get column name.
         * 
         * @return Priority Name
         */
        public String getPriorityName() {
            return priorityName;
        }

        /**
         * Get the integer value of the priority
         * 
         * @return The integer value of the priority.
         */
        public int getPriorityValue() {
            return priorityValue;
        }

        @Override
        public String toString() {
            return priorityName;
        }

        /**
         * Check whether this priority is higher than the other priority.
         * 
         * @param other
         *            the other priority
         * @return true if higher priority
         */
        public boolean isHigherPriorityThan(SubscriptionPriority other) {
            return this.priorityValue < other.priorityValue;
        }

        /**
         * Retrieve the {@link SubscriptionPriority} by its string
         * representation.
         * 
         * @param string
         *            the string representation
         * @return the {@link SubscriptionPriority}
         */
        public static SubscriptionPriority fromPriorityName(String string) {
            for (SubscriptionPriority potential : SubscriptionPriority.values()) {
                if (potential.getPriorityName().equals(string)) {
                    return potential;
                }
            }
            throw new IllegalArgumentException(
                    "Unable to find priority with priority name [" + string
                            + "]");
        }
    }

    /** Dataset Name slot */
    String DATA_SET_SLOT = "dataSetName";

    /** Provider slot */
    String PROVIDER_NAME_SLOT = "provider";

    /** Name slot */
    String NAME_SLOT = "name";

    /** Owner slot */
    String OWNER_SLOT = "owner";

    /** Route slot */
    String ROUTE_SLOT = "route";

    /** Originating Site slot */
    String ORIGINATING_SITE_SLOT = "originatingSite";

    /** Subscription type slot (query, recurring) */
    String SUBSCRIPTION_TYPE_SLOT = "subscriptionType";

    /**
     * Get subscription name.
     * 
     * @return subscription name
     */
    String getName();

    /**
     * Set subscription name.
     * 
     * @param name
     *            the name of the subscription
     */
    void setName(String name);

    /**
     * Get subscription group name.
     * 
     * @return subscription group name
     */
    String getGroupName();

    /**
     * Set subscription group name.
     * 
     * @param groupName
     *            the name of the subscription group
     */
    void setGroupName(String groupName);

    /**
     * Set subscription provider name.
     * 
     * @param provider
     *            the name of the subscription provider
     */
    void setProvider(String provider);

    /**
     * Get provider name.
     * 
     * @return provider name
     */
    String getProvider();

    /**
     * Get subscription owner name.
     * 
     * @return subscription owner name
     */
    String getOwner();

    /**
     * Get office ids.
     * 
     * @return office id list
     */
    Set<String> getOfficeIDs();

    /**
     * Set office ids.
     * 
     * @param officeIDs
     *            the office ids
     */
    void setOfficeIDs(Set<String> officeIDs);

    /**
     * Get subscription priority for fulfillment.
     * 
     * @return subscription name
     */
    SubscriptionPriority getPriority();

    /**
     * Set subscription priority.
     * 
     * @param priority
     *            priority
     */
    void setPriority(SubscriptionPriority priority);

    /**
     * Get subscription start time.
     * 
     * @return subscription start
     */
    Date getSubscriptionStart();

    /**
     * Set subscription start time.
     * 
     * @param subscriptionStart
     *            date time group for subscription start
     */
    void setSubscriptionStart(Date subscriptionStart);

    /**
     * Get subscription end time.
     * 
     * @return subscription end time date time group for subscription end
     */
    Date getSubscriptionEnd();

    /**
     * Set subscription end time.
     * 
     * @param subscriptionEnd
     *            date time group for subscription end
     */
    void setSubscriptionEnd(Date subscriptionEnd);

    /**
     * Get active period start date.
     * 
     * @return activePeriodStart
     */
    Date getActivePeriodStart();

    /**
     * Set active period start date.
     * 
     * @param activePeriodStart
     *            date for subscription start
     */
    void setActivePeriodStart(Date activePeriodStart);

    /**
     * Get active period end date.
     * 
     * @return activePeriodEnd
     */
    Date getActivePeriodEnd();

    /**
     * Set active period end date.
     * 
     * @param activePeriodEnd
     *            date for subscription end
     */
    void setActivePeriodEnd(Date activePeriodEnd);

    /**
     * Calculate the earliest that this subscription is valid based on active
     * period and start time.
     * 
     * @param startConstraint
     *            the earliest valid time.
     * 
     * @return the valid subscription start Date.
     */
    Calendar calculateStart(Calendar startConstraint);

    /**
     * Calculate the latest that this subscription is valid based on active
     * period and end time.
     * 
     * @param endConstraint
     *            the latest valid time.
     * 
     * @return the valid subscription end Date.
     */
    Calendar calculateEnd(Calendar endConstraint);

    /**
     * isNotify flag for subscription.
     * 
     * @return boolean true if full dataset
     */
    boolean isFullDataSet();

    /**
     * Set fullDataSet flag.
     * 
     * @param fullDataSet
     *            true if full dataset
     */
    void setFullDataSet(boolean fullDataSet);

    /**
     * Get size of the dataset for the subscription.
     * 
     * @return dataSetSize size of dataset
     */
    long getDataSetSize();

    /**
     * Set the dataset size for the subscription.
     * 
     * @param dataSetSize
     *            size of dataset
     */
    void setDataSetSize(long dataSetSize);

    /**
     * Get subscription coverage area.
     * 
     * @return coverage
     */
    C getCoverage();

    /**
     * Set the coverage area for the subscription.
     * 
     * @param coverage
     *            coverage area
     */
    void setCoverage(C coverage);

    /**
     * Get subscription submission time.
     * 
     * @return subscription time
     */
    T getTime();

    /**
     * Set the subscription submission time.
     * 
     * @param time
     *            time stamp
     */
    void setTime(T time);

    /**
     * Set the subscription parameters.
     * 
     * @param parameter
     *            subscription parameter list
     */
    void setParameter(List<Parameter> parameter);

    /**
     * Get subscription parameter list.
     * 
     * @return subscription parameter list
     */
    List<Parameter> getParameter();

    /**
     * Add subscription parameters.
     * 
     * @param par
     *            a subscription parameter
     */
    void addParameter(Parameter par);

    /**
     * Remove subscription parameters.
     * 
     * @param par
     *            a subscription parameter
     */
    void removeParameter(Parameter par);

    /**
     * Add subscription id.
     * 
     * @param subscriptionId
     *            a subscription id
     */
    void setSubscriptionId(String subscriptionId);

    /**
     * Get subscription id.
     * 
     * @return subscription id
     */
    String getSubscriptionId();

    /**
     * Get subscription description.
     * 
     * @return subscription description
     */
    String getDescription();

    /**
     * Set the subscription description.
     * 
     * @param description
     *            subscription description
     */
    void setDescription(String description);

    /**
     * Get subscription dataset name.
     * 
     * @return subscription dataset name
     */
    String getDataSetName();

    /**
     * Set the subscription dataSetName.
     * 
     * @param dataSetName
     *            subscription dataSetName
     */
    void setDataSetName(String dataSetName);

    /**
     * isActive flag for subscription status.
     * 
     * @return boolean true if subscription is Active
     */
    boolean isActive();

    /**
     * Set the subscription status to active.
     * 
     * @param active
     *            subscription active
     */
    void setActive(boolean active);

    /**
     * Set subscription valid.
     * 
     * @param valid
     *            true if subscription valid
     */
    void setValid(boolean valid);

    /**
     * Return if subscription is valid or invalid
     * 
     * @return true if subscription is valid
     */
    boolean isValid();

    /**
     * Get the subscription url.
     * 
     * @return the url
     */
    String getUrl();

    /**
     * Set the subscription url.
     * 
     * @param url
     *            the url to set
     */
    void setUrl(String url);

    /**
     * Get subscription dataset type.
     * 
     * @return subscription dataset type
     */
    DataType getDataSetType();

    /**
     * Set the dataset type
     * 
     * @param dataSetType
     *            the dataSetType to set
     */
    void setDataSetType(DataType dataSetType);

    /**
     * isDeleted flag.
     * 
     * @return true if the subscription has been deleted
     */
    boolean isDeleted();

    /**
     * Set the deleted flag.
     * 
     * @param deleted
     *            set subscription to deleted
     */
    void setDeleted(boolean deleted);

    /**
     * @return the unscheduled
     */
    boolean isUnscheduled();

    /**
     * @param unscheduled
     *            the unscheduled to set
     */
    void setUnscheduled(boolean unscheduled);

    /**
     * Get subscription id.
     * 
     * @return subscription id
     */
    String getId();

    /**
     * Set the subscription id.
     * 
     * @param id
     *            set subscription id
     */
    void setId(String id);

    /**
     * Determine if subscription status is expired.
     * 
     * @return true if status is expired
     */
    boolean isExpired();

    /**
     * Get the current subscription status.
     * 
     * @return String value of SUBSCRIPTION_STATUS
     */
    String getStatus();

    /**
     * Get the route.
     * 
     * @return the route
     */
    Network getRoute();

    /**
     * Set the route.
     * 
     * @param route
     *            the route
     */
    void setRoute(Network route);

    /**
     * Set the latency in minutes.
     * 
     * @param latencyInMinutes
     *            the latency, in minutes
     * 
     */
    void setLatencyInMinutes(int latencyInMinutes);

    /**
     * Get the latency, in minutes.
     * 
     * @return the latency in minutes
     */
    int getLatencyInMinutes();

    /**
     * Get the {@link Ensemble}.
     * 
     * @return the ensemble
     */
    Ensemble getEnsemble();

    /**
     * Set the ensememble.
     * 
     * @param ensemble
     *            the ensemble
     */
    void setEnsemble(Ensemble ensemble);

    /**
     * Copy the subscription.
     * 
     * @return the copy
     */
    Subscription<T, C> copy();

    /**
     * Copy the subscription.
     * 
     * @return the copy with the new name
     */
    Subscription<T, C> copy(String newName);

    /**
     * @param currentUser
     * @return
     */
    InitialPendingSubscription<T, C> initialPending(String currentUser);

    /**
     * @param currentUser
     * @return
     */
    PendingSubscription<T, C> pending(String currentUser);

    /**
     * Add an office Id to the list.
     * 
     * @param officeId
     *            Office Id to add
     */
    void addOfficeID(String officeId);

    /**
     * Gets the original site the subscription was created as
     * 
     * @return
     */
    String getOriginatingSite();

    /**
     * Sets the originating Site the subscription was created as
     * 
     * @param originatingSite
     */
    void setOriginatingSite(String originatingSite);

    /**
     * Get the subscription type (Recurring or Query)
     * 
     * @return the SubscriptionType
     */
    SubscriptionType getSubscriptionType();

    /**
     * Set the subscription type.
     * 
     * @param subType
     */
    void setSubscriptionType(SubscriptionType subType);
}