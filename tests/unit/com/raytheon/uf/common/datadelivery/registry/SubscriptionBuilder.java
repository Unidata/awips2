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

import java.util.Date;

import com.raytheon.uf.common.registry.ebxml.RegistryUtil;
import com.raytheon.uf.common.time.util.TimeUtil;

/**
 * Build {@link Subscription} objects with custom values.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jan 07, 2013 1453       djohnson     Initial creation
 * 
 * </pre>
 * 
 * @author djohnson
 * @version 1.0
 */

public class SubscriptionBuilder {

    private int latencyInMinutes = 0;

    private boolean active = true;

    private Date activePeriodStart;

    private Date activePeriodEnd;

    private String dataSetName = "someDataSet";

    private long dataSetSize;

    private DataType dataType = DataType.GRID;

    private boolean deleted = false;

    private String description = "someDescription";

    private boolean fullDataSet = false;

    private String groupName = "None";

    private String name = "your_awesome_subscription";

    private boolean notify = true;

    private String officeId = "officeId";

    private String owner = "your_user";

    private int priority = 1;

    private Date subscriptionStart = TimeUtil.newDate();

    private Date subscriptionEnd;

    private String url = "http://someurl";

    /**
     * Constructor.
     */
    public SubscriptionBuilder() {
    }

    /**
     * {@inheritDoc}
     */
    public Subscription build() {
        Subscription subscription = SubscriptionFixture.INSTANCE.get();
        subscription.setActive(active);
        subscription.setActivePeriodStart(activePeriodStart);
        subscription.setActivePeriodEnd(activePeriodEnd);
        subscription.setDataSetName(dataSetName);
        subscription.setDataSetSize(dataSetSize);
        subscription.setDataSetType(dataType);
        subscription.setDeleted(deleted);
        subscription.setDescription(description);
        subscription.setFullDataSet(fullDataSet);
        subscription.setGroupName(groupName);
        subscription.setLatencyInMinutes(latencyInMinutes);
        subscription.setName(name);
        subscription.setNotify(notify);
        subscription.setOfficeID(officeId);
        subscription.setOwner(owner);
        subscription.setPriority(priority);
        subscription.setSubscriptionStart(subscriptionStart);
        subscription.setSubscriptionEnd(subscriptionEnd);
        subscription.setUrl(url);

        subscription.setId(RegistryUtil.getRegistryObjectKey(subscription));

        return subscription;
    }

    /**
     * @param latencyInMinutes
     *            the latencyInMinutes to set
     */
    public SubscriptionBuilder withLatencyInMinutes(int latencyInMinutes) {
        this.latencyInMinutes = latencyInMinutes;
        return this;
    }

    /**
     * @param active
     *            the active to set
     */
    public SubscriptionBuilder withActive(boolean active) {
        this.active = active;
        return this;
    }

    /**
     * @param activePeriodStart
     *            the activePeriodStart to set
     */
    public SubscriptionBuilder withActivePeriodStart(Date activePeriodStart) {
        this.activePeriodStart = activePeriodStart;
        return this;
    }

    /**
     * @param activePeriodEnd
     *            the activePeriodEnd to set
     */
    public SubscriptionBuilder withActivePeriodEnd(Date activePeriodEnd) {
        this.activePeriodEnd = activePeriodEnd;
        return this;
    }

    /**
     * @param dataSetName
     *            the dataSetName to set
     */
    public SubscriptionBuilder withDataSetName(String dataSetName) {
        this.dataSetName = dataSetName;
        return this;
    }

    /**
     * @param dataSetSize
     *            the dataSetSize to set
     */
    public SubscriptionBuilder withDataSetSize(long dataSetSize) {
        this.dataSetSize = dataSetSize;
        return this;
    }

    /**
     * @param dataType
     *            the dataType to set
     */
    public SubscriptionBuilder withDataType(DataType dataType) {
        this.dataType = dataType;
        return this;
    }

    /**
     * @param deleted
     *            the deleted to set
     */
    public SubscriptionBuilder withDeleted(boolean deleted) {
        this.deleted = deleted;
        return this;
    }

    /**
     * @param description
     *            the description to set
     */
    public SubscriptionBuilder withDescription(String description) {
        this.description = description;
        return this;
    }

    /**
     * @param fullDataSet
     *            the fullDataSet to set
     */
    public SubscriptionBuilder withFullDataSet(boolean fullDataSet) {
        this.fullDataSet = fullDataSet;
        return this;
    }

    /**
     * @param groupName
     *            the groupName to set
     */
    public SubscriptionBuilder withGroupName(String groupName) {
        this.groupName = groupName;
        return this;
    }

    /**
     * @param name
     *            the name to set
     */
    public SubscriptionBuilder withName(String name) {
        this.name = name;
        return this;
    }

    /**
     * @param notify
     *            the notify to set
     */
    public SubscriptionBuilder withNotify(boolean notify) {
        this.notify = notify;
        return this;
    }

    /**
     * @param officeId
     *            the officeId to set
     */
    public SubscriptionBuilder withOfficeId(String officeId) {
        this.officeId = officeId;
        return this;
    }

    /**
     * @param owner
     *            the owner to set
     */
    public SubscriptionBuilder withOwner(String owner) {
        this.owner = owner;
        return this;
    }

    /**
     * @param priority
     *            the priority to set
     */
    public SubscriptionBuilder withPriority(int priority) {
        this.priority = priority;
        return this;
    }

    /**
     * @param subscriptionStart
     *            the subscriptionStart to set
     */
    public SubscriptionBuilder withSubscriptionStart(Date subscriptionStart) {
        this.subscriptionStart = subscriptionStart;
        return this;
    }

    /**
     * @param subscriptionEnd
     *            the subscriptionEnd to set
     */
    public SubscriptionBuilder withSubscriptionEnd(Date subscriptionEnd) {
        this.subscriptionEnd = subscriptionEnd;
        return this;
    }

    /**
     * @param url
     *            the url to set
     */
    public SubscriptionBuilder withUrl(String url) {
        this.url = url;
        return this;
    }

}
