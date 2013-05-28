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

import java.io.Serializable;
import java.util.ArrayList;
import java.util.Calendar;
import java.util.Date;
import java.util.List;
import java.util.Set;
import java.util.TreeSet;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlElements;
import javax.xml.bind.annotation.XmlSeeAlso;

import org.apache.commons.lang.builder.EqualsBuilder;
import org.apache.commons.lang.builder.HashCodeBuilder;

import com.google.common.collect.Sets;
import com.raytheon.uf.common.datadelivery.registry.Utils.SubscriptionStatus;
import com.raytheon.uf.common.registry.annotations.SlotAttribute;
import com.raytheon.uf.common.registry.annotations.SlotAttributeConverter;
import com.raytheon.uf.common.registry.ebxml.RegistryUtil;
import com.raytheon.uf.common.registry.ebxml.slots.SetSlotConverter;
import com.raytheon.uf.common.serialization.ISerializableObject;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;
import com.raytheon.uf.common.time.util.TimeUtil;

/**
 * Base definition of a recurring subscription.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Mar 25, 2013 1841       djohnson     Extracted from Subscription.
 * Apr 08, 2013 1826       djohnson     Remove delivery options.
 * May 15, 2013 1040       mpduff       Changed to use Set for office id.
 * May 21, 2013 2020       mpduff       Rename UserSubscription to SiteSubscription.
 * 
 * </pre>
 * 
 * @author djohnson
 * @version 1.0
 */
@XmlAccessorType(XmlAccessType.NONE)
@XmlSeeAlso({ PendingSiteSubscription.class, PendingSharedSubscription.class,
        AdhocSubscription.class, SiteSubscription.class,
        SharedSubscription.class })
public abstract class RecurringSubscription implements ISerializableObject,
        Serializable, Subscription {

    private static final long serialVersionUID = -6422673887457060034L;

    /**
     * Constructor.
     */
    public RecurringSubscription() {

    }

    /**
     * Initialization constructor.
     * 
     * @param sub
     *            Subscription object
     * @param name
     *            New subscription name
     */
    public RecurringSubscription(Subscription sub, String name) {
        this.setActive(sub.isActive());
        this.setActivePeriodEnd(sub.getActivePeriodEnd());
        this.setActivePeriodStart(sub.getActivePeriodStart());
        this.setCoverage(sub.getCoverage());
        this.setDataSetName(sub.getDataSetName());
        this.setDataSetSize(sub.getDataSetSize());
        this.setDescription(sub.getDescription());
        this.setFullDataSet(sub.isFullDataSet());
        this.setGroupName(sub.getGroupName());
        this.setId(sub.getId());
        this.setName(name);
        this.setOfficeIDs(sub.getOfficeIDs());
        this.setParameter(sub.getParameter());
        this.setPriority(sub.getPriority());
        this.setProvider(sub.getProvider());
        this.setSubscriptionEnd(sub.getSubscriptionEnd());
        this.setSubscriptionId(sub.getSubscriptionId());
        this.setSubscriptionStart(sub.getSubscriptionStart());
        this.setTime(sub.getTime());
        this.setUrl(sub.getUrl());
        this.setDataSetType(sub.getDataSetType());
        this.setRoute(sub.getRoute());
        this.setLatencyInMinutes(sub.getLatencyInMinutes());
        this.setEnsemble(sub.getEnsemble());

        // Set the registry id
        this.setId(RegistryUtil.getRegistryObjectKey(this));
    }

    /**
     * Copy constructor.
     * 
     * @param sub
     *            Subscription object
     */
    public RecurringSubscription(Subscription sub) {
        this(sub, sub.getName());
    }

    @XmlAttribute
    @DynamicSerializeElement
    private String subscriptionId;

    @XmlAttribute
    @DynamicSerializeElement
    @SlotAttribute(NAME_SLOT)
    private String name;

    @XmlAttribute
    @DynamicSerializeElement
    @SlotAttribute
    private String groupName = GroupDefinition.NO_GROUP;

    @XmlAttribute
    @DynamicSerializeElement
    @SlotAttribute(PROVIDER_NAME_SLOT)
    private String provider;

    @XmlElements({ @XmlElement(name = "officeId") })
    @DynamicSerializeElement
    @SlotAttribute
    @SlotAttributeConverter(SetSlotConverter.class)
    protected Set<String> officeIDs = Sets.newTreeSet();

    @XmlAttribute
    @DynamicSerializeElement
    private SubscriptionPriority priority = SubscriptionPriority.NORMAL;

    @XmlAttribute
    @DynamicSerializeElement
    private Date subscriptionStart;

    @XmlAttribute
    @DynamicSerializeElement
    private Date subscriptionEnd;

    @XmlAttribute
    @DynamicSerializeElement
    private Date activePeriodStart;

    @XmlAttribute
    @DynamicSerializeElement
    private Date activePeriodEnd;

    @XmlAttribute
    @DynamicSerializeElement
    @SlotAttribute
    private boolean fullDataSet;

    @XmlAttribute
    @DynamicSerializeElement
    private long dataSetSize;

    @XmlElement(name = "coverage")
    @DynamicSerializeElement
    private Coverage coverage;

    @XmlElement
    @DynamicSerializeElement
    @SlotAttribute
    @SlotAttributeConverter(TimeSlotConverter.class)
    private Time time;

    @XmlAttribute
    @DynamicSerializeElement
    private String description;

    @XmlAttribute
    @DynamicSerializeElement
    @SlotAttribute(Subscription.DATA_SET_SLOT)
    private String dataSetName;

    @XmlAttribute
    @DynamicSerializeElement
    @SlotAttribute
    private boolean active;

    @XmlAttribute
    @DynamicSerializeElement
    @SlotAttribute
    private boolean valid = true;

    @XmlAttribute
    @DynamicSerializeElement
    private boolean unscheduled;

    @XmlAttribute
    @DynamicSerializeElement
    private String url;

    @XmlAttribute
    @DynamicSerializeElement
    @SlotAttribute
    private DataType dataSetType;

    @XmlElements({ @XmlElement })
    @DynamicSerializeElement
    private List<Parameter> parameter;

    @XmlElement
    @DynamicSerializeElement
    private Ensemble ensemble;

    @XmlAttribute
    @DynamicSerializeElement
    private boolean deleted;

    @XmlAttribute
    @DynamicSerializeElement
    private String id;

    @XmlAttribute
    @DynamicSerializeElement
    @SlotAttribute(Subscription.ROUTE_SLOT)
    private Network route = Network.OPSNET;

    @XmlAttribute
    @DynamicSerializeElement
    private int latencyInMinutes;

    /**
     * Get subscription name.
     * 
     * @return subscription name
     */
    @Override
    public String getName() {
        return name;
    }

    /**
     * Set subscription name.
     * 
     * @param name
     *            the name of the subscription
     */
    @Override
    public void setName(String name) {
        this.name = name;
    }

    /**
     * Get subscription group name.
     * 
     * @return subscription group name
     */
    @Override
    public String getGroupName() {
        return groupName;
    }

    /**
     * Set subscription group name.
     * 
     * @param groupName
     *            the name of the subscription group
     */
    @Override
    public void setGroupName(String groupName) {
        this.groupName = groupName;
    }

    /**
     * Set subscription provider name.
     * 
     * @param provider
     *            the name of the subscription provider
     */
    @Override
    public void setProvider(String provider) {
        this.provider = provider;
    }

    /**
     * Get provider name.
     * 
     * @return provider name
     */
    @Override
    public String getProvider() {
        return provider;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public Set<String> getOfficeIDs() {
        return officeIDs;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void setOfficeIDs(Set<String> officeIDs) {
        this.officeIDs = new TreeSet<String>(officeIDs);
    }

    /**
     * Get subscription priority for fulfillment.
     * 
     * @return subscription name
     */
    @Override
    public SubscriptionPriority getPriority() {
        return priority;
    }

    /**
     * Set subscription priority.
     * 
     * @param priority
     *            priority
     */
    @Override
    public void setPriority(SubscriptionPriority priority) {
        this.priority = priority;
    }

    /**
     * Get subscription start time.
     * 
     * @return subscription start
     */
    @Override
    public Date getSubscriptionStart() {
        return subscriptionStart;
    }

    /**
     * Set subscription start time.
     * 
     * @param subscriptionStart
     *            date time group for subscription start
     */
    @Override
    public void setSubscriptionStart(Date subscriptionStart) {
        this.subscriptionStart = subscriptionStart;
    }

    /**
     * Get subscription end time.
     * 
     * @return subscription end time date time group for subscription end
     */
    @Override
    public Date getSubscriptionEnd() {
        return subscriptionEnd;
    }

    /**
     * Set subscription end time.
     * 
     * @param subscriptionEnd
     *            date time group for subscription end
     */
    @Override
    public void setSubscriptionEnd(Date subscriptionEnd) {
        this.subscriptionEnd = subscriptionEnd;
    }

    /**
     * Get active period start date.
     * 
     * @return activePeriodStart
     */
    @Override
    public Date getActivePeriodStart() {
        return activePeriodStart;
    }

    /**
     * Set active period start date.
     * 
     * @param activePeriodStart
     *            date for subscription start
     */
    @Override
    public void setActivePeriodStart(Date activePeriodStart) {
        this.activePeriodStart = activePeriodStart;
    }

    /**
     * Get active period end date.
     * 
     * @return activePeriodEnd
     */
    @Override
    public Date getActivePeriodEnd() {
        return activePeriodEnd;
    }

    /**
     * Set active period end date.
     * 
     * @param activePeriodEnd
     *            date for subscription end
     */
    @Override
    public void setActivePeriodEnd(Date activePeriodEnd) {
        this.activePeriodEnd = activePeriodEnd;
    }

    /**
     * isNotify flag for subscription.
     * 
     * @return boolean true if full dataset
     */
    @Override
    public boolean isFullDataSet() {
        return fullDataSet;
    }

    /**
     * Set fullDataSet flag.
     * 
     * @param fullDataSet
     *            true if full dataset
     */
    @Override
    public void setFullDataSet(boolean fullDataSet) {
        this.fullDataSet = fullDataSet;
    }

    /**
     * Get size of the dataset for the subscription.
     * 
     * @return dataSetSize size of dataset
     */
    @Override
    public long getDataSetSize() {
        return dataSetSize;
    }

    /**
     * Set the dataset size for the subscription.
     * 
     * @param dataSetSize
     *            size of dataset
     */
    @Override
    public void setDataSetSize(long dataSetSize) {
        this.dataSetSize = dataSetSize;
    }

    /**
     * Get subscription coverage area.
     * 
     * @return coverage
     */
    @Override
    public Coverage getCoverage() {
        return coverage;
    }

    /**
     * Set the coverage area for the subscription.
     * 
     * @param coverage
     *            coverage area
     */
    @Override
    public void setCoverage(Coverage coverage) {
        this.coverage = coverage;
    }

    /**
     * Get subscription submission time.
     * 
     * @return subscription time
     */
    @Override
    public Time getTime() {
        return time;
    }

    /**
     * Set the subscription submission time.
     * 
     * @param time
     *            time stamp
     */
    @Override
    public void setTime(Time time) {
        this.time = time;
    }

    /**
     * Set the subscription parameters.
     * 
     * @param parameter
     *            subscription parameter list
     */
    @Override
    public void setParameter(List<Parameter> parameter) {
        this.parameter = parameter;
    }

    /**
     * Get subscription parameter list.
     * 
     * @return subscription parameter list
     */
    @Override
    public List<Parameter> getParameter() {
        return parameter;
    }

    /**
     * Add subscription parameters.
     * 
     * @param par
     *            a subscription parameter
     */
    @Override
    public void addParameter(Parameter par) {
        if (parameter == null) {
            parameter = new ArrayList<Parameter>();
        }

        parameter.add(par);
    }

    /**
     * Remove subscription parameters.
     * 
     * @param par
     *            a subscription parameter
     */
    @Override
    public void removeParameter(Parameter par) {
        parameter.remove(par);
    }

    /**
     * Add subscription id.
     * 
     * @param subscriptionId
     *            a subscription id
     */
    @Override
    public void setSubscriptionId(String subscriptionId) {
        this.subscriptionId = subscriptionId;
    }

    /**
     * Get subscription id.
     * 
     * @return subscription id
     */
    @Override
    public String getSubscriptionId() {
        return subscriptionId;
    }

    /**
     * Get subscription description.
     * 
     * @return subscription description
     */
    @Override
    public String getDescription() {
        return description;
    }

    /**
     * Set the subscription description.
     * 
     * @param description
     *            subscription description
     */
    @Override
    public void setDescription(String description) {
        this.description = description;
    }

    /**
     * Get subscription dataset name.
     * 
     * @return subscription dataset name
     */
    @Override
    public String getDataSetName() {
        return dataSetName;
    }

    /**
     * Set the subscription dataSetName.
     * 
     * @param dataSetName
     *            subscription dataSetName
     */
    @Override
    public void setDataSetName(String dataSetName) {
        this.dataSetName = dataSetName;
    }

    /**
     * isActive flag for subscription status.
     * 
     * @return boolean true if subscription is Active
     */
    @Override
    public boolean isActive() {
        return active;
    }

    /**
     * Set the subscription status to active.
     * 
     * @param active
     *            subscription active
     */
    @Override
    public void setActive(boolean active) {
        this.active = active;
    }

    /**
     * Set subscription valid.
     * 
     * @param valid
     *            true if subscription valid
     */
    @Override
    public void setValid(boolean valid) {
        this.valid = valid;
    }

    /**
     * Return if subscription is valid or invalid
     * 
     * @return true if subscription is valid
     */
    @Override
    public boolean isValid() {
        return valid;
    }

    /**
     * Get the subscription url.
     * 
     * @return the url
     */
    @Override
    public String getUrl() {
        return url;
    }

    /**
     * Set the subscription url.
     * 
     * @param url
     *            the url to set
     */
    @Override
    public void setUrl(String url) {
        this.url = url;
    }

    /**
     * Get subscription dataset type.
     * 
     * @return subscription dataset type
     */
    @Override
    public DataType getDataSetType() {
        return dataSetType;
    }

    /**
     * Set the dataset type
     * 
     * @param dataSetType
     *            the dataSetType to set
     */
    @Override
    public void setDataSetType(DataType dataSetType) {
        this.dataSetType = dataSetType;
    }

    /**
     * isDeleted flag.
     * 
     * @return true if the subscription has been deleted
     */
    @Override
    public boolean isDeleted() {
        return deleted;
    }

    /**
     * Set the deleted flag.
     * 
     * @param deleted
     *            set subscription to deleted
     */
    @Override
    public void setDeleted(boolean deleted) {
        this.deleted = deleted;
    }

    /**
     * @return the unscheduled
     */
    @Override
    public boolean isUnscheduled() {
        return unscheduled;
    }

    /**
     * @param unscheduled
     *            the unscheduled to set
     */
    @Override
    public void setUnscheduled(boolean unscheduled) {
        this.unscheduled = unscheduled;
    }

    /**
     * Get subscription id.
     * 
     * @return subscription id
     */
    @Override
    public String getId() {
        return id;
    }

    /**
     * Set the subscription id.
     * 
     * @param id
     *            set subscription id
     */
    @Override
    public void setId(String id) {
        this.id = id;
    }

    @Override
    public boolean equals(Object obj) {
        if (obj instanceof Subscription) {
            Subscription other = (Subscription) obj;

            EqualsBuilder builder = new EqualsBuilder();
            builder.append(getProvider(), other.getProvider());
            builder.append(getName(), other.getName());
            builder.append(getDataSetName(), other.getDataSetName());
            builder.append(getOwner(), other.getOwner());

            return builder.isEquals();
        }
        return super.equals(obj);
    }

    @Override
    public int hashCode() {
        HashCodeBuilder builder = new HashCodeBuilder();
        builder.append(getProvider());
        builder.append(getName());
        builder.append(getDataSetName());
        builder.append(getOwner());

        return builder.toHashCode();
    }

    @Override
    public String toString() {
        return getName() + "::" + getProvider() + "::" + getDataSetName()
                + "::" + getOwner();
    }

    /**
     * Determine if subscription status is expired.
     * 
     * @return true if status is expired
     */
    @Override
    public boolean isExpired() {
        Calendar cal = TimeUtil.newGmtCalendar();
        Date today = cal.getTime();
        boolean expired = false;
        if (this.getSubscriptionEnd() != null
                && today.after(this.getSubscriptionEnd())) {
            expired = true;
        }

        return expired;
    }

    /**
     * Get the current subscription status.
     * 
     * @return String value of SUBSCRIPTION_STATUS
     */
    @Override
    public String getStatus() {
        SubscriptionStatus status = SubscriptionStatus.INVALID;

        if (isValid()) {
            if (isExpired()) {
                status = SubscriptionStatus.EXPIRED;
            } else if (!isActive()) {
                status = SubscriptionStatus.INACTIVE;
            } else {
                Calendar cal = TimeUtil.newGmtCalendar();
                Date today = cal.getTime();

                status = (inWindow(today)) ? SubscriptionStatus.ACTIVE
                        : SubscriptionStatus.INACTIVE;

                if (status == SubscriptionStatus.ACTIVE && isUnscheduled()) {
                    status = SubscriptionStatus.UNSCHEDULED;
                }
            }
        }

        return status.toString();

    }

    private boolean inWindow(Date checkDate) {
        if (activePeriodStart == null && activePeriodEnd == null) {
            return true;
        } else if (activePeriodStart != null && activePeriodEnd != null) {
            Calendar startCal = TimeUtil.newGmtCalendar();
            startCal.setTime(activePeriodStart);
            startCal = TimeUtil.minCalendarFields(startCal,
                    Calendar.HOUR_OF_DAY, Calendar.MINUTE, Calendar.SECOND,
                    Calendar.MILLISECOND);
            activePeriodStart = startCal.getTime();

            Calendar endCal = TimeUtil.newGmtCalendar();
            endCal.setTime(activePeriodEnd);
            endCal = TimeUtil.maxCalendarFields(endCal, Calendar.HOUR_OF_DAY,
                    Calendar.MINUTE, Calendar.SECOND, Calendar.MILLISECOND);

            // If the period crosses a year boundary, add a year to the end
            if (endCal.before(startCal)) {
                endCal.add(Calendar.YEAR, 1);
            }

            activePeriodEnd = endCal.getTime();

            // Only concerned with month and day, need to set the years equal
            Calendar c = TimeUtil.newGmtCalendar();
            c.setTime(checkDate);
            c.set(Calendar.YEAR, startCal.get(Calendar.YEAR));
            Date date = c.getTime();

            return (activePeriodStart.before(date) && activePeriodEnd
                    .after(date));
        }
        return false;
    }

    @Override
    public Network getRoute() {
        return this.route;
    }

    @Override
    public void setRoute(Network route) {
        this.route = route;
    }

    /**
     * Set the latency in minutes.
     * 
     * @param latencyInMinutes
     *            the latency, in minutes
     * 
     */
    @Override
    public void setLatencyInMinutes(int latencyInMinutes) {
        this.latencyInMinutes = latencyInMinutes;
    }

    /**
     * Get the latency, in minutes.
     * 
     * @return the latency in minutes
     */
    @Override
    public int getLatencyInMinutes() {
        return latencyInMinutes;
    }

    @Override
    public Ensemble getEnsemble() {
        return ensemble;
    }

    @Override
    public void setEnsemble(Ensemble ensemble) {
        this.ensemble = ensemble;
    }
}
