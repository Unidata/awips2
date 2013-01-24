package com.raytheon.uf.common.datadelivery.registry;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.Calendar;
import java.util.Date;
import java.util.TimeZone;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlElements;
import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.bind.annotation.XmlSeeAlso;

import org.apache.commons.lang.builder.EqualsBuilder;
import org.apache.commons.lang.builder.HashCodeBuilder;

import com.raytheon.uf.common.datadelivery.registry.Utils.SubscriptionStatus;
import com.raytheon.uf.common.registry.annotations.RegistryObject;
import com.raytheon.uf.common.registry.annotations.SlotAttribute;
import com.raytheon.uf.common.registry.annotations.SlotAttributeConverter;
import com.raytheon.uf.common.registry.ebxml.RegistryUtil;
import com.raytheon.uf.common.serialization.ISerializableObject;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;
import com.raytheon.uf.common.time.util.TimeUtil;

/**
 * Subscription XML
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jan 07, 2011    191      dhladky     Initial creation.
 * Mar 13, 2012             jpiatt      Modified for additional elements.
 * Jul  2, 2012    702      jpiatt      Added group name.
 * Aug 10, 2012   1002      mpduff      Change dataset size from int to long.
 * Aug 22, 2012   0743      djohnson    Store data type, fix copy constructor.
 * Aug 31, 2012   1128      mpduff      Fixed subscription status indication.
 * Sep 07, 2012   1102      djohnson    Add some JAXB XmlSeeAlso values.
 * Oct  1, 2012   1103      jpiatt      Added invalid subscription status.
 * Oct 10, 2012   0726      djohnson    Add network route.
 * Oct 26, 2012   1286      djohnson    Add toString, equals, hashcode.
 * Nov 19, 2012 1166        djohnson    Clean up JAXB representation of registry objects.
 * Nov 20, 2012 1166        djohnson    Use attributes for Subscription fields.
 * Nov 20, 2012 1286        djohnson    Add unscheduled.
 * Dec 12, 2012 1433        bgonzale    Refactored Subscription copy ctor into two ctors.
 * Jan 03, 2013 1441        djohnson    Default to no group.
 * 
 * </pre>
 * 
 * @author dhladky
 * @version 1.0
 * @param <RegistryTypeObject>
 */

@XmlRootElement
@XmlAccessorType(XmlAccessType.NONE)
@DynamicSerialize
@XmlSeeAlso({ PendingSubscription.class, AdhocSubscription.class })
@RegistryObject({ Subscription.PROVIDER_NAME_SLOT, Subscription.NAME_SLOT,
    Subscription.DATA_SET_SLOT, Subscription.OWNER_SLOT })
public class Subscription implements ISerializableObject, Serializable {

    private static final long serialVersionUID = -6422673887457060034L;

    /** Dataset Name slot */
    public static final String DATA_SET_SLOT = "dataSetName";

    /** Provider slot */
    public static final String PROVIDER_NAME_SLOT = "provider";

    /** Name slot */
    public static final String NAME_SLOT = "name";

    /** Owner slot */
    public static final String OWNER_SLOT = "owner";

    /**
     * Constructor.
     */
    public Subscription() {

    }

    /**
     * Initialization constructor.
     * 
     * @param sub
     *            Subscription object
     * @param name
     *            New subscription name
     */
    public Subscription(Subscription sub, String name) {
        this(sub);
        this.setName(name);
        this.setId(RegistryUtil.getRegistryObjectKey(this));
    }

    /**
     * Copy constructor.
     * 
     * @param sub
     *            Subscription object
     */
    public Subscription(Subscription sub) {
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
        this.setName(sub.getName());
        this.setNotify(sub.isNotify());
        this.setOfficeID(sub.getOfficeID());
        this.setOwner(sub.getOwner());
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
    @SlotAttribute(OWNER_SLOT)
    private String owner;

    @XmlAttribute
    @DynamicSerializeElement
    @SlotAttribute(PROVIDER_NAME_SLOT)
    private String provider;

    @XmlAttribute
    @DynamicSerializeElement
    @SlotAttribute
    private String officeID;

    @XmlAttribute
    @DynamicSerializeElement
    private Integer priority;

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
    private boolean notify;

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

    @XmlElements({ @XmlElement})
    @DynamicSerializeElement
    private ArrayList<Parameter> parameter;

    @XmlAttribute
    @DynamicSerializeElement
    private boolean deleted;

    @XmlAttribute
    @DynamicSerializeElement
    private String id;

    @XmlAttribute
    @DynamicSerializeElement
    private Network route = Network.OPSNET;

    @XmlAttribute
    @DynamicSerializeElement
    private int latencyInMinutes;

    /**
     * Get subscription name.
     * 
     * @return subscription name
     */
    public String getName() {
        return name;
    }

    /**
     * Set subscription name.
     * 
     * @param name
     *           the name of the subscription
     */
    public void setName(String name) {
        this.name = name;
    }

    /**
     * Get subscription group name.
     * 
     * @return subscription group name
     */
    public String getGroupName() {
        return groupName;
    }

    /**
     * Set subscription group name.
     * 
     * @param groupName
     *            the name of the subscription group
     */
    public void setGroupName(String groupName) {
        this.groupName = groupName;
    }

    /**
     * Set subscription provider name.
     * 
     * @param provider
     *           the name of the subscription provider
     */
    public void setProvider(String provider) {
        this.provider = provider;
    }

    /**
     * Get provider name.
     * 
     * @return provider name
     */
    public String getProvider() {
        return provider;
    }

    /**
     * Get subscription owner name.
     * 
     * @return subscription owner name
     */
    public String getOwner() {
        return owner;
    }

    /**
     * Set subscription owner name.
     * 
     * @param owner
     *           the name of the subscription owner
     */
    public void setOwner(String owner) {
        this.owner = owner;
    }

    /**
     * Get owner office id.
     * 
     * @return office id
     */
    public String getOfficeID() {
        return officeID;
    }

    /**
     * Set office id.
     * 
     * @param officeID
     *           the office id
     */
    public void setOfficeID(String officeID) {
        this.officeID = officeID;
    }

    /**
     * Get subscription priority for fulfillment.
     * 
     * @return subscription name
     */
    public Integer getPriority() {
        return priority;
    }

    /**
     * Set subscription priority.
     * 
     * @param priority
     *           priority
     */
    public void setPriority(Integer priority) {
        this.priority = priority;
    }

    /**
     * Get subscription start time.
     * 
     * @return subscription start
     */
    public Date getSubscriptionStart() {
        return subscriptionStart;
    }

    /**
     * Set subscription start time.
     * 
     * @param subscriptionStart
     *           date time group for subscription start
     */
    public void setSubscriptionStart(Date subscriptionStart) {
        this.subscriptionStart = subscriptionStart;
    }

    /**
     * Get subscription end time.
     * 
     * @return subscription end time
     *         date time group for subscription end
     */          
    public Date getSubscriptionEnd() {
        return subscriptionEnd;
    }

    /**
     * Set subscription end time.
     * 
     * @param subscriptionEnd
     *           date time group for subscription end
     */
    public void setSubscriptionEnd(Date subscriptionEnd) {
        this.subscriptionEnd = subscriptionEnd;
    }

    /**
     * Get active period start date.
     * 
     * @return activePeriodStart
     */
    public Date getActivePeriodStart() {
        return activePeriodStart;
    }

    /**
     * Set active period start date.
     * 
     * @param activePeriodStart
     *           date for subscription start
     */
    public void setActivePeriodStart(Date activePeriodStart) {
        this.activePeriodStart = activePeriodStart;
    }

    /**
     * Get active period end date.
     * 
     * @return activePeriodEnd
     */
    public Date getActivePeriodEnd() {
        return activePeriodEnd;
    }

    /**
     * Set active period end date.
     * 
     * @param activePeriodEnd
     *           date for subscription end
     */
    public void setActivePeriodEnd(Date activePeriodEnd) {
        this.activePeriodEnd = activePeriodEnd;
    }

    /**
     * isNotify flag for subscription.
     * 
     * @return boolean
     *             true if notify subscriber 
     *             false if deliver to subscriber
     */
    public boolean isNotify() {
        return notify;
    }

    /**
     * Set isNotify flag.
     * 
     * @param notify
     *           date for subscription end
     */
    public void setNotify(boolean notify) {
        this.notify = notify;
    }

    /**
     * isNotify flag for subscription.
     * 
     * @return boolean
     *             true if full dataset
     */
    public boolean isFullDataSet() {
        return fullDataSet;
    }

    /**
     * Set fullDataSet flag.
     * 
     * @param fullDataSet
     *           true if full dataset 
     */
    public void setFullDataSet(boolean fullDataSet) {
        this.fullDataSet = fullDataSet;
    }

    /**
     * Get size of the dataset for the subscription.
     * 
     * @return dataSetSize
     *           size of dataset
     */
    public long getDataSetSize() {
        return dataSetSize;
    }

    /**
     * Set the dataset size for the subscription.
     * 
     * @param dataSetSize
     *           size of dataset
     */
    public void setDataSetSize(long dataSetSize) {
        this.dataSetSize = dataSetSize;
    }

    /**
     * Get subscription coverage area.
     * 
     * @return coverage
     */
    public Coverage getCoverage() {
        return coverage;
    }

    /**
     * Set the coverage area for the subscription.
     * 
     * @param coverage
     *           coverage area
     */
    public void setCoverage(Coverage coverage) {
        this.coverage = coverage;
    }

    /**
     * Get subscription submission time.
     * 
     * @return subscription time
     */
    public Time getTime() {
        return time;
    }

    /**
     * Set the subscription submission time.
     * 
     * @param time
     *           time stamp
     */
    public void setTime(Time time) {
        this.time = time;
    }

    /**
     * Set the subscription parameters.
     * 
     * @param parameter
     *           subscription parameter list
     */
    public void setParameter(ArrayList<Parameter> parameter) {
        this.parameter = parameter;
    }

    /**
     * Get subscription parameter list.
     * 
     * @return subscription parameter list
     */
    public ArrayList<Parameter> getParameter() {
        return parameter;
    }

    /**
     * Add subscription parameters.
     * 
     * @param par
     *           a subscription parameter
     */
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
     *           a subscription parameter
     */
    public void removeParameter(Parameter par) {
        parameter.remove(par);
    }

    /**
     * Add subscription id.
     * 
     * @param subscriptionId
     *           a subscription id
     */
    public void setSubscriptionId(String subscriptionId) {
        this.subscriptionId = subscriptionId;
    }

    /**
     * Get subscription id.
     * 
     * @return subscription id
     */
    public String getSubscriptionId() {
        return subscriptionId;
    }

    /**
     * Get subscription description.
     * 
     * @return subscription description
     */
    public String getDescription() {
        return description;
    }

    /**
     * Set the subscription description.
     * 
     * @param description
     *           subscription description
     */
    public void setDescription(String description) {
        this.description = description;
    }

    /**
     * Get subscription dataset name.
     * 
     * @return subscription dataset name
     */
    public String getDataSetName() {
        return dataSetName;
    }

    /**
     * Set the subscription dataSetName.
     * 
     * @param dataSetName
     *           subscription dataSetName
     */
    public void setDataSetName(String dataSetName) {
        this.dataSetName = dataSetName;
    }

    /**
     * isActive flag for subscription status.
     * 
     * @return boolean
     *             true if subscription is Active
     */
    public boolean isActive() {
        return active;
    }

    /**
     * Set the subscription status to active.
     * 
     * @param active
     *           subscription active
     */
    public void setActive(boolean active) {
        this.active = active;
    }

    /**
     * Set subscription valid.
     * 
     * @param valid
     *            true if subscription valid
     */
    public void setValid(boolean valid) {
        this.valid = valid;
    }

    /**
     * Return if subscription is valid or invalid
     * 
     * @return true if subscription is valid
     */
    public boolean isValid() {
        return valid;
    }

    /**
     * Get the subscription url.
     * 
     * @return the url
     */
    public String getUrl() {
        return url;
    }

    /**
     * Set the subscription url.
     * 
     * @param url
     *            the url to set
     */
    public void setUrl(String url) {
        this.url = url;
    }

    /**
     * Get subscription dataset type.
     * 
     * @return subscription dataset type
     */
    public DataType getDataSetType() {
        return dataSetType;
    }

    /**
     * Set the dataset type
     * 
     * @param dataSetType
     *            the dataSetType to set
     */
    public void setDataSetType(DataType dataSetType) {
        this.dataSetType = dataSetType;
    }

    /**
     * isDeleted flag.
     * 
     * @return true if the subscription has been deleted
     */
    public boolean isDeleted() {
        return deleted;
    }

    /**
     * Set the deleted flag.
     * 
     * @param deleted
     *           set subscription to deleted
     */
    public void setDeleted(boolean deleted) {
        this.deleted = deleted;
    }

    /**
     * @return the unscheduled
     */
    public boolean isUnscheduled() {
        return unscheduled;
    }

    /**
     * @param unscheduled
     *            the unscheduled to set
     */
    public void setUnscheduled(boolean unscheduled) {
        this.unscheduled = unscheduled;
    }

    /**
     * Get subscription id.
     * 
     * @return subscription id
     */
    public String getId() {
        return id;
    }

    /**
     * Set the subscription id.
     * 
     * @param id
     *           set subscription id
     */
    public void setId(String id) {
        this.id = id;
    }

    @Override
    public boolean equals(Object obj) {
        if (obj instanceof Subscription) {
            Subscription other = (Subscription) obj;

            EqualsBuilder builder = new EqualsBuilder();
            builder.append(provider, other.provider);
            builder.append(name, other.name);
            builder.append(dataSetName, other.dataSetName);
            builder.append(owner, other.owner);

            return builder.isEquals();
        }
        return super.equals(obj);
    }

    @Override
    public int hashCode() {
        HashCodeBuilder builder = new HashCodeBuilder();
        builder.append(provider);
        builder.append(name);
        builder.append(dataSetName);
        builder.append(owner);

        return builder.toHashCode();
    }

    @Override
    public String toString() {
        return name + "::" + provider + "::" + dataSetName + "::" + owner;
    }

    /**
     * Determine if subscription status is expired.
     * 
     * @return true if status is expired
     */
    public boolean isExpired() {
        Calendar cal = Calendar.getInstance(TimeZone.getTimeZone("GMT"));
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

                status = (inWindow(today)) ? SubscriptionStatus.ACTIVE : SubscriptionStatus.INACTIVE;

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
                    Calendar.HOUR_OF_DAY,
                    Calendar.MINUTE, Calendar.SECOND, Calendar.MILLISECOND);
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

    public Network getRoute() {
        return this.route;
    }

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
    public void setLatencyInMinutes(int latencyInMinutes) {
        this.latencyInMinutes = latencyInMinutes;
    }

    /**
     * Get the latency, in minutes.
     * 
     * @return the latency in minutes
     */
    public int getLatencyInMinutes() {
        return latencyInMinutes;
    }
}
