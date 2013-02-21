/**
 *
 */
package com.raytheon.uf.common.datadelivery.registry;

import java.util.Date;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.bind.annotation.adapters.XmlJavaTypeAdapter;

import org.geotools.geometry.jts.ReferencedEnvelope;

import com.raytheon.uf.common.registry.annotations.RegistryObject;
import com.raytheon.uf.common.registry.annotations.SlotAttribute;
import com.raytheon.uf.common.serialization.adapters.ReferencedEnvelopeAdapter;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;

/**
 * Subscription Group Definition.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jul 10, 2012            jpiatt     Initial creation.
 * Oct 24, 2012   1290     mpduff     Added isArealDataSet method.
 * Nov 19, 2012 1166       djohnson   Clean up JAXB representation of registry objects.
 * Dec 10, 2012   1259     bsteffen   Switch Data Delivery from LatLon to referenced envelopes.
 * Jan 02, 2013 1441       djohnson   Add constants.
 * 
 * </pre>
 * 
 * @author jpiatt
 * @version 1.0
 */
@XmlRootElement
@XmlAccessorType(XmlAccessType.NONE)
@DynamicSerialize
@RegistryObject({ GroupDefinition.GROUP_NAME_SLOT })
public class GroupDefinition {

    public static final String GROUP_NAME_SLOT = "groupName";

    /** Constant representing no group */
    public static final String NO_GROUP = "None";

    /**
     * Constructor.
     */
    public GroupDefinition() {

    }

    @XmlAttribute
    @DynamicSerializeElement
    @SlotAttribute
    protected String groupName;

    @XmlAttribute
    @DynamicSerializeElement
    protected Integer option;

    @XmlAttribute
    @DynamicSerializeElement
    @SlotAttribute
    protected String owner;

    @XmlAttribute
    @DynamicSerializeElement
    protected Date subscriptionStart;

    @XmlAttribute
    @DynamicSerializeElement
    protected Date subscriptionEnd;

    @XmlAttribute
    @DynamicSerializeElement
    protected Date activePeriodStart;

    @XmlAttribute
    @DynamicSerializeElement
    protected Date activePeriodEnd;

    @XmlElement
    @DynamicSerializeElement
    @XmlJavaTypeAdapter(value = ReferencedEnvelopeAdapter.class)
    protected ReferencedEnvelope envelope;

    /**
     * Get the group name value.
     *
     * @return group name
     */
    public String getGroupName() {
        return groupName;
    }

    /**
     * Set the group name value.
     *
     * @param groupName
     */
    public void setGroupName(String groupName) {
        this.groupName = groupName;
    }

    /**
     * Get the group owner.
     *
     * @return group owner
     */
    public String getOwner() {
        return owner;
    }

    /**
     * Set the group owner.
     *
     * @param owner
     */
    public void setOwner(String owner) {
        this.owner = owner;
    }

    /**
     * Get the group subscription start date.
     *
     * @return group subscription start date
     */
    public Date getSubscriptionStart() {
        return subscriptionStart;
    }

    /**
     * Set the group subscription start date.
     *
     * @param subscriptionStart
     */
    public void setSubscriptionStart(Date subscriptionStart) {
        this.subscriptionStart = subscriptionStart;
    }

    /**
     * Get the group subscription end date.
     *
     * @return group subscription end date
     */
    public Date getSubscriptionEnd() {
        return subscriptionEnd;
    }

    /**
     * Set the group subscription end date.
     *
     * @param subscriptionEnd
     */
    public void setSubscriptionEnd(Date subscriptionEnd) {
        this.subscriptionEnd = subscriptionEnd;
    }

    /**
     * Get the group active period start date.
     *
     * @return group active period start date
     */
    public Date getActivePeriodStart() {
        return activePeriodStart;
    }

    /**
     * Set the group subscription active period start date.
     *
     * @param activePeriodStart
     */
    public void setActivePeriodStart(Date activePeriodStart) {
        this.activePeriodStart = activePeriodStart;
    }

    /**
     * Get the group active end start date.
     *
     * @return active period end date
     */
    public Date getActivePeriodEnd() {
        return activePeriodEnd;
    }

    /**
     * Set the group subscription active period end date.
     *
     * @param activePeriodEnd
     */
    public void setActivePeriodEnd(Date activePeriodEnd) {
        this.activePeriodEnd = activePeriodEnd;
    }

    /**
     * @return the envelope
     */
    public ReferencedEnvelope getEnvelope() {
        return envelope;
    }

    /**
     * @param envelope
     *            the envelope to set
     */
    public void setEnvelope(ReferencedEnvelope envelope) {
        this.envelope = envelope;
    }

    /**
     * Get the group delivery option
     *
     * @return option
     */
    public Integer getOption() {
        return option;
    }

    /**
     * Set the group delivery option.
     *
     * @param option
     */
    public void setOption(Integer option) {
        this.option = option;
    }

    /**
     * Check if this group has areal data associated with it.
     *
     * @return true if areal data are set
     */
    public boolean isArealDataSet() {
        return envelope != null;
    }
}
