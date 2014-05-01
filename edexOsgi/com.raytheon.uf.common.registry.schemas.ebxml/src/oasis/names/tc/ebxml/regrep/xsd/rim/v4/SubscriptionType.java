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

package oasis.names.tc.ebxml.regrep.xsd.rim.v4;

import java.util.ArrayList;
import java.util.List;

import javax.persistence.CascadeType;
import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.JoinTable;
import javax.persistence.OneToMany;
import javax.persistence.OneToOne;
import javax.persistence.Table;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.bind.annotation.XmlSchemaType;
import javax.xml.bind.annotation.XmlType;
import javax.xml.datatype.Duration;
import javax.xml.datatype.XMLGregorianCalendar;

import org.hibernate.annotations.Cache;
import org.hibernate.annotations.CacheConcurrencyStrategy;
import org.hibernate.annotations.Type;

import com.raytheon.uf.common.registry.RegrepUtil;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;

/**
 * Represents a Subscription for specified server Events.
 * 
 * <p>
 * Java class for SubscriptionType complex type.
 * 
 * <p>
 * The following schema fragment specifies the expected content contained within
 * this class.
 * 
 * <pre>
 * &lt;complexType name="SubscriptionType">
 *   &lt;complexContent>
 *     &lt;extension base="{urn:oasis:names:tc:ebxml-regrep:xsd:rim:4.0}RegistryObjectType">
 *       &lt;sequence>
 *         &lt;element name="DeliveryInfo" type="{urn:oasis:names:tc:ebxml-regrep:xsd:rim:4.0}DeliveryInfoType" maxOccurs="unbounded" minOccurs="0"/>
 *         &lt;element name="Selector" type="{urn:oasis:names:tc:ebxml-regrep:xsd:rim:4.0}QueryType"/>
 *       &lt;/sequence>
 *       &lt;attribute name="startTime" type="{http://www.w3.org/2001/XMLSchema}dateTime" />
 *       &lt;attribute name="endTime" type="{http://www.w3.org/2001/XMLSchema}dateTime" />
 *       &lt;attribute name="notificationInterval" type="{http://www.w3.org/2001/XMLSchema}duration" />
 *     &lt;/extension>
 *   &lt;/complexContent>
 * &lt;/complexType>
 * </pre>
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#     Engineer    Description
 * ------------ ----------  ----------- --------------------------
 * 2012                     bphillip    Initial implementation
 * 10/17/2013    1682       bphillip    Added software history
 * 12/2/2013     1829       bphillip    Made ExtensibleObjectType persistable, 
 *                                      modified persistence annotations, added 
 *                                      constructors, hashCode, toString and equals
 * </pre>
 * 
 * @author bphillip
 * @version 1
 */
@XmlRootElement(name = "Subscription")
@XmlAccessorType(XmlAccessType.FIELD)
@XmlType(name = "SubscriptionType", propOrder = { "deliveryInfo", "selector" })
@DynamicSerialize
@Entity
@Cache(region = RegrepUtil.DB_CACHE_REGION, usage = CacheConcurrencyStrategy.TRANSACTIONAL)
@Table(schema = RegrepUtil.EBXML_SCHEMA, name = "Subscription")
public class SubscriptionType extends RegistryObjectType {

    private static final long serialVersionUID = -4081551671995600532L;

    @OneToMany(cascade = CascadeType.ALL)
    @XmlElement(name = "DeliveryInfo")
    @DynamicSerializeElement
    @JoinTable(schema = RegrepUtil.EBXML_SCHEMA)
    protected List<DeliveryInfoType> deliveryInfo;

    @OneToOne(cascade = CascadeType.ALL)
    @XmlElement(name = "Selector", required = true)
    @DynamicSerializeElement
    protected QueryType selector;

    @Column
    @Type(type = "com.raytheon.uf.common.registry.schemas.ebxml.util.XMLGregorianCalendarType")
    @XmlAttribute
    @XmlSchemaType(name = "dateTime")
    @DynamicSerializeElement
    protected XMLGregorianCalendar startTime;

    @Column
    @Type(type = "com.raytheon.uf.common.registry.schemas.ebxml.util.XMLGregorianCalendarType")
    @XmlAttribute
    @XmlSchemaType(name = "dateTime")
    @DynamicSerializeElement
    protected XMLGregorianCalendar endTime;

    @XmlAttribute
    @DynamicSerializeElement
    @Type(type = "com.raytheon.uf.common.registry.schemas.ebxml.util.DurationType")
    protected Duration notificationInterval;

    public SubscriptionType() {
        super();

    }

    public SubscriptionType(String id, String lid, String objectType,
            String owner, String status, String name, String description) {
        super(id, lid, objectType, owner, status, name, description);

    }

    public SubscriptionType(String id, String lid) {
        super(id, lid);

    }

    /**
     * Gets the value of the deliveryInfo property.
     * 
     * <p>
     * This accessor method returns a reference to the live list, not a
     * snapshot. Therefore any modification you make to the returned list will
     * be present inside the JAXB object. This is why there is not a
     * <CODE>set</CODE> method for the deliveryInfo property.
     * 
     * <p>
     * For example, to add a new item, do as follows:
     * 
     * <pre>
     * getDeliveryInfo().add(newItem);
     * </pre>
     * 
     * 
     * <p>
     * Objects of the following type(s) are allowed in the list
     * {@link DeliveryInfoType }
     * 
     * 
     */
    public List<DeliveryInfoType> getDeliveryInfo() {
        if (deliveryInfo == null) {
            deliveryInfo = new ArrayList<DeliveryInfoType>();
        }
        return this.deliveryInfo;
    }

    public void setDeliveryInfo(List<DeliveryInfoType> deliveryInfo) {
        this.deliveryInfo = deliveryInfo;
    }

    /**
     * Gets the value of the selector property.
     * 
     * @return possible object is {@link QueryType }
     * 
     */
    public QueryType getSelector() {
        return selector;
    }

    /**
     * Sets the value of the selector property.
     * 
     * @param value
     *            allowed object is {@link QueryType }
     * 
     */
    public void setSelector(QueryType value) {
        this.selector = value;
    }

    /**
     * Gets the value of the startTime property.
     * 
     * @return possible object is {@link XMLGregorianCalendar }
     * 
     */
    public XMLGregorianCalendar getStartTime() {
        return startTime;
    }

    /**
     * Sets the value of the startTime property.
     * 
     * @param value
     *            allowed object is {@link XMLGregorianCalendar }
     * 
     */
    public void setStartTime(XMLGregorianCalendar value) {
        this.startTime = value;
    }

    /**
     * Gets the value of the endTime property.
     * 
     * @return possible object is {@link XMLGregorianCalendar }
     * 
     */
    public XMLGregorianCalendar getEndTime() {
        return endTime;
    }

    /**
     * Sets the value of the endTime property.
     * 
     * @param value
     *            allowed object is {@link XMLGregorianCalendar }
     * 
     */
    public void setEndTime(XMLGregorianCalendar value) {
        this.endTime = value;
    }

    /**
     * Gets the value of the notificationInterval property.
     * 
     * @return possible object is {@link Duration }
     * 
     */
    public Duration getNotificationInterval() {
        return notificationInterval;
    }

    /**
     * Sets the value of the notificationInterval property.
     * 
     * @param value
     *            allowed object is {@link Duration }
     * 
     */
    public void setNotificationInterval(Duration value) {
        this.notificationInterval = value;
    }

    @Override
    public int hashCode() {
        final int prime = 31;
        int result = super.hashCode();
        result = prime * result
                + ((deliveryInfo == null) ? 0 : deliveryInfo.hashCode());
        result = prime * result + ((endTime == null) ? 0 : endTime.hashCode());
        result = prime
                * result
                + ((notificationInterval == null) ? 0 : notificationInterval
                        .hashCode());
        result = prime * result
                + ((selector == null) ? 0 : selector.hashCode());
        result = prime * result
                + ((startTime == null) ? 0 : startTime.hashCode());
        return result;
    }

    @Override
    public boolean equals(Object obj) {
        if (this == obj)
            return true;
        if (!super.equals(obj))
            return false;
        if (getClass() != obj.getClass())
            return false;
        SubscriptionType other = (SubscriptionType) obj;
        if (deliveryInfo == null) {
            if (other.deliveryInfo != null)
                return false;
        } else if (!deliveryInfo.equals(other.deliveryInfo))
            return false;
        if (endTime == null) {
            if (other.endTime != null)
                return false;
        } else if (!endTime.equals(other.endTime))
            return false;
        if (notificationInterval == null) {
            if (other.notificationInterval != null)
                return false;
        } else if (!notificationInterval.equals(other.notificationInterval))
            return false;
        if (selector == null) {
            if (other.selector != null)
                return false;
        } else if (!selector.equals(other.selector))
            return false;
        if (startTime == null) {
            if (other.startTime != null)
                return false;
        } else if (!startTime.equals(other.startTime))
            return false;
        return true;
    }

    @Override
    public String toString() {
        StringBuilder builder = new StringBuilder();
        builder.append("SubscriptionType \n[name=");
        builder.append(name);
        builder.append(", \ndescription=");
        builder.append(description);
        builder.append(", \nversionInfo=");
        builder.append(versionInfo);
        builder.append(", \nclassification=");
        builder.append(classification);
        builder.append(", \nexternalIdentifier=");
        builder.append(externalIdentifier);
        builder.append(", \nexternalLink=");
        builder.append(externalLink);
        builder.append(", \nlid=");
        builder.append(lid);
        builder.append(", \nobjectType=");
        builder.append(objectType);
        builder.append(", \nowner=");
        builder.append(owner);
        builder.append(", \nstatus=");
        builder.append(status);
        builder.append(", \nid=");
        builder.append(id);
        builder.append(", \nslot=");
        builder.append(slot);
        builder.append(", \ndeliveryInfo=");
        builder.append(deliveryInfo);
        builder.append(", \nselector=");
        builder.append(selector);
        builder.append(", \nstartTime=");
        builder.append(startTime);
        builder.append(", \nendTime=");
        builder.append(endTime);
        builder.append(", \nnotificationInterval=");
        builder.append(notificationInterval);
        builder.append("]");
        return builder.toString();
    }

}
