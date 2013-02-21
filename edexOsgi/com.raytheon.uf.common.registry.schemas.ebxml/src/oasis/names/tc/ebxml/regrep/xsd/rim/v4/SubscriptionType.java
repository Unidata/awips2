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
import javax.persistence.ManyToMany;
import javax.persistence.OneToOne;
import javax.persistence.Table;
import javax.persistence.Transient;
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
 * 
 */
@XmlRootElement
@XmlAccessorType(XmlAccessType.FIELD)
@XmlType(name = "SubscriptionType", propOrder = { "deliveryInfo", "selector" })
@DynamicSerialize
@Entity
@Cache(region="registryObjects",usage = CacheConcurrencyStrategy.TRANSACTIONAL)
@Table(name = "Subscription")
public class SubscriptionType extends RegistryObjectType {

    @ManyToMany(cascade = CascadeType.ALL)
    @XmlElement(name = "DeliveryInfo")
    @DynamicSerializeElement
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

    @Transient
    @XmlAttribute
    @DynamicSerializeElement
    protected Duration notificationInterval;

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

}
