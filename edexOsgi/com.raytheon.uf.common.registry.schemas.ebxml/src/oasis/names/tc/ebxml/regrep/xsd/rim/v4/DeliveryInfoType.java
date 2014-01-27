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

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.Table;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.bind.annotation.XmlType;
import javax.xml.ws.wsaddressing.W3CEndpointReference;

import org.hibernate.annotations.Cache;
import org.hibernate.annotations.CacheConcurrencyStrategy;
import org.hibernate.annotations.Type;

import com.raytheon.uf.common.registry.RegrepUtil;
import com.raytheon.uf.common.registry.schemas.ebxml.util.annotations.RegistryObjectReference;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;

/**
 * 
 * Contains the information needed by the server to deliver notifications for
 * the subscription. It includes the reference to the endpoint where
 * notifications should be delivered.
 * 
 * 
 * <p>
 * Java class for DeliveryInfoType complex type.
 * 
 * <p>
 * The following schema fragment specifies the expected content contained within
 * this class.
 * 
 * <pre>
 * &lt;complexType name="DeliveryInfoType">
 *   &lt;complexContent>
 *     &lt;extension base="{urn:oasis:names:tc:ebxml-regrep:xsd:rim:4.0}ExtensibleObjectType">
 *       &lt;sequence>
 *         &lt;element name="NotifyTo" type="{http://www.w3.org/2005/08/addressing}EndpointReferenceType"/>
 *       &lt;/sequence>
 *       &lt;attribute name="notificationOption" type="{urn:oasis:names:tc:ebxml-regrep:xsd:rim:4.0}objectReferenceType" default="urn:oasis:names:tc:ebxml-regrep:NotificationOptionType:ObjectRefs" />
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
 * 12/2/2013     1829       bphillip    Made ExtensibleObjectType persistable, modified persistence annotations, added hashCode and equals
 * </pre>
 * 
 * @author bphillip
 * @version 1
 */
@XmlRootElement(name = "DeliveryInfo")
@XmlAccessorType(XmlAccessType.FIELD)
@XmlType(name = "DeliveryInfoType")
@DynamicSerialize
@Entity
@Cache(region = RegrepUtil.DB_CACHE_REGION, usage = CacheConcurrencyStrategy.TRANSACTIONAL)
@Table(schema = RegrepUtil.EBXML_SCHEMA, name = "DeliveryInfo")
public class DeliveryInfoType extends ExtensibleObjectType {

    private static final long serialVersionUID = -5102364923190271786L;

    // TODO: Revisit how to handle serialization of this field
    // @DynamicSerializeElement
    @XmlElement(name = "NotifyTo", required = true)
    @Column(name = "notifyTo", columnDefinition = "text")
    @Type(type = "com.raytheon.uf.common.registry.schemas.ebxml.util.SerializedType")
    protected W3CEndpointReference notifyTo;

    @XmlAttribute
    @DynamicSerializeElement
    @RegistryObjectReference
    protected String notificationOption;

    public DeliveryInfoType() {
        super();
    }

    /**
     * Gets the value of the notifyTo property.
     * 
     * @return possible object is {@link W3CEndpointReference }
     * 
     */
    public W3CEndpointReference getNotifyTo() {
        return notifyTo;
    }

    /**
     * Sets the value of the notifyTo property.
     * 
     * @param value
     *            allowed object is {@link W3CEndpointReference }
     * 
     */
    public void setNotifyTo(W3CEndpointReference value) {
        this.notifyTo = value;
    }

    /**
     * Gets the value of the notificationOption property.
     * 
     * @return possible object is {@link String }
     * 
     */
    public String getNotificationOption() {
        if (notificationOption == null) {
            return "urn:oasis:names:tc:ebxml-regrep:NotificationOptionType:ObjectRefs";
        } else {
            return notificationOption;
        }
    }

    /**
     * Sets the value of the notificationOption property.
     * 
     * @param value
     *            allowed object is {@link String }
     * 
     */
    public void setNotificationOption(String value) {
        this.notificationOption = value;
    }

    @Override
    public int hashCode() {
        final int prime = 31;
        int result = super.hashCode();
        result = prime
                * result
                + ((notificationOption == null) ? 0 : notificationOption
                        .hashCode());
        result = prime * result
                + ((notifyTo == null) ? 0 : notifyTo.hashCode());
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
        DeliveryInfoType other = (DeliveryInfoType) obj;
        if (notificationOption == null) {
            if (other.notificationOption != null)
                return false;
        } else if (!notificationOption.equals(other.notificationOption))
            return false;
        if (notifyTo == null) {
            if (other.notifyTo != null)
                return false;
        } else if (!notifyTo.equals(other.notifyTo))
            return false;
        return true;
    }

    @Override
    public String toString() {
        StringBuilder builder = new StringBuilder();
        builder.append("DeliveryInfoType \n[id=");
        builder.append(id);
        builder.append(", \nslot=");
        builder.append(slot);
        builder.append(", \nnotifyTo=");
        builder.append(notifyTo);
        builder.append(", \nnotificationOption=");
        builder.append(notificationOption);
        builder.append("]");
        return builder.toString();
    }

}
