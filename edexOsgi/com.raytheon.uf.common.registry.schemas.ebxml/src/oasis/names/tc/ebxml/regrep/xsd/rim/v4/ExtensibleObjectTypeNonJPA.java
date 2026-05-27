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

import java.io.Serializable;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.UUID;

import jakarta.persistence.Id;
import jakarta.xml.bind.annotation.XmlAccessType;
import jakarta.xml.bind.annotation.XmlAccessorType;
import jakarta.xml.bind.annotation.XmlElement;
import jakarta.xml.bind.annotation.XmlRootElement;
import jakarta.xml.bind.annotation.XmlSeeAlso;
import jakarta.xml.bind.annotation.XmlTransient;
import jakarta.xml.bind.annotation.XmlType;

import org.hibernate.annotations.BatchSize;

import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;

import oasis.names.tc.ebxml.regrep.xsd.rs.v4.RegistryExceptionType;
import oasis.names.tc.ebxml.regrep.xsd.rs.v4.RegistryRequestType;
import oasis.names.tc.ebxml.regrep.xsd.rs.v4.RegistryResponseType;

/**
 *
 * Common base type for all types need to support extensibility via slots.
 *
 *
 * <p>
 * Java class for ExtensibleObjectType complex type.
 *
 * <p>
 * The following schema fragment specifies the expected content contained within
 * this class.
 *
 * <pre>
 * &lt;complexType name="ExtensibleObjectType">
 *   &lt;complexContent>
 *     &lt;restriction base="{http://www.w3.org/2001/XMLSchema}anyType">
 *       &lt;sequence>
 *         &lt;element name="Slot" type="{urn:oasis:names:tc:ebxml-regrep:xsd:rim:4.0}SlotType" maxOccurs="unbounded" minOccurs="0"/>
 *       &lt;/sequence>
 *     &lt;/restriction>
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
 * 10/27/2020    8170       ksunil    Removed empty tables. New class to support non JPA activity.
 * </pre>
 *
 * @author ksunil
 */
@XmlRootElement(name = "ExtensibleObject")
@XmlAccessorType(XmlAccessType.NONE)
@XmlType(name = "ExtensibleObjectTypeNonJPA", propOrder = { "slot" })
@XmlSeeAlso({ PostalAddressType.class, TelephoneNumberType.class,
        ParameterType.class, QueryType.class, DeliveryInfoType.class,
        PersonNameType.class, ObjectRefType.class, SlotType.class,
        IdentifiableType.class, EmailAddressType.class,
        QueryExpressionType.class, RegistryExceptionType.class,
        RegistryResponseType.class, RegistryRequestType.class })
@DynamicSerialize

public abstract class ExtensibleObjectTypeNonJPA implements Serializable {

    private static final long serialVersionUID = 785780260533569469L;

    @Id
    @DynamicSerializeElement
    @XmlTransient
    protected String id;

    @XmlElement(name = "Slot")
    @BatchSize(size = 50)
    @DynamicSerializeElement
    protected List<SlotType> slot;

    public ExtensibleObjectTypeNonJPA() {
        super();
        this.id = UUID.randomUUID().toString();
    }

    public ExtensibleObjectTypeNonJPA(String id) {
        this.id = id;
    }

    public ExtensibleObjectTypeNonJPA(String id, List<SlotType> slot) {
        super();
        this.id = id;
        this.slot = slot;
    }

    public SlotType getSlotByName(String slotName) {
        for (SlotType slot : getSlot()) {
            if (slot.getName() != null && slot.getName().equals(slotName)) {
                return slot;
            }
        }
        return null;
    }

    /**
     * Gets the value of the slot property.
     *
     * <p>
     * This accessor method returns a reference to the live list, not a
     * snapshot. Therefore any modification you make to the returned list will
     * be present inside the JAXB object. This is why there is not a
     * <CODE>set</CODE> method for the slot property.
     *
     * <p>
     * For example, to add a new item, do as follows:
     *
     * <pre>
     * getSlot().add(newItem);
     * </pre>
     *
     *
     * <p>
     * Objects of the following type(s) are allowed in the list {@link SlotType
     * }
     *
     *
     */
    public List<SlotType> getSlot() {
        if (slot == null) {
            slot = new ArrayList<>();
        }
        return this.slot;
    }

    public void setSlot(List<SlotType> slot) {
        this.slot = slot;
    }

    @SuppressWarnings("unchecked")
    public <T extends Object> T getSlotValue(String slotName) {
        Object retVal = null;
        for (SlotType slot : getSlot()) {
            if (slot.getName().equals(slotName)) {
                retVal = slot.getSlotValue().getValue();
                break;
            }
        }
        return (T) retVal;
    }

    @SuppressWarnings("unchecked")
    public <T> List<T> getSlotValueAsList(String slotName) {
        List<T> retVal = new ArrayList<>();
        for (SlotType slot : getSlot()) {
            if (slot.getName().equals(slotName)) {
                retVal.add((T) slot.getSlotValue().getValue());
            }
        }
        return retVal;
    }

    public Map<String, Object> getSlotNameValues() {
        if (this.getSlot().isEmpty()) {
            return Collections.emptyMap();
        }
        Map<String, Object> map = new HashMap<>(slot.size());
        for (SlotType slot : this.getSlot()) {
            map.put(slot.getName(), slot.getSlotValue().getValue());
        }
        return map;
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
        result = prime * result + ((slot == null) ? 0 : slot.hashCode());
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
        if (getClass() != obj.getClass()) {
            return false;
        }
        ExtensibleObjectTypeNonJPA other = (ExtensibleObjectTypeNonJPA) obj;
        if (slot == null) {
            if (other.slot != null) {
                return false;
            }
        } else if (!slot.equals(other.slot)) {
            return false;
        }
        return true;
    }

    @Override
    public String toString() {
        StringBuilder builder = new StringBuilder();
        builder.append("ExtensibleObjectType \n[id=");
        builder.append(id);
        builder.append(", \nslot=");
        builder.append(slot);
        builder.append("]");
        return builder.toString();
    }

    public String getId() {
        return id;
    }

    public void setId(String id) {
        this.id = id;
    }

    public String getIdentifier() {
        return id;
    }

}
