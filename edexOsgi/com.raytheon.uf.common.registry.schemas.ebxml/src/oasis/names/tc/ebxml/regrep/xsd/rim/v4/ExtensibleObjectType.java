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

import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;

import javax.persistence.CascadeType;
import javax.persistence.FetchType;
import javax.persistence.JoinColumn;
import javax.persistence.JoinTable;
import javax.persistence.ManyToMany;
import javax.persistence.MappedSuperclass;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.bind.annotation.XmlSeeAlso;
import javax.xml.bind.annotation.XmlType;

import oasis.names.tc.ebxml.regrep.xsd.rs.v4.RegistryExceptionType;
import oasis.names.tc.ebxml.regrep.xsd.rs.v4.RegistryRequestType;
import oasis.names.tc.ebxml.regrep.xsd.rs.v4.RegistryResponseType;

import org.hibernate.annotations.BatchSize;
import org.hibernate.annotations.Cache;
import org.hibernate.annotations.CacheConcurrencyStrategy;

import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;

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
 * 
 */
@XmlRootElement
@XmlAccessorType(XmlAccessType.FIELD)
@XmlType(name = "ExtensibleObjectType", propOrder = { "slot" })
@XmlSeeAlso({ PostalAddressType.class, TelephoneNumberType.class,
        ParameterType.class, QueryType.class, ActionType.class,
        DeliveryInfoType.class, PersonNameType.class, ObjectRefType.class,
        SlotType.class, IdentifiableType.class, EmailAddressType.class,
        QueryExpressionType.class, RegistryExceptionType.class,
        RegistryResponseType.class, RegistryRequestType.class })
@DynamicSerialize
@MappedSuperclass
@Cache(region = "registryObjects", usage = CacheConcurrencyStrategy.TRANSACTIONAL, include = "all")
public abstract class ExtensibleObjectType {

    private static final IUFStatusHandler statusHandler = UFStatus
            .getHandler(ExtensibleObjectType.class);

    @BatchSize(size = 500)
    @ManyToMany(cascade = CascadeType.ALL, fetch = FetchType.LAZY)
    @JoinTable(schema = "ebxml", inverseJoinColumns = @JoinColumn(name = "child_slot_key"))
    @XmlElement(name = "Slot")
    @DynamicSerializeElement
    protected Set<SlotType> slot;

    private static final Map<Class<?>, Class<?>> SLOT_VALUE_TYPE_MAP;

    static {
        Map<Class<?>, Class<?>> map = new HashMap<Class<?>, Class<?>>();
        map.put(String.class, StringValueType.class);
        map.put(Integer.class, IntegerValueType.class);
        map.put(Float.class, FloatValueType.class);
        SLOT_VALUE_TYPE_MAP = Collections.unmodifiableMap(map);

    }

    protected ExtensibleObjectType() {

    }

    protected ExtensibleObjectType(Collection<SlotType> slots) {
        if (slots != null) {
            getSlot().addAll(slots);
        }
    }

    protected ExtensibleObjectType(Object... slotNameValues) {
        if (slotNameValues.length % 2 != 0) {
            throw new IllegalArgumentException(
                    "Incorrect number of arguments submitted to ExtensibleObjectType constructor");
        }
        for (int i = 0; i < slotNameValues.length; i += 2) {
            addSlot((String) slotNameValues[i], slotNameValues[i + 1]);
        }
    }

    public SlotType createSlot(String slotName, Object slotValue) {
        SlotType slot = new SlotType();
        slot.setName(slotName);
        Class<?> slotValueTypeClass = SLOT_VALUE_TYPE_MAP.get(slotValue
                .getClass());
        ValueType slotValueObject = null;
        if (slotValueTypeClass != null) {
            try {
                slotValueObject = (ValueType) slotValueTypeClass.newInstance();
            } catch (Exception e) {
                statusHandler.error("Error instantiating slot value!", e);
            }
        } else {
            throw new IllegalArgumentException(
                    "Unable to create slot for type: " + slotValue.getClass());
        }
        slotValueObject.setValue(slotValue);
        slot.setSlotValue(slotValueObject);
        return slot;
    }

    public void addSlot(String slotName, Object slotValue) {
        getSlot().add(createSlot(slotName, slotValue));
    }

    public void updateSlot(String slotName, Object slotValue) {
        for (SlotType slot : getSlot()) {
            if (slot.getName().equals(slotName)) {
                slot.getSlotValue().setValue(slotValue);
                return;
            }
        }
        addSlot(slotName, slotValue);
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
     * Objects of the following type(s) are allowed in the list {@link SlotType }
     * 
     * 
     */
    public Set<SlotType> getSlot() {
        if (slot == null) {
            slot = new HashSet<SlotType>();
        }
        return this.slot;
    }

    public void setSlot(Set<SlotType> slot) {
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
        ExtensibleObjectType other = (ExtensibleObjectType) obj;
        if (slot == null) {
            if (other.slot != null) {
                return false;
            }
        } else if (!slot.equals(other.slot)) {
            return false;
        }
        return true;
    }

}
