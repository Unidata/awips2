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

import javax.persistence.CascadeType;
import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.FetchType;
import javax.persistence.Id;
import javax.persistence.Inheritance;
import javax.persistence.InheritanceType;
import javax.persistence.JoinColumn;
import javax.persistence.OneToMany;
import javax.persistence.Table;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.bind.annotation.XmlSeeAlso;
import javax.xml.bind.annotation.XmlTransient;
import javax.xml.bind.annotation.XmlType;

import oasis.names.tc.ebxml.regrep.xsd.rs.v4.RegistryExceptionType;
import oasis.names.tc.ebxml.regrep.xsd.rs.v4.RegistryRequestType;
import oasis.names.tc.ebxml.regrep.xsd.rs.v4.RegistryResponseType;

import org.hibernate.annotations.Cache;
import org.hibernate.annotations.CacheConcurrencyStrategy;

import com.raytheon.uf.common.dataplugin.persist.IPersistableDataObject;
import com.raytheon.uf.common.registry.RegrepUtil;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;

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
 * 2012                     bphillip    Initial implementation
 * 10/17/2013    1682       bphillip    Added software history
 * 12/2/2013     1829       bphillip    Made ExtensibleObjectType persistable, modified persistence annotations, added hashCode and equals
 * </pre>
 * 
 * @author bphillip
 * @version 1
 */
@XmlRootElement(name = "ExtensibleObject")
@XmlAccessorType(XmlAccessType.FIELD)
@XmlType(name = "ExtensibleObjectType", propOrder = { "slot" })
@XmlSeeAlso({ PostalAddressType.class, TelephoneNumberType.class,
        ParameterType.class, QueryType.class, ActionType.class,
        DeliveryInfoType.class, PersonNameType.class, ObjectRefType.class,
        SlotType.class, IdentifiableType.class, EmailAddressType.class,
        QueryExpressionType.class, RegistryExceptionType.class,
        RegistryResponseType.class, RegistryRequestType.class })
@DynamicSerialize
@Entity
@Table(schema = RegrepUtil.EBXML_SCHEMA, name = "ExtensibleObject")
@Inheritance(strategy = InheritanceType.TABLE_PER_CLASS)
@Cache(region = RegrepUtil.DB_CACHE_REGION, usage = CacheConcurrencyStrategy.TRANSACTIONAL, include = "all")
public abstract class ExtensibleObjectType implements Serializable,
        IPersistableDataObject<String> {

    private static final long serialVersionUID = 785780260533569469L;

    @Id
    @Column(length = 1024)
    @DynamicSerializeElement
    @XmlTransient
    protected String id;

    @OneToMany(cascade = CascadeType.ALL, fetch = FetchType.LAZY)
    @JoinColumn(name = "parent_id", nullable = false, referencedColumnName = "id")
    @XmlElement(name = "Slot")
    @DynamicSerializeElement
    protected List<SlotType> slot;

    public ExtensibleObjectType() {
        super();
        this.id = UUID.randomUUID().toString();
    }

    public ExtensibleObjectType(String id) {
        this.id = id;
    }

    public ExtensibleObjectType(String id, List<SlotType> slot) {
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
     * Objects of the following type(s) are allowed in the list {@link SlotType }
     * 
     * 
     */
    public List<SlotType> getSlot() {
        if (slot == null) {
            slot = new ArrayList<SlotType>();
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
        List<T> retVal = new ArrayList<T>();
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
        Map<String, Object> map = new HashMap<String, Object>(slot.size());
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

    @Override
    public String getIdentifier() {
        return id;
    }

}
