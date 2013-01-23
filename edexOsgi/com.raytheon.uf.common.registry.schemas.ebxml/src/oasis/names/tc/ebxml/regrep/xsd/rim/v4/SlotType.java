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

import javax.persistence.CascadeType;
import javax.persistence.Entity;
import javax.persistence.GeneratedValue;
import javax.persistence.Id;
import javax.persistence.ManyToOne;
import javax.persistence.Table;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.bind.annotation.XmlTransient;
import javax.xml.bind.annotation.XmlType;

import org.hibernate.annotations.Cache;
import org.hibernate.annotations.CacheConcurrencyStrategy;
import org.hibernate.annotations.Index;

import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;

/**
 * 
 * Represents an extensible attribute that may be dynamically added to any
 * ExtensibleObjectType instance. It is an important extensibility mechanism
 * with ebRIM. A SlotType instance contains a name and a value. The value may be
 * of any type.
 * 
 * 
 * <p>
 * Java class for SlotType complex type.
 * 
 * <p>
 * The following schema fragment specifies the expected content contained within
 * this class.
 * 
 * <pre>
 * &lt;complexType name="SlotType">
 *   &lt;complexContent>
 *     &lt;extension base="{urn:oasis:names:tc:ebxml-regrep:xsd:rim:4.0}ExtensibleObjectType">
 *       &lt;sequence>
 *         &lt;element name="SlotValue" type="{urn:oasis:names:tc:ebxml-regrep:xsd:rim:4.0}ValueType" minOccurs="0"/>
 *       &lt;/sequence>
 *       &lt;attribute name="name" use="required" type="{urn:oasis:names:tc:ebxml-regrep:xsd:rim:4.0}LongText" />
 *       &lt;attribute name="type" type="{urn:oasis:names:tc:ebxml-regrep:xsd:rim:4.0}LongText" />
 *     &lt;/extension>
 *   &lt;/complexContent>
 * &lt;/complexType>
 * </pre>
 * 
 * 
 */
@XmlRootElement
@XmlAccessorType(XmlAccessType.FIELD)
@XmlType(name = "SlotType", propOrder = { "slotValue" })
@DynamicSerialize
@Entity
// @AssociationOverride(name = "slot", joinTable = @JoinTable(name =
// "slot_join_slot", joinColumns = @JoinColumn(name = "parent_slot_key",
// referencedColumnName = "key"), inverseJoinColumns = @JoinColumn(name =
// "child_slot_key", referencedColumnName = "key")))
@Cache(region="registryObjects",usage = CacheConcurrencyStrategy.TRANSACTIONAL, include = "all")
@Table(name = "Slot")
public class SlotType extends ExtensibleObjectType implements Serializable {

    private static final long serialVersionUID = -2184582316481503043L;

    @Id
    @GeneratedValue
    @XmlTransient
    private Integer key;

    @ManyToOne(cascade = CascadeType.ALL)
    @XmlElement(name = "SlotValue")
    @DynamicSerializeElement
    protected ValueType slotValue;

    @XmlAttribute(required = true)
    @DynamicSerializeElement
    @Index(name = "slot_name_idx")
    protected String name;

    @XmlAttribute
    @DynamicSerializeElement
    protected String type;

    public Integer getKey() {
        return key;
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
        result = prime * result + ((name == null) ? 0 : name.hashCode());
        result = prime * result
                + ((slotValue == null) ? 0 : slotValue.hashCode());
        result = prime * result + ((type == null) ? 0 : type.hashCode());
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
        SlotType other = (SlotType) obj;
        if (name == null) {
            if (other.name != null) {
                return false;
            }
        } else if (!name.equals(other.name)) {
            return false;
        }
        if (slotValue == null) {
            if (other.slotValue != null) {
                return false;
            }
        } else if (!slotValue.equals(other.slotValue)) {
            return false;
        }
        if (type == null) {
            if (other.type != null) {
                return false;
            }
        } else if (!type.equals(other.type)) {
            return false;
        }
        return true;
    }

    public void setKey(int key) {
        this.key = this.hashCode();
    }

    /**
     * Gets the value of the slotValue property.
     * 
     * @return possible object is {@link ValueType }
     * 
     */
    public ValueType getSlotValue() {
        return slotValue;
    }

    /**
     * Sets the value of the slotValue property.
     * 
     * @param value
     *            allowed object is {@link ValueType }
     * 
     */
    public void setSlotValue(ValueType value) {
        this.slotValue = value;
    }

    /**
     * Gets the value of the name property.
     * 
     * @return possible object is {@link String }
     * 
     */
    public String getName() {
        return name;
    }

    /**
     * Sets the value of the name property.
     * 
     * @param value
     *            allowed object is {@link String }
     * 
     */
    public void setName(String value) {
        this.name = value;
    }

    /**
     * Gets the value of the type property.
     * 
     * @return possible object is {@link String }
     * 
     */
    public String getType() {
        return type;
    }

    /**
     * Sets the value of the type property.
     * 
     * @param value
     *            allowed object is {@link String }
     * 
     */
    public void setType(String value) {
        this.type = value;
    }

}
