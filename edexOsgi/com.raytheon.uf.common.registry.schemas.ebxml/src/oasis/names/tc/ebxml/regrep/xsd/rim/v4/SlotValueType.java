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
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.bind.annotation.XmlType;

import org.hibernate.annotations.Cache;
import org.hibernate.annotations.CacheConcurrencyStrategy;
import org.hibernate.annotations.Type;

import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;

/**
 * 
 * A specialized ValueType that may be used as a container for a Slot value.
 * 
 * 
 * <p>
 * Java class for SlotValueType complex type.
 * 
 * <p>
 * The following schema fragment specifies the expected content contained within
 * this class.
 * 
 * <pre>
 * &lt;complexType name="SlotValueType">
 *   &lt;complexContent>
 *     &lt;extension base="{urn:oasis:names:tc:ebxml-regrep:xsd:rim:4.0}ValueType">
 *       &lt;sequence>
 *         &lt;element name="Slot" type="{urn:oasis:names:tc:ebxml-regrep:xsd:rim:4.0}SlotType" minOccurs="0"/>
 *       &lt;/sequence>
 *     &lt;/extension>
 *   &lt;/complexContent>
 * &lt;/complexType>
 * </pre>
 * 
 * 
 */
@XmlRootElement
@XmlAccessorType(XmlAccessType.FIELD)
@XmlType(name = "SlotValueType", propOrder = { "slotValue" })
@DynamicSerialize
@Entity
@Cache(region="registryObjects",usage = CacheConcurrencyStrategy.TRANSACTIONAL)
@Table(name = "SlotType")
public class SlotValueType extends ValueType {

    @XmlElement(name = "Slot")
    @DynamicSerializeElement
    @Column(name = COLUMN_NAME, columnDefinition = "text")
    @Type(type = "com.raytheon.uf.common.registry.schemas.ebxml.util.SerializedType")
    protected SlotType slotValue;

    private static final String COLUMN_NAME = "slotValue";

    @Override
    public String getColumnName() {
        return COLUMN_NAME;
    }

    /**
     * Gets the value of the slot property.
     * 
     * @return possible object is {@link SlotType }
     * 
     */
    public SlotType getSlot() {
        return slotValue;
    }

    /**
     * Sets the value of the slot property.
     * 
     * @param value
     *            allowed object is {@link SlotType }
     * 
     */
    public void setSlot(SlotType value) {
        this.slotValue = value;
    }

    @Override
    @SuppressWarnings("unchecked")
    public SlotType getValue() {
        return slotValue;
    }

    public SlotType getSlotValue() {
        return slotValue;
    }

    public void setSlotValue(SlotType slotValue) {
        this.slotValue = slotValue;
    }

    @Override
    public void setValue(Object slotValue) {
        this.slotValue = (SlotType) slotValue;
    }

}
