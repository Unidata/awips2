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
import javax.persistence.Id;
import javax.persistence.OneToOne;
import javax.persistence.Table;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.bind.annotation.XmlType;

import org.hibernate.annotations.Cache;
import org.hibernate.annotations.CacheConcurrencyStrategy;

import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;

/**
 * 
 * Represents a map entry that has a ValueType as key and a ValueType as value.
 * Collection values can be represneted by a CollectionValueType value.
 * 
 * 
 * <p>
 * Java class for EntryType complex type.
 * 
 * <p>
 * The following schema fragment specifies the expected content contained within
 * this class.
 * 
 * <pre>
 * &lt;complexType name="EntryType">
 *   &lt;complexContent>
 *     &lt;restriction base="{http://www.w3.org/2001/XMLSchema}anyType">
 *       &lt;sequence>
 *         &lt;element name="EntryKey" type="{urn:oasis:names:tc:ebxml-regrep:xsd:rim:4.0}ValueType"/>
 *         &lt;element name="EntryValue" type="{urn:oasis:names:tc:ebxml-regrep:xsd:rim:4.0}ValueType" minOccurs="0"/>
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
@XmlType(name = "EntryType", propOrder = { "entryKey", "entryValue" })
@DynamicSerialize
@Entity
@Cache(region="registryObjects",usage = CacheConcurrencyStrategy.TRANSACTIONAL)
@Table(name = "EntryType")
public class EntryType implements Serializable {

    private static final long serialVersionUID = -641063902591977048L;

    @Id
    @XmlElement(name = "EntryKey", required = true)
    @DynamicSerializeElement
    @OneToOne(cascade = CascadeType.ALL)
    protected ValueType entryKey;

    @Id
    @XmlElement(name = "EntryValue")
    @DynamicSerializeElement
    @OneToOne(cascade = CascadeType.ALL)
    protected ValueType entryValue;

    /**
     * Gets the value of the entryKey property.
     * 
     * @return possible object is {@link ValueType }
     * 
     */
    public ValueType getEntryKey() {
        return entryKey;
    }

    /**
     * Sets the value of the entryKey property.
     * 
     * @param value
     *            allowed object is {@link ValueType }
     * 
     */
    public void setEntryKey(ValueType value) {
        this.entryKey = value;
    }

    /**
     * Gets the value of the entryValue property.
     * 
     * @return possible object is {@link ValueType }
     * 
     */
    public ValueType getEntryValue() {
        return entryValue;
    }

    /**
     * Sets the value of the entryValue property.
     * 
     * @param value
     *            allowed object is {@link ValueType }
     * 
     */
    public void setEntryValue(ValueType value) {
        this.entryValue = value;
    }

}
