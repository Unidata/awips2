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

import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;

/**
 * 
 * A specialized ValueType that may be used as a container for a boolean value.
 * 
 * 
 * <p>
 * Java class for BooleanValueType complex type.
 * 
 * <p>
 * The following schema fragment specifies the expected content contained within
 * this class.
 * 
 * <pre>
 * &lt;complexType name="BooleanValueType">
 *   &lt;complexContent>
 *     &lt;extension base="{urn:oasis:names:tc:ebxml-regrep:xsd:rim:4.0}ValueType">
 *       &lt;sequence>
 *         &lt;element name="Value" type="{http://www.w3.org/2001/XMLSchema}boolean" minOccurs="0"/>
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
@XmlType(name = "BooleanValueType", propOrder = { "booleanValue" })
@DynamicSerialize
@Entity
@Cache(region="registryObjects",usage = CacheConcurrencyStrategy.TRANSACTIONAL)
@Table(name = "BooleanValue")
public class BooleanValueType extends ValueType {

    @Column(name = COLUMN_NAME)
    @XmlElement(name = "Value")
    @DynamicSerializeElement
    protected Boolean booleanValue;

    private static final String COLUMN_NAME = "booleanValue";

    @Override
    public String getColumnName() {
        return COLUMN_NAME;
    }

    /**
     * Gets the value of the value property.
     * 
     * @return possible object is {@link Boolean }
     * 
     */
    public Boolean isValue() {
        return booleanValue;
    }

    /**
     * Sets the value of the value property.
     * 
     * @param value
     *            allowed object is {@link Boolean }
     * 
     */
    @Override
    public void setValue(Object value) {
        this.booleanValue = (Boolean) value;
    }

    @Override
    public Boolean getValue() {
        return isValue();
    }

    public Boolean getBooleanValue() {
        return isValue();
    }

    public void setBooleanValue(Boolean booleanValue) {
        setValue(booleanValue);
    }

}
