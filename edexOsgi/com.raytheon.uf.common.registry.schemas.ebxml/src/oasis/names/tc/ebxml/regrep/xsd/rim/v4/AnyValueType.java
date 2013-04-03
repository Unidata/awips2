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
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlAnyElement;
import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.bind.annotation.XmlType;

import org.hibernate.annotations.Cache;
import org.hibernate.annotations.CacheConcurrencyStrategy;
import org.hibernate.annotations.Type;
import org.w3c.dom.Element;

import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;

/**
 * 
 * A specialized ValueType that may be used as a container for any well-formed
 * XML element value in any namespace. Use this to specify an XML element as the
 * value for a Slot.
 * 
 * 
 * <p>
 * Java class for AnyValueType complex type.
 * 
 * <p>
 * The following schema fragment specifies the expected content contained within
 * this class.
 * 
 * <pre>
 * &lt;complexType name="AnyValueType">
 *   &lt;complexContent>
 *     &lt;extension base="{urn:oasis:names:tc:ebxml-regrep:xsd:rim:4.0}ValueType">
 *       &lt;sequence>
 *         &lt;any processContents='lax' minOccurs="0"/>
 *       &lt;/sequence>
 *     &lt;/extension>
 *   &lt;/complexContent>
 * &lt;/complexType>
 * </pre>
 * 
 * 
 */
@Entity
@Cache(region="registryObjects",usage = CacheConcurrencyStrategy.TRANSACTIONAL)
@XmlRootElement
@XmlAccessorType(XmlAccessType.FIELD)
@XmlType(name = "AnyValueType", propOrder = { "anyValue" })
@DynamicSerialize
public class AnyValueType extends ValueType {

    @XmlAnyElement(lax = true)
    @DynamicSerializeElement
    @Column(name = COLUMN_NAME, columnDefinition = "text")
    @Type(type = "com.raytheon.uf.common.registry.schemas.ebxml.util.SerializedType")
    protected Object anyValue;

    private static final String COLUMN_NAME = "anyValue";

    @Override
    public String getColumnName() {
        return COLUMN_NAME;
    }

    /**
     * Gets the value of the any property.
     * 
     * @return possible object is {@link Element } {@link Object }
     * 
     */
    public Object getAny() {
        return anyValue;
    }

    /**
     * Sets the value of the any property.
     * 
     * @param value
     *            allowed object is {@link Element } {@link Object }
     * 
     */
    public void setAny(Object value) {
        this.anyValue = value;
    }

    @Override
    public Object getValue() {
        return getAny();
    }

    public Object getAnyValue() {
        return anyValue;
    }

    public void setAnyValue(Object anyValue) {
        this.anyValue = anyValue;
    }

    @Override
    public void setValue(Object value) {
        setAny(value);
    }

}
