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

import javax.persistence.CascadeType;
import javax.persistence.Entity;
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
 * A specialized ValueType that may be used as a container for a
 * InternationalString value.
 * 
 * 
 * <p>
 * Java class for InternationalStringValueType complex type.
 * 
 * <p>
 * The following schema fragment specifies the expected content contained within
 * this class.
 * 
 * <pre>
 * &lt;complexType name="InternationalStringValueType">
 *   &lt;complexContent>
 *     &lt;extension base="{urn:oasis:names:tc:ebxml-regrep:xsd:rim:4.0}ValueType">
 *       &lt;sequence>
 *         &lt;element name="Value" type="{urn:oasis:names:tc:ebxml-regrep:xsd:rim:4.0}InternationalStringType" minOccurs="0"/>
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
@XmlType(name = "InternationalStringValueType", propOrder = { "internationalStringValue" })
@DynamicSerialize
@Entity
@Cache(region="registryObjects",usage = CacheConcurrencyStrategy.TRANSACTIONAL)
@Table(name = "InternationalStringValue")
public class InternationalStringValueType extends ValueType {

    @XmlElement(name = "Value")
    @DynamicSerializeElement
    @OneToOne(cascade = CascadeType.ALL)
    protected InternationalStringType internationalStringValue;

    private static final String COLUMN_NAME = "internationalStringValue";

    @Override
    public String getColumnName() {
        return COLUMN_NAME;
    }

    /**
     * Gets the value of the value property.
     * 
     * @return possible object is {@link InternationalStringType }
     * 
     */
    @Override
    @SuppressWarnings("unchecked")
    public InternationalStringType getValue() {
        return internationalStringValue;
    }

    public void setValue(InternationalStringType obj) {
        this.internationalStringValue = obj;
    }

    @Override
    public void setValue(Object obj) {
        this.internationalStringValue = (InternationalStringType) obj;
    }

    public InternationalStringType getInternationalStringValue() {
        return internationalStringValue;
    }

    public void setInternationalStringValue(
            InternationalStringType internationalStringValue) {
        this.internationalStringValue = internationalStringValue;
    }

}
