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

import java.math.BigInteger;

import javax.persistence.CascadeType;
import javax.persistence.Entity;
import javax.persistence.ManyToOne;
import javax.persistence.Table;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.bind.annotation.XmlSchemaType;
import javax.xml.bind.annotation.XmlType;

import org.hibernate.annotations.Cache;
import org.hibernate.annotations.CacheConcurrencyStrategy;

import com.raytheon.uf.common.registry.RegrepUtil;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;

/**
 * 
 * Represents a parameter for a parameterized query or, an AttributeDef for an
 * ObjectType ClassificationNode.
 * 
 * 
 * <p>
 * Java class for ParameterType complex type.
 * 
 * <p>
 * The following schema fragment specifies the expected content contained within
 * this class.
 * 
 * <pre>
 * &lt;complexType name="ParameterType">
 *   &lt;complexContent>
 *     &lt;extension base="{urn:oasis:names:tc:ebxml-regrep:xsd:rim:4.0}ExtensibleObjectType">
 *       &lt;sequence>
 *         &lt;element name="Name" type="{urn:oasis:names:tc:ebxml-regrep:xsd:rim:4.0}InternationalStringType"/>
 *         &lt;element name="Description" type="{urn:oasis:names:tc:ebxml-regrep:xsd:rim:4.0}InternationalStringType" minOccurs="0"/>
 *       &lt;/sequence>
 *       &lt;attribute name="parameterName" use="required" type="{http://www.w3.org/2001/XMLSchema}string" />
 *       &lt;attribute name="dataType" use="required" type="{http://www.w3.org/2001/XMLSchema}string" />
 *       &lt;attribute name="defaultValue" type="{http://www.w3.org/2001/XMLSchema}string" />
 *       &lt;attribute name="minOccurs" type="{http://www.w3.org/2001/XMLSchema}nonNegativeInteger" default="1" />
 *       &lt;attribute name="maxOccurs" type="{http://www.w3.org/2001/XMLSchema}nonNegativeInteger" default="1" />
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
 * 12/2/2013     1829       bphillip    Made ExtensibleObjectType persistable, 
 *                                      modified persistence annotations, added 
 *                                      constructors, hashCode, toString and equals
 * </pre>
 * 
 * @author bphillip
 * @version 1
 */
@XmlRootElement(name = "Parameter")
@XmlAccessorType(XmlAccessType.FIELD)
@XmlType(name = "ParameterType", propOrder = { "name", "description" })
@DynamicSerialize
@Entity
@Cache(region = RegrepUtil.DB_CACHE_REGION, usage = CacheConcurrencyStrategy.TRANSACTIONAL)
@Table(schema = RegrepUtil.EBXML_SCHEMA, name = "Parameter")
public class ParameterType extends ExtensibleObjectType {

    private static final long serialVersionUID = -2798644090274364236L;

    @ManyToOne(cascade = CascadeType.ALL)
    @XmlElement(name = "Name", required = true)
    @DynamicSerializeElement
    protected InternationalStringType name;

    @ManyToOne(cascade = CascadeType.ALL)
    @XmlElement(name = "Description")
    @DynamicSerializeElement
    protected InternationalStringType description;

    @XmlAttribute(required = true)
    @DynamicSerializeElement
    protected String parameterName;

    @XmlAttribute(required = true)
    @DynamicSerializeElement
    protected String dataType;

    @XmlAttribute
    @DynamicSerializeElement
    protected String defaultValue;

    @XmlAttribute
    @XmlSchemaType(name = "nonNegativeInteger")
    @DynamicSerializeElement
    protected BigInteger minOccurs;

    @XmlAttribute
    @XmlSchemaType(name = "nonNegativeInteger")
    @DynamicSerializeElement
    protected BigInteger maxOccurs;

    public ParameterType() {
        super();
    }

    /**
     * Gets the value of the name property.
     * 
     * @return possible object is {@link InternationalStringType }
     * 
     */
    public InternationalStringType getName() {
        return name;
    }

    /**
     * Sets the value of the name property.
     * 
     * @param value
     *            allowed object is {@link InternationalStringType }
     * 
     */
    public void setName(InternationalStringType value) {
        this.name = value;
    }

    /**
     * Gets the value of the description property.
     * 
     * @return possible object is {@link InternationalStringType }
     * 
     */
    public InternationalStringType getDescription() {
        return description;
    }

    /**
     * Sets the value of the description property.
     * 
     * @param value
     *            allowed object is {@link InternationalStringType }
     * 
     */
    public void setDescription(InternationalStringType value) {
        this.description = value;
    }

    /**
     * Gets the value of the parameterName property.
     * 
     * @return possible object is {@link String }
     * 
     */
    public String getParameterName() {
        return parameterName;
    }

    /**
     * Sets the value of the parameterName property.
     * 
     * @param value
     *            allowed object is {@link String }
     * 
     */
    public void setParameterName(String value) {
        this.parameterName = value;
    }

    /**
     * Gets the value of the dataType property.
     * 
     * @return possible object is {@link String }
     * 
     */
    public String getDataType() {
        return dataType;
    }

    /**
     * Sets the value of the dataType property.
     * 
     * @param value
     *            allowed object is {@link String }
     * 
     */
    public void setDataType(String value) {
        this.dataType = value;
    }

    /**
     * Gets the value of the defaultValue property.
     * 
     * @return possible object is {@link String }
     * 
     */
    public String getDefaultValue() {
        return defaultValue;
    }

    /**
     * Sets the value of the defaultValue property.
     * 
     * @param value
     *            allowed object is {@link String }
     * 
     */
    public void setDefaultValue(String value) {
        this.defaultValue = value;
    }

    /**
     * Gets the value of the minOccurs property.
     * 
     * @return possible object is {@link BigInteger }
     * 
     */
    public BigInteger getMinOccurs() {
        if (minOccurs == null) {
            return new BigInteger("1");
        } else {
            return minOccurs;
        }
    }

    /**
     * Sets the value of the minOccurs property.
     * 
     * @param value
     *            allowed object is {@link BigInteger }
     * 
     */
    public void setMinOccurs(BigInteger value) {
        this.minOccurs = value;
    }

    /**
     * Gets the value of the maxOccurs property.
     * 
     * @return possible object is {@link BigInteger }
     * 
     */
    public BigInteger getMaxOccurs() {
        if (maxOccurs == null) {
            return new BigInteger("1");
        } else {
            return maxOccurs;
        }
    }

    /**
     * Sets the value of the maxOccurs property.
     * 
     * @param value
     *            allowed object is {@link BigInteger }
     * 
     */
    public void setMaxOccurs(BigInteger value) {
        this.maxOccurs = value;
    }

    /*
     * (non-Javadoc)
     * 
     * @see java.lang.Object#hashCode()
     */
    @Override
    public int hashCode() {
        final int prime = 31;
        int result = super.hashCode();
        result = prime * result
                + ((dataType == null) ? 0 : dataType.hashCode());
        result = prime * result
                + ((defaultValue == null) ? 0 : defaultValue.hashCode());
        result = prime * result
                + ((description == null) ? 0 : description.hashCode());
        result = prime * result
                + ((maxOccurs == null) ? 0 : maxOccurs.hashCode());
        result = prime * result
                + ((minOccurs == null) ? 0 : minOccurs.hashCode());
        result = prime * result + ((name == null) ? 0 : name.hashCode());
        result = prime * result
                + ((parameterName == null) ? 0 : parameterName.hashCode());
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
        if (!super.equals(obj)) {
            return false;
        }
        if (getClass() != obj.getClass()) {
            return false;
        }
        ParameterType other = (ParameterType) obj;
        if (dataType == null) {
            if (other.dataType != null) {
                return false;
            }
        } else if (!dataType.equals(other.dataType)) {
            return false;
        }
        if (defaultValue == null) {
            if (other.defaultValue != null) {
                return false;
            }
        } else if (!defaultValue.equals(other.defaultValue)) {
            return false;
        }
        if (description == null) {
            if (other.description != null) {
                return false;
            }
        } else if (!description.equals(other.description)) {
            return false;
        }
        if (maxOccurs == null) {
            if (other.maxOccurs != null) {
                return false;
            }
        } else if (!maxOccurs.equals(other.maxOccurs)) {
            return false;
        }
        if (minOccurs == null) {
            if (other.minOccurs != null) {
                return false;
            }
        } else if (!minOccurs.equals(other.minOccurs)) {
            return false;
        }
        if (name == null) {
            if (other.name != null) {
                return false;
            }
        } else if (!name.equals(other.name)) {
            return false;
        }
        if (parameterName == null) {
            if (other.parameterName != null) {
                return false;
            }
        } else if (!parameterName.equals(other.parameterName)) {
            return false;
        }
        return true;
    }

    @Override
    public String toString() {
        StringBuilder builder = new StringBuilder();
        builder.append("ParameterType \n[id=");
        builder.append(id);
        builder.append(", \nslot=");
        builder.append(slot);
        builder.append(", \nname=");
        builder.append(name);
        builder.append(", \ndescription=");
        builder.append(description);
        builder.append(", \nparameterName=");
        builder.append(parameterName);
        builder.append(", \ndataType=");
        builder.append(dataType);
        builder.append(", \ndefaultValue=");
        builder.append(defaultValue);
        builder.append(", \nminOccurs=");
        builder.append(minOccurs);
        builder.append(", \nmaxOccurs=");
        builder.append(maxOccurs);
        builder.append("]");
        return builder.toString();
    }

}
