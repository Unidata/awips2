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

import javax.persistence.Entity;
import javax.persistence.Table;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.bind.annotation.XmlType;

import org.hibernate.annotations.Cache;
import org.hibernate.annotations.CacheConcurrencyStrategy;

import com.raytheon.uf.common.registry.RegrepUtil;
import com.raytheon.uf.common.registry.schemas.ebxml.util.annotations.RegistryObjectReference;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;

/**
 * Represents a postal address in ebRIM.
 * 
 * <p>
 * Java class for PostalAddressType complex type.
 * 
 * <p>
 * The following schema fragment specifies the expected content contained within
 * this class.
 * 
 * <pre>
 * &lt;complexType name="PostalAddressType">
 *   &lt;complexContent>
 *     &lt;extension base="{urn:oasis:names:tc:ebxml-regrep:xsd:rim:4.0}ExtensibleObjectType">
 *       &lt;attribute name="city" type="{urn:oasis:names:tc:ebxml-regrep:xsd:rim:4.0}ShortText" />
 *       &lt;attribute name="country" type="{urn:oasis:names:tc:ebxml-regrep:xsd:rim:4.0}ShortText" />
 *       &lt;attribute name="postalCode" type="{urn:oasis:names:tc:ebxml-regrep:xsd:rim:4.0}ShortText" />
 *       &lt;attribute name="stateOrProvince" type="{urn:oasis:names:tc:ebxml-regrep:xsd:rim:4.0}ShortText" />
 *       &lt;attribute name="street" type="{urn:oasis:names:tc:ebxml-regrep:xsd:rim:4.0}ShortText" />
 *       &lt;attribute name="streetNumber" type="{urn:oasis:names:tc:ebxml-regrep:xsd:rim:4.0}String32" />
 *       &lt;attribute name="type" type="{urn:oasis:names:tc:ebxml-regrep:xsd:rim:4.0}objectReferenceType" />
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
@XmlRootElement(name = "PostalAddress")
@XmlAccessorType(XmlAccessType.FIELD)
@XmlType(name = "PostalAddressType")
@DynamicSerialize
@Entity
@Cache(region = RegrepUtil.DB_CACHE_REGION, usage = CacheConcurrencyStrategy.TRANSACTIONAL)
@Table(schema = RegrepUtil.EBXML_SCHEMA, name = "PostalAddress")
public class PostalAddressType extends ExtensibleObjectType {

    private static final long serialVersionUID = 600729702802113902L;

    @XmlAttribute
    @DynamicSerializeElement
    protected String city;

    @XmlAttribute
    @DynamicSerializeElement
    protected String country;

    @XmlAttribute
    @DynamicSerializeElement
    protected String postalCode;

    @XmlAttribute
    @DynamicSerializeElement
    protected String stateOrProvince;

    @XmlAttribute
    @DynamicSerializeElement
    protected String street;

    @XmlAttribute
    @DynamicSerializeElement
    protected String streetNumber;

    @XmlAttribute
    @DynamicSerializeElement
    @RegistryObjectReference
    protected String type;

    public PostalAddressType() {
        super();
    }

    /**
     * Gets the value of the city property.
     * 
     * @return possible object is {@link String }
     * 
     */
    public String getCity() {
        return city;
    }

    /**
     * Sets the value of the city property.
     * 
     * @param value
     *            allowed object is {@link String }
     * 
     */
    public void setCity(String value) {
        this.city = value;
    }

    /**
     * Gets the value of the country property.
     * 
     * @return possible object is {@link String }
     * 
     */
    public String getCountry() {
        return country;
    }

    /**
     * Sets the value of the country property.
     * 
     * @param value
     *            allowed object is {@link String }
     * 
     */
    public void setCountry(String value) {
        this.country = value;
    }

    /**
     * Gets the value of the postalCode property.
     * 
     * @return possible object is {@link String }
     * 
     */
    public String getPostalCode() {
        return postalCode;
    }

    /**
     * Sets the value of the postalCode property.
     * 
     * @param value
     *            allowed object is {@link String }
     * 
     */
    public void setPostalCode(String value) {
        this.postalCode = value;
    }

    /**
     * Gets the value of the stateOrProvince property.
     * 
     * @return possible object is {@link String }
     * 
     */
    public String getStateOrProvince() {
        return stateOrProvince;
    }

    /**
     * Sets the value of the stateOrProvince property.
     * 
     * @param value
     *            allowed object is {@link String }
     * 
     */
    public void setStateOrProvince(String value) {
        this.stateOrProvince = value;
    }

    /**
     * Gets the value of the street property.
     * 
     * @return possible object is {@link String }
     * 
     */
    public String getStreet() {
        return street;
    }

    /**
     * Sets the value of the street property.
     * 
     * @param value
     *            allowed object is {@link String }
     * 
     */
    public void setStreet(String value) {
        this.street = value;
    }

    /**
     * Gets the value of the streetNumber property.
     * 
     * @return possible object is {@link String }
     * 
     */
    public String getStreetNumber() {
        return streetNumber;
    }

    /**
     * Sets the value of the streetNumber property.
     * 
     * @param value
     *            allowed object is {@link String }
     * 
     */
    public void setStreetNumber(String value) {
        this.streetNumber = value;
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

    /*
     * (non-Javadoc)
     * 
     * @see java.lang.Object#hashCode()
     */
    @Override
    public int hashCode() {
        final int prime = 31;
        int result = super.hashCode();
        result = prime * result + ((city == null) ? 0 : city.hashCode());
        result = prime * result + ((country == null) ? 0 : country.hashCode());
        result = prime * result
                + ((postalCode == null) ? 0 : postalCode.hashCode());
        result = prime * result
                + ((stateOrProvince == null) ? 0 : stateOrProvince.hashCode());
        result = prime * result + ((street == null) ? 0 : street.hashCode());
        result = prime * result
                + ((streetNumber == null) ? 0 : streetNumber.hashCode());
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
        if (!super.equals(obj)) {
            return false;
        }
        if (getClass() != obj.getClass()) {
            return false;
        }
        PostalAddressType other = (PostalAddressType) obj;
        if (city == null) {
            if (other.city != null) {
                return false;
            }
        } else if (!city.equals(other.city)) {
            return false;
        }
        if (country == null) {
            if (other.country != null) {
                return false;
            }
        } else if (!country.equals(other.country)) {
            return false;
        }
        if (postalCode == null) {
            if (other.postalCode != null) {
                return false;
            }
        } else if (!postalCode.equals(other.postalCode)) {
            return false;
        }
        if (stateOrProvince == null) {
            if (other.stateOrProvince != null) {
                return false;
            }
        } else if (!stateOrProvince.equals(other.stateOrProvince)) {
            return false;
        }
        if (street == null) {
            if (other.street != null) {
                return false;
            }
        } else if (!street.equals(other.street)) {
            return false;
        }
        if (streetNumber == null) {
            if (other.streetNumber != null) {
                return false;
            }
        } else if (!streetNumber.equals(other.streetNumber)) {
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

    @Override
    public String toString() {
        StringBuilder builder = new StringBuilder();
        builder.append("PostalAddressType \n[id=");
        builder.append(id);
        builder.append(", \nslot=");
        builder.append(slot);
        builder.append(", \ncity=");
        builder.append(city);
        builder.append(", \ncountry=");
        builder.append(country);
        builder.append(", \npostalCode=");
        builder.append(postalCode);
        builder.append(", \nstateOrProvince=");
        builder.append(stateOrProvince);
        builder.append(", \nstreet=");
        builder.append(street);
        builder.append(", \nstreetNumber=");
        builder.append(streetNumber);
        builder.append(", \ntype=");
        builder.append(type);
        builder.append("]");
        return builder.toString();
    }

}
