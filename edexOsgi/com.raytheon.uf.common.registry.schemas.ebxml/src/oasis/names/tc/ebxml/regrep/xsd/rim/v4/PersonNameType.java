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
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;

/**
 * Represents the name of a Person in ebRIM.
 * 
 * <p>
 * Java class for PersonNameType complex type.
 * 
 * <p>
 * The following schema fragment specifies the expected content contained within
 * this class.
 * 
 * <pre>
 * &lt;complexType name="PersonNameType">
 *   &lt;complexContent>
 *     &lt;extension base="{urn:oasis:names:tc:ebxml-regrep:xsd:rim:4.0}ExtensibleObjectType">
 *       &lt;attribute name="firstName" type="{urn:oasis:names:tc:ebxml-regrep:xsd:rim:4.0}ShortText" />
 *       &lt;attribute name="middleName" type="{urn:oasis:names:tc:ebxml-regrep:xsd:rim:4.0}ShortText" />
 *       &lt;attribute name="lastName" type="{urn:oasis:names:tc:ebxml-regrep:xsd:rim:4.0}ShortText" />
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
@XmlRootElement(name = "PersonName")
@XmlAccessorType(XmlAccessType.FIELD)
@XmlType(name = "PersonNameType")
@DynamicSerialize
@Entity
@Cache(region = RegrepUtil.DB_CACHE_REGION, usage = CacheConcurrencyStrategy.TRANSACTIONAL)
@Table(schema = RegrepUtil.EBXML_SCHEMA, name = "PersonName")
public class PersonNameType extends ExtensibleObjectType {

    private static final long serialVersionUID = -7453253089839327032L;

    @XmlAttribute
    @DynamicSerializeElement
    protected String firstName;

    @XmlAttribute
    @DynamicSerializeElement
    protected String middleName;

    @XmlAttribute
    @DynamicSerializeElement
    protected String lastName;

    public PersonNameType() {
        super();
    }

    /**
     * Gets the value of the firstName property.
     * 
     * @return possible object is {@link String }
     * 
     */
    public String getFirstName() {
        return firstName;
    }

    /**
     * Sets the value of the firstName property.
     * 
     * @param value
     *            allowed object is {@link String }
     * 
     */
    public void setFirstName(String value) {
        this.firstName = value;
    }

    /**
     * Gets the value of the middleName property.
     * 
     * @return possible object is {@link String }
     * 
     */
    public String getMiddleName() {
        return middleName;
    }

    /**
     * Sets the value of the middleName property.
     * 
     * @param value
     *            allowed object is {@link String }
     * 
     */
    public void setMiddleName(String value) {
        this.middleName = value;
    }

    /**
     * Gets the value of the lastName property.
     * 
     * @return possible object is {@link String }
     * 
     */
    public String getLastName() {
        return lastName;
    }

    /**
     * Sets the value of the lastName property.
     * 
     * @param value
     *            allowed object is {@link String }
     * 
     */
    public void setLastName(String value) {
        this.lastName = value;
    }

    @Override
    public int hashCode() {
        final int prime = 31;
        int result = super.hashCode();
        result = prime * result
                + ((firstName == null) ? 0 : firstName.hashCode());
        result = prime * result
                + ((lastName == null) ? 0 : lastName.hashCode());
        result = prime * result
                + ((middleName == null) ? 0 : middleName.hashCode());
        return result;
    }

    @Override
    public boolean equals(Object obj) {
        if (this == obj)
            return true;
        if (!super.equals(obj))
            return false;
        if (getClass() != obj.getClass())
            return false;
        PersonNameType other = (PersonNameType) obj;
        if (firstName == null) {
            if (other.firstName != null)
                return false;
        } else if (!firstName.equals(other.firstName))
            return false;
        if (lastName == null) {
            if (other.lastName != null)
                return false;
        } else if (!lastName.equals(other.lastName))
            return false;
        if (middleName == null) {
            if (other.middleName != null)
                return false;
        } else if (!middleName.equals(other.middleName))
            return false;
        return true;
    }

    @Override
    public String toString() {
        StringBuilder builder = new StringBuilder();
        builder.append("PersonNameType \n[id=");
        builder.append(id);
        builder.append(", \nslot=");
        builder.append(slot);
        builder.append(", \nfirstName=");
        builder.append(firstName);
        builder.append(", \nmiddleName=");
        builder.append(middleName);
        builder.append(", \nlastName=");
        builder.append(lastName);
        builder.append("]");
        return builder.toString();
    }

}
