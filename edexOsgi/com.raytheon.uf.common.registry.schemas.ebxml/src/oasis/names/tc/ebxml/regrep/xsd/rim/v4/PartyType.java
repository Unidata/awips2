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

import java.util.ArrayList;
import java.util.List;

import javax.persistence.CascadeType;
import javax.persistence.JoinTable;
import javax.persistence.MappedSuperclass;
import javax.persistence.OneToMany;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.bind.annotation.XmlSeeAlso;
import javax.xml.bind.annotation.XmlType;

import com.raytheon.uf.common.registry.RegrepUtil;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;

/**
 * Represents a Party such as Person or Organization.
 * 
 * <p>
 * Java class for PartyType complex type.
 * 
 * <p>
 * The following schema fragment specifies the expected content contained within
 * this class.
 * 
 * <pre>
 * &lt;complexType name="PartyType">
 *   &lt;complexContent>
 *     &lt;extension base="{urn:oasis:names:tc:ebxml-regrep:xsd:rim:4.0}RegistryObjectType">
 *       &lt;sequence>
 *         &lt;element name="PostalAddress" type="{urn:oasis:names:tc:ebxml-regrep:xsd:rim:4.0}PostalAddressType" maxOccurs="unbounded" minOccurs="0"/>
 *         &lt;element name="TelephoneNumber" type="{urn:oasis:names:tc:ebxml-regrep:xsd:rim:4.0}TelephoneNumberType" maxOccurs="unbounded" minOccurs="0"/>
 *         &lt;element name="EmailAddress" type="{urn:oasis:names:tc:ebxml-regrep:xsd:rim:4.0}EmailAddressType" maxOccurs="unbounded" minOccurs="0"/>
 *       &lt;/sequence>
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
@XmlRootElement(name = "Party")
@XmlAccessorType(XmlAccessType.FIELD)
@XmlType(name = "PartyType", propOrder = { "postalAddress", "telephoneNumber",
        "emailAddress" })
@XmlSeeAlso({ PersonType.class, OrganizationType.class })
@DynamicSerialize
@MappedSuperclass
public abstract class PartyType extends RegistryObjectType {

    private static final long serialVersionUID = 4326656165028413864L;

    @XmlElement(name = "PostalAddress")
    @DynamicSerializeElement
    @OneToMany(cascade = CascadeType.ALL)
    @JoinTable(schema = RegrepUtil.EBXML_SCHEMA)
    protected List<PostalAddressType> postalAddress;

    @XmlElement(name = "TelephoneNumber")
    @DynamicSerializeElement
    @OneToMany(cascade = CascadeType.ALL)
    @JoinTable(schema = RegrepUtil.EBXML_SCHEMA)
    protected List<TelephoneNumberType> telephoneNumber;

    @XmlElement(name = "EmailAddress")
    @DynamicSerializeElement
    @OneToMany(cascade = CascadeType.ALL)
    @JoinTable(schema = RegrepUtil.EBXML_SCHEMA)
    protected List<EmailAddressType> emailAddress;

    public PartyType() {
        super();

    }

    public PartyType(String id, String lid, String objectType, String owner,
            String status, String name, String description) {
        super(id, lid, objectType, owner, status, name, description);

    }

    public PartyType(String id, String lid) {
        super(id, lid);

    }

    /**
     * Gets the value of the postalAddress property.
     * 
     * <p>
     * This accessor method returns a reference to the live list, not a
     * snapshot. Therefore any modification you make to the returned list will
     * be present inside the JAXB object. This is why there is not a
     * <CODE>set</CODE> method for the postalAddress property.
     * 
     * <p>
     * For example, to add a new item, do as follows:
     * 
     * <pre>
     * getPostalAddress().add(newItem);
     * </pre>
     * 
     * 
     * <p>
     * Objects of the following type(s) are allowed in the list
     * {@link PostalAddressType }
     * 
     * 
     */
    public List<PostalAddressType> getPostalAddress() {
        if (postalAddress == null) {
            postalAddress = new ArrayList<PostalAddressType>();
        }
        return this.postalAddress;
    }

    /**
     * Gets the value of the telephoneNumber property.
     * 
     * <p>
     * This accessor method returns a reference to the live list, not a
     * snapshot. Therefore any modification you make to the returned list will
     * be present inside the JAXB object. This is why there is not a
     * <CODE>set</CODE> method for the telephoneNumber property.
     * 
     * <p>
     * For example, to add a new item, do as follows:
     * 
     * <pre>
     * getTelephoneNumber().add(newItem);
     * </pre>
     * 
     * 
     * <p>
     * Objects of the following type(s) are allowed in the list
     * {@link TelephoneNumberType }
     * 
     * 
     */
    public List<TelephoneNumberType> getTelephoneNumber() {
        if (telephoneNumber == null) {
            telephoneNumber = new ArrayList<TelephoneNumberType>();
        }
        return this.telephoneNumber;
    }

    /**
     * Gets the value of the emailAddress property.
     * 
     * <p>
     * This accessor method returns a reference to the live list, not a
     * snapshot. Therefore any modification you make to the returned list will
     * be present inside the JAXB object. This is why there is not a
     * <CODE>set</CODE> method for the emailAddress property.
     * 
     * <p>
     * For example, to add a new item, do as follows:
     * 
     * <pre>
     * getEmailAddress().add(newItem);
     * </pre>
     * 
     * 
     * <p>
     * Objects of the following type(s) are allowed in the list
     * {@link EmailAddressType }
     * 
     * 
     */
    public List<EmailAddressType> getEmailAddress() {
        if (emailAddress == null) {
            emailAddress = new ArrayList<EmailAddressType>();
        }
        return this.emailAddress;
    }

    public void setPostalAddress(List<PostalAddressType> postalAddress) {
        this.postalAddress = postalAddress;
    }

    public void setTelephoneNumber(List<TelephoneNumberType> telephoneNumber) {
        this.telephoneNumber = telephoneNumber;
    }

    public void setEmailAddress(List<EmailAddressType> emailAddress) {
        this.emailAddress = emailAddress;
    }

    @Override
    public int hashCode() {
        final int prime = 31;
        int result = super.hashCode();
        result = prime * result
                + ((emailAddress == null) ? 0 : emailAddress.hashCode());
        result = prime * result
                + ((postalAddress == null) ? 0 : postalAddress.hashCode());
        result = prime * result
                + ((telephoneNumber == null) ? 0 : telephoneNumber.hashCode());
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
        PartyType other = (PartyType) obj;
        if (emailAddress == null) {
            if (other.emailAddress != null)
                return false;
        } else if (!emailAddress.equals(other.emailAddress))
            return false;
        if (postalAddress == null) {
            if (other.postalAddress != null)
                return false;
        } else if (!postalAddress.equals(other.postalAddress))
            return false;
        if (telephoneNumber == null) {
            if (other.telephoneNumber != null)
                return false;
        } else if (!telephoneNumber.equals(other.telephoneNumber))
            return false;
        return true;
    }

    @Override
    public String toString() {
        StringBuilder builder = new StringBuilder();
        builder.append("PartyType \n[name=");
        builder.append(name);
        builder.append(", \ndescription=");
        builder.append(description);
        builder.append(", \nversionInfo=");
        builder.append(versionInfo);
        builder.append(", \nclassification=");
        builder.append(classification);
        builder.append(", \nexternalIdentifier=");
        builder.append(externalIdentifier);
        builder.append(", \nexternalLink=");
        builder.append(externalLink);
        builder.append(", \nlid=");
        builder.append(lid);
        builder.append(", \nobjectType=");
        builder.append(objectType);
        builder.append(", \nowner=");
        builder.append(owner);
        builder.append(", \nstatus=");
        builder.append(status);
        builder.append(", \nid=");
        builder.append(id);
        builder.append(", \nslot=");
        builder.append(slot);
        builder.append(", \npostalAddress=");
        builder.append(postalAddress);
        builder.append(", \ntelephoneNumber=");
        builder.append(telephoneNumber);
        builder.append(", \nemailAddress=");
        builder.append(emailAddress);
        builder.append("]");
        return builder.toString();
    }

}
