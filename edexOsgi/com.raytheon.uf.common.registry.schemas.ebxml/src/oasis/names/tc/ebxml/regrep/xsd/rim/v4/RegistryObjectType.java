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

import java.util.HashSet;
import java.util.Set;

import javax.persistence.CascadeType;
import javax.persistence.Entity;
import javax.persistence.FetchType;
import javax.persistence.Inheritance;
import javax.persistence.InheritanceType;
import javax.persistence.ManyToMany;
import javax.persistence.ManyToOne;
import javax.persistence.OneToOne;
import javax.persistence.Table;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.bind.annotation.XmlSeeAlso;
import javax.xml.bind.annotation.XmlType;

import org.hibernate.annotations.Cache;
import org.hibernate.annotations.CacheConcurrencyStrategy;
import org.hibernate.annotations.Cascade;
import org.hibernate.annotations.Index;

import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;

/**
 * 
 * This type is the common base type for all query-able metadata elements in
 * ebRIM.
 * 
 * 
 * <p>
 * Java class for RegistryObjectType complex type.
 * 
 * <p>
 * The following schema fragment specifies the expected content contained within
 * this class.
 * 
 * <pre>
 * &lt;complexType name="RegistryObjectType">
 *   &lt;complexContent>
 *     &lt;extension base="{urn:oasis:names:tc:ebxml-regrep:xsd:rim:4.0}IdentifiableType">
 *       &lt;sequence>
 *         &lt;element name="Name" type="{urn:oasis:names:tc:ebxml-regrep:xsd:rim:4.0}InternationalStringType" minOccurs="0"/>
 *         &lt;element name="Description" type="{urn:oasis:names:tc:ebxml-regrep:xsd:rim:4.0}InternationalStringType" minOccurs="0"/>
 *         &lt;element name="VersionInfo" type="{urn:oasis:names:tc:ebxml-regrep:xsd:rim:4.0}VersionInfoType" minOccurs="0"/>
 *         &lt;element name="Classification" type="{urn:oasis:names:tc:ebxml-regrep:xsd:rim:4.0}ClassificationType" maxOccurs="unbounded" minOccurs="0"/>
 *         &lt;element name="ExternalIdentifier" type="{urn:oasis:names:tc:ebxml-regrep:xsd:rim:4.0}ExternalIdentifierType" maxOccurs="unbounded" minOccurs="0"/>
 *         &lt;element name="ExternalLink" type="{urn:oasis:names:tc:ebxml-regrep:xsd:rim:4.0}ExternalLinkType" maxOccurs="unbounded" minOccurs="0"/>
 *       &lt;/sequence>
 *       &lt;attribute name="lid" type="{http://www.w3.org/2001/XMLSchema}string" />
 *       &lt;attribute name="objectType" type="{urn:oasis:names:tc:ebxml-regrep:xsd:rim:4.0}objectReferenceType" />
 *       &lt;attribute name="owner" type="{http://www.w3.org/2001/XMLSchema}string" />
 *       &lt;attribute name="status" type="{urn:oasis:names:tc:ebxml-regrep:xsd:rim:4.0}objectReferenceType" />
 *     &lt;/extension>
 *   &lt;/complexContent>
 * &lt;/complexType>
 * </pre>
 * 
 * 
 */
@XmlRootElement(name = "RegistryObject")
@XmlAccessorType(XmlAccessType.FIELD)
@XmlType(name = "RegistryObjectType", propOrder = { "name", "description",
        "versionInfo", "classification", "externalIdentifier", "externalLink" })
@XmlSeeAlso({ NotificationType.class, ServiceType.class,
        ServiceInterfaceType.class, RegistryType.class,
        ExtrinsicObjectType.class, FederationType.class,
        WorkflowActionType.class, ExternalLinkType.class,
        AssociationType.class, ExternalIdentifierType.class,
        ServiceEndpointType.class, ClassificationType.class,
        TaxonomyElementType.class, RegistryPackageType.class,
        QueryDefinitionType.class, SubscriptionType.class,
        AuditableEventType.class, ServiceBindingType.class, PartyType.class,
        RoleType.class })
@DynamicSerialize
@Entity
@Cache(region="registryObjects",usage = CacheConcurrencyStrategy.TRANSACTIONAL, include = "all")
@Inheritance(strategy = InheritanceType.TABLE_PER_CLASS)
@Table(name = "RegistryObject")
public class RegistryObjectType extends IdentifiableType {
    @XmlElement(name = "Name")
    @DynamicSerializeElement
    @OneToOne(cascade = CascadeType.ALL, fetch = FetchType.EAGER)
    protected InternationalStringType name;

    @XmlElement(name = "Description")
    @DynamicSerializeElement
    @OneToOne(cascade = CascadeType.ALL, fetch = FetchType.EAGER)
    protected InternationalStringType description;

    @XmlElement(name = "VersionInfo")
    @DynamicSerializeElement
    @Cascade(value = { org.hibernate.annotations.CascadeType.DETACH })
    @ManyToOne
    protected VersionInfoType versionInfo;

    @XmlElement(name = "Classification")
    @DynamicSerializeElement
    @Cascade(value = { org.hibernate.annotations.CascadeType.SAVE_UPDATE,
            org.hibernate.annotations.CascadeType.DETACH })
    @ManyToMany
    protected Set<ClassificationType> classification;

    @XmlElement(name = "ExternalIdentifier")
    @DynamicSerializeElement
    @Cascade(value = { org.hibernate.annotations.CascadeType.SAVE_UPDATE,
            org.hibernate.annotations.CascadeType.DETACH })
    @ManyToMany
    protected Set<ExternalIdentifierType> externalIdentifier;

    @XmlElement(name = "ExternalLink")
    @DynamicSerializeElement
    @Cascade(value = { org.hibernate.annotations.CascadeType.SAVE_UPDATE,
            org.hibernate.annotations.CascadeType.DETACH })
    @ManyToMany
    protected Set<ExternalLinkType> externalLink;

    @XmlAttribute
    @DynamicSerializeElement
    @Index(name = "LID_Index")
    protected String lid;

    @XmlAttribute
    @DynamicSerializeElement
    @Index(name = "RegistryObjectType_objectType_idx")
    protected String objectType;

    @XmlAttribute
    @DynamicSerializeElement
    protected String owner;

    @XmlAttribute
    @DynamicSerializeElement
    protected String status;

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
     * Gets the value of the versionInfo property.
     * 
     * @return possible object is {@link VersionInfoType }
     * 
     */
    public VersionInfoType getVersionInfo() {
        return versionInfo;
    }

    /**
     * Sets the value of the versionInfo property.
     * 
     * @param value
     *            allowed object is {@link VersionInfoType }
     * 
     */
    public void setVersionInfo(VersionInfoType value) {
        this.versionInfo = value;
    }

    /**
     * Gets the value of the classification property.
     * 
     * <p>
     * This accessor method returns a reference to the live list, not a
     * snapshot. Therefore any modification you make to the returned list will
     * be present inside the JAXB object. This is why there is not a
     * <CODE>set</CODE> method for the classification property.
     * 
     * <p>
     * For example, to add a new item, do as follows:
     * 
     * <pre>
     * getClassification().add(newItem);
     * </pre>
     * 
     * 
     * <p>
     * Objects of the following type(s) are allowed in the list
     * {@link ClassificationType }
     * 
     * 
     */
    public Set<ClassificationType> getClassification() {
        if (classification == null) {
            classification = new HashSet<ClassificationType>();
        }
        return this.classification;
    }

    public void setClassification(Set<ClassificationType> classification) {
        this.classification = classification;
    }

    /**
     * Gets the value of the externalIdentifier property.
     * 
     * <p>
     * This accessor method returns a reference to the live list, not a
     * snapshot. Therefore any modification you make to the returned list will
     * be present inside the JAXB object. This is why there is not a
     * <CODE>set</CODE> method for the externalIdentifier property.
     * 
     * <p>
     * For example, to add a new item, do as follows:
     * 
     * <pre>
     * getExternalIdentifier().add(newItem);
     * </pre>
     * 
     * 
     * <p>
     * Objects of the following type(s) are allowed in the list
     * {@link ExternalIdentifierType }
     * 
     * 
     */
    public Set<ExternalIdentifierType> getExternalIdentifier() {
        if (externalIdentifier == null) {
            externalIdentifier = new HashSet<ExternalIdentifierType>();
        }
        return this.externalIdentifier;
    }

    public void setExternalIdentifier(
            Set<ExternalIdentifierType> externalIdentifier) {
        this.externalIdentifier = externalIdentifier;
    }

    /**
     * Gets the value of the externalLink property.
     * 
     * <p>
     * This accessor method returns a reference to the live list, not a
     * snapshot. Therefore any modification you make to the returned list will
     * be present inside the JAXB object. This is why there is not a
     * <CODE>set</CODE> method for the externalLink property.
     * 
     * <p>
     * For example, to add a new item, do as follows:
     * 
     * <pre>
     * getExternalLink().add(newItem);
     * </pre>
     * 
     * 
     * <p>
     * Objects of the following type(s) are allowed in the list
     * {@link ExternalLinkType }
     * 
     * 
     */
    public Set<ExternalLinkType> getExternalLink() {
        if (externalLink == null) {
            externalLink = new HashSet<ExternalLinkType>();
        }
        return this.externalLink;
    }

    public void setExternalLink(Set<ExternalLinkType> externalLink) {
        this.externalLink = externalLink;
    }

    /**
     * Gets the value of the lid property.
     * 
     * @return possible object is {@link String }
     * 
     */
    public String getLid() {
        return lid;
    }

    /**
     * Sets the value of the lid property.
     * 
     * @param value
     *            allowed object is {@link String }
     * 
     */
    public void setLid(String value) {
        this.lid = value;
    }

    /**
     * Gets the value of the objectType property.
     * 
     * @return possible object is {@link String }
     * 
     */
    public String getObjectType() {
        return objectType;
    }

    /**
     * Sets the value of the objectType property.
     * 
     * @param value
     *            allowed object is {@link String }
     * 
     */
    public void setObjectType(String value) {
        this.objectType = value;
    }

    /**
     * Gets the value of the owner property.
     * 
     * @return possible object is {@link String }
     * 
     */
    public String getOwner() {
        return owner;
    }

    /**
     * Sets the value of the owner property.
     * 
     * @param value
     *            allowed object is {@link String }
     * 
     */
    public void setOwner(String value) {
        this.owner = value;
    }

    /**
     * Gets the value of the status property.
     * 
     * @return possible object is {@link String }
     * 
     */
    public String getStatus() {
        return status;
    }

    /**
     * Sets the value of the status property.
     * 
     * @param value
     *            allowed object is {@link String }
     * 
     */
    public void setStatus(String value) {
        this.status = value;
    }

}
