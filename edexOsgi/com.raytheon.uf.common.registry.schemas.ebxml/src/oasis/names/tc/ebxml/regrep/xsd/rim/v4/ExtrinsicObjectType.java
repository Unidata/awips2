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

import javax.activation.DataHandler;
import javax.persistence.CascadeType;
import javax.persistence.Entity;
import javax.persistence.ManyToOne;
import javax.persistence.Table;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlMimeType;
import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.bind.annotation.XmlSeeAlso;
import javax.xml.bind.annotation.XmlType;

import org.hibernate.annotations.Cache;
import org.hibernate.annotations.CacheConcurrencyStrategy;

import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;

/**
 * 
 * Metadata data type capable of having repository content associated with it as
 * a repository item. Often used as base type for extended types defines by
 * profiles of ebXML RegRep.
 * 
 * 
 * <p>
 * Java class for ExtrinsicObjectType complex type.
 * 
 * <p>
 * The following schema fragment specifies the expected content contained within
 * this class.
 * 
 * <pre>
 * &lt;complexType name="ExtrinsicObjectType">
 *   &lt;complexContent>
 *     &lt;extension base="{urn:oasis:names:tc:ebxml-regrep:xsd:rim:4.0}RegistryObjectType">
 *       &lt;sequence>
 *         &lt;element name="ContentVersionInfo" type="{urn:oasis:names:tc:ebxml-regrep:xsd:rim:4.0}VersionInfoType" minOccurs="0"/>
 *         &lt;choice minOccurs="0">
 *           &lt;element name="RepositoryItemRef" type="{urn:oasis:names:tc:ebxml-regrep:xsd:rim:4.0}SimpleLinkType"/>
 *           &lt;element name="RepositoryItem" type="{http://www.w3.org/2001/XMLSchema}base64Binary"/>
 *         &lt;/choice>
 *       &lt;/sequence>
 *       &lt;attribute name="mimeType" type="{urn:oasis:names:tc:ebxml-regrep:xsd:rim:4.0}LongText" />
 *     &lt;/extension>
 *   &lt;/complexContent>
 * &lt;/complexType>
 * </pre>
 * 
 * 
 */
@XmlRootElement
@XmlAccessorType(XmlAccessType.FIELD)
@XmlType(name = "ExtrinsicObjectType", propOrder = { "contentVersionInfo",
        "repositoryItemRef", "repositoryItem" })
@XmlSeeAlso({ CommentType.class })
@DynamicSerialize
@Entity
@Cache(region="registryObjects",usage = CacheConcurrencyStrategy.TRANSACTIONAL)
@Table(name = "ExtrinsicObject")
public class ExtrinsicObjectType extends RegistryObjectType {

    @XmlElement(name = "ContentVersionInfo")
    @DynamicSerializeElement
    @ManyToOne(cascade = CascadeType.ALL)
    protected VersionInfoType contentVersionInfo;

    @XmlElement(name = "RepositoryItemRef")
    @DynamicSerializeElement
    @ManyToOne(cascade = CascadeType.ALL)
    protected SimpleLinkType repositoryItemRef;

    @XmlElement(name = "RepositoryItem")
    @XmlMimeType("application/octet-stream")
    @DynamicSerializeElement
    protected byte[] repositoryItem;

    // Hiberate annotations
    // @Column
    // @Type(type =
    // "com.raytheon.uf.common.registry.schemas.ebxml.util.Base64EncodedType")
    // protected byte[] repositoryItem;

    @XmlAttribute
    @DynamicSerializeElement
    protected String mimeType;

    /**
     * Gets the value of the contentVersionInfo property.
     * 
     * @return possible object is {@link VersionInfoType }
     * 
     */
    public VersionInfoType getContentVersionInfo() {
        return contentVersionInfo;
    }

    /**
     * Sets the value of the contentVersionInfo property.
     * 
     * @param value
     *            allowed object is {@link VersionInfoType }
     * 
     */
    public void setContentVersionInfo(VersionInfoType value) {
        this.contentVersionInfo = value;
    }

    /**
     * Gets the value of the repositoryItemRef property.
     * 
     * @return possible object is {@link SimpleLinkType }
     * 
     */
    public SimpleLinkType getRepositoryItemRef() {
        return repositoryItemRef;
    }

    /**
     * Sets the value of the repositoryItemRef property.
     * 
     * @param value
     *            allowed object is {@link SimpleLinkType }
     * 
     */
    public void setRepositoryItemRef(SimpleLinkType value) {
        this.repositoryItemRef = value;
    }

    /**
     * Gets the value of the repositoryItem property.
     * 
     * @return possible object is {@link DataHandler }
     * 
     */
    public byte[] getRepositoryItem() {
        return repositoryItem;
    }

    /**
     * Sets the value of the repositoryItem property.
     * 
     * @param value
     *            allowed object is {@link DataHandler }
     * 
     */
    public void setRepositoryItem(byte[] value) {
        this.repositoryItem = value;
    }

    /**
     * Gets the value of the mimeType property.
     * 
     * @return possible object is {@link String }
     * 
     */
    public String getMimeType() {
        return mimeType;
    }

    /**
     * Sets the value of the mimeType property.
     * 
     * @param value
     *            allowed object is {@link String }
     * 
     */
    public void setMimeType(String value) {
        this.mimeType = value;
    }

}
