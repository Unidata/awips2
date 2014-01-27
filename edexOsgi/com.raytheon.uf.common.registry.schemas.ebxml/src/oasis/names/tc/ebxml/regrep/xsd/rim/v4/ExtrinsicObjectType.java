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

import java.util.Arrays;

import javax.activation.DataHandler;
import javax.persistence.AttributeOverride;
import javax.persistence.AttributeOverrides;
import javax.persistence.Column;
import javax.persistence.Embedded;
import javax.persistence.Entity;
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

import com.raytheon.uf.common.registry.RegrepUtil;
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
@XmlRootElement(name = "ExtrinsicObject")
@XmlAccessorType(XmlAccessType.FIELD)
@XmlType(name = "ExtrinsicObjectType", propOrder = { "contentVersionInfo",
        "repositoryItemRef", "repositoryItem" })
@XmlSeeAlso({ CommentType.class })
@DynamicSerialize
@Entity
@Cache(region = RegrepUtil.DB_CACHE_REGION, usage = CacheConcurrencyStrategy.TRANSACTIONAL)
@Table(schema = RegrepUtil.EBXML_SCHEMA, name = "ExtrinsicObject")
public class ExtrinsicObjectType extends RegistryObjectType {

    private static final long serialVersionUID = -2572225450725363471L;

    @XmlElement(name = "ContentVersionInfo")
    @DynamicSerializeElement
    @AttributeOverrides({
            @AttributeOverride(name = "versionName", column = @Column(name = "contentVersionName")),
            @AttributeOverride(name = "userVersionName", column = @Column(name = "contentUserVersionName")) })
    @Embedded
    protected VersionInfoType contentVersionInfo;

    @XmlElement(name = "RepositoryItemRef")
    @DynamicSerializeElement
    @Embedded
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

    public ExtrinsicObjectType() {
        super();

    }

    public ExtrinsicObjectType(String id, String lid, String objectType,
            String owner, String status, String name, String description) {
        super(id, lid, objectType, owner, status, name, description);

    }

    public ExtrinsicObjectType(String id, String lid) {
        super(id, lid);

    }

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

    @Override
    public int hashCode() {
        final int prime = 31;
        int result = super.hashCode();
        result = prime
                * result
                + ((contentVersionInfo == null) ? 0 : contentVersionInfo
                        .hashCode());
        result = prime * result
                + ((mimeType == null) ? 0 : mimeType.hashCode());
        result = prime * result + Arrays.hashCode(repositoryItem);
        result = prime
                * result
                + ((repositoryItemRef == null) ? 0 : repositoryItemRef
                        .hashCode());
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
        ExtrinsicObjectType other = (ExtrinsicObjectType) obj;
        if (contentVersionInfo == null) {
            if (other.contentVersionInfo != null)
                return false;
        } else if (!contentVersionInfo.equals(other.contentVersionInfo))
            return false;
        if (mimeType == null) {
            if (other.mimeType != null)
                return false;
        } else if (!mimeType.equals(other.mimeType))
            return false;
        if (!Arrays.equals(repositoryItem, other.repositoryItem))
            return false;
        if (repositoryItemRef == null) {
            if (other.repositoryItemRef != null)
                return false;
        } else if (!repositoryItemRef.equals(other.repositoryItemRef))
            return false;
        return true;
    }

    @Override
    public String toString() {
        StringBuilder builder = new StringBuilder();
        builder.append("ExtrinsicObjectType \n[name=");
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
        builder.append(", \ncontentVersionInfo=");
        builder.append(contentVersionInfo);
        builder.append(", \nrepositoryItemRef=");
        builder.append(repositoryItemRef);
        builder.append(", \nrepositoryItem=");
        builder.append(Arrays.toString(repositoryItem));
        builder.append(", \nmimeType=");
        builder.append(mimeType);
        builder.append("]");
        return builder.toString();
    }

}
