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
import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.bind.annotation.XmlSchemaType;
import javax.xml.bind.annotation.XmlType;
import javax.xml.bind.annotation.adapters.CollapsedStringAdapter;
import javax.xml.bind.annotation.adapters.XmlJavaTypeAdapter;
import javax.xml.datatype.Duration;

import org.hibernate.annotations.Cache;
import org.hibernate.annotations.CacheConcurrencyStrategy;
import org.hibernate.annotations.Type;

import com.raytheon.uf.common.registry.RegrepUtil;
import com.raytheon.uf.common.registry.schemas.ebxml.util.annotations.RegistryObjectReference;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;

/**
 * Represents an ebXML RegRep server in ebRIM.
 * 
 * <p>
 * Java class for RegistryType complex type.
 * 
 * <p>
 * The following schema fragment specifies the expected content contained within
 * this class.
 * 
 * <pre>
 * &lt;complexType name="RegistryType">
 *   &lt;complexContent>
 *     &lt;extension base="{urn:oasis:names:tc:ebxml-regrep:xsd:rim:4.0}RegistryObjectType">
 *       &lt;attribute name="baseURL" use="required" type="{http://www.w3.org/2001/XMLSchema}anyURI" />
 *       &lt;attribute name="operator" use="required" type="{urn:oasis:names:tc:ebxml-regrep:xsd:rim:4.0}objectReferenceType" />
 *       &lt;attribute name="specificationVersion" use="required" type="{http://www.w3.org/2001/XMLSchema}string" />
 *       &lt;attribute name="replicationSyncLatency" type="{http://www.w3.org/2001/XMLSchema}duration" default="P1D" />
 *       &lt;attribute name="catalogingLatency" type="{http://www.w3.org/2001/XMLSchema}duration" default="PT0S" />
 *       &lt;attribute name="conformanceProfile" default="RegistryLite">
 *         &lt;simpleType>
 *           &lt;restriction base="{http://www.w3.org/2001/XMLSchema}NCName">
 *             &lt;enumeration value="RegistryFull"/>
 *             &lt;enumeration value="RegistryLite"/>
 *           &lt;/restriction>
 *         &lt;/simpleType>
 *       &lt;/attribute>
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
@XmlRootElement(name = "Registry")
@XmlAccessorType(XmlAccessType.FIELD)
@XmlType(name = "RegistryType")
@DynamicSerialize
@Entity
@Cache(region = RegrepUtil.DB_CACHE_REGION, usage = CacheConcurrencyStrategy.TRANSACTIONAL)
@Table(schema = RegrepUtil.EBXML_SCHEMA, name = "Registry")
public class RegistryType extends RegistryObjectType {

    private static final long serialVersionUID = -7896396916703130423L;

    @XmlAttribute(required = true)
    @XmlSchemaType(name = "anyURI")
    @DynamicSerializeElement
    protected String baseURL;

    @XmlAttribute(required = true)
    @DynamicSerializeElement
    @RegistryObjectReference
    protected String operator;

    @XmlAttribute(required = true)
    @DynamicSerializeElement
    protected String specificationVersion;

    @Column
    @Type(type = "com.raytheon.uf.common.registry.schemas.ebxml.util.DurationType")
    @XmlAttribute
    @DynamicSerializeElement
    protected Duration replicationSyncLatency;

    @Column
    @Type(type = "com.raytheon.uf.common.registry.schemas.ebxml.util.DurationType")
    @XmlAttribute
    @DynamicSerializeElement
    protected Duration catalogingLatency;

    @XmlAttribute
    @XmlJavaTypeAdapter(CollapsedStringAdapter.class)
    @DynamicSerializeElement
    protected String conformanceProfile;

    public RegistryType() {
        super();

    }

    public RegistryType(String id, String lid, String objectType, String owner,
            String status, String name, String description) {
        super(id, lid, objectType, owner, status, name, description);

    }

    public RegistryType(String id, String lid) {
        super(id, lid);

    }

    /**
     * Gets the value of the baseURL property.
     * 
     * @return possible object is {@link String }
     * 
     */
    public String getBaseURL() {
        return baseURL;
    }

    /**
     * Sets the value of the baseURL property.
     * 
     * @param value
     *            allowed object is {@link String }
     * 
     */
    public void setBaseURL(String value) {
        this.baseURL = value;
    }

    /**
     * Gets the value of the operator property.
     * 
     * @return possible object is {@link String }
     * 
     */
    public String getOperator() {
        return operator;
    }

    /**
     * Sets the value of the operator property.
     * 
     * @param value
     *            allowed object is {@link String }
     * 
     */
    public void setOperator(String value) {
        this.operator = value;
    }

    /**
     * Gets the value of the specificationVersion property.
     * 
     * @return possible object is {@link String }
     * 
     */
    public String getSpecificationVersion() {
        return specificationVersion;
    }

    /**
     * Sets the value of the specificationVersion property.
     * 
     * @param value
     *            allowed object is {@link String }
     * 
     */
    public void setSpecificationVersion(String value) {
        this.specificationVersion = value;
    }

    /**
     * Gets the value of the replicationSyncLatency property.
     * 
     * @return possible object is {@link Duration }
     * 
     */
    public Duration getReplicationSyncLatency() {
        return replicationSyncLatency;
    }

    /**
     * Sets the value of the replicationSyncLatency property.
     * 
     * @param value
     *            allowed object is {@link Duration }
     * 
     */
    public void setReplicationSyncLatency(Duration value) {
        this.replicationSyncLatency = value;
    }

    /**
     * Gets the value of the catalogingLatency property.
     * 
     * @return possible object is {@link Duration }
     * 
     */
    public Duration getCatalogingLatency() {
        return catalogingLatency;
    }

    /**
     * Sets the value of the catalogingLatency property.
     * 
     * @param value
     *            allowed object is {@link Duration }
     * 
     */
    public void setCatalogingLatency(Duration value) {
        this.catalogingLatency = value;
    }

    /**
     * Gets the value of the conformanceProfile property.
     * 
     * @return possible object is {@link String }
     * 
     */
    public String getConformanceProfile() {
        if (conformanceProfile == null) {
            return "RegistryLite";
        } else {
            return conformanceProfile;
        }
    }

    /**
     * Sets the value of the conformanceProfile property.
     * 
     * @param value
     *            allowed object is {@link String }
     * 
     */
    public void setConformanceProfile(String value) {
        this.conformanceProfile = value;
    }

    @Override
    public int hashCode() {
        final int prime = 31;
        int result = super.hashCode();
        result = prime * result + ((baseURL == null) ? 0 : baseURL.hashCode());
        result = prime
                * result
                + ((catalogingLatency == null) ? 0 : catalogingLatency
                        .hashCode());
        result = prime
                * result
                + ((conformanceProfile == null) ? 0 : conformanceProfile
                        .hashCode());
        result = prime * result
                + ((operator == null) ? 0 : operator.hashCode());
        result = prime
                * result
                + ((replicationSyncLatency == null) ? 0
                        : replicationSyncLatency.hashCode());
        result = prime
                * result
                + ((specificationVersion == null) ? 0 : specificationVersion
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
        RegistryType other = (RegistryType) obj;
        if (baseURL == null) {
            if (other.baseURL != null)
                return false;
        } else if (!baseURL.equals(other.baseURL))
            return false;
        if (catalogingLatency == null) {
            if (other.catalogingLatency != null)
                return false;
        } else if (!catalogingLatency.equals(other.catalogingLatency))
            return false;
        if (conformanceProfile == null) {
            if (other.conformanceProfile != null)
                return false;
        } else if (!conformanceProfile.equals(other.conformanceProfile))
            return false;
        if (operator == null) {
            if (other.operator != null)
                return false;
        } else if (!operator.equals(other.operator))
            return false;
        if (replicationSyncLatency == null) {
            if (other.replicationSyncLatency != null)
                return false;
        } else if (!replicationSyncLatency.equals(other.replicationSyncLatency))
            return false;
        if (specificationVersion == null) {
            if (other.specificationVersion != null)
                return false;
        } else if (!specificationVersion.equals(other.specificationVersion))
            return false;
        return true;
    }

    @Override
    public String toString() {
        StringBuilder builder = new StringBuilder();
        builder.append("RegistryType \n[name=");
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
        builder.append(", \nbaseURL=");
        builder.append(baseURL);
        builder.append(", \noperator=");
        builder.append(operator);
        builder.append(", \nspecificationVersion=");
        builder.append(specificationVersion);
        builder.append(", \nreplicationSyncLatency=");
        builder.append(replicationSyncLatency);
        builder.append(", \ncatalogingLatency=");
        builder.append(catalogingLatency);
        builder.append(", \nconformanceProfile=");
        builder.append(conformanceProfile);
        builder.append("]");
        return builder.toString();
    }

}
