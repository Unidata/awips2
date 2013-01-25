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
import javax.persistence.Transient;
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
 * 
 */
@XmlRootElement
@XmlAccessorType(XmlAccessType.FIELD)
@XmlType(name = "RegistryType")
@DynamicSerialize
@Entity
@Cache(region="registryObjects",usage = CacheConcurrencyStrategy.TRANSACTIONAL)
@Table(name = "Registry")
public class RegistryType extends RegistryObjectType {

    @XmlAttribute(required = true)
    @XmlSchemaType(name = "anyURI")
    @DynamicSerializeElement
    protected String baseURL;

    @XmlAttribute(required = true)
    @DynamicSerializeElement
    protected String operator;

    @XmlAttribute(required = true)
    @DynamicSerializeElement
    protected String specificationVersion;

    @Transient
    @XmlAttribute
    @DynamicSerializeElement
    protected Duration replicationSyncLatency;

    @Transient
    @XmlAttribute
    @DynamicSerializeElement
    protected Duration catalogingLatency;

    @XmlAttribute
    @XmlJavaTypeAdapter(CollapsedStringAdapter.class)
    @DynamicSerializeElement
    protected String conformanceProfile;

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

}
