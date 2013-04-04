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
import javax.persistence.ManyToOne;
import javax.persistence.Table;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.bind.annotation.XmlType;

import org.hibernate.annotations.Cache;
import org.hibernate.annotations.CacheConcurrencyStrategy;

import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;

/**
 * 
 * Represents a link to any external content not managed by an ebXML RegRep.
 * 
 * 
 * <p>
 * Java class for ExternalLinkType complex type.
 * 
 * <p>
 * The following schema fragment specifies the expected content contained within
 * this class.
 * 
 * <pre>
 * &lt;complexType name="ExternalLinkType">
 *   &lt;complexContent>
 *     &lt;extension base="{urn:oasis:names:tc:ebxml-regrep:xsd:rim:4.0}RegistryObjectType">
 *       &lt;sequence>
 *         &lt;element name="ExternalRef" type="{urn:oasis:names:tc:ebxml-regrep:xsd:rim:4.0}SimpleLinkType"/>
 *       &lt;/sequence>
 *       &lt;attribute name="registryObject" type="{urn:oasis:names:tc:ebxml-regrep:xsd:rim:4.0}objectReferenceType" />
 *     &lt;/extension>
 *   &lt;/complexContent>
 * &lt;/complexType>
 * </pre>
 * 
 * 
 */
@XmlRootElement
@XmlAccessorType(XmlAccessType.FIELD)
@XmlType(name = "ExternalLinkType", propOrder = { "externalRef" })
@DynamicSerialize
@Entity
@Cache(region="registryObjects",usage = CacheConcurrencyStrategy.TRANSACTIONAL)
@Table(name = "ExternalLink")
public class ExternalLinkType extends RegistryObjectType {

    @XmlElement(name = "ExternalRef", required = true)
    @DynamicSerializeElement
    @ManyToOne(cascade = CascadeType.ALL)
    protected SimpleLinkType externalRef;

    @XmlAttribute
    @DynamicSerializeElement
    protected String registryObject;

    /**
     * Gets the value of the externalRef property.
     * 
     * @return possible object is {@link SimpleLinkType }
     * 
     */
    public SimpleLinkType getExternalRef() {
        return externalRef;
    }

    /**
     * Sets the value of the externalRef property.
     * 
     * @param value
     *            allowed object is {@link SimpleLinkType }
     * 
     */
    public void setExternalRef(SimpleLinkType value) {
        this.externalRef = value;
    }

    /**
     * Gets the value of the registryObject property.
     * 
     * @return possible object is {@link String }
     * 
     */
    public String getRegistryObject() {
        return registryObject;
    }

    /**
     * Sets the value of the registryObject property.
     * 
     * @param value
     *            allowed object is {@link String }
     * 
     */
    public void setRegistryObject(String value) {
        this.registryObject = value;
    }

}
