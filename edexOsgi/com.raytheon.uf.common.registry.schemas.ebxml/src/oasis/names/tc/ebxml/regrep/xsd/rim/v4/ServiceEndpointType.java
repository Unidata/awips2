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
import javax.xml.bind.annotation.XmlSchemaType;
import javax.xml.bind.annotation.XmlType;

import org.hibernate.annotations.Cache;
import org.hibernate.annotations.CacheConcurrencyStrategy;

import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;

/**
 * 
 * Represents a service endpoint in ebRIM. Matches endpoint as defined in WSDL
 * 2.
 * 
 * 
 * <p>
 * Java class for ServiceEndpointType complex type.
 * 
 * <p>
 * The following schema fragment specifies the expected content contained within
 * this class.
 * 
 * <pre>
 * &lt;complexType name="ServiceEndpointType">
 *   &lt;complexContent>
 *     &lt;extension base="{urn:oasis:names:tc:ebxml-regrep:xsd:rim:4.0}RegistryObjectType">
 *       &lt;attribute name="address" type="{http://www.w3.org/2001/XMLSchema}anyURI" />
 *       &lt;attribute name="serviceBinding" type="{urn:oasis:names:tc:ebxml-regrep:xsd:rim:4.0}objectReferenceType" />
 *     &lt;/extension>
 *   &lt;/complexContent>
 * &lt;/complexType>
 * </pre>
 * 
 * 
 */
@XmlRootElement
@XmlAccessorType(XmlAccessType.FIELD)
@XmlType(name = "ServiceEndpointType")
@DynamicSerialize
@Entity
@Cache(region="registryObjects",usage = CacheConcurrencyStrategy.TRANSACTIONAL)
@Table(name = "ServiceEndpoint")
public class ServiceEndpointType extends RegistryObjectType {

    @XmlAttribute
    @XmlSchemaType(name = "anyURI")
    @DynamicSerializeElement
    protected String address;

    @XmlAttribute
    @DynamicSerializeElement
    protected String serviceBinding;

    /**
     * Gets the value of the address property.
     * 
     * @return possible object is {@link String }
     * 
     */
    public String getAddress() {
        return address;
    }

    /**
     * Sets the value of the address property.
     * 
     * @param value
     *            allowed object is {@link String }
     * 
     */
    public void setAddress(String value) {
        this.address = value;
    }

    /**
     * Gets the value of the serviceBinding property.
     * 
     * @return possible object is {@link String }
     * 
     */
    public String getServiceBinding() {
        return serviceBinding;
    }

    /**
     * Sets the value of the serviceBinding property.
     * 
     * @param value
     *            allowed object is {@link String }
     * 
     */
    public void setServiceBinding(String value) {
        this.serviceBinding = value;
    }

}
