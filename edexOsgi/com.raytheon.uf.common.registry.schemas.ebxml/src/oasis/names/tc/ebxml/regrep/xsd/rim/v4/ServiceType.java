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
import javax.persistence.Entity;
import javax.persistence.ManyToMany;
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
 * Represents a service in ebRIM. Matches service as defined in WSDL 2.
 * 
 * 
 * <p>
 * Java class for ServiceType complex type.
 * 
 * <p>
 * The following schema fragment specifies the expected content contained within
 * this class.
 * 
 * <pre>
 * &lt;complexType name="ServiceType">
 *   &lt;complexContent>
 *     &lt;extension base="{urn:oasis:names:tc:ebxml-regrep:xsd:rim:4.0}RegistryObjectType">
 *       &lt;sequence>
 *         &lt;element name="ServiceEndpoint" type="{urn:oasis:names:tc:ebxml-regrep:xsd:rim:4.0}ServiceEndpointType" maxOccurs="unbounded" minOccurs="0"/>
 *       &lt;/sequence>
 *       &lt;attribute name="serviceInterface" type="{urn:oasis:names:tc:ebxml-regrep:xsd:rim:4.0}objectReferenceType" />
 *     &lt;/extension>
 *   &lt;/complexContent>
 * &lt;/complexType>
 * </pre>
 * 
 * 
 */
@XmlRootElement
@XmlAccessorType(XmlAccessType.FIELD)
@XmlType(name = "ServiceType", propOrder = { "serviceEndpoint" })
@DynamicSerialize
@Entity
@Cache(region="registryObjects",usage = CacheConcurrencyStrategy.TRANSACTIONAL)
@Table(name = "Service")
public class ServiceType extends RegistryObjectType {

    @ManyToMany(cascade = CascadeType.ALL)
    @XmlElement(name = "ServiceEndpoint")
    @DynamicSerializeElement
    protected List<ServiceEndpointType> serviceEndpoint;

    @XmlAttribute
    @DynamicSerializeElement
    protected String serviceInterface;

    /**
     * Gets the value of the serviceEndpoint property.
     * 
     * <p>
     * This accessor method returns a reference to the live list, not a
     * snapshot. Therefore any modification you make to the returned list will
     * be present inside the JAXB object. This is why there is not a
     * <CODE>set</CODE> method for the serviceEndpoint property.
     * 
     * <p>
     * For example, to add a new item, do as follows:
     * 
     * <pre>
     * getServiceEndpoint().add(newItem);
     * </pre>
     * 
     * 
     * <p>
     * Objects of the following type(s) are allowed in the list
     * {@link ServiceEndpointType }
     * 
     * 
     */
    public List<ServiceEndpointType> getServiceEndpoint() {
        if (serviceEndpoint == null) {
            serviceEndpoint = new ArrayList<ServiceEndpointType>();
        }
        return this.serviceEndpoint;
    }

    public void setServiceEndpoint(List<ServiceEndpointType> serviceEndpoint) {
        this.serviceEndpoint = serviceEndpoint;
    }

    /**
     * Gets the value of the serviceInterface property.
     * 
     * @return possible object is {@link String }
     * 
     */
    public String getServiceInterface() {
        return serviceInterface;
    }

    /**
     * Sets the value of the serviceInterface property.
     * 
     * @param value
     *            allowed object is {@link String }
     * 
     */
    public void setServiceInterface(String value) {
        this.serviceInterface = value;
    }

}
