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

package oasis.names.tc.ebxml.regrep.xsd.rs.v4;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.List;

import javax.persistence.CascadeType;
import javax.persistence.Entity;
import javax.persistence.FetchType;
import javax.persistence.JoinColumn;
import javax.persistence.OneToMany;
import javax.persistence.OneToOne;
import javax.persistence.Table;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.bind.annotation.XmlSchemaType;
import javax.xml.bind.annotation.XmlSeeAlso;
import javax.xml.bind.annotation.XmlType;

import oasis.names.tc.ebxml.regrep.xsd.query.v4.QueryResponse;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.ExtensibleObjectType;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.ObjectRefListType;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.ObjectRefType;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.RegistryObjectListType;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.RegistryObjectType;
import oasis.names.tc.ebxml.regrep.xsd.spi.v4.CatalogObjectsResponse;
import oasis.names.tc.ebxml.regrep.xsd.spi.v4.FilterObjectsResponse;
import oasis.names.tc.ebxml.regrep.xsd.spi.v4.ValidateObjectsResponse;

import org.hibernate.annotations.Cache;
import org.hibernate.annotations.CacheConcurrencyStrategy;

import com.raytheon.uf.common.registry.EbxmlNamespaces;
import com.raytheon.uf.common.registry.RegrepUtil;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;
import com.raytheon.uf.common.util.CollectionUtil;

/**
 * Base type for all ebXML Registry responses
 * 
 * <p>
 * Java class for RegistryResponseType complex type.
 * 
 * <p>
 * The following schema fragment specifies the expected content contained within
 * this class.
 * 
 * <pre>
 * &lt;complexType name="RegistryResponseType">
 *   &lt;complexContent>
 *     &lt;extension base="{urn:oasis:names:tc:ebxml-regrep:xsd:rim:4.0}ExtensibleObjectType">
 *       &lt;sequence>
 *         &lt;element name="Exception" type="{urn:oasis:names:tc:ebxml-regrep:xsd:rs:4.0}RegistryExceptionType" maxOccurs="unbounded" minOccurs="0"/>
 *         &lt;element ref="{urn:oasis:names:tc:ebxml-regrep:xsd:rim:4.0}RegistryObjectList" minOccurs="0"/>
 *         &lt;element ref="{urn:oasis:names:tc:ebxml-regrep:xsd:rim:4.0}ObjectRefList" minOccurs="0"/>
 *       &lt;/sequence>
 *       &lt;attribute name="status" use="required" type="{urn:oasis:names:tc:ebxml-regrep:xsd:rim:4.0}objectReferenceType" />
 *       &lt;attribute name="requestId" type="{http://www.w3.org/2001/XMLSchema}anyURI" />
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
 * 
 */
@XmlRootElement(name = "RegistryResponse")
@XmlAccessorType(XmlAccessType.FIELD)
@XmlType(name = "RegistryResponseType", propOrder = { "exception",
        "registryObjectList", "objectRefList" })
@XmlSeeAlso({ ValidateObjectsResponse.class, CatalogObjectsResponse.class,
        FilterObjectsResponse.class, QueryResponse.class })
@DynamicSerialize
@Entity
@Cache(region = RegrepUtil.DB_CACHE_REGION, usage = CacheConcurrencyStrategy.TRANSACTIONAL)
@Table(schema = RegrepUtil.EBXML_SCHEMA, name = "RegistryResponse")
public class RegistryResponseType extends ExtensibleObjectType {

    private static final long serialVersionUID = 3258800433937559672L;

    @XmlElement(name = "Exception")
    @DynamicSerializeElement
    @OneToMany(cascade = CascadeType.ALL, fetch = FetchType.LAZY)
    @JoinColumn(name = "exception_id", nullable = false, referencedColumnName = "id")
    protected List<RegistryExceptionType> exception;

    @XmlElement(name = "RegistryObjectList", namespace = EbxmlNamespaces.RIM_URI)
    @DynamicSerializeElement
    @OneToOne(cascade = CascadeType.ALL)
    protected RegistryObjectListType registryObjectList;

    @XmlElement(name = "ObjectRefList", namespace = EbxmlNamespaces.RIM_URI)
    @DynamicSerializeElement
    @OneToOne(cascade = CascadeType.ALL)
    protected ObjectRefListType objectRefList;

    @XmlAttribute(required = true)
    @DynamicSerializeElement
    protected RegistryResponseStatus status;

    @XmlAttribute
    @XmlSchemaType(name = "anyURI")
    @DynamicSerializeElement
    protected String requestId;

    public RegistryResponseType() {
        super();
    }

    public boolean isOk() {
        return status.equals(RegistryResponseStatus.SUCCESS);
    }

    public List<RegistryObjectType> getRegistryObjects() {
        if (registryObjectList == null) {
            this.registryObjectList = new RegistryObjectListType();
        }
        return registryObjectList.getRegistryObject();
    }

    public void addRegistryObjects(Collection<RegistryObjectType> regObjs) {
        if (regObjs == null || regObjs.isEmpty()) {
            return;
        }
        if (registryObjectList == null) {
            registryObjectList = new RegistryObjectListType();
        }
        registryObjectList.getRegistryObject().addAll(regObjs);
    }

    public List<ObjectRefType> getObjectRefs() {
        if (objectRefList == null) {
            return Collections.emptyList();
        }
        return objectRefList.getObjectRef();
    }

    public void addObjectRefs(Collection<ObjectRefType> objRefs) {
        if (CollectionUtil.isNullOrEmpty(objRefs)) {
            return;
        }
        if (objectRefList == null) {
            objectRefList = new ObjectRefListType();
        }
        objectRefList.getObjectRef().addAll(objRefs);
    }

    public void addObjectRef(ObjectRefType objRef) {
        if (objectRefList == null) {
            objectRefList = new ObjectRefListType();
        }
        objectRefList.getObjectRef().add(objRef);
    }

    /**
     * Gets the value of the exception property.
     * 
     * <p>
     * This accessor method returns a reference to the live list, not a
     * snapshot. Therefore any modification you make to the returned list will
     * be present inside the JAXB object. This is why there is not a
     * <CODE>set</CODE> method for the exception property.
     * 
     * <p>
     * For example, to add a new item, do as follows:
     * 
     * <pre>
     * getException().add(newItem);
     * </pre>
     * 
     * 
     * <p>
     * Objects of the following type(s) are allowed in the list
     * {@link RegistryExceptionType }
     * 
     * 
     */
    public List<RegistryExceptionType> getException() {
        if (exception == null) {
            exception = new ArrayList<RegistryExceptionType>();
        }
        return this.exception;
    }

    public void setException(List<RegistryExceptionType> exception) {
        this.exception = exception;
    }

    /**
     * Gets the value of the registryObjectList property.
     * 
     * @return possible object is {@link RegistryObjectListType }
     * 
     */
    public RegistryObjectListType getRegistryObjectList() {
        return registryObjectList;
    }

    /**
     * Sets the value of the registryObjectList property.
     * 
     * @param value
     *            allowed object is {@link RegistryObjectListType }
     * 
     */
    public void setRegistryObjectList(RegistryObjectListType value) {
        this.registryObjectList = value;
    }

    /**
     * Gets the value of the objectRefList property.
     * 
     * @return possible object is {@link ObjectRefListType }
     * 
     */
    public ObjectRefListType getObjectRefList() {
        return objectRefList;
    }

    /**
     * Sets the value of the objectRefList property.
     * 
     * @param value
     *            allowed object is {@link ObjectRefListType }
     * 
     */
    public void setObjectRefList(ObjectRefListType value) {
        this.objectRefList = value;
    }

    /**
     * Gets the value of the status property.
     * 
     * @return possible object is {@link RegistryResponseStatus }
     * 
     */
    public RegistryResponseStatus getStatus() {
        return status;
    }

    /**
     * Sets the value of the status property.
     * 
     * @param value
     *            allowed object is {@link RegistryResponseStatus }
     * 
     */
    public void setStatus(RegistryResponseStatus value) {
        this.status = value;
    }

    /**
     * Gets the value of the requestId property.
     * 
     * @return possible object is {@link String }
     * 
     */
    public String getRequestId() {
        return requestId;
    }

    /**
     * Sets the value of the requestId property.
     * 
     * @param value
     *            allowed object is {@link String }
     * 
     */
    public void setRequestId(String value) {
        this.requestId = value;
    }

    @Override
    public int hashCode() {
        final int prime = 31;
        int result = super.hashCode();
        result = prime * result
                + ((exception == null) ? 0 : exception.hashCode());
        result = prime * result
                + ((objectRefList == null) ? 0 : objectRefList.hashCode());
        result = prime
                * result
                + ((registryObjectList == null) ? 0 : registryObjectList
                        .hashCode());
        result = prime * result
                + ((requestId == null) ? 0 : requestId.hashCode());
        result = prime * result + ((status == null) ? 0 : status.hashCode());
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
        RegistryResponseType other = (RegistryResponseType) obj;
        if (exception == null) {
            if (other.exception != null)
                return false;
        } else if (!exception.equals(other.exception))
            return false;
        if (objectRefList == null) {
            if (other.objectRefList != null)
                return false;
        } else if (!objectRefList.equals(other.objectRefList))
            return false;
        if (registryObjectList == null) {
            if (other.registryObjectList != null)
                return false;
        } else if (!registryObjectList.equals(other.registryObjectList))
            return false;
        if (requestId == null) {
            if (other.requestId != null)
                return false;
        } else if (!requestId.equals(other.requestId))
            return false;
        if (status != other.status)
            return false;
        return true;
    }

    @Override
    public String toString() {
        StringBuilder builder = new StringBuilder();
        builder.append("RegistryResponseType \n[id=");
        builder.append(id);
        builder.append(", \nslot=");
        builder.append(slot);
        builder.append(", \nexception=");
        builder.append(exception);
        builder.append(", \nregistryObjectList=");
        builder.append(registryObjectList);
        builder.append(", \nobjectRefList=");
        builder.append(objectRefList);
        builder.append(", \nstatus=");
        builder.append(status);
        builder.append(", \nrequestId=");
        builder.append(requestId);
        builder.append("]");
        return builder.toString();
    }

}
