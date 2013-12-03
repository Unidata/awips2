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

package oasis.names.tc.ebxml.regrep.xsd.spi.v4;

import java.util.ArrayList;
import java.util.List;

import javax.persistence.CascadeType;
import javax.persistence.Entity;
import javax.persistence.JoinColumn;
import javax.persistence.OneToOne;
import javax.persistence.Table;
import javax.persistence.Transient;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.bind.annotation.XmlType;

import oasis.names.tc.ebxml.regrep.xsd.rim.v4.ExtrinsicObjectType;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.ObjectRefListType;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.ObjectRefType;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.QueryType;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.RegistryObjectListType;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.SlotType;
import oasis.names.tc.ebxml.regrep.xsd.rs.v4.RegistryRequestType;

import org.hibernate.annotations.Cache;
import org.hibernate.annotations.CacheConcurrencyStrategy;

import com.raytheon.uf.common.registry.EbxmlNamespaces;
import com.raytheon.uf.common.registry.RegrepUtil;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;

/**
 * <p>
 * Java class for anonymous complex type.
 * 
 * <p>
 * The following schema fragment specifies the expected content contained within
 * this class.
 * 
 * <pre>
 * &lt;complexType>
 *   &lt;complexContent>
 *     &lt;extension base="{urn:oasis:names:tc:ebxml-regrep:xsd:rs:4.0}RegistryRequestType">
 *       &lt;sequence>
 *         &lt;element name="Query" type="{urn:oasis:names:tc:ebxml-regrep:xsd:rim:4.0}QueryType" minOccurs="0"/>
 *         &lt;element ref="{urn:oasis:names:tc:ebxml-regrep:xsd:rim:4.0}ObjectRefList" minOccurs="0"/>
 *         &lt;element name="OriginalObjects" type="{urn:oasis:names:tc:ebxml-regrep:xsd:rim:4.0}RegistryObjectListType" minOccurs="0"/>
 *         &lt;element name="InvocationControlFile" type="{urn:oasis:names:tc:ebxml-regrep:xsd:rim:4.0}ExtrinsicObjectType" maxOccurs="unbounded" minOccurs="0"/>
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
@XmlAccessorType(XmlAccessType.FIELD)
@XmlType(name = "", propOrder = { "query", "objectRefList", "originalObjects",
        "invocationControlFile" })
@XmlRootElement(name = "ValidateObjectsRequest")
@DynamicSerialize
@Entity
@Cache(region = RegrepUtil.DB_CACHE_REGION, usage = CacheConcurrencyStrategy.TRANSACTIONAL)
@Table(schema = RegrepUtil.EBXML_SCHEMA, name = "ValidateObjectsRequest")
public class ValidateObjectsRequest extends RegistryRequestType {

    private static final long serialVersionUID = 271447490509409750L;

    @XmlElement(name = "Query")
    @DynamicSerializeElement
    @OneToOne(cascade = CascadeType.ALL)
    @JoinColumn(name = "query_id", referencedColumnName = "id")
    protected QueryType query;

    @XmlElement(name = "ObjectRefList", namespace = EbxmlNamespaces.RIM_URI)
    @DynamicSerializeElement
    @OneToOne(cascade = CascadeType.ALL)
    protected ObjectRefListType objectRefList;

    @XmlElement(name = "OriginalObjects")
    @DynamicSerializeElement
    @OneToOne(cascade = CascadeType.ALL)
    protected RegistryObjectListType originalObjects;

    @XmlElement(name = "InvocationControlFile")
    @DynamicSerializeElement
    @Transient
    protected List<ExtrinsicObjectType> invocationControlFile;

    public ValidateObjectsRequest() {
        super();
    }

    public ValidateObjectsRequest(String id, String comment,
            List<SlotType> slots, QueryType query,
            ObjectRefListType objectRefList,
            RegistryObjectListType originalObjects,
            List<ExtrinsicObjectType> invocationControlFile) {
        super(id, comment, slots);
        this.query = query;
        this.objectRefList = objectRefList;
        this.originalObjects = originalObjects;
        this.invocationControlFile = invocationControlFile;
    }

    /**
     * Gets the value of the query property.
     * 
     * @return possible object is {@link QueryType }
     * 
     */
    public QueryType getQuery() {
        return query;
    }

    /**
     * Sets the value of the query property.
     * 
     * @param value
     *            allowed object is {@link QueryType }
     * 
     */
    public void setQuery(QueryType value) {
        this.query = value;
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
     * Gets the value of the originalObjects property.
     * 
     * @return possible object is {@link RegistryObjectListType }
     * 
     */
    public RegistryObjectListType getOriginalObjects() {
        return originalObjects;
    }

    /**
     * Sets the value of the originalObjects property.
     * 
     * @param value
     *            allowed object is {@link RegistryObjectListType }
     * 
     */
    public void setOriginalObjects(RegistryObjectListType value) {
        this.originalObjects = value;
    }

    /**
     * Gets the value of the invocationControlFile property.
     * 
     * <p>
     * This accessor method returns a reference to the live list, not a
     * snapshot. Therefore any modification you make to the returned list will
     * be present inside the JAXB object. This is why there is not a
     * <CODE>set</CODE> method for the invocationControlFile property.
     * 
     * <p>
     * For example, to add a new item, do as follows:
     * 
     * <pre>
     * getInvocationControlFile().add(newItem);
     * </pre>
     * 
     * 
     * <p>
     * Objects of the following type(s) are allowed in the list
     * {@link ExtrinsicObjectType }
     * 
     * 
     */
    public List<ExtrinsicObjectType> getInvocationControlFile() {
        if (invocationControlFile == null) {
            invocationControlFile = new ArrayList<ExtrinsicObjectType>();
        }
        return this.invocationControlFile;
    }

    public void setInvocationControlFile(
            List<ExtrinsicObjectType> invocationControlFile) {
        this.invocationControlFile = invocationControlFile;
    }

    /**
     * Get the object references contained by the request.
     * 
     * @return the object references
     */
    public List<ObjectRefType> getObjectRefs() {
        if (objectRefList == null) {
            objectRefList = new ObjectRefListType();
        }
        return objectRefList.getObjectRef();
    }

    @Override
    public int hashCode() {
        final int prime = 31;
        int result = super.hashCode();
        result = prime
                * result
                + ((invocationControlFile == null) ? 0 : invocationControlFile
                        .hashCode());
        result = prime * result
                + ((objectRefList == null) ? 0 : objectRefList.hashCode());
        result = prime * result
                + ((originalObjects == null) ? 0 : originalObjects.hashCode());
        result = prime * result + ((query == null) ? 0 : query.hashCode());
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
        ValidateObjectsRequest other = (ValidateObjectsRequest) obj;
        if (invocationControlFile == null) {
            if (other.invocationControlFile != null)
                return false;
        } else if (!invocationControlFile.equals(other.invocationControlFile))
            return false;
        if (objectRefList == null) {
            if (other.objectRefList != null)
                return false;
        } else if (!objectRefList.equals(other.objectRefList))
            return false;
        if (originalObjects == null) {
            if (other.originalObjects != null)
                return false;
        } else if (!originalObjects.equals(other.originalObjects))
            return false;
        if (query == null) {
            if (other.query != null)
                return false;
        } else if (!query.equals(other.query))
            return false;
        return true;
    }

    @Override
    public String toString() {
        StringBuilder builder = new StringBuilder();
        builder.append("ValidateObjectsRequest \n[comment=");
        builder.append(comment);
        builder.append(", \nid=");
        builder.append(id);
        builder.append(", \nslot=");
        builder.append(slot);
        builder.append(", \nquery=");
        builder.append(query);
        builder.append(", \nobjectRefList=");
        builder.append(objectRefList);
        builder.append(", \noriginalObjects=");
        builder.append(originalObjects);
        builder.append(", \ninvocationControlFile=");
        builder.append(invocationControlFile);
        builder.append("]");
        return builder.toString();
    }

}
