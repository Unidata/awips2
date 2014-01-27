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

package oasis.names.tc.ebxml.regrep.xsd.lcm.v4;

import java.util.List;

import javax.persistence.CascadeType;
import javax.persistence.Entity;
import javax.persistence.JoinColumn;
import javax.persistence.OneToOne;
import javax.persistence.Table;
import javax.persistence.Transient;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.bind.annotation.XmlType;

import oasis.names.tc.ebxml.regrep.xsd.rim.v4.ObjectRefListType;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.QueryType;
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
 *       &lt;/sequence>
 *       &lt;attribute name="checkReferences" type="{http://www.w3.org/2001/XMLSchema}boolean" default="false" />
 *       &lt;attribute name="deleteChildren" type="{http://www.w3.org/2001/XMLSchema}boolean" default="false" />
 *       &lt;attribute name="deletionScope" type="{urn:oasis:names:tc:ebxml-regrep:xsd:rim:4.0}objectReferenceType" default="urn:oasis:names:tc:ebxml-regrep:DeletionScopeType:DeleteAll" />
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
 * 12/2/2013     1829       bphillip    Added Hibernate annotations
 * </pre>
 * 
 * @author bphillip
 * @version 1
 */
@XmlAccessorType(XmlAccessType.FIELD)
@XmlType(name = "", propOrder = { "query", "objectRefList" })
@XmlRootElement(name = "RemoveObjectsRequest")
@DynamicSerialize
@Entity
@Cache(region = RegrepUtil.DB_CACHE_REGION, usage = CacheConcurrencyStrategy.TRANSACTIONAL)
@Table(schema = RegrepUtil.EBXML_SCHEMA, name = "RemoveObjectsRequest")
public class RemoveObjectsRequest extends RegistryRequestType {

    private static final long serialVersionUID = 654608816540789417L;

    @XmlElement(name = "Query")
    @DynamicSerializeElement
    @OneToOne(cascade = CascadeType.ALL)
    @JoinColumn(name = "query_id", referencedColumnName = "id")
    protected QueryType query;

    @XmlElement(name = "ObjectRefList", namespace = EbxmlNamespaces.RIM_URI)
    @DynamicSerializeElement
    @OneToOne(cascade = CascadeType.ALL)
    protected ObjectRefListType objectRefList;

    @XmlAttribute
    @DynamicSerializeElement
    protected Boolean checkReferences;

    @XmlAttribute
    @DynamicSerializeElement
    protected Boolean deleteChildren;

    @XmlAttribute
    @DynamicSerializeElement
    protected String deletionScope;

    @Transient
    @XmlAttribute
    @DynamicSerializeElement
    protected String username;

    public RemoveObjectsRequest() {
        super();
    }

    public RemoveObjectsRequest(String id, String comment,
            List<SlotType> slots, QueryType query,
            ObjectRefListType objectRefList, Boolean checkReferences,
            Boolean deleteChildren, String deletionScope) {
        super(id, comment, slots);
        this.query = query;
        this.objectRefList = objectRefList;
        this.checkReferences = checkReferences;
        this.deleteChildren = deleteChildren;
        this.deletionScope = deletionScope;
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
     * Gets the value of the checkReferences property.
     * 
     * @return possible object is {@link Boolean }
     * 
     */
    public boolean isCheckReferences() {
        if (checkReferences == null) {
            return false;
        } else {
            return checkReferences;
        }
    }

    /**
     * Sets the value of the checkReferences property.
     * 
     * @param value
     *            allowed object is {@link Boolean }
     * 
     */
    public void setCheckReferences(Boolean value) {
        this.checkReferences = value;
    }

    /**
     * Gets the value of the deleteChildren property.
     * 
     * @return possible object is {@link Boolean }
     * 
     */
    public boolean isDeleteChildren() {
        if (deleteChildren == null) {
            return false;
        } else {
            return deleteChildren;
        }
    }

    /**
     * Sets the value of the deleteChildren property.
     * 
     * @param value
     *            allowed object is {@link Boolean }
     * 
     */
    public void setDeleteChildren(Boolean value) {
        this.deleteChildren = value;
    }

    /**
     * Gets the value of the deletionScope property.
     * 
     * @return possible object is {@link String }
     * 
     */
    public String getDeletionScope() {
        if (deletionScope == null) {
            return "urn:oasis:names:tc:ebxml-regrep:DeletionScopeType:DeleteAll";
        } else {
            return deletionScope;
        }
    }

    /**
     * Sets the value of the deletionScope property.
     * 
     * @param value
     *            allowed object is {@link String }
     * 
     */
    public void setDeletionScope(String value) {
        this.deletionScope = value;
    }

    public String getUsername() {
        return username;
    }

    public void setUsername(String username) {
        this.username = username;
    }

    @Override
    public int hashCode() {
        final int prime = 31;
        int result = super.hashCode();
        result = prime * result
                + ((checkReferences == null) ? 0 : checkReferences.hashCode());
        result = prime * result
                + ((deleteChildren == null) ? 0 : deleteChildren.hashCode());
        result = prime * result
                + ((deletionScope == null) ? 0 : deletionScope.hashCode());
        result = prime * result
                + ((objectRefList == null) ? 0 : objectRefList.hashCode());
        result = prime * result + ((query == null) ? 0 : query.hashCode());
        result = prime * result
                + ((username == null) ? 0 : username.hashCode());
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
        RemoveObjectsRequest other = (RemoveObjectsRequest) obj;
        if (checkReferences == null) {
            if (other.checkReferences != null)
                return false;
        } else if (!checkReferences.equals(other.checkReferences))
            return false;
        if (deleteChildren == null) {
            if (other.deleteChildren != null)
                return false;
        } else if (!deleteChildren.equals(other.deleteChildren))
            return false;
        if (deletionScope == null) {
            if (other.deletionScope != null)
                return false;
        } else if (!deletionScope.equals(other.deletionScope))
            return false;
        if (objectRefList == null) {
            if (other.objectRefList != null)
                return false;
        } else if (!objectRefList.equals(other.objectRefList))
            return false;
        if (query == null) {
            if (other.query != null)
                return false;
        } else if (!query.equals(other.query))
            return false;
        if (username == null) {
            if (other.username != null)
                return false;
        } else if (!username.equals(other.username))
            return false;
        return true;
    }

    @Override
    public String toString() {
        StringBuilder builder = new StringBuilder();
        builder.append("RemoveObjectsRequest \n[comment=");
        builder.append(comment);
        builder.append(", \nid=");
        builder.append(id);
        builder.append(", \nslot=");
        builder.append(slot);
        builder.append(", \nquery=");
        builder.append(query);
        builder.append(", \nobjectRefList=");
        builder.append(objectRefList);
        builder.append(", \ncheckReferences=");
        builder.append(checkReferences);
        builder.append(", \ndeleteChildren=");
        builder.append(deleteChildren);
        builder.append(", \ndeletionScope=");
        builder.append(deletionScope);
        builder.append(", \nusername=");
        builder.append(username);
        builder.append("]");
        return builder.toString();
    }

}
