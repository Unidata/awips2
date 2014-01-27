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

import java.util.ArrayList;
import java.util.List;

import javax.persistence.CascadeType;
import javax.persistence.Embedded;
import javax.persistence.Entity;
import javax.persistence.JoinColumn;
import javax.persistence.OneToOne;
import javax.persistence.Table;
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
 *         &lt;element name="UpdateAction" type="{urn:oasis:names:tc:ebxml-regrep:xsd:lcm:4.0}UpdateActionType" maxOccurs="unbounded"/>
 *       &lt;/sequence>
 *       &lt;attribute name="checkReferences" type="{http://www.w3.org/2001/XMLSchema}boolean" default="false" />
 *       &lt;attribute name="mode" type="{urn:oasis:names:tc:ebxml-regrep:xsd:lcm:4.0}mode" default="CreateOrReplace" />
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
@XmlType(name = "", propOrder = { "query", "objectRefList", "updateAction" })
@XmlRootElement(name = "UpdateObjectsRequest")
@DynamicSerialize
@Entity
@Cache(region = RegrepUtil.DB_CACHE_REGION, usage = CacheConcurrencyStrategy.TRANSACTIONAL)
@Table(schema = RegrepUtil.EBXML_SCHEMA, name = "UpdateObjectsRequest")
public class UpdateObjectsRequest extends RegistryRequestType {

    private static final long serialVersionUID = 2714648132054313507L;

    @XmlElement(name = "Query")
    @DynamicSerializeElement
    @OneToOne(cascade = CascadeType.ALL)
    @JoinColumn(name = "query_id", referencedColumnName = "id")
    protected QueryType query;

    @XmlElement(name = "ObjectRefList", namespace = EbxmlNamespaces.RIM_URI)
    @DynamicSerializeElement
    @OneToOne(cascade = CascadeType.ALL)
    protected ObjectRefListType objectRefList;

    @XmlElement(name = "UpdateAction", required = true)
    @DynamicSerializeElement
    @Embedded
    protected List<UpdateActionType> updateAction;

    @XmlAttribute
    @DynamicSerializeElement
    protected Boolean checkReferences;

    @XmlAttribute
    @DynamicSerializeElement
    protected Mode mode;

    public UpdateObjectsRequest() {
        super();
    }

    public UpdateObjectsRequest(String id, String comment,
            List<SlotType> slots, QueryType query,
            ObjectRefListType objectRefList,
            List<UpdateActionType> updateAction, Boolean checkReferences,
            Mode mode) {
        super(id, comment, slots);
        this.query = query;
        this.objectRefList = objectRefList;
        this.updateAction = updateAction;
        this.checkReferences = checkReferences;
        this.mode = mode;
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
     * Gets the value of the updateAction property.
     * 
     * <p>
     * This accessor method returns a reference to the live list, not a
     * snapshot. Therefore any modification you make to the returned list will
     * be present inside the JAXB object. This is why there is not a
     * <CODE>set</CODE> method for the updateAction property.
     * 
     * <p>
     * For example, to add a new item, do as follows:
     * 
     * <pre>
     * getUpdateAction().add(newItem);
     * </pre>
     * 
     * 
     * <p>
     * Objects of the following type(s) are allowed in the list
     * {@link UpdateActionType }
     * 
     * 
     */
    public List<UpdateActionType> getUpdateAction() {
        if (updateAction == null) {
            updateAction = new ArrayList<UpdateActionType>();
        }
        return this.updateAction;
    }

    public void setUpdateAction(List<UpdateActionType> updateAction) {
        this.updateAction = updateAction;
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

    public Boolean getCheckReferences() {
        return checkReferences;
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
     * Gets the value of the mode property.
     * 
     * @return possible object is {@link Mode }
     * 
     */
    public Mode getMode() {
        if (mode == null) {
            return Mode.CREATE_OR_REPLACE;
        } else {
            return mode;
        }
    }

    /**
     * Sets the value of the mode property.
     * 
     * @param value
     *            allowed object is {@link Mode }
     * 
     */
    public void setMode(Mode value) {
        this.mode = value;
    }

    @Override
    public int hashCode() {
        final int prime = 31;
        int result = super.hashCode();
        result = prime * result
                + ((checkReferences == null) ? 0 : checkReferences.hashCode());
        result = prime * result + ((mode == null) ? 0 : mode.hashCode());
        result = prime * result
                + ((objectRefList == null) ? 0 : objectRefList.hashCode());
        result = prime * result + ((query == null) ? 0 : query.hashCode());
        result = prime * result
                + ((updateAction == null) ? 0 : updateAction.hashCode());
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
        UpdateObjectsRequest other = (UpdateObjectsRequest) obj;
        if (checkReferences == null) {
            if (other.checkReferences != null)
                return false;
        } else if (!checkReferences.equals(other.checkReferences))
            return false;
        if (mode != other.mode)
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
        if (updateAction == null) {
            if (other.updateAction != null)
                return false;
        } else if (!updateAction.equals(other.updateAction))
            return false;
        return true;
    }

    @Override
    public String toString() {
        StringBuilder builder = new StringBuilder();
        builder.append("UpdateObjectsRequest \n[comment=");
        builder.append(comment);
        builder.append(", \nid=");
        builder.append(id);
        builder.append(", \nslot=");
        builder.append(slot);
        builder.append(", \nquery=");
        builder.append(query);
        builder.append(", \nobjectRefList=");
        builder.append(objectRefList);
        builder.append(", \nupdateAction=");
        builder.append(updateAction);
        builder.append(", \ncheckReferences=");
        builder.append(checkReferences);
        builder.append(", \nmode=");
        builder.append(mode);
        builder.append("]");
        return builder.toString();
    }

}
