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

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.bind.annotation.XmlType;

import oasis.names.tc.ebxml.regrep.xsd.rim.v4.ObjectRefListType;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.QueryType;
import oasis.names.tc.ebxml.regrep.xsd.rs.v4.RegistryRequestType;

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
 * 
 */
@XmlAccessorType(XmlAccessType.FIELD)
@XmlType(name = "", propOrder = { "query", "objectRefList", "updateAction" })
@XmlRootElement(name = "UpdateObjectsRequest")
@DynamicSerialize
public class UpdateObjectsRequest extends RegistryRequestType {

    @XmlElement(name = "Query")
    @DynamicSerializeElement
    protected QueryType query;

    @XmlElement(name = "ObjectRefList", namespace = "urn:oasis:names:tc:ebxml-regrep:xsd:rim:4.0")
    @DynamicSerializeElement
    protected ObjectRefListType objectRefList;

    @XmlElement(name = "UpdateAction", required = true)
    @DynamicSerializeElement
    protected List<UpdateActionType> updateAction;

    @XmlAttribute
    @DynamicSerializeElement
    protected Boolean checkReferences;

    @XmlAttribute
    @DynamicSerializeElement
    protected Mode mode;

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

}
