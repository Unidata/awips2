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

import javax.persistence.Transient;
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
 *       &lt;/sequence>
 *       &lt;attribute name="checkReferences" type="{http://www.w3.org/2001/XMLSchema}boolean" default="false" />
 *       &lt;attribute name="deleteChildren" type="{http://www.w3.org/2001/XMLSchema}boolean" default="false" />
 *       &lt;attribute name="deletionScope" type="{urn:oasis:names:tc:ebxml-regrep:xsd:rim:4.0}objectReferenceType" default="urn:oasis:names:tc:ebxml-regrep:DeletionScopeType:DeleteAll" />
 *     &lt;/extension>
 *   &lt;/complexContent>
 * &lt;/complexType>
 * </pre>
 * 
 * 
 */
@XmlAccessorType(XmlAccessType.FIELD)
@XmlType(name = "", propOrder = { "query", "objectRefList" })
@XmlRootElement(name = "RemoveObjectsRequest")
@DynamicSerialize
public class RemoveObjectsRequest extends RegistryRequestType {

    @XmlElement(name = "Query")
    @DynamicSerializeElement
    protected QueryType query;

    @XmlElement(name = "ObjectRefList", namespace = "urn:oasis:names:tc:ebxml-regrep:xsd:rim:4.0")
    @DynamicSerializeElement
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

}
