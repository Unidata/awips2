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
import javax.persistence.FetchType;
import javax.persistence.JoinTable;
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

import com.raytheon.uf.common.registry.RegrepUtil;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;

/**
 * 
 * A specialized ValueType that may be used as a container for a collection of
 * elements where each element is a ValueType.
 * 
 * 
 * <p>
 * Java class for CollectionValueType complex type.
 * 
 * <p>
 * The following schema fragment specifies the expected content contained within
 * this class.
 * 
 * <pre>
 * &lt;complexType name="CollectionValueType">
 *   &lt;complexContent>
 *     &lt;extension base="{urn:oasis:names:tc:ebxml-regrep:xsd:rim:4.0}ValueType">
 *       &lt;sequence>
 *         &lt;element name="Element" type="{urn:oasis:names:tc:ebxml-regrep:xsd:rim:4.0}ValueType" maxOccurs="unbounded" minOccurs="0"/>
 *       &lt;/sequence>
 *       &lt;attribute name="collectionType" type="{urn:oasis:names:tc:ebxml-regrep:xsd:rim:4.0}objectReferenceType" default="urn:oasis:names:tc:ebxml-regrep:CollectionType:Bag" />
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
 * 12/2/2013     1829       bphillip    Removed generic methods, 
 *                                      modified persistence annotations, added 
 *                                      constructors, hashCode, toString and equals
 * </pre>
 * 
 * @author bphillip
 * @version 1
 */
@XmlRootElement(name = "CollectionValue")
@XmlAccessorType(XmlAccessType.FIELD)
@XmlType(name = "CollectionValueType", propOrder = { "collectionValue" })
@DynamicSerialize
@Entity
@Cache(region = RegrepUtil.DB_CACHE_REGION, usage = CacheConcurrencyStrategy.TRANSACTIONAL)
@Table(schema = RegrepUtil.EBXML_SCHEMA, name = "CollectionValue")
public class CollectionValueType extends ValueType {

    @XmlElement(name = "Element")
    @DynamicSerializeElement
    @ManyToMany(cascade = CascadeType.ALL, fetch = FetchType.LAZY)
    @JoinTable(schema = RegrepUtil.EBXML_SCHEMA)
    private List<ValueType> collectionValue;

    @XmlAttribute
    @DynamicSerializeElement
    private String collectionType;

    public CollectionValueType() {
        super();
    }

    public CollectionValueType(Integer id) {
        super(id);
    }

    public CollectionValueType(List<ValueType> collectionValue,
            String collectionType) {
        super();
        this.collectionValue = collectionValue;
        this.collectionType = collectionType;
    }

    public CollectionValueType(Integer id, List<ValueType> collectionValue,
            String collectionType) {
        super(id);
        this.collectionValue = collectionValue;
        this.collectionType = collectionType;
    }

    @SuppressWarnings("unchecked")
    @Override
    public <T> T getValue() {
        return (T) getCollectionValue();
    }

    public List<ValueType> getCollectionValue() {
        return collectionValue;
    }

    public void setCollectionValue(List<ValueType> collectionValue) {
        this.collectionValue = collectionValue;
    }

    public void setCollectionType(String collectionType) {
        this.collectionType = collectionType;
    }

    /**
     * Gets the value of the element property.
     * 
     * <p>
     * This accessor method returns a reference to the live list, not a
     * snapshot. Therefore any modification you make to the returned list will
     * be present inside the JAXB object. This is why there is not a
     * <CODE>set</CODE> method for the element property.
     * 
     * <p>
     * For example, to add a new item, do as follows:
     * 
     * <pre>
     * getElement().add(newItem);
     * </pre>
     * 
     * 
     * <p>
     * Objects of the following type(s) are allowed in the list
     * {@link ValueType }
     * 
     * 
     */
    public List<ValueType> getElement() {
        if (collectionValue == null) {
            collectionValue = new ArrayList<ValueType>();
        }
        return this.collectionValue;
    }

    /**
     * Gets the value of the collectionType property.
     * 
     * @return possible object is {@link String }
     * 
     */
    public String getCollectionType() {
        if (collectionType == null) {
            return "urn:oasis:names:tc:ebxml-regrep:CollectionType:Bag";
        } else {
            return collectionType;
        }
    }

    @Override
    public int hashCode() {
        final int prime = 31;
        int result = super.hashCode();
        result = prime * result
                + ((collectionType == null) ? 0 : collectionType.hashCode());
        result = prime * result
                + ((collectionValue == null) ? 0 : collectionValue.hashCode());
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
        CollectionValueType other = (CollectionValueType) obj;
        if (collectionType == null) {
            if (other.collectionType != null)
                return false;
        } else if (!collectionType.equals(other.collectionType))
            return false;
        if (collectionValue == null) {
            if (other.collectionValue != null)
                return false;
        } else if (!collectionValue.equals(other.collectionValue))
            return false;
        return true;
    }

    @Override
    public String toString() {
        return "CollectionValueType [collectionValue=" + collectionValue
                + ", collectionType=" + collectionType + ", id=" + id + "]";
    }

}
