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
import javax.persistence.Column;
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
 * 
 */
@XmlRootElement
@XmlAccessorType(XmlAccessType.FIELD)
@XmlType(name = "CollectionValueType", propOrder = { "collectionValue" })
@DynamicSerialize
@Entity
@Cache(region="registryObjects",usage = CacheConcurrencyStrategy.TRANSACTIONAL)
@Table(name = "CollectionValue")
public class CollectionValueType extends ValueType {

    @XmlElement(name = "Element")
    @DynamicSerializeElement
    @Column(name = COLUMN_NAME)
    @ManyToMany(cascade = CascadeType.ALL)
    protected List<ValueType> collectionValue;

    @XmlAttribute
    @DynamicSerializeElement
    protected String collectionType;

    private static final String COLUMN_NAME = "collectionValue";

    @Override
    public String getColumnName() {
        return COLUMN_NAME;
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

    /**
     * Sets the value of the collectionType property.
     * 
     * @param value
     *            allowed object is {@link String }
     * 
     */
    public void setCollectionType(String value) {
        this.collectionType = value;
    }

    @Override
    public List<ValueType> getValue() {
        return getElement();
    }

    @Override
    public void setValue(Object obj) {
        this.collectionValue = (List<ValueType>) obj;

    }

    public List<ValueType> getCollectionValue() {
        return collectionValue;
    }

    public void setCollectionValue(List<ValueType> collectionValue) {
        this.collectionValue = collectionValue;
    }

}
