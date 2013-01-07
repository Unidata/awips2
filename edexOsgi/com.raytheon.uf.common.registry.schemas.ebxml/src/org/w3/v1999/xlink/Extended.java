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

package org.w3.v1999.xlink;

import java.util.ArrayList;
import java.util.List;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlElements;
import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.bind.annotation.XmlType;

import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;

/**
 * 
 * Intended for use as the type of user-declared elements to make them extended
 * links. Note that the elements referenced in the content model are all
 * abstract. The intention is that by simply declaring elements with these as
 * their substitutionGroup, all the right things will happen.
 * 
 * 
 * <p>
 * Java class for extended complex type.
 * 
 * <p>
 * The following schema fragment specifies the expected content contained within
 * this class.
 * 
 * <pre>
 * &lt;complexType name="extended">
 *   &lt;complexContent>
 *     &lt;restriction base="{http://www.w3.org/2001/XMLSchema}anyType">
 *       &lt;group ref="{http://www.w3.org/1999/xlink}extendedModel" maxOccurs="unbounded" minOccurs="0"/>
 *       &lt;attGroup ref="{http://www.w3.org/1999/xlink}extendedAttrs"/>
 *     &lt;/restriction>
 *   &lt;/complexContent>
 * &lt;/complexType>
 * </pre>
 * 
 * 
 */
@XmlRootElement
@XmlAccessorType(XmlAccessType.FIELD)
@XmlType(name = "extended", propOrder = { "extendedModel" })
@DynamicSerialize
public class Extended {

    @XmlElements({ @XmlElement(name = "locator", type = LocatorType.class),
            @XmlElement(name = "resource", type = ResourceType.class),
            @XmlElement(name = "arc", type = ArcType.class),
            @XmlElement(name = "title", type = TitleEltType.class) })
    @DynamicSerializeElement
    protected List<Object> extendedModel;

    @XmlAttribute(namespace = "http://www.w3.org/1999/xlink", required = true)
    @DynamicSerializeElement
    protected TypeType type;

    @XmlAttribute(namespace = "http://www.w3.org/1999/xlink")
    @DynamicSerializeElement
    protected String role;

    @XmlAttribute(namespace = "http://www.w3.org/1999/xlink")
    @DynamicSerializeElement
    protected String title;

    /**
     * Gets the value of the extendedModel property.
     * 
     * <p>
     * This accessor method returns a reference to the live list, not a
     * snapshot. Therefore any modification you make to the returned list will
     * be present inside the JAXB object. This is why there is not a
     * <CODE>set</CODE> method for the extendedModel property.
     * 
     * <p>
     * For example, to add a new item, do as follows:
     * 
     * <pre>
     * getExtendedModel().add(newItem);
     * </pre>
     * 
     * 
     * <p>
     * Objects of the following type(s) are allowed in the list
     * {@link LocatorType } {@link ResourceType } {@link ArcType }
     * {@link TitleEltType }
     * 
     * 
     */
    public List<Object> getExtendedModel() {
        if (extendedModel == null) {
            extendedModel = new ArrayList<Object>();
        }
        return this.extendedModel;
    }

    public void setExtendedModel(List<Object> extendedModel) {
        this.extendedModel = extendedModel;
    }

    /**
     * Gets the value of the type property.
     * 
     * @return possible object is {@link TypeType }
     * 
     */
    public TypeType getType() {
        if (type == null) {
            return TypeType.EXTENDED;
        } else {
            return type;
        }
    }

    /**
     * Sets the value of the type property.
     * 
     * @param value
     *            allowed object is {@link TypeType }
     * 
     */
    public void setType(TypeType value) {
        this.type = value;
    }

    /**
     * Gets the value of the role property.
     * 
     * @return possible object is {@link String }
     * 
     */
    public String getRole() {
        return role;
    }

    /**
     * Sets the value of the role property.
     * 
     * @param value
     *            allowed object is {@link String }
     * 
     */
    public void setRole(String value) {
        this.role = value;
    }

    /**
     * Gets the value of the title property.
     * 
     * @return possible object is {@link String }
     * 
     */
    public String getTitle() {
        return title;
    }

    /**
     * Sets the value of the title property.
     * 
     * @param value
     *            allowed object is {@link String }
     * 
     */
    public void setTitle(String value) {
        this.title = value;
    }

}
