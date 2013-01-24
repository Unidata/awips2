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

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.bind.annotation.XmlType;
import javax.xml.bind.annotation.adapters.CollapsedStringAdapter;
import javax.xml.bind.annotation.adapters.XmlJavaTypeAdapter;

import oasis.names.tc.ebxml.regrep.xsd.rim.v4.QueryExpressionType;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.ValueType;

import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;

/**
 * <p>
 * Java class for UpdateActionType complex type.
 * 
 * <p>
 * The following schema fragment specifies the expected content contained within
 * this class.
 * 
 * <pre>
 * &lt;complexType name="UpdateActionType">
 *   &lt;complexContent>
 *     &lt;restriction base="{http://www.w3.org/2001/XMLSchema}anyType">
 *       &lt;sequence>
 *         &lt;element name="ValueHolder" type="{urn:oasis:names:tc:ebxml-regrep:xsd:rim:4.0}ValueType" minOccurs="0"/>
 *         &lt;element name="Selector" type="{urn:oasis:names:tc:ebxml-regrep:xsd:rim:4.0}QueryExpressionType"/>
 *       &lt;/sequence>
 *       &lt;attribute name="mode" use="required">
 *         &lt;simpleType>
 *           &lt;restriction base="{http://www.w3.org/2001/XMLSchema}NCName">
 *             &lt;enumeration value="Insert"/>
 *             &lt;enumeration value="Update"/>
 *             &lt;enumeration value="Delete"/>
 *           &lt;/restriction>
 *         &lt;/simpleType>
 *       &lt;/attribute>
 *     &lt;/restriction>
 *   &lt;/complexContent>
 * &lt;/complexType>
 * </pre>
 * 
 * 
 */
@XmlRootElement
@XmlAccessorType(XmlAccessType.FIELD)
@XmlType(name = "UpdateActionType", propOrder = { "valueHolder", "selector" })
@DynamicSerialize
public class UpdateActionType {

    @XmlElement(name = "ValueHolder")
    @DynamicSerializeElement
    protected ValueType valueHolder;

    @XmlElement(name = "Selector", required = true)
    @DynamicSerializeElement
    protected QueryExpressionType selector;

    @XmlAttribute(required = true)
    @XmlJavaTypeAdapter(CollapsedStringAdapter.class)
    @DynamicSerializeElement
    protected String mode;

    /**
     * Gets the value of the valueHolder property.
     * 
     * @return possible object is {@link ValueType }
     * 
     */
    public ValueType getValueHolder() {
        return valueHolder;
    }

    /**
     * Sets the value of the valueHolder property.
     * 
     * @param value
     *            allowed object is {@link ValueType }
     * 
     */
    public void setValueHolder(ValueType value) {
        this.valueHolder = value;
    }

    /**
     * Gets the value of the selector property.
     * 
     * @return possible object is {@link QueryExpressionType }
     * 
     */
    public QueryExpressionType getSelector() {
        return selector;
    }

    /**
     * Sets the value of the selector property.
     * 
     * @param value
     *            allowed object is {@link QueryExpressionType }
     * 
     */
    public void setSelector(QueryExpressionType value) {
        this.selector = value;
    }

    /**
     * Gets the value of the mode property.
     * 
     * @return possible object is {@link String }
     * 
     */
    public String getMode() {
        return mode;
    }

    /**
     * Sets the value of the mode property.
     * 
     * @param value
     *            allowed object is {@link String }
     * 
     */
    public void setMode(String value) {
        this.mode = value;
    }

}
