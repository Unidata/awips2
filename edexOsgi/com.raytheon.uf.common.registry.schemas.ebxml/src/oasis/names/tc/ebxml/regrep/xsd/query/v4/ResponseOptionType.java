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

package oasis.names.tc.ebxml.regrep.xsd.query.v4;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.bind.annotation.XmlType;
import javax.xml.bind.annotation.adapters.CollapsedStringAdapter;
import javax.xml.bind.annotation.adapters.XmlJavaTypeAdapter;

import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;

/**
 * <p>
 * Java class for ResponseOptionType complex type.
 * 
 * <p>
 * The following schema fragment specifies the expected content contained within
 * this class.
 * 
 * <pre>
 * &lt;complexType name="ResponseOptionType">
 *   &lt;complexContent>
 *     &lt;restriction base="{http://www.w3.org/2001/XMLSchema}anyType">
 *       &lt;attribute name="returnType" default="LeafClassWithRepositoryItem">
 *         &lt;simpleType>
 *           &lt;restriction base="{http://www.w3.org/2001/XMLSchema}NCName">
 *             &lt;enumeration value="ObjectRef"/>
 *             &lt;enumeration value="RegistryObject"/>
 *             &lt;enumeration value="LeafClass"/>
 *             &lt;enumeration value="LeafClassWithRepositoryItem"/>
 *           &lt;/restriction>
 *         &lt;/simpleType>
 *       &lt;/attribute>
 *       &lt;attribute name="returnComposedObjects" type="{http://www.w3.org/2001/XMLSchema}boolean" default="false" />
 *     &lt;/restriction>
 *   &lt;/complexContent>
 * &lt;/complexType>
 * </pre>
 * 
 * 
 */
@XmlRootElement
@XmlAccessorType(XmlAccessType.FIELD)
@XmlType(name = "ResponseOptionType")
@DynamicSerialize
public class ResponseOptionType {

    @XmlAttribute
    @XmlJavaTypeAdapter(CollapsedStringAdapter.class)
    @DynamicSerializeElement
    protected String returnType;

    @XmlAttribute
    @DynamicSerializeElement
    protected Boolean returnComposedObjects;

    /**
     * Gets the value of the returnType property.
     * 
     * @return possible object is {@link String }
     * 
     */
    public String getReturnType() {
        if (returnType == null) {
            return "LeafClassWithRepositoryItem";
        } else {
            return returnType;
        }
    }

    /**
     * Sets the value of the returnType property.
     * 
     * @param value
     *            allowed object is {@link String }
     * 
     */
    public void setReturnType(String value) {
        this.returnType = value;
    }

    /**
     * Gets the value of the returnComposedObjects property.
     * 
     * @return possible object is {@link Boolean }
     * 
     */
    public boolean isReturnComposedObjects() {
        if (returnComposedObjects == null) {
            return false;
        } else {
            return returnComposedObjects;
        }
    }

    /**
     * Sets the value of the returnComposedObjects property.
     * 
     * @param value
     *            allowed object is {@link Boolean }
     * 
     */
    public void setReturnComposedObjects(Boolean value) {
        this.returnComposedObjects = value;
    }

}
