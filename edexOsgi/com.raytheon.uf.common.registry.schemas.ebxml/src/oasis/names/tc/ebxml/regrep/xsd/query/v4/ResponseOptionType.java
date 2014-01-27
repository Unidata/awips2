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

import javax.persistence.Embeddable;
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
@XmlRootElement(name = "ResponseOption")
@XmlAccessorType(XmlAccessType.FIELD)
@XmlType(name = "ResponseOptionType")
@DynamicSerialize
@Embeddable
public class ResponseOptionType {

    /**
     * ObjectRef - This option specifies that the QueryResponse MUST contain a
     * <rim:ObjectRefList> element. The purpose of this option is to return
     * references to objects rather than the actual objects.
     * 
     * RegistryObject - This option specifies that the QueryResponse MUST
     * contain a <rim:RegistryObjectList> element containing
     * <rim:RegistryObject> elements with xsi:type=rim:RegistryObjectType.
     * 
     * LeafClass - This option specifies that the QueryResponse MUST contain a
     * collection of <rim:RegistryObjectList> element containing
     * <rim:RegistryObject> elements that have an xsi:type attribute that
     * corresponds to leaf classes as defined in [regrep-xsd-v4.0]. No
     * RepositoryItems SHOULD be included for any rim:ExtrinsicObjectType
     * instance in the <rim:RegistryObjectList> element.
     * 
     * LeafClassWithRepositoryItem - This option is the same as the LeafClass
     * option with the additional requirement that the response include the
     * RepositoryItems, if any, for every rim:ExtrinsicObjectType instance in
     * the <rim:RegistryObjectList> element.
     */
    public enum RETURN_TYPE {
        ObjectRef, RegistryObject, LeafClass, LeafClassWithRepositoryItem
    }

    /** The default return type */
    public static final RETURN_TYPE DEFAULT_RETURN_TYPE = RETURN_TYPE.LeafClassWithRepositoryItem;

    @XmlAttribute
    @XmlJavaTypeAdapter(CollapsedStringAdapter.class)
    @DynamicSerializeElement
    protected String returnType;

    @XmlAttribute
    @DynamicSerializeElement
    protected Boolean returnComposedObjects;

    public ResponseOptionType() {

    }

    public ResponseOptionType(String returnType, Boolean returnComposedObjects) {
        this.returnType = returnType;
        this.returnComposedObjects = returnComposedObjects;
    }

    /**
     * Gets the value of the returnType property.
     * 
     * @return possible object is {@link String }
     * 
     */
    public String getReturnType() {
        if (returnType == null) {
            return DEFAULT_RETURN_TYPE.toString();
        } else {
            return returnType;
        }
    }

    public RETURN_TYPE getReturnTypeEnum() {
        return RETURN_TYPE.valueOf(getReturnType());
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

    @Override
    public int hashCode() {
        final int prime = 31;
        int result = 1;
        result = prime
                * result
                + ((returnComposedObjects == null) ? 0 : returnComposedObjects
                        .hashCode());
        result = prime * result
                + ((returnType == null) ? 0 : returnType.hashCode());
        return result;
    }

    @Override
    public boolean equals(Object obj) {
        if (this == obj)
            return true;
        if (obj == null)
            return false;
        if (getClass() != obj.getClass())
            return false;
        ResponseOptionType other = (ResponseOptionType) obj;
        if (returnComposedObjects == null) {
            if (other.returnComposedObjects != null)
                return false;
        } else if (!returnComposedObjects.equals(other.returnComposedObjects))
            return false;
        if (returnType == null) {
            if (other.returnType != null)
                return false;
        } else if (!returnType.equals(other.returnType))
            return false;
        return true;
    }

    @Override
    public String toString() {
        return "ResponseOptionType [returnType=" + returnType
                + ", returnComposedObjects=" + returnComposedObjects + "]";
    }

}
