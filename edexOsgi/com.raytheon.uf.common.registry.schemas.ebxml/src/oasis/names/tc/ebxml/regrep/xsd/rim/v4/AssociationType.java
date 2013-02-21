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

import javax.persistence.Entity;
import javax.persistence.Table;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.bind.annotation.XmlType;

import org.hibernate.annotations.Cache;
import org.hibernate.annotations.CacheConcurrencyStrategy;
import org.hibernate.annotations.Index;

import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;

/**
 * 
 * Represents an association or relationship between two RegistryObjects. The
 * sourceObject is id of the sourceObject in association The targetObject is id
 * of the targetObject in association
 * 
 * 
 * <p>
 * Java class for AssociationType complex type.
 * 
 * <p>
 * The following schema fragment specifies the expected content contained within
 * this class.
 * 
 * <pre>
 * &lt;complexType name="AssociationType">
 *   &lt;complexContent>
 *     &lt;extension base="{urn:oasis:names:tc:ebxml-regrep:xsd:rim:4.0}RegistryObjectType">
 *       &lt;attribute name="type" use="required" type="{urn:oasis:names:tc:ebxml-regrep:xsd:rim:4.0}objectReferenceType" />
 *       &lt;attribute name="sourceObject" use="required" type="{urn:oasis:names:tc:ebxml-regrep:xsd:rim:4.0}objectReferenceType" />
 *       &lt;attribute name="targetObject" use="required" type="{urn:oasis:names:tc:ebxml-regrep:xsd:rim:4.0}objectReferenceType" />
 *     &lt;/extension>
 *   &lt;/complexContent>
 * &lt;/complexType>
 * </pre>
 * 
 * 
 */
@XmlRootElement
@XmlAccessorType(XmlAccessType.FIELD)
@XmlType(name = "AssociationType")
@DynamicSerialize
@Entity
@Cache(region="registryObjects",usage = CacheConcurrencyStrategy.TRANSACTIONAL)
@Table(name = "Association")
public class AssociationType extends RegistryObjectType {

    @XmlAttribute(required = true)
    @DynamicSerializeElement
    @Index(name = "assoociation_type_idx")
    protected String type;

    @XmlAttribute(required = true)
    @DynamicSerializeElement
    protected String sourceObject;

    @XmlAttribute(required = true)
    @DynamicSerializeElement
    protected String targetObject;

    /**
     * Gets the value of the type property.
     * 
     * @return possible object is {@link String }
     * 
     */
    public String getType() {
        return type;
    }

    /**
     * Sets the value of the type property.
     * 
     * @param value
     *            allowed object is {@link String }
     * 
     */
    public void setType(String value) {
        this.type = value;
    }

    /**
     * Gets the value of the sourceObject property.
     * 
     * @return possible object is {@link String }
     * 
     */
    public String getSourceObject() {
        return sourceObject;
    }

    /**
     * Sets the value of the sourceObject property.
     * 
     * @param value
     *            allowed object is {@link String }
     * 
     */
    public void setSourceObject(String value) {
        this.sourceObject = value;
    }

    /**
     * Gets the value of the targetObject property.
     * 
     * @return possible object is {@link String }
     * 
     */
    public String getTargetObject() {
        return targetObject;
    }

    /**
     * Sets the value of the targetObject property.
     * 
     * @param value
     *            allowed object is {@link String }
     * 
     */
    public void setTargetObject(String value) {
        this.targetObject = value;
    }

}
