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

import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;

/**
 * 
 * Represents a classification of its parent object within specified value in a
 * ClassificationScheme.
 * 
 * 
 * <p>
 * Java class for ClassificationType complex type.
 * 
 * <p>
 * The following schema fragment specifies the expected content contained within
 * this class.
 * 
 * <pre>
 * &lt;complexType name="ClassificationType">
 *   &lt;complexContent>
 *     &lt;extension base="{urn:oasis:names:tc:ebxml-regrep:xsd:rim:4.0}RegistryObjectType">
 *       &lt;attribute name="classificationScheme" type="{urn:oasis:names:tc:ebxml-regrep:xsd:rim:4.0}objectReferenceType" />
 *       &lt;attribute name="classifiedObject" type="{urn:oasis:names:tc:ebxml-regrep:xsd:rim:4.0}objectReferenceType" />
 *       &lt;attribute name="classificationNode" type="{urn:oasis:names:tc:ebxml-regrep:xsd:rim:4.0}objectReferenceType" />
 *       &lt;attribute name="nodeRepresentation" type="{urn:oasis:names:tc:ebxml-regrep:xsd:rim:4.0}LongText" />
 *     &lt;/extension>
 *   &lt;/complexContent>
 * &lt;/complexType>
 * </pre>
 * 
 * 
 */
@XmlRootElement
@XmlAccessorType(XmlAccessType.FIELD)
@XmlType(name = "ClassificationType")
@DynamicSerialize
@Entity
@Cache(region="registryObjects",usage = CacheConcurrencyStrategy.TRANSACTIONAL)
@Table(name = "Classification")
public class ClassificationType extends RegistryObjectType {

    @XmlAttribute
    @DynamicSerializeElement
    protected String classificationScheme;

    @XmlAttribute
    @DynamicSerializeElement
    protected String classifiedObject;

    @XmlAttribute
    @DynamicSerializeElement
    protected String classificationNode;

    @XmlAttribute
    @DynamicSerializeElement
    protected String nodeRepresentation;

    /**
     * Gets the value of the classificationScheme property.
     * 
     * @return possible object is {@link String }
     * 
     */
    public String getClassificationScheme() {
        return classificationScheme;
    }

    /**
     * Sets the value of the classificationScheme property.
     * 
     * @param value
     *            allowed object is {@link String }
     * 
     */
    public void setClassificationScheme(String value) {
        this.classificationScheme = value;
    }

    /**
     * Gets the value of the classifiedObject property.
     * 
     * @return possible object is {@link String }
     * 
     */
    public String getClassifiedObject() {
        return classifiedObject;
    }

    /**
     * Sets the value of the classifiedObject property.
     * 
     * @param value
     *            allowed object is {@link String }
     * 
     */
    public void setClassifiedObject(String value) {
        this.classifiedObject = value;
    }

    /**
     * Gets the value of the classificationNode property.
     * 
     * @return possible object is {@link String }
     * 
     */
    public String getClassificationNode() {
        return classificationNode;
    }

    /**
     * Sets the value of the classificationNode property.
     * 
     * @param value
     *            allowed object is {@link String }
     * 
     */
    public void setClassificationNode(String value) {
        this.classificationNode = value;
    }

    /**
     * Gets the value of the nodeRepresentation property.
     * 
     * @return possible object is {@link String }
     * 
     */
    public String getNodeRepresentation() {
        return nodeRepresentation;
    }

    /**
     * Sets the value of the nodeRepresentation property.
     * 
     * @param value
     *            allowed object is {@link String }
     * 
     */
    public void setNodeRepresentation(String value) {
        this.nodeRepresentation = value;
    }

    @Override
    public int hashCode() {
        final int prime = 31;
        int result = super.hashCode();
        result = prime
                * result
                + ((classificationNode == null) ? 0 : classificationNode
                        .hashCode());
        result = prime
                * result
                + ((classificationScheme == null) ? 0 : classificationScheme
                        .hashCode());
        result = prime
                * result
                + ((classifiedObject == null) ? 0 : classifiedObject.hashCode());
        result = prime
                * result
                + ((nodeRepresentation == null) ? 0 : nodeRepresentation
                        .hashCode());
        return result;
    }

    @Override
    public boolean equals(Object obj) {
        if (this == obj) {
            return true;
        }
        if (!super.equals(obj)) {
            return false;
        }
        if (getClass() != obj.getClass()) {
            return false;
        }
        ClassificationType other = (ClassificationType) obj;
        if (classificationNode == null) {
            if (other.classificationNode != null) {
                return false;
            }
        } else if (!classificationNode.equals(other.classificationNode)) {
            return false;
        }
        if (classificationScheme == null) {
            if (other.classificationScheme != null) {
                return false;
            }
        } else if (!classificationScheme.equals(other.classificationScheme)) {
            return false;
        }
        if (classifiedObject == null) {
            if (other.classifiedObject != null) {
                return false;
            }
        } else if (!classifiedObject.equals(other.classifiedObject)) {
            return false;
        }
        if (nodeRepresentation == null) {
            if (other.nodeRepresentation != null) {
                return false;
            }
        } else if (!nodeRepresentation.equals(other.nodeRepresentation)) {
            return false;
        }
        return true;
    }

}
