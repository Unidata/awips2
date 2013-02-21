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

import java.util.HashSet;
import java.util.Set;

import javax.persistence.Entity;
import javax.persistence.Inheritance;
import javax.persistence.InheritanceType;
import javax.persistence.ManyToMany;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.bind.annotation.XmlSeeAlso;
import javax.xml.bind.annotation.XmlType;

import org.hibernate.annotations.Cascade;

import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;

/**
 * 
 * Common base type for ClassificationScheme and ClassificationNode
 * 
 * 
 * <p>
 * Java class for TaxonomyElementType complex type.
 * 
 * <p>
 * The following schema fragment specifies the expected content contained within
 * this class.
 * 
 * <pre>
 * &lt;complexType name="TaxonomyElementType">
 *   &lt;complexContent>
 *     &lt;extension base="{urn:oasis:names:tc:ebxml-regrep:xsd:rim:4.0}RegistryObjectType">
 *       &lt;sequence>
 *         &lt;element name="ClassificationNode" type="{urn:oasis:names:tc:ebxml-regrep:xsd:rim:4.0}ClassificationNodeType" maxOccurs="unbounded" minOccurs="0"/>
 *       &lt;/sequence>
 *     &lt;/extension>
 *   &lt;/complexContent>
 * &lt;/complexType>
 * </pre>
 * 
 * 
 */
@XmlRootElement
@XmlAccessorType(XmlAccessType.FIELD)
@XmlType(name = "TaxonomyElementType", propOrder = { "classificationNode" })
@XmlSeeAlso({ ClassificationSchemeType.class, ClassificationNodeType.class })
@DynamicSerialize
@Entity
@Inheritance(strategy = InheritanceType.TABLE_PER_CLASS)
public abstract class TaxonomyElementType extends RegistryObjectType {

    @ManyToMany
    @Cascade(value = { org.hibernate.annotations.CascadeType.SAVE_UPDATE,
            org.hibernate.annotations.CascadeType.DETACH })
    @XmlElement(name = "ClassificationNode")
    @DynamicSerializeElement
    protected Set<ClassificationNodeType> classificationNode;

    /**
     * Gets the value of the classificationNode property.
     * 
     * <p>
     * This accessor method returns a reference to the live list, not a
     * snapshot. Therefore any modification you make to the returned list will
     * be present inside the JAXB object. This is why there is not a
     * <CODE>set</CODE> method for the classificationNode property.
     * 
     * <p>
     * For example, to add a new item, do as follows:
     * 
     * <pre>
     * getClassificationNode().add(newItem);
     * </pre>
     * 
     * 
     * <p>
     * Objects of the following type(s) are allowed in the list
     * {@link ClassificationNodeType }
     * 
     * 
     */
    public Set<ClassificationNodeType> getClassificationNode() {
        if (classificationNode == null) {
            classificationNode = new HashSet<ClassificationNodeType>();
        }
        return this.classificationNode;
    }

    public void setClassificationNode(
            Set<ClassificationNodeType> classificationNode) {
        this.classificationNode = classificationNode;
    }

}
