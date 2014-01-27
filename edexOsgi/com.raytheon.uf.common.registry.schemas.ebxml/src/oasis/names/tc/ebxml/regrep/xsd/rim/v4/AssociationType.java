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

import com.raytheon.uf.common.registry.RegrepUtil;
import com.raytheon.uf.common.registry.schemas.ebxml.util.annotations.RegistryObjectReference;
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
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#     Engineer    Description
 * ------------ ----------  ----------- --------------------------
 * 2012                     bphillip    Initial implementation
 * 10/17/2013    1682       bphillip    Added software history
 * 12/2/2013     1829       bphillip    Made ExtensibleObjectType persistable, modified persistence annotations, added hashCode, toString and equals
 * </pre>
 * 
 * @author bphillip
 * @version 1
 */
@XmlRootElement(name = "Association")
@XmlAccessorType(XmlAccessType.FIELD)
@XmlType(name = "AssociationType")
@DynamicSerialize
@Entity
@Cache(region = RegrepUtil.DB_CACHE_REGION, usage = CacheConcurrencyStrategy.TRANSACTIONAL)
@Table(schema = RegrepUtil.EBXML_SCHEMA, name = "Association")
public class AssociationType extends RegistryObjectType {

    private static final long serialVersionUID = 2987540344551300053L;

    @XmlAttribute(required = true)
    @DynamicSerializeElement
    @Index(name = "assoociation_type_idx")
    @RegistryObjectReference
    protected String type;

    @XmlAttribute(required = true)
    @DynamicSerializeElement
    @RegistryObjectReference
    protected String sourceObject;

    @XmlAttribute(required = true)
    @DynamicSerializeElement
    @RegistryObjectReference
    protected String targetObject;

    public AssociationType() {
        super();
    }

    public AssociationType(String id, String lid, String objectType,
            String owner, String status, String name, String description) {
        super(id, lid, objectType, owner, status, name, description);
    }

    public AssociationType(String id, String lid) {
        super(id, lid);
    }

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

    @Override
    public int hashCode() {
        final int prime = 31;
        int result = super.hashCode();
        result = prime * result
                + ((sourceObject == null) ? 0 : sourceObject.hashCode());
        result = prime * result
                + ((targetObject == null) ? 0 : targetObject.hashCode());
        result = prime * result + ((type == null) ? 0 : type.hashCode());
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
        AssociationType other = (AssociationType) obj;
        if (sourceObject == null) {
            if (other.sourceObject != null)
                return false;
        } else if (!sourceObject.equals(other.sourceObject))
            return false;
        if (targetObject == null) {
            if (other.targetObject != null)
                return false;
        } else if (!targetObject.equals(other.targetObject))
            return false;
        if (type == null) {
            if (other.type != null)
                return false;
        } else if (!type.equals(other.type))
            return false;
        return true;
    }

    @Override
    public String toString() {
        StringBuilder builder = new StringBuilder();
        builder.append("AssociationType \n[name=");
        builder.append(name);
        builder.append(", \ndescription=");
        builder.append(description);
        builder.append(", \nversionInfo=");
        builder.append(versionInfo);
        builder.append(", \nclassification=");
        builder.append(classification);
        builder.append(", \nexternalIdentifier=");
        builder.append(externalIdentifier);
        builder.append(", \nexternalLink=");
        builder.append(externalLink);
        builder.append(", \nlid=");
        builder.append(lid);
        builder.append(", \nobjectType=");
        builder.append(objectType);
        builder.append(", \nowner=");
        builder.append(owner);
        builder.append(", \nstatus=");
        builder.append(status);
        builder.append(", \nid=");
        builder.append(id);
        builder.append(", \nslot=");
        builder.append(slot);
        builder.append(", \ntype=");
        builder.append(type);
        builder.append(", \nsourceObject=");
        builder.append(sourceObject);
        builder.append(", \ntargetObject=");
        builder.append(targetObject);
        builder.append("]");
        return builder.toString();
    }

}
