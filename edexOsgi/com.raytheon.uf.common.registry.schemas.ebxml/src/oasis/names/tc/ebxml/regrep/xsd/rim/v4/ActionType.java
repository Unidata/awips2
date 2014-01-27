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

import javax.persistence.CascadeType;
import javax.persistence.Entity;
import javax.persistence.OneToOne;
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
import com.raytheon.uf.common.registry.schemas.ebxml.util.annotations.RegistryObjectReference;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;

/**
 * Represents an action on a set of affected RegistryObjects within an
 * AuditableEvent.
 * 
 * <p>
 * Java class for ActionType complex type.
 * 
 * <p>
 * The following schema fragment specifies the expected content contained within
 * this class.
 * 
 * <pre>
 * &lt;complexType name="ActionType">
 *   &lt;complexContent>
 *     &lt;extension base="{urn:oasis:names:tc:ebxml-regrep:xsd:rim:4.0}ExtensibleObjectType">
 *       &lt;sequence>
 *         &lt;element name="AffectedObjects" type="{urn:oasis:names:tc:ebxml-regrep:xsd:rim:4.0}RegistryObjectListType" minOccurs="0"/>
 *         &lt;element name="AffectedObjectRefs" type="{urn:oasis:names:tc:ebxml-regrep:xsd:rim:4.0}ObjectRefListType" minOccurs="0"/>
 *       &lt;/sequence>
 *       &lt;attribute name="eventType" use="required" type="{urn:oasis:names:tc:ebxml-regrep:xsd:rim:4.0}objectReferenceType" />
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
 * 12/2/2013     1829       bphillip    Made ExtensibleObjectType persistable, modified persistence annotations, added hashCode and equals
 * </pre>
 * 
 * @author bphillip
 * @version 1
 */
@XmlRootElement(name = "Action")
@XmlAccessorType(XmlAccessType.FIELD)
@XmlType(name = "ActionType", propOrder = { "affectedObjects",
        "affectedObjectRefs" })
@DynamicSerialize
@Entity
@Cache(region = RegrepUtil.DB_CACHE_REGION, usage = CacheConcurrencyStrategy.TRANSACTIONAL)
@Table(schema = RegrepUtil.EBXML_SCHEMA, name = "Action")
public class ActionType extends ExtensibleObjectType {

    private static final long serialVersionUID = -8469820571747325703L;

    @XmlElement(name = "AffectedObjects")
    @DynamicSerializeElement
    @OneToOne(cascade = CascadeType.ALL)
    protected RegistryObjectListType affectedObjects;

    @OneToOne(cascade = CascadeType.ALL)
    @XmlElement(name = "AffectedObjectRefs")
    @DynamicSerializeElement
    protected ObjectRefListType affectedObjectRefs;

    @XmlAttribute(required = true)
    @DynamicSerializeElement
    @RegistryObjectReference
    protected String eventType;

    public ActionType() {
        super();
    }

    @Override
    public int hashCode() {
        final int prime = 31;
        int result = super.hashCode();
        result = prime
                * result
                + ((affectedObjectRefs == null) ? 0 : affectedObjectRefs
                        .hashCode());
        result = prime * result
                + ((affectedObjects == null) ? 0 : affectedObjects.hashCode());
        result = prime * result
                + ((eventType == null) ? 0 : eventType.hashCode());
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
        ActionType other = (ActionType) obj;
        if (affectedObjectRefs == null) {
            if (other.affectedObjectRefs != null)
                return false;
        } else if (!affectedObjectRefs.equals(other.affectedObjectRefs))
            return false;
        if (affectedObjects == null) {
            if (other.affectedObjects != null)
                return false;
        } else if (!affectedObjects.equals(other.affectedObjects))
            return false;
        if (eventType == null) {
            if (other.eventType != null)
                return false;
        } else if (!eventType.equals(other.eventType))
            return false;
        return true;
    }

    @Override
    public String toString() {
        StringBuilder builder = new StringBuilder();
        builder.append("ActionType \n[id=");
        builder.append(id);
        builder.append(", \nslot=");
        builder.append(slot);
        builder.append(", \naffectedObjects=");
        builder.append(affectedObjects);
        builder.append(", \naffectedObjectRefs=");
        builder.append(affectedObjectRefs);
        builder.append(", \neventType=");
        builder.append(eventType);
        builder.append("]");
        return builder.toString();
    }

    /**
     * Gets the value of the affectedObjects property.
     * 
     * @return possible object is {@link RegistryObjectListType }
     * 
     */
    public RegistryObjectListType getAffectedObjects() {
        return affectedObjects;
    }

    /**
     * Sets the value of the affectedObjects property.
     * 
     * @param value
     *            allowed object is {@link RegistryObjectListType }
     * 
     */
    public void setAffectedObjects(RegistryObjectListType value) {
        this.affectedObjects = value;
    }

    /**
     * Gets the value of the affectedObjectRefs property.
     * 
     * @return possible object is {@link ObjectRefListType }
     * 
     */
    public ObjectRefListType getAffectedObjectRefs() {
        return affectedObjectRefs;
    }

    /**
     * Sets the value of the affectedObjectRefs property.
     * 
     * @param value
     *            allowed object is {@link ObjectRefListType }
     * 
     */
    public void setAffectedObjectRefs(ObjectRefListType value) {
        this.affectedObjectRefs = value;
    }

    /**
     * Gets the value of the eventType property.
     * 
     * @return possible object is {@link String }
     * 
     */
    public String getEventType() {
        return eventType;
    }

    /**
     * Sets the value of the eventType property.
     * 
     * @param value
     *            allowed object is {@link String }
     * 
     */
    public void setEventType(String value) {
        this.eventType = value;
    }

}
