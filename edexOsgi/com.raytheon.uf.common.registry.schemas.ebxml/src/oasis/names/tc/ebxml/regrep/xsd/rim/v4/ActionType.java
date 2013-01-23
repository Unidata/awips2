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

import java.io.Serializable;

import javax.persistence.Entity;
import javax.persistence.GeneratedValue;
import javax.persistence.Id;
import javax.persistence.OneToOne;
import javax.persistence.Table;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.bind.annotation.XmlTransient;
import javax.xml.bind.annotation.XmlType;

import org.hibernate.annotations.Cache;
import org.hibernate.annotations.CacheConcurrencyStrategy;
import org.hibernate.annotations.Cascade;

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
 * 
 */
@XmlRootElement
@XmlAccessorType(XmlAccessType.FIELD)
@XmlType(name = "ActionType", propOrder = { "affectedObjects",
        "affectedObjectRefs" })
@DynamicSerialize
@Entity
@Cache(region = "registryObjects", usage = CacheConcurrencyStrategy.TRANSACTIONAL)
@Table(name = "Action")
public class ActionType extends ExtensibleObjectType implements Serializable {

    private static final long serialVersionUID = -8469820571747325703L;

    @Id
    @GeneratedValue
    @XmlTransient
    private Integer key;

    @OneToOne
    @Cascade(value = { org.hibernate.annotations.CascadeType.SAVE_UPDATE,
            org.hibernate.annotations.CascadeType.DETACH })
    @XmlElement(name = "AffectedObjects")
    @DynamicSerializeElement
    protected RegistryObjectListType affectedObjects;

    @OneToOne
    @Cascade(value = { org.hibernate.annotations.CascadeType.SAVE_UPDATE,
            org.hibernate.annotations.CascadeType.DETACH })
    @XmlElement(name = "AffectedObjectRefs")
    @DynamicSerializeElement
    protected ObjectRefListType affectedObjectRefs;

    @XmlAttribute(required = true)
    @DynamicSerializeElement
    protected String eventType;

    public Integer getKey() {
        return key;
    }

    public void setKey(Integer key) {
        this.key = key;
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
