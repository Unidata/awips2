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
import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import java.util.Objects;

import jakarta.persistence.Entity;
import jakarta.persistence.FetchType;
import jakarta.persistence.GeneratedValue;
import jakarta.persistence.Id;
import jakarta.persistence.JoinColumn;
import jakarta.persistence.JoinTable;
import jakarta.persistence.ManyToMany;
import jakarta.persistence.SequenceGenerator;
import jakarta.persistence.Table;
import jakarta.xml.bind.annotation.XmlAccessType;
import jakarta.xml.bind.annotation.XmlAccessorType;
import jakarta.xml.bind.annotation.XmlElement;
import jakarta.xml.bind.annotation.XmlRootElement;
import jakarta.xml.bind.annotation.XmlTransient;
import jakarta.xml.bind.annotation.XmlType;

import org.hibernate.annotations.Cache;
import org.hibernate.annotations.CacheConcurrencyStrategy;
import org.hibernate.annotations.Cascade;
import org.hibernate.annotations.CascadeType;

import com.raytheon.uf.common.registry.schemas.ebxml.util.RegrepUtil;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;

/**
 *
 * Represents a list of RegistryObjectType instances.
 *
 *
 * <p>
 * Java class for RegistryObjectListType complex type.
 *
 * <p>
 * The following schema fragment specifies the expected content contained within
 * this class.
 *
 * <pre>
 * &lt;complexType name="RegistryObjectListType">
 *   &lt;complexContent>
 *     &lt;restriction base="{http://www.w3.org/2001/XMLSchema}anyType">
 *       &lt;sequence>
 *         &lt;element ref="{urn:oasis:names:tc:ebxml-regrep:xsd:rim:4.0}RegistryObject" maxOccurs="unbounded" minOccurs="0"/>
 *       &lt;/sequence>
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
 * 12/2/2013     1829       bphillip    Modified persistence annotations, added
 *                                      constructors, hashCode, toString and equals
 * Aug 12, 2019  6140       tgurney     Explicitly specify join column names
 * Feb 08, 2022  7849       mapeters    Update cascade annotation to support deletion
 * </pre>
 *
 * @author bphillip
 */
@XmlRootElement(name = "RegistryObjectList")
@XmlAccessorType(XmlAccessType.FIELD)
@XmlType(name = "RegistryObjectListType", propOrder = { "registryObject" })
@DynamicSerialize
@Entity
@Cache(region = RegrepUtil.DB_CACHE_REGION, usage = CacheConcurrencyStrategy.TRANSACTIONAL)
@Table(schema = RegrepUtil.EBXML_SCHEMA, name = "RegistryObjectList")
public class RegistryObjectListType implements Serializable {

    private static final long serialVersionUID = -254507015539461400L;

    @Id
    @SequenceGenerator(name = "RegistryObjectListGenerator", schema = RegrepUtil.EBXML_SCHEMA, sequenceName = RegrepUtil.EBXML_SCHEMA
            + ".RegistryObjectList_sequence")
    @GeneratedValue(generator = "RegistryObjectListGenerator")
    @XmlTransient
    private int id;

    @ManyToMany(fetch = FetchType.EAGER)
    @Cascade({ CascadeType.ALL })
    @JoinTable(schema = RegrepUtil.EBXML_SCHEMA, joinColumns = {
            @JoinColumn(name = "registryobjectlist_id") })
    @XmlElement(name = "RegistryObject")
    @DynamicSerializeElement
    protected List<RegistryObjectType> registryObject;

    /**
     * Constructor.
     */
    public RegistryObjectListType() {

    }

    /**
     * Constructor.
     *
     * @param registryObjects
     *            the collection of registry objects
     */
    public RegistryObjectListType(List<RegistryObjectType> registryObjects) {
        // Defensive list copy, not using the original list
        this.registryObject = new ArrayList<>(registryObjects);
    }

    public RegistryObjectListType(RegistryObjectType registryObject) {
        List<RegistryObjectType> list = new ArrayList<>(1);
        list.add(registryObject);
        this.registryObject = list;
    }

    public int getId() {
        return id;
    }

    public void setId(int id) {
        this.id = id;
    }

    /**
     * Gets the value of the registryObject property.
     *
     * <p>
     * This accessor method returns a reference to the live list, not a
     * snapshot. Therefore any modification you make to the returned list will
     * be present inside the JAXB object. This is why there is not a
     * <CODE>set</CODE> method for the registryObject property.
     *
     * <p>
     * For example, to add a new item, do as follows:
     *
     * <pre>
     * getRegistryObject().add(newItem);
     * </pre>
     *
     *
     * <p>
     * Objects of the following type(s) are allowed in the list
     * {@link RegistryObjectType }
     *
     *
     */
    public List<RegistryObjectType> getRegistryObject() {
        if (registryObject == null) {
            registryObject = new ArrayList<>();
        }
        return this.registryObject;
    }

    public void setRegistryObject(List<RegistryObjectType> registryObject) {
        this.registryObject = registryObject;
    }

    public void addRegistryObjects(
            Collection<RegistryObjectType> registryObject) {
        getRegistryObject().addAll(registryObject);
    }

    /*
     * (non-Javadoc)
     *
     * @see java.lang.Object#hashCode()
     */
    @Override
    public int hashCode() {
        return Objects.hash(registryObject);
    }

    /*
     * (non-Javadoc)
     *
     * @see java.lang.Object#equals(java.lang.Object)
     */
    @Override
    public boolean equals(Object obj) {
        if (this == obj) {
            return true;
        }
        if (obj == null) {
            return false;
        }
        if (getClass() != obj.getClass()) {
            return false;
        }
        RegistryObjectListType other = (RegistryObjectListType) obj;
        if (!Objects.equals(registryObject, other.registryObject)) {
            return false;
        }
        return true;
    }

    @Override
    public String toString() {
        StringBuilder builder = new StringBuilder();
        builder.append("RegistryObjectListType \n[registryObject=");
        builder.append(registryObject);
        builder.append("]");
        return builder.toString();
    }

}
