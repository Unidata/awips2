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
import java.util.List;

import javax.persistence.CascadeType;
import javax.persistence.Entity;
import javax.persistence.FetchType;
import javax.persistence.GeneratedValue;
import javax.persistence.Id;
import javax.persistence.JoinColumn;
import javax.persistence.OneToMany;
import javax.persistence.SequenceGenerator;
import javax.persistence.Table;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.bind.annotation.XmlTransient;
import javax.xml.bind.annotation.XmlType;

import org.hibernate.annotations.Cache;
import org.hibernate.annotations.CacheConcurrencyStrategy;

import com.raytheon.uf.common.registry.RegrepUtil;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;

/**
 * 
 * A type that is used throughout the schema whenever a textual value needs to
 * be represented in multiple local languages. It has a sequence of
 * LocalizedString instances, where each String is specific to a particular
 * locale.
 * 
 * 
 * <p>
 * Java class for InternationalStringType complex type.
 * 
 * <p>
 * The following schema fragment specifies the expected content contained within
 * this class.
 * 
 * <pre>
 * &lt;complexType name="InternationalStringType">
 *   &lt;complexContent>
 *     &lt;restriction base="{http://www.w3.org/2001/XMLSchema}anyType">
 *       &lt;sequence>
 *         &lt;element name="LocalizedString" type="{urn:oasis:names:tc:ebxml-regrep:xsd:rim:4.0}LocalizedStringType" maxOccurs="unbounded" minOccurs="0"/>
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
 * </pre>
 * 
 * @author bphillip
 * @version 1
 */
@XmlRootElement(name = "InternationalString")
@XmlAccessorType(XmlAccessType.FIELD)
@DynamicSerialize
@XmlType(name = "InternationalStringType", propOrder = { "localizedString" })
@Entity
@Cache(region = RegrepUtil.DB_CACHE_REGION, usage = CacheConcurrencyStrategy.TRANSACTIONAL, include = "all")
@Table(schema = RegrepUtil.EBXML_SCHEMA, name = "InternationalString")
public class InternationalStringType implements Serializable {

    private static final long serialVersionUID = 2414977045816695691L;

    @Id
    @SequenceGenerator(name = "InternationalStringTypeGenerator", schema = RegrepUtil.EBXML_SCHEMA, sequenceName = RegrepUtil.EBXML_SCHEMA
            + ".InternationalString_sequence")
    @GeneratedValue(generator = "InternationalStringTypeGenerator")
    @XmlTransient
    private Integer id;

    @OneToMany(cascade = CascadeType.ALL, fetch = FetchType.LAZY)
    @JoinColumn(name = "string_id", nullable = false, referencedColumnName = "id")
    @XmlElement(name = "LocalizedString")
    @DynamicSerializeElement
    protected List<LocalizedStringType> localizedString;

    public InternationalStringType() {

    }

    public InternationalStringType(String lang, String value) {
        this.getLocalizedString().add(new LocalizedStringType(lang, value));
    }

    public InternationalStringType(String value) {
        this.getLocalizedString().add(new LocalizedStringType(value));
    }

    /**
     * Gets the value of the localizedString property.
     * 
     * <p>
     * This accessor method returns a reference to the live list, not a
     * snapshot. Therefore any modification you make to the returned list will
     * be present inside the JAXB object. This is why there is not a
     * <CODE>set</CODE> method for the localizedString property.
     * 
     * <p>
     * For example, to add a new item, do as follows:
     * 
     * <pre>
     * getLocalizedString().add(newItem);
     * </pre>
     * 
     * 
     * <p>
     * Objects of the following type(s) are allowed in the list
     * {@link LocalizedStringType }
     * 
     * 
     */
    public List<LocalizedStringType> getLocalizedString() {
        if (localizedString == null) {
            localizedString = new ArrayList<LocalizedStringType>();
        }
        return this.localizedString;
    }

    public void setLocalizedString(List<LocalizedStringType> localizedString) {
        this.localizedString = localizedString;
    }

    /*
     * (non-Javadoc)
     * 
     * @see java.lang.Object#hashCode()
     */
    @Override
    public int hashCode() {
        final int prime = 31;
        int result = 1;
        for (LocalizedStringType str : localizedString) {
            result = prime * result + ((str == null) ? 0 : str.hashCode());
        }
        return result;
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
        InternationalStringType other = (InternationalStringType) obj;
        if (localizedString == null) {
            if (other.localizedString != null) {
                return false;
            }
        } else if (!localizedString.equals(other.localizedString)) {
            return false;
        }
        return true;
    }

    @Override
    public String toString() {
        return "InternationalStringType [id=" + id + ", localizedString="
                + localizedString + "]";
    }

    public Integer getId() {
        return id;
    }

    public void setId(Integer id) {
        this.id = id;
    }

}
