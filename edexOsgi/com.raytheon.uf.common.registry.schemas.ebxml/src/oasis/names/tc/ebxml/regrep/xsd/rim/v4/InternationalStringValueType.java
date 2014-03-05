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
import javax.persistence.JoinColumn;
import javax.persistence.OneToOne;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.bind.annotation.XmlType;

import org.hibernate.annotations.Cache;
import org.hibernate.annotations.CacheConcurrencyStrategy;

import com.raytheon.uf.common.registry.RegrepUtil;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;

/**
 * 
 * A specialized ValueType that may be used as a container for a
 * InternationalString value.
 * 
 * 
 * <p>
 * Java class for InternationalStringValueType complex type.
 * 
 * <p>
 * The following schema fragment specifies the expected content contained within
 * this class.
 * 
 * <pre>
 * &lt;complexType name="InternationalStringValueType">
 *   &lt;complexContent>
 *     &lt;extension base="{urn:oasis:names:tc:ebxml-regrep:xsd:rim:4.0}ValueType">
 *       &lt;sequence>
 *         &lt;element name="Value" type="{urn:oasis:names:tc:ebxml-regrep:xsd:rim:4.0}InternationalStringType" minOccurs="0"/>
 *       &lt;/sequence>
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
 * 12/2/2013     1829       bphillip    Removed generic methods, 
 *                                      modified persistence annotations, added 
 *                                      constructors, hashCode, toString and equals
 * Jan 17, 2014 2125        rjpeter     Removed invalid @Table annotation.
 * </pre>
 * 
 * @author bphillip
 * @version 1
 */
@XmlRootElement(name = "InternationalStringValue")
@XmlAccessorType(XmlAccessType.FIELD)
@XmlType(name = "InternationalStringValueType", propOrder = { "internationalStringValue" })
@DynamicSerialize
@Entity
@Cache(region = RegrepUtil.DB_CACHE_REGION, usage = CacheConcurrencyStrategy.TRANSACTIONAL)
public class InternationalStringValueType extends ValueType {

    @XmlElement(name = "Value")
    @DynamicSerializeElement
    @OneToOne(cascade = CascadeType.ALL)
    @JoinColumn(name = "internationalstring_id", referencedColumnName = "id")
    protected InternationalStringType internationalStringValue;

    public InternationalStringValueType() {
        super();
    }

    public InternationalStringValueType(Integer id) {
        super(id);
    }

    public InternationalStringValueType(
            InternationalStringType internationalStringValue) {
        super();
        this.internationalStringValue = internationalStringValue;
    }

    public InternationalStringValueType(Integer id,
            InternationalStringType internationalStringValue) {
        super(id);
        this.internationalStringValue = internationalStringValue;
    }

    @SuppressWarnings("unchecked")
    @Override
    public <T> T getValue() {
        return (T) getInternationalStringValue();
    }

    public InternationalStringType getInternationalStringValue() {
        return internationalStringValue;
    }

    public void setInternationalStringValue(
            InternationalStringType internationalStringValue) {
        this.internationalStringValue = internationalStringValue;
    }

    @Override
    public int hashCode() {
        final int prime = 31;
        int result = super.hashCode();
        result = (prime * result)
                + ((internationalStringValue == null) ? 0
                        : internationalStringValue.hashCode());
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
        InternationalStringValueType other = (InternationalStringValueType) obj;
        if (internationalStringValue == null) {
            if (other.internationalStringValue != null) {
                return false;
            }
        } else if (!internationalStringValue
                .equals(other.internationalStringValue)) {
            return false;
        }
        return true;
    }

    @Override
    public String toString() {
        return "InternationalStringValueType [internationalStringValue="
                + internationalStringValue + ", id=" + id + "]";
    }

}
