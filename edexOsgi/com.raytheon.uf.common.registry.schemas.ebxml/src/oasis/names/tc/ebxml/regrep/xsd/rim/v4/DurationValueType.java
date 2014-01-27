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

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.Table;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.bind.annotation.XmlType;
import javax.xml.datatype.Duration;

import org.hibernate.annotations.Cache;
import org.hibernate.annotations.CacheConcurrencyStrategy;
import org.hibernate.annotations.Type;

import com.raytheon.uf.common.registry.RegrepUtil;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;

/**
 * 
 * A specialized ValueType that may be used as a container for a duration value.
 * 
 * 
 * <p>
 * Java class for DurationValueType complex type.
 * 
 * <p>
 * The following schema fragment specifies the expected content contained within
 * this class.
 * 
 * <pre>
 * &lt;complexType name="DurationValueType">
 *   &lt;complexContent>
 *     &lt;extension base="{urn:oasis:names:tc:ebxml-regrep:xsd:rim:4.0}ValueType">
 *       &lt;sequence>
 *         &lt;element name="Value" type="{http://www.w3.org/2001/XMLSchema}duration" minOccurs="0"/>
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
 * </pre>
 * 
 * @author bphillip
 * @version 1
 */
@XmlRootElement(name = "DurationValue")
@XmlAccessorType(XmlAccessType.FIELD)
@XmlType(name = "DurationValueType", propOrder = { "durationValue" })
@DynamicSerialize
@Entity
@Cache(region = RegrepUtil.DB_CACHE_REGION, usage = CacheConcurrencyStrategy.TRANSACTIONAL)
@Table(schema = RegrepUtil.EBXML_SCHEMA, name = "DurationValue")
public class DurationValueType extends ValueType {

    @XmlElement(name = "Value")
    @DynamicSerializeElement
    @Type(type = "com.raytheon.uf.common.registry.schemas.ebxml.util.DurationType")
    @Column
    protected Duration durationValue;

    public DurationValueType() {
        super();
    }

    public DurationValueType(Integer id) {
        super(id);
    }

    public DurationValueType(Duration durationValue) {
        super();
        this.durationValue = durationValue;
    }

    public DurationValueType(Integer id, Duration durationValue) {
        super(id);
        this.durationValue = durationValue;
    }

    @SuppressWarnings("unchecked")
    @Override
    public <T> T getValue() {
        return (T) getDurationValue();
    }

    public Duration getDurationValue() {
        return durationValue;
    }

    public void setDurationValue(Duration durationValue) {
        this.durationValue = durationValue;
    }

    @Override
    public int hashCode() {
        final int prime = 31;
        int result = super.hashCode();
        result = prime * result
                + ((durationValue == null) ? 0 : durationValue.hashCode());
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
        DurationValueType other = (DurationValueType) obj;
        if (durationValue == null) {
            if (other.durationValue != null)
                return false;
        } else if (!durationValue.equals(other.durationValue))
            return false;
        return true;
    }

    @Override
    public String toString() {
        return "DurationValueType [durationValue=" + durationValue + ", id="
                + id + "]";
    }

}
