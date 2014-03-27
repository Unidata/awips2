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
 * A specialized ValueType that may be used as a container for a String value.
 * This type is the most commonly used to specify slot values.
 * 
 * 
 * <p>
 * Java class for StringValueType complex type.
 * 
 * <p>
 * The following schema fragment specifies the expected content contained within
 * this class.
 * 
 * <pre>
 * &lt;complexType name="StringValueType">
 *   &lt;complexContent>
 *     &lt;extension base="{urn:oasis:names:tc:ebxml-regrep:xsd:rim:4.0}ValueType">
 *       &lt;sequence>
 *         &lt;element name="Value" type="{urn:oasis:names:tc:ebxml-regrep:xsd:rim:4.0}LongText" minOccurs="0"/>
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
 * Jan 17, 2014  2125       rjpeter     Removed invalid @Table annotation.
 * </pre>
 * 
 * @author bphillip
 * @version 1
 */
@XmlRootElement(name = "StringValue")
@XmlAccessorType(XmlAccessType.FIELD)
@XmlType(name = "StringValueType", propOrder = { "stringValue" })
@DynamicSerialize
@Entity
@Cache(region = RegrepUtil.DB_CACHE_REGION, usage = CacheConcurrencyStrategy.TRANSACTIONAL)
public class StringValueType extends ValueType {

    @Column(columnDefinition = "text")
    @XmlElement(name = "Value")
    @DynamicSerializeElement
    protected String stringValue;

    public StringValueType() {
        super();
    }

    public StringValueType(Integer id) {
        super(id);
    }

    public StringValueType(String stringValue) {
        super();
        this.stringValue = stringValue;
    }

    public StringValueType(Integer id, String stringValue) {
        super(id);
        this.stringValue = stringValue;
    }

    @SuppressWarnings("unchecked")
    @Override
    public <T> T getValue() {
        return (T) getStringValue();
    }

    public String getStringValue() {
        return stringValue;
    }

    public void setStringValue(String stringValue) {
        this.stringValue = stringValue;
    }

    @Override
    public int hashCode() {
        final int prime = 31;
        int result = super.hashCode();
        result = (prime * result)
                + ((stringValue == null) ? 0 : stringValue.hashCode());
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
        StringValueType other = (StringValueType) obj;
        if (stringValue == null) {
            if (other.stringValue != null) {
                return false;
            }
        } else if (!stringValue.equals(other.stringValue)) {
            return false;
        }
        return true;
    }

    @Override
    public String toString() {
        return "StringValueType [stringValue=" + stringValue + ", id=" + id
                + "]";
    }

}
