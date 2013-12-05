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
import javax.xml.bind.annotation.XmlAnyElement;
import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.bind.annotation.XmlType;

import org.hibernate.annotations.Cache;
import org.hibernate.annotations.CacheConcurrencyStrategy;
import org.hibernate.annotations.Type;

import com.raytheon.uf.common.registry.RegrepUtil;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;

/**
 * 
 * A specialized ValueType that may be used as a container for any well-formed
 * XML element value in any namespace. Use this to specify an XML element as the
 * value for a Slot.
 * 
 * 
 * <p>
 * Java class for AnyValueType complex type.
 * 
 * <p>
 * The following schema fragment specifies the expected content contained within
 * this class.
 * 
 * <pre>
 * &lt;complexType name="AnyValueType">
 *   &lt;complexContent>
 *     &lt;extension base="{urn:oasis:names:tc:ebxml-regrep:xsd:rim:4.0}ValueType">
 *       &lt;sequence>
 *         &lt;any processContents='lax' minOccurs="0"/>
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
 * 
 */
@Entity
@Cache(region = RegrepUtil.DB_CACHE_REGION, usage = CacheConcurrencyStrategy.TRANSACTIONAL)
@Table(schema = RegrepUtil.EBXML_SCHEMA, name = "AnyValue")
@XmlRootElement(name = "AnyValue")
@XmlAccessorType(XmlAccessType.FIELD)
@XmlType(name = "AnyValueType", propOrder = { "anyValue" })
@DynamicSerialize
public class AnyValueType extends ValueType {

    @XmlAnyElement(lax = true)
    @DynamicSerializeElement
    @Column(columnDefinition = "text")
    @Type(type = "com.raytheon.uf.common.registry.schemas.ebxml.util.SerializedType")
    protected Object anyValue;

    public AnyValueType() {
        super();
    }

    public AnyValueType(Integer id) {
        super(id);
    }

    public AnyValueType(Object anyValue) {
        super();
        this.anyValue = anyValue;
    }

    public AnyValueType(Integer id, Object anyValue) {
        super(id);
        this.anyValue = anyValue;
    }

    @SuppressWarnings("unchecked")
    @Override
    public <T> T getValue() {
        return (T) getAnyValue();
    }

    public Object getAnyValue() {
        return anyValue;
    }

    public void setAnyValue(Object anyValue) {
        this.anyValue = anyValue;
    }

    @Override
    public int hashCode() {
        final int prime = 31;
        int result = super.hashCode();
        result = prime * result
                + ((anyValue == null) ? 0 : anyValue.hashCode());
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
        AnyValueType other = (AnyValueType) obj;
        if (anyValue == null) {
            if (other.anyValue != null)
                return false;
        } else if (!anyValue.equals(other.anyValue))
            return false;
        return true;
    }

    @Override
    public String toString() {
        return "AnyValueType [anyValue=" + anyValue + ", id=" + id + "]";
    }

}
