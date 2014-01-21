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

import java.util.GregorianCalendar;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.bind.annotation.XmlSchemaType;
import javax.xml.bind.annotation.XmlType;
import javax.xml.datatype.DatatypeConfigurationException;
import javax.xml.datatype.DatatypeFactory;
import javax.xml.datatype.XMLGregorianCalendar;

import org.hibernate.annotations.Cache;
import org.hibernate.annotations.CacheConcurrencyStrategy;
import org.hibernate.annotations.Type;

import com.raytheon.uf.common.registry.RegrepUtil;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;

/**
 * 
 * A specialized ValueType that may be used as a container for a dateTime value.
 * 
 * 
 * <p>
 * Java class for DateTimeValueType complex type.
 * 
 * <p>
 * The following schema fragment specifies the expected content contained within
 * this class.
 * 
 * <pre>
 * &lt;complexType name="DateTimeValueType">
 *   &lt;complexContent>
 *     &lt;extension base="{urn:oasis:names:tc:ebxml-regrep:xsd:rim:4.0}ValueType">
 *       &lt;sequence>
 *         &lt;element name="Value" type="{http://www.w3.org/2001/XMLSchema}dateTime" minOccurs="0"/>
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
 * 10/23/2013    1538       bphillip    Added setTime method
 * 12/2/2013     1829       bphillip    Removed generic methods, 
 *                                      modified persistence annotations, added 
 *                                      constructors, hashCode, toString and equals
 * Jan 17, 2014 2125        rjpeter     Removed invalid @Table annotation.
 * </pre>
 * 
 * @author bphillip
 * @version 1
 */
@XmlRootElement(name = "DateTimeValue")
@XmlAccessorType(XmlAccessType.FIELD)
@XmlType(name = "DateTimeValueType", propOrder = { "dateTimeValue" })
@DynamicSerialize
@Entity
@Cache(region = RegrepUtil.DB_CACHE_REGION, usage = CacheConcurrencyStrategy.TRANSACTIONAL)
public class DateTimeValueType extends ValueType {

    @Column
    @Type(type = "com.raytheon.uf.common.registry.schemas.ebxml.util.XMLGregorianCalendarType")
    @XmlElement(name = "Value")
    @XmlSchemaType(name = "dateTime")
    @DynamicSerializeElement
    protected XMLGregorianCalendar dateTimeValue;

    public DateTimeValueType() {
        super();
    }

    public DateTimeValueType(Integer id) {
        super(id);
    }

    public DateTimeValueType(XMLGregorianCalendar dateTimeValue) {
        super();
        this.dateTimeValue = dateTimeValue;
    }

    public DateTimeValueType(Integer id, XMLGregorianCalendar dateTimeValue) {
        super(id);
        this.dateTimeValue = dateTimeValue;
    }

    public DateTimeValueType(Long time) {
        setTime(time);
    }

    @SuppressWarnings("unchecked")
    @Override
    public <T> T getValue() {
        return (T) getDateTimeValue();
    }

    public void setTime(long time) {
        GregorianCalendar cal = new GregorianCalendar();
        cal.setTimeInMillis(time);
        try {
            this.dateTimeValue = DatatypeFactory.newInstance()
                    .newXMLGregorianCalendar(cal);
        } catch (DatatypeConfigurationException e) {
            throw new RuntimeException("Error creating XMLGregorianCalendar!",
                    e);
        }
    }

    public XMLGregorianCalendar getDateTimeValue() {
        return dateTimeValue;
    }

    public void setDateTimeValue(XMLGregorianCalendar dateTimeValue) {
        this.dateTimeValue = dateTimeValue;
    }

    @Override
    public int hashCode() {
        final int prime = 31;
        int result = super.hashCode();
        result = (prime * result)
                + ((dateTimeValue == null) ? 0 : dateTimeValue.hashCode());
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
        DateTimeValueType other = (DateTimeValueType) obj;
        if (dateTimeValue == null) {
            if (other.dateTimeValue != null) {
                return false;
            }
        } else if (!dateTimeValue.equals(other.dateTimeValue)) {
            return false;
        }
        return true;
    }

    @Override
    public String toString() {
        return "DateTimeValueType [dateTimeValue=" + dateTimeValue + ", id="
                + id + "]";
    }

}
