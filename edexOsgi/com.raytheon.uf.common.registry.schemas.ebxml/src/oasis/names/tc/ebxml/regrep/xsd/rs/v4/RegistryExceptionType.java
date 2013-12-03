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

package oasis.names.tc.ebxml.regrep.xsd.rs.v4;

import javax.persistence.Entity;
import javax.persistence.Table;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.bind.annotation.XmlSeeAlso;
import javax.xml.bind.annotation.XmlType;

import oasis.names.tc.ebxml.regrep.xsd.query.v4.QueryExceptionType;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.ExtensibleObjectType;
import oasis.names.tc.ebxml.regrep.xsd.spi.v4.CatalogingExceptionType;
import oasis.names.tc.ebxml.regrep.xsd.spi.v4.FilteringExceptionType;
import oasis.names.tc.ebxml.regrep.xsd.spi.v4.ValidationExceptionType;

import org.hibernate.annotations.Cache;
import org.hibernate.annotations.CacheConcurrencyStrategy;

import com.raytheon.uf.common.registry.RegrepUtil;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;

/**
 * Base for all registry exceptions. Based upon SOAPFault:
 * http://www.w3schools.com/soap/soap_fault.asp
 * 
 * <p>
 * Java class for RegistryExceptionType complex type.
 * 
 * <p>
 * The following schema fragment specifies the expected content contained within
 * this class.
 * 
 * <pre>
 * &lt;complexType name="RegistryExceptionType">
 *   &lt;complexContent>
 *     &lt;extension base="{urn:oasis:names:tc:ebxml-regrep:xsd:rim:4.0}ExtensibleObjectType">
 *       &lt;attribute name="code" type="{http://www.w3.org/2001/XMLSchema}string" />
 *       &lt;attribute name="detail" type="{http://www.w3.org/2001/XMLSchema}string" />
 *       &lt;attribute name="message" type="{http://www.w3.org/2001/XMLSchema}string" />
 *       &lt;attribute name="severity" type="{urn:oasis:names:tc:ebxml-regrep:xsd:rim:4.0}objectReferenceType" default="urn:oasis:names:tc:ebxml-regrep:ErrorSeverityType:Error" />
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
 * 12/2/2013     1829       bphillip    Made ExtensibleObjectType persistable, 
 *                                      modified persistence annotations, added 
 *                                      constructors, hashCode, toString and equals
 * </pre>
 * 
 * @author bphillip
 * @version 1
 */
@XmlRootElement(name = "RegistryException")
@XmlAccessorType(XmlAccessType.FIELD)
@XmlType(name = "RegistryExceptionType")
@XmlSeeAlso({ ObjectNotFoundExceptionType.class,
        UnsupportedCapabilityExceptionType.class,
        AuthenticationExceptionType.class, InvalidRequestExceptionType.class,
        ReferencesExistExceptionType.class, TimeoutExceptionType.class,
        QuotaExceededExceptionType.class, AuthorizationExceptionType.class,
        UnresolvedReferenceExceptionType.class,
        ObjectExistsExceptionType.class, QueryExceptionType.class,
        FilteringExceptionType.class, ValidationExceptionType.class,
        CatalogingExceptionType.class })
@DynamicSerialize
@Entity
@Cache(region = RegrepUtil.DB_CACHE_REGION, usage = CacheConcurrencyStrategy.TRANSACTIONAL)
@Table(schema = RegrepUtil.EBXML_SCHEMA, name = "RegistryException")
public class RegistryExceptionType extends ExtensibleObjectType {

    private static final long serialVersionUID = -811672255660498468L;

    @XmlAttribute
    @DynamicSerializeElement
    protected String code;

    @XmlAttribute
    @DynamicSerializeElement
    protected String detail;

    @XmlAttribute
    @DynamicSerializeElement
    protected String message;

    @XmlAttribute
    @DynamicSerializeElement
    protected String severity;

    public RegistryExceptionType() {
        super();
    }

    /**
     * Gets the value of the code property.
     * 
     * @return possible object is {@link String }
     * 
     */
    public String getCode() {
        return code;
    }

    /**
     * Sets the value of the code property.
     * 
     * @param value
     *            allowed object is {@link String }
     * 
     */
    public void setCode(String value) {
        this.code = value;
    }

    /**
     * Gets the value of the detail property.
     * 
     * @return possible object is {@link String }
     * 
     */
    public String getDetail() {
        return detail;
    }

    /**
     * Sets the value of the detail property.
     * 
     * @param value
     *            allowed object is {@link String }
     * 
     */
    public void setDetail(String value) {
        this.detail = value;
    }

    /**
     * Gets the value of the message property.
     * 
     * @return possible object is {@link String }
     * 
     */
    public String getMessage() {
        return message;
    }

    /**
     * Sets the value of the message property.
     * 
     * @param value
     *            allowed object is {@link String }
     * 
     */
    public void setMessage(String value) {
        this.message = value;
    }

    /**
     * Gets the value of the severity property.
     * 
     * @return possible object is {@link String }
     * 
     */
    public String getSeverity() {
        if (severity == null) {
            return "urn:oasis:names:tc:ebxml-regrep:ErrorSeverityType:Error";
        } else {
            return severity;
        }
    }

    /**
     * Sets the value of the severity property.
     * 
     * @param value
     *            allowed object is {@link String }
     * 
     */
    public void setSeverity(String value) {
        this.severity = value;
    }

    @Override
    public String toString() {
        StringBuilder builder = new StringBuilder();
        builder.append("RegistryExceptionType \n[id=");
        builder.append(id);
        builder.append(", \nslot=");
        builder.append(slot);
        builder.append(", \ncode=");
        builder.append(code);
        builder.append(", \ndetail=");
        builder.append(detail);
        builder.append(", \nmessage=");
        builder.append(message);
        builder.append(", \nseverity=");
        builder.append(severity);
        builder.append("]");
        return builder.toString();
    }

    @Override
    public int hashCode() {
        final int prime = 31;
        int result = super.hashCode();
        result = prime * result + ((code == null) ? 0 : code.hashCode());
        result = prime * result + ((detail == null) ? 0 : detail.hashCode());
        result = prime * result + ((message == null) ? 0 : message.hashCode());
        result = prime * result
                + ((severity == null) ? 0 : severity.hashCode());
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
        RegistryExceptionType other = (RegistryExceptionType) obj;
        if (code == null) {
            if (other.code != null)
                return false;
        } else if (!code.equals(other.code))
            return false;
        if (detail == null) {
            if (other.detail != null)
                return false;
        } else if (!detail.equals(other.detail))
            return false;
        if (message == null) {
            if (other.message != null)
                return false;
        } else if (!message.equals(other.message))
            return false;
        if (severity == null) {
            if (other.severity != null)
                return false;
        } else if (!severity.equals(other.severity))
            return false;
        return true;
    }

}
