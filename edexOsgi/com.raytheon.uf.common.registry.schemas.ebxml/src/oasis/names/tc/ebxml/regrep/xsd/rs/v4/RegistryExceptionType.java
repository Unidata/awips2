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
 * 
 */
@XmlRootElement
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
public class RegistryExceptionType extends ExtensibleObjectType {

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
        StringBuilder strBuilder = new StringBuilder();
        strBuilder.append("\n").append(this.getClass().getName()).append("\n");
        if (code == null) {
            strBuilder.append("    CODE: [").append("NONE SPECIFIED")
                    .append("]\n");
        } else {
            strBuilder.append("    CODE: [").append(code).append("]\n");
        }

        strBuilder.append("SEVERITY: [").append(severity).append("]\n");
        strBuilder.append(" MESSAGE: [").append(message).append("]\n");
        strBuilder.append("  DETAIL: [").append(detail).append("]\n");

        return strBuilder.toString();
    }
}
