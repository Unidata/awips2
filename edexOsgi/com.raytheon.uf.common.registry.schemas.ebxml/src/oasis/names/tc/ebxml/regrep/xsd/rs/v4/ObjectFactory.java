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

import javax.xml.bind.JAXBElement;
import javax.xml.bind.annotation.XmlElementDecl;
import javax.xml.bind.annotation.XmlRegistry;
import javax.xml.namespace.QName;

import com.raytheon.uf.common.serialization.ISerializableObject;

/**
 * This object contains factory methods for each Java content interface and Java
 * element interface generated in the oasis.names.tc.ebxml_regrep.xsd.rs._4
 * package.
 * <p>
 * An ObjectFactory allows you to programatically construct new instances of the
 * Java representation for XML content. The Java representation of XML content
 * can consist of schema derived interfaces and classes representing the binding
 * of schema type definitions, element declarations and model groups. Factory
 * methods for each of these are provided in this class.
 * 
 */
@XmlRegistry
public class ObjectFactory implements ISerializableObject {

    private final static QName _RegistryResponse_QNAME = new QName(
            "urn:oasis:names:tc:ebxml-regrep:xsd:rs:4.0", "RegistryResponse");

    private final static QName _RegistryException_QNAME = new QName(
            "urn:oasis:names:tc:ebxml-regrep:xsd:rs:4.0", "RegistryException");

    private final static QName _RegistryRequest_QNAME = new QName(
            "urn:oasis:names:tc:ebxml-regrep:xsd:rs:4.0", "RegistryRequest");

    /**
     * Create a new ObjectFactory that can be used to create new instances of
     * schema derived classes for package: oasis.names.tc.ebxml_regrep.xsd.rs._4
     * 
     */
    public ObjectFactory() {
    }

    /**
     * Create an instance of {@link ReferencesExistExceptionType }
     * 
     */
    public ReferencesExistExceptionType createReferencesExistExceptionType() {
        return new ReferencesExistExceptionType();
    }

    /**
     * Create an instance of {@link UnresolvedReferenceExceptionType }
     * 
     */
    public UnresolvedReferenceExceptionType createUnresolvedReferenceExceptionType() {
        return new UnresolvedReferenceExceptionType();
    }

    /**
     * Create an instance of {@link UnsupportedCapabilityExceptionType }
     * 
     */
    public UnsupportedCapabilityExceptionType createUnsupportedCapabilityExceptionType() {
        return new UnsupportedCapabilityExceptionType();
    }

    /**
     * Create an instance of {@link TimeoutExceptionType }
     * 
     */
    public TimeoutExceptionType createTimeoutExceptionType() {
        return new TimeoutExceptionType();
    }

    /**
     * Create an instance of {@link RegistryResponseType }
     * 
     */
    public RegistryResponseType createRegistryResponseType() {
        return new RegistryResponseType();
    }

    /**
     * Create an instance of {@link InvalidRequestExceptionType }
     * 
     */
    public InvalidRequestExceptionType createInvalidRequestExceptionType() {
        return new InvalidRequestExceptionType();
    }

    /**
     * Create an instance of {@link ObjectNotFoundExceptionType }
     * 
     */
    public ObjectNotFoundExceptionType createObjectNotFoundExceptionType() {
        return new ObjectNotFoundExceptionType();
    }

    /**
     * Create an instance of {@link RegistryRequestType }
     * 
     */
    public RegistryRequestType createRegistryRequestType() {
        return new RegistryRequestType();
    }

    /**
     * Create an instance of {@link AuthenticationExceptionType }
     * 
     */
    public AuthenticationExceptionType createAuthenticationExceptionType() {
        return new AuthenticationExceptionType();
    }

    /**
     * Create an instance of {@link AuthorizationExceptionType }
     * 
     */
    public AuthorizationExceptionType createAuthorizationExceptionType() {
        return new AuthorizationExceptionType();
    }

    /**
     * Create an instance of {@link RegistryExceptionType }
     * 
     */
    public RegistryExceptionType createRegistryExceptionType() {
        return new RegistryExceptionType();
    }

    /**
     * Create an instance of {@link ObjectExistsExceptionType }
     * 
     */
    public ObjectExistsExceptionType createObjectExistsExceptionType() {
        return new ObjectExistsExceptionType();
    }

    /**
     * Create an instance of {@link QuotaExceededExceptionType }
     * 
     */
    public QuotaExceededExceptionType createQuotaExceededExceptionType() {
        return new QuotaExceededExceptionType();
    }

    /**
     * Create an instance of {@link JAXBElement }{@code <}
     * {@link RegistryResponseType }{@code >}
     * 
     */
    @XmlElementDecl(namespace = "urn:oasis:names:tc:ebxml-regrep:xsd:rs:4.0", name = "RegistryResponse")
    public JAXBElement<RegistryResponseType> createRegistryResponse(
            RegistryResponseType value) {
        return new JAXBElement<RegistryResponseType>(_RegistryResponse_QNAME,
                RegistryResponseType.class, null, value);
    }

    /**
     * Create an instance of {@link JAXBElement }{@code <}
     * {@link RegistryExceptionType }{@code >}
     * 
     */
    @XmlElementDecl(namespace = "urn:oasis:names:tc:ebxml-regrep:xsd:rs:4.0", name = "RegistryException")
    public JAXBElement<RegistryExceptionType> createRegistryException(
            RegistryExceptionType value) {
        return new JAXBElement<RegistryExceptionType>(_RegistryException_QNAME,
                RegistryExceptionType.class, null, value);
    }

    /**
     * Create an instance of {@link JAXBElement }{@code <}
     * {@link RegistryRequestType }{@code >}
     * 
     */
    @XmlElementDecl(namespace = "urn:oasis:names:tc:ebxml-regrep:xsd:rs:4.0", name = "RegistryRequest")
    public JAXBElement<RegistryRequestType> createRegistryRequest(
            RegistryRequestType value) {
        return new JAXBElement<RegistryRequestType>(_RegistryRequest_QNAME,
                RegistryRequestType.class, null, value);
    }

}
