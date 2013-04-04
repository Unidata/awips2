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

package oasis.names.tc.ebxml.regrep.xsd.query.v4;

import javax.xml.bind.JAXBElement;
import javax.xml.bind.annotation.XmlElementDecl;
import javax.xml.bind.annotation.XmlRegistry;
import javax.xml.namespace.QName;

import com.raytheon.uf.common.serialization.ISerializableObject;

/**
 * This object contains factory methods for each Java content interface and Java
 * element interface generated in the oasis.names.tc.ebxml_regrep.xsd.query._4
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

    private final static QName _ResponseOption_QNAME = new QName(
            "urn:oasis:names:tc:ebxml-regrep:xsd:query:4.0", "ResponseOption");

    /**
     * Create a new ObjectFactory that can be used to create new instances of
     * schema derived classes for package:
     * oasis.names.tc.ebxml_regrep.xsd.query._4
     * 
     */
    public ObjectFactory() {
    }

    /**
     * Create an instance of {@link ResponseOptionType }
     * 
     */
    public ResponseOptionType createResponseOptionType() {
        return new ResponseOptionType();
    }

    /**
     * Create an instance of {@link QueryRequest }
     * 
     */
    public QueryRequest createQueryRequest() {
        return new QueryRequest();
    }

    /**
     * Create an instance of {@link QueryResponse }
     * 
     */
    public QueryResponse createQueryResponse() {
        return new QueryResponse();
    }

    /**
     * Create an instance of {@link QueryExceptionType }
     * 
     */
    public QueryExceptionType createQueryExceptionType() {
        return new QueryExceptionType();
    }

    /**
     * Create an instance of {@link JAXBElement }{@code <}
     * {@link ResponseOptionType }{@code >}
     * 
     */
    @XmlElementDecl(namespace = "urn:oasis:names:tc:ebxml-regrep:xsd:query:4.0", name = "ResponseOption")
    public JAXBElement<ResponseOptionType> createResponseOption(
            ResponseOptionType value) {
        return new JAXBElement<ResponseOptionType>(_ResponseOption_QNAME,
                ResponseOptionType.class, null, value);
    }

}
