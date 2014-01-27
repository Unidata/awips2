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

package oasis.names.tc.ebxml.regrep.wsdl.registry.services.v4;

import javax.jws.WebMethod;
import javax.jws.WebParam;
import javax.jws.WebResult;
import javax.jws.WebService;
import javax.jws.soap.SOAPBinding;
import javax.xml.bind.annotation.XmlSeeAlso;

import oasis.names.tc.ebxml.regrep.xsd.spi.v4.ValidateObjectsRequest;
import oasis.names.tc.ebxml.regrep.xsd.spi.v4.ValidateObjectsResponse;

import org.apache.cxf.annotations.FastInfoset;

import com.raytheon.uf.common.registry.EbxmlNamespaces;

/**
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#     Engineer    Description
 * ------------ ----------  ----------- --------------------------
 * 2012                     bphillip    Initial implementation
 * 10/17/2013    1682       bphillip    Added software history
 * 12/9/2013     2613       bphillip    Changed to use FastInfoset
 * </pre>
 * 
 * @author bphillip
 * @version 1
 */
@FastInfoset
@WebService(name = "Validator", targetNamespace = EbxmlNamespaces.SPI_INT_URI)
@SOAPBinding(parameterStyle = SOAPBinding.ParameterStyle.BARE)
@XmlSeeAlso({ oasis.names.tc.ebxml.regrep.xsd.rim.v4.ObjectFactory.class,
        oasis.names.tc.ebxml.regrep.xsd.spi.v4.ObjectFactory.class,
        oasis.names.tc.ebxml.regrep.xsd.lcm.v4.ObjectFactory.class,
        org.w3.v1999.xlink.ObjectFactory.class,
        oasis.names.tc.ebxml.regrep.xsd.rs.v4.ObjectFactory.class,
        oasis.names.tc.ebxml.regrep.xsd.query.v4.ObjectFactory.class })
public interface Validator {

    public static final String VALIDATE_OBJECTS_ACTION = EbxmlNamespaces.OASIS_WSDL_PREFIX
            + "spi:bindings:4.0:Validator#validateObjects";

    /**
     * 
     * @param partValidateObjectsRequest
     * @return returns
     *         oasis.names.tc.ebxml_regrep.xsd.spi._4.ValidateObjectsResponse
     * @throws MsgRegistryException
     */
    @WebMethod(action = VALIDATE_OBJECTS_ACTION)
    @WebResult(name = "ValidateObjectsResponse", targetNamespace = EbxmlNamespaces.SPI_URI, partName = "partValidateObjectsResponse")
    public ValidateObjectsResponse validateObjects(
            @WebParam(name = "ValidateObjectsRequest", targetNamespace = EbxmlNamespaces.SPI_URI, partName = "partValidateObjectsRequest") ValidateObjectsRequest partValidateObjectsRequest)
            throws MsgRegistryException;

}
