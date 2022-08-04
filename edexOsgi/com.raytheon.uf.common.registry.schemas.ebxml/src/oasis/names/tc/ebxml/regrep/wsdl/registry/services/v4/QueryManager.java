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

import oasis.names.tc.ebxml.regrep.xsd.query.v4.QueryRequest;
import oasis.names.tc.ebxml.regrep.xsd.query.v4.QueryResponse;

import org.apache.cxf.annotations.FastInfoset;

import com.raytheon.uf.common.registry.schemas.ebxml.util.EbxmlNamespaces;

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
 * 2/19/2014     2769       bphillip    Added Transactional annotation to executeQuery method
 * 10/19/2020    8263       ksunil      Removed the Tx annotation declaration from the interface
 * </pre>
 * 
 * @author bphillip
 */
@FastInfoset
@WebService(name = "QueryManager", targetNamespace = EbxmlNamespaces.RR_INT_URI)
@SOAPBinding(parameterStyle = SOAPBinding.ParameterStyle.BARE)
@XmlSeeAlso({ oasis.names.tc.ebxml.regrep.xsd.rim.v4.ObjectFactory.class,
        oasis.names.tc.ebxml.regrep.xsd.spi.v4.ObjectFactory.class,
        oasis.names.tc.ebxml.regrep.xsd.lcm.v4.ObjectFactory.class,
        org.w3.v1999.xlink.ObjectFactory.class,
        oasis.names.tc.ebxml.regrep.xsd.rs.v4.ObjectFactory.class,
        oasis.names.tc.ebxml.regrep.xsd.query.v4.ObjectFactory.class })
public interface QueryManager {

    public static final String EXECUTE_QUERY_ACTION = EbxmlNamespaces.OASIS_WSDL_PREFIX
            + "registry:bindings:4.0:QueryManager#executeQuery";

    /**
     * 
     * @param partQueryRequest
     * @return returns oasis.names.tc.ebxml_regrep.xsd.query._4.QueryResponse
     * @throws MsgRegistryException
     */
    @WebMethod(action = EXECUTE_QUERY_ACTION)
    @WebResult(name = "QueryResponse", targetNamespace = EbxmlNamespaces.QUERY_URI, partName = "partQueryResponse")
    public QueryResponse executeQuery(
            @WebParam(name = "QueryRequest", targetNamespace = EbxmlNamespaces.QUERY_URI, partName = "partQueryRequest") QueryRequest queryRequest)
            throws MsgRegistryException;

}
