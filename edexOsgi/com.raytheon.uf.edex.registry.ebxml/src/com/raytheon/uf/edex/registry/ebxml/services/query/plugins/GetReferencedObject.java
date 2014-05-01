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
package com.raytheon.uf.edex.registry.ebxml.services.query.plugins;

import java.util.Arrays;
import java.util.Collections;

import javax.jws.WebMethod;
import javax.jws.WebParam;
import javax.jws.WebResult;

import oasis.names.tc.ebxml.regrep.wsdl.registry.services.v4.MsgRegistryException;
import oasis.names.tc.ebxml.regrep.wsdl.registry.services.v4.QueryManager;
import oasis.names.tc.ebxml.regrep.xsd.query.v4.QueryRequest;
import oasis.names.tc.ebxml.regrep.xsd.query.v4.QueryResponse;
import oasis.names.tc.ebxml.regrep.xsd.query.v4.ResponseOptionType;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.QueryType;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.RegistryObjectType;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.SlotType;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.StringValueType;

import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

import com.raytheon.uf.common.registry.EbxmlNamespaces;
import com.raytheon.uf.common.registry.constants.CanonicalQueryTypes;
import com.raytheon.uf.common.registry.constants.QueryReturnTypes;
import com.raytheon.uf.common.registry.services.RegistrySOAPServices;
import com.raytheon.uf.edex.registry.ebxml.exception.EbxmlRegistryException;
import com.raytheon.uf.edex.registry.ebxml.services.lifecycle.ObjectReferenceResolver;
import com.raytheon.uf.edex.registry.ebxml.services.query.QueryConstants;
import com.raytheon.uf.edex.registry.ebxml.util.EbxmlExceptionUtil;

/**
 * 
 * EBXML Canonical query to get a referenced object
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#     Engineer    Description
 * ------------ ----------  ----------- --------------------------
 * 9/18/2013    1705        bphillip    Initial implementation
 * 10/8/2013    1682       bphillip    Refactored querying
 * 10/30/2013   1538       bphillip    Changed to use non-static soap service client
 * </pre>
 * 
 * @author bphillip
 * @version 1
 */
public class GetReferencedObject extends RegistryQueryPlugin {

    /** Object used to resolve object references */
    private ObjectReferenceResolver referenceResolver;

    /** Registry soap service client */
    private RegistrySOAPServices registrySoapClient;

    @Override
    @WebMethod(action = EXECUTE_QUERY_ACTION)
    @WebResult(name = "QueryResponse", targetNamespace = EbxmlNamespaces.QUERY_URI, partName = "partQueryResponse")
    @Transactional(propagation = Propagation.MANDATORY, readOnly = true)
    public QueryResponse executeQuery(
            @WebParam(name = "QueryRequest", targetNamespace = EbxmlNamespaces.QUERY_URI, partName = "partQueryRequest") QueryRequest queryRequest)
            throws MsgRegistryException {
        QueryType queryType = queryRequest.getQuery();
        RegistryObjectType referencedObject = null;
        String objectReference = queryType
                .getSlotValue(QueryConstants.OBJECT_REFERENCE);
        if (objectReference == null) {
            return createResponse(Collections.emptyList());
        }

        referencedObject = referenceResolver
                .getStaticReferencedObject(objectReference);
        if (referencedObject == null) {
            try {
                referencedObject = referenceResolver
                        .getDynamicReferencedObject(objectReference);
            } catch (EbxmlRegistryException e) {
                throw EbxmlExceptionUtil.createMsgRegistryException(
                        "Error resolving object!", e);
            }
            if (referencedObject == null) {
                if (referenceResolver.isValidURL(objectReference)) {
                    String objectId = objectReference.substring(objectReference
                            .lastIndexOf("/") + 1);
                    String remoteAddress = objectReference.replace(
                            "/rest/registryObjects/" + objectId, "");
                    QueryType queryObj = new QueryType();
                    queryObj.setQueryDefinition(CanonicalQueryTypes.GET_REFERENCED_OBJECT);
                    SlotType slot = new SlotType(
                            QueryConstants.OBJECT_REFERENCE,
                            new StringValueType(objectId));
                    queryObj.getSlot().add(slot);
                    QueryRequest query = new QueryRequest();
                    query.setId("Resolve reference [" + objectReference + "]");
                    query.setQuery(queryObj);
                    query.setResponseOption(new ResponseOptionType(
                            QueryReturnTypes.REGISTRY_OBJECT, true));
                    QueryManager remoteQueryManager = registrySoapClient
                            .getQueryServiceForHost(remoteAddress);

                    QueryResponse remoteResponse = remoteQueryManager
                            .executeQuery(query);
                    referencedObject = remoteResponse.getRegistryObjects().get(
                            0);

                } else {
                    throw EbxmlExceptionUtil.createQueryExceptionType(
                            "Unable to resolve reference for value ["
                                    + objectReference + "]", "");
                }
            }
        }
        return createResponse(Arrays.asList(referencedObject));
    }

    public void setReferenceResolver(ObjectReferenceResolver referenceResolver) {
        this.referenceResolver = referenceResolver;
    }

    public void setRegistrySoapClient(RegistrySOAPServices registrySoapClient) {
        this.registrySoapClient = registrySoapClient;
    }

}
