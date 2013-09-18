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
package com.raytheon.uf.edex.registry.ebxml.services.query.types.canonical;

import java.util.ArrayList;
import java.util.List;

import oasis.names.tc.ebxml.regrep.wsdl.registry.services.v4.MsgRegistryException;
import oasis.names.tc.ebxml.regrep.wsdl.registry.services.v4.QueryManager;
import oasis.names.tc.ebxml.regrep.xsd.query.v4.QueryRequest;
import oasis.names.tc.ebxml.regrep.xsd.query.v4.QueryResponse;
import oasis.names.tc.ebxml.regrep.xsd.query.v4.ResponseOptionType;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.QueryType;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.RegistryObjectType;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.SlotType;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.StringValueType;

import com.raytheon.uf.common.registry.constants.CanonicalQueryTypes;
import com.raytheon.uf.common.registry.constants.QueryReturnTypes;
import com.raytheon.uf.common.registry.services.RegistrySOAPServices;
import com.raytheon.uf.edex.registry.ebxml.exception.EbxmlRegistryException;
import com.raytheon.uf.edex.registry.ebxml.services.lifecycle.ObjectReferenceResolver;
import com.raytheon.uf.edex.registry.ebxml.services.query.QueryConstants;
import com.raytheon.uf.edex.registry.ebxml.services.query.QueryParameters;
import com.raytheon.uf.edex.registry.ebxml.services.query.types.CanonicalEbxmlQuery;

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
 * </pre>
 * 
 * @author bphillip
 * @version 1
 */
public class GetReferencedObject extends CanonicalEbxmlQuery {

    /** The list of valid parameters for this query */
    private static final List<String> QUERY_PARAMETERS = new ArrayList<String>();

    /** Object used to resolve object references */
    private ObjectReferenceResolver referenceResolver;

    /* Initializes the list of parameters */
    static {
        QUERY_PARAMETERS.add(QueryConstants.OBJECT_REFERENCE);
    }

    @Override
    protected void query(QueryType queryType, QueryResponse queryResponse,
            String client) throws EbxmlRegistryException {
        RegistryObjectType referencedObject = null;
        QueryParameters parameters = getParameterMap(queryType.getSlot(),
                queryResponse);
        if (parameters.containsParameter(QueryConstants.OBJECT_REFERENCE)) {

            String refValue = (String) parameters.getParameter(
                    QueryConstants.OBJECT_REFERENCE).get(0);
            referencedObject = referenceResolver
                    .getStaticReferencedObject(refValue);
            if (referencedObject == null) {
                try {
                    referencedObject = referenceResolver
                            .getDynamicReferencedObject(refValue);
                } catch (MsgRegistryException e) {
                    throw new EbxmlRegistryException("Error resolving object!",
                            e);
                }
                if (referencedObject == null) {
                    if (referenceResolver.isValidURL(refValue)) {
                        String objectId = refValue.substring(refValue
                                .lastIndexOf("/") + 1);
                        String remoteAddress = refValue.replace(
                                "/rest/registryObjects/" + objectId, "");
                        QueryType queryObj = new QueryType();
                        queryObj.setQueryDefinition(CanonicalQueryTypes.GET_REFERENCED_OBJECT);
                        SlotType slot = new SlotType(
                                QueryConstants.OBJECT_REFERENCE,
                                new StringValueType(objectId));
                        queryObj.getSlot().add(slot);
                        QueryRequest query = new QueryRequest();
                        query.setId("Resolve reference [" + refValue + "]");
                        query.setQuery(queryObj);
                        query.setResponseOption(new ResponseOptionType(
                                QueryReturnTypes.REGISTRY_OBJECT, true));
                        QueryManager remoteQueryManager = RegistrySOAPServices
                                .getQueryServiceForHost(remoteAddress);
                        try {
                            QueryResponse remoteResponse = remoteQueryManager
                                    .executeQuery(query);
                            referencedObject = remoteResponse
                                    .getRegistryObjects().get(0);
                        } catch (MsgRegistryException e) {
                            throw new EbxmlRegistryException(
                                    "Error querying remote registry at ["
                                            + remoteAddress
                                            + "] to resolve reference to object "
                                            + objectId, e);
                        }
                    } else {
                        throw new EbxmlRegistryException(
                                "Unable to resolve reference for value ["
                                        + refValue + "]");
                    }
                }
            }

        } else {
            statusHandler.info("Query did not specify object reference");
        }
        List<Object> values = new ArrayList<Object>();
        values.add(referencedObject);
        this.setResponsePayload(queryResponse, values);
    }

    @Override
    protected List<String> getValidParameters() {
        return QUERY_PARAMETERS;
    }

    @Override
    public String getQueryDefinition() {
        return CanonicalQueryTypes.GET_REFERENCED_OBJECT;
    }

    public void setReferenceResolver(ObjectReferenceResolver referenceResolver) {
        this.referenceResolver = referenceResolver;
    }

}
