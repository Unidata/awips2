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
package com.raytheon.uf.edex.registry.ebxml.services.query.types;

import java.math.BigInteger;
import java.util.ArrayList;
import java.util.Collection;
import java.util.List;

import oasis.names.tc.ebxml.regrep.xsd.query.v4.QueryRequest;
import oasis.names.tc.ebxml.regrep.xsd.query.v4.QueryResponse;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.QueryType;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.RegistryObjectType;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.SlotType;
import oasis.names.tc.ebxml.regrep.xsd.rs.v4.UnsupportedCapabilityExceptionType;

import org.springframework.transaction.annotation.Transactional;

import com.raytheon.uf.common.registry.constants.ErrorSeverity;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.edex.registry.ebxml.dao.RegistryObjectTypeDao;
import com.raytheon.uf.edex.registry.ebxml.exception.EbxmlRegistryException;
import com.raytheon.uf.edex.registry.ebxml.services.query.QueryManagerImpl;
import com.raytheon.uf.edex.registry.ebxml.services.query.QueryManagerImpl.RETURN_TYPE;
import com.raytheon.uf.edex.registry.ebxml.services.query.QueryParameters;
import com.raytheon.uf.edex.registry.ebxml.util.EbxmlExceptionUtil;
import com.raytheon.uf.edex.registry.ebxml.util.EbxmlObjectUtil;

/**
 * Abstract representation of a query used for querying the EBXML registry
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 2/21/2012    #184       bphillip     Initial creation
 * 3/18/2013    1802       bphillip    Modified to use transaction boundaries and spring dao injection
 * 4/9/2013     1802       bphillip    Refactor of registry query handling
 * 
 * </pre>
 * 
 * @author bphillip
 * @version 1.0
 */
@Transactional
public abstract class AbstractEbxmlQuery implements IRegistryQuery {

    protected static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(IRegistryQuery.class);

    protected abstract void query(QueryType queryType,
            QueryResponse queryResponse) throws EbxmlRegistryException;

    protected abstract List<String> getValidParameters();

    protected boolean matchOlderVersions = false;

    protected int maxResults = -1;

    protected RETURN_TYPE returnType;

    protected RegistryObjectTypeDao<RegistryObjectType> registryObjectDao;

    public void executeQuery(QueryRequest queryRequest,
            QueryResponse queryResponse) throws EbxmlRegistryException {
        /*
         * The full functionality of querying will be implemented at a later
         * time under a different ticket. Parts of this method have been removed
         * and will be more efficiently implemented
         */

        // TODO: Implement version matching using matchOlderVersions
        // TODO: Handle max results. Partially handled currently by some queries
        // TODO: Add support for specifying query depth
        // TODO: Add support for start index

        returnType = getReturnType(queryRequest.getResponseOption()
                .getReturnType());
        @SuppressWarnings("unused")
        int depth = queryRequest.getDepth().intValue();
        matchOlderVersions = queryRequest.isMatchOlderVersions();
        maxResults = queryRequest.getMaxResults().intValue();
        if (maxResults < 0) {
            maxResults = 0;
        }

        query(queryRequest.getQuery(), queryResponse);
        statusHandler.info("Query completed.");
    }

    protected QueryParameters getParameterMap(Collection<SlotType> slots,
            QueryResponse queryResponse, boolean suppressWarnings) {
        QueryParameters parameters = new QueryParameters();
        String slotName = null;
        for (SlotType slot : slots) {
            slotName = slot.getName();

            if (this.getValidParameters().contains(slotName)) {
                parameters.addParameter(slot);

            } else if (!suppressWarnings) {
                queryResponse
                        .getException()
                        .add(EbxmlExceptionUtil
                                .createRegistryException(
                                        UnsupportedCapabilityExceptionType.class,
                                        "",
                                        "Unsupported parameter specified",
                                        "The canonical query ["
                                                + this.getQueryDefinition()
                                                + "] does not support the "
                                                + slotName
                                                + " parameter.  This parameter will be ignored",
                                        ErrorSeverity.WARNING, statusHandler));
            }
        }
        return parameters;
    }

    protected void setResponsePayload(QueryResponse queryResponse,
            List<Object> values) {
        switch (returnType) {
        case ObjectRef:
            queryResponse.setObjectRefList(EbxmlObjectUtil
                    .createObjectRefList(values));
            queryResponse.setTotalResultCount(BigInteger.valueOf(queryResponse
                    .getObjectRefList().getObjectRef().size()));
            break;
        case RegistryObject:
            queryResponse.setRegistryObjectList(EbxmlObjectUtil
                    .createRegistryObjectList(values));
            queryResponse.setTotalResultCount(BigInteger.valueOf(queryResponse
                    .getRegistryObjectList().getRegistryObject().size()));
            break;
        case LeafClass:
            // TODO: Add support for this type
        case LeafClassWithRepositoryItem:
        default:
            // TODO: Add support for this type
            queryResponse
                    .getException()
                    .add(EbxmlExceptionUtil
                            .createRegistryException(
                                    UnsupportedCapabilityExceptionType.class,
                                    "",
                                    "Return type not currently not supported",
                                    "The ["
                                            + returnType
                                            + "] return type is currently not supported",
                                    ErrorSeverity.WARNING, statusHandler));
            break;

        }
    }

    protected QueryParameters getParameterMap(Collection<SlotType> slots,
            QueryResponse queryResponse) {
        return getParameterMap(slots, queryResponse, false);
    }

    private RETURN_TYPE getReturnType(String returnType) {
        for (RETURN_TYPE type : RETURN_TYPE.values()) {
            if (type.name().equals(returnType)) {
                return type;
            }
        }
        return QueryManagerImpl.DEFAULT_RETURN_TYPE;
    }

    @SuppressWarnings("unchecked")
    protected <T extends RegistryObjectType> List<T> filterResults(
            List<Object> results, Class<T> filterClass) {
        List<T> retVal = new ArrayList<T>();
        for (Object result : results) {
            if (result.getClass().isArray()) {
                Object[] resultArray = (Object[]) result;
                for (Object obj : resultArray) {
                    if (filterClass == null) {
                        if (obj instanceof RegistryObjectType) {
                            retVal.add((T) obj);
                        }
                    } else {
                        if (obj.getClass().equals(filterClass)) {
                            retVal.add((T) obj);
                        }
                    }

                }
            } else {
                retVal.add((T) result);
            }
        }
        return retVal;
    }

    protected <T extends RegistryObjectType> List<T> filterResults(
            List<Object> results) {
        return filterResults(results, null);
    }

    public void setRegistryObjectDao(
            RegistryObjectTypeDao<RegistryObjectType> registryObjectDao) {
        this.registryObjectDao = registryObjectDao;
    }

    public void setReturnType(RETURN_TYPE returnType) {
        this.returnType = returnType;
    }

}
