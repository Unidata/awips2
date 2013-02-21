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
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import oasis.names.tc.ebxml.regrep.xsd.query.v4.QueryRequest;
import oasis.names.tc.ebxml.regrep.xsd.query.v4.QueryResponse;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.QueryType;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.RegistryObjectListType;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.RegistryObjectType;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.SlotType;
import oasis.names.tc.ebxml.regrep.xsd.rs.v4.UnsupportedCapabilityExceptionType;

import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.edex.registry.ebxml.constants.ErrorSeverity;
import com.raytheon.uf.edex.registry.ebxml.dao.RegistryObjectTypeDao;
import com.raytheon.uf.edex.registry.ebxml.exception.EbxmlRegistryException;
import com.raytheon.uf.edex.registry.ebxml.services.query.QueryManagerImpl;
import com.raytheon.uf.edex.registry.ebxml.services.query.QueryParameters;
import com.raytheon.uf.edex.registry.ebxml.services.query.QueryManagerImpl.RETURN_TYPE;
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
 * 
 * </pre>
 * 
 * @author bphillip
 * @version 1.0
 */
public abstract class AbstractEbxmlQuery implements IRegistryQuery {

    protected static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(IRegistryQuery.class);

    protected abstract <T extends RegistryObjectType> List<T> query(
            QueryType queryType, QueryResponse queryResponse)
            throws EbxmlRegistryException;

    protected abstract List<String> getValidParameters();

    protected boolean matchOlderVersions = false;

    protected RegistryObjectTypeDao registryObjectDao = new RegistryObjectTypeDao();

    public void executeQuery(QueryRequest queryRequest,
            QueryResponse queryResponse) throws EbxmlRegistryException {
        // TODO: Add support for specifying query depth
        @SuppressWarnings("unused")
        int depth = queryRequest.getDepth().intValue();
        matchOlderVersions = queryRequest.isMatchOlderVersions();
        int maxResults = queryRequest.getMaxResults().intValue();
        int startIndex = queryRequest.getStartIndex().intValue();

        List<RegistryObjectType> childQueryResults = query(
                queryRequest.getQuery(), queryResponse);
        List<RegistryObjectType> queryResults = null;
        if (matchOlderVersions) {
            queryResults = childQueryResults;
        } else {
            queryResults = new ArrayList<RegistryObjectType>();
            Map<String, RegistryObjectType> maxVersionMap = new HashMap<String, RegistryObjectType>();
            String lid = null;
            int objVersion = 0;
            for (RegistryObjectType regObj : childQueryResults) {
                int version = 0;
                if (regObj.getVersionInfo() == null) {
                    queryResults.add(regObj);
                    continue;
                } else {
                    objVersion = regObj.getVersionInfo().getVersionNumber();
                }
                lid = regObj.getLid();

                RegistryObjectType maxObj = maxVersionMap.get(lid);
                if (maxObj != null) {
                    version = maxVersionMap.get(lid).getVersionInfo()
                            .getVersionNumber();
                }
                if (objVersion > version) {
                    maxVersionMap.put(lid, regObj);
                }
            }
            queryResults.addAll(maxVersionMap.values());
        }

        RETURN_TYPE returnType = getReturnType(queryRequest.getResponseOption()
                .getReturnType());
        if (queryResults == null || queryResults.isEmpty()) {
            return;
        }
        List<RegistryObjectType> results = new ArrayList<RegistryObjectType>();

        int start = 0;
        if (maxResults <= QueryManagerImpl.DEFAULT_MAX_RESULTS) {
            results = queryResults;
        } else {
            if (startIndex <= QueryManagerImpl.DEFAULT_START_INDEX) {
                start = 0;
            } else {
                statusHandler.info("Start index is set to " + startIndex);
                start = startIndex;
            }
            for (int i = start; i < start + maxResults
                    && i < queryResults.size(); i++) {
                results.add(queryResults.get(i));
            }
            statusHandler.info((queryResults.size() - results.size())
                    + " items have been discarded from the result set.");
        }
        queryResponse.setStartIndex(new BigInteger(String.valueOf(start)));

        switch (returnType) {
        case ObjectRef:
            queryResponse.setObjectRefList(EbxmlObjectUtil
                    .createObjectRefListFromObjects(results));
            break;
        case RegistryObject:
            RegistryObjectListType objList = EbxmlObjectUtil.rimObjectFactory
                    .createRegistryObjectListType();
            objList.getRegistryObject().addAll(results);
            queryResponse.setRegistryObjectList(objList);
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
        statusHandler.info("Query completed.");
        queryResponse.setTotalResultCount(new BigInteger(String.valueOf(results
                .size())));
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

}
