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
package com.raytheon.uf.edex.registry.ebxml.services.query;

import java.math.BigInteger;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.concurrent.Callable;
import java.util.concurrent.CancellationException;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.Future;

import javax.annotation.Resource;
import javax.xml.bind.JAXBException;
import javax.xml.ws.WebServiceContext;

import oasis.names.tc.ebxml.regrep.wsdl.registry.services.v4.MsgRegistryException;
import oasis.names.tc.ebxml.regrep.wsdl.registry.services.v4.QueryManager;
import oasis.names.tc.ebxml.regrep.xsd.query.v4.QueryExceptionType;
import oasis.names.tc.ebxml.regrep.xsd.query.v4.QueryRequest;
import oasis.names.tc.ebxml.regrep.xsd.query.v4.QueryResponse;
import oasis.names.tc.ebxml.regrep.xsd.query.v4.ResponseOptionType;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.FederationType;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.QueryType;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.RegistryObjectType;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.RegistryType;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.SlotType;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.StringValueType;
import oasis.names.tc.ebxml.regrep.xsd.rs.v4.RegistryExceptionType;
import oasis.names.tc.ebxml.regrep.xsd.rs.v4.RegistryResponseStatus;
import oasis.names.tc.ebxml.regrep.xsd.rs.v4.UnsupportedCapabilityExceptionType;

import org.springframework.transaction.annotation.Transactional;

import com.raytheon.uf.common.registry.constants.CanonicalQueryTypes;
import com.raytheon.uf.common.registry.constants.ErrorSeverity;
import com.raytheon.uf.common.registry.constants.Format;
import com.raytheon.uf.common.registry.constants.Languages;
import com.raytheon.uf.common.registry.constants.QueryReturnTypes;
import com.raytheon.uf.common.registry.constants.RegistryObjectTypes;
import com.raytheon.uf.common.registry.services.RegistrySOAPServices;
import com.raytheon.uf.common.serialization.SerializationUtil;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.time.util.ITimer;
import com.raytheon.uf.common.time.util.TimeUtil;
import com.raytheon.uf.edex.registry.ebxml.dao.FederationDao;
import com.raytheon.uf.edex.registry.ebxml.exception.EbxmlRegistryException;
import com.raytheon.uf.edex.registry.ebxml.services.query.types.IRegistryQuery;
import com.raytheon.uf.edex.registry.ebxml.util.EbxmlExceptionUtil;
import com.raytheon.uf.edex.registry.ebxml.util.EbxmlObjectUtil;

/**
 * The QueryManager interface allows a client to invoke queries on the server.
 * 
 * A server may support any number of pre-configured queries known as
 * Parameterized Queries, that may be invoked by clients. Parameterized queries
 * are similar in concept to stored procedures in SQL.
 * 
 * This specification defines a number of canonical queries that are standard
 * queries that MUST be supported by a server. Profiles, implementations and
 * deployments may define additional parameterized queries beyond the canonical
 * queries defined by this specification.
 * 
 * A client invokes a parameterized query supported by the server by specifying
 * its unique id as well as values for any parameters supported by the query.
 * 
 * A parameterized query MAY be stored in the server as a specialized
 * RegistryObject called QueryDefinition object which is defined by
 * [regrep-rim-v4.0]. The definition of a QueryDefinition may contain any number
 * of Parameters supported by the query.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jan 18, 2012 184        bphillip     Initial creation
 * 3/18/2013    1802       bphillip    Modified to use transaction boundaries and spring injection
 * Apr 24, 2013 1910       djohnson    RegistryResponseStatus is now an enum.
 * 
 * </pre>
 * 
 * @author bphillip
 * @version 1.0
 */
@Transactional
public class QueryManagerImpl implements QueryManager {

    /** The logger */
    protected static final IUFStatusHandler statusHandler = UFStatus
            .getHandler(QueryManagerImpl.class);

    @Resource
    private WebServiceContext wsContext;

    /**
     * Boolean denoting that the results should be eagerly fetched from the
     * database
     */
    private boolean eagerFetch = false;

    /**
     * Query type manager for getting query objects based on values provided in
     * a submitted query
     */
    private QueryTypeManager queryTypeMgr;

    /**
     * Data access object for interacting with FederationType objects. Used
     * during processing of federated queries.
     */
    private FederationDao federationDao;

    /**
     * Executor service used to submit queries to federation members in parallel
     * during the processing of a federated query
     */
    private ExecutorService queryExecutor;

    /**
     * ObjectRef - This option specifies that the QueryResponse MUST contain a
     * <rim:ObjectRefList> element. The purpose of this option is to return
     * references to objects rather than the actual objects.
     * 
     * RegistryObject - This option specifies that the QueryResponse MUST
     * contain a <rim:RegistryObjectList> element containing
     * <rim:RegistryObject> elements with xsi:type=rim:RegistryObjectType.
     * 
     * LeafClass - This option specifies that the QueryResponse MUST contain a
     * collection of <rim:RegistryObjectList> element containing
     * <rim:RegistryObject> elements that have an xsi:type attribute that
     * corresponds to leaf classes as defined in [regrep-xsd-v4.0]. No
     * RepositoryItems SHOULD be included for any rim:ExtrinsicObjectType
     * instance in the <rim:RegistryObjectList> element.
     * 
     * LeafClassWithRepositoryItem - This option is the same as the LeafClass
     * option with the additional requirement that the response include the
     * RepositoryItems, if any, for every rim:ExtrinsicObjectType instance in
     * the <rim:RegistryObjectList> element.
     */
    public enum RETURN_TYPE {
        ObjectRef, RegistryObject, LeafClass, LeafClassWithRepositoryItem
    }

    /** The default format for query responses */
    private static final String DEFAULT_RESPONSE_FORMAT = "application/ebrim+xml";

    public static final int DEFAULT_MAX_RESULTS = -1;

    public static final int DEFAULT_START_INDEX = 0;

    public static final int DEFAULT_DEPTH = 0;

    public static final int TRANSITIVE_CLOSURE_DEPTH = -1;

    public static final boolean DEFAULT_MATCH_OLDER_VERSIONS = false;

    /** The default return type */
    public static final RETURN_TYPE DEFAULT_RETURN_TYPE = RETURN_TYPE.LeafClassWithRepositoryItem;

    /**
     * Creates a new QueryManagerImpl
     */
    public QueryManagerImpl() {
        queryExecutor = Executors.newCachedThreadPool();
    }

    /**
     * A client invokes a parameterized query using the Query protocol defined
     * by the executeQuery operation of the QueryManager interface.
     * 
     * A client initiates the Query protocol by sending a QueryRequest message
     * to the QueryManager endpoint.
     * 
     * The QueryManager sends a QueryResponse back to the client as response.
     * The QueryResponse contains a set of objects that match the query.
     */
    @Override
    public QueryResponse executeQuery(QueryRequest queryRequest)
            throws MsgRegistryException {
        String client = EbxmlObjectUtil.getClientHost(wsContext);
        ITimer timer = TimeUtil.getTimer();
        timer.start();
        statusHandler.info("QueryManager received executeQuery Request "
                + queryRequest.getId() + " from [" + client + "]");
        QueryResponse response = EbxmlObjectUtil.queryObjectFactory
                .createQueryResponse();
        response.setStatus(RegistryResponseStatus.SUCCESS);
        String format = queryRequest.getFormat();
        String lang = queryRequest.getLang();

        if (!format.equals(DEFAULT_RESPONSE_FORMAT)) {
            // TODO: Implement support for different response formats
            response.getException()
                    .add(EbxmlExceptionUtil
                            .createRegistryException(
                                    UnsupportedCapabilityExceptionType.class,
                                    "",
                                    "Non default response formats not currently supported",
                                    "This EBXML registry currently does not currently support queries specifying response types other than "
                                            + DEFAULT_RESPONSE_FORMAT,
                                    ErrorSeverity.WARNING, statusHandler));
        }

        if (lang != null) {
            // TODO: Add support for specifying the lang attribute
            statusHandler.warn("Lang attribute currently not supported.");
        }

        try {
            if (queryRequest.isFederated()) {
                if (federationDao.federationsExist()) {
                    executeFederatedQuery(queryRequest, response, client);
                } else {
                    statusHandler
                            .warn("Federated query request received but no federations are known to this registry. Executing query locally.");
                    executeLocalQuery(queryRequest, response, client);
                }
            } else {
                executeLocalQuery(queryRequest, response, client);
            }
        } catch (EbxmlRegistryException e) {
            throw EbxmlExceptionUtil.createMsgRegistryException(
                    "Error executing query!", QueryExceptionType.class, "",
                    "Error executing query", e.getMessage(),
                    ErrorSeverity.ERROR, e, statusHandler);

        }
        timer.stop();
        String queryRequestId = queryRequest.getId();
        statusHandler.info("QueryManager executeQuery id [" + queryRequestId
                + "] operation completed in " + timer.getElapsedTime() + " ms");
        response.setRequestId(queryRequestId);
        return response;

    }

    /**
     * Executes a query on the local registry
     * 
     * @param queryRequest
     *            The query request object
     * @param queryResponse
     *            The query response object
     * @param client
     *            The address of the client that made the request
     * @throws MsgRegistryException
     *             If errors occur when resolving the query type
     * @throws EbxmlRegistryException
     *             If errors occur while executing the actual query
     */
    private void executeLocalQuery(QueryRequest queryRequest,
            QueryResponse queryResponse, String client)
            throws MsgRegistryException, EbxmlRegistryException {
        IRegistryQuery query = getQuery(queryRequest.getQuery());
        query.executeQuery(queryRequest, queryResponse, client);
        if (eagerFetch) {
            try {
                SerializationUtil.getJaxbManager().marshalToXml(queryResponse);
            } catch (JAXBException e) {
                throw new EbxmlRegistryException(
                        "Error eagerly fetching items", e);
            }
        }
    }

    /**
     * Executes a federated query
     * 
     * @param queryRequest
     *            The query request object
     * @param queryResponse
     *            The query response object
     * @param client
     *            The address of the client that made the request
     * @throws MsgRegistryException
     *             If errors occur when resolving the query type
     * @throws EbxmlRegistryException
     *             If errors occur while executing the actual query
     */
    private void executeFederatedQuery(QueryRequest queryRequest,
            QueryResponse queryResponse, String client)
            throws EbxmlRegistryException, MsgRegistryException {

        /*
         * If somehow, we get into this method with a non-federated query,
         * execute locally
         */
        if (!queryRequest.isFederated()) {
            statusHandler
                    .warn("Non-federated query passed to federated query executor. Executing as a local query");
            executeLocalQuery(queryRequest, queryResponse, client);
            return;
        }

        /*
         * Get the set of registries to query
         */
        statusHandler.info("Getting list of registries for federated query...");
        Set<RegistryType> registriesToQuery = new HashSet<RegistryType>();
        String federationToQuery = queryRequest.getFederation();
        if (federationToQuery == null || federationToQuery.isEmpty()) {
            List<FederationType> federations = federationDao.getAll();
            for (FederationType federation : federations) {
                registriesToQuery.addAll(getMembersOfFederation(federation
                        .getId()));
            }
        } else {
            registriesToQuery.addAll(getMembersOfFederation(federationToQuery));
        }

        /*
         * If no registries found to query, execute as local query
         */
        if (registriesToQuery.isEmpty()) {
            statusHandler
                    .warn("Federated query found no registries to query! Executing as local query.");
            executeLocalQuery(queryRequest, queryResponse, client);
        } else {
            statusHandler.info("Submitting federated query to "
                    + registriesToQuery.size() + " remote registries.");

            /*
             * Create a list of queries to execute concurrently
             */
            List<RemoteRegistryQuery> queryRunners = new ArrayList<RemoteRegistryQuery>();
            for (RegistryType registryToQuery : registriesToQuery) {
                queryRunners.add(new RemoteRegistryQuery(registryToQuery,
                        queryRequest));
            }

            try {
                List<Future<QueryResponse>> responses = queryExecutor
                        .invokeAll(queryRunners);

                for (Future<QueryResponse> response : responses) {
                    if (response.isCancelled()) {
                        statusHandler
                                .error("Federated query invocation was cancelled!");
                    } else {
                        try {
                            consolidateQueryResponse(queryResponse,
                                    response.get());
                        } catch (CancellationException e) {
                            statusHandler.error(
                                    "Federated query invocation was cancelled",
                                    e);
                            queryResponse.getException().add(getException(e));
                        } catch (ExecutionException e) {
                            statusHandler
                                    .error("Federated query invocation encountered an erroring execution!",
                                            e);
                            queryResponse.getException().add(getException(e));
                        } catch (InterruptedException e) {
                            statusHandler
                                    .error("Federated query invocation was interrupted",
                                            e);
                            queryResponse.getException().add(getException(e));
                        }
                    }
                }
                if (queryResponse.getException().isEmpty()) {
                    queryResponse.setStatus(RegistryResponseStatus.SUCCESS);
                } else if (queryResponse.getException().size() == responses
                        .size()) {
                    throw new EbxmlRegistryException(
                            "All "
                                    + responses.size()
                                    + " query requests generated by the federated query failed!");
                } else {
                    statusHandler
                            .warn("Federated query was partial successful. "
                                    + queryResponse.getException().size()
                                    + " of " + responses.size() + " failed.");
                    queryResponse
                            .setStatus(RegistryResponseStatus.PARTIAL_SUCCESS);
                }
            } catch (InterruptedException e) {
                throw new EbxmlRegistryException(
                        "Error executing federated query!", e);
            }
        }
    }

    /**
     * Convenience method to wrap an exception in a RegistryExceptionType
     * 
     * @param e
     *            The exception to wrap
     * @return The RegistryExceptionType wrapper
     */
    private RegistryExceptionType getException(Exception e) {
        return EbxmlExceptionUtil.createRegistryException(
                QueryExceptionType.class, "", e.getLocalizedMessage(),
                e.getLocalizedMessage(), ErrorSeverity.ERROR, e, statusHandler);
    }

    /**
     * Convenience method for consolidating two query responses together. Used
     * for generating a single query response from a federated query that
     * queries many registries
     * 
     * @param response1
     *            The response to append to
     * @param response2
     *            The response that will be appended to response1
     */
    private void consolidateQueryResponse(QueryResponse response1,
            QueryResponse response2) {
        response1.addRegistryObjects(response2.getRegistryObjects());
        response1.addObjectRefs(response2.getObjectRefs());
        response1.getException().addAll(response2.getException());
        response1.incrementResultCount(response2.getTotalResultCount());

    }

    /**
     * Gets the RegistryType members of the given federation. If a federation is
     * found to be a member of the given federation, this method is called
     * recursively to get the set of actual registries.
     * 
     * @param federationId
     *            The id of the federation to get the members for
     * @return The RegistryType members of the federation
     * @throws MsgRegistryException
     *             If errors occur while querying for federation members.
     */
    private Set<RegistryType> getMembersOfFederation(String federationId)
            throws MsgRegistryException {
        Set<RegistryType> retVal = new HashSet<RegistryType>();
        statusHandler.info("Getting federation members for " + federationId
                + "...");
        SlotType associationTypeSlot = new SlotType(
                QueryConstants.ASSOCIATION_TYPE, new StringValueType(
                        RegistryObjectTypes.ASSOCIATION_PATH));
        SlotType sourceObjectIdSlot = new SlotType(
                QueryConstants.SOURCE_OBJECT_ID, new StringValueType(
                        federationId));
        QueryType query = new QueryType(
                CanonicalQueryTypes.FIND_ASSOCIATED_OBJECTS,
                associationTypeSlot, sourceObjectIdSlot);
        QueryRequest queryRequest = new QueryRequest(
                "Query for federation members",
                "Query to get the members of the federation",
                new ResponseOptionType(QueryReturnTypes.REGISTRY_OBJECT, true),
                query, false, null, Format.EBRIM, Languages.EN_US, 0, 0, 0,
                false);
        QueryResponse response = executeQuery(queryRequest);
        if (response.getRegistryObjectList() != null) {
            List<RegistryObjectType> responseObjects = response
                    .getRegistryObjectList().getRegistryObject();
            for (RegistryObjectType regObj : responseObjects) {
                if (regObj instanceof RegistryType) {
                    retVal.add((RegistryType) regObj);
                } else if (regObj instanceof FederationType) {
                    retVal.addAll(getMembersOfFederation(regObj.getId()));
                }
            }
        }
        return retVal;
    }

    /**
     * 
     * Callable class to run a query on a remote registry
     * 
     * <pre>
     * 
     * SOFTWARE HISTORY
     * 
     * Date         Ticket#     Engineer    Description
     * ------------ ----------  ----------- --------------------------
     * 5/24/2013    2036        bphillip    Initial implementation
     * </pre>
     * 
     * @author bphillip
     * @version 1
     */
    private class RemoteRegistryQuery implements Callable<QueryResponse> {

        /** The registry to query */
        private RegistryType registryToQuery;

        /** The request to submit to the remote registry */
        private QueryRequest queryRequest;

        /**
         * Creates a new RemoteRegistryQuery.
         * 
         * @param registryToQuery
         *            The registry to query
         * @param queryRequest
         *            The request to submit to the remote registry
         */
        public RemoteRegistryQuery(RegistryType registryToQuery,
                QueryRequest queryRequest) {
            this.registryToQuery = registryToQuery;
            this.queryRequest = queryRequest;
            this.queryRequest.setFederated(false);
            this.queryRequest.setFederation(null);

        }

        @Override
        public QueryResponse call() throws Exception {
            statusHandler.info("Submitting federated query to ["
                    + registryToQuery.getId() + "] at ["
                    + registryToQuery.getBaseURL() + "]...");
            return RegistrySOAPServices.getQueryServiceForHost(
                    registryToQuery.getBaseURL()).executeQuery(queryRequest);

        }
    }

    /**
     * Executes a query with the give parameters. This method is not exposed as
     * a service and is only intended to be used internally
     * 
     * @param response
     * @param responseOption
     * @param queryType
     * @param depth
     * @param matchOlderVersions
     * @param maxResults
     * @param startIndex
     * @return The query response
     * @throws MsgRegistryException
     *             If the query fails
     */
    public QueryResponse executeQuery(ResponseOptionType responseOption,
            QueryType queryType, int depth, boolean matchOlderVersions,
            int maxResults, int startIndex) throws MsgRegistryException {
        statusHandler
                .info("Received internal request for query using specified values");
        QueryRequest queryRequest = EbxmlObjectUtil.queryObjectFactory
                .createQueryRequest();
        queryRequest.setResponseOption(responseOption);
        queryRequest.setQuery(queryType);
        queryRequest.setDepth(new BigInteger(String.valueOf(depth)));
        queryRequest.setMatchOlderVersions(matchOlderVersions);
        queryRequest.setMaxResults(new BigInteger(String.valueOf(maxResults)));
        queryRequest.setStartIndex(new BigInteger(String.valueOf(startIndex)));
        QueryResponse queryResponse = executeQuery(queryRequest);
        return queryResponse;
    }

    public QueryResponse executeQuery(ResponseOptionType responseOption,
            QueryType queryType) throws MsgRegistryException {
        statusHandler
                .info("Received internal request for query using default values");
        QueryRequest queryRequest = EbxmlObjectUtil.queryObjectFactory
                .createQueryRequest();
        queryRequest.setResponseOption(responseOption);
        queryRequest.setQuery(queryType);
        queryRequest.setDepth(new BigInteger(String.valueOf(DEFAULT_DEPTH)));
        queryRequest.setMatchOlderVersions(DEFAULT_MATCH_OLDER_VERSIONS);
        queryRequest.setMaxResults(new BigInteger(String
                .valueOf(DEFAULT_MAX_RESULTS)));
        queryRequest.setStartIndex(new BigInteger(String
                .valueOf(DEFAULT_START_INDEX)));
        QueryResponse queryResponse = executeQuery(queryRequest);
        return queryResponse;
    }

    /**
     * Gets the query type implementation
     * 
     * @param queryType
     *            The query type to retrieve
     * @return The implementation of the given query type
     * @throws MsgRegistryException
     *             If the given query type does not have an implementation
     */
    private IRegistryQuery getQuery(QueryType queryType)
            throws MsgRegistryException {
        String queryDefinition = queryType.getQueryDefinition();

        IRegistryQuery query = queryTypeMgr.getQueryType(queryDefinition);
        if (query == null) {
            throw EbxmlExceptionUtil.createMsgRegistryException(
                    "Query Type Not Supported",
                    UnsupportedCapabilityExceptionType.class, "",
                    "Unsupported query type",
                    "The query type [" + queryType.getQueryDefinition()
                            + "] is not registered as a vaild query type",
                    ErrorSeverity.ERROR, statusHandler);
        }
        return query;
    }

    public void setQueryTypeMgr(QueryTypeManager queryTypeMgr) {
        this.queryTypeMgr = queryTypeMgr;
    }

    public void setEagerFetch(boolean eagerFetch) {
        this.eagerFetch = eagerFetch;
    }

    public void setFederationDao(FederationDao federationDao) {
        this.federationDao = federationDao;
    }

}
