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

import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.concurrent.Callable;
import java.util.concurrent.CancellationException;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.Future;

import javax.annotation.Resource;
import javax.xml.ws.WebServiceContext;

import oasis.names.tc.ebxml.regrep.wsdl.registry.services.v4.MsgRegistryException;
import oasis.names.tc.ebxml.regrep.wsdl.registry.services.v4.QueryManager;
import oasis.names.tc.ebxml.regrep.xsd.query.v4.QueryRequest;
import oasis.names.tc.ebxml.regrep.xsd.query.v4.QueryResponse;
import oasis.names.tc.ebxml.regrep.xsd.query.v4.ResponseOptionType;
import oasis.names.tc.ebxml.regrep.xsd.query.v4.ResponseOptionType.RETURN_TYPE;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.ExtensibleObjectType;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.ExtrinsicObjectType;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.FederationType;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.InternationalStringType;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.LocalizedStringType;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.ObjectRefListType;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.ObjectRefType;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.ParameterType;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.QueryDefinitionType;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.QueryExpressionType;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.QueryType;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.RegistryObjectType;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.RegistryType;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.SlotType;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.StringQueryExpressionType;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.StringValueType;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.XMLQueryExpressionType;
import oasis.names.tc.ebxml.regrep.xsd.rs.v4.RegistryResponseStatus;

import org.hibernate.SessionFactory;
import org.springframework.beans.BeansException;
import org.springframework.context.ApplicationContext;
import org.springframework.context.ApplicationContextAware;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

import com.raytheon.uf.common.registry.constants.CanonicalQueryTypes;
import com.raytheon.uf.common.registry.constants.QueryReturnTypes;
import com.raytheon.uf.common.registry.constants.RegistryObjectTypes;
import com.raytheon.uf.common.registry.services.RegistrySOAPServices;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.time.util.ITimer;
import com.raytheon.uf.common.time.util.TimeUtil;
import com.raytheon.uf.common.util.CollectionUtil;
import com.raytheon.uf.edex.registry.ebxml.dao.ClassificationNodeDao;
import com.raytheon.uf.edex.registry.ebxml.dao.FederationDao;
import com.raytheon.uf.edex.registry.ebxml.dao.QueryDefinitionDao;
import com.raytheon.uf.edex.registry.ebxml.dao.RegistryObjectDao;
import com.raytheon.uf.edex.registry.ebxml.exception.EbxmlRegistryException;
import com.raytheon.uf.edex.registry.ebxml.services.lifecycle.ObjectReferenceResolver;
import com.raytheon.uf.edex.registry.ebxml.services.query.plugins.RegistryQueryPlugin;
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
 * Jun 24, 2013 2106       djohnson    Transaction must already be open.
 * 9/5/2013     1538       bphillip    Removed log message
 * 10/8/2013    1682       bphillip    Refactored querying
 * 10/2013      1682       bphillip    Fixed federated query invocation
 * 10/23/2013   1538       bphillip    Remove extra executeQuery method
 * 10/30/2013   1538       bphillip    Changed to use non-static soap service client
 * 2/19/2014    2769        bphillip   Moved Transactional Annotation, fixed plugin cache usage
 * 
 * </pre>
 * 
 * @author bphillip
 * @version 1.0
 */
public class QueryManagerImpl implements QueryManager, ApplicationContextAware {

    /** The logger */
    protected static final IUFStatusHandler statusHandler = UFStatus
            .getHandler(QueryManagerImpl.class);

    /** Standard query error message */
    private static final String QUERY_ERROR_MSG = "Error executing query";

    /** List of EBXML leaf classes */
    private static List<Class<?>> LEAF_CLASSES;

    /** Default sorter for sorting query results */
    private static final Comparator<RegistryObjectType> RESULT_SORTER = new Comparator<RegistryObjectType>() {
        @Override
        public int compare(RegistryObjectType o1, RegistryObjectType o2) {
            return o1.getId().compareTo(o2.getId());
        }
    };

    private Map<String, RegistryQueryPlugin> queryPlugins = new HashMap<String, RegistryQueryPlugin>();

    /** The web service context if this class was called via a web service */
    @Resource
    private WebServiceContext wsContext;

    /**
     * The Spring session factory
     */
    private SessionFactory sessionFactory;

    /**
     * Data access object for interacting with FederationType objects. Used
     * during processing of federated queries.
     */
    private FederationDao federationDao;

    /**
     * Data access object for interacting with Query Definitions
     */
    private QueryDefinitionDao queryDefinitionDao;

    /**
     * Data access object for getting classification nodes
     */
    private ClassificationNodeDao classificationNodeDao;

    /**
     * Data access object for getting registry objects
     */
    private RegistryObjectDao registryObjectDao;

    /**
     * Executor service used to submit queries to federation members in parallel
     * during the processing of a federated query
     */
    private final ExecutorService queryExecutor;

    /**
     * Utility object used to resolve registry object references
     */
    private ObjectReferenceResolver referenceResolver;

    private RegistrySOAPServices registrySoapClient;

    /**
     * Creates a new QueryManagerImpl
     */
    public QueryManagerImpl() {
        queryExecutor = Executors.newCachedThreadPool();

    }

    @Override
    public void setApplicationContext(ApplicationContext applicationContext)
            throws BeansException {
        for (RegistryQueryPlugin plugin : applicationContext.getBeansOfType(
                RegistryQueryPlugin.class).values()) {
            queryPlugins.put(plugin.getQueryDefinition(), plugin);
        }
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
    @Transactional(propagation = Propagation.MANDATORY, readOnly = true)
    public QueryResponse executeQuery(QueryRequest queryRequest)
            throws MsgRegistryException {
        String client = EbxmlObjectUtil.getClientHost(wsContext);
        ITimer timer = TimeUtil.getTimer();
        timer.start();
        statusHandler.info("QueryManager received executeQuery Request "
                + queryRequest.getId() + " from [" + client + "]");
        QueryResponse response = new QueryResponse();
        response.setStatus(RegistryResponseStatus.SUCCESS);
        ResponseOptionType responseOption = queryRequest.getResponseOption();
        int depth = queryRequest.getDepth().intValue();
        String format = queryRequest.getFormat();
        String lang = queryRequest.getLang();
        boolean matchOlderVersions = queryRequest.isMatchOlderVersions();
        int maxResults = queryRequest.getMaxResults().intValue();
        int startIndex = queryRequest.getStartIndex().intValue();

        /*
         * We are only supporting the default response format. Support for
         * others may be added later.
         */
        if (!format.equals(QueryRequest.DEFAULT_RESPONSE_FORMAT)) {
            throw EbxmlExceptionUtil.createUnsupportedCapabilityExceptionType(
                    QUERY_ERROR_MSG,
                    "This registry does not support the specified response format of ["
                            + format + "]. Accepted response formats are ["
                            + QueryRequest.DEFAULT_RESPONSE_FORMAT + "]");
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
                    QUERY_ERROR_MSG, e);
        }

        List<RegistryObjectType> result = response.getRegistryObjects();
        processDepth(result, depth);
        // Sort the result so iterative queries are consistent
        CollectionUtil.removeNulls(result);
        Collections.sort(result, RESULT_SORTER);

        // If we are not to match older versions, the results must be filtered
        // to contain only the latest version of each object
        if (!matchOlderVersions) {
            Map<String, RegistryObjectType> versionMap = new HashMap<String, RegistryObjectType>(
                    result.size(), 1);
            for (RegistryObjectType obj : result) {
                RegistryObjectType existingObject = versionMap
                        .get(obj.getLid());
                if (existingObject == null
                        || obj.getVersionInfo().greaterThan(
                                existingObject.getVersionInfo())) {
                    versionMap.put(obj.getLid(), obj);
                }
            }
            result = new ArrayList<RegistryObjectType>(versionMap.values());
        }

        switch (responseOption.getReturnTypeEnum()) {
        case LeafClassWithRepositoryItem:
        case LeafClass:
            List<RegistryObjectType> leafObjects = new ArrayList<RegistryObjectType>();
            for (RegistryObjectType obj : result) {
                if (isRimLeafClass(obj.getClass())) {
                    if (responseOption.getReturnTypeEnum().equals(
                            RETURN_TYPE.LeafClass)
                            && (obj instanceof ExtrinsicObjectType)) {
                        ExtrinsicObjectType extObj = (ExtrinsicObjectType) obj;
                        extObj.setRepositoryItem(null);
                        extObj.setRepositoryItemRef(null);

                    }
                    leafObjects.add(obj);
                }
            }
            response.getRegistryObjectList().setRegistryObject(
                    trimResult(leafObjects, startIndex, maxResults));

            break;
        case RegistryObject:
            response.getRegistryObjectList().setRegistryObject(
                    trimResult(result, startIndex, maxResults));
            break;
        case ObjectRef:
            response.setRegistryObjectList(null);
            response.setObjectRefList(new ObjectRefListType());
            for (RegistryObjectType obj : result) {
                response.getObjectRefList().getObjectRef()
                        .add(new ObjectRefType(obj.getId()));
            }
            response.getObjectRefList()
                    .setObjectRef(
                            trimResult(response.getObjectRefs(), startIndex,
                                    maxResults));
            break;
        }

        /*
         * Process the language constraint
         */
        if (!responseOption.getReturnTypeEnum().equals(RETURN_TYPE.ObjectRef)
                && lang != null && !lang.isEmpty()) {
            for (RegistryObjectType obj : response.getRegistryObjects()) {
                filterLanguage(obj.getName(), lang);
                filterLanguage(obj.getDescription(), lang);
            }
        }

        timer.stop();
        String queryRequestId = queryRequest.getId();
        statusHandler.info("QueryManager executeQuery id [" + queryRequestId
                + "] operation completed in " + timer.getElapsedTime() + " ms");
        response.setRequestId(queryRequestId);
        return response;

    }

    /**
     * Initializes the static leaf classes
     * 
     * @return The leaf class list
     */
    private synchronized List<Class<?>> getLeafClasses() {
        if (CollectionUtil.isNullOrEmpty(LEAF_CLASSES)) {
            LEAF_CLASSES = new ArrayList<Class<?>>();
            List<Class<?>> leafClasses = new ArrayList<Class<?>>();
            for (Object obj : sessionFactory.getAllClassMetadata().keySet()) {
                try {
                    Class<?> clazz = Class.forName((String) obj);
                    if (clazz.getName().startsWith(
                            "oasis.names.tc.ebxml.regrep.xsd.rim.v4")) {
                        leafClasses.add(clazz);
                    }
                } catch (ClassNotFoundException e) {
                    statusHandler.error(
                            "Error initializing RegRep database. Class not found: "
                                    + obj, e);
                }
            }
            LEAF_CLASSES.addAll(leafClasses);
            for (Class<?> clazz : leafClasses) {
                if (ExtensibleObjectType.class.isAssignableFrom(clazz)) {
                    Class<?> currentClass = clazz;
                    while (!currentClass.equals(Object.class)) {
                        currentClass = currentClass.getSuperclass();
                        if (currentClass != null) {
                            LEAF_CLASSES.remove(currentClass);
                        }
                    }
                } else {
                    LEAF_CLASSES.remove(clazz);
                }
            }
        }
        return LEAF_CLASSES;
    }

    /**
     * Checks if the given class is a EBXML leaf class as defined by the EBXML
     * 4.0 spec
     * 
     * @param clazzToCheck
     *            The class to check
     * @return True if the given class is a leaf class
     */
    public boolean isRimLeafClass(Class<?> clazzToCheck) {
        return getLeafClasses().contains(clazzToCheck);
    }

    /**
     * Filters the query to only include strings of a given language
     * 
     * @param intlStr
     *            The International string to filter
     * @param language
     *            The language to filter on
     */
    private void filterLanguage(InternationalStringType intlStr, String language) {
        if (intlStr != null) {
            LocalizedStringType str = null;
            for (LocalizedStringType localStr : intlStr.getLocalizedString()) {
                if (localStr.getLang() != null
                        && localStr.getLang().equals(language)) {
                    str = localStr;
                    break;
                }
            }
            if (str != null) {
                intlStr.getLocalizedString().clear();
                intlStr.getLocalizedString().add(str);
            }
        }
    }

    /**
     * Processes the depth attribute
     * 
     * @param result
     *            The result to process
     * @param depth
     *            The depth used for resolving references
     * @throws MsgRegistryException
     *             If errors occur while getting the referenced objects
     */
    private void processDepth(List<RegistryObjectType> result, int depth)
            throws MsgRegistryException {
        /*
         * A depth of 0 indicates that the server MUST return only those objects
         * that match the query.
         */
        if (depth == 0) {
            return;
        }

        Set<RegistryObjectType> objsToCheck = new HashSet<RegistryObjectType>(
                result);
        if (depth == -1) {
            boolean transitiveClosure = false;
            while (!transitiveClosure) {
                Set<RegistryObjectType> referencedObjects = referenceResolver
                        .getReferencedObjects(objsToCheck);
                if (result.containsAll(referencedObjects)) {
                    transitiveClosure = true;
                }
                result.addAll(referencedObjects);
            }

        } else {
            for (int i = 0; i < depth; i++) {
                Set<RegistryObjectType> referencedObjects = referenceResolver
                        .getReferencedObjects(objsToCheck);
                result.addAll(referencedObjects);
                objsToCheck = referencedObjects;
            }
        }

    }

    /**
     * Trims the result set based on the max results and start index of the
     * query
     * 
     * @param result
     *            The result to trim
     * @param startIndex
     *            The start index for the results
     * @param maxResults
     *            The maximum number of results to be returned
     * @return The trimmed result list
     */
    private <T extends Object> List<T> trimResult(List<T> result,
            int startIndex, int maxResults) {
        if (maxResults == QueryRequest.DEFAULT_MAX_RESULTS.intValue()
                && startIndex == QueryRequest.DEFAULT_START_INDEX.intValue()) {
            return result;
        }
        if (maxResults == QueryRequest.DEFAULT_MAX_RESULTS.intValue()) {
            return CollectionUtil.subList(result, startIndex, result.size()
                    - startIndex);
        }
        return CollectionUtil.subList(result, startIndex, maxResults);
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
        QueryType query = queryRequest.getQuery();
        String queryDefinition = query.getQueryDefinition();
        RegistryQueryPlugin plugin = queryPlugins.get(queryDefinition);
        if (plugin == null) {
            QueryDefinitionType queryDef = queryDefinitionDao.loadById(query
                    .getQueryDefinition());
            if (queryDef == null) {
                throw EbxmlExceptionUtil.createQueryExceptionType(
                        "Query not found [" + queryDef + "]", "The query ["
                                + queryDef
                                + "] is not defined in this registry");
            }

            QueryExpressionType queryExpression = queryDef.getQueryExpression();
            String queryLanguage = queryExpression.getQueryLanguage();

            if (queryExpression instanceof XMLQueryExpressionType) {
                throw EbxmlExceptionUtil
                        .createQueryExceptionType(
                                "Unsupported Query Expression Type",
                                "This registry does not support quries using the XMLQueryExpression type. Only StringExpressionType expressions are valid");
            }
            if (classificationNodeDao.getById(queryLanguage) == null) {
                throw EbxmlExceptionUtil.createQueryExceptionType(
                        "Unsupported query language [" + queryLanguage + "]",
                        "This registry does not support the query language ["
                                + queryLanguage + "]");
            }
            checkQueryParameters(query);
            String queryString = ((StringQueryExpressionType) queryExpression)
                    .getValue();
            Object[] queryParameters = RegistryQueryUtil.getQueryParameters(
                    queryDef, query);
            List<RegistryObjectType> results = registryObjectDao
                    .executeHQLQuery(queryString, queryParameters);
            queryResponse.addRegistryObjects(results);
        } else {
            checkQueryParameters(queryRequest.getQuery());
            QueryResponse response = plugin.executeQuery(queryRequest);
            consolidateQueryResponse(queryResponse, response);
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
                            queryResponse.getException().add(
                                    EbxmlExceptionUtil
                                            .createMsgRegistryException(
                                                    QUERY_ERROR_MSG, e)
                                            .getFaultInfo());
                        } catch (ExecutionException e) {
                            statusHandler
                                    .error("Federated query invocation encountered an erroring execution!",
                                            e);
                            queryResponse.getException().add(
                                    EbxmlExceptionUtil
                                            .createMsgRegistryException(
                                                    QUERY_ERROR_MSG, e)
                                            .getFaultInfo());
                        } catch (InterruptedException e) {
                            statusHandler
                                    .error("Federated query invocation was interrupted",
                                            e);
                            queryResponse.getException().add(
                                    EbxmlExceptionUtil
                                            .createMsgRegistryException(
                                                    QUERY_ERROR_MSG, e)
                                            .getFaultInfo());
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
        QueryRequest request = new QueryRequest();
        request.setResponseOption(new ResponseOptionType(
                QueryReturnTypes.REGISTRY_OBJECT, true));
        request.setId("Get Members of Federation Query");
        request.setQuery(query);
        QueryResponse response = executeQuery(request);
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
        private final RegistryType registryToQuery;

        /** The request to submit to the remote registry */
        private final QueryRequest queryRequest;

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
            return registrySoapClient.getQueryServiceForHost(
                    registryToQuery.getBaseURL()).executeQuery(queryRequest);

        }
    }

    /**
     * Checks to ensure that the specified query contains all the required
     * parameters
     * 
     * @param query
     *            The query to check
     * @throws MsgRegistryException
     *             If the query parameters are invalid or missing
     */
    private void checkQueryParameters(QueryType query)
            throws MsgRegistryException {
        List<ParameterType> parameters = queryDefinitionDao
                .getParametersForQuery(query.getQueryDefinition());
        for (ParameterType param : parameters) {
            if (param.getMinOccurs() != null) {
                int minOccurs = param.getMinOccurs().intValue();
                List<Object> values = query.getSlotValueAsList(param
                        .getParameterName());
                if (values.size() < minOccurs
                        || (param.getMaxOccurs() != null && values.size() > param
                                .getMaxOccurs().intValue())) {
                    throw EbxmlExceptionUtil.createMsgRegistryException(
                            "Invalid parameters passed to query!",
                            new EbxmlRegistryException(
                                    "Invalid parameter count detected for query ["
                                            + query.getQueryDefinition()
                                            + "] for parameter ["
                                            + param.getParameterName() + "]"));
                }
            }
        }
    }

    public void setFederationDao(FederationDao federationDao) {
        this.federationDao = federationDao;
    }

    public void setReferenceResolver(ObjectReferenceResolver referenceResolver) {
        this.referenceResolver = referenceResolver;
    }

    public void setQueryDefinitionDao(QueryDefinitionDao queryDefinitionDao) {
        this.queryDefinitionDao = queryDefinitionDao;
    }

    public void setClassificationNodeDao(
            ClassificationNodeDao classificationNodeDao) {
        this.classificationNodeDao = classificationNodeDao;
    }

    public void setRegistryObjectDao(RegistryObjectDao registryObjectDao) {
        this.registryObjectDao = registryObjectDao;
    }

    public void setSessionFactory(SessionFactory sessionFactory) {
        this.sessionFactory = sessionFactory;
    }

    public void setRegistrySoapClient(RegistrySOAPServices registrySoapClient) {
        this.registrySoapClient = registrySoapClient;
    }

}
