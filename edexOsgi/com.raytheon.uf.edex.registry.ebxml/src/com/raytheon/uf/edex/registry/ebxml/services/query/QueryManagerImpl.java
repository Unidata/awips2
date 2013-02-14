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

import oasis.names.tc.ebxml.regrep.wsdl.registry.services.v4.MsgRegistryException;
import oasis.names.tc.ebxml.regrep.wsdl.registry.services.v4.QueryManager;
import oasis.names.tc.ebxml.regrep.xsd.query.v4.QueryExceptionType;
import oasis.names.tc.ebxml.regrep.xsd.query.v4.QueryRequest;
import oasis.names.tc.ebxml.regrep.xsd.query.v4.QueryResponse;
import oasis.names.tc.ebxml.regrep.xsd.query.v4.ResponseOptionType;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.QueryType;
import oasis.names.tc.ebxml.regrep.xsd.rs.v4.UnsupportedCapabilityExceptionType;

import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.time.util.ITimer;
import com.raytheon.uf.common.time.util.TimeUtil;
import com.raytheon.uf.edex.registry.ebxml.constants.ErrorSeverity;
import com.raytheon.uf.edex.registry.ebxml.constants.RegistryResponseStatus;
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
 * 
 * </pre>
 * 
 * @author bphillip
 * @version 1.0
 */

public class QueryManagerImpl implements QueryManager {

    protected static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(QueryManagerImpl.class);

    private QueryTypeManager queryTypeMgr;

    /**
     * · ObjectRef - This option specifies that the QueryResponse MUST contain a
     * <rim:ObjectRefList> element. The purpose of this option is to return
     * references to objects rather than the actual objects.
     * 
     * · RegistryObject - This option specifies that the QueryResponse MUST
     * contain a <rim:RegistryObjectList> element containing
     * <rim:RegistryObject> elements with xsi:type=“rim:RegistryObjectType”.
     * 
     * · LeafClass - This option specifies that the QueryResponse MUST contain a
     * collection of <rim:RegistryObjectList> element containing
     * <rim:RegistryObject> elements that have an xsi:type attribute that
     * corresponds to leaf classes as defined in [regrep-xsd-v4.0]. No
     * RepositoryItems SHOULD be included for any rim:ExtrinsicObjectType
     * instance in the <rim:RegistryObjectList> element.
     * 
     * · LeafClassWithRepositoryItem - This option is the same as the LeafClass
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
        ITimer timer = TimeUtil.getTimer();
        timer.start();

        statusHandler.info("QueryManager received executeQuery Request\n"
                + queryRequest);
        QueryResponse response = EbxmlObjectUtil.queryObjectFactory
                .createQueryResponse();
        response.setStatus(RegistryResponseStatus.SUCCESS);
        QueryType queryType = queryRequest.getQuery();
        boolean federated = queryRequest.isFederated();
        // TODO: Add support for federated queries
        @SuppressWarnings("unused")
        String federation = queryRequest.getFederation();
        String format = queryRequest.getFormat();
        String lang = queryRequest.getLang();
        queryRequest.isMatchOlderVersions();

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
            response.setStatus(RegistryResponseStatus.PARTIAL_SUCCESS);
        }

        if (federated) {
            // TODO: Add support for federated queries
            response.getException()
                    .add(EbxmlExceptionUtil
                            .createRegistryException(
                                    UnsupportedCapabilityExceptionType.class,
                                    "",
                                    "Federated queries not currently supported",
                                    "This EBXML registry currently does not currently support federated queries",
                                    ErrorSeverity.WARNING, statusHandler));
            response.setStatus(RegistryResponseStatus.PARTIAL_SUCCESS);
        }

        if (lang != null) {
            // TODO: Add support for specifying the lang attribute
            response.getException()
                    .add(EbxmlExceptionUtil
                            .createRegistryException(
                                    UnsupportedCapabilityExceptionType.class,
                                    "",
                                    "lang attribute not currently supported",
                                    "This EBXML registry does not currently support the lang attribute on the QueryRequest object",
                                    ErrorSeverity.WARNING, statusHandler));
            response.setStatus(RegistryResponseStatus.PARTIAL_SUCCESS);
        }

        IRegistryQuery query = getQuery(queryType);

        try {
            query.executeQuery(queryRequest, response);
        } catch (EbxmlRegistryException e) {
            throw EbxmlExceptionUtil.createMsgRegistryException(
                    "Error executing query!", QueryExceptionType.class, "",
                    "Error executing query", e.getMessage(),
                    ErrorSeverity.ERROR, e, statusHandler);

        }
        timer.stop();
        String queryRequestId = queryRequest.getId();
        statusHandler.info("QueryManager executeQuery id [" + queryRequestId
                + "] operation completed in "
                + timer.getElapsedTime() + " ms");
        response.setRequestId(queryRequestId);
        return response;

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

}
