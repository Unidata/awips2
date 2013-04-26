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
package com.raytheon.uf.edex.registry.ebxml.services.rest;

import java.math.BigInteger;
import java.util.ArrayList;
import java.util.List;

import javax.ws.rs.GET;
import javax.ws.rs.Path;
import javax.ws.rs.Produces;
import javax.ws.rs.core.Context;
import javax.ws.rs.core.MultivaluedMap;
import javax.ws.rs.core.UriInfo;
import javax.xml.bind.JAXBException;

import oasis.names.tc.ebxml.regrep.wsdl.registry.services.v4.MsgRegistryException;
import oasis.names.tc.ebxml.regrep.wsdl.registry.services.v4.QueryManager;
import oasis.names.tc.ebxml.regrep.xsd.query.v4.QueryRequest;
import oasis.names.tc.ebxml.regrep.xsd.query.v4.QueryResponse;
import oasis.names.tc.ebxml.regrep.xsd.query.v4.ResponseOptionType;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.QueryType;

import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import com.raytheon.uf.common.registry.constants.Languages;
import com.raytheon.uf.common.registry.constants.QueryReturnTypes;
import com.raytheon.uf.common.serialization.JAXBManager;

/**
 * 
 * REST service implementation for the Query Protocol according to
 * specifications in Section 12.2 of the ebRS specification
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#     Engineer    Description
 * ------------ ----------  ----------- --------------------------
 * 4/19/2013    1931        bphillip    Initial implementation
 * </pre>
 * 
 * @author bphillip
 * @version 1
 */
@Path("/rest/search")
@Service
@Transactional
public class QueryProtocolRestService {

    /** The queryID canonical query parameter name */
    private static final String QUERY_ID = "queryId";

    /** The depth canonical query parameter name */
    private static final String DEPTH = "depth";

    /** The format canonical query parameter name */
    private static final String FORMAT = "format";

    /** The federated canonical query parameter name */
    private static final String FEDERATED = "federated";

    /** The federation canonical query parameter name */
    private static final String FEDERATION = "federation";

    /** The matchOlderVersion canonical query parameter name */
    private static final String MATCH_OLDER_VERSIONS = "matchOlderVersions";

    /** The startIndex canonical query parameter name */
    private static final String START_INDEX = "startIndex";

    /** The lang canonical query parameter name */
    private static final String LANG = "lang";

    /** The maxResults canonical query parameter name */
    private static final String MAX_RESULTS = "maxResults";

    /** Convenience list of all the canonical query parameter names */
    private static final List<String> CANONICAL_QUERY_PARAMETERS;

    static {
        CANONICAL_QUERY_PARAMETERS = new ArrayList<String>();
        CANONICAL_QUERY_PARAMETERS.add(QUERY_ID);
        CANONICAL_QUERY_PARAMETERS.add(DEPTH);
        CANONICAL_QUERY_PARAMETERS.add(FORMAT);
        CANONICAL_QUERY_PARAMETERS.add(FEDERATED);
        CANONICAL_QUERY_PARAMETERS.add(FEDERATION);
        CANONICAL_QUERY_PARAMETERS.add(MATCH_OLDER_VERSIONS);
        CANONICAL_QUERY_PARAMETERS.add(START_INDEX);
        CANONICAL_QUERY_PARAMETERS.add(LANG);
        CANONICAL_QUERY_PARAMETERS.add(MAX_RESULTS);

    }

    /** The local query manager */
    private QueryManager queryManager;

    /** Jaxb Manager for marshalling the response */
    private JAXBManager responseJaxb;

    /**
     * Creates a new QueryProtocolRestService instance
     * 
     * @throws JAXBException
     *             If errors occur while initializing the JAXBManager
     */
    public QueryProtocolRestService() throws JAXBException {
        responseJaxb = new JAXBManager(QueryResponse.class);
    }

    /**
     * Executes a query based on the submitted query parameters
     * 
     * @param info
     *            The UriInfo containing the query parameters
     * @return The marshalled QueryResponse
     * @throws JAXBException
     *             If errors occur while marshalling the response
     * @throws MsgRegistryException
     *             If errors occur in the registry while querying for the
     *             objects
     */
    @GET
    @Produces("text/xml")
    public String executeQuery(@Context UriInfo info) throws JAXBException,
            MsgRegistryException {
        /*
         * Extract out the canonical query parameters
         */
        MultivaluedMap<String, String> queryParameters = info
                .getQueryParameters();
        String queryId = queryParameters.getFirst(QUERY_ID) == null ? "urn:oasis:names:tc:ebxml-regrep:query:GetObjectById"
                : queryParameters.getFirst(QUERY_ID);

        BigInteger depth = queryParameters.getFirst(DEPTH) == null ? null
                : new BigInteger(queryParameters.getFirst(DEPTH));

        String format = queryParameters.getFirst(FORMAT);

        boolean federated = queryParameters.getFirst(FEDERATED) == null ? false
                : Boolean.parseBoolean(queryParameters.getFirst(FEDERATED));

        String federation = queryParameters.getFirst(FEDERATION);

        boolean matchOlderVersions = queryParameters
                .getFirst(MATCH_OLDER_VERSIONS) == null ? true : Boolean
                .parseBoolean(queryParameters.getFirst(MATCH_OLDER_VERSIONS));

        BigInteger startIndex = queryParameters.getFirst(START_INDEX) == null ? new BigInteger(
                "0") : new BigInteger(queryParameters.getFirst(START_INDEX));

        String lang = queryParameters.getFirst(LANG) == null ? Languages.EN_US
                : queryParameters.getFirst(LANG);

        BigInteger maxResults = queryParameters.getFirst(MAX_RESULTS) == null ? new BigInteger(
                "0") : new BigInteger(queryParameters.getFirst(MAX_RESULTS));

        /*
         * Create the query request object
         */
        QueryRequest restQueryRequest = new QueryRequest();
        ResponseOptionType responseOption = new ResponseOptionType();
        responseOption.setReturnType(QueryReturnTypes.REGISTRY_OBJECT);
        responseOption.setReturnComposedObjects(true);
        QueryType queryType = new QueryType();
        queryType.setQueryDefinition(queryId);

        restQueryRequest.setDepth(depth);
        restQueryRequest.setFormat(format);
        restQueryRequest.setFederated(federated);
        restQueryRequest.setFederation(federation);
        restQueryRequest.setMatchOlderVersions(matchOlderVersions);
        restQueryRequest.setStartIndex(startIndex);
        restQueryRequest.setLang(lang);
        restQueryRequest.setMaxResults(maxResults);
        restQueryRequest.setQuery(queryType);
        restQueryRequest.setResponseOption(responseOption);

        /*
         * Extract out any non-canonical query parameters and assign them as
         * slots to be used as query specific arguments
         */
        for (String key : queryParameters.keySet()) {
            if (!CANONICAL_QUERY_PARAMETERS.contains(key)) {
                queryType.addSlot(key, queryParameters.getFirst(key));
            }
        }
        return responseJaxb.marshalToXml(queryManager
                .executeQuery(restQueryRequest));
    }

    public void setQueryManager(QueryManager queryManager) {
        this.queryManager = queryManager;
    }

}
