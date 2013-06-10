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
import com.raytheon.uf.common.registry.services.rest.IQueryProtocolRestService;
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
 * 5/21/2013    2022        bphillip    Added interface and moved constants
 * </pre>
 * 
 * @author bphillip
 * @version 1
 */
@Path("/rest/search")
@Service
@Transactional
public class QueryProtocolRestService implements IQueryProtocolRestService {

    /** Convenience list of all the canonical query parameter names */
    private static final List<String> CANONICAL_QUERY_PARAMETERS;

    static {
        CANONICAL_QUERY_PARAMETERS = new ArrayList<String>();
        CANONICAL_QUERY_PARAMETERS.add(QueryRequest.QUERY_ID);
        CANONICAL_QUERY_PARAMETERS.add(QueryRequest.DEPTH);
        CANONICAL_QUERY_PARAMETERS.add(QueryRequest.FORMAT);
        CANONICAL_QUERY_PARAMETERS.add(QueryRequest.FEDERATED);
        CANONICAL_QUERY_PARAMETERS.add(QueryRequest.FEDERATION);
        CANONICAL_QUERY_PARAMETERS.add(QueryRequest.MATCH_OLDER_VERSIONS);
        CANONICAL_QUERY_PARAMETERS.add(QueryRequest.START_INDEX);
        CANONICAL_QUERY_PARAMETERS.add(QueryRequest.LANG);
        CANONICAL_QUERY_PARAMETERS.add(QueryRequest.MAX_RESULTS);

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

    public String executeQuery() {

        return null;
    }

    @GET
    @Produces("text/xml")
    public String executeQuery(@Context UriInfo info) throws JAXBException,
            MsgRegistryException {
        /*
         * Extract out the canonical query parameters
         */
        MultivaluedMap<String, String> queryParameters = info
                .getQueryParameters();
        String queryId = queryParameters.getFirst(QueryRequest.QUERY_ID) == null ? "urn:oasis:names:tc:ebxml-regrep:query:GetObjectById"
                : queryParameters.getFirst(QueryRequest.QUERY_ID);

        BigInteger depth = queryParameters.getFirst(QueryRequest.DEPTH) == null ? null
                : new BigInteger(queryParameters.getFirst(QueryRequest.DEPTH));

        String format = queryParameters.getFirst(QueryRequest.FORMAT);

        boolean federated = queryParameters.getFirst(QueryRequest.FEDERATED) == null ? false
                : Boolean.parseBoolean(queryParameters
                        .getFirst(QueryRequest.FEDERATED));

        String federation = queryParameters.getFirst(QueryRequest.FEDERATION);

        boolean matchOlderVersions = queryParameters
                .getFirst(QueryRequest.MATCH_OLDER_VERSIONS) == null ? true
                : Boolean.parseBoolean(queryParameters
                        .getFirst(QueryRequest.MATCH_OLDER_VERSIONS));

        BigInteger startIndex = queryParameters
                .getFirst(QueryRequest.START_INDEX) == null ? new BigInteger(
                "0") : new BigInteger(
                queryParameters.getFirst(QueryRequest.START_INDEX));

        String lang = queryParameters.getFirst(QueryRequest.LANG) == null ? Languages.EN_US
                : queryParameters.getFirst(QueryRequest.LANG);

        BigInteger maxResults = queryParameters
                .getFirst(QueryRequest.MAX_RESULTS) == null ? new BigInteger(
                "0") : new BigInteger(
                queryParameters.getFirst(QueryRequest.MAX_RESULTS));

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
