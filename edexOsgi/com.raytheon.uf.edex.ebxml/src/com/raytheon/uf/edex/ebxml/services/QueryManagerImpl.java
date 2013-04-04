/**
 * 
 */
package com.raytheon.uf.edex.ebxml.services;

import java.io.IOException;
import java.io.PrintWriter;
import java.io.StringWriter;
import java.math.BigInteger;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import oasis.names.tc.ebxml.regrep.wsdl.registry.services.v4.MsgRegistryException;
import oasis.names.tc.ebxml.regrep.wsdl.registry.services.v4.QueryManager;
import oasis.names.tc.ebxml.regrep.xsd.query.v4.QueryRequest;
import oasis.names.tc.ebxml.regrep.xsd.query.v4.QueryResponse;
import oasis.names.tc.ebxml.regrep.xsd.query.v4.ResponseOptionType;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.QueryType;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.RegistryObjectListType;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.RegistryObjectType;
import oasis.names.tc.ebxml.regrep.xsd.rs.v4.RegistryExceptionType;
import oasis.names.tc.ebxml.regrep.xsd.rs.v4.UnsupportedCapabilityExceptionType;

import com.raytheon.uf.edex.ebxml.query.AdhocQueryHandler;
import com.raytheon.uf.edex.ebxml.query.BasicQueryHandler;
import com.raytheon.uf.edex.ebxml.query.IQueryHandler;

/**
 * Implementation of the {@link QueryManager}.
 * 
 * <br>
 * <br>
 * Note: Quoted comments come from regrep-rs.pdf
 * 
 * @author jsherida
 */
public class QueryManagerImpl implements QueryManager {

    protected Map<String, IQueryHandler> queryHandlers = Collections
            .synchronizedMap(new HashMap<String, IQueryHandler>());

    /** Default Constructor. */
    public QueryManagerImpl() {
        queryHandlers.put(BasicQueryHandler.QUERY_DEFINITION,
                new BasicQueryHandler());
        queryHandlers.put(AdhocQueryHandler.QUERY_DEFINITION,
                new AdhocQueryHandler());
    }

    /** {@inheritDoc} */
    @Override
    public QueryResponse executeQuery(QueryRequest partQueryRequest)
            throws MsgRegistryException {

        ResponseOptionType responseOption = partQueryRequest
                .getResponseOption();

        long depth = partQueryRequest.getDepth().longValue();
        boolean federated = partQueryRequest.isFederated();
        String federation = partQueryRequest.getFederation();
        String lang = partQueryRequest.getLang();
        boolean matchOlderVersions = partQueryRequest.isMatchOlderVersions();
        long maxResults = partQueryRequest.getMaxResults().longValue();
        long startIndex = partQueryRequest.getStartIndex().longValue();

        QueryType query = partQueryRequest.getQuery();
        IQueryHandler handler = queryHandlers.get(query.getQueryDefinition());
        List<RegistryObjectType> matchingObjects = new ArrayList<RegistryObjectType>();
        if (handler == null) {
            oasis.names.tc.ebxml.regrep.xsd.rs.v4.ObjectFactory rsFactory = new oasis.names.tc.ebxml.regrep.xsd.rs.v4.ObjectFactory();

            UnsupportedCapabilityExceptionType e = rsFactory
                    .createUnsupportedCapabilityExceptionType();
            e.setMessage("Query type not supported: "
                    + query.getQueryDefinition());
            e.setDetail(query.getQueryDefinition());
            throw new MsgRegistryException("Query not supported", e);
        } else {
            try {
                matchingObjects.addAll(handler.handleQuery(query, federated,
                        federation, startIndex, maxResults, depth,
                        matchOlderVersions));
            } catch (IOException ioe) {
                oasis.names.tc.ebxml.regrep.xsd.rs.v4.ObjectFactory rsFactory = new oasis.names.tc.ebxml.regrep.xsd.rs.v4.ObjectFactory();

                StringWriter writer = new StringWriter();
                ioe.printStackTrace(new PrintWriter(writer));

                RegistryExceptionType e = rsFactory
                        .createRegistryExceptionType();
                e.setMessage(ioe.getMessage());
                e.setDetail(writer.toString());
                e.setCode(ioe.getClass().toString());
                throw new MsgRegistryException("Query failed", e, ioe);
            }
        }

        oasis.names.tc.ebxml.regrep.xsd.query.v4.ObjectFactory queryFactory = new oasis.names.tc.ebxml.regrep.xsd.query.v4.ObjectFactory();
        oasis.names.tc.ebxml.regrep.xsd.rim.v4.ObjectFactory rimFactory = new oasis.names.tc.ebxml.regrep.xsd.rim.v4.ObjectFactory();

        QueryResponse response = queryFactory.createQueryResponse();
        response.setRequestId(partQueryRequest.getId());
        response.setStatus("urn:oasis:names:tc:ebxml-regrep:ResponseStatusType:Success");
        response.setTotalResultCount(new BigInteger(Integer
                .toString(matchingObjects.size())));

        RegistryObjectListType objList = rimFactory
                .createRegistryObjectListType();
        List<RegistryObjectType> objects = objList.getRegistryObject();
        objects.addAll(matchingObjects);

        response.setRegistryObjectList(objList);

        return response;
    }

}
