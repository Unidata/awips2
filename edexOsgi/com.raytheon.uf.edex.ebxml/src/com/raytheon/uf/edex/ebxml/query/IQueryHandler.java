package com.raytheon.uf.edex.ebxml.query;

import java.io.IOException;
import java.util.List;

import oasis.names.tc.ebxml.regrep.xsd.rim.v4.QueryType;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.RegistryObjectType;

/**
 * Interface for objects that handle specific queries.
 * 
 * @author jsherida
 */
public interface IQueryHandler {

    /**
     * Returns the definition string for the query that this handler handles.
     * 
     * @return the definition string.
     */
    public String getQueryDefinition();

    /**
     * Handle the given query.
     * 
     * @param query
     *            The query.
     * @return A list of {@link RegistryObjectType}s that correspond to the
     *         query, if applicable.
     * @throws IOException
     *             if there was a problem contacting the registry.
     */
    public List<RegistryObjectType> handleQuery(QueryType query, boolean federated,
            String federation, long startIndex, long maxResults,
            long depth, boolean matchOlderVersion) throws IOException;

}