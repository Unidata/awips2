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
package com.raytheon.uf.edex.registry.ebxml.dao;

import java.util.List;

import oasis.names.tc.ebxml.regrep.xsd.rim.v4.ParameterType;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.QueryDefinitionType;

/**
 * 
 * Data access object for QueryDefinitionType objects
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#     Engineer    Description
 * ------------ ----------  ----------- --------------------------
 * 10/8/2013    1682        bphillip    Initial implementation
 * 12/2/2013    1829        bphillip    Changed get parameters for query method
 * </pre>
 * 
 * @author bphillip
 * @version 1
 */
public class QueryDefinitionDao extends
        RegistryObjectTypeDao<QueryDefinitionType> {

    /**
     * Query used to get the query Ids of all the query definitions contained in
     * the registry
     */
    private static final String GET_QUERY_IDS_QUERY = "SELECT obj.id FROM QueryDefinitionType obj order by obj.id asc";

    /**
     * Gets the ids of all the query definitions contained in the registry
     * 
     * @return The ids of the query definitions in the registry
     */
    public List<String> getQueryIds() {
        return executeHQLQuery(GET_QUERY_IDS_QUERY);
    }

    /**
     * Gets the parameters for the given query
     * 
     * @param queryId
     *            The query id to get the parameters for
     * @return The parameters for the specified query
     */
    public List<ParameterType> getParametersForQuery(String queryId) {
        return this.getById(queryId).getParameter();
    }

    @Override
    protected Class<QueryDefinitionType> getEntityClass() {
        return QueryDefinitionType.class;
    }

}
