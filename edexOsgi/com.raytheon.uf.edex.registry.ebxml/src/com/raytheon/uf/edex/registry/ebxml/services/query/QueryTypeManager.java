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

import java.util.HashMap;
import java.util.List;
import java.util.Map;

import com.raytheon.uf.edex.registry.ebxml.services.query.types.IRegistryQuery;

/**
 * This class contains a mapping of query definitions to query implementations.
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
public class QueryTypeManager {

    /**
     * This is a convenience variable used only by Spring to inject the query
     * types
     */
    @SuppressWarnings("unused")
    private List<IRegistryQuery> queryList;

    /** The query map */
    private Map<String, IRegistryQuery> queryTypeMap = new HashMap<String, IRegistryQuery>();

    /**
     * Private constructor
     */
    public QueryTypeManager() {
    }

    /**
     * Gets a query implementation from the given query definition
     * 
     * @param queryDefinition
     *            The query definition for which to get the associated
     *            implementation
     * @return The query implementation
     */
    public IRegistryQuery getQueryType(String queryDefinition) {
        return queryTypeMap.get(queryDefinition);
    }

    /**
     * Setter method used by Spring to inject the list of query types
     * 
     * @param queryList
     *            The list of query types
     */
    public void setQueryList(List<IRegistryQuery> queryList) {
        this.queryList = queryList;
        for (IRegistryQuery query : queryList) {
            queryTypeMap.put(query.getQueryDefinition(), query);
        }
    }

}
