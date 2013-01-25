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
package com.raytheon.uf.edex.registry.ebxml.services.query.types.canonical;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

import oasis.names.tc.ebxml.regrep.xsd.query.v4.QueryResponse;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.QueryType;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.RegistryObjectType;

import com.raytheon.uf.edex.registry.ebxml.dao.HqlQueryUtil;
import com.raytheon.uf.edex.registry.ebxml.exception.EbxmlRegistryException;
import com.raytheon.uf.edex.registry.ebxml.services.query.QueryConstants;
import com.raytheon.uf.edex.registry.ebxml.services.query.QueryParameters;
import com.raytheon.uf.edex.registry.ebxml.services.query.types.CanonicalEbxmlQuery;

/**
 * TODO Add Description
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jan 18, 2012            bphillip     Initial creation
 * 
 * </pre>
 * 
 * @author bphillip
 * @version 1.0
 */

public class GetObjectsByLid extends CanonicalEbxmlQuery {

    /** The list of valid parameters for this query */
    private static final List<String> QUERY_PARAMETERS = new ArrayList<String>();

    /* Initializes the list of parameters */
    static {
        QUERY_PARAMETERS.add(QueryConstants.LID);
    }

    public static final String QUERY_DEFINITION = QUERY_CANONICAL_PREFIX
            + "GetObjectByLid";

    @Override
    protected <T extends RegistryObjectType> List<T> query(QueryType queryType,
            QueryResponse queryResponse) throws EbxmlRegistryException {

        QueryParameters parameters = getParameterMap(queryType.getSlot(),
                queryResponse);
        // The client did not specify the required parameter
        if (parameters.isEmpty()
                || !parameters.containsParameter(QueryConstants.LID)) {
            throw new EbxmlRegistryException("Canonical query ["
                    + this.getQueryDefinition()
                    + "] is missing required parameter ["
                    + QUERY_PARAMETERS.get(0) + "]");
        }

        StringBuilder query = new StringBuilder();
        String lid = parameters.getFirstParameter(QueryConstants.LID);
        List<String> lids = new ArrayList<String>();
        if (lid.contains("_") || lid.contains("%")) {
            List<String> matchingLids = registryObjectDao.getMatchingLids(lid);
            if (matchingLids.isEmpty()) {
                return Collections.emptyList();
            }
            lids.addAll(matchingLids);

        } else {
            lids.add(lid);
        }
        HqlQueryUtil.assembleSingleParamQuery(query, RegistryObjectType.class,
                QueryConstants.LID, HqlQueryUtil.IN, lids);
        query.append(" order by obj.lid asc,obj.versionInfo.versionNumber desc");
        return registryObjectDao.executeHQLQuery(query);
    }

    @Override
    protected List<String> getValidParameters() {
        return QUERY_PARAMETERS;
    }

    @Override
    public String getQueryDefinition() {
        return QUERY_DEFINITION;
    }
}
