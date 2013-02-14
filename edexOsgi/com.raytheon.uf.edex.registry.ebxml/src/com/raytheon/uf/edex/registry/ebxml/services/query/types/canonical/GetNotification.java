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
import java.util.List;

import oasis.names.tc.ebxml.regrep.xsd.query.v4.QueryResponse;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.QueryType;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.RegistryObjectType;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.SubscriptionType;

import com.raytheon.uf.edex.registry.ebxml.dao.HqlQueryUtil;
import com.raytheon.uf.edex.registry.ebxml.dao.RegistryObjectTypeDao;
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

public class GetNotification extends CanonicalEbxmlQuery {

    public static final String QUERY_DEFINITION = QUERY_CANONICAL_PREFIX
            + "GetNotification";

    /** The list of valid parameters for this query */
    private static final List<String> QUERY_PARAMETERS = new ArrayList<String>();

    /* Initializes the list of parameters */
    static {
        QUERY_PARAMETERS.add(QueryConstants.SUBSCRIPTION_ID);
        QUERY_PARAMETERS.add(QueryConstants.START_TIME);
    }

    @Override
    protected <T extends RegistryObjectType> List<T> query(QueryType queryType,
            QueryResponse queryResponse) throws EbxmlRegistryException {
        RegistryObjectTypeDao subscriptionDao = new RegistryObjectTypeDao(
                SubscriptionType.class);
        QueryParameters parameters = getParameterMap(queryType.getSlot(),
                queryResponse);
        // The client did not specify the required parameter
        if (parameters.isEmpty()
                || !parameters
                        .containsParameter(QueryConstants.SUBSCRIPTION_ID)) {
            throw new EbxmlRegistryException("Canonical query ["
                    + this.getQueryDefinition()
                    + "] is missing required parameter ["
                    + QUERY_PARAMETERS.get(0) + "]");
        }

        String subscriptionId = parameters
                .getFirstParameter(QueryConstants.SUBSCRIPTION_ID);
        Object startTime = parameters
                .getFirstParameter(QueryConstants.START_TIME);
        StringBuilder query = new StringBuilder();
        HqlQueryUtil.assembleSingleParamQuery(query, SubscriptionType.class,
                QueryConstants.ID, HqlQueryUtil.EQUALS, subscriptionId);
        if (startTime != null) {
            HqlQueryUtil.assembleSingleParamClause(query,
                    QueryConstants.START_TIME, HqlQueryUtil.EQUALS,
                    startTime.toString());
        }
        return subscriptionDao.executeHQLQuery(query);
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
