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
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.ExtrinsicObjectType;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.QueryType;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.RegistryObjectType;

import com.raytheon.uf.edex.registry.ebxml.dao.HqlQueryUtil;
import com.raytheon.uf.edex.registry.ebxml.exception.EbxmlRegistryException;
import com.raytheon.uf.edex.registry.ebxml.services.query.QueryConstants;
import com.raytheon.uf.edex.registry.ebxml.services.query.QueryParameters;
import com.raytheon.uf.edex.registry.ebxml.services.query.types.CanonicalEbxmlQuery;

/**
 * BsicQuery extension for ExtrinsicObjects. Adds mimeTypes parameter.
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
 * @see com.raytheon.uf.edex.registry.ebxml.services.query.types.canonical
 *      .BasicQuery
 */

public class ExtrinsicObjectQuery extends CanonicalEbxmlQuery {

    public static final String QUERY_DEFINITION = QUERY_CANONICAL_PREFIX
            + "ExtrinsicObjectQuery";

    /** The list of valid parameters for this query */
    private static final List<String> QUERY_PARAMETERS = new ArrayList<String>();

    /* Initializes the list of parameters */
    static {
        QUERY_PARAMETERS.add(QueryConstants.CLASSIFICATIONS);
        QUERY_PARAMETERS.add(QueryConstants.DESCRIPTION);
        QUERY_PARAMETERS.add(QueryConstants.MATCH_ANY);
        QUERY_PARAMETERS.add(QueryConstants.NAME);
        QUERY_PARAMETERS.add(QueryConstants.OBJECT_TYPE);
        QUERY_PARAMETERS.add(QueryConstants.STATUS);
        QUERY_PARAMETERS.add(QueryConstants.MIME_TYPES);
    }

    @Override
    protected <T extends RegistryObjectType> List<T> query(QueryType queryType,
            QueryResponse queryResponse) throws EbxmlRegistryException {
        QueryParameters params = getParameterMap(queryType.getSlot(),
                queryResponse);
        registryObjectDao.setDaoClass(ExtrinsicObjectType.class);
        @SuppressWarnings("unchecked")
        List<T> results = (List<T>) new BasicQuery().query(queryType,
                queryResponse);
        List<String> ids = new ArrayList<String>();
        for (RegistryObjectType regObj : results) {
            ids.add(regObj.getId());
        }
        if (ids.isEmpty()) {
            return Collections.emptyList();
        }
        List<Object> mimeTypes = params.getParameter(QueryConstants.MIME_TYPES);
        if (mimeTypes == null) {
            return results;
        }
        StringBuilder query = new StringBuilder();
        HqlQueryUtil.assembleSingleParamQuery(query, ExtrinsicObjectType.class,
                QueryConstants.ID, HqlQueryUtil.IN, ids);
        query.append(HqlQueryUtil.AND);
        HqlQueryUtil.assembleSingleParamClause(query, QueryConstants.MIME_TYPE,
                HqlQueryUtil.IN, mimeTypes);
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
