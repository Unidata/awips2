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
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.ObjectRefListType;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.QueryType;

import com.raytheon.uf.common.registry.constants.CanonicalQueryTypes;
import com.raytheon.uf.edex.registry.ebxml.exception.EbxmlRegistryException;
import com.raytheon.uf.edex.registry.ebxml.services.query.QueryConstants;
import com.raytheon.uf.edex.registry.ebxml.services.query.QueryManagerImpl.RETURN_TYPE;
import com.raytheon.uf.edex.registry.ebxml.services.query.QueryParameters;
import com.raytheon.uf.edex.registry.ebxml.services.query.types.CanonicalEbxmlQuery;
import com.raytheon.uf.edex.registry.ebxml.util.EbxmlObjectUtil;

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
 * 3/18/2013    1802       bphillip    Modified to use transaction boundaries and spring dao injection
 * 4/9/2013     1802       bphillip     Changed abstract method signature, modified return processing, and changed static variables
 * 
 * </pre>
 * 
 * @author bphillip
 * @version 1.0
 * @see com.raytheon.uf.edex.registry.ebxml.services.query.types.canonical
 *      .BasicQuery
 */

public class ExtrinsicObjectQuery extends CanonicalEbxmlQuery {

    /** The list of valid parameters for this query */
    private static final List<String> QUERY_PARAMETERS = new ArrayList<String>();

    private static final String QUERY_STRING = "FROM ExtrinsicObjectType obj WHERE obj.mimeTypes in (:mimeTypes)";

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

    private BasicQuery basicQuery;

    @Override
    protected void query(QueryType queryType, QueryResponse queryResponse)
            throws EbxmlRegistryException {
        QueryParameters params = getParameterMap(queryType.getSlot(),
                queryResponse);

        QueryResponse basicQueryResponse = new QueryResponse();
        basicQuery.setReturnType(RETURN_TYPE.ObjectRef);
        basicQuery.query(queryType, basicQueryResponse);
        ObjectRefListType basicQueryResult = basicQueryResponse
                .getObjectRefList();
        if (basicQueryResult == null
                || basicQueryResult.getObjectRef().isEmpty()) {
            return;
        }

        List<String> ids = EbxmlObjectUtil
                .getIdsFromObjectRefListType(basicQueryResult);
        List<Object> mimeTypes = params.getParameter(QueryConstants.MIME_TYPES);
        if (mimeTypes == null) {
            return;
        }

        StringBuilder query = new StringBuilder();
        if (returnType.equals(RETURN_TYPE.ObjectRef)) {
            query.append("SELECT obj.id ");
        } else {
            query.append("SELECT obj ");
        }
        setResponsePayload(queryResponse, registryObjectDao.executeHQLQuery(
                QUERY_STRING, maxResults, "mimeTypes", ids));
    }

    @Override
    protected List<String> getValidParameters() {
        return QUERY_PARAMETERS;
    }

    @Override
    public String getQueryDefinition() {
        return CanonicalQueryTypes.EXTRINSIC_OBJECT_QUERY;
    }

    public void setBasicQuery(BasicQuery basicQuery) {
        this.basicQuery = basicQuery;
    }

}
