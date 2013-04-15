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
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.ClassificationSchemeType;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.QueryType;

import com.raytheon.uf.common.registry.constants.CanonicalQueryTypes;
import com.raytheon.uf.edex.registry.ebxml.dao.RegistryObjectTypeDao;
import com.raytheon.uf.edex.registry.ebxml.exception.EbxmlRegistryException;
import com.raytheon.uf.edex.registry.ebxml.services.query.QueryConstants;
import com.raytheon.uf.edex.registry.ebxml.services.query.QueryParameters;
import com.raytheon.uf.edex.registry.ebxml.services.query.types.CanonicalEbxmlQuery;

/**
 * The canonical query GetClassificationSchemesById allows clients to fetch
 * specified ClassificationSchemes.
 * 
 * <p>
 * <b>Parameter Summary:</b> <br>
 * · <b><i>id</i></b> -- Matches
 * rim:/RegistryObject[@xsi:type="rim:ClassificationSchemeType"]/@id.<br>
 * Allows use of “%” wildcard character to match multiple characters.<br>
 * Allows use of “?” wildcard character to match a single character.<br>
 * <p>
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
 */

public class GetClassificationSchemesById extends CanonicalEbxmlQuery {

    /** The list of valid parameters for this query */
    private static final List<String> QUERY_PARAMETERS = new ArrayList<String>();

    /* Initializes the list of parameters */
    static {
        QUERY_PARAMETERS.add(QueryConstants.ID);
    }

    private RegistryObjectTypeDao<ClassificationSchemeType> classificationSchemeTypeDao;

    @Override
    protected void query(QueryType queryType, QueryResponse queryResponse)
            throws EbxmlRegistryException {
        QueryParameters parameters = getParameterMap(queryType.getSlot(),
                queryResponse);
        // The client did not specify the required parameter
        if (parameters.isEmpty()
                || !parameters.containsParameter(QueryConstants.ID)) {
            throw new EbxmlRegistryException("Canonical query ["
                    + this.getQueryDefinition()
                    + "] is missing required parameter ["
                    + QUERY_PARAMETERS.get(0) + "]");
        }

        String id = parameters.getFirstParameter(QueryConstants.ID);
        List<String> ids = new ArrayList<String>();
        if (id.contains("_") || id.contains("%")) {
            List<String> matchingIds = classificationSchemeTypeDao
                    .getMatchingIds(id);
            if (matchingIds.isEmpty()) {
                return;
            }
            ids.addAll(matchingIds);

        } else {
            ids.add(id);
        }

        queryResponse.getRegistryObjectList().getRegistryObject()
                .addAll(classificationSchemeTypeDao.getById(ids));
    }

    @Override
    protected List<String> getValidParameters() {
        return QUERY_PARAMETERS;
    }

    @Override
    public String getQueryDefinition() {
        return CanonicalQueryTypes.GET_CLASSIFICATION_SCHEMES_BY_ID;
    }

    public void setClassificationSchemeTypeDao(
            RegistryObjectTypeDao<ClassificationSchemeType> classificationSchemeTypeDao) {
        this.classificationSchemeTypeDao = classificationSchemeTypeDao;
    }

}
