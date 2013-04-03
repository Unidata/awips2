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
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.ClassificationType;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.QueryType;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.RegistryObjectType;

import com.raytheon.uf.edex.registry.ebxml.dao.HqlQueryUtil;
import com.raytheon.uf.edex.registry.ebxml.dao.RegistryObjectTypeDao;
import com.raytheon.uf.edex.registry.ebxml.exception.EbxmlRegistryException;
import com.raytheon.uf.edex.registry.ebxml.services.query.QueryConstants;
import com.raytheon.uf.edex.registry.ebxml.services.query.QueryParameters;
import com.raytheon.uf.edex.registry.ebxml.services.query.types.CanonicalEbxmlQuery;

/**
 * The canonical query BasicQuery allows clients to query for RegistryObjects by
 * their name, description, type, status and classifications.
 * <p>
 * <b>Parameter Summary:</b> <br>
 * · <b><i>classifications</i></b> -- Set whose elements are path attribute
 * values to ClassificationNodes.
 * <p>
 * Matches RegistryObjects that have a classification whose classificationNode
 * attribute value matches the id of the ClassificationNode where
 * rim:RegistryObject[@xsi:type="rim:ClassificationNodeType"]/@path matches
 * specified value
 * <p>
 * · <b><i>description</i></b> -- Matches
 * rim:RegistryObject/rim:Description/rim:LocalizedString/@value
 * <p>
 * · <b><i>matchOnAnyParameter</i></b> -- If true then use logical OR between
 * predicates for each parameter.
 * <p>
 * · <b><i>name</i></b> -- Matches
 * rim:RegistryObject/rim:Name/rim:LocalizedString/@value
 * <p>
 * · <b><i>objectType</i></b> -- Matches RegistryObjects whose objectType
 * attribute matches the id of the ClassificationNode where
 * rim:ClassificationNode/@path matches specified value
 * <p>
 * · <b><i>owner</i></b> -- Matches rim:RegistryObject/@owner. Note that a
 * parameter value of “#@'@#rs:currentUserId()#@'@#” may be used to specify the
 * id of the user associated with the current request
 * <p>
 * · <b><i>status</i></b> -- Matches RegistryObjects whose status attribute
 * matches the id of the ClassificationNode where rim:ClassificationNode/@path
 * matches specified value
 * <p>
 * · This query has several optional parameters
 * <p>
 * · Each parameter implies a predicate within the underlying query
 * <p>
 * · Predicates for each supplied parameter are combined using with an implicit
 * LOGICAL AND if matchOnAnyParameter is unspecified or false. If it is
 * specified as true then predicates for each supplied parameters are combined
 * using a LOGICAL OR
 * <p>
 * · If an optional parameter is not supplied then its corresponding predicate
 * MUST NOT be included in the underlying query
 * <p>
 * 
 * 
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 2/13/2012    #184       bphillip    Initial creation
 * 
 * </pre>
 * 
 * @author bphillip
 * @version 1.0
 */
public class BasicQuery extends CanonicalEbxmlQuery {

    public static final String QUERY_DEFINITION = QUERY_CANONICAL_PREFIX
            + "BasicQuery";

    /** The list of valid parameters for this query */
    private static final List<String> QUERY_PARAMETERS = new ArrayList<String>();

    /* Initializes the list of parameters */
    static {
        QUERY_PARAMETERS.add(QueryConstants.CLASSIFICATIONS);
        QUERY_PARAMETERS.add(QueryConstants.DESCRIPTION);
        QUERY_PARAMETERS.add(QueryConstants.MATCH_ANY);
        QUERY_PARAMETERS.add(QueryConstants.NAME);
        QUERY_PARAMETERS.add(QueryConstants.OBJECT_TYPE);
        QUERY_PARAMETERS.add(QueryConstants.OWNER);
        QUERY_PARAMETERS.add(QueryConstants.STATUS);
    }

    private boolean buildClassificationsClause(StringBuilder query,
            List<Object> classifications) throws EbxmlRegistryException {
        if (classifications == null || classifications.isEmpty()) {
            return true;
        }

        List<String> ids = new ArrayList<String>();

        RegistryObjectTypeDao dao = new RegistryObjectTypeDao(
                ClassificationType.class);

        for (int i = 0; i < classifications.size(); i++) {
            String subQuery = HqlQueryUtil.assembleSingleParamQuery(
                    ClassificationType.class, "classifiedObject",
                    HqlQueryUtil.EQUALS, classifications.get(i));
            if (i == 0) {
                ids = dao.executeHQLQuery(subQuery);
            } else {
                ids.retainAll(dao.executeHQLQuery(subQuery));
            }
        }

        if (ids.isEmpty()) {
            return false;
        }
        HqlQueryUtil.assembleInClause(query, "obj." + QueryConstants.ID, ids);
        return true;
    }

    protected List<RegistryObjectType> query(QueryType queryType,
            QueryResponse queryResponse) throws EbxmlRegistryException {

        QueryParameters params = getParameterMap(queryType.getSlot(),
                queryResponse);
        String description = params
                .getFirstParameter(QueryConstants.DESCRIPTION);
        String name = params.getFirstParameter(QueryConstants.NAME);
        String objectType = params
                .getFirstParameter(QueryConstants.OBJECT_TYPE);
        String owner = params.getFirstParameter(QueryConstants.OWNER);
        String status = params.getFirstParameter(QueryConstants.STATUS);
        String conjunction = HqlQueryUtil.AND;
        if (params.containsParameter(QueryConstants.MATCH_ANY)) {
            if (((Boolean) params.getFirstParameter(QueryConstants.MATCH_ANY))
                    .booleanValue()) {
                conjunction = HqlQueryUtil.OR;
            }
        }

        StringBuilder query = new StringBuilder();
        query.append("select ").append(HqlQueryUtil.OBJ)
                .append("from RegistryObjectType ").append(HqlQueryUtil.OBJ);
        if (params.containsParameter(QueryConstants.NAME)) {
            query.append(HqlQueryUtil.INNER_JOIN).append(HqlQueryUtil.OBJ_DOT)
                    .append(QueryConstants.NAME)
                    .append(".localizedString as names ");
        }
        if (params.containsParameter(QueryConstants.DESCRIPTION)) {
            query.append(HqlQueryUtil.INNER_JOIN).append(HqlQueryUtil.OBJ_DOT)
                    .append(QueryConstants.DESCRIPTION)
                    .append(".localizedString as descriptions ");
        }
        query.append(HqlQueryUtil.WHERE);
        if (description != null) {
            query.append("descriptions.value='").append(description)
                    .append("'");
            query.append(conjunction);
        }
        if (name != null) {
            query.append("names.value='").append(description).append("'");
            query.append(conjunction);
        }
        if (objectType != null) {
            HqlQueryUtil
                    .assembleSingleParamClause(query,
                            QueryConstants.OBJECT_TYPE, HqlQueryUtil.EQUALS,
                            objectType);
            query.append(conjunction);
        }
        if (owner != null) {
            HqlQueryUtil.assembleSingleParamClause(query, QueryConstants.OWNER,
                    HqlQueryUtil.EQUALS, owner);
            query.append(conjunction);
        }
        if (status != null) {
            HqlQueryUtil.assembleSingleParamClause(query,
                    QueryConstants.STATUS, HqlQueryUtil.EQUALS, status);
            query.append(conjunction);
        }

        if (!buildClassificationsClause(query,
                params.getParameter(QueryConstants.CLASSIFICATIONS))) {
            return Collections.emptyList();
        }
        return registryObjectDao.executeHQLQuery(query.substring(0,
                query.length() - conjunction.length()));

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
