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
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.AssociationType;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.ClassificationNodeType;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.QueryType;

import com.raytheon.uf.edex.registry.ebxml.dao.ClassificationNodeDao;
import com.raytheon.uf.edex.registry.ebxml.dao.HqlQueryUtil;
import com.raytheon.uf.edex.registry.ebxml.exception.EbxmlRegistryException;
import com.raytheon.uf.edex.registry.ebxml.services.query.QueryConstants;
import com.raytheon.uf.edex.registry.ebxml.services.query.QueryParameters;
import com.raytheon.uf.edex.registry.ebxml.services.query.types.CanonicalEbxmlQuery;

/**
 * The canonical query FindAssociations query allows clients to find
 * Associations that match the specified criteria.
 * <p>
 * <b>Parameter Summary:</b> <br>
 * · <b><i>associationType</i></b> -- Matches Associations whose type attribute
 * references a ClassificationNode where rim:ClassificationNode/@path matches
 * specified value
 * <p>
 * · <b><i>matchOnAnyParameter</i></b> -- If true then use logical OR between
 * predicates for each parameter
 * <p>
 * · <b><i>sourceObjectId</i></b> -- Matches
 * rim:/RegistryObject[@xsi:type="rim:AssociationType"]/@sourceObject.<br>
 * Allows use of “%” wildcard character to match multiple characters.<br>
 * Allows use of “?” wildcard character to match a single character.<br>
 * <p>
 * · <b><i>sourceObjectType</i></b> -- Matches Associations whose sourceObject
 * attribute references a RegistryObject whose objectType attribute matches the
 * id of the ClassificationNode where rim:ClassificationNode/@path matches
 * specified value
 * <p>
 * · <b><i>targetObjectId</i></b> -- Matches
 * rim:/RegistryObject[@xsi:type="rim:AssociationType"]/@targetObject.<br>
 * Allows use of “%” wildcard character to match multiple characters.<br>
 * Allows use of “?” wildcard character to match a single character.<br>
 * <p>
 * · <b><i>targetObjectType</i></b> -- Matches Associations whose targetObject
 * attribute references a RegistryObject whose objectType attribute matches the
 * id of the ClassificationNode where rim:ClassificationNode/@path matches
 * specified value
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 2/13/2012    #184       bphillip     Initial creation
 * 
 * </pre>
 * 
 * @author bphillip
 * @version 1.0
 */
public class FindAssociations extends CanonicalEbxmlQuery {

    public static final String QUERY_DEFINITION = QUERY_CANONICAL_PREFIX
            + "FindAssociations";

    /** The valid query parameter for this query **/
    private static final List<String> QUERY_PARAMETERS = new ArrayList<String>();
    static {
        QUERY_PARAMETERS.add(QueryConstants.ASSOCIATION_TYPE);
        QUERY_PARAMETERS.add(QueryConstants.MATCH_ANY);
        QUERY_PARAMETERS.add(QueryConstants.SOURCE_OBJECT_ID);
        QUERY_PARAMETERS.add(QueryConstants.SOURCE_OBJECT_TYPE);
        QUERY_PARAMETERS.add(QueryConstants.TARGET_OBJECT_ID);
        QUERY_PARAMETERS.add(QueryConstants.TARGET_OBJECT_TYPE);
    }

    private String getTypeClause(String associationType)
            throws EbxmlRegistryException {
        ClassificationNodeType node = new ClassificationNodeDao()
                .getByPath(associationType);
        if (node == null) {
            throw new EbxmlRegistryException(
                    "Unknown association type specified in "
                            + this.getQueryDefinition());
        }
        associationType = node.getId();
        return " association.type = '" + associationType + "' ";

    }

    @SuppressWarnings("unchecked")
    @Override
    protected List<AssociationType> query(QueryType queryType,
            QueryResponse queryResponse) throws EbxmlRegistryException {
        QueryParameters parameters = this.getParameterMap(queryType.getSlot(),
                queryResponse);
        String associationType = parameters
                .getFirstParameter(QueryConstants.ASSOCIATION_TYPE);
        String sourceObjectId = parameters
                .getFirstParameter(QueryConstants.SOURCE_OBJECT_ID);
        String sourceObjectType = parameters
                .getFirstParameter(QueryConstants.SOURCE_OBJECT_TYPE);
        String targetObjectId = parameters
                .getFirstParameter(QueryConstants.TARGET_OBJECT_ID);
        String targetObjectType = parameters
                .getFirstParameter(QueryConstants.TARGET_OBJECT_TYPE);
        String conjunction = HqlQueryUtil.AND;
        if (parameters.containsParameter(QueryConstants.MATCH_ANY)) {
            if (((Boolean) parameters
                    .getFirstParameter(QueryConstants.MATCH_ANY))
                    .booleanValue()) {
                conjunction = HqlQueryUtil.OR;
            }
        }
        List<String> clauses = new ArrayList<String>();
        if (associationType != null) {
            clauses.add(getTypeClause(associationType));
        }
        if (sourceObjectId != null) {
            clauses.add(" association.sourceObject like '" + sourceObjectId
                    + "'");
        }
        if (targetObjectId != null) {
            clauses.add(" association.targetObject like '" + targetObjectId
                    + "'");
        }
        if (sourceObjectType != null) {
            List<String> ids = registryObjectDao
                    .executeHQLQuery("select id from RegistryObjectType obj where obj.objectType = '"
                            + targetObjectType + "'");
            if (ids.isEmpty()) {
                return Collections.emptyList();
            } else {
                StringBuilder clause = new StringBuilder();
                clause.append(" association.sourceObject in (");
                for (int i = 0; i < ids.size(); i++) {
                    clause.append("'" + ids.get(i) + "'");
                    if (i != ids.size() - 1) {
                        clause.append(",");
                    }
                }
                clause.append(") ");
                clauses.add(clause.toString());
            }
        }
        if (targetObjectType != null) {
            List<String> ids = registryObjectDao
                    .executeHQLQuery("select id from RegistryObjectType obj where obj.objectType = '"
                            + targetObjectType + "'");
            if (ids.isEmpty()) {
                return Collections.emptyList();
            } else {
                StringBuilder clause = new StringBuilder();
                clause.append(" association.targetObject in (");
                for (int i = 0; i < ids.size(); i++) {
                    clause.append("'" + ids.get(i) + "'");
                    if (i != ids.size() - 1) {
                        clause.append(",");
                    }
                }
                clause.append(") ");
                clauses.add(clause.toString());
            }
        }

        StringBuilder query = new StringBuilder();
        if (clauses.isEmpty()) {
            query.append("from AssociationType");
        } else {
            query.append("select association from AssociationType association where ");
            for (int i = 0; i < clauses.size(); i++) {
                query.append(clauses.get(i));
                if (i != clauses.size() - 1) {
                    query.append(conjunction);
                }
            }
        }
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
