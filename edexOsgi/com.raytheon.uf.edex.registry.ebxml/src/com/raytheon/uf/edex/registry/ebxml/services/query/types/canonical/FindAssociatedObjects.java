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
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.AssociationType;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.ClassificationNodeType;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.QueryType;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.RegistryObjectType;

import com.raytheon.uf.edex.registry.ebxml.dao.ClassificationNodeDao;
import com.raytheon.uf.edex.registry.ebxml.dao.HqlQueryUtil;
import com.raytheon.uf.edex.registry.ebxml.exception.EbxmlRegistryException;
import com.raytheon.uf.edex.registry.ebxml.services.query.QueryConstants;
import com.raytheon.uf.edex.registry.ebxml.services.query.QueryParameters;
import com.raytheon.uf.edex.registry.ebxml.services.query.types.CanonicalEbxmlQuery;

/**
 * The canonical query FindAssociatedObjects allows clients to find
 * RegistryObjects that are associated with the specified RegistryObject and
 * match the specified criteria.
 * <p>
 * <b>Parameter Summary:</b> <br>
 * · <b><i>associationType</i></b> -- Matches associated RegistryObjects of
 * Association's whose type attribute references a ClassificationNode where
 * rim:ClassificationNode/@path matches specified value
 * <p>
 * · <b><i>matchOnAnyParameter</i></b> -- If true then use logical OR between
 * predicates for each parameter
 * <p>
 * · <b><i>sourceObjectId</i></b> --Matches target RegistryObjects of
 * Associations where the source RegistryObject's id matches
 * rim:/RegistryObject[@xsi:type="rim:AssociationType"]/@sourceObject.<br>
 * Allows use of “%” wildcard character to match multiple characters.<br>
 * Allows use of “?” wildcard character to match a single character.<br>
 * <p>
 * · <b><i>sourceObjectType</i></b> -- Matches target RegistryObjects of
 * Associations whose sourceObject attribute references a RegistryObject whose
 * objectType attribute matches the id of the ClassificationNode where
 * rim:ClassificationNode/@path matches specified value
 * <p>
 * · <b><i>targetObjectId</i></b> --
 * 
 * Matches source RegistryObjects of Associations where the target
 * RegistryObject's id matches
 * rim:/RegistryObject[@xsi:type="rim:AssociationType"]/@targetObject.<br>
 * Allows use of “%” wildcard character to match multiple characters.<br>
 * Allows use of “?” wildcard character to match a single character.<br>
 * <p>
 * · <b><i>targetObjectType</i></b> --
 * 
 * Matches source RegistryObjects of Associations whose targetObject attribute
 * references a RegistryObject whose objectType attribute matches the id of the
 * ClassificationNode where rim:ClassificationNode/@path matches specified value
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
public class FindAssociatedObjects extends CanonicalEbxmlQuery {

    public static final String QUERY_DEFINITION = QUERY_CANONICAL_PREFIX
            + "FindAssociatedObjects";

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

    @Override
    protected List<RegistryObjectType> query(QueryType queryType,
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

        if (targetObjectId == null && sourceObjectId == null) {
            throw new EbxmlRegistryException(
                    "Either sourceObjectId or targetObjectId MUST be specified.  Neither were present in submitted query");
        }

        if (sourceObjectId != null && targetObjectId != null) {
            throw new EbxmlRegistryException(
                    "Both sourceObjectId and targetObjectId MUST NOT be specified.");
        }

        if (sourceObjectType != null && targetObjectType != null) {
            throw new EbxmlRegistryException(
                    "Both sourceObjectType and targetObjectType MUST NOT be specified.");
        }
        List<AssociationType> associations = new FindAssociations().query(
                queryType, queryResponse);
        List<String> ids = new ArrayList<String>();
        for (AssociationType association : associations) {
            if (sourceObjectId == null) {
                ids.add(association.getSourceObject());
            } else {
                ids.add(association.getTargetObject());
            }
        }
        return registryObjectDao.getById(ids);

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
        return " (obj.id in (select association.sourceObject from AssociationType association where association.type = '"
                + associationType
                + "') or obj.in (select association.targetObject from AssociationType association where association.type = '"
                + associationType + "'))";

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
