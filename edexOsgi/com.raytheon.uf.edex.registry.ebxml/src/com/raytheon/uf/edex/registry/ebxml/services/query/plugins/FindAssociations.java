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
package com.raytheon.uf.edex.registry.ebxml.services.query.plugins;

import java.util.ArrayList;
import java.util.List;

import javax.jws.WebMethod;
import javax.jws.WebParam;
import javax.jws.WebResult;

import oasis.names.tc.ebxml.regrep.wsdl.registry.services.v4.MsgRegistryException;
import oasis.names.tc.ebxml.regrep.xsd.query.v4.QueryRequest;
import oasis.names.tc.ebxml.regrep.xsd.query.v4.QueryResponse;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.AssociationType;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.QueryType;

import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

import com.raytheon.uf.common.registry.EbxmlNamespaces;
import com.raytheon.uf.common.util.CollectionUtil;
import com.raytheon.uf.edex.registry.ebxml.dao.AssociationDao;
import com.raytheon.uf.edex.registry.ebxml.dao.RegistryObjectDao;
import com.raytheon.uf.edex.registry.ebxml.services.query.QueryConstants;
import com.raytheon.uf.edex.registry.ebxml.services.query.RegistryQueryUtil;

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
 * 3/18/2013    1802       bphillip    Modified to use transaction boundaries and spring dao injection
 * 4/9/2013     1802       bphillip     Changed abstract method signature, modified return processing, and changed static variables
 * 8/1/2013     1693       bphilip     Fixed minor typo
 * 10/8/2013    1682       bphillip    Refactored querying
 * 
 * </pre>
 * 
 * @author bphillip
 * @version 1.0
 */
public class FindAssociations extends RegistryQueryPlugin {

    /** Data access object for accessing registry objects */
    protected RegistryObjectDao registryObjectDao;

    /** Data access object for associations */
    private AssociationDao associationDao;

    @Override
    @WebMethod(action = EXECUTE_QUERY_ACTION)
    @WebResult(name = "QueryResponse", targetNamespace = EbxmlNamespaces.QUERY_URI, partName = "partQueryResponse")
    @Transactional(propagation = Propagation.MANDATORY, readOnly = true)
    public QueryResponse executeQuery(
            @WebParam(name = "QueryRequest", targetNamespace = EbxmlNamespaces.QUERY_URI, partName = "partQueryRequest") QueryRequest queryRequest)
            throws MsgRegistryException {
        QueryType queryType = queryRequest.getQuery();
        String associationType = getClassificationNodeIdFromPath((String) queryType
                .getSlotValue(QueryConstants.ASSOCIATION_TYPE));
        String sourceObjectId = queryType
                .getSlotValue(QueryConstants.SOURCE_OBJECT_ID);
        String sourceObjectType = getClassificationNodeIdFromPath((String) queryType
                .getSlotValue(QueryConstants.SOURCE_OBJECT_TYPE));
        String targetObjectId = queryType
                .getSlotValue(QueryConstants.TARGET_OBJECT_ID);
        String targetObjectType = getClassificationNodeIdFromPath((String) queryType
                .getSlotValue(QueryConstants.TARGET_OBJECT_TYPE));
        Boolean matchOnAnyParameter = queryType
                .getSlotValue(QueryConstants.MATCH_ANY);
        String conjunction = getConjunction(matchOnAnyParameter);

        List<String> clauses = new ArrayList<String>(6);
        StringBuilder query = new StringBuilder(1024);
        query.append("FROM AssociationType obj ");

        clauses.add(getAssociationTypeClause(associationType));
        clauses.add(getSourceIdClause(sourceObjectId));
        clauses.add(getTargetIdClause(targetObjectId));
        if (!CollectionUtil.removeNulls(clauses).isEmpty()) {
            query.append(" WHERE ");
            assembleClauses(query, conjunction, clauses);
        }

        List<AssociationType> associations = associationDao
                .executeHQLQuery(query.toString());

        List<AssociationType> toRemove = new ArrayList<AssociationType>();
        for (AssociationType association : associations) {
            if (sourceObjectType != null) {
                if (CollectionUtil
                        .isNullOrEmpty(registryObjectDao
                                .executeHQLQuery(
                                        "FROM RegistryObjectType obj WHERE obj.id=:id AND obj.objectType=:objectType",
                                        "id", association.getSourceObject(),
                                        "objectType", sourceObjectType))) {
                    toRemove.add(association);
                }
            }
            if (targetObjectType != null) {
                if (CollectionUtil
                        .isNullOrEmpty(registryObjectDao
                                .executeHQLQuery(
                                        "FROM RegistryObjectType obj WHERE obj.id=:id AND obj.objectType=:objectType",
                                        "id", association.getTargetObject(),
                                        "objectType", targetObjectType))) {
                    toRemove.add(association);
                }
            }
        }
        associations.removeAll(toRemove);
        return createResponse(associations);

    }

    /**
     * Gets the associationType clause
     * 
     * @param associationType
     *            The associationType used to generated the clause
     * @return null if associationType is null, else the associationType query
     *         clause
     */
    private String getAssociationTypeClause(String associationType) {
        return associationType == null ? null : " obj.type = '"
                + associationType + "' ";
    }

    /**
     * Gets the sourceId clause
     * 
     * @param sourceId
     *            The sourceId used to generate the clause
     * @return null if the sourceId is null, else the sourceId query clause
     */
    private String getSourceIdClause(String sourceId) {
        return sourceId == null ? null : " obj.sourceObject like '"
                + RegistryQueryUtil.substituteWildcardCharacters(sourceId)
                + "' ";
    }

    /**
     * Gets the targetId clause
     * 
     * @param targetId
     *            The targetId used to generated the clause
     * @return null if the targetId is null, else the targetId query clause
     */
    private String getTargetIdClause(String targetId) {
        return targetId == null ? null : " obj.targetObject like '"
                + RegistryQueryUtil.substituteWildcardCharacters(targetId)
                + "' ";
    }

    public void setAssociationDao(AssociationDao associationDao) {
        this.associationDao = associationDao;
    }

    public void setRegistryObjectDao(RegistryObjectDao registryObjectDao) {
        this.registryObjectDao = registryObjectDao;
    }

}
