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
import java.util.Set;

import javax.jws.WebMethod;
import javax.jws.WebParam;
import javax.jws.WebResult;

import oasis.names.tc.ebxml.regrep.wsdl.registry.services.v4.MsgRegistryException;
import oasis.names.tc.ebxml.regrep.xsd.query.v4.QueryRequest;
import oasis.names.tc.ebxml.regrep.xsd.query.v4.QueryResponse;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.ClassificationType;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.QueryType;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.RegistryObjectType;

import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

import com.raytheon.uf.common.registry.EbxmlNamespaces;
import com.raytheon.uf.common.util.CollectionUtil;
import com.raytheon.uf.edex.registry.ebxml.dao.RegistryObjectDao;
import com.raytheon.uf.edex.registry.ebxml.services.query.QueryConstants;

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
 * 3/18/2013    1802       bphillip    Modified to use transaction boundaries and spring dao injection
 * 4/9/2013     1802       bphillip     Changed abstract method signature, modified return processing, and changed static variables
 * 8/1/2013     1693       bphillip    Fixed minor typo
 * 10/8/2013    1682       bphillip    Refactored querying
 * 
 * 
 * </pre>
 * 
 * @author bphillip
 * @version 1.0
 */
public class BasicQuery extends RegistryQueryPlugin {

    /** Data Access object for registry objects */
    private RegistryObjectDao registryObjectDao;

    @Override
    @WebMethod(action = EXECUTE_QUERY_ACTION)
    @WebResult(name = "QueryResponse", targetNamespace = EbxmlNamespaces.QUERY_URI, partName = "partQueryResponse")
    @Transactional(propagation = Propagation.MANDATORY, readOnly = true)
    public QueryResponse executeQuery(
            @WebParam(name = "QueryRequest", targetNamespace = EbxmlNamespaces.QUERY_URI, partName = "partQueryRequest") QueryRequest queryRequest)
            throws MsgRegistryException {
        QueryType queryType = queryRequest.getQuery();
        List<String> classifications = queryType
                .getSlotValueAsList(QueryConstants.CLASSIFICATIONS);
        List<String> classificationIds = getClassificationNodeIdFromPath(classifications);

        String description = queryType.getSlotValue(QueryConstants.DESCRIPTION);
        String name = queryType.getSlotValue(QueryConstants.NAME);
        Boolean matchOnAnyParameter = queryType
                .getSlotValue(QueryConstants.MATCH_ANY);
        String objectType = getClassificationNodeIdFromPath((String) queryType
                .getSlotValue(QueryConstants.OBJECT_TYPE));
        String owner = queryType.getSlotValue(QueryConstants.OWNER);
        String status = getClassificationNodeIdFromPath((String) queryType
                .getSlotValue(QueryConstants.STATUS));

        List<String> clauses = new ArrayList<String>();
        StringBuilder query = new StringBuilder(512);
        query.append("SELECT distinct(obj) FROM RegistryObjectType obj ");
        if (name != null) {
            query.append(" INNER JOIN obj.name.localizedString as nameStrings ");
        }
        if (description != null) {
            query.append(" INNER JOIN obj.description.localizedString as descriptionStrings ");
        }
        if (!CollectionUtil.isNullOrEmpty(classificationIds)) {
            query.append(" INNER JOIN obj.classification as classification ");
        }
        query.append(" WHERE ");

        String conjunction = getConjunction(matchOnAnyParameter);
        clauses.add(getClassficiationsClause(classificationIds));
        clauses.add(getDescriptionClause(description));
        clauses.add(getNameClause(name));
        clauses.add(getObjectTypeClause(objectType));
        clauses.add(getOwnerClause(owner));
        clauses.add(getStatusClause(status));
        assembleClauses(query, conjunction, clauses);

        List<RegistryObjectType> result = registryObjectDao
                .executeHQLQuery(query.toString());
        if (!CollectionUtil.isNullOrEmpty(classificationIds)) {
            for (int i = 0; i < result.size(); i++) {
                if (!containsAllClassifications(result.get(i),
                        classificationIds)) {
                    result.remove(i--);
                }
            }
        }
        return createResponse(CollectionUtil.removeNulls(result));
    }

    /**
     * Checks if the given registry object contains the referenced
     * classifications
     * 
     * @param obj
     *            The object to check
     * @param classificationIds
     *            The ids of the classifications to check
     * @return True if the object contains the given classifications, else false
     */
    private boolean containsAllClassifications(RegistryObjectType obj,
            List<String> classificationIds) {
        Set<ClassificationType> classifications = obj.getClassification();
        if (!CollectionUtil.isNullOrEmpty(classifications)) {
            List<String> ids = new ArrayList<String>(classifications.size());
            for (ClassificationType cl : classifications) {
                ids.add(cl.getClassificationNode());
            }
            return ids.containsAll(classificationIds);
        }
        return false;
    }

    /**
     * Generates the classification HQL clause for the given set of ids
     * 
     * @param ids
     *            The ids used to generated the HQL classifications clause
     * @return The classifications query clause
     */
    private String getClassficiationsClause(List<String> ids) {
        if (CollectionUtil.isNullOrEmpty(ids)) {
            return null;
        }
        StringBuilder builder = new StringBuilder(150 * ids.size());
        for (int i = 0; i < ids.size(); i++) {
            builder.append(" classification.classificationNode='")
                    .append(ids.get(i)).append("' ");
            if (i != ids.size() - 1) {
                builder.append("OR ");
            }
        }
        return builder.toString();
    }

    /**
     * Gets the description clause
     * 
     * @param description
     *            The description to generate the query clause for
     * @return null if the provided description is null, else the description
     *         query clause
     */
    private String getDescriptionClause(String description) {
        return description == null ? null : " descriptionStrings.value = '"
                + description + "' ";
    }

    /**
     * Gets the name clause
     * 
     * @param name
     *            The name to generate the query clause for
     * @return null if the provided name is null, else the name query clause
     */
    private String getNameClause(String name) {
        return name == null ? null : " nameStrings.value = '" + name + "' ";
    }

    /**
     * Gets the objectType clause
     * 
     * @param objectType
     *            The object type used to generate the query clause
     * @return null if the provided objectType is null, else the objectType
     *         query clause
     */
    private String getObjectTypeClause(String objectType) {
        return objectType == null ? null : " obj.objectType = '" + objectType
                + "' ";
    }

    /**
     * Gets the owner clause
     * 
     * @param owner
     *            The owner used to generate the query clause
     * @return null if the provided owner is null, else the owner clause
     */
    private String getOwnerClause(String owner) {
        return owner == null ? null : " obj.owner = '" + owner + "' ";
    }

    /**
     * Gets the status clause
     * 
     * @param status
     *            The status used to generate the status clause
     * @return null if the provided statis is null, else the status clause
     */
    private String getStatusClause(String status) {
        return status == null ? null : " obj.status = '" + status + "' ";
    }

    public void setRegistryObjectDao(RegistryObjectDao registryObjectDao) {
        this.registryObjectDao = registryObjectDao;
    }

}
