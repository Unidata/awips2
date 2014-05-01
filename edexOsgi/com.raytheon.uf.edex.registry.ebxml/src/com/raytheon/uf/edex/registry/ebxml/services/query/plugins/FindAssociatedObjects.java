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
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.RegistryObjectType;

import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

import com.raytheon.uf.common.registry.EbxmlNamespaces;
import com.raytheon.uf.common.util.CollectionUtil;
import com.raytheon.uf.edex.registry.ebxml.dao.RegistryObjectDao;
import com.raytheon.uf.edex.registry.ebxml.exception.EbxmlRegistryException;
import com.raytheon.uf.edex.registry.ebxml.services.query.QueryConstants;
import com.raytheon.uf.edex.registry.ebxml.util.EbxmlExceptionUtil;

/**
 * The canonical query FindAssociatedObjects allows clients to find
 * RegistryObjects that are associated with the specified RegistryObject and
 * match the specified criteria.
 * <p>
 * <b>Parameter Summary:</b> <br>
 * <b><i>associationType</i></b> -- Matches associated RegistryObjects of
 * Association's whose type attribute references a ClassificationNode where
 * rim:ClassificationNode/@path matches specified value
 * <p>
 * <b><i>matchOnAnyParameter</i></b> -- If true then use logical OR between
 * predicates for each parameter
 * <p>
 * <b><i>sourceObjectId</i></b> --Matches target RegistryObjects of Associations
 * where the source RegistryObject's id matches
 * rim:/RegistryObject[@xsi:type="rim:AssociationType"]/@sourceObject.<br>
 * Allows use of % wildcard character to match multiple characters.<br>
 * Allows use of ? wildcard character to match a single character.<br>
 * <p>
 * <b><i>sourceObjectType</i></b> -- Matches target RegistryObjects of
 * Associations whose sourceObject attribute references a RegistryObject whose
 * objectType attribute matches the id of the ClassificationNode where
 * rim:ClassificationNode/@path matches specified value
 * <p>
 * <b><i>targetObjectId</i></b> --
 * 
 * Matches source RegistryObjects of Associations where the target
 * RegistryObject's id matches
 * rim:/RegistryObject[@xsi:type="rim:AssociationType"]/@targetObject.<br>
 * Allows use of % wildcard character to match multiple characters.<br>
 * Allows use of ? wildcard character to match a single character.<br>
 * <p>
 * <b><i>targetObjectType</i></b> --
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
 * 3/18/2013    1802       bphillip     Modified to use transaction boundaries and spring dao injection
 * 4/9/2013     1802       bphillip     Changed abstract method signature, modified return processing, and changed static variables
 * Apr 23, 2013 1910       djohnson     Don't allow NPE on registry object list, remove non ANSI Javadoc.
 * 5/21/2013    2022       bphillip     Set return type on call to findAssociations
 * 10/8/2013    1682       bphillip    Refactored querying
 * 10/23/2013   1682       bphillip     Returns empty query result if no ids were found
 * 
 * </pre>
 * 
 * @author bphillip
 * @version 1.0
 */
public class FindAssociatedObjects extends RegistryQueryPlugin {

    /** Data access object for accessing registry objects */
    private RegistryObjectDao registryObjectDao;

    /** The FindAssociations query object */
    private FindAssociations findAssociations;

    @Override
    @WebMethod(action = EXECUTE_QUERY_ACTION)
    @WebResult(name = "QueryResponse", targetNamespace = EbxmlNamespaces.QUERY_URI, partName = "partQueryResponse")
    @Transactional(propagation = Propagation.MANDATORY, readOnly = true)
    public QueryResponse executeQuery(
            @WebParam(name = "QueryRequest", targetNamespace = EbxmlNamespaces.QUERY_URI, partName = "partQueryRequest") QueryRequest queryRequest)
            throws MsgRegistryException {
        QueryType queryType = queryRequest.getQuery();
        String sourceObjectId = queryType
                .getSlotValue(QueryConstants.SOURCE_OBJECT_ID);
        String sourceObjectType = queryType
                .getSlotValue(QueryConstants.SOURCE_OBJECT_TYPE);
        String targetObjectId = queryType
                .getSlotValue(QueryConstants.TARGET_OBJECT_ID);
        String targetObjectType = queryType
                .getSlotValue(QueryConstants.TARGET_OBJECT_TYPE);

        if (targetObjectId == null && sourceObjectId == null) {
            throw EbxmlExceptionUtil
                    .createQueryExceptionType(
                            "Either sourceObjectId or targetObjectId MUST be specified.  Neither were present in submitted query",
                            "");
        }

        if (sourceObjectId != null && targetObjectId != null) {
            throw EbxmlExceptionUtil
                    .createQueryExceptionType(
                            "Both sourceObjectId and targetObjectId MUST NOT be specified.",
                            "");
        }

        if (sourceObjectType != null && targetObjectType != null) {
            throw EbxmlExceptionUtil
                    .createQueryExceptionType(
                            "Both sourceObjectType and targetObjectType MUST NOT be specified.",
                            "");
        }

        /*
         * First, find the associations
         */

        QueryResponse findAssociationsResponse = findAssociations
                .executeQuery(queryRequest);

        List<String> ids = new ArrayList<String>(findAssociationsResponse
                .getRegistryObjects().size());
        /*
         * Then get the referenced objects based on the given parameters
         */
        for (RegistryObjectType obj : findAssociationsResponse
                .getRegistryObjects()) {

            if (sourceObjectId == null) {
                ids.add(((AssociationType) obj).getSourceObject());
            } else {
                ids.add(((AssociationType) obj).getTargetObject());
            }
        }

        try {
            if (CollectionUtil.isNullOrEmpty(ids)) {
                return new QueryResponse();
            }
            return createResponse(registryObjectDao.getById(ids));
        } catch (EbxmlRegistryException e) {
            throw EbxmlExceptionUtil.createMsgRegistryException(
                    "Error executing query", e);
        }
    }

    public void setRegistryObjectDao(RegistryObjectDao registryObjectDao) {
        this.registryObjectDao = registryObjectDao;
    }

    public void setFindAssociations(FindAssociations findAssociations) {
        this.findAssociations = findAssociations;
    }

}
