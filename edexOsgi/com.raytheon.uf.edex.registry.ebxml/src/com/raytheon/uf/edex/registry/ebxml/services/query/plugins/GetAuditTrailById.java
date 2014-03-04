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
import javax.xml.datatype.XMLGregorianCalendar;

import oasis.names.tc.ebxml.regrep.wsdl.registry.services.v4.MsgRegistryException;
import oasis.names.tc.ebxml.regrep.xsd.query.v4.QueryRequest;
import oasis.names.tc.ebxml.regrep.xsd.query.v4.QueryResponse;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.QueryType;

import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

import com.raytheon.uf.common.registry.EbxmlNamespaces;
import com.raytheon.uf.edex.registry.ebxml.dao.AuditableEventTypeDao;
import com.raytheon.uf.edex.registry.ebxml.services.query.QueryConstants;

/**
 * The canonical query GetAuditTrailByLid allows clients to get the change
 * history or audit trail for all RegistryObjects whose lid attribute value is
 * the same as the value of the lid parameter.
 * 
 * <p>
 * <b>Parameter Summary:</b> <br>
 * · <b><i>endTime</i></b> -- Specifies the end of the time interval (inclusive)
 * for rim:/RegistryObject[@xsi:type="rim:AuditableEventType"]/@timestamp value
 * <p>
 * · <b><i>lid</i></b> -- Matches rim:/RegistryObject/@lid.
 * <p>
 * <b><i>startTime</i></b> -- Specifies the end of the time interval (inclusive)
 * for rim:/RegistryObject[@xsi:type="rim:AuditableEventType"]/@timestamp value
 * <p>
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 2/15/2012    #184       bphillip     Initial creation
 * 3/18/2013    1802       bphillip    Modified to use transaction boundaries and spring dao injection
 * 4/9/2013     1802       bphillip     Changed abstract method signature, modified return processing, and changed static variables
 * 10/8/2013    1682       bphillip    Refactored querying
 * 
 * </pre>
 * 
 * @author bphillip
 * @version 1.0
 */
public class GetAuditTrailById extends RegistryQueryPlugin {

    /** Data Access Object for auditableEvents */
    private AuditableEventTypeDao auditableEventDao;

    /**
     * Query to find events referencing the id provided to the query
     */
    private static final String FIND_EVENTS_FOR_ID_QUERY = "select event from AuditableEventType as event "
            + "left outer join event.action as action "
            + "left outer join action.affectedObjects as AffectedObjects "
            + "left outer join action.affectedObjectRefs as AffectedObjectRefs "
            + "left outer join AffectedObjects.registryObject as RegistryObjects "
            + "left outer join AffectedObjectRefs.objectRef as ObjRefs "
            + "where (ObjRefs.id =:id OR RegistryObjects.id =:id) ";

    /**
     * The order clause to append to the query to ensure uniform ordering of
     * results
     */
    private static final String ORDER_BY_CLAUSE = " order by event.timestamp desc";

    @Override
    @WebMethod(action = EXECUTE_QUERY_ACTION)
    @WebResult(name = "QueryResponse", targetNamespace = EbxmlNamespaces.QUERY_URI, partName = "partQueryResponse")
    @Transactional(propagation = Propagation.MANDATORY, readOnly = true)
    public QueryResponse executeQuery(
            @WebParam(name = "QueryRequest", targetNamespace = EbxmlNamespaces.QUERY_URI, partName = "partQueryRequest") QueryRequest queryRequest)
            throws MsgRegistryException {
        QueryType queryType = queryRequest.getQuery();
        String id = queryType.getSlotValue(QueryConstants.ID);
        XMLGregorianCalendar startTime = queryType
                .getSlotValue(QueryConstants.START_TIME);
        XMLGregorianCalendar endTime = queryType
                .getSlotValue(QueryConstants.END_TIME);
        List<Object> parameters = new ArrayList<Object>(4);
        StringBuilder query = new StringBuilder(
                FIND_EVENTS_FOR_ID_QUERY.replaceAll(":id", "'" + id + "'"));
        if (startTime != null) {
            query.append(" AND event.timestamp >= :startTime ");
            parameters.add(QueryConstants.START_TIME);
            parameters.add(startTime);
        }
        if (endTime != null) {
            query.append(" AND event.timestamp <= :endTime ");
            parameters.add(QueryConstants.END_TIME);
            parameters.add(endTime);
        }
        query.append(ORDER_BY_CLAUSE);
        return createResponse(auditableEventDao.executeHQLQuery(
                query.toString(),
                parameters.toArray(new Object[parameters.size()])));
    }

    public void setAuditableEventDao(AuditableEventTypeDao auditableEventDao) {
        this.auditableEventDao = auditableEventDao;
    }
}
