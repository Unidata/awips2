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

import javax.jws.WebMethod;
import javax.jws.WebParam;
import javax.jws.WebResult;
import javax.xml.datatype.DatatypeConfigurationException;
import javax.xml.datatype.DatatypeFactory;
import javax.xml.datatype.XMLGregorianCalendar;

import oasis.names.tc.ebxml.regrep.wsdl.registry.services.v4.MsgRegistryException;
import oasis.names.tc.ebxml.regrep.xsd.query.v4.QueryRequest;
import oasis.names.tc.ebxml.regrep.xsd.query.v4.QueryResponse;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.QueryType;

import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

import com.raytheon.uf.common.registry.EbxmlNamespaces;
import com.raytheon.uf.common.registry.constants.CanonicalQueryTypes;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.time.util.TimeUtil;
import com.raytheon.uf.edex.registry.ebxml.dao.AuditableEventTypeDao;
import com.raytheon.uf.edex.registry.ebxml.exception.EbxmlRegistryException;
import com.raytheon.uf.edex.registry.ebxml.services.query.QueryConstants;
import com.raytheon.uf.edex.registry.ebxml.util.EbxmlExceptionUtil;
import com.raytheon.uf.edex.registry.ebxml.util.EbxmlObjectUtil;

/**
 * The canonical query GetAuditTrailByTimeInterval allows clients to get all
 * changes to all objects in the server within a specified time interval. This
 * query may be used to keep a client periodically synchronized with changes in
 * the server.
 * <p>
 * The server MUST return a set of AuditableEvents whose timestamp attribute is
 * within the time interval specified by startTime and endTime parameters. The
 * set is sorted by the timestamp attribute value in descending order (latest
 * first)
 * <p>
 * The server MUST only include AuditableEvents whose timestamp is >= startTime
 * parameter
 * 
 * value
 * <p>
 * The server MUST only include AuditableEvents whose timestamp is <= endTime
 * parameter value
 * 
 * 
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
 * 10/8/2013    1682       bphillip    Refactored querying
 * 11/13/2013   1686       bphillip    Fixed the arguments of this query
 * 
 * </pre>
 * 
 * @author bphillip
 * @version 1.0
 */
public class GetAuditTrailByTimeInterval extends RegistryQueryPlugin {

    /** The logger */
    protected static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(GetAuditTrailByTimeInterval.class);

    /** Query to get all AuditableEvents within the given timestamp boundaries */
    private static final String TIME_INTERVAL_QUERY = ""
            + "FROM AuditableEventType event "
            + "WHERE event.timestamp >= :startTime "
            + "AND event.timestamp <= :endTime "
            + "ORDER BY event.timestamp desc";

    private AuditableEventTypeDao auditableEventDao;

    @Override
    @WebMethod(action = EXECUTE_QUERY_ACTION)
    @WebResult(name = "QueryResponse", targetNamespace = EbxmlNamespaces.QUERY_URI, partName = "partQueryResponse")
    @Transactional(propagation = Propagation.MANDATORY, readOnly = true)
    public QueryResponse executeQuery(
            @WebParam(name = "QueryRequest", targetNamespace = EbxmlNamespaces.QUERY_URI, partName = "partQueryRequest") QueryRequest queryRequest)
            throws MsgRegistryException {
        QueryType queryType = queryRequest.getQuery();

        XMLGregorianCalendar startTime = null;
        XMLGregorianCalendar endTime = null;
        try {
            startTime = DatatypeFactory.newInstance().newXMLGregorianCalendar(
                    (String) queryType.getSlotValue(QueryConstants.START_TIME));
        } catch (DatatypeConfigurationException e) {
            throw EbxmlExceptionUtil.createMsgRegistryException(
                    "Error parsing start time", e);
        }

        try {
            endTime = DatatypeFactory.newInstance().newXMLGregorianCalendar(
                    (String) queryType.getSlotValue(QueryConstants.END_TIME));
        } catch (DatatypeConfigurationException e) {
            throw EbxmlExceptionUtil.createMsgRegistryException(
                    "Error parsing end time", e);
        }

        // Start time defaults to current Time
        long currentTime = TimeUtil.currentTimeMillis();
        if (startTime == null) {
            try {
                startTime = EbxmlObjectUtil
                        .getTimeAsXMLGregorianCalendar(currentTime);
            } catch (EbxmlRegistryException e) {
                throw EbxmlExceptionUtil.createMsgRegistryException(
                        "Error assigning start time", e);
            }
        }

        // End time defaults to 5 minutes before current time
        if (endTime == null) {
            try {
                endTime = EbxmlObjectUtil
                        .getTimeAsXMLGregorianCalendar(currentTime
                                - TimeUtil.MILLIS_PER_MINUTE * 5);
            } catch (EbxmlRegistryException e) {
                throw EbxmlExceptionUtil.createMsgRegistryException(
                        "Error assigning end time", e);
            }
        }
        return createResponse(auditableEventDao.executeHQLQuery(
                TIME_INTERVAL_QUERY, QueryConstants.START_TIME, startTime,
                QueryConstants.END_TIME, endTime));
    }

    @Override
    public String getQueryDefinition() {
        return CanonicalQueryTypes.GET_AUDIT_TRAIL_BY_TIME_INTERVAL;
    }

    public void setAuditableEventDao(AuditableEventTypeDao auditableEventDao) {
        this.auditableEventDao = auditableEventDao;
    }

}
