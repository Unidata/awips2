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
import java.util.Collections;
import java.util.Comparator;
import java.util.List;

import javax.jws.WebMethod;
import javax.jws.WebParam;
import javax.jws.WebResult;
import javax.xml.datatype.XMLGregorianCalendar;

import oasis.names.tc.ebxml.regrep.wsdl.registry.services.v4.MsgRegistryException;
import oasis.names.tc.ebxml.regrep.xsd.query.v4.QueryRequest;
import oasis.names.tc.ebxml.regrep.xsd.query.v4.QueryResponse;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.AuditableEventType;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.DateTimeValueType;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.QueryType;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.RegistryObjectType;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.SlotType;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.StringValueType;

import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

import com.raytheon.uf.common.registry.EbxmlNamespaces;
import com.raytheon.uf.common.registry.constants.CanonicalQueryTypes;
import com.raytheon.uf.edex.registry.ebxml.dao.RegistryObjectDao;
import com.raytheon.uf.edex.registry.ebxml.services.query.QueryConstants;

/**
 * 
 * <p>
 * <b>Parameter Summary:</b> <br>
 * · <b><i>endTime</i></b> -- Specifies the end of the time interval (inclusive)
 * for rim:/RegistryObject[@xsi:type="rim:AuditableEventType"]/@timestamp value
 * <p>
 * · <b><i>id</i></b> -- Matches rim:/RegistryObject/@id.
 * <p>
 * <b><i>startTime</i></b> -- Specifies the end of the time interval (inclusive)
 * for rim:/RegistryObject[@xsi:type="rim:AuditableEventType"]/@timestamp value
 * <p>
 * 
 * <pre>
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jan 18, 2012            bphillip    Initial creation 
 * 3/18/2013    1802       bphillip    Modified to use transaction boundaries and spring dao injection
 * 4/9/2013     1802       bphillip    Changed abstract method signature, modified return
 *                                     processing, and changed static variables
 * 10/8/2013    1682       bphillip    Refactored querying
 * 
 * </pre>
 * 
 * @author bphillip
 * @version 1.0
 */
public class GetAuditTrailByLid extends RegistryQueryPlugin {

    /** Data access object for accessing registry objects */
    private RegistryObjectDao registryObjectDao;

    /** GetAuditTrailById query object */
    private GetAuditTrailById getAuditTrailById;

    @Override
    @WebMethod(action = EXECUTE_QUERY_ACTION)
    @WebResult(name = "QueryResponse", targetNamespace = EbxmlNamespaces.QUERY_URI, partName = "partQueryResponse")
    @Transactional(propagation = Propagation.MANDATORY, readOnly = true)
    public QueryResponse executeQuery(
            @WebParam(name = "QueryRequest", targetNamespace = EbxmlNamespaces.QUERY_URI, partName = "partQueryRequest") QueryRequest queryRequest)
            throws MsgRegistryException {
        QueryType queryType = queryRequest.getQuery();
        List<AuditableEventType> retVal = new ArrayList<AuditableEventType>();

        String lid = queryType.getSlotValue(QueryConstants.LID);
        XMLGregorianCalendar startTime = queryType
                .getSlotValue(QueryConstants.START_TIME);
        XMLGregorianCalendar endTime = queryType
                .getSlotValue(QueryConstants.END_TIME);

        List<String> ids = registryObjectDao.executeHQLQuery(
                "SELECT obj.id FROM RegistryObjectType obj where obj.lid=:lid",
                "lid", lid);

        for (String id : ids) {
            QueryType byIdQuery = new QueryType(
                    CanonicalQueryTypes.GET_AUDIT_TRAIL_BY_ID);

            byIdQuery.getSlot().add(
                    new SlotType(QueryConstants.ID, new StringValueType(id)));
            if (startTime != null) {
                byIdQuery.getSlot().add(
                        new SlotType(QueryConstants.START_TIME,
                                new DateTimeValueType(startTime)));
            }
            if (endTime != null) {
                byIdQuery.getSlot().add(
                        new SlotType(QueryConstants.END_TIME,
                                new DateTimeValueType(endTime)));
            }
            QueryRequest getByIdRequest = new QueryRequest();
            getByIdRequest.setQuery(byIdQuery);
            QueryResponse getByIdResponse = getAuditTrailById
                    .executeQuery(getByIdRequest);
            for (RegistryObjectType obj : getByIdResponse.getRegistryObjects()) {
                retVal.add((AuditableEventType) obj);
            }
        }
        Collections.sort(retVal, new Comparator<AuditableEventType>() {
            @Override
            public int compare(AuditableEventType o1, AuditableEventType o2) {
                // Did the comparison backward so we get a descending list
                // instead of ascending
                return o2.getTimestamp().compare(o1.getTimestamp());
            }
        });
        return createResponse(retVal);
    }

    public void setGetAuditTrailById(GetAuditTrailById getAuditTrailById) {
        this.getAuditTrailById = getAuditTrailById;
    }

    public void setRegistryObjectDao(RegistryObjectDao registryObjectDao) {
        this.registryObjectDao = registryObjectDao;
    }

}
