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

import java.util.Arrays;
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
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.ObjectRefType;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.QueryType;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.SubscriptionType;

import com.raytheon.uf.common.registry.EbxmlNamespaces;
import com.raytheon.uf.common.registry.constants.CanonicalQueryTypes;
import com.raytheon.uf.edex.registry.ebxml.dao.SubscriptionDao;
import com.raytheon.uf.edex.registry.ebxml.exception.EbxmlRegistryException;
import com.raytheon.uf.edex.registry.ebxml.services.notification.NotificationDestination;
import com.raytheon.uf.edex.registry.ebxml.services.notification.RegistryNotificationManager;
import com.raytheon.uf.edex.registry.ebxml.services.notification.RegistrySubscriptionManager;
import com.raytheon.uf.edex.registry.ebxml.services.query.QueryConstants;
import com.raytheon.uf.edex.registry.ebxml.util.EbxmlExceptionUtil;
import com.raytheon.uf.edex.registry.ebxml.util.EbxmlObjectUtil;

/**
 * The canonical query GetNotification allows clients to “pull” any pending
 * Notification for a Subscription at a time of their choosing. This is defined
 * in detail under section titled “Pulling Notification on Demand”.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jan 18, 2012            bphillip     Initial creation
 * 3/18/2013    1802       bphillip    Modified to use transaction boundaries and spring dao injection
 * 4/9/2013     1802       bphillip    Changed abstract method signature, modified return processing, and changed static variables
 * Jun 24, 2013 2106       djohnson    Requires a transaction to be open, will not create one.
 * 10/8/2013    1682       bphillip    Refactored querying
 * 11/20/2013   2534       bphillip    Changed call to getNotificationDestinations which is not in a utility class
 * 01/21/2014   2613       bphillip    Modifications to account for changed method signatures in RegistryNotificationManager
 * 
 * </pre>
 * 
 * @author bphillip
 * @version 1.0
 */
public class GetNotification extends RegistryQueryPlugin {

    /** The subscription manager */
    private RegistrySubscriptionManager subscriptionManager;

    /** The notification manager */
    private RegistryNotificationManager notificationManager;

    /** Data access object for accessing registry subscriptions */
    private SubscriptionDao subscriptionDao;

    @Override
    @WebMethod(action = EXECUTE_QUERY_ACTION)
    @WebResult(name = "QueryResponse", targetNamespace = EbxmlNamespaces.QUERY_URI, partName = "partQueryResponse")
    public QueryResponse executeQuery(
            @WebParam(name = "QueryRequest", targetNamespace = EbxmlNamespaces.QUERY_URI, partName = "partQueryRequest") QueryRequest queryRequest)
            throws MsgRegistryException {
        QueryType queryType = queryRequest.getQuery();
        String subscriptionId = queryType
                .getSlotValue(QueryConstants.SUBSCRIPTION_ID);
        DateTimeValueType startTimeSlot = queryType
                .getSlotValue(QueryConstants.START_TIME);
        XMLGregorianCalendar startTime = null;

        SubscriptionType subscription = subscriptionDao.getById(subscriptionId);
        if (subscription == null) {
            throw EbxmlExceptionUtil.createQueryExceptionType(
                    "No subscription with id: " + subscriptionId
                            + " present in registry", "");
        }

        List<NotificationDestination> destinations;
        try {
            destinations = EbxmlObjectUtil
                    .getNotificationDestinations(subscription);
        } catch (EbxmlRegistryException e1) {
            throw EbxmlExceptionUtil.createMsgRegistryException(
                    "Error getting notification destinations", e1);
        }
        if (destinations.isEmpty() && startTimeSlot == null) {
            throw EbxmlExceptionUtil
                    .createQueryExceptionType(
                            "Subscription does not define any push delivery addresses. Start time must be defined.",
                            "");
        }

        if (startTimeSlot == null) {
            startTimeSlot = subscription
                    .getSlotValue(EbxmlObjectUtil.SUBSCRIPTION_LAST_RUN_TIME_SLOT_NAME);
            if (startTimeSlot == null) {
                try {
                    startTime = EbxmlObjectUtil
                            .getTimeAsXMLGregorianCalendar(0);
                } catch (EbxmlRegistryException e) {
                    throw EbxmlExceptionUtil.createMsgRegistryException(
                            "Error creating gregorian calendar", e);
                }

            }
        } else {
            startTime = startTimeSlot.getDateTimeValue();
        }

        List<ObjectRefType> objectsOfInterest = notificationManager
                .getObjectsOfInterest(subscription);
        List<AuditableEventType> eventsOfInterest = null;
        try {
            eventsOfInterest = notificationManager.getEventsOfInterest(
                    subscription, destinations.get(0).getDestination(),
                    startTime, null, objectsOfInterest);
        } catch (EbxmlRegistryException e1) {
            throw EbxmlExceptionUtil.createMsgRegistryException(
                    "Error getting events!", e1);
        }
        try {
            return createResponse(Arrays.asList(notificationManager
                    .getNotification(subscription, "Test Address",
                            objectsOfInterest, eventsOfInterest)));
        } catch (EbxmlRegistryException e) {
            throw EbxmlExceptionUtil.createMsgRegistryException(
                    "Error creating notification", e);
        }

    }

    @Override
    public String getQueryDefinition() {
        return CanonicalQueryTypes.GET_NOTIFICATION;
    }

    public void setSubscriptionManager(
            RegistrySubscriptionManager subscriptionManager) {
        this.subscriptionManager = subscriptionManager;
    }

    public void setNotificationManager(
            RegistryNotificationManager notificationManager) {
        this.notificationManager = notificationManager;
    }

    public void setSubscriptionDao(SubscriptionDao subscriptionDao) {
        this.subscriptionDao = subscriptionDao;
    }

}
