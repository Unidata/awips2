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

import javax.xml.datatype.XMLGregorianCalendar;

import oasis.names.tc.ebxml.regrep.wsdl.registry.services.v4.MsgRegistryException;
import oasis.names.tc.ebxml.regrep.xsd.query.v4.QueryResponse;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.NotificationType;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.QueryType;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.SubscriptionType;

import org.springframework.transaction.annotation.Transactional;

import com.raytheon.uf.common.registry.constants.CanonicalQueryTypes;
import com.raytheon.uf.edex.registry.ebxml.dao.SubscriptionDao;
import com.raytheon.uf.edex.registry.ebxml.exception.EbxmlRegistryException;
import com.raytheon.uf.edex.registry.ebxml.services.notification.NotificationDestination;
import com.raytheon.uf.edex.registry.ebxml.services.notification.RegistrySubscriptionManager;
import com.raytheon.uf.edex.registry.ebxml.services.query.QueryConstants;
import com.raytheon.uf.edex.registry.ebxml.services.query.QueryParameters;
import com.raytheon.uf.edex.registry.ebxml.services.query.types.CanonicalEbxmlQuery;
import com.raytheon.uf.edex.registry.ebxml.util.EbxmlObjectUtil;

/**
 * TODO Add Description
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
 * 
 * </pre>
 * 
 * @author bphillip
 * @version 1.0
 */
@Transactional
public class GetNotification extends CanonicalEbxmlQuery {

    /** The list of valid parameters for this query */
    private static final List<String> QUERY_PARAMETERS = new ArrayList<String>();

    /* Initializes the list of parameters */
    static {
        QUERY_PARAMETERS.add(QueryConstants.SUBSCRIPTION_ID);
        QUERY_PARAMETERS.add(QueryConstants.START_TIME);
    }

    /** The subscription manager */
    private RegistrySubscriptionManager subscriptionManager;

    /** Data access object for accessing registry subscriptions */
    private SubscriptionDao subscriptionDao;

    @Override
    protected void query(QueryType queryType, QueryResponse queryResponse,
            String client) throws EbxmlRegistryException {
        QueryParameters parameters = getParameterMap(queryType.getSlot(),
                queryResponse);
        // The client did not specify the required parameter
        if (parameters.isEmpty()
                || !parameters
                        .containsParameter(QueryConstants.SUBSCRIPTION_ID)) {
            throw new EbxmlRegistryException("Canonical query ["
                    + this.getQueryDefinition()
                    + "] is missing required parameter ["
                    + QUERY_PARAMETERS.get(0) + "]");
        }

        String subscriptionId = parameters
                .getFirstParameter(QueryConstants.SUBSCRIPTION_ID);
        XMLGregorianCalendar startTime = parameters
                .getFirstParameter(QueryConstants.START_TIME);

        SubscriptionType subscription = subscriptionDao.getById(subscriptionId);
        if (subscription == null) {
            throw new EbxmlRegistryException("No subscription with id: "
                    + subscriptionId + " present in registry");
        }

        List<NotificationDestination> destinations = subscriptionManager
                .getNotificationDestinations(subscription);
        if (destinations.isEmpty() && startTime == null) {
            throw new EbxmlRegistryException(
                    "Subscription does not define any push delivery addresses. Start time must be defined.");
        }

        if (startTime == null) {
            startTime = subscription
                    .getSlotValue(EbxmlObjectUtil.SUBSCRIPTION_LAST_RUN_TIME_SLOT_NAME);
            if (startTime == null) {
                startTime = EbxmlObjectUtil.getTimeAsXMLGregorianCalendar(0);

            }
        }

        try {
            NotificationType notification = subscriptionManager
                    .getOnDemandNotification("http://" + client,
                            subscriptionId, startTime);
            List<Object> retVal = new ArrayList<Object>();
            retVal.add(notification);
            setResponsePayload(queryResponse, retVal);
        } catch (MsgRegistryException e) {
            throw new EbxmlRegistryException(
                    "Error getting on demand notification", e.getCause());
        }
    }

    @Override
    protected List<String> getValidParameters() {
        return QUERY_PARAMETERS;
    }

    @Override
    public String getQueryDefinition() {
        return CanonicalQueryTypes.GET_NOTIFICATION;
    }

    public void setSubscriptionManager(
            RegistrySubscriptionManager subscriptionManager) {
        this.subscriptionManager = subscriptionManager;
    }

    public void setSubscriptionDao(SubscriptionDao subscriptionDao) {
        this.subscriptionDao = subscriptionDao;
    }

}
