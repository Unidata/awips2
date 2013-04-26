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
package com.raytheon.uf.edex.registry.ebxml.services;

import java.util.List;
import java.util.Map;

import oasis.names.tc.ebxml.regrep.xsd.lcm.v4.RemoveObjectsRequest;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.AuditableEventType;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.ObjectRefType;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.RegistryObjectType;
import oasis.names.tc.ebxml.regrep.xsd.rs.v4.RegistryRequestType;

import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.edex.registry.ebxml.dao.AuditableEventTypeDao;
import com.raytheon.uf.edex.registry.ebxml.exception.EbxmlRegistryException;

/**
 * Service to interact with {@link AuditableEventType} objects.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * May 02, 2013 1910       djohnson     Extracted subscription notification from the dao.
 * 
 * </pre>
 * 
 * @author djohnson
 * @version 1.0
 */

public class AuditableEventService {

    private static final IUFStatusHandler statusHandler = UFStatus
            .getHandler(AuditableEventService.class);

    private final AuditableEventTypeDao auditDao;

    private final IRegistrySubscriptionManager subscriptionManager;

    public AuditableEventService(AuditableEventTypeDao auditDao,
            IRegistrySubscriptionManager subscriptionManager) {
        this.auditDao = auditDao;
        this.subscriptionManager = subscriptionManager;
    }

    /**
     * Creates an auditable event from a registry request and registry objects
     * 
     * @param request
     *            The request that generated the events
     * @param actionMap
     *            The actions that occurred
     * @param currentTime
     *            The time the event occurred @ the event
     * @throws EbxmlRegistryException
     *             If errors occur while creating
     */
    public void createAuditableEventsFromObjects(RegistryRequestType request,
            Map<String, List<RegistryObjectType>> actionMap, long currentTime)
            throws EbxmlRegistryException {
        auditDao.createAuditableEventsFromObjects(request, actionMap,
                currentTime);
        notifySubscriptionManager();
    }

    /**
     * Creates an auditable event from a registry request and object references
     * 
     * @param request
     *            The request that generated the events
     * @param actionMap
     *            The actions that occurred
     * @param currentTime
     *            The time the event occurred @ the event
     * @throws EbxmlRegistryException
     *             If errors occur while creating
     */
    public void createAuditableEventsFromRefs(RemoveObjectsRequest request,
            Map<String, List<ObjectRefType>> actionMap, long currentTimeMillis)
            throws EbxmlRegistryException {
        auditDao.createAuditableEventsFromRefs(request, actionMap,
                currentTimeMillis);
        notifySubscriptionManager();
    }

    private void notifySubscriptionManager() {
        // Notify the subscription monitor that a new event has occurred
        try {
            subscriptionManager.processSubscriptions();
        } catch (Throwable t) {
            statusHandler
                    .error("Unexpected error ecountered while processing subscriptions!",
                            t);
        }
    }
}
