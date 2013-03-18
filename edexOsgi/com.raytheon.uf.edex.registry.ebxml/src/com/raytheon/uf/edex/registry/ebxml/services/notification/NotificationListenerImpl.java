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
package com.raytheon.uf.edex.registry.ebxml.services.notification;

import oasis.names.tc.ebxml.regrep.wsdl.registry.services.v4.NotificationListener;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.NotificationType;

import org.springframework.transaction.annotation.Transactional;

import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;

/**
 * Implementation of the ebxml notificationListenerImpl.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 3/18/2013    1082       bphillip    Initial creation
 * 
 * </pre>
 * 
 * @author bphillip
 * @version 1.0
 */
@Transactional
public class NotificationListenerImpl implements NotificationListener {

    /** The logger */
    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(NotificationListenerImpl.class);

    @Override
    public void onNotification(NotificationType notification) {
        statusHandler.info("Received Notification: [" + notification.getId()
                + "]");
    }

}
