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
package com.raytheon.uf.edex.registry.ebxml.services.notification.listeners;

import oasis.names.tc.ebxml.regrep.wsdl.registry.services.v4.NotificationListener;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.NotificationType;

import com.raytheon.uf.common.registry.services.RegistrySOAPServices;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.edex.registry.ebxml.exception.EbxmlRegistryException;
import com.raytheon.uf.edex.registry.ebxml.services.notification.NotificationDestination;

/**
 * Notifies listeners via a web service.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Apr 16, 2013 1672       djohnson     Extracted from RegistryNotificationManager.
 * 
 * </pre>
 * 
 * @author djohnson
 * @version 1.0
 */

public class WebServiceNotificationListener implements NotificationListener {

    /** The log handler */
    private static final IUFStatusHandler statusHandler = UFStatus
            .getHandler(WebServiceNotificationListener.class);

    private final String destination;

    /**
     * @param destination
     */
    public WebServiceNotificationListener(NotificationDestination destination) {
        this.destination = destination.getDestination();
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void onNotification(NotificationType notification) {
        try {
            sendNotificationViaSoap(notification, destination);
        } catch (EbxmlRegistryException e) {
            throw new RuntimeException(
                    "Error sending subscription notification.", e);
        }
    }

    /**
     * Sends the notification via SOAP
     * 
     * @param notification
     *            The notification to send
     * @param serviceAddress
     *            The address to send the notification to
     * @throws EbxmlRegistryException
     *             If errors occur while sending the notification
     */
    protected void sendNotificationViaSoap(NotificationType notification,
            String serviceAddress) throws EbxmlRegistryException {
        statusHandler.info("Sending notification [" + notification.getId()
                + "]");
        RegistrySOAPServices.getNotificationListenerServiceForUrl(
                serviceAddress).onNotification(notification);
        statusHandler.info("Notification [" + notification.getId() + "] sent!");
    }
}
