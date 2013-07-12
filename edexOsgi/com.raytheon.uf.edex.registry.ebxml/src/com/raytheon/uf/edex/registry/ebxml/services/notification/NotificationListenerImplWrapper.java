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

import javax.jws.Oneway;
import javax.jws.WebMethod;
import javax.jws.WebParam;

import oasis.names.tc.ebxml.regrep.wsdl.registry.services.v4.NotificationListener;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.NotificationType;

import org.springframework.transaction.annotation.Transactional;

/**
 * 
 * Wrapper for the notificationlistener service to be used with the SOAP
 * interface.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#     Engineer    Description
 * ------------ ----------  ----------- --------------------------
 * 7/11/2013    1707        bphillip    Initial implementation
 * </pre>
 * 
 * @author bphillip
 * @version 1
 */
@Transactional
public class NotificationListenerImplWrapper implements NotificationListener {

    private NotificationListenerImpl notificationListener;

    public NotificationListenerImplWrapper() {

    }

    public NotificationListenerImplWrapper(
            NotificationListenerImpl notificationListener) {
        this.notificationListener = notificationListener;
    }

    @Override
    @WebMethod(action = "urn:oasis:names:tc:ebxml-regrep:wsdl:NotificationListener:bindings:4.0:NotificationListener:onNotification")
    @Oneway
    public void onNotification(
            @WebParam(name = "Notification", targetNamespace = "urn:oasis:names:tc:ebxml-regrep:xsd:rim:4.0", partName = "Notification") NotificationType notification) {
        notificationListener.onNotification(notification);
    }

}
