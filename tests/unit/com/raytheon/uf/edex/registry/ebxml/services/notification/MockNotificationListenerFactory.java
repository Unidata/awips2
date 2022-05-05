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

import static org.mockito.Mockito.mock;

import java.util.HashMap;
import java.util.Map;

import oasis.names.tc.ebxml.regrep.wsdl.registry.services.v4.NotificationListener;

import com.raytheon.uf.edex.registry.ebxml.services.soap.RegistrySOAPServices;
import com.raytheon.uf.edex.registry.ebxml.services.notification.listeners.WebServiceNotificationListener;

/**
 * Extends {@link NotificationListenerFactory} to create mocks and allow them to
 * be retrieved from tests.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Apr 17, 2013 1672       djohnson     Initial creation
 * Aug 26, 2014 3365       ccody        Separate Data Delivery tests out of AWIPS 2 baseline.
 * 
 * </pre>
 * 
 * @author djohnson
 * @version 1.0
 */

public class MockNotificationListenerFactory extends
        NotificationListenerFactory {

    /**
     * Constructor.
     * 
     * @param emailSender
     */
    public MockNotificationListenerFactory(EmailSender emailSender,
            RegistrySOAPServices registrySoapClient) {
        super(emailSender, registrySoapClient);
    }

    private final Map<String, NotificationListener> mockListeners = new HashMap<String, NotificationListener>();

    /**
     * {@inheritDoc}
     */
    @Override
    NotificationListener getWebServiceNotificationListener(
            NotificationDestination destination) {
        final NotificationListener mock = mock(WebServiceNotificationListener.class);
        mockListeners.put(destination.getDestination(), mock);
        return mock;
    }

    /**
     * Get the mock listeners created for a destination.
     * 
     * @param destination
     *            the destination
     * @return the mock
     */
    public NotificationListener getMockForDestination(String destination) {
        return mockListeners.get(destination);
    }

}
