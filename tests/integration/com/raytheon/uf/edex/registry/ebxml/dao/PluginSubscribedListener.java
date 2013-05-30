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
package com.raytheon.uf.edex.registry.ebxml.dao;

import oasis.names.tc.ebxml.regrep.wsdl.registry.services.v4.NotificationListener;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.NotificationType;

import org.junit.Ignore;

/**
 * A plugin subscribed listener.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Apr 17, 2013 1672       djohnson     Initial creation
 * 
 * </pre>
 * 
 * @author djohnson
 * @version 1.0
 */
@Ignore
public class PluginSubscribedListener implements NotificationListener {

    private boolean notified;

    /**
     * {@inheritDoc}
     */
    @Override
    public void onNotification(NotificationType notification) {
        this.notified = true;
    }

    /**
     * Check whether the listener was notified.
     * 
     * @return true if notified
     */
    public boolean hasBeenNotified() {
        return notified;
    }
}
