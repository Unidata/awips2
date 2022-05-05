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
package com.raytheon.viz.gfe.textformatter.test;

import org.apache.commons.lang3.StringUtils;

import com.raytheon.uf.common.activetable.ActiveTableMode;
import com.raytheon.uf.common.activetable.VTECChange;
import com.raytheon.uf.common.activetable.VTECTableChangeNotification;
import com.raytheon.uf.common.jms.notification.INotificationObserver;
import com.raytheon.uf.common.jms.notification.NotificationException;
import com.raytheon.uf.common.jms.notification.NotificationMessage;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;

/**
 * An implementation of the {@link INotificationObserver} interface that is used
 * exclusively by the GFE formatter auto test suite to detect when a VTEC
 * product has been decoded and stored by EDEX.
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Mar 09, 2017  #6211     dgilling     Initial creation
 *
 * </pre>
 *
 * @author dgilling
 */

public class AutoTestVTECNotificationListener implements INotificationObserver {

    private final IUFStatusHandler statusHandler = UFStatus
            .getHandler(getClass());

    private String expectedPil;

    private volatile boolean receivedNotification;

    public AutoTestVTECNotificationListener() {
        this.expectedPil = StringUtils.EMPTY;
        this.receivedNotification = false;
    }

    public void resetListener(final String newPil) {
        expectedPil = newPil;
        receivedNotification = false;
    }

    @Override
    public void notificationArrived(NotificationMessage[] messages) {
        if (expectedPil.isEmpty()) {
            return;
        }

        for (NotificationMessage msg : messages) {
            try {
                if (msg.getMessagePayload() instanceof VTECTableChangeNotification) {
                    VTECTableChangeNotification notification = (VTECTableChangeNotification) msg
                            .getMessagePayload();
                    if (ActiveTableMode.PRACTICE == notification.getMode()) {
                        for (VTECChange change : notification.getChanges()) {
                            String pil = change.getPil() + change.getXxxid();
                            if (pil.equals(expectedPil)) {
                                receivedNotification = true;
                                expectedPil = StringUtils.EMPTY;
                                return;
                            }
                        }
                    }
                }
            } catch (NotificationException e) {
                statusHandler.error("Unable to unmarshall NotificationMessage",
                        e);
            }
        }
    }

    public boolean isReceivedNotification() {
        return receivedNotification;
    }
}
