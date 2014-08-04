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
package com.raytheon.viz.texteditor.alarmalert.util;

import com.raytheon.uf.common.dataplugin.text.alarms.AlarmAlertProduct;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.viz.core.notification.INotificationObserver;
import com.raytheon.uf.viz.core.notification.NotificationMessage;
import com.raytheon.uf.viz.core.notification.jobs.NotificationManagerJob;
import com.raytheon.viz.core.mode.CAVEMode;

/**
 * Alarm Alert Notification Observer
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Oct 13, 2009            mnash       Initial creation
 * Jun 07, 2010 5851       cjeanbap    Properly stop alarm/alert observer listener.
 * Jul 24, 2014 3423       randerso    Get afos command execution off the UI thread
 * 
 * </pre>
 * 
 * @author mnash
 * @version 1.0
 */

public class AlarmAlertNotificationObserver implements INotificationObserver {
    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(AlarmAlertNotificationObserver.class);

    private static final String ALARM_ALERT_TOPIC = "edex.alarms.msg";

    private static AlarmAlertNotificationObserver instance = null;

    private AlarmAlertNotificationObserver() {
    }

    public static synchronized AlarmAlertNotificationObserver getInstance() {
        if (instance == null) {
            instance = new AlarmAlertNotificationObserver();
            NotificationManagerJob.addObserver(ALARM_ALERT_TOPIC, instance);
        }
        return instance;
    }

    /**
     * Remove the alert message observer from the Notification Manager Job
     * listener.
     */
    public static synchronized void removeNotificationObserver() {
        if (instance != null) {
            NotificationManagerJob.removeObserver(ALARM_ALERT_TOPIC, instance);
            instance = null;
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @seecom.raytheon.uf.viz.core.notification.INotificationObserver#
     * notificationArrived
     * (com.raytheon.uf.viz.core.notification.NotificationMessage[])
     */
    @Override
    public void notificationArrived(NotificationMessage[] messages) {
        for (NotificationMessage message : messages) {
            try {
                AlarmRun run = null;

                CAVEMode mode = CAVEMode.getMode();
                Object payload = message.getMessagePayload();
                if (payload instanceof AlarmAlertProduct) {
                    // ONLY execute AlarmAlert Products when in OPERATIONAL/TEST
                    // mode
                    // and the payload is for the OPERATIONAL/TEST mode.
                    AlarmAlertProduct aap = (AlarmAlertProduct) payload;
                    if ((CAVEMode.OPERATIONAL.equals(mode) || CAVEMode.TEST
                            .equals(mode)) && aap.getOperationalMode()) {
                        run = new AlarmRun(aap);
                    }
                } else {
                    if (CAVEMode.OPERATIONAL.equals(mode)
                            || CAVEMode.TEST.equals(mode)) {
                        run = new AlarmRun((String) payload);
                    }
                }

                if (run != null) {
                    Thread thread = new Thread(run);
                    thread.start();
                }
            } catch (Exception e) {
                statusHandler.handle(Priority.PROBLEM,
                        "Could not pass message from server", e);
            }
        }
    }

    private static class AlarmRun implements Runnable {

        AlarmAlertProduct aap = null;

        public AlarmRun(String p) {
            aap = new AlarmAlertProduct();// new AlarmAlertProduct[p.length];
            String[] temp;
            // for (int i = 0; i < p.length; i++) {
            temp = p.split("_");
            aap.setDateReceived(temp[1]);
            aap.setProductId(temp[0]);
            // }
        }

        public AlarmRun(AlarmAlertProduct aap) {
            this.aap = aap;
        }

        @Override
        public void run() {
            AlarmAlertFunctions.isInAlarmList(aap);
        }
    }
}
