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
package com.raytheon.viz.gfe.core.internal;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;

import org.eclipse.core.runtime.ListenerList;

import com.raytheon.uf.common.dataplugin.gfe.server.notify.GfeNotification;
import com.raytheon.uf.common.dataplugin.gfe.server.notify.UserMessageNotification;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.viz.core.VizApp;
import com.raytheon.uf.viz.core.notification.INotificationObserver;
import com.raytheon.uf.viz.core.notification.NotificationException;
import com.raytheon.uf.viz.core.notification.NotificationMessage;
import com.raytheon.viz.gfe.Activator;

/**
 * Provides a bridge between the notification system and the Java Listeners in
 * GFE
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#     Engineer    Description
 * ------------ ----------  ----------- --------------------------
 * Jun 11, 2008             chammack    Initial creation
 * Sep 03, 2008  1448       chammack    Implement refactored interface
 * Mar 01, 2012  #346       dgilling    Use identity-based ListenerLists.
 * May 22, 2014  #3110      randerso    Queue messages received prior to start rather 
 *                                      than dropping them on the foor
 * 
 * </pre>
 * 
 * @author chammack
 * @version 1.0
 */

public class NotificationRouter implements INotificationObserver {
    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(NotificationRouter.class);

    private final ListenerList observers;

    private boolean isReady;

    private String siteID;

    private List<NotificationMessage[]> messageList;

    /**
     * Constructor
     */
    public NotificationRouter(String siteID) {
        this.siteID = siteID;
        this.messageList = new LinkedList<NotificationMessage[]>();
        this.observers = new ListenerList(ListenerList.IDENTITY);
        this.observers
                .add(new AbstractGFENotificationObserver<UserMessageNotification>(
                        UserMessageNotification.class) {
                    Map<String, IUFStatusHandler> userMsgHandlers = new HashMap<String, IUFStatusHandler>();

                    @Override
                    public void notify(
                            final UserMessageNotification notificationMessage) {

                        VizApp.runAsync(new Runnable() {

                            @Override
                            public void run() {
                                String category = notificationMessage
                                        .getCategory();
                                IUFStatusHandler handler = userMsgHandlers
                                        .get(category);
                                if (handler == null) {
                                    handler = UFStatus.getHandler(
                                            Activator.PLUGIN_ID, category,
                                            "GFE");
                                    userMsgHandlers.put(category, handler);
                                }
                                handler.handle(
                                        notificationMessage.getPriority(),
                                        notificationMessage.getMessage());
                            }
                        });
                    }
                });

    }

    /**
     * Provides an abstract observer for notifications to be handled.
     * 
     * The class should be constructed with the notification message as the
     * generic type (and also passed into the constructor as class).
     * 
     * @author chammack
     * @version 1.0
     * @param <T>
     */
    public static abstract class AbstractGFENotificationObserver<T extends Object> {

        private final Class<T> clazz;

        /**
         * Constructor
         * 
         * NOTE: the class must be passed in due to runtime limitations of J2SE
         * 5.0 Generics implementation.
         * 
         * @param clazz
         *            the notification class (should match <T>)
         */
        public AbstractGFENotificationObserver(Class<T> clazz) {
            this.clazz = clazz;
        }

        /**
         * Return the notification class
         * 
         * @return the notification class
         */
        public Class<T> getNotificationClass() {
            return this.clazz;
        }

        /**
         * This is the callback to pass a notification message from the alerting
         * system, for conversion and dispensation to the framework.
         * 
         * @param notificationMessage
         */
        public abstract void notify(T notificationMessage);

    }

    /**
     * Add an observer
     * 
     * @param observer
     *            the observer
     */
    public void addObserver(AbstractGFENotificationObserver<?> observer) {
        this.observers.add(observer);
    }

    /**
     * Remove an observer
     * 
     * @param observer
     *            the observer
     */
    public void removeObserver(AbstractGFENotificationObserver<?> observer) {
        this.observers.remove(observer);
    }

    /**
     * Start routing
     */
    public void start() {
        synchronized (this) {
            this.isReady = true;
        }

        for (NotificationMessage[] messages : messageList) {
            notificationArrived(messages);
        }
    }

    /**
     * Notification that a message has arrived
     */
    @Override
    @SuppressWarnings("unchecked")
    public void notificationArrived(NotificationMessage[] messages) {
        // If DataManager is not initialized yet, do not start listening

        if (!isReady) {
            synchronized (this) {
                if (!isReady) {
                    this.messageList.add(messages);
                    statusHandler
                            .handle(Priority.VERBOSE,
                                    messages.length
                                            + "notification messages queued because router is not started");
                    return;
                }
            }
        }

        for (NotificationMessage message : messages) {
            try {
                if ((message.getProperty("siteID") == null)
                        || (!message.getProperty("siteID").equals(siteID))) {
                    continue;
                }
            } catch (NotificationException e1) {
                statusHandler
                        .handle(Priority.EVENTA,
                                "Unable to read incoming notification's siteID property.",
                                e1);
                continue;
            }

            Class<?> incomingMessageClass = null;
            List<GfeNotification> messageList = null;

            try {
                Object obj = message.getMessagePayload();
                if (obj instanceof List<?>) {
                    messageList = (List<GfeNotification>) obj;
                    if (messageList.size() > 0) {
                        incomingMessageClass = messageList.get(0).getClass();
                    }
                } else if (obj instanceof GfeNotification) {
                    incomingMessageClass = obj.getClass();
                    messageList = new ArrayList<GfeNotification>();
                    messageList.add((GfeNotification) obj);
                }

            } catch (Exception e) {
                statusHandler.handle(Priority.EVENTA,
                        "Unable to read incoming notification", e);
                continue;
            }

            if (incomingMessageClass == null) {
                continue;
            }

            for (Object listener : this.observers.getListeners()) {

                AbstractGFENotificationObserver<Object> observer = (AbstractGFENotificationObserver<Object>) listener;

                if (observer.getNotificationClass()
                        .equals(incomingMessageClass)) {
                    try {
                        for (GfeNotification msg : messageList) {
                            observer.notify(msg);
                        }
                    } catch (Exception e) {
                        String msg = String.format(
                                "Error notifying listener %s",
                                listener.toString());
                        statusHandler.handle(Priority.EVENTA, msg, e);

                    }
                }
            }
        }
    }
}
