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
package com.raytheon.uf.common.dataplugin.grid.derivparam.cache;

import java.util.Set;
import java.util.concurrent.CopyOnWriteArraySet;

import com.raytheon.uf.common.dataplugin.annotations.DataURI;
import com.raytheon.uf.common.dataplugin.grid.GridConstants;
import com.raytheon.uf.common.dataplugin.grid.GridRecord;
import com.raytheon.uf.common.dataplugin.message.DataURINotificationMessage;
import com.raytheon.uf.common.jms.notification.INotificationObserver;
import com.raytheon.uf.common.jms.notification.NotificationException;
import com.raytheon.uf.common.jms.notification.NotificationMessage;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;

/**
 * 
 * Provide a centralized location for managing several grid caches. When this is
 * enabled it will receive {@link DataURINotificationMessage}s and update any
 * registered caches as needed.
 * 
 * In the default state, this object does nothing and all caches that register
 * should not be caching. For this object to be enabled, something within the
 * application must call {@link #getObserver()} and register the observer to
 * receive updates and then call {@link #enable()}. When enable is called it
 * will cause all the registered caches to enable updates and the registered
 * caches will receive updates when new data arrives.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date          Ticket#  Engineer  Description
 * ------------- -------- --------- -----------------
 * Mar 03, 2016  5439     bsteffen  Initial creation
 * 
 * </pre>
 * 
 * @author bsteffen
 */
public class GridCacheUpdater {

    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(GridCacheUpdater.class);
    
    private static final String DATAURI_PREFIX = DataURI.SEPARATOR
            + GridConstants.GRID + DataURI.SEPARATOR;

    private static final GridCacheUpdater instance = new GridCacheUpdater();

    private volatile boolean enabled = false;

    private final Set<GridUpdateListener> listeners = new CopyOnWriteArraySet<>();

    private final INotificationObserver observer = new INotificationObserver() {

        @Override
        public void notificationArrived(NotificationMessage[] messages) {
            for (NotificationMessage message : messages) {
                try {
                    handleNotification(message);
                } catch (NotificationException e) {
                    statusHandler.handle(Priority.WARN,
                            "GridUpdater failed to process a notification", e);
                }
            }
        }
    };

    private GridCacheUpdater() {

    }

    /**
     * Add a listener that will be notified whenever a new grid record arrives
     * while the updater is enabled.
     */
    public void addListener(GridUpdateListener listener) {
        listeners.add(listener);
        if (enabled) {
            listener.enableUpdates();
        }
    }

    public void removeListener(GridUpdateListener listener) {
        listeners.remove(listener);
    }

    /**
     * Get the observer that should be registered so that the updater can
     * recieve notifications.
     */
    public INotificationObserver getObserver(){
        return observer;
    }

    protected void handleNotification(NotificationMessage message)
            throws NotificationException {
        Object payLoad = message.getMessagePayload();
        if (payLoad instanceof DataURINotificationMessage) {
            DataURINotificationMessage datauriMessage = (DataURINotificationMessage) payLoad;
            for (String datauri : datauriMessage.getDataURIs()) {
                if (datauri.startsWith(DATAURI_PREFIX)) {
                    notifyListeners(new GridRecord(datauri));
                }
            }
        }
    }

    protected void notifyListeners(GridRecord record) {
        for (GridUpdateListener listener : listeners) {
            listener.update(record);
        }
    }
    
    /**
     * Set this updater into an enabled state. This should be done after the
     * observer from {@link #getObserver()} has been successfully registered.
     */
    public void enable(){
        for (GridUpdateListener listener : listeners) {
            listener.enableUpdates();
        }
    }

    /**
     * Set this updater into its default, disabled, state. This should be done
     * if the observer is unregistered or if the observing connection fails.
     */
    public void disable(){
        for (GridUpdateListener listener : listeners) {
            listener.disableUpdates();
        }
    }
    
    public boolean isEnabled() {
        return enabled;
    }

    public static GridCacheUpdater getInstance() {
        return instance;
    }

    public static interface GridUpdateListener {

        /**
         * This will be called any time a new grid record arrives in the system
         */
        public void update(GridRecord record);

        /**
         * This will be called as soon as the updater is successfully receiving
         * notifications.
         */
        public void enableUpdates();

        /**
         * This will be called if the updater is disabled, meaning it will not
         * reliably receive notifications of new grid data.
         */
        public void disableUpdates();

    }
    
}
