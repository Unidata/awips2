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
package com.raytheon.uf.viz.alertview.store;

import java.util.Arrays;
import java.util.List;
import java.util.concurrent.ConcurrentLinkedQueue;

import com.raytheon.uf.viz.alertview.Alert;
import com.raytheon.uf.viz.alertview.AlertDestination;
import com.raytheon.uf.viz.alertview.AlertStore;
import com.raytheon.uf.viz.alertview.prefs.AlertViewPreferences;
import com.raytheon.uf.viz.alertview.prefs.PreferenceFile;

/**
 * 
 * A simple {@link AlertStore} that retains all {@link Alert}s that arrive in
 * memory.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date          Ticket#  Engineer  Description
 * ------------- -------- --------- --------------------------
 * Jun 17, 2015  4474     bsteffen  Initial creation
 * 
 * </pre>
 * 
 * @author bsteffen
 * @version 1.0
 */
public class MemoryAlertStore implements AlertStore, AlertDestination,
        PreferenceFile.Listener<AlertViewPreferences> {

    private ConcurrentLinkedQueue<Alert> alerts = new ConcurrentLinkedQueue<>();

    private final PreferenceFile<AlertViewPreferences> prefsFile;

    private int retentionCount;

    public MemoryAlertStore() {
        prefsFile = AlertViewPreferences.load(this);
        retentionCount = prefsFile.get().getAlertsToLoad();
    }

    @Override
    public void update(AlertViewPreferences preferences) {
        retentionCount = preferences.getAlertsToLoad();
        while (alerts.size() > retentionCount) {
            alerts.poll();
        }
    }

    /**
     * This will get called automatically by declaritive services.
     */
    public void deactivate() {
        prefsFile.close();
    }

    @Override
    public void handleAlert(Alert alert) {
        /* Trim too desired number of alerts */
        if (alerts.size() >= retentionCount) {
            alerts.poll();
        }
        alerts.offer(alert);

    }

    @Override
    public List<Alert> getAlerts() {
        return Arrays.asList(alerts.toArray(new Alert[0]));
    }

}
