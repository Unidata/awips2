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
package com.raytheon.uf.viz.alertview.ui.view;

import org.eclipse.swt.widgets.Display;

import com.raytheon.uf.viz.alertview.Alert;
import com.raytheon.uf.viz.alertview.AlertDestination;
import com.raytheon.uf.viz.alertview.filter.AlertFilter;
import com.raytheon.uf.viz.alertview.filter.FilterManager;
import com.raytheon.uf.viz.alertview.prefs.AlertViewPreferences;
import com.raytheon.uf.viz.alertview.prefs.PreferenceFile;

/**
 * Automatically open {@link AlertView} whenever new {@link Alert}s arrive that
 * have a high enough priority.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date          Ticket#  Engineer  Description
 * ------------- -------- --------- --------------------------
 * Jun 25, 2015  4474     bsteffen  Initial creation
 * 
 * </pre>
 * 
 * @author bsteffen
 * @version 1.0
 */
public class AlertViewAutoOpen implements AlertDestination,
        PreferenceFile.Listener<AlertViewPreferences> {

    private final PreferenceFile<AlertViewPreferences> prefsFile;

    private final FilterManager filters = new FilterManager();

    private AlertFilter filter;

    public AlertViewAutoOpen() {
        prefsFile = AlertViewPreferences.load(this);
        filter = filters.getFilter(prefsFile.get().getOpenFilter());
    }

    @Override
    public void update(AlertViewPreferences preference) {
        filter = filters.getFilter(prefsFile.get().getOpenFilter());

    }

    @Override
    public void handleAlert(final Alert alert) {
        if (filter.filter(alert)) {
            Display.getDefault().asyncExec(new Runnable() {

                @Override
                public void run() {
                    OpenAlertViewHandler.openInAlertView(alert);
                }
            });
        }
    }

}
