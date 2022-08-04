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
package com.raytheon.uf.viz.thinclient.cave.refresh;

import java.util.ArrayList;
import java.util.Collection;

import org.eclipse.jface.preference.IPreferenceStore;

import com.raytheon.uf.viz.core.alerts.AlertMessage;
import com.raytheon.uf.viz.thinclient.Activator;
import com.raytheon.uf.viz.thinclient.preferences.ThinClientPreferenceConstants;
import com.raytheon.uf.viz.thinclient.refresh.TimedRefresher.RefreshTimerTask;
import com.raytheon.viz.alerts.observers.ProductAlertObserver;

/**
 * Timer task responsible for refreshing IEditorParts that implement
 * IDisplayPaneContainer. Does not currently operate on IViewParts that may be
 * an instance of IDisplayPaneContainer
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date          Ticket#  Engineer   Description
 * ------------- -------- ---------- -------------------------------------------
 * Nov 10, 2011  7393     mschenke   Initial creation
 * Feb 21, 2014  16744    dfriedman  Update all alert observers
 * Dec 04, 2015  5169     bsteffen   Allow ProductAlertObserver to send messages
 *                                   to the AutoUpdater
 * Feb 08, 2016  5281     tjensen    Replaced disableJms with dataRefreshMethod
 * 
 * </pre>
 * 
 * @author mschenke
 * @version 1.0
 */

public class DataRefreshTask implements RefreshTimerTask {

    @Override
    public void run() {
        IPreferenceStore store = Activator.getDefault().getPreferenceStore();
        if (ThinClientPreferenceConstants.P_DATA_REFRESH_METHOD_POLL
                .equals(store
                        .getString(ThinClientPreferenceConstants.P_DATA_REFRESH_METHOD))) {
            Collection<AlertMessage> alerts = ThinClientDataUpdateTree
                    .getInstance().updateAllData();

            // Make sure it gets to GridUpdater
            ArrayList<String> s = new ArrayList<String>(alerts.size());
            for (AlertMessage am : alerts) {
                s.add(am.dataURI);
            }
            ProductAlertObserver.processDataURIAlerts(s);
        }
    }

    @Override
    public void scheduled() {

    }

    @Override
    public void stopped() {

    }

}
