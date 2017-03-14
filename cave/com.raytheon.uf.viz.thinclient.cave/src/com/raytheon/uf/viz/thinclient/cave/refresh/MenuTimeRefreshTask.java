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

import org.eclipse.jface.preference.IPreferenceStore;

import com.raytheon.uf.viz.thinclient.Activator;
import com.raytheon.uf.viz.thinclient.preferences.ThinClientPreferenceConstants;
import com.raytheon.uf.viz.thinclient.refresh.TimedRefresher.RefreshTimerTask;

/**
 * Timer task that is responsible for requerying menu times
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Nov 10, 2011            mschenke     Initial creation
 * Feb 08, 2016  5281      tjensen      Replaced disableJms with dataRefreshMethod
 * 
 * </pre>
 * 
 * @author mschenke
 * @version 1.0
 */

public class MenuTimeRefreshTask implements RefreshTimerTask {

    @Override
    public void run() {
        IPreferenceStore store = Activator.getDefault().getPreferenceStore();
        if (ThinClientPreferenceConstants.P_DATA_REFRESH_METHOD_PUSH
                .equals(store
                        .getString(ThinClientPreferenceConstants.P_DATA_REFRESH_METHOD))) {
            return;
        }
        ThinClientURICatalog.getInstance().requeryAllMenuTimes();

    }

    @Override
    public void scheduled() {

    }

    @Override
    public void stopped() {

    }

}
