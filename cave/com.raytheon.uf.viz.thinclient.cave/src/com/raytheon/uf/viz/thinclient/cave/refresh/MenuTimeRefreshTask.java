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

import com.raytheon.uf.viz.thinclient.refresh.TimedRefresher.RefreshTimerTask;
import com.raytheon.viz.alerts.jobs.MenuUpdater;
import com.raytheon.viz.alerts.observers.ProductAlertObserver;

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
 * 
 * </pre>
 * 
 * @author mschenke
 * @version 1.0
 */

public class MenuTimeRefreshTask implements RefreshTimerTask {

    private MenuUpdater menuUpdater = new MenuUpdater();

    /*
     * (non-Javadoc)
     * 
     * @see java.util.TimerTask#run()
     */
    @Override
    public void run() {
        ThinClientURICatalog.getInstance().requeryAllMenuTimes();

    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.thinclient.refresh.TimedRefresher.RefreshTimerTask
     * #scheduled()
     */
    @Override
    public void scheduled() {
        ProductAlertObserver.removeObserver(null, menuUpdater);
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.thinclient.refresh.TimedRefresher.RefreshTimerTask
     * #stopped()
     */
    @Override
    public void stopped() {
        ProductAlertObserver.addObserver(null, menuUpdater);
    }

}
