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

import org.eclipse.ui.IWorkbenchWindow;
import org.eclipse.ui.PlatformUI;

import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.viz.core.AbstractTimeMatcher;
import com.raytheon.uf.viz.core.IDisplayPane;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.thinclient.refresh.TimedRefresher;
import com.raytheon.uf.viz.thinclient.refresh.TimedRefresher.RefreshTimerTask;
import com.raytheon.viz.alerts.jobs.AutoUpdater;
import com.raytheon.viz.alerts.observers.ProductAlertObserver;
import com.raytheon.viz.ui.editor.AbstractEditor;
import com.raytheon.viz.ui.perspectives.AbstractVizPerspectiveManager;
import com.raytheon.viz.ui.perspectives.VizPerspectiveListener;

/**
 * Timer task responsible for refreshing IEditorParts that implement
 * IDisplayPaneContainer. Does not currently operate on IViewParts that may be
 * an instance of IDisplayPaneContainer
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

public class DataRefreshTask implements RefreshTimerTask {

    private IUFStatusHandler statusHandler = UFStatus
            .getHandler(DataRefreshTask.class);

    private AutoUpdater autoUpdater = new AutoUpdater();

    /*
     * (non-Javadoc)
     * 
     * @see java.util.TimerTask#run()
     */
    @Override
    public void run() {
        // For every window open, redo time matching on every descriptor on
        // every editor for the window. A bit D2D specific, but it is how the
        // AutoUpdater works which is what we are mimicking.
        for (IWorkbenchWindow window : PlatformUI.getWorkbench()
                .getWorkbenchWindows()) {
            // Find listener for window
            VizPerspectiveListener listener = VizPerspectiveListener
                    .getInstance(window);
            if (listener != null) {
                // Find active perspective manager for listener
                AbstractVizPerspectiveManager manager = listener
                        .getActivePerspectiveManager();
                if (manager != null) {
                    // Process each editor
                    for (AbstractEditor editor : manager
                            .getPerspectiveEditors()) {
                        for (IDisplayPane pane : editor.getDisplayPanes()) {
                            // Get the time matcher
                            AbstractTimeMatcher atm = pane.getDescriptor()
                                    .getTimeMatcher();
                            if (atm != null) {
                                try {
                                    // Redo time matching. This will trigger
                                    // updates and removal of old data
                                    atm.redoTimeMatching(pane.getDescriptor());
                                } catch (VizException e) {
                                    statusHandler.handle(Priority.PROBLEM,
                                            e.getLocalizedMessage(), e);
                                }
                            }
                        }
                    }
                }
            }
        }
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
        ProductAlertObserver.removeObserver(null, autoUpdater);
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
        ProductAlertObserver.addObserver(null, autoUpdater);
    }

}
