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
package com.raytheon.uf.viz.datadelivery.actions;

import org.eclipse.core.commands.AbstractHandler;
import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.commands.ExecutionException;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.PlatformUI;

import com.raytheon.uf.viz.datadelivery.bandwidth.ui.BandwidthUtilizationDlg;
import com.raytheon.uf.viz.datadelivery.bandwidth.ui.GraphDataUtil;

/**
 * Action handler for the bandwidth scheduling graph.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Nov 25, 2012   1269     mpduff      Initial creation.
 * Dec 13, 2012   1269     lvenable    Updated to use a graph utility for the graph data.
 * 
 * </pre>
 * 
 * @author mpduff
 * @version 1.0
 */

public class BandwidthScheduleGraphAction extends AbstractHandler {

    /** Dialog */
    private BandwidthUtilizationDlg dlg;

    /**
     * {@inheritDoc}
     */
    @Override
    public Object execute(ExecutionEvent event) throws ExecutionException {
        if (dlg == null || dlg.isDisposed()) {
            GraphDataUtil gdu = new GraphDataUtil(null);
            gdu.retrieveData();
            Shell shell = PlatformUI.getWorkbench().getActiveWorkbenchWindow()
                    .getShell();
            dlg = new BandwidthUtilizationDlg(shell, gdu);
            dlg.open();
        } else {
            dlg.redrawGraph();
            dlg.open();
        }

        return null;
    }
}
