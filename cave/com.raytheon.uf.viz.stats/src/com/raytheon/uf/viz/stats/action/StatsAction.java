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
package com.raytheon.uf.viz.stats.action;

import org.eclipse.core.commands.AbstractHandler;
import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.commands.ExecutionException;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.PlatformUI;

import com.raytheon.uf.common.stats.GraphDataRequest;
import com.raytheon.uf.common.stats.GraphDataResponse;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.requests.ThriftClient;
import com.raytheon.uf.viz.stats.ui.StatsControlDlg;

/**
 * Stats Action Handler.
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Sep 11, 2012    728     mpduff      Initial creation
 *
 * </pre>
 *
 * @author mpduff
 * @version 1.0
 */

public class StatsAction extends AbstractHandler {
    private final IUFStatusHandler statusHandler = UFStatus
            .getHandler(StatsAction.class);

    /** Dialog instance */
    private StatsControlDlg statsControlDlg = null;

    @Override
    public Object execute(ExecutionEvent event) throws ExecutionException {
        if ((statsControlDlg == null) || (statsControlDlg.isDisposed() == true)) {
            GraphDataRequest request = new GraphDataRequest();
            request.setMetaDataRequest(true);

            GraphDataResponse response = sendRequest(request);

            if (response != null) {
                Shell shell = PlatformUI.getWorkbench()
                        .getActiveWorkbenchWindow().getShell();
                statsControlDlg = new StatsControlDlg(shell);
                statsControlDlg.setConfigList(response.getConfigList());
                statsControlDlg.open();
            }
        } else {
            statsControlDlg.bringToTop();
        }

        return null;
    }

    /**
     * Send GraphDataRequest.
     *
     * @param req
     *            The request to send
     * @return The GraphDataResponse
     */
    private GraphDataResponse sendRequest(GraphDataRequest req) {
        try {
            return (GraphDataResponse) ThriftClient.sendRequest(req);
        } catch (VizException e) {
            statusHandler.handle(Priority.ERROR, "Error Requesting Data", e);
        }

        return null;
    }
}
