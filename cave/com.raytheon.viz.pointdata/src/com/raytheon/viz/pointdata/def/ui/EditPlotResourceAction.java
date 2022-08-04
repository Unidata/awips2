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

package com.raytheon.viz.pointdata.def.ui;

import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.viz.pointdata.PlotModelFactory;
import com.raytheon.viz.pointdata.rsc.PlotResource;
import com.raytheon.viz.ui.VizWorkbenchManager;
import com.raytheon.viz.ui.cmenu.AbstractRightClickAction;

/**
 * EditPlotResourceDialog called when user right clicks and selects "Edit Plot
 * attributes". Initializes EditPlotModelComposite
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer      Description
 * ------------ ---------- -----------   --------------------------
 * 10/10/2019   71272      Mark Peters   Initial Creation
 * 01/13/2020   73084      K Sunil       Included the .svg file name in the error message.
 *
 * </pre>
 *
 * @author mpeters
 */

public class EditPlotResourceAction extends AbstractRightClickAction {

    private static final IUFStatusHandler statusHandler = UFStatus
            .getHandler(EditPlotResourceAction.class);

    public EditPlotResourceAction() {

        super("Edit Plot Attributes");
    }

    @Override
    public void run() {
        PlotResource plotResource = (PlotResource) getSelectedRsc();
        try {
            if (!PlotModelFactory.isNewSVGFormat(
                    plotResource.getResourceData().getPlotModelFile())) {
                statusHandler
                        .error("Cannot open Edit Plot Attributes dialog for old style .svg file: "
                                + plotResource.getResourceData()
                                        .getPlotModelFile());
                return;
            }
        } catch (VizException e1) {
            statusHandler.error("Cannot open Edit Plot Attributes dialog", e1);
            return;
        }
        EditPlotResourceDialog dialog = new EditPlotResourceDialog(
                VizWorkbenchManager.getInstance().getCurrentWindow().getShell(),
                plotResource);
        dialog.open();
    }
}
