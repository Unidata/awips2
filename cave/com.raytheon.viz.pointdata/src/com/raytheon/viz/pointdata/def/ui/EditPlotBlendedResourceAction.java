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

import org.eclipse.swt.graphics.Point;

import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.viz.pointdata.PlotModelFactory;
import com.raytheon.viz.pointdata.rsc.PlotBlendedResource;
import com.raytheon.viz.pointdata.rsc.PlotResource;
import com.raytheon.viz.ui.VizWorkbenchManager;
import com.raytheon.viz.ui.cmenu.AbstractRightClickAction;

/**
 * The action associated with right clicks ==> "Edit Plot attributes" for a
 * blended plot resource. Initializes multiple EditPlotResourceDialog
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer      Description
 * ------------ ---------- -----------   --------------------------
 * 2/25/2020    75195      K Sunil   Initial Creation
 *
 * </pre>
 *
 * @author ksunil
 */

public class EditPlotBlendedResourceAction extends AbstractRightClickAction {

    private static final IUFStatusHandler statusHandler = UFStatus
            .getHandler(EditPlotBlendedResourceAction.class);

    public EditPlotBlendedResourceAction() {
        super("Edit Plot(s) Attributes");
    }

    @Override
    public void run() {
        PlotBlendedResource plotBlendedResource = (PlotBlendedResource) getSelectedRsc();

        final Point prevDialogLocation = new Point(Integer.MIN_VALUE, 0);
        for (int i = 0; i < plotBlendedResource.getResourceList().size(); i++) {

            if (plotBlendedResource.getResourceList().get(i)
                    .getResource() instanceof PlotResource) {
                PlotResource plotResource = (PlotResource) plotBlendedResource
                        .getResourceList().get(i).getResource();
                try {
                    if (!PlotModelFactory.isNewSVGFormat(plotResource
                            .getResourceData().getPlotModelFile())) {
                        statusHandler
                                .error("Cannot open Edit Plot Attributes dialog for old style .svg file: "
                                        + plotResource.getResourceData()
                                                .getPlotModelFile());
                        continue;
                    }
                } catch (VizException e1) {
                    statusHandler
                            .error("Cannot open Edit Plot Attributes dialog for "
                                    + plotResource.getResourceData()
                                            .getPlotModelFile(),
                                    e1);
                    continue;
                }
                EditPlotResourceDialog dialog = new EditPlotResourceDialog(
                        VizWorkbenchManager.getInstance().getCurrentWindow()
                                .getShell(),
                        plotResource) {
                    protected void preOpened() {
                        super.preOpened();
                        if (prevDialogLocation.x != Integer.MIN_VALUE) {
                            getShell().setLocation(prevDialogLocation.x + 50,
                                    prevDialogLocation.y + 50);

                        }
                    }
                };

                dialog.open();
                prevDialogLocation.x = dialog.getShell().getLocation().x;
                prevDialogLocation.y = dialog.getShell().getLocation().y;
            }
        }
    }
}
