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
package com.raytheon.viz.gfe.rsc.colorbar;

import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.PlatformUI;

import com.raytheon.uf.common.colormap.IColorMap;
import com.raytheon.uf.viz.core.rsc.capabilities.ColorMapCapability;
import com.raytheon.viz.gfe.core.parm.Parm;
import com.raytheon.viz.ui.cmenu.AbstractRightClickAction;
import com.raytheon.viz.ui.dialogs.ColormapDialog;
import com.raytheon.viz.ui.dialogs.ICloseCallback;

/**
 * Action for right click menu for changing range of color map values.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Feb 2, 2011  7999      dgilling     Initial creation
 * Oct 17, 2012 1229       rferrel     Changes for non-blocking ColormapDialog.
 * 
 * </pre>
 * 
 * @author dgilling
 * @version 1.0
 */

public class ChangeColorMapRangeAction extends AbstractRightClickAction {

    private Parm parm;

    private ColormapDialog colorMapDlg;

    public ChangeColorMapRangeAction(Parm parm) {
        super("Set Range...");
        this.parm = parm;
    }

    /*
     * (non-Javadoc)
     * 
     * @see org.eclipse.jface.action.Action#run()
     */
    @Override
    public void run() {
        if (colorMapDlg == null || colorMapDlg.getShell() == null
                || colorMapDlg.isDisposed()) {
            Shell shell = PlatformUI.getWorkbench().getActiveWorkbenchWindow()
                    .getShell();
            ColorMapCapability cap = getSelectedRsc().getCapability(
                    ColorMapCapability.class);
            final IColorMap prevColorMap = cap.getColorMapParameters()
                    .getColorMap();
            final float prevMax = cap.getColorMapParameters().getColorMapMax();
            final float prevMin = cap.getColorMapParameters().getColorMapMin();

            colorMapDlg = new ColormapDialog(shell, "Set Color Table Range",
                    cap, parm.getGridInfo().getPrecision());
            colorMapDlg.setBlockOnOpen(false);
            colorMapDlg.setCloseCallback(new ICloseCallback() {

                @Override
                public void dialogClosed(Object returnValue) {
                    if (returnValue instanceof Integer) {
                        int status = (Integer) returnValue;
                        if (status != ColormapDialog.OK) {
                            ColorMapCapability cap = colorMapDlg
                                    .getColorMapCapability();
                            cap.getColorMapParameters().setColorMap(
                                    prevColorMap);
                            cap.getColorMapParameters().setColorMapMax(prevMax);
                            cap.getColorMapParameters().setColorMapMin(prevMin);
                            cap.notifyResources();
                        }
                    }
                    colorMapDlg = null;
                }
            });
            colorMapDlg.open();
        } else {
            colorMapDlg.bringToTop();
        }
    }
}
