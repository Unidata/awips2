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
package com.raytheon.viz.warngen.gui;

import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.commands.ExecutionException;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.PlatformUI;

import com.raytheon.uf.viz.core.IDisplayPane;
import com.raytheon.uf.viz.core.drawables.IDescriptor;
import com.raytheon.uf.viz.core.drawables.ResourcePair;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.rsc.LoadProperties;
import com.raytheon.uf.viz.core.rsc.tools.GenericToolsResourceData;
import com.raytheon.uf.viz.core.rsc.tools.action.AbstractGenericToolAction;
import com.raytheon.viz.textworkstation.TextWorkstationDlg;
import com.raytheon.viz.ui.dialogs.ICloseCallback;
import com.raytheon.viz.ui.input.EditableManager;
import com.raytheon.viz.ui.simulatedtime.SimulatedTimeOperations;

/**
 * Simple action for loading the warngen layer
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * May  4, 2010            mschenke     Initial creation
 * Oct 10, 2010  6990      Qinglu Lin   Used D. Friedman short solution,
 *                                      with minor changes.
 * Aug 15, 2013  DR 16418  D. Friedman  Always show the dialog.
 * Sep 22, 2015  4859      dgilling     Prevent dialog from showing in DRT mode.
 * Jun 15, 2017  ----      mjames@ucar  Open TextWorkstation.
 * 
 * </pre>
 * 
 * @author mschenke
 * @version 1.0
 */

public class WarngenAction extends AbstractGenericToolAction<WarngenLayer> {
	
    private static TextWorkstationDlg textWorkstationDlg;

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.awipstools.ui.action.MapToolAction#getResourceData()
     */
    @Override
    protected GenericToolsResourceData<WarngenLayer> getResourceData() {
        return new GenericToolsResourceData<WarngenLayer>(
                "Interactive Warngen", WarngenLayer.class);
    }

    @Override
    protected WarngenLayer getResource(LoadProperties loadProperties,
            IDescriptor descriptor) throws VizException {

        for (IDisplayPane pane : getSelectedPanes()) {
            for (ResourcePair rp : pane.getDescriptor().getResourceList()) {
                if (rp.getResource() instanceof WarngenLayer) {
                    EditableManager.makeEditable(rp.getResource(), true);
                    ((WarngenLayer) rp.getResource()).showDialog(true);
                    return (WarngenLayer) rp.getResource();
                }
            }
        }

        WarngenLayer layer = super.getResource(loadProperties, descriptor);
        layer.showDialog(true);
        return layer;
    }

    @Override
    public Object execute(ExecutionEvent arg0) throws ExecutionException {
    	
    	if ((textWorkstationDlg == null) || textWorkstationDlg.isDisposed()) {
            textWorkstationDlg = new TextWorkstationDlg(Display.getCurrent());
            textWorkstationDlg.setCloseCallback(new ICloseCallback() {

                @Override
                public void dialogClosed(Object returnValue) {
                    textWorkstationDlg = null;
                }
            });
            textWorkstationDlg.open();
        } else {
            textWorkstationDlg.bringToTop();
        }
    	
        if (!SimulatedTimeOperations.isTransmitAllowed()) {
            Shell shell = PlatformUI.getWorkbench().getActiveWorkbenchWindow()
                    .getShell();
            SimulatedTimeOperations
                    .displayFeatureLevelWarning(shell, "WarnGen");
            return null;
        }

        return super.execute(arg0);
    }
}
