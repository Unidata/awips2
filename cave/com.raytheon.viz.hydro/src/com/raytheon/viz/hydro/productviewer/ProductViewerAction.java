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

package com.raytheon.viz.hydro.productviewer;

import org.eclipse.core.commands.AbstractHandler;
import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.commands.ExecutionException;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.handlers.HandlerUtil;

import com.raytheon.viz.hydrocommon.HydroDisplayManager;
import com.raytheon.viz.ui.dialogs.ICloseCallback;

/**
 * Action for Product Viewer Dialog.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date       	Ticket#		Engineer	Description
 * ------------	----------	-----------	--------------------------
 * 6/27/06                  lvenable    Initial creation.
 * 02/07/2013   1578        rferrel     Changes for non-blocking ProductViewerDlg.
 * 03/27/2013   1790        rferrel     Bug fix for non-blocking dialogs.
 * 06/19/2013   2119        rferrel     Changed check for no selected lid.
 * 04/12/2016   5483        dgilling    Fixes to support changes to ProductViewerDlg.
 * 
 * </pre>
 * 
 * @author lvenable
 * 
 */
public class ProductViewerAction extends AbstractHandler {
    /** Instance of the dialog. */
    ProductViewerDlg dialog;

    @Override
    public Object execute(ExecutionEvent arg0) throws ExecutionException {
        HydroDisplayManager manager = HydroDisplayManager.getInstance();
        Shell shell = HandlerUtil.getActiveShellChecked(arg0);

        if (manager.isCurrentLidSelected(shell)) {
            if (dialog == null || dialog.isDisposed()) {
                dialog = new ProductViewerDlg(shell);
                dialog.addCloseCallback(new ICloseCallback() {

                    @Override
                    public void dialogClosed(Object returnValue) {
                        dialog = null;
                    }
                });
                dialog.open();
            } else {
                dialog.setLid(manager.getCurrentLid());
                dialog.bringToTop();
            }
        }
        return null;
    }
}
