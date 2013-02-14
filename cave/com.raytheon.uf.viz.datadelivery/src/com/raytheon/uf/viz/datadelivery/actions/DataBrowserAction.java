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

import com.raytheon.uf.common.auth.user.IUser;
import com.raytheon.uf.common.datadelivery.request.DataDeliveryPermission;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.viz.core.auth.UserController;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.datadelivery.browser.DataBrowserDlg;
import com.raytheon.uf.viz.datadelivery.services.DataDeliveryServices;

/**
 * Action class. This is called when the Data Browser is selected from the CAVE
 * menu.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Feb 12, 2012            lvenable     Initial creation
 * Oct 03, 2012 1241       djohnson     Use {@link DataDeliveryPermission}.
 * 
 * </pre>
 * 
 * @author lvenable
 * @version 1.0
 */
public class DataBrowserAction extends AbstractHandler {
    /** Status Handler */
    private final IUFStatusHandler statusHandler = UFStatus
            .getHandler(DataBrowserAction.class);

    /** Instance of the dialog */
    private DataBrowserDlg dlg = null;

    private final DataDeliveryPermission permission = DataDeliveryPermission.SUBSCRIPTION_DATASET_BROWSER;

    @Override
    public Object execute(ExecutionEvent arg0) throws ExecutionException {
        try {
            // check if user is authorized
            IUser user = UserController.getUserObject();
            String msg = user.uniqueId()
                    + " is not authorized to access the Dataset Discovery Browser\nPermission: "
                    + permission;

            if (DataDeliveryServices.getPermissionsService()
                    .checkPermission(user, msg, permission)
                    .isAuthorized()) {
                if ((dlg == null) || (dlg.isDisposed() == true)) {
                    Shell shell = PlatformUI.getWorkbench()
                            .getActiveWorkbenchWindow().getShell();
                    dlg = new DataBrowserDlg(shell);
                    dlg.open();
                } else {
                    dlg.bringToTop();
                }
            }
        } catch (VizException e) {
            statusHandler.handle(Priority.PROBLEM, e.getLocalizedMessage(), e);
        }

        return null;
    }
}
