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
import org.eclipse.swt.SWT;
import org.eclipse.swt.widgets.MessageBox;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.PlatformUI;

import com.raytheon.uf.common.auth.AuthException;
import com.raytheon.uf.common.auth.user.IUser;
import com.raytheon.uf.common.datadelivery.registry.DataSet;
import com.raytheon.uf.common.datadelivery.registry.DataType;
import com.raytheon.uf.common.datadelivery.request.DataDeliveryPermission;
import com.raytheon.uf.common.localization.LocalizationFile;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.viz.core.auth.UserController;
import com.raytheon.uf.viz.datadelivery.filter.MetaDataManager;
import com.raytheon.uf.viz.datadelivery.services.DataDeliveryServices;
import com.raytheon.uf.viz.datadelivery.subscription.subset.SubsetFileManager;
import com.raytheon.uf.viz.datadelivery.subscription.subset.SubsetManagerDlg;
import com.raytheon.uf.viz.datadelivery.subscription.subset.xml.SubsetXML;
import com.raytheon.viz.ui.dialogs.ICloseCallback;
import com.raytheon.viz.ui.dialogs.ListSelectionDlg;

/**
 * Handler for launching the Subset Manager Dialog.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Apr 02, 2012            mpduff       Initial creation
 * Aug 06, 2012 955        djohnson     Change to accept {@link DataSet}.
 * Aug 10, 2012 1022       djohnson     Store provider name in {@link SubsetXML}, use GriddedDataSet.
 * Aug 21, 2012 0743       djohnson     Change getMetaData to getDataSet.
 * Oct 03, 2012 1241       djohnson     Use {@link DataDeliveryPermission}.
 * Jul 26, 2013   2232     mpduff       Refactored Data Delivery permissions.
 * Sep 04, 2013   2314     mpduff       LoadSave dialog now non-blocking.
 * Oct 11, 2013   2386     mpduff       Refactor DD Front end.
 * Apr 10, 2014   2864     mpduff       Changed how saved subset files are stored.
 * Apr 22, 2014   3053     lvenable     Updated constructor args for ListSelectionDlg, put
 *                                      in a null check when the dialog is canceled, and removed
 *                                      throwing an exception that will be handled by a message box
 *                                      in the list selection dialog.
 * 
 * </pre>
 * 
 * @author mpduff
 * @version 1.0
 */

public class SubsetAction extends AbstractHandler {
    /** Status Handler */
    private final IUFStatusHandler statusHandler = UFStatus
            .getHandler(SubsetAction.class);

    /** Dialog instance */
    private SubsetManagerDlg dlg = null;

    /** Dialog instance */
    private ListSelectionDlg selectionDlg = null;

    private final String permission = DataDeliveryPermission.SUBSCRIPTION_EDIT
            .toString();

    @Override
    public Object execute(ExecutionEvent arg0) throws ExecutionException {
        try {
            // Check subscription.edit permissions
            IUser user = UserController.getUserObject();
            String msg = user.uniqueId()
                    + " is not authorized to edit subscriptions\nPermission: "
                    + permission;
            if (DataDeliveryServices.getPermissionsService()
                    .checkPermissions(user, msg, permission).isAuthorized()) {
                String[] choices = SubsetFileManager.getInstance()
                        .getAllLocalizationFileNames();
                final Shell shell = PlatformUI.getWorkbench()
                        .getActiveWorkbenchWindow().getShell();
                if (selectionDlg == null || selectionDlg.isDisposed()) {
                    selectionDlg = new ListSelectionDlg(shell, choices, true,
                            ListSelectionDlg.ReturnArray.ARRAY_STRING_ITEMS,
                            "Select");
                    selectionDlg.setCloseCallback(new ICloseCallback() {
                        @Override
                        public void dialogClosed(Object returnValue) {
                            // The the return value is null then return since
                            // the dialog was canceled.
                            if (returnValue == null) {
                                return;
                            }

                            if (returnValue instanceof String[]) {
                                String[] selection = (String[]) returnValue;

                                if (selection.length == 0) {
                                    return;
                                } else if (selection.length == 1) {
                                    String[] parts = selection[0].split(":");

                                    SubsetFileManager sfm = SubsetFileManager
                                            .getInstance();
                                    LocalizationFile locFile = sfm.getFile(
                                            parts[1],
                                            DataType.valueOf(parts[0]));
                                    SubsetXML subset = SubsetFileManager
                                            .getInstance().loadSubset(locFile);

                                    DataSet data = MetaDataManager
                                            .getInstance().getDataSet(
                                                    subset.getDatasetName(),
                                                    subset.getProviderName());

                                    if (dlg == null || dlg.isDisposed()) {
                                        dlg = SubsetManagerDlg.fromSubsetXML(
                                                shell, data, true, subset);
                                        dlg.open();
                                    } else {
                                        dlg.bringToTop();
                                    }
                                } else {
                                    /*
                                     * This is just a safety check in case the
                                     * dialog is configured incorrectly.
                                     */
                                    MessageBox mb = new MessageBox(shell,
                                            SWT.ICON_WARNING | SWT.OK);
                                    mb.setText("Multiple Items");
                                    mb.setMessage("Multiple items were selected.  Only one item can be processed.\n"
                                            + "You must relaunch the dialog and select one item to process.");
                                    mb.open();
                                }
                            } else {
                                throw new IllegalArgumentException(
                                        "Invalid return type from ListSelectionDlg: "
                                                + returnValue.getClass());
                            }
                        }
                    });
                    selectionDlg.open();
                } else {
                    selectionDlg.bringToTop();
                }
            }
        } catch (AuthException e) {
            statusHandler.handle(Priority.PROBLEM, e.getLocalizedMessage(), e);
        }

        return null;
    }
}
