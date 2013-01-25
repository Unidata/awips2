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

import java.io.File;

import org.eclipse.core.commands.AbstractHandler;
import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.commands.ExecutionException;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.PlatformUI;

import com.raytheon.uf.common.auth.user.IUser;
import com.raytheon.uf.common.datadelivery.registry.DataSet;
import com.raytheon.uf.common.datadelivery.request.DataDeliveryPermission;
import com.raytheon.uf.common.localization.LocalizationFile;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.viz.core.auth.UserController;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.datadelivery.common.ui.LoadSaveConfigDlg;
import com.raytheon.uf.viz.datadelivery.common.ui.LoadSaveConfigDlg.DialogType;
import com.raytheon.uf.viz.datadelivery.filter.MetaDataManager;
import com.raytheon.uf.viz.datadelivery.services.DataDeliveryServices;
import com.raytheon.uf.viz.datadelivery.subscription.subset.SubsetFileManager;
import com.raytheon.uf.viz.datadelivery.subscription.subset.SubsetManagerDlg;
import com.raytheon.uf.viz.datadelivery.subscription.subset.xml.SubsetXML;

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
 * Aug 10, 2012 1022       djohnson     Store provider name in {@link SubsetXml}, use GriddedDataSet.
 * Aug 21, 2012 0743       djohnson     Change getMetaData to getDataSet.
 * Oct 03, 2012 1241       djohnson     Use {@link DataDeliveryPermission}.
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

    /** Saved subset path */
    private final String SUBSET_PATH = "dataDelivery" + File.separator
            + "subset" + File.separator;

    /** Dialog instance */
    private SubsetManagerDlg<?, ?, ?> dlg = null;

    /** Dialog instance */
    private LoadSaveConfigDlg loadDlg = null;

    private final DataDeliveryPermission permission = DataDeliveryPermission.SUBSCRIPTION_EDIT;

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
                Shell shell = PlatformUI.getWorkbench()
                        .getActiveWorkbenchWindow().getShell();
                if (loadDlg == null || loadDlg.isDisposed()) {
                    loadDlg = new LoadSaveConfigDlg(shell, DialogType.OPEN,
                            SUBSET_PATH, "", true);
                    loadDlg.open();
                } else {
                    loadDlg.bringToTop();
                }
                LocalizationFile locFile = (LocalizationFile) loadDlg
                        .getReturnValue();
                if (locFile == null) {
                    return null;
                }
                SubsetXML<?> subset = SubsetFileManager.getInstance()
                        .loadSubset(locFile.getFile().getName());

                DataSet data = MetaDataManager.getInstance().getDataSet(
                        subset.getDatasetName(), subset.getProviderName());

                if (dlg == null || dlg.isDisposed()) {
                    dlg = SubsetManagerDlg.fromSubsetXML(shell, data, true,
                            subset);
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
