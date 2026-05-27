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
package com.raytheon.uf.viz.backupsvc.actions;

import org.eclipse.core.commands.AbstractHandler;
import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.handlers.HandlerUtil;

import com.raytheon.uf.common.auth.req.CheckAuthorizationRequest;
import com.raytheon.uf.common.auth.util.PermissionUtils;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.viz.backupsvc.ui.BackupServicesDialog;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.requests.ThriftClient;

/**
 * Display the Backup Services GUI
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date          Ticket#  Engineer    Description
 * ------------- -------- ----------- -----------------
 * Mar 02, 2021  84466    Robert.Blum Initial creation
 * Apr 20, 2021  84641    Added BackupServiceAdmin role
 *
 * </pre>
 *
 * @author Robert.Blum
 */

public class BackupServicesAction extends AbstractHandler {
    private final IUFStatusHandler statusHandler = UFStatus
            .getHandler(this.getClass());

    private BackupServicesDialog backupSvcDlg;

    @Override
    public Object execute(ExecutionEvent event) {
        if (isAuthorized()) {
            if (backupSvcDlg == null || backupSvcDlg.isDisposed()) {
                Shell shell = HandlerUtil.getActiveShell(event);
                backupSvcDlg = new BackupServicesDialog(shell);
                backupSvcDlg.open();
            } else {
                backupSvcDlg.bringToTop();
            }
        }

        return null;
    }

    private boolean isAuthorized() {
        String permission = PermissionUtils
                .buildPermissionString("backupService.dialog");
        CheckAuthorizationRequest request = new CheckAuthorizationRequest(
                permission);

        try {
            return (Boolean) ThriftClient.sendRequest(request);
        } catch (VizException e) {
            statusHandler
                    .error("Error checking backup services user permissions.");
        }

        return false;
    }
}
