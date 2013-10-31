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
package com.raytheon.uf.viz.archive;

import org.eclipse.core.commands.AbstractHandler;
import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.commands.ExecutionException;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.PlatformUI;

import com.raytheon.uf.common.archive.request.ArchiveAdminAuthRequest;
import com.raytheon.uf.common.auth.user.IUser;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.viz.archive.ui.ArchiveRetentionDlg;
import com.raytheon.uf.viz.core.auth.UserController;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.requests.ThriftClient;

/**
 * Action to display the Archive Retention dialog.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * May 16, 2013 1966       rferrel     Initial creation
 * Oct 02, 2013 2326       rferrel     Check for administration authorization.
 * 
 * </pre>
 * 
 * @author rferrel
 * @version 1.0
 */
public class ArchiveRetentionDialogAction extends AbstractHandler {
    private final IUFStatusHandler statusHandler = UFStatus
            .getHandler(ArchiveRetentionDialogAction.class);

    private ArchiveRetentionDlg dialog;

    /** Retention Administration permission */
    private final String PERMISSION = "archive.retention";

    /*
     * (non-Javadoc)
     * 
     * @see
     * org.eclipse.core.commands.AbstractHandler#execute(org.eclipse.core.commands
     * .ExecutionEvent)
     */
    @Override
    public Object execute(ExecutionEvent event) throws ExecutionException {
        if (isAuthorized()) {
            if (dialog == null || dialog.isDisposed()) {
                Shell shell = PlatformUI.getWorkbench()
                        .getActiveWorkbenchWindow().getShell();
                dialog = new ArchiveRetentionDlg(shell);
                dialog.open();
            } else {
                dialog.bringToTop();
            }
        }

        return null;
    }

    /**
     * Is user authorized?
     * 
     * @return true if authorized
     */
    private boolean isAuthorized() {
        IUser user = UserController.getUserObject();
        String msg = user.uniqueId()
                + " does not have permission to access archive retention dialog.";
        ArchiveAdminAuthRequest request = new ArchiveAdminAuthRequest();
        request.setRoleId(PERMISSION);
        request.setNotAuthorizedMessage(msg);
        request.setUser(user);

        try {
            Object o = ThriftClient.sendPrivilegedRequest(request);
            if (o instanceof ArchiveAdminAuthRequest) {
                ArchiveAdminAuthRequest r = (ArchiveAdminAuthRequest) o;
                return r.isAuthorized();
            }
        } catch (VizException e) {
            statusHandler.handle(Priority.PROBLEM, e.getLocalizedMessage(), e);
        }

        return false;
    }
}
