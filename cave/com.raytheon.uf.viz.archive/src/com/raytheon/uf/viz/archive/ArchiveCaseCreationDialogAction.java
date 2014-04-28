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

import com.raytheon.uf.common.archive.request.ArchiveCaseCreationAuthRequest;
import com.raytheon.uf.common.auth.user.IUser;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.viz.archive.ui.CaseCreationDlg;
import com.raytheon.uf.viz.core.auth.UserController;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.requests.ThriftClient;

/**
 * Action to bring up the Archive Case Creation dialog..
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
public class ArchiveCaseCreationDialogAction extends AbstractHandler {
    private final IUFStatusHandler statusHandler = UFStatus
            .getHandler(ArchiveCaseCreationDialogAction.class);

    /** Dialog to display */
    private CaseCreationDlg dialog;

    /** Default case directory location. */
    private String caseDir;

    /** Case Administration permission */
    private final String PERMISSION = "archive.casecreation";

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
                dialog = new CaseCreationDlg(shell, caseDir);
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
                + " does not have permission to access archive case creation dialog.";
        ArchiveCaseCreationAuthRequest request = new ArchiveCaseCreationAuthRequest();
        request.setRoleId(PERMISSION);
        request.setNotAuthorizedMessage(msg);
        request.setUser(user);

        try {
            Object o = ThriftClient.sendPrivilegedRequest(request);
            if (o instanceof ArchiveCaseCreationAuthRequest) {
                ArchiveCaseCreationAuthRequest r = (ArchiveCaseCreationAuthRequest) o;
                if (r.isAuthorized()) {
                    this.caseDir = r.getCaseDirectory();
                    return true;
                }
            } else {
                statusHandler
                        .handle(Priority.ERROR,
                                String.format(
                                        "Cannot validate user expected response type ArchiveCaseCreationAuthRequest, received %s",
                                        o.getClass().getName()));
            }
        } catch (VizException e) {
            statusHandler.handle(Priority.PROBLEM, e.getLocalizedMessage(), e);
        }

        return false;
    }
}
