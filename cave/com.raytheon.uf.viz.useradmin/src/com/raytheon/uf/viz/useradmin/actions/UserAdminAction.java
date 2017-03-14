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
package com.raytheon.uf.viz.useradmin.actions;

import org.eclipse.core.commands.AbstractHandler;
import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.commands.ExecutionException;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.PlatformUI;

import com.raytheon.uf.common.auth.user.IUser;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.common.useradmin.request.UserAdminAuthRequest;
import com.raytheon.uf.viz.core.auth.UserController;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.requests.ThriftClient;
import com.raytheon.uf.viz.useradmin.ui.UserAdminSelectDlg;

/**
 * Action class to launch the User Administration Dialog.
 * 
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * May 12, 2012 224        mpduff      Initial creation
 *
 * </pre>
 *
 * @author mpduff
 * @version 1.0	
 */

public class UserAdminAction extends AbstractHandler {
    private static final transient IUFStatusHandler statusHandler = UFStatus.getHandler(UserAdminAction.class);

    /** User Administration permission */
    private final String permission = "awips.user.admin";
    
    private UserAdminSelectDlg adminDlg = null;
    
    /*
     * (non-Javadoc)
     * 
     * @see
     * org.eclipse.core.commands.IHandler#execute(org.eclipse.core.commands.
     * ExecutionEvent)
     */
    @Override
    public Object execute(ExecutionEvent event) throws ExecutionException {
        if (isAuthorized()) {
            if ((adminDlg == null) || (adminDlg.isDisposed() == true)) {
                Shell shell = PlatformUI.getWorkbench().getActiveWorkbenchWindow()
                        .getShell();
                adminDlg = new UserAdminSelectDlg(shell);
                adminDlg.open();
            } else {
                adminDlg.bringToTop();
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
        String msg = user.uniqueId() + " is not a user administrator.";
        UserAdminAuthRequest request = new UserAdminAuthRequest();
        request.setRoleId(permission);
        request.setNotAuthorizedMessage(msg);
        request.setUser(user);
        
        try {
            Object o = ThriftClient.sendPrivilegedRequest(request);
            if (o instanceof UserAdminAuthRequest) {
                UserAdminAuthRequest r = (UserAdminAuthRequest) o;
                return r.isAuthorized();
            }
        } catch (VizException e) {
            statusHandler.handle(Priority.PROBLEM, e.getLocalizedMessage(), e);
        }
        
        return false;
    }
}
