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
package com.raytheon.uf.viz.collaboration.ui.actions;

import org.eclipse.jface.action.Action;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Shell;

import com.raytheon.uf.viz.collaboration.comm.provider.connection.CollaborationConnection;
import com.raytheon.uf.viz.collaboration.ui.Activator;
import com.raytheon.uf.viz.collaboration.ui.login.LoginDialog;
import com.raytheon.uf.viz.core.icon.IconUtil;

/**
 * Open a dialog to log into collaboration.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jul 11, 2012            bsteffen     Initial creation
 * Apr 11, 2014 2903       bclement    added success flag
 * 
 * </pre>
 * 
 * @author bsteffen
 * @version 1.0
 */

public class LoginAction extends Action {
    
    private boolean success = false;

    public LoginAction() {
        super("Login...", IconUtil.getImageDescriptor(Activator.getDefault()
                .getBundle(), "login.png"));
        setEnabled(CollaborationConnection.getConnection() == null);
    }

    @Override
    public void run() {
        CollaborationConnection connection = CollaborationConnection
                .getConnection();
        if (connection == null) {
            Shell shell = Display.getDefault().getActiveShell();
            if (shell == null) {
                return;
            }
            success = new LoginDialog(shell).login();
        }
    }

    /**
     * @return the success
     */
    public boolean isSuccess() {
        return success;
    }

    /**
     * Convenience method to run action and return results
     * 
     * @return true if login was successful
     */
    public boolean login() {
        this.run();
        return isSuccess();
    }
}
