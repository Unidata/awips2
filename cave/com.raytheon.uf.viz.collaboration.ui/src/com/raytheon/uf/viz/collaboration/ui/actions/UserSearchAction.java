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

import com.raytheon.uf.viz.collaboration.comm.provider.connection.CollaborationConnection;
import com.raytheon.uf.viz.collaboration.ui.Activator;
import com.raytheon.uf.viz.collaboration.ui.UserSearchDialog;
import com.raytheon.uf.viz.core.icon.IconUtil;

/**
 * Open the User Search Dialog
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jul 5, 2012            bsteffen     Initial creation
 * Feb 19, 2014 2751       bclement    added icon
 * 
 * </pre>
 * 
 * @author bsteffen
 * @version 1.0
 */

public class UserSearchAction extends Action {

    public UserSearchAction() {
        super("User Search...", IconUtil.getImageDescriptor(Activator
                .getDefault().getBundle(), "spyglass.gif"));
        setEnabled(CollaborationConnection.getConnection() != null);
    }

    public void run() {
        new UserSearchDialog(Display.getCurrent().getActiveShell()).open();
    };

}
