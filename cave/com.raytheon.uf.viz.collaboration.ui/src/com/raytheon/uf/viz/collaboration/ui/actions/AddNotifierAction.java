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

import java.util.ArrayList;
import java.util.List;

import org.eclipse.jface.action.Action;
import org.eclipse.jface.resource.ImageDescriptor;
import org.eclipse.swt.widgets.Display;

import com.raytheon.uf.viz.collaboration.comm.provider.user.UserId;
import com.raytheon.uf.viz.collaboration.ui.Activator;
import com.raytheon.uf.viz.collaboration.ui.IUserSelector;
import com.raytheon.uf.viz.collaboration.ui.session.AddNotifierDlg;
import com.raytheon.uf.viz.core.icon.IconUtil;

/**
 * Launch the AddNotifier dialog.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Feb 20, 2014    2632    mpduff      Initial creation
 * 
 * </pre>
 * 
 * @author mpduff
 * @version 1.0
 */

public class AddNotifierAction extends Action {
    /** The IUserSelector */
    private final IUserSelector userSelection;

    /**
     * Constructor.
     * 
     * @param userSelection
     */
    public AddNotifierAction(IUserSelector userSelection) {
        super("Add Notifier...", getNotifierImageDescriptor());
        this.userSelection = userSelection;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void run() {
        List<String> userList = new ArrayList<String>();
        for (UserId id : userSelection.getSelectedUsers()) {
            userList.add(id.getName());
        }
        AddNotifierDlg dlg = new AddNotifierDlg(Display.getCurrent()
                .getActiveShell(),
                userList.toArray(new String[userList.size()]));
        dlg.open();
    }

    /**
     * Get the image descriptor.
     * 
     * @return The ImageDescriptor object
     */
    public static ImageDescriptor getNotifierImageDescriptor() {
        return IconUtil.getImageDescriptor(Activator.getDefault().getBundle(),
                "add_correction.gif");
    }
}
