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

import java.io.IOException;

import org.eclipse.jface.action.Action;
import org.eclipse.swt.SWT;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.MessageBox;
import org.eclipse.ui.IEditorPart;
import org.eclipse.ui.IEditorReference;
import org.eclipse.ui.IViewPart;
import org.eclipse.ui.IViewReference;
import org.eclipse.ui.PlatformUI;

import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.viz.collaboration.comm.provider.session.CollaborationConnection;
import com.raytheon.uf.viz.collaboration.ui.Activator;
import com.raytheon.uf.viz.collaboration.ui.CollaborationGroupView;
import com.raytheon.uf.viz.collaboration.ui.ConnectionSubscriber;
import com.raytheon.uf.viz.collaboration.ui.editor.CollaborationEditor;
import com.raytheon.uf.viz.collaboration.ui.session.AbstractSessionView;
import com.raytheon.uf.viz.core.icon.IconUtil;
import com.raytheon.viz.ui.views.CaveWorkbenchPageManager;

/**
 * Ask the user for confirmation, then logout.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jul 11, 2012            bsteffen     Initial creation
 * Dec 19, 2013 2563       bclement     moved close logic to public method
 * 
 * </pre>
 * 
 * @author bsteffen
 * @version 1.0
 */

public class LogoutAction extends Action {

    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(LogoutAction.class);

    public LogoutAction() {
        super("Logout", IconUtil.getImageDescriptor(Activator.getDefault()
                .getBundle(), "logout.gif"));
        setEnabled(CollaborationConnection.getConnection() != null);
    }

    @Override
    public void run() {
        MessageBox messageBox = new MessageBox(Display.getCurrent()
                .getActiveShell(), SWT.ICON_WARNING | SWT.OK | SWT.CANCEL);
        messageBox.setText("Log Out of Collaboration");
        messageBox.setMessage("Logging out will sever your\n"
                + "connection to the server and\n"
                + "close all collaboration views\n" + "and editors.");
        int result = messageBox.open();
        if (result == SWT.OK) {
            closeCollaboration();
        }
    }

    /**
     * Close collaboration UI and close connection
     * 
     */
    public void closeCollaboration() {
        for (IViewReference ref : CaveWorkbenchPageManager.getActiveInstance()
                .getViewReferences()) {
            IViewPart view = ref.getView(false);
            if (view instanceof AbstractSessionView
                    || view instanceof CollaborationGroupView) {
                CaveWorkbenchPageManager.getActiveInstance().hideView(ref);
            }
        }

        // Close all Collaboration Editors.
        for (IEditorReference ref : PlatformUI.getWorkbench()
                .getActiveWorkbenchWindow().getActivePage()
                .getEditorReferences()) {
            IEditorPart editor = ref.getEditor(false);
            if (editor instanceof CollaborationEditor) {
                PlatformUI.getWorkbench().getActiveWorkbenchWindow()
                        .getActivePage().hideEditor(ref);
            }
        }
        try {
            Activator.getDefault().getPreferenceStore().save();
        } catch (IOException e) {
            statusHandler
                    .handle(Priority.WARN, "Unable to save preferences", e);
        }
        CollaborationConnection connection = CollaborationConnection
                .getConnection();
        ConnectionSubscriber.unsubscribe(connection);
        connection.close();
    }

}
