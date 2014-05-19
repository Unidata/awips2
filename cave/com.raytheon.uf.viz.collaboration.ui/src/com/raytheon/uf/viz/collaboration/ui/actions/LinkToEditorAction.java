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

import java.util.HashMap;
import java.util.Map;

import org.eclipse.jface.action.Action;
import org.eclipse.ui.IEditorPart;
import org.eclipse.ui.IPartListener;
import org.eclipse.ui.IViewReference;
import org.eclipse.ui.IWorkbenchPage;
import org.eclipse.ui.IWorkbenchPart;
import org.eclipse.ui.IWorkbenchWindow;
import org.eclipse.ui.PlatformUI;

import com.raytheon.uf.viz.collaboration.comm.identity.ISharedDisplaySession;
import com.raytheon.uf.viz.collaboration.comm.provider.connection.CollaborationConnection;
import com.raytheon.uf.viz.collaboration.display.IRemoteDisplayContainer;
import com.raytheon.uf.viz.collaboration.display.editor.ICollaborationEditor;
import com.raytheon.uf.viz.collaboration.display.roles.dataprovider.SharedEditorsManager;
import com.raytheon.uf.viz.collaboration.ui.Activator;
import com.raytheon.uf.viz.collaboration.ui.session.CollaborationSessionView;
import com.raytheon.uf.viz.core.icon.IconUtil;
import com.raytheon.viz.ui.editor.AbstractEditor;
import com.raytheon.viz.ui.views.CaveWorkbenchPageManager;

/**
 * Link shared editors to their shared Sesion views.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jul 5, 2012            bsteffen     Initial creation
 * 
 * </pre>
 * 
 * @author bsteffen
 * @version 1.0
 */

public class LinkToEditorAction extends Action {

    private static Map<IWorkbenchWindow, LinkToEditorAction> instanceMap = new HashMap<IWorkbenchWindow, LinkToEditorAction>();

    private final IWorkbenchWindow window;

    private final String LINK_TO_EDITOR_PREF = "linktoeditor";

    private PartListener partListener = new PartListener();

    private LinkToEditorAction(IWorkbenchWindow window) {
        super("Link Editor to Chat Session", Action.AS_CHECK_BOX);
        setImageDescriptor(IconUtil.getImageDescriptor(Activator.getDefault()
                .getBundle(), "link_to_editor.gif"));
        // check the preference store on whether the user wants the preference
        // checked or not
        boolean checked = Activator.getDefault().getPreferenceStore()
                .getBoolean(LINK_TO_EDITOR_PREF);
        setChecked(checked);
        this.window = window;
        run();
    }

    @Override
    public void run() {
        boolean checked = isChecked();
        if (checked) {
            window.getActivePage().addPartListener(partListener);
        } else {
            window.getActivePage().removePartListener(partListener);
        }
        Activator.getDefault().getPreferenceStore()
                .setValue(LINK_TO_EDITOR_PREF, checked);
    }

    public static synchronized LinkToEditorAction getInstance(
            IWorkbenchWindow window) {
        LinkToEditorAction instance = instanceMap.get(window);
        if (instance == null) {
            instance = new LinkToEditorAction(window);
            instanceMap.put(window, instance);
        }
        instance.setEnabled(CollaborationConnection.getConnection() != null);
        return instance;
    }

    private static class PartListener implements IPartListener {

        @Override
        public void partActivated(IWorkbenchPart part) {
            if (part instanceof CollaborationSessionView) {
                IWorkbenchPage page = PlatformUI.getWorkbench()
                        .getActiveWorkbenchWindow().getActivePage();
                IRemoteDisplayContainer container = ((CollaborationSessionView) part)
                        .getDisplayContainer();
                if (container != null) {
                    IEditorPart editor = container.getActiveDisplayEditor();
                    if (editor != null) {
                        page.bringToTop(editor);
                    } else if (container instanceof SharedEditorsManager) {
                        SharedEditorsManager sem = (SharedEditorsManager) container;
                        for (AbstractEditor sharedEditor : sem
                                .getSharedEditors()) {
                            page.bringToTop(sharedEditor);
                            break;
                        }
                    }
                }
            } else {
                CaveWorkbenchPageManager page = CaveWorkbenchPageManager
                        .getActiveInstance();
                String sessionId = null;
                if (part instanceof ICollaborationEditor) {
                    sessionId = ((ICollaborationEditor) part).getSessionId();
                } else if (part instanceof AbstractEditor) {
                    ISharedDisplaySession session = SharedEditorsManager
                            .getSharedEditorSession((AbstractEditor) part);
                    if (session != null) {
                        sessionId = session.getSessionId();
                    }
                }
                if (sessionId != null) {
                    for (IViewReference ref : page.getViewReferences()) {
                        if (CollaborationSessionView.ID.equals(ref.getId())) {
                            CollaborationSessionView view = (CollaborationSessionView) ref
                                    .getPart(false);
                            if (sessionId.equals(view.getSessionId())) {
                                page.bringToTop(view);
                                break;
                            }
                        }
                    }
                }
            }
        }

        @Override
        public void partBroughtToTop(IWorkbenchPart part) {
            // Do nothing
        }

        @Override
        public void partClosed(IWorkbenchPart part) {
            // Do nothing
        }

        @Override
        public void partDeactivated(IWorkbenchPart part) {
            // Do nothing
        }

        @Override
        public void partOpened(IWorkbenchPart part) {
            // Do nothing
        }

    }

}
