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
import org.eclipse.swt.widgets.Display;
import org.eclipse.ui.IEditorPart;
import org.eclipse.ui.IViewReference;
import org.eclipse.ui.IWorkbenchPage;
import org.eclipse.ui.IWorkbenchPart;
import org.eclipse.ui.IWorkbenchPartReference;
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
import com.raytheon.viz.ui.views.PartAdapter2;

/**
 * Link shared editors to their shared Session views.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jul 05, 2012            bsteffen     Initial creation
 * Aug 26, 2014 3539       bclement     refactored to fix recursion warning in part listener
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

    private static class PartListener extends PartAdapter2 {

        /*
         * (non-Javadoc)
         * 
         * @see org.eclipse.ui.IPartListener2#partActivated(org.eclipse.ui.
         * IWorkbenchPartReference)
         */
        @Override
        public void partActivated(IWorkbenchPartReference partRef) {
            IWorkbenchPart part = partRef.getPart(false);
            if (part != null) {
                if (part instanceof CollaborationSessionView) {
                    handleSessionActivated((CollaborationSessionView) part);
                } else if (part instanceof ICollaborationEditor) {
                    String sessionId = ((ICollaborationEditor) part)
                            .getSessionId();
                    handleEditorActivated(sessionId);
                } else if (part instanceof AbstractEditor) {
                    ISharedDisplaySession session = SharedEditorsManager
                            .getSharedEditorSession((AbstractEditor) part);
                    if (session != null) {
                        String sessionId = session.getSessionId();
                        handleEditorActivated(sessionId);
                    }
                }
            }
        }

        /**
         * Handles link from session to map editor. When the user clicks on the
         * session tab, this brings the map editor to the front of the page.
         * 
         * @param session
         */
        private void handleSessionActivated(CollaborationSessionView session) {
            IWorkbenchPage page = PlatformUI.getWorkbench()
                    .getActiveWorkbenchWindow().getActivePage();
            String sessionId = session.getSessionId();
            IViewReference viewRef = getViewRef(sessionId);
            if (viewRef == null || isMinimized(viewRef, page)) {
                /*
                 * this means that the activation wasn't from a user clicking on
                 * the view (ie changing perspectives), so we don't want to
                 * bring the editor to the top
                 */
                return;
            }
            IRemoteDisplayContainer container = session.getDisplayContainer();
            if (container != null) {
                IEditorPart editor = container.getActiveDisplayEditor();
                if (editor != null) {
                    bringToTop(page, editor);
                } else if (container instanceof SharedEditorsManager) {
                    SharedEditorsManager sem = (SharedEditorsManager) container;
                    for (AbstractEditor sharedEditor : sem.getSharedEditors()) {
                        bringToTop(page, sharedEditor);
                        break;
                    }
                }
            }
        }

        /**
         * Handles link from map editor to session view. When the user clicks on
         * the map editor tab, this brings the session view to the front of the
         * page.
         * 
         * @param sessionId
         */
        private void handleEditorActivated(String sessionId) {
            if (sessionId != null) {
                IWorkbenchPage page = PlatformUI.getWorkbench()
                        .getActiveWorkbenchWindow().getActivePage();
                final IViewReference viewRef = getViewRef(sessionId);
                /* avoid bringing view to top if user has it minimized */
                if (viewRef != null && !isMinimized(viewRef, page)) {
                    bringToTop(viewRef);
                }
            }
        }

        /**
         * Brings view to top of page and activates it. This is done
         * asynchronously.
         * 
         * @param viewRef
         */
        private void bringToTop(final IViewReference viewRef) {
            Display.getDefault().asyncExec(new Runnable() {
                @Override
                public void run() {
                    CaveWorkbenchPageManager pageManager = CaveWorkbenchPageManager
                            .getActiveInstance();
                    pageManager.bringToTop(viewRef.getView(false));
                }
            });
        }

        /**
         * Brings editor to top of page and activates it. This is done
         * asynchronously.
         * 
         * @param page
         * @param editor
         */
        private void bringToTop(final IWorkbenchPage page,
                final IWorkbenchPart editor) {
            Display.getDefault().asyncExec(new Runnable() {
                @Override
                public void run() {
                    page.bringToTop(editor);
                }
            });
        }

        /**
         * @param sessionId
         * @return the first view in page that matches session id, null if not
         *         found
         */
        private IViewReference getViewRef(String sessionId) {
            CaveWorkbenchPageManager pageManager = CaveWorkbenchPageManager
                    .getActiveInstance();
            for (IViewReference ref : pageManager.getViewReferences()) {
                if (CollaborationSessionView.ID.equals(ref.getId())) {
                    CollaborationSessionView view = (CollaborationSessionView) ref
                            .getPart(false);
                    if (view.getSessionId().equals(sessionId)) {
                        return ref;
                    }
                }
            }
            return null;
        }

        /**
         * @param ref
         * @param page
         * @return true if part stack containing view is minimized
         */
        private boolean isMinimized(IViewReference ref, IWorkbenchPage page) {
            /*
             * TODO investigate why this sometimes returns false when part stack
             * is minimized for the first time after starting cave. May have
             * something to do with floating views.
             */
            return page.getPartState(ref) == IWorkbenchPage.STATE_MINIMIZED;
        }

    }

}
