package com.raytheon.uf.viz.collaboration.ui.session;

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

import java.util.ArrayList;
import java.util.IdentityHashMap;
import java.util.List;
import java.util.Map;

import org.eclipse.ecf.presence.roster.IRosterEntry;
import org.eclipse.jface.action.Action;
import org.eclipse.jface.action.ActionContributionItem;
import org.eclipse.jface.action.IMenuManager;
import org.eclipse.jface.action.Separator;
import org.eclipse.jface.action.ToolBarManager;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.swt.SWT;
import org.eclipse.swt.graphics.RGB;
import org.eclipse.swt.widgets.ColorDialog;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.ToolItem;
import org.eclipse.ui.IEditorPart;
import org.eclipse.ui.IPartListener;
import org.eclipse.ui.IViewSite;
import org.eclipse.ui.IWorkbenchPage;
import org.eclipse.ui.IWorkbenchPart;
import org.eclipse.ui.PartInitException;

import com.google.common.eventbus.Subscribe;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.viz.collaboration.comm.identity.CollaborationException;
import com.raytheon.uf.viz.collaboration.comm.identity.ISharedDisplaySession;
import com.raytheon.uf.viz.collaboration.comm.identity.IVenueSession;
import com.raytheon.uf.viz.collaboration.comm.identity.info.IVenueInfo;
import com.raytheon.uf.viz.collaboration.comm.identity.user.SharedDisplayRole;
import com.raytheon.uf.viz.collaboration.comm.provider.TransferRoleCommand;
import com.raytheon.uf.viz.collaboration.comm.provider.session.CollaborationConnection;
import com.raytheon.uf.viz.collaboration.comm.provider.user.IDConverter;
import com.raytheon.uf.viz.collaboration.comm.provider.user.UserId;
import com.raytheon.uf.viz.collaboration.display.IRemoteDisplayContainer;
import com.raytheon.uf.viz.collaboration.display.IRemoteDisplayContainer.IRemoteDisplayChangedListener;
import com.raytheon.uf.viz.collaboration.display.IRemoteDisplayContainer.RemoteDisplay;
import com.raytheon.uf.viz.collaboration.display.IRemoteDisplayContainer.RemoteDisplayChangeType;
import com.raytheon.uf.viz.collaboration.display.data.ColorChangeEvent;
import com.raytheon.uf.viz.collaboration.display.data.SessionContainer;
import com.raytheon.uf.viz.collaboration.display.data.SharedDisplaySessionMgr;
import com.raytheon.uf.viz.collaboration.display.editor.ICollaborationEditor;
import com.raytheon.uf.viz.collaboration.display.roles.dataprovider.SharedEditorsManager;
import com.raytheon.uf.viz.collaboration.display.rsc.SelfAddingSystemResourceListener;
import com.raytheon.uf.viz.collaboration.display.rsc.telestrator.CollaborationDrawingEvent;
import com.raytheon.uf.viz.collaboration.display.rsc.telestrator.CollaborationDrawingResource;
import com.raytheon.uf.viz.collaboration.display.rsc.telestrator.CollaborationDrawingResourceData;
import com.raytheon.uf.viz.collaboration.ui.Activator;
import com.raytheon.uf.viz.core.ContextManager;
import com.raytheon.uf.viz.core.VizApp;
import com.raytheon.uf.viz.core.drawables.IRenderableDisplay;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.icon.IconUtil;
import com.raytheon.uf.viz.drawing.DrawingToolLayer;
import com.raytheon.uf.viz.drawing.DrawingToolLayer.DrawMode;
import com.raytheon.viz.ui.VizWorkbenchManager;

/**
 * View class for a collaboration session
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Mar 1, 2012            rferrel     Initial creation
 * 
 * </pre>
 * 
 * @author rferrel
 * @version 1.0
 */
public class CollaborationSessionView extends SessionView implements
        IPartListener, IRemoteDisplayChangedListener {
    public static final String ID = "com.raytheon.uf.viz.collaboration.CollaborationSession";

    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(CollaborationSessionView.class);

    private static final String COLLABORATION_SESSION_IMAGE_NAME = "messages.gif";

    private Runnable actionUpdater = new Runnable() {
        @Override
        public void run() {
            updateToolItems();
        }
    };

    private Action colorChangeAction;

    private ActionContributionItem drawAction;

    private ActionContributionItem undoAction;

    private ActionContributionItem redoAction;

    private ActionContributionItem eraseAction;

    private ActionContributionItem clearAction;

    private ActionContributionItem lockAction;

    private ISharedDisplaySession session;

    private IRemoteDisplayContainer container;

    private IRenderableDisplay currentDisplay;

    private boolean locked = false;

    private DrawMode drawMode = DrawMode.NONE;

    private Map<IRenderableDisplay, SelfAddingSystemResourceListener> listeners = new IdentityHashMap<IRenderableDisplay, SelfAddingSystemResourceListener>();

    public CollaborationDrawingResource getCurrentDrawingResource() {
        CollaborationDrawingResource currentResource = null;
        if (currentDisplay != null) {
            for (CollaborationDrawingResource resource : currentDisplay
                    .getDescriptor()
                    .getResourceList()
                    .getResourcesByTypeAsType(
                            CollaborationDrawingResource.class)) {
                currentResource = resource;
                break;
            }
        }
        return currentResource;
    }

    private DrawingToolLayer getCurrentLayer() {
        CollaborationDrawingResource resource = getCurrentDrawingResource();
        if (resource != null) {
            return resource.getDrawingLayerFor(resource.getMyUser());
        }
        return null;
    }

    /*
     * (non-Javadoc)
     * 
     * @see org.eclipse.ui.part.ViewPart#init(org.eclipse.ui.IViewSite)
     */
    @Override
    public void init(IViewSite site) throws PartInitException {
        super.init(site);
        site.getPage().addPartListener(this);
        SessionContainer sc = SharedDisplaySessionMgr
                .getSessionContainer(sessionId);
        if (sc != null) {
            session = sc.getSession();
            if (sc.getCollaborationEditor() != null) {
                container = sc.getCollaborationEditor();
            } else {
                container = SharedEditorsManager.getManager(session);
            }
            container.addRemoteDisplayChangedListener(this);
            RemoteDisplay remoteDisplay = container.getActiveDisplay();
            if (remoteDisplay != null) {
                remoteDisplayChanged(container.getActiveDisplay(),
                        RemoteDisplayChangeType.ACTIVATED);
            }
        }
    }

    protected void createActions() {
        super.createActions();

        colorChangeAction = new Action("Change Color...") {
            @Override
            public void run() {
                ColorDialog dlg = new ColorDialog(Display.getCurrent()
                        .getActiveShell());
                RGB rgb = dlg.open();
                IStructuredSelection selection = (IStructuredSelection) usersTable
                        .getSelection();
                IRosterEntry entry = (IRosterEntry) selection.getFirstElement();
                ColorChangeEvent event = new ColorChangeEvent(
                        IDConverter.convertFrom(entry.getUser()), rgb);
                try {
                    session.sendObjectToVenue(event);
                } catch (CollaborationException e) {
                    statusHandler.handle(Priority.PROBLEM,
                            "Unable to send color change to venue", e);
                }
            }
        };

    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.viz.ui.views.CaveFloatingView#createToolbarButton()
     */
    @Override
    protected void createToolbarButton() {
        super.createToolbarButton();

        drawAction = new ActionContributionItem(new Action("Draw", SWT.TOGGLE) {
            @Override
            public void run() {
                DrawingToolLayer layer = getCurrentLayer();
                if (layer != null) {
                    if (layer.getDrawMode() == DrawMode.DRAW) {
                        layer.setDrawMode(DrawMode.NONE);
                    } else {
                        layer.setDrawMode(DrawMode.DRAW);
                    }
                    drawMode = layer.getDrawMode();
                }
                updateToolItems();
            }
        });
        drawAction.getAction().setImageDescriptor(
                IconUtil.getImageDescriptor(
                        com.raytheon.uf.viz.drawing.Activator.getDefault()
                                .getBundle(), "draw.gif"));

        undoAction = new ActionContributionItem(new Action("Undo") {
            @Override
            public void run() {
                DrawingToolLayer layer = getCurrentLayer();
                if (layer != null) {
                    layer.undo();
                }
                updateToolItems();
            }
        });
        undoAction.getAction().setImageDescriptor(
                IconUtil.getImageDescriptor(
                        com.raytheon.uf.viz.drawing.Activator.getDefault()
                                .getBundle(), "undo.gif"));

        redoAction = new ActionContributionItem(new Action("Redo") {
            @Override
            public void run() {
                DrawingToolLayer layer = getCurrentLayer();
                if (layer != null) {
                    layer.redo();
                }
                updateToolItems();
            }
        });
        redoAction.getAction().setImageDescriptor(
                IconUtil.getImageDescriptor(
                        com.raytheon.uf.viz.drawing.Activator.getDefault()
                                .getBundle(), "redo.gif"));

        eraseAction = new ActionContributionItem(
                new Action("Erase", SWT.TOGGLE) {
                    @Override
                    public void run() {
                        DrawingToolLayer layer = getCurrentLayer();
                        if (layer != null) {
                            if (layer.getDrawMode() == DrawMode.ERASE) {
                                layer.setDrawMode(DrawMode.NONE);
                            } else {
                                layer.setDrawMode(DrawMode.ERASE);
                            }
                            drawMode = layer.getDrawMode();
                        }
                        updateToolItems();
                    }
                });
        eraseAction.getAction().setImageDescriptor(
                IconUtil.getImageDescriptor(
                        com.raytheon.uf.viz.drawing.Activator.getDefault()
                                .getBundle(), "eraser.png"));

        clearAction = new ActionContributionItem(new Action("Clear") {
            public void run() {
                DrawingToolLayer layer = getCurrentLayer();
                if (layer != null) {
                    layer.clear();
                }
                updateToolItems();
            };
        });
        clearAction.getAction().setImageDescriptor(
                IconUtil.getImageDescriptor(
                        com.raytheon.uf.viz.drawing.Activator.getDefault()
                                .getBundle(), "remove.gif"));

        lockAction = new ActionContributionItem(new Action(
                "Lock Collaborators", SWT.TOGGLE) {
            public void run() {
                CollaborationDrawingResource resource = getCurrentDrawingResource();
                if (resource != null) {
                    resource.setLockingDrawing(((ToolItem) lockAction
                            .getWidget()).getSelection());
                    locked = resource.isLockingDrawing();
                    updateToolItems();
                }
            };
        });
        lockAction.getAction().setImageDescriptor(
                IconUtil.getImageDescriptor(Activator.getDefault().getBundle(),
                        "lock.gif"));
        ToolBarManager mgr = (ToolBarManager) getViewSite().getActionBars()
                .getToolBarManager();
        mgr.insert(mgr.getSize() - 1, drawAction);

        mgr.insert(mgr.getSize() - 1, undoAction);
        mgr.insert(mgr.getSize() - 1, redoAction);
        mgr.insert(mgr.getSize() - 1, clearAction);
        mgr.insert(mgr.getSize() - 1, eraseAction);
        mgr.insert(mgr.getSize() - 1, lockAction);
        mgr.insert(mgr.getSize() - 1, new Separator());

        updateToolItems();
    }

    public void updateToolItems() {
        CollaborationDrawingResource currentResource = getCurrentDrawingResource();
        DrawingToolLayer layer = null;
        if (currentResource != null) {
            layer = currentResource.getDrawingLayerFor(currentResource
                    .getMyUser());
        }
        if (layer != null
                && (currentResource.isLockingDrawing() == false || currentResource
                        .isSessionLeader())) {
            drawAction.getAction().setEnabled(true);
            undoAction.getAction().setEnabled(layer.canUndo());
            redoAction.getAction().setEnabled(layer.canRedo());
            clearAction.getAction().setEnabled(layer.canClear());
            eraseAction.getAction().setEnabled(true);
            switch (layer.getDrawMode()) {
            case DRAW:
                drawAction.getAction().setChecked(true);
                eraseAction.getAction().setChecked(false);
                break;
            case ERASE:
                drawAction.getAction().setChecked(false);
                eraseAction.getAction().setChecked(true);
                break;
            case NONE:
                drawAction.getAction().setChecked(false);
                eraseAction.getAction().setChecked(false);
                break;
            }
            lockAction.getAction().setChecked(
                    currentResource.isLockingDrawing());
        } else {
            drawAction.getAction().setEnabled(false);
            undoAction.getAction().setEnabled(false);
            redoAction.getAction().setEnabled(false);
            clearAction.getAction().setEnabled(false);
            eraseAction.getAction().setEnabled(false);
            lockAction.getAction().setEnabled(false);
        }
    }

    /**
     * @return the redoAction
     */
    public ActionContributionItem getRedoAction() {
        return redoAction;
    }

    /**
     * @return the undoAction
     */
    public ActionContributionItem getUndoAction() {
        return undoAction;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.collaboration.ui.session.SessionView#initColorManager
     * ()
     */
    @Override
    protected void initColorManager() {
        manager = SharedDisplaySessionMgr.getSessionContainer(sessionId)
                .getColorManager();
    }

    @Subscribe
    public void refreshAfterTransfer(TransferRoleCommand command) {
        VizApp.runAsync(new Runnable() {
            @Override
            public void run() {
                usersTable.refresh();
            }
        });
    }

    @Override
    protected String getSessionImageName() {
        return COLLABORATION_SESSION_IMAGE_NAME;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.collaboration.ui.session.SessionView#sendMessage()
     */
    @Override
    public void sendMessage() {
        String message = getComposedMessage();
        if (message.length() > 0) {
            try {
                UserId id = CollaborationConnection.getConnection().getUser();
                appendMessage(id, System.currentTimeMillis(), message);
                ((IVenueSession) session).sendChatMessage(message);
            } catch (CollaborationException e) {
                statusHandler.handle(Priority.ERROR,
                        "Unable to send chat message", e);
            }
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.collaboration.ui.session.SessionView#fillContextMenu
     * (org.eclipse.jface.action.IMenuManager)
     */
    @Override
    protected void fillContextMenu(IMenuManager manager) {
        super.fillContextMenu(manager);
        if (session.hasRole(SharedDisplayRole.DATA_PROVIDER)
                || session.hasRole(SharedDisplayRole.SESSION_LEADER)) {
            if (session.hasRole(SharedDisplayRole.SESSION_LEADER)) {
                manager.add(new Separator());
                manager.add(colorChangeAction);
            }
        }
    }

    @Subscribe
    public void modifyColors(ColorChangeEvent event) {
        SharedDisplaySessionMgr.getSessionContainer(sessionId)
                .getColorManager()
                .setColorForUser(event.getUserName(), event.getColor());
        VizApp.runAsync(new Runnable() {
            @Override
            public void run() {
                usersTable.refresh();
            }
        });
    }

    @Subscribe
    public void collaborationEvent(CollaborationDrawingEvent event) {
        VizApp.runAsync(actionUpdater);
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.viz.collaboration.ui.session.AbstractSessionView#
     * setMessageLabel(org.eclipse.swt.widgets.Label)
     */
    @Override
    protected void setMessageLabel(Composite comp) {
        Label label = new Label(comp, SWT.NONE);
        StringBuilder labelInfo = new StringBuilder();
        if (session != null) {
            IVenueInfo info = ((IVenueSession) session).getVenue().getInfo();
            labelInfo.append(info.getVenueSubject());
            label.setToolTipText(info.getVenueSubject());
        }
        label.setText(labelInfo.toString());
    }

    public String getSessionId() {
        return session.getSessionId();
    }

    @Override
    public void dispose() {
        ICollaborationEditor assocEditor = SharedDisplaySessionMgr
                .getSessionContainer(session.getSessionId())
                .getCollaborationEditor();
        if (assocEditor != null) {
            IWorkbenchPage page = VizWorkbenchManager.getInstance()
                    .getCurrentWindow().getActivePage();
            if (page != null) {
                page.closeEditor(assocEditor, false);
            }
        }
        SharedDisplaySessionMgr.exitSession(session.getSessionId());
        session.close();
        super.dispose();
        getSite().getPage().removePartListener(this);
        if (container != null) {
            container.removeRemoteDisplayChangedListener(this);
        }
    }

    // =================== Context activation code ===================

    /*
     * (non-Javadoc)
     * 
     * @see
     * org.eclipse.ui.IPartListener#partActivated(org.eclipse.ui.IWorkbenchPart)
     */
    @Override
    public void partActivated(IWorkbenchPart part) {
        // only done if we care about the part that was activated
        SessionContainer sc = SharedDisplaySessionMgr
                .getSessionContainer(sessionId);
        List<IEditorPart> editors = new ArrayList<IEditorPart>();
        if (sc.getCollaborationEditor() == null) {
            editors.addAll(SharedEditorsManager.getManager(sc.getSession())
                    .getSharedEditors());
        } else {
            editors.add(sc.getCollaborationEditor());
        }

        if (this == part || editors.contains(part)) {
            ContextManager
                    .getInstance(getSite().getPage().getWorkbenchWindow())
                    .activateContexts(this);
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * org.eclipse.ui.IPartListener#partBroughtToTop(org.eclipse.ui.IWorkbenchPart
     * )
     */
    @Override
    public void partBroughtToTop(IWorkbenchPart part) {
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * org.eclipse.ui.IPartListener#partClosed(org.eclipse.ui.IWorkbenchPart)
     */
    @Override
    public void partClosed(IWorkbenchPart part) {
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * org.eclipse.ui.IPartListener#partDeactivated(org.eclipse.ui.IWorkbenchPart
     * )
     */
    @Override
    public void partDeactivated(IWorkbenchPart part) {
        // only done if we care about the part that was deactivated
        SessionContainer sc = SharedDisplaySessionMgr
                .getSessionContainer(sessionId);
        List<IEditorPart> editors = new ArrayList<IEditorPart>();
        if (sc.getCollaborationEditor() == null) {
            editors.addAll(SharedEditorsManager.getManager(sc.getSession())
                    .getSharedEditors());
        } else {
            editors.add(sc.getCollaborationEditor());
        }

        if (this == part || editors.contains(part)) {
            ContextManager
                    .getInstance(getSite().getPage().getWorkbenchWindow())
                    .deactivateContexts(this);
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * org.eclipse.ui.IPartListener#partOpened(org.eclipse.ui.IWorkbenchPart)
     */
    @Override
    public void partOpened(IWorkbenchPart part) {
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.viz.collaboration.display.IRemoteDisplayContainer.
     * IRemoteDisplayChangedListener
     * #remoteDisplayChanged(com.raytheon.uf.viz.core
     * .drawables.IRenderableDisplay,
     * com.raytheon.uf.viz.collaboration.display.IRemoteDisplayContainer
     * .RemoteDisplayChangeType)
     */
    @Override
    public void remoteDisplayChanged(RemoteDisplay remoteDisplay,
            RemoteDisplayChangeType changeType) {
        IRenderableDisplay display = remoteDisplay.getDisplay();
        int displayId = remoteDisplay.getDisplayId();
        switch (changeType) {
        case CREATED:
            if (listeners.containsKey(display) == false) {
                CollaborationDrawingResourceData resourceData = new CollaborationDrawingResourceData();
                resourceData.setSessionId(sessionId);
                resourceData.setDisplayId(displayId);
                try {
                    listeners.put(display,
                            new SelfAddingSystemResourceListener(resourceData,
                                    display.getDescriptor()));
                    display.refresh();
                } catch (VizException e) {
                    Activator.statusHandler.handle(Priority.PROBLEM,
                            e.getLocalizedMessage(), e);
                }
            }
            break;
        case ACTIVATED:
            if (listeners.containsKey(display) == false) {
                remoteDisplayChanged(remoteDisplay,
                        RemoteDisplayChangeType.CREATED);
            }
            currentDisplay = display;
            CollaborationDrawingResource resource = getCurrentDrawingResource();
            if (resource != null && resource.isSessionLeader()) {
                resource.setLockingDrawing(locked);
                resource.getDrawingLayerFor(resource.getMyUser()).setDrawMode(
                        drawMode);
            }
            VizApp.runAsync(actionUpdater);
            break;
        case DISPOSED:
            SelfAddingSystemResourceListener listener = listeners
                    .remove(display);
            if (listener != null) {
                listener.dispose();
            }
            if (display == currentDisplay) {
                currentDisplay = null;
                VizApp.runAsync(actionUpdater);
            }
            break;
        }
    }
}
