package com.raytheon.uf.viz.collaboration.ui;

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

import java.io.IOException;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import org.eclipse.jface.action.Action;
import org.eclipse.jface.action.ActionContributionItem;
import org.eclipse.jface.action.IMenuCreator;
import org.eclipse.jface.action.IMenuListener;
import org.eclipse.jface.action.IMenuManager;
import org.eclipse.jface.action.IToolBarManager;
import org.eclipse.jface.action.MenuManager;
import org.eclipse.jface.action.Separator;
import org.eclipse.jface.viewers.DoubleClickEvent;
import org.eclipse.jface.viewers.IDoubleClickListener;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.viewers.TreeSelection;
import org.eclipse.jface.viewers.TreeViewer;
import org.eclipse.swt.SWT;
import org.eclipse.swt.custom.TreeEditor;
import org.eclipse.swt.events.KeyAdapter;
import org.eclipse.swt.events.KeyEvent;
import org.eclipse.swt.events.ModifyEvent;
import org.eclipse.swt.events.ModifyListener;
import org.eclipse.swt.events.MouseEvent;
import org.eclipse.swt.events.MouseTrackAdapter;
import org.eclipse.swt.graphics.GC;
import org.eclipse.swt.graphics.Point;
import org.eclipse.swt.graphics.Rectangle;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Event;
import org.eclipse.swt.widgets.Listener;
import org.eclipse.swt.widgets.Menu;
import org.eclipse.swt.widgets.MessageBox;
import org.eclipse.swt.widgets.Text;
import org.eclipse.swt.widgets.TreeColumn;
import org.eclipse.swt.widgets.TreeItem;
import org.eclipse.ui.IEditorPart;
import org.eclipse.ui.IEditorReference;
import org.eclipse.ui.IPartListener;
import org.eclipse.ui.IViewPart;
import org.eclipse.ui.IViewReference;
import org.eclipse.ui.IWorkbenchPage;
import org.eclipse.ui.IWorkbenchPart;
import org.eclipse.ui.PartInitException;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.part.ViewPart;
import org.osgi.framework.Bundle;

import com.google.common.eventbus.Subscribe;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.viz.collaboration.comm.identity.CollaborationException;
import com.raytheon.uf.viz.collaboration.comm.identity.IPresence;
import com.raytheon.uf.viz.collaboration.comm.identity.IPresence.Mode;
import com.raytheon.uf.viz.collaboration.comm.identity.IPresence.Type;
import com.raytheon.uf.viz.collaboration.comm.identity.ISession;
import com.raytheon.uf.viz.collaboration.comm.identity.ISharedDisplaySession;
import com.raytheon.uf.viz.collaboration.comm.identity.IVenueSession;
import com.raytheon.uf.viz.collaboration.comm.identity.event.IRosterChangeEvent;
import com.raytheon.uf.viz.collaboration.comm.identity.info.IVenueInfo;
import com.raytheon.uf.viz.collaboration.comm.identity.invite.SharedDisplayVenueInvite;
import com.raytheon.uf.viz.collaboration.comm.identity.invite.VenueInvite;
import com.raytheon.uf.viz.collaboration.comm.identity.roster.IRosterEntry;
import com.raytheon.uf.viz.collaboration.comm.identity.roster.IRosterGroup;
import com.raytheon.uf.viz.collaboration.comm.identity.roster.IRosterItem;
import com.raytheon.uf.viz.collaboration.comm.identity.roster.IRosterManager;
import com.raytheon.uf.viz.collaboration.comm.identity.user.IQualifiedID;
import com.raytheon.uf.viz.collaboration.comm.provider.session.CollaborationConnection;
import com.raytheon.uf.viz.collaboration.comm.provider.user.IDConverter;
import com.raytheon.uf.viz.collaboration.comm.provider.user.UserId;
import com.raytheon.uf.viz.collaboration.data.CollaborationDataManager;
import com.raytheon.uf.viz.collaboration.data.CollaborationGroupContainer;
import com.raytheon.uf.viz.collaboration.data.SessionGroupContainer;
import com.raytheon.uf.viz.collaboration.data.SharedDisplaySessionMgr;
import com.raytheon.uf.viz.collaboration.display.editor.CollaborationEditor;
import com.raytheon.uf.viz.collaboration.ui.login.ChangeStatusDialog;
import com.raytheon.uf.viz.collaboration.ui.prefs.CollabPrefConstants;
import com.raytheon.uf.viz.collaboration.ui.session.AbstractSessionView;
import com.raytheon.uf.viz.collaboration.ui.session.CollaborationSessionView;
import com.raytheon.uf.viz.collaboration.ui.session.PeerToPeerView;
import com.raytheon.uf.viz.collaboration.ui.session.SessionView;
import com.raytheon.uf.viz.core.VizApp;
import com.raytheon.uf.viz.core.icon.IconUtil;
import com.raytheon.uf.viz.drawing.PathToolbar;

/**
 * This class is the main view to display the user's information and allow the
 * user to create sessions.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Mar 1, 2012             rferrel     Initial creation
 * 
 * </pre>
 * 
 * @author rferrel
 * @version 1.0
 */
public class CollaborationGroupView extends ViewPart implements IPartListener {
    public static final String ID = "com.raytheon.uf.viz.collaboration.ui.CollaborationGroupView";

    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(CollaborationGroupView.class);

    private SessionGroupContainer activeSessionGroup;

    private TreeViewer usersTreeViewer;

    private CollaborationGroupContainer topLevel;

    private Action createSessionAction;

    private Action linkToEditorAction;

    private Action inviteAction;

    private Action joinAction;

    private Action peerToPeerChatAction;

    private Action logonAction;

    private Action logoutAction;

    private Action aliasAction;

    private Action addGroupAction;

    private Action addUserAction;

    private Action selectGroups;

    private Action changeStatusMessageAction;

    private Action changeStatusAction;

    private Action changePasswordAction;

    private Action drawToolbarAction;

    private Action collapseAllAction;

    private TreeEditor treeEditor;

    /**
     * @param parent
     */
    @Override
    public void createPartControl(Composite parent) {
        createActions();
        createToolbar();
        createMenubar();

        getViewSite().getWorkbenchWindow().getPartService()
                .addPartListener(this);

        createUsersTree(parent);
        addDoubleClickListeners();
        createContextMenu();
        if (CollaborationDataManager.getInstance().isConnected() == false) {
            usersTreeViewer.getTree().setEnabled(false);
        }
    }

    /**
     * 
     */
    private void createActions() {
        Bundle bundle = Activator.getDefault().getBundle();

        createSessionAction = new Action("Create Session...") {
            @Override
            public void run() {
                createSession();
            }

        };
        createSessionAction.setImageDescriptor(IconUtil.getImageDescriptor(
                bundle, "add_collaborate.gif"));
        createSessionAction.setEnabled(CollaborationDataManager.getInstance()
                .isConnected());

        linkToEditorAction = new Action("Link Editor to Chat Session",
                Action.AS_CHECK_BOX) {
            @Override
            public void run() {
                // TODO store to preferences
            }
        };
        linkToEditorAction.setImageDescriptor(IconUtil.getImageDescriptor(
                bundle, "link_to_editor.gif"));
        // TODO pull from prefs
        // linkToEditorAction.setChecked(CollaborationDataManager.getInstance()
        // .getLinkCollaboration());

        inviteAction = new Action("Invite...") {
            @Override
            public void run() {
                String sessionId = getId();
                IVenueSession session = CollaborationDataManager.getInstance()
                        .getSession(sessionId);
                String roomName = session.getVenue().getInfo()
                        .getVenueDescription();
                List<UserId> ids = new ArrayList<UserId>();

                for (IRosterEntry user : getSelectedUsers()) {
                    UserId id = user.getUser();
                    System.out.println("Add Selected User: " + id);
                    ids.add(id);
                }
                IVenueInfo info = session.getVenue().getInfo();
                System.out.println("room: " + info.getVenueName());
                System.out.println("subject: "
                        + session.getVenue().getInfo().getVenueSubject());
                try {
                    VenueInvite invite = null;
                    if (session instanceof ISharedDisplaySession) {
                        invite = buildDisplayInvite(sessionId, session
                                .getVenue().getInfo().getVenueSubject(), "");
                    } else {
                        invite = buildInvite(sessionId, session.getVenue()
                                .getInfo().getVenueSubject(), "");
                    }
                    session.sendInvitation(ids, invite);
                } catch (CollaborationException e) {
                    statusHandler.handle(Priority.PROBLEM,
                            "Error sending invitiation", e);
                }
            };
        };
        inviteAction.setImageDescriptor(IconUtil.getImageDescriptor(bundle,
                "invite.gif"));
        inviteAction
                .setToolTipText("Invite selected user(s) to join a session.");

        joinAction = new Action("Open Session") {
            @Override
            public void run() {
                // TODO, maybe need to switch tab to the session here?, or is
                // this even needed since we are already part of the session?
            }
        };

        peerToPeerChatAction = new Action("Chat") {
            @Override
            public void run() {
                createP2PChat(IDConverter.convertFrom(getId()));
            }
        };
        peerToPeerChatAction.setImageDescriptor(IconUtil.getImageDescriptor(
                bundle, "chats.gif"));

        logonAction = new Action("Login...") {
            @Override
            public void run() {
                populateTree();
            }
        };
        logonAction.setImageDescriptor(IconUtil.getImageDescriptor(bundle,
                "login.png"));

        logoutAction = new Action("Logout") {
            @Override
            public void run() {
                performLogout();
            }
        };

        logoutAction.setImageDescriptor(IconUtil.getImageDescriptor(bundle,
                "logout.gif"));

        aliasAction = new Action("Alias") {
            @Override
            public void run() {
                aliasItem();
            };
        };

        addUserAction = new Action("Add User") {
            public void run() {
                // addUsersToGroup();
                nyiFeature("Add User to a Group");
            };
        };
        addUserAction.setImageDescriptor(IconUtil.getImageDescriptor(bundle,
                "add_contact.gif"));
        addUserAction.setEnabled(false);

        addGroupAction = new Action("Create Group") {
            public void run() {
                System.out.println("Create Group here");
                nyiFeature("Create Group");
            };
        };
        addGroupAction.setImageDescriptor(IconUtil.getImageDescriptor(bundle,
                "add_group.gif"));
        addGroupAction.setEnabled(false);

        selectGroups = new Action("Select System Groups...") {
            public void run() {
                System.out.println("Select System Groups to Display...");
                nyiFeature("Select System Groups.");
            }
        };
        selectGroups.setEnabled(false);

        changeStatusMessageAction = new Action("Change Status Message...") {
            public void run() {
                changeStatusMessage();
            };
        };
        changeStatusMessageAction.setEnabled(false);

        changePasswordAction = new Action("Change Password...") {
            public void run() {
                changePassword();
            };
        };
        changePasswordAction.setEnabled(false);

        changeStatusAction = new Action("Change Status",
                Action.AS_DROP_DOWN_MENU) {
            public void run() {
                Activator.getDefault().getPreferenceStore()
                        .setValue(CollabPrefConstants.P_STATUS, this.getId());
                changeStatus();
            };
        };
        changeStatusAction.setEnabled(false);

        // refreshActiveSessionsAction = new Action("Refresh") {
        // public void run() {
        // System.out.println("Refresh Active Sessions");
        // refreshActiveSessions();
        // }
        // };
        // refreshActiveSessionsAction.setImageDescriptor(IconUtil
        // .getImageDescriptor(bundle, "refresh.gif"));
        // refreshActiveSessionsAction
        // .setToolTipText("Refresh the Active Sessions Entries.");

        collapseAllAction = new Action("Collapse All") {
            public void run() {
                usersTreeViewer.collapseAll();
            }
        };
        collapseAllAction.setImageDescriptor(IconUtil.getImageDescriptor(
                bundle, "collapseall.gif"));

        IMenuCreator creator = new IMenuCreator() {

            Menu menu;

            @Override
            public Menu getMenu(Menu parent) {
                menu = new Menu(parent);
                fillStatusMenu(menu);
                return menu;
            }

            @Override
            public Menu getMenu(Control parent) {
                menu = new Menu(parent);
                fillStatusMenu(menu);
                return menu;
            }

            @Override
            public void dispose() {
                menu.dispose();
            }
        };
        changeStatusAction.setMenuCreator(creator);

        drawToolbarAction = new Action("Drawing Toolbar") {
            @Override
            public void run() {
                PathToolbar.getToolbar().open();
            }
        };
        drawToolbarAction.setImageDescriptor(IconUtil.getImageDescriptor(
                com.raytheon.uf.viz.drawing.Activator.getDefault().getBundle(),
                "draw.gif"));
    }

    private void changePassword() {
        ChangePasswordDialog dialog = new ChangePasswordDialog(Display
                .getCurrent().getActiveShell());
        dialog.open();

        Object result = dialog.getReturnValue();
        if (result != null) {
            char[] password = result.toString().toCharArray();
            CollaborationConnection sessionManager = CollaborationDataManager
                    .getInstance().getCollaborationConnection();
            try {
                sessionManager.getAccountManager().changePassword(password);
            } catch (CollaborationException e) {
                statusHandler.handle(Priority.PROBLEM,
                        "Unable to change password", e);
            }
        }
    }

    private void changeStatusMessage() {
        ChangeStatusDialog dialog = new ChangeStatusDialog(Display.getCurrent()
                .getActiveShell());
        dialog.open();
        changeStatus();
    }

    private void changeStatus() {
        Mode mode = Mode.valueOf(Activator.getDefault().getPreferenceStore()
                .getString(CollabPrefConstants.P_STATUS));
        String msg = Activator.getDefault().getPreferenceStore()
                .getString(CollabPrefConstants.P_MESSAGE);
        CollaborationDataManager.getInstance().fireModifiedPresence(mode, msg);
    }

    /**
     * This displays a warning dialog then closes all collaboration views and
     * disconnects from the server.
     */
    private void performLogout() {
        MessageBox messageBox = new MessageBox(Display.getCurrent()
                .getActiveShell(), SWT.ICON_WARNING | SWT.OK | SWT.CANCEL);
        messageBox.setText("Log Out of Collaboration");
        messageBox.setMessage("Logging out will sever your\n"
                + "connection to the server and\n"
                + "close all collaboration views\n" + "and editors.");
        int result = messageBox.open();
        if (result == SWT.OK) {
            // Close all Session Views
            PlatformUI.getWorkbench().getActiveWorkbenchWindow()
                    .getActivePage().hideView(this);
            for (IViewReference ref : PlatformUI.getWorkbench()
                    .getActiveWorkbenchWindow().getActivePage()
                    .getViewReferences()) {
                IViewPart view = ref.getView(false);
                if (view instanceof AbstractSessionView) {
                    PlatformUI.getWorkbench().getActiveWorkbenchWindow()
                            .getActivePage().hideView(view);
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
                statusHandler.handle(Priority.WARN,
                        "Unable to save preferences", e);
            }
            CollaborationDataManager.getInstance().closeManager();
        }
    }

    /**
     * 
     */
    protected void aliasItem() {
        Control oldEditor = treeEditor.getEditor();
        if (oldEditor != null) {
            oldEditor.dispose();
        }
        TreeSelection selection = (TreeSelection) usersTreeViewer
                .getSelection();
        final IRosterEntry entry = (IRosterEntry) selection.getFirstElement();
        final Composite comp = new Composite(usersTreeViewer.getTree(),
                SWT.NONE);
        comp.setBackground(Display.getCurrent().getSystemColor(SWT.COLOR_BLACK));
        final Text text = new Text(comp, SWT.NONE);

        text.addModifyListener(new ModifyListener() {
            public void modifyText(ModifyEvent e) {
                Text text = (Text) treeEditor.getEditor();
                treeEditor.getItem().setText(text.getText());
            }
        });

        text.addKeyListener(new KeyAdapter() {
            @Override
            public void keyReleased(KeyEvent e) {
                if (e.keyCode == SWT.CR || e.keyCode == SWT.KEYPAD_CR) {
                    entry.getUser().setAlias(treeEditor.getItem().getText());
                    CollaborationUtils.addAlias();
                    CollaborationDataManager.getInstance()
                            .getCollaborationConnection().getEventPublisher()
                            .post(entry.getUser());
                }
            }
        });

        final TreeItem[] lastItem = new TreeItem[1];
        // usersTreeViewer.getTree().addListener(SWT.Selection, new Listener() {
        // public void handleEvent(Event event) {
        // final TreeItem item = (TreeItem) event.item;
        // if (item != null && item == lastItem[0]) {
        final TreeItem item = usersTreeViewer.getTree().getSelection()[0];
        boolean showBorder = true;
        final Composite composite = new Composite(usersTreeViewer.getTree(),
                SWT.NONE);
        if (showBorder)
            composite.setBackground(Display.getCurrent().getSystemColor(
                    SWT.COLOR_BLACK));
        final Text modText = new Text(composite, SWT.NONE);
        final int inset = showBorder ? 1 : 0;
        composite.addListener(SWT.Resize, new Listener() {
            public void handleEvent(Event e) {
                Rectangle rect = composite.getClientArea();
                modText.setBounds(rect.x + inset, rect.y + inset, rect.width
                        - inset * 2, rect.height - inset * 2);
            }
        });
        Listener textListener = new Listener() {
            public void handleEvent(final Event e) {
                switch (e.type) {
                case SWT.KeyUp:
                    if (e.keyCode == SWT.CR || e.keyCode == SWT.KEYPAD_CR) {
                        // do nothing, want to go on to the focus out
                    } else {
                        break;
                    }
                case SWT.FocusOut:
                    item.setText(modText.getText());
                    composite.dispose();
                    entry.getUser().setAlias(treeEditor.getItem().getText());
                    CollaborationUtils.addAlias();
                    CollaborationDataManager.getInstance()
                            .getCollaborationConnection().getEventPublisher()
                            .post(entry.getUser());
                    break;
                case SWT.Verify:
                    String newText = modText.getText();
                    String leftText = newText.substring(0, e.start);
                    String rightText = newText.substring(e.end,
                            newText.length());
                    GC gc = new GC(modText);
                    Point size = gc.textExtent(leftText + e.text + rightText);
                    gc.dispose();
                    size = modText.computeSize(size.x, SWT.DEFAULT);
                    treeEditor.horizontalAlignment = SWT.LEFT;
                    Rectangle itemRect = item.getBounds(),
                    rect = usersTreeViewer.getTree().getClientArea();
                    treeEditor.minimumWidth = Math.max(size.x, itemRect.width)
                            + inset * 2;
                    int left = itemRect.x,
                    right = rect.x + rect.width;
                    treeEditor.minimumWidth = Math.min(treeEditor.minimumWidth,
                            right - left);
                    treeEditor.minimumHeight = size.y + inset * 2;
                    treeEditor.layout();
                    break;
                }
            }
        };

        modText.addListener(SWT.KeyUp, textListener);
        modText.addListener(SWT.Verify, textListener);
        modText.addListener(SWT.FocusOut, textListener);
        treeEditor.setEditor(composite, item);
        modText.setText(item.getText());
        modText.selectAll();
        modText.setFocus();
        // }
        lastItem[0] = item;
        // }
        // });
    }

    private void fillStatusMenu(Menu menu) {
        for (int index = 0; index < CollaborationUtils.statusModes.length; ++index) {
            IPresence.Mode mode = CollaborationUtils.statusModes[index];
            Action action = new Action(mode.getMode()) {
                public void run() {
                    changeStatusAction.setId(getId());
                    changeStatusAction.run();
                };
            };
            action.setId(mode.toString());
            ActionContributionItem item = new ActionContributionItem(action);
            action.setImageDescriptor(IconUtil.getImageDescriptor(Activator
                    .getDefault().getBundle(), mode.name().toLowerCase()
                    + ".gif"));
            item.fill(menu, -1);
        }
    }

    /**
     * Create the toolbar on top of the group view
     */
    private void createToolbar() {
        IToolBarManager mgr = getViewSite().getActionBars().getToolBarManager();
        mgr.add(createSessionAction);
        mgr.add(collapseAllAction);
        mgr.add(linkToEditorAction);
    }

    /**
     * Create the menu bar that is shown when the user clicks the down arrow
     * next to the toolbar
     */
    private void createMenubar() {
        IMenuManager mgr = getViewSite().getActionBars().getMenuManager();
        createMenu(mgr);
        mgr.addMenuListener(new IMenuListener() {

            @Override
            public void menuAboutToShow(IMenuManager manager) {
                manager.removeAll();
                createMenu(manager);
            }
        });
    }

    private void createMenu(IMenuManager mgr) {
        mgr.add(addGroupAction);
        mgr.add(addUserAction);
        mgr.add(selectGroups);
        mgr.add(new Separator());
        mgr.add(changeStatusAction);
        mgr.add(changeStatusMessageAction);
        mgr.add(changePasswordAction);
        mgr.add(new Separator());

        mgr.add(drawToolbarAction);

        mgr.add(new Separator());
        if (CollaborationDataManager.getInstance().isConnected()) {
            mgr.add(logoutAction);
        } else {
            mgr.add(logonAction);
        }
    }

    private void createSession() {
        CollaborationDataManager manager = CollaborationDataManager
                .getInstance();
        CollaborationConnection sessionManager = manager
                .getCollaborationConnection();
        if (sessionManager == null) {
            System.err.println("Unable to get session manager");
            return;
        }

        CreateSessionDialog dialog = new CreateSessionDialog(Display
                .getCurrent().getActiveShell(), usersSelected());
        dialog.open();

        CreateSessionData result = (CreateSessionData) dialog.getReturnValue();

        if (result != null) {
            if (result.isCollaborationSession()) {
                createCollaborationView(result);
            } else {
                createTextOnlyView(result);
            }

            try {
                if (result.isInviteUsers()) {
                    IVenueSession session = CollaborationDataManager
                            .getInstance().getSession(result.getSessionId());
                    List<UserId> usersList = new ArrayList<UserId>();
                    for (IRosterEntry user : getSelectedUsers()) {
                        usersList.add(user.getUser());
                    }
                    String b = result.getInviteMessage();

                    VenueInvite invite = null;
                    if (session instanceof ISharedDisplaySession) {
                        invite = buildDisplayInvite(session.getSessionId(),
                                result.getSubject(), b);
                    } else {
                        invite = buildInvite(session.getSessionId(),
                                result.getSubject(), b);
                    }
                    session.sendInvitation(usersList, invite);
                }
            } catch (Exception e) {
                statusHandler.handle(Priority.ERROR,
                        "Error sending invitation", e);
            }
        }
    }

    private VenueInvite buildInvite(String sessionId, String subject,
            String body) {
        VenueInvite invite = new VenueInvite();
        invite.setMessage(body);
        invite.setSessionId(sessionId);
        invite.setSubject(subject);
        return invite;
    }

    private VenueInvite buildDisplayInvite(String sessionId, String subject,
            String msg) {
        SharedDisplayVenueInvite invite = new SharedDisplayVenueInvite();
        invite.setMessage(msg);
        invite.setSessionId(sessionId);
        invite.setSubject(subject);
        invite.setRGBColors(SharedDisplaySessionMgr
                .getSessionContainer(sessionId).getColorManager().getColors());
        invite.setDataProvider(SharedDisplaySessionMgr
                .getSessionContainer(sessionId).getSession()
                .getCurrentDataProvider());
        invite.setSessionLeader(SharedDisplaySessionMgr
                .getSessionContainer(sessionId).getSession()
                .getCurrentSessionLeader());
        return invite;
    }

    private void createCollaborationView(CreateSessionData result) {
        String sessionId = result.getSessionId();
        try {
            PlatformUI
                    .getWorkbench()
                    .getActiveWorkbenchWindow()
                    .getActivePage()
                    .showView(CollaborationSessionView.ID, sessionId,
                            IWorkbenchPage.VIEW_ACTIVATE);
        } catch (PartInitException e) {
            statusHandler.handle(Priority.PROBLEM,
                    "Unable to open collaboration sesson", e);
        } catch (Exception e) {
            statusHandler.handle(Priority.ERROR, "Unexpected excepton", e);
        }
    }

    /**
     * Generate a view for text only session.
     * 
     * @param result
     */
    private void createTextOnlyView(CreateSessionData result) {
        String sessionId = result.getSessionId();
        try {
            PlatformUI
                    .getWorkbench()
                    .getActiveWorkbenchWindow()
                    .getActivePage()
                    .showView(SessionView.ID, sessionId,
                            IWorkbenchPage.VIEW_ACTIVATE);
        } catch (PartInitException e) {
            statusHandler.handle(Priority.PROBLEM,
                    "Unable to open text  only chat session", e);
        } catch (Exception e) {
            statusHandler.handle(Priority.ERROR, "Unexpected exception", e);
        }
    }

    /**
     * Generate a view to for messages to/from a user via a Peer to Peer
     * connection.
     * 
     * @param user
     */
    private void createP2PChat(IQualifiedID peer) {
        try {
            String name = "";
            UserId id = (UserId) peer;
            if (id.getAlias() != null && !id.getAlias().isEmpty()) {
                name = id.getAlias();
            } else {
                name = id.getName();
            }
            PeerToPeerView p2pView = (PeerToPeerView) PlatformUI
                    .getWorkbench()
                    .getActiveWorkbenchWindow()
                    .getActivePage()
                    .showView(PeerToPeerView.ID, name,
                            IWorkbenchPage.VIEW_ACTIVATE);
            p2pView.setPeer(peer);
        } catch (PartInitException e) {
            statusHandler.handle(Priority.PROBLEM, "Unable to open chat", e);
        }
    }

    /**
     * Generate the Tree View component and add tooltip tracking.
     * 
     * @param parent
     */
    private void createUsersTree(Composite parent) {
        Composite child = new Composite(parent, SWT.NONE);
        child.setLayout(new GridLayout(1, false));
        child.setLayoutData(new GridData(SWT.FILL, SWT.FILL, true, true));
        usersTreeViewer = new TreeViewer(child);
        usersTreeViewer.getTree().setLayoutData(
                new GridData(SWT.FILL, SWT.FILL, true, true));

        TreeColumn column = new TreeColumn(usersTreeViewer.getTree(), SWT.NONE);
        column.setWidth(200); // any width would work

        usersTreeViewer.setContentProvider(new UsersTreeContentProvider());
        usersTreeViewer.setLabelProvider(new UsersTreeLabelProvider());
        usersTreeViewer.setSorter(new UsersTreeViewerSorter());
        topLevel = new CollaborationGroupContainer();
        usersTreeViewer.setInput(topLevel);

        treeEditor = new TreeEditor(usersTreeViewer.getTree());
        usersTreeViewer.getTree().addMouseTrackListener(
                new MouseTrackAdapter() {
                    @Override
                    public void mouseHover(MouseEvent e) {
                        TreeItem item = usersTreeViewer.getTree().getItem(
                                new Point(e.x, e.y));
                        if (item != null) {
                            Object node = item.getData();
                            StringBuilder builder = new StringBuilder();
                            if (node instanceof IRosterEntry) {
                                IRosterEntry user = (IRosterEntry) node;
                                builder.append("ID: ").append(
                                        user.getUser().getFQName());
                                builder.append("\nStatus: ");
                                if (user.getPresence().getType() == Type.UNAVAILABLE) {
                                    builder.append("Offline");
                                } else {
                                    builder.append(user.getPresence().getMode()
                                            .getMode());

                                    // builder.append("Type: ").append(user.getType())
                                    // .append("\n");
                                    String message = user.getPresence()
                                            .getStatusMessage();
                                    if (message != null && message.length() > 0) {
                                        builder.append("\n");
                                        builder.append("Message: \"").append(
                                                user.getPresence()
                                                        .getStatusMessage()
                                                        + "\"");
                                    }
                                }
                            }
                            // builds the tooltip text for the session group
                            // portion of the view
                            else if (node instanceof IVenueSession) {
                                IVenueSession sessGroup = (IVenueSession) node;
                                IVenueInfo info = sessGroup.getVenue()
                                        .getInfo();
                                builder.append("ID: ")
                                        .append(info.getVenueID());
                                builder.append("\nName: ")
                                        .append(info.getVenueDescription())
                                        .append("\n");
                                builder.append("Subject: ")
                                        .append(info.getVenueSubject())
                                        .append("\n");
                                builder.append("Participants: ").append(
                                        info.getParticipantCount());
                            }
                            usersTreeViewer.getTree().setToolTipText(
                                    builder.toString());
                        } else {
                            usersTreeViewer.getTree().setToolTipText("");
                        }
                    }
                });
    }

    private void createContextMenu() {
        MenuManager menuMgr = new MenuManager();
        menuMgr.setRemoveAllWhenShown(true);
        menuMgr.addMenuListener(new IMenuListener() {

            @Override
            public void menuAboutToShow(IMenuManager manager) {
                fillContextMenu(manager);
            }
        });
        Menu menu = menuMgr.createContextMenu(usersTreeViewer.getControl());
        usersTreeViewer.getControl().setMenu(menu);
        getSite().registerContextMenu(menuMgr, usersTreeViewer);
    }

    /**
     * Filling the context menu for the tree depending on whether the item is a
     * group or a user
     * 
     * @paramfillContextMenu manager
     */
    private void fillContextMenu(IMenuManager manager) {
        IStructuredSelection selection = (IStructuredSelection) usersTreeViewer
                .getSelection();
        Object o = selection.getFirstElement();

        // handle the session group portion of the group view
        if (o instanceof SessionGroupContainer) {
            manager.add(createSessionAction);
            return;
        } else if (o instanceof IVenueSession) {
            manager.add(joinAction);
            return;
        } else if (o instanceof UserId) {
            createMenu(manager);
            return;
        }

        // the user, both the logged in user as well as his buddies
        if (o instanceof IRosterEntry) {
            IRosterEntry user = (IRosterEntry) o;
            if (user.getPresence().getType() == Type.AVAILABLE) {
                MenuManager inviteManager = new MenuManager("Invite to...");
                // get current open chats
                Map<String, IVenueSession> sessions = CollaborationDataManager
                        .getInstance().getSessions();
                for (String name : sessions.keySet()) {
                    final ISession session = sessions.get(name);
                    if (session != null) {
                        final IVenueInfo info = sessions.get(name).getVenue()
                                .getInfo();
                        if (info != null) {
                            Action action = new Action(
                                    info.getVenueDescription()) {
                                /*
                                 * (non-Javadoc)
                                 * 
                                 * @see org.eclipse.jface.action.Action#run()
                                 */
                                @Override
                                public void run() {
                                    inviteAction.setId(session.getSessionId());
                                    inviteAction.run();
                                }
                            };
                            action.setId(info.getVenueID());
                            inviteManager.add(action);
                        }
                    }
                }
                manager.add(inviteManager);
                manager.add(peerToPeerChatAction);
                // use the fq name so that we know who we need to chat with
                peerToPeerChatAction.setId(user.getUser().getFQName());
                manager.add(new Separator());
                manager.add(createSessionAction);
            }
            manager.add(aliasAction);
        } else if (o instanceof IRosterGroup) {
            manager.add(createSessionAction);
        }
    }

    /**
     * Get entries for all part of the Tree Viewer and enable actions.
     */
    protected void populateTree() {
        CollaborationDataManager manager = CollaborationDataManager
                .getInstance();
        CollaborationConnection sessionManager = manager
                .getCollaborationConnection();
        topLevel.clear();
        // set all the menu actions to false to start with
        if (sessionManager == null) {
            usersTreeViewer.getTree().setEnabled(false);
            addGroupAction.setEnabled(false);
            addUserAction.setEnabled(false);
            selectGroups.setEnabled(false);
            changeStatusAction.setEnabled(false);
            drawToolbarAction.setEnabled(false);
            changeStatusMessageAction.setEnabled(false);
            changePasswordAction.setEnabled(false);
            return;
        }

        // enable all the actions
        addGroupAction.setEnabled(true);
        addUserAction.setEnabled(true);
        selectGroups.setEnabled(true);
        changeStatusAction.setEnabled(true);
        drawToolbarAction.setEnabled(true);
        changeStatusMessageAction.setEnabled(true);
        changePasswordAction.setEnabled(true);

        // make the first thing to show up in the list, which happens to be the
        // user's name and gives the user options to modify status and other
        // things
        UserId user = manager.getCollaborationConnection().getUser();
        topLevel.addObject(user);

        activeSessionGroup = new SessionGroupContainer();
        topLevel.addObject(activeSessionGroup);

        // populates the sessions that the user currently is involved with
        populateActiveSessions();

        // populates the groups that the user is a part of
        populateGroups();

        // enable the tree, and then refresh it just to be safe
        usersTreeViewer.getTree().setEnabled(true);
        usersTreeViewer.refresh(topLevel, true);
        createSessionAction.setEnabled(true);
    }

    /**
     * Clears and populates the Tree Viewer's active session node.
     */
    private void populateActiveSessions() {
        activeSessionGroup.clear();
        try {
            CollaborationDataManager manager = CollaborationDataManager
                    .getInstance();
            for (IViewReference ref : getViewSite().getWorkbenchWindow()
                    .getActivePage().getViewReferences()) {
                IViewPart viewPart = ref.getView(false);
                if (viewPart instanceof SessionView) {
                    String sessionId = viewPart.getViewSite().getSecondaryId();
                    activeSessionGroup.addObject(manager.getSession(sessionId));
                }
            }
        } catch (NullPointerException e) {
            // Ignore happens when creating view when starting CAVE.
            // TODO bad to ignore, need to take care of
            statusHandler.handle(Priority.ERROR,
                    "Unable to populate active sessions", e);
        }
    }

    /**
     * Clear and populate the groups from the roster manager entries.
     */
    private void populateGroups() {
        CollaborationDataManager manager = CollaborationDataManager
                .getInstance();

        // go through and clear out everything above the groups (my user, the
        // sessions)
        List<Object> obs = new ArrayList<Object>();
        obs.addAll(topLevel.getObjects());
        for (IRosterGroup node : manager.getCollaborationConnection()
                .getRosterManager().getRoster().getGroups()) {
            topLevel.addObject(node);
        }

        CollaborationUtils.readAliases();
        for (IRosterEntry node : manager.getCollaborationConnection()
                .getRosterManager().getRoster().getEntries()) {
            topLevel.addObject(node);
        }

        // topLevel.addObject(orphans);
    }

    /**
     * @return
     */
    private boolean usersSelected() {
        return getSelectedUsers().size() > 0;
    }

    /**
     * Get a unique set of selected users that have a Type of AVAILABLE. This
     * does a recursive search so will work even when groups contain groups.
     * 
     * @return
     */
    private Set<IRosterEntry> getSelectedUsers() {
        Set<IRosterEntry> selectedUsers = new HashSet<IRosterEntry>();
        IStructuredSelection selection = (IStructuredSelection) usersTreeViewer
                .getSelection();
        Object[] nodes = selection.toArray();

        for (Object node : nodes) {
            if (node instanceof IRosterEntry) {
                IRosterEntry user = (IRosterEntry) node;
                if (user.getPresence().getType() == Type.AVAILABLE) {
                    selectedUsers.add(user);
                }
            } else if (node instanceof IRosterGroup) {
                selectedUsers.addAll(getSelectedUsers((IRosterGroup) node));
            }
        }

        return selectedUsers;
    }

    /**
     * This recursively searches group Nodes and returns all users with Type
     * AVAILABLE.
     * 
     * @param groupNode
     * @return users
     */
    private Set<IRosterEntry> getSelectedUsers(IRosterGroup groupNode) {
        Set<IRosterEntry> selectedUsers = new HashSet<IRosterEntry>();
        for (IRosterItem node : groupNode.getEntries()) {
            if (node instanceof IRosterEntry) {
                IRosterEntry user = (IRosterEntry) node;
                if (user.getPresence().getType() == Type.AVAILABLE) {
                    selectedUsers.add((IRosterEntry) node);
                }
            } else if (node instanceof IRosterGroup) {
                selectedUsers.addAll(getSelectedUsers((IRosterGroup) node));
            }
        }
        return selectedUsers;
    }

    private void addUsersToGroup() {
        // TODO, one add user is implemented, remove this
        if (true) {
            nyiFeature("Add user is not yet implemented");
            return;
        }
        Set<IRosterEntry> users = getSelectedUsers();
        IStructuredSelection selection = (IStructuredSelection) usersTreeViewer
                .getSelection();
        Object[] nodes = selection.toArray();
        System.out.println("Add User: " + users.size());
        IRosterManager rosterManager = CollaborationDataManager.getInstance()
                .getCollaborationConnection().getRosterManager();
        for (Object node : nodes) {
            if (node instanceof IRosterEntry) {
                IRosterEntry user = (IRosterEntry) node;
                try {
                    UserId account = user.getUser();
                    String nickname = account.getAlias();
                    if (nickname == null || nickname.isEmpty()) {
                        nickname = account.getName();
                    }

                    String[] groups = null;
                    rosterManager.sendRosterAdd(account, groups);
                } catch (CollaborationException e) {
                    statusHandler.handle(Priority.PROBLEM,
                            e.getLocalizedMessage(), e);
                }
            }
        }
    }

    /**
     * This should go away as all actions are implemented.
     * 
     * @param feature
     */
    private void nyiFeature(String feature) {
        MessageBox messageBox = new MessageBox(Display.getCurrent()
                .getActiveShell(), SWT.ICON_INFORMATION);
        messageBox.setText("Not Yet Implemented");
        messageBox.setMessage(feature);
        messageBox.open();
    }

    /*
     * (non-Javadoc)
     * 
     * @see org.eclipse.ui.part.WorkbenchPart#setFocus()
     */
    @Override
    public void setFocus() {
    }

    private void addDoubleClickListeners() {
        usersTreeViewer.addDoubleClickListener(new IDoubleClickListener() {
            @Override
            public void doubleClick(DoubleClickEvent event) {
                TreeSelection selection = (TreeSelection) event.getSelection();
                Object node = selection.getFirstElement();
                if (node instanceof SessionGroupContainer) {
                    // SessionGroup group = (SessionGroup) node;
                    // if (!group.isSessionRoot()) {
                    // createJoinCollaboration();
                    // }
                } else if (node instanceof IRosterEntry) {
                    IRosterEntry user = (IRosterEntry) node;
                    if (user.getPresence().getType() == Type.AVAILABLE) {
                        UserId loginUserId = CollaborationDataManager
                                .getInstance().getCollaborationConnection()
                                .getUser();
                        if (!loginUserId.equals(user)) {
                            createP2PChat(user.getUser());
                        }
                    }
                }
            }
        });
    }

    @Override
    public void dispose() {
        super.dispose();
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * org.eclipse.ui.IPartListener#partActivated(org.eclipse.ui.IWorkbenchPart)
     */
    @Override
    public void partActivated(IWorkbenchPart part) {
        if (linkToEditorAction.isChecked()) {
            IWorkbenchPage page = PlatformUI.getWorkbench()
                    .getActiveWorkbenchWindow().getActivePage();
            if (part instanceof CollaborationEditor) {
                String sessionId = ((CollaborationEditor) part).getSessionId();
                for (IViewReference ref : page.getViewReferences()) {
                    if (ref.getPart(false) instanceof CollaborationSessionView) {
                        CollaborationSessionView view = (CollaborationSessionView) ref
                                .getPart(false);
                        if (view.getSessionId().equals(sessionId)) {
                            page.bringToTop(view);
                            break;
                        }
                    }
                }
            } else if (part instanceof CollaborationSessionView) {
                String sessionId = ((CollaborationSessionView) part)
                        .getSessionId();
                CollaborationEditor editor = SharedDisplaySessionMgr
                        .getSessionContainer(sessionId)
                        .getCollaborationEditor();
                if (editor != null) {
                    page.bringToTop(editor);
                }
            }
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
        if (part instanceof SessionView) {
            SessionView sessionView = (SessionView) part;
            String sessionId = sessionView.getViewSite().getSecondaryId();
            for (Object node : activeSessionGroup.getObjects()) {
                IVenueSession group = (IVenueSession) node;
                if (group.getVenue() == null
                        || sessionId.equals(group.getVenue().getInfo()
                                .getVenueID())) {
                    activeSessionGroup.removeObject(node);
                    usersTreeViewer.refresh(activeSessionGroup);
                    break;
                }
            }
        } else if (part == this) {
            CollaborationConnection connection = CollaborationDataManager
                    .getInstance().getCollaborationConnection();
            if (connection != null) {
                connection.unRegisterEventHandler(this);
            }
            getViewSite().getWorkbenchWindow().getPartService()
                    .removePartListener(this);
        }
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
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * org.eclipse.ui.IPartListener#partOpened(org.eclipse.ui.IWorkbenchPart)
     */
    @Override
    public void partOpened(IWorkbenchPart part) {
        if (part instanceof SessionView) {
            SessionView sessionView = (SessionView) part;
            String sessionId = sessionView.getViewSite().getSecondaryId();
            IVenueSession session = CollaborationDataManager.getInstance()
                    .getSession(sessionId);
            activeSessionGroup.addObject(session);
            session.registerEventHandler(sessionView);
            usersTreeViewer.refresh(activeSessionGroup);
        } else if (part == this) {
            CollaborationConnection connection = CollaborationDataManager
                    .getInstance().getCollaborationConnection();
            if (connection != null) {
                connection.registerEventHandler(this);
            }
            populateTree();
            usersTreeViewer.refresh();
        }
    }

    /**
     * Refresh the labels on the View Tree to reflect presence change.
     * 
     * @param rosterEntry
     */
    @Subscribe
    public void handleModifiedPresence(final IRosterEntry rosterEntry) {
        // Only need to update the usersTreeViewer.
        System.out.println("group view roster entry for:"
                + rosterEntry.getUser().getName() + "@"
                + rosterEntry.getUser().getHost() + " "
                + rosterEntry.getPresence().getMode() + "/"
                + rosterEntry.getPresence().getType());

        VizApp.runAsync(new Runnable() {

            @Override
            public void run() {
                for (IRosterGroup group : rosterEntry.getGroups()) {
                    refreshUser(rosterEntry.getUser(), group);
                }
                usersTreeViewer.refresh(topLevel, true);
            }

        });
    }

    private void refreshUser(UserId userId, IRosterGroup group) {
        for (Object child : group.getEntries()) {
            if (child instanceof IRosterEntry) {
                if (userId.equals(((IRosterEntry) child).getUser())) {
                    usersTreeViewer.refresh(child, true);
                }
            } else if (child instanceof IRosterGroup) {
                refreshUser(userId, (IRosterGroup) child);
            }
        }
    }

    @Subscribe
    public void handleRosterChangeEvent(IRosterChangeEvent rosterChangeEvent) {
        // Assume the CollaborationDataManager has updated the back end.
        // Only need to update the usersTreeView.
        // System.out.println("CollaborationGroupView rosterChangeEvent<"
        // + rosterChangeEvent.getType() + ">: "
        // + rosterChangeEvent.getEntry().getUser().getFQName());
        IRosterEntry rosterEntry = rosterChangeEvent.getEntry();
        UserId userId = rosterEntry.getUser();
        List<String> groupIds = new ArrayList<String>();
        for (IRosterGroup rosterGroup : rosterEntry.getGroups()) {
            groupIds.add(rosterGroup.getName());
        }
        switch (rosterChangeEvent.getType()) {
        case ADD:
            // Should be a rare event after initial population.
            // OrphanGroup orphanGroup = null;
            // if (groupIds.size() == 0) {
            // // remove from all groups and add to Orphans.
            // for (CollaborationNode node : topLevel.getChildren()) {
            // if (node instanceof OrphanGroup) {
            // CollaborationUser user = null;
            // orphanGroup = (OrphanGroup) node;
            // for (CollaborationNode child : orphanGroup
            // .getChildren()) {
            // if (userId.equals(child.getId())) {
            // user = (IRosterEntry) child;
            // break;
            // }
            // }
            // } else if (node instanceof IRosterGroup
            // && !(node instanceof SessionGroup)) {
            // IRosterGroup groupNode = (IRosterGroup) node;
            // for (CollaborationNode child : groupNode.getChildren()) {
            // if (userId.equals(child.getId())) {
            // if (!groupIds.contains(groupNode.getId())) {
            // groupNode.removeChild(child);
            // usersTreeViewer.refresh(groupNode);
            // break;
            // }
            // }
            // }
            // }
            // }
            // if (orphanGroup == null) {
            // orphanGroup = new OrphanGroup(ORPHAN_GROUP_ID);
            // orphanGroup.addChild(new CollaborationUser(rosterEntry
            // .getUser()));
            // topLevel.addChild(orphanGroup);
            // usersTreeViewer.refresh();
            // }
            // } else {
            // // TODO add user to groups and remove from others including
            // // orphan group.
            // for (Object node : topLevel.getObjects()) {
            // if (node instanceof IRosterGroup
            // && !(node instanceof SessionGroup)) {
            // IRosterGroup group = (IRosterGroup) node;
            // if (group instanceof OrphanGroup) {
            // orphanGroup = (OrphanGroup) group;
            // }
            // boolean addUser = groupIds.contains(group.getId());
            // for (CollaborationNode child : group.getChildren()) {
            // if (userId.equals(child.getId())) {
            // if (addUser) {
            // // User already in the group no need to
            // // add.
            // groupIds.remove(group.getId());
            // addUser = false;
            // } else {
            // // User no longer in this group.
            // group.removeChild(child);
            // usersTreeViewer.refresh(group);
            // }
            // break;
            // }
            // }
            // if (addUser) {
            // group.addChild(new CollaborationUser(userId));
            // groupIds.remove(group.getId());
            // }
            // }
            // }
            //
            // boolean refreshTopLevel = false;
            // if (orphanGroup != null
            // && orphanGroup.getChildren().size() == 0) {
            // topLevel.removeChild(orphanGroup);
            // refreshTopLevel = true;
            // }
            //
            // // groups now contains new groups. See if they are on the
            // // display list.
            // if (groupIds.size() > 0) {
            // CollaborationDataManager manager = CollaborationDataManager
            // .getInstance();
            // for (String groupId : groupIds) {
            // if (manager.displayGroup(groupId)) {
            // IRosterGroup groupNode = new IRosterGroup(groupId);
            // topLevel.addObject(groupNode);
            // groupNode.addChild(new CollaborationUser(userId));
            // }
            // }
            // refreshTopLevel = true;
            // }
            // if (refreshTopLevel) {
            // usersTreeViewer.refresh(topLevel);
            // }
            // }
            break;
        case DELETE:
            // Should be a rare event.
            // for (Object node : topLevel.getObjects()) {
            // if (node instanceof IRosterGroup
            // && !(node instanceof SessionGroup)) {
            // IRosterGroup groupNode = (IRosterGroup) node;
            // for (IRosterEntry child : groupNode.getEntries()) {
            // if (userId.equals(child.getUser())) {
            // groupNode.removeObject(child);
            // usersTreeViewer.refresh(groupNode);
            // break;
            // }
            // }
            // }
            // }
            break;
        case MODIFY:
            // TODO, remove this case if nothing is done
            // Assume this only changes the presence of a user in the
            // desired
            // groups.
            // Since this is handled by the handleModifiedPresence nothing
            // needs
            // to be done.
            break;
        default:
            statusHandler.handle(Priority.PROBLEM, "Unknown type: "
                    + rosterChangeEvent.getType());
            break;
        }
    }
}
