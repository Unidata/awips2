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

import java.util.Collection;
import java.util.HashSet;
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
import org.eclipse.swt.events.FocusEvent;
import org.eclipse.swt.events.FocusListener;
import org.eclipse.swt.events.ModifyEvent;
import org.eclipse.swt.events.ModifyListener;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Menu;
import org.eclipse.swt.widgets.MessageBox;
import org.eclipse.swt.widgets.Text;
import org.eclipse.ui.IViewPart;
import org.eclipse.ui.IViewReference;
import org.eclipse.ui.IWorkbenchPage;
import org.eclipse.ui.PartInitException;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.part.ViewPart;

import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.viz.collaboration.comm.identity.IPresence;
import com.raytheon.uf.viz.collaboration.comm.identity.IPresence.Mode;
import com.raytheon.uf.viz.collaboration.comm.identity.ISession;
import com.raytheon.uf.viz.collaboration.comm.identity.IVenueSession;
import com.raytheon.uf.viz.collaboration.comm.identity.info.IVenueInfo;
import com.raytheon.uf.viz.collaboration.comm.identity.roster.IRoster;
import com.raytheon.uf.viz.collaboration.comm.identity.roster.IRosterEntry;
import com.raytheon.uf.viz.collaboration.comm.identity.roster.IRosterGroup;
import com.raytheon.uf.viz.collaboration.comm.identity.roster.IRosterManager;
import com.raytheon.uf.viz.collaboration.comm.provider.session.SessionManager;
import com.raytheon.uf.viz.collaboration.data.CollaborationDataManager;
import com.raytheon.uf.viz.collaboration.data.CollaborationGroup;
import com.raytheon.uf.viz.collaboration.data.CollaborationNode;
import com.raytheon.uf.viz.collaboration.data.CollaborationUser;
import com.raytheon.uf.viz.collaboration.data.LoginUser;
import com.raytheon.uf.viz.collaboration.data.SessionGroup;
import com.raytheon.uf.viz.collaboration.ui.session.AbstractSessionView;
import com.raytheon.uf.viz.collaboration.ui.session.CollaborationSessionView;
import com.raytheon.uf.viz.collaboration.ui.session.PeerToPeerView;
import com.raytheon.uf.viz.collaboration.ui.session.SessionView;
import com.raytheon.viz.ui.perspectives.VizPerspectiveListener;

/**
 * TODO Add Description
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
public class CollaborationGroupView extends ViewPart {
    public static final String ID = "com.raytheon.uf.viz.collaboration.ui.CollaborationGroupView";

    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(CollaborationGroupView.class);

    private SessionGroup activeSessionGroup;

    private TreeViewer usersTreeViewer;

    CollaborationGroup topLevel;

    Map<String, String[]> groupMap;

    private Action createSessionAction;

    private Action linkToEditorAction;

    private Action inviteAction;

    private Action joinAction;

    private Action joinCollaborationAction;

    private Action peerToPeerChatAction;

    private Action logonAction;

    private Action logoutAction;

    private Action aliasAction;

    private Action renameAction;

    private Action addGroupAction;

    private Action addUserAction;

    private Action selectGroups;

    private Action removeGroupAction;

    private Action removeUserAction;

    private Action changeMessageAction;

    private Action changePasswordAction;

    private Action changeStatusAction;

    private Action refreshActiveSessionsAction;

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
        createUsersTree(parent);
        addDoubleClickListeners();
        createContextMenu();
        if (CollaborationDataManager.getInstance().isConnected()) {
            populateTree();
        } else {
            usersTreeViewer.getTree().setEnabled(false);
        }
    }

    /**
     * 
     */
    private void createActions() {

        createSessionAction = new Action("Create Session...") {
            @Override
            public void run() {
                createSession();
            }

        };
        createSessionAction.setImageDescriptor(CollaborationUtils
                .getImageDescriptor("add_collaborate.gif"));
        createSessionAction.setEnabled(CollaborationDataManager.getInstance()
                .isConnected());

        linkToEditorAction = new Action("Link Editor to Chat Session",
                Action.AS_CHECK_BOX) {
            @Override
            public void run() {
                // TODO
                System.out.println("Link to editor here");
                // createPrivateChat();
            }
        };
        linkToEditorAction.setImageDescriptor(CollaborationUtils
                .getImageDescriptor("link_to_editor.gif"));

        inviteAction = new Action("Invite...") {
            @Override
            public void run() {
                System.out.println("Invite...");
            };
        };
        inviteAction.setImageDescriptor(CollaborationUtils
                .getImageDescriptor("invite.gif"));

        joinAction = new Action("Join Session") {
            @Override
            public void run() {
                createJoinCollaboration();
            }
        };

        peerToPeerChatAction = new Action("Private Chat") {
            @Override
            public void run() {
                createPrivateChat(getId());
            }
        };

        logonAction = new Action("Logon...") {
            @Override
            public void run() {
                populateTree();
            }
        };
        logonAction.setImageDescriptor(CollaborationUtils
                .getImageDescriptor("logout.gif"));

        logoutAction = new Action("Logout") {
            @Override
            public void run() {
                performLogout();
            }
        };

        logoutAction.setImageDescriptor(CollaborationUtils
                .getImageDescriptor("logout.gif"));

        aliasAction = new Action("Alias") {
            @Override
            public void run() {
                aliasItem();
                System.out.println("Alias");
            };
        };

        renameAction = new Action("Rename") {
            @Override
            public void run() {
                System.out.println("Rename action");
            }
        };

        addUserAction = new Action("Add User") {
            public void run() {
                System.out.println("Add User");
            };
        };
        addUserAction.setImageDescriptor(CollaborationUtils
                .getImageDescriptor("add_contact.gif"));

        addGroupAction = new Action("Create Group") {
            public void run() {
                System.out.println("Create Group here");
            };
        };
        addGroupAction.setImageDescriptor(CollaborationUtils
                .getImageDescriptor("add_group.gif"));

        selectGroups = new Action("Select System Groups...") {
            public void run() {
                System.out.println("Select System Groups to Display...");
            }
        };

        changeMessageAction = new Action("Change Message...") {
            public void run() {
                System.out.println("Change message");
            };
        };

        changePasswordAction = new Action("Change password...") {
            public void run() {
                System.out.println("Change password here");
            };
        };

        changeStatusAction = new Action("Change Status",
                Action.AS_DROP_DOWN_MENU) {
            public void run() {
                System.out.println("Change Status here to: " + getId());
            };
        };

        refreshActiveSessionsAction = new Action("Refresh") {
            public void run() {
                System.out.println("Refresh Active Sessions");
                refreshActiveSessions();
            }
        };
        refreshActiveSessionsAction.setImageDescriptor(CollaborationUtils
                .getImageDescriptor("refresh.gif"));
        refreshActiveSessionsAction
                .setToolTipText("Refresh the Active Sessions Entries.");

        collapseAllAction = new Action("Collapse All") {
            public void run() {
                usersTreeViewer.collapseAll();
            }
        };
        collapseAllAction.setImageDescriptor(CollaborationUtils
                .getImageDescriptor("collapseall.gif"));

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

        removeGroupAction = new Action("Remove Group") {
            public void run() {
            };
        };
        removeUserAction = new Action("Remove User") {
            public void run() {
            };
        };
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
            // TODO close collaboration CAVE editor(s).
            // for (IEditorReference ref :
            // PlatformUI.getWorkbench().getActiveWorkbenchWindow().getActivePage().getEditorReferences())
            // {
            // IEditorPart editor = ref.getEditor(false);
            // if (editor instanceof CollaborationEditor) {
            // PlatformUI.getWorkbench().getActiveWorkbenchWindow()
            // .getActivePage().hideEditor(ref);
            // }
            // }
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
        selection.getFirstElement();

        Text newEditor = new Text(usersTreeViewer.getTree(), SWT.NONE);
        newEditor.setText(((CollaborationNode) selection.getFirstElement())
                .getId());
        newEditor.addModifyListener(new ModifyListener() {
            public void modifyText(ModifyEvent e) {
                Text text = (Text) treeEditor.getEditor();
                treeEditor.getItem().setText(text.getText());
            }
        });
        newEditor.selectAll();
        newEditor.setFocus();
        treeEditor.setEditor(newEditor, usersTreeViewer.getTree()
                .getSelection()[0]);

    }

    private void fillStatusMenu(Menu menu) {
        for (IPresence.Mode type : CollaborationUtils.statusModes) {
            Action action = new Action(type.getMode()) {
                public void run() {
                    changeStatusAction.setId(getId());
                    changeStatusAction.run();
                };
            };
            action.setId(type.name());
            ActionContributionItem item = new ActionContributionItem(action);
            action.setImageDescriptor(CollaborationUtils
                    .getImageDescriptor(type.name().toLowerCase() + ".gif"));
            item.fill(menu, -1);
        }
    }

    private void createToolbar() {
        IToolBarManager mgr = getViewSite().getActionBars().getToolBarManager();
        // mgr.add(joinCollaborationAction);
        mgr.add(createSessionAction);
        mgr.add(collapseAllAction);
        // mgr.add(privateChatAction);
        mgr.add(linkToEditorAction);
    }

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
        mgr.add(changeMessageAction);
        mgr.add(changePasswordAction);
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
        SessionManager sessionManager = manager.getSessionManager();
        if (sessionManager == null) {
            System.err.println("Unable to get session manager");
            return;
        }

        // TODO determine invite based on if any users/groups selected.

        CreateSessionDialog dialog = new CreateSessionDialog(Display
                .getCurrent().getActiveShell(), usersSelected());
        dialog.open();

        CreateSessionData result = (CreateSessionData) dialog.getReturnValue();

        if (result != null) {
            if (result.isCollaborationSession()) {
                createCollaborationView(result);
            } else {
                createPrivateView(result);
            }
        }
    }

    private void createCollaborationView(CreateSessionData result) {
        String sessionId = null;
        try {
            sessionId = CollaborationDataManager.getInstance()
                    .createCollaborationSession(result.getName(),
                            result.getSubject());
        } catch (Exception e) {
            e.printStackTrace();
        } finally {
            if (sessionId == null) {
                return;
            }
        }

        try {
            IViewPart part = PlatformUI
                    .getWorkbench()
                    .getActiveWorkbenchWindow()
                    .getActivePage()
                    .showView(CollaborationSessionView.ID, sessionId,
                            IWorkbenchPage.VIEW_ACTIVATE);

            if (result.isInviteUsers()) {
                // TODO send invites to the users
                Set<CollaborationUser> selectedUsers = getSelectedUsers();
                for (CollaborationUser user : selectedUsers) {
                    System.out.println("sessionId - Invite: " + user.getId());
                }
            }
            refreshActiveSessions();
        } catch (PartInitException e) {
            statusHandler.handle(Priority.PROBLEM,
                    "Unable to open collaboation sesson", e);
        } catch (Exception e) {
            statusHandler.handle(Priority.ERROR, "Unexpected excepton", e);
        }
    }

    private void createPrivateView(CreateSessionData result) {
        String sessionId = null;
        try {
            // TODO Do not use createCollaborationSession once private session
            // implemented.
            sessionId = CollaborationDataManager.getInstance()
                    .createCollaborationSession(result.getName(),
                            result.getSubject());
        } catch (Exception e) {
            e.printStackTrace();
        } finally {
            if (sessionId == null) {
                return;
            }
        }

        try {
            IViewPart part = PlatformUI
                    .getWorkbench()
                    .getActiveWorkbenchWindow()
                    .getActivePage()
                    .showView(SessionView.ID, sessionId,
                            IWorkbenchPage.VIEW_ACTIVATE);

            if (result.isInviteUsers()) {
                // TODO send invites to the users
                Set<CollaborationUser> selectedUsers = getSelectedUsers();
                for (CollaborationUser user : selectedUsers) {
                    System.out.println("sessionId - Invite: " + user.getId());
                }
            }
            refreshActiveSessions();
        } catch (PartInitException e) {
            statusHandler.handle(Priority.PROBLEM,
                    "Unable to open collaboation sesson", e);
        } catch (Exception e) {
            statusHandler.handle(Priority.ERROR, "Unexpected excepton", e);
        }
    }

    private void createJoinCollaboration() {
        IStructuredSelection selection = (IStructuredSelection) usersTreeViewer
                .getSelection();
        Object[] nodes = selection.toArray();
        for (Object node : nodes) {
            if (node instanceof SessionGroup) {
                SessionGroup sg = (SessionGroup) node;
                System.out.println("Join: " + sg.getId());
                String sessionId = CollaborationDataManager.getInstance()
                        .joinCollaborationSession(sg.getText(), sg.getId());
                try {
                    IViewPart part = PlatformUI
                            .getWorkbench()
                            .getActiveWorkbenchWindow()
                            .getActivePage()
                            .showView(CollaborationSessionView.ID, sessionId,
                                    IWorkbenchPage.VIEW_ACTIVATE);

                } catch (PartInitException e) {
                    statusHandler.handle(Priority.PROBLEM,
                            "Unable to open collaboation sesson", e);
                } catch (Exception e) {
                    statusHandler.handle(Priority.ERROR, "Unexpected excepton",
                            e);
                }
            }
        }
    }

    private void createPrivateChat(String user) {
        // IStructuredSelection selection = (IStructuredSelection)
        // usersTreeViewer
        // .getSelection();
        // TODO
        // List<ID> users = new ArrayList<ID>();
        // ID id = IDFactory.getDefault().createID(
        // CollaborationData.getInstance().getClient()
        // .getConnectNamespace(), "abc@awipscm.omaha.us.ray.com");
        // users.add(id);

        try {
            // if (users.size() > 0) {
            // CollaborationUtils.createChat(users);
            PlatformUI
                    .getWorkbench()
                    .getActiveWorkbenchWindow()
                    .getActivePage()
                    .showView(PeerToPeerView.ID, user,
                            IWorkbenchPage.VIEW_ACTIVATE);
            // }
        } catch (PartInitException e) {
            statusHandler.handle(Priority.PROBLEM, "Unable to open chat", e);
        }
    }

    private void createUsersTree(Composite parent) {
        Composite child = new Composite(parent, SWT.NONE);
        child.setLayout(new GridLayout(1, false));
        child.setLayoutData(new GridData(SWT.FILL, SWT.FILL, true, true));
        usersTreeViewer = new TreeViewer(child);
        usersTreeViewer.getTree().setLayoutData(
                new GridData(SWT.FILL, SWT.FILL, true, true));
        usersTreeViewer.getTree().addFocusListener(new FocusListener() {
            @Override
            public void focusGained(FocusEvent e) {
                VizPerspectiveListener.getCurrentPerspectiveManager()
                        .deactivateContexts();
            }

            @Override
            public void focusLost(FocusEvent e) {
                VizPerspectiveListener.getCurrentPerspectiveManager()
                        .activateContexts();
            }
        });
        usersTreeViewer.setContentProvider(new UsersTreeContentProvider());
        usersTreeViewer.setLabelProvider(new UsersTreeLabelProvider());
        usersTreeViewer.setSorter(new UsersTreeViewerSorter());
        topLevel = new CollaborationGroup("kickstart");
        usersTreeViewer.setInput(topLevel);

        treeEditor = new TreeEditor(usersTreeViewer.getTree());
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
        if (o instanceof LoginUser) {
            createMenu(manager);
            return;
        }

        if (o instanceof SessionGroup) {
            SessionGroup sessionGroup = (SessionGroup) o;
            if (sessionGroup.isSessionRoot()) {
                manager.add(createSessionAction);
                manager.add(refreshActiveSessionsAction);
            } else {
                manager.add(joinAction);
            }
            return;
        }

        if (o instanceof CollaborationUser) {
            CollaborationUser user = (CollaborationUser) o;
            MenuManager inviteManager = new MenuManager("Invite to...");
            // get current open chats
            Map<String, IVenueSession> sessions = CollaborationDataManager
                    .getInstance().getSessions();
            for (String name : sessions.keySet()) {
                ISession session = sessions.get(name);
                if (session != null) {
                    final IVenueInfo info = sessions.get(name).getVenue()
                            .getInfo();
                    if (info != null) {
                        System.out.println("Add to Invite To menu: "
                                + info.getVenueDescription());
                        Action action = new Action(info.getVenueDescription()) {
                            /*
                             * (non-Javadoc)
                             * 
                             * @see org.eclipse.jface.action.Action#run()
                             */
                            @Override
                            public void run() {
                                joinAction.setId(info.getVenueID());
                                joinAction.run();
                            }
                        };
                        action.setId(info.getVenueID());
                        inviteManager.add(action);
                    }
                }
            }
            manager.add(inviteManager);
            manager.add(peerToPeerChatAction);
            peerToPeerChatAction.setId(user.getId());
            if (user.isLocal()) {
                manager.add(addUserAction);
                manager.add(addGroupAction);
                manager.add(removeUserAction);
                manager.add(removeGroupAction);
            }
        } else if (o instanceof CollaborationGroup) {
            CollaborationGroup group = (CollaborationGroup) o;
            manager.add(createSessionAction);
            if (group.isLocal()) {
                manager.add(addUserAction);
                manager.add(addGroupAction);
                manager.add(removeGroupAction);
                manager.add(renameAction);
            }
        }
    }

    protected void populateTree() {
        CollaborationDataManager manager = CollaborationDataManager
                .getInstance();
        SessionManager sessionManager = manager.getSessionManager();
        topLevel.removeChildren();
        if (sessionManager == null) {
            usersTreeViewer.getTree().setEnabled(false);
            return;
        }
        LoginUser user = new LoginUser(CollaborationDataManager.getInstance()
                .getLoginId());
        topLevel.addChild(user);
        activeSessionGroup = new SessionGroup("Active Sessions");
        activeSessionGroup.setSessionRoot(true);
        topLevel.addChild(activeSessionGroup);

        populateActiveSessions();

        populateGroups();

        // usersTreeViewer.setInput(topLevel);
        usersTreeViewer.getTree().setEnabled(true);
        usersTreeViewer.refresh(topLevel, true);
        createSessionAction.setEnabled(true);
    }

    private void refreshActiveSessions() {
        populateActiveSessions();
        usersTreeViewer.refresh(activeSessionGroup, true);
    }

    private void populateActiveSessions() {
        activeSessionGroup.removeChildren();
        Collection<IVenueInfo> venuList = CollaborationDataManager
                .getInstance().getSessionManager().getVenueInfo();
        for (IVenueInfo venu : venuList) {
            SessionGroup gp = new SessionGroup(CollaborationDataManager
                    .getInstance().venuIdToSessionId(venu.getVenueID()));
            gp.setText(venu.getVenueName());

            if (venu.getParticipantCount() > 0) {
                // TODO add current participants of the venu here?
            }
            activeSessionGroup.addChild(gp);
        }
    }

    private void populateGroups() {
        for (CollaborationNode node : topLevel.getChildren()) {
            if (!(node instanceof LoginUser || node instanceof SessionGroup)) {
                topLevel.removeChild(node);
            }
        }

        IRosterManager rosterManager = CollaborationDataManager.getInstance()
                .getSessionManager().getRosterManager();

        IRoster roster = rosterManager.getRoster();
        
        String name = null;
        int rsize = -1;
        int gsize = -1;
        if(roster != null) {
            if(roster.getUser() != null) {
                name = roster.getUser().getName();
            }
            if(roster.getEntries() != null) {
                rsize = roster.getEntries().size();
            }
            if(roster.getGroups() != null) {
                gsize = roster.getGroups().size();
            }
            System.out.println("rosterManager Name " + name
                    + ": group size " + gsize + ": entry size "
                    + rsize);
            for (IRosterGroup rosterGroup : roster.getGroups()) {
                if(rosterGroup != null) {
                    populateGroup(topLevel, rosterGroup);
                }
            }

            // TODO get Groups from server.
            for (String g : new String[] { "Mybuddy1", "buddy1" }) {
                CollaborationGroup group = new CollaborationGroup(g);
                group.setLocal(true);
                group.setModifiable(true);
                topLevel.addChild(group);
                for (String u : new String[] { "jkorman@awipscm.omaha.us.ray.com",
                        "abc@awipscm.omaha.us.ray.com",
                        "mnash@awipscm.omaha.us.ray.com" }) {
                    CollaborationUser item = new CollaborationUser(u);
                    group.addChild(item);
                    item.setMode(Mode.AVAILABLE);
                }
            }

            // TODO get from server
            for (String g : new String[] { "OAX", "DSM", "LBF", "FSD" }) {
                CollaborationGroup group = new CollaborationGroup(g);
                group.setLocal(false);
                topLevel.addChild(group);
                for (String u : new String[] { g + "_user2", g + "_user3",
                        g + "_user1" }) {
                    CollaborationUser item = new CollaborationUser(u);
                    group.addChild(item);
                    item.setMode(Mode.AWAY);
                }
            }
        
        }
            

        CollaborationUser me = new CollaborationUser("OAX_rferrel");
        me.setMode(Mode.AVAILABLE);
        for (CollaborationNode node : topLevel.getChildren()) {
            if ("OAX".equals(node.getId())) {
                ((CollaborationGroup) node).addChild(me);
                break;
            }
        }
    }

    /**
     * This creates a group node, populates it with its children and makes it a
     * child of its parent.
     * 
     * @param parent
     * @param rosterGroup
     *            - Information about the group and its children -
     */
    private void populateGroup(CollaborationGroup parent,
            IRosterGroup rosterGroup) {

        CollaborationGroup groupNode = new CollaborationGroup(
                rosterGroup.getName());
        // TODO determine if group is modifiable (User) or System group.
        groupNode.setLocal(true);
        groupNode.setModifiable(true);
        parent.addChild(groupNode);
        System.out.println("group Name " + rosterGroup.getName() + ": entries "
                + rosterGroup.getEntries());
        if (rosterGroup.getGroups() != null) {
            for (IRosterGroup childGroup : rosterGroup.getGroups()) {
                populateGroup(groupNode, childGroup);
            }
        }

        for (IRosterEntry e : rosterGroup.getEntries()) {
            CollaborationUser child = new CollaborationUser(e.getName());
            groupNode.addChild(child);
        }
    }

    /**
     * @return
     */
    private boolean usersSelected() {
        IStructuredSelection selection = (IStructuredSelection) usersTreeViewer
                .getSelection();
        Object[] nodes = selection.toArray();
        boolean result = false;

        for (Object node : nodes) {
            if ((node instanceof LoginUser) == false
                    && (node instanceof SessionGroup) == false) {
                result = true;
                break;
            }
        }
        return result;
    }

    private Set<CollaborationUser> getSelectedUsers() {
        Set<CollaborationUser> selectedUsers = new HashSet<CollaborationUser>();
        IStructuredSelection selection = (IStructuredSelection) usersTreeViewer
                .getSelection();
        Object[] nodes = selection.toArray();
        for (Object node : nodes) {
            if (node instanceof CollaborationUser) {
                if ((node instanceof LoginUser) == false) {
                    selectedUsers.add((CollaborationUser) node);
                }
            } else if ((node instanceof SessionGroup) == false) {
                selectedUsers
                        .addAll(getSelectedUsers((CollaborationGroup) node));
            }
        }

        return selectedUsers;
    }

    private Collection<CollaborationUser> getSelectedUsers(
            CollaborationGroup groupNode) {
        Set<CollaborationUser> selectedUsers = new HashSet<CollaborationUser>();
        for (CollaborationNode node : groupNode.getChildren()) {
            if (node instanceof CollaborationUser) {
                selectedUsers.add((CollaborationUser) node);
            } else if (node instanceof CollaborationGroup) {
                selectedUsers
                        .addAll(getSelectedUsers((CollaborationGroup) node));
            }
        }
        return selectedUsers;
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
                CollaborationNode node = (CollaborationNode) selection
                        .getFirstElement();
                if (node instanceof SessionGroup) {
                    SessionGroup group = (SessionGroup) selection
                            .getFirstElement();
                    if (!group.isSessionRoot()) {
                        createJoinCollaboration();
                    }
                } else if (node instanceof CollaborationUser) {
                    String loginUserId = CollaborationDataManager.getInstance()
                            .getLoginId();
                    if (loginUserId.equals(node.getId()) == false) {
                        createPrivateChat(node.getId());
                    }
                }
            }
        });
    }

    @Override
    public void dispose() {
        // TODO Auto-generated method stub
        System.out.println("Disposing: " + getClass().getName());
        super.dispose();
    }
}
