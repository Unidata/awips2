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

import java.util.ArrayList;
import java.util.Collection;
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
import org.eclipse.jface.viewers.DoubleClickEvent;
import org.eclipse.jface.viewers.IDoubleClickListener;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.viewers.TreeSelection;
import org.eclipse.jface.viewers.TreeViewer;
import org.eclipse.swt.SWT;
import org.eclipse.swt.custom.TreeEditor;
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
import org.eclipse.ui.IWorkbenchPage;
import org.eclipse.ui.PartInitException;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.part.ViewPart;

import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.viz.collaboration.comm.SessionManager;
import com.raytheon.uf.viz.collaboration.comm.identity.info.IVenueInfo;
import com.raytheon.uf.viz.collaboration.data.CollaborationDataManager;
import com.raytheon.uf.viz.collaboration.data.CollaborationGroup;
import com.raytheon.uf.viz.collaboration.data.CollaborationNode;
import com.raytheon.uf.viz.collaboration.data.CollaborationUser;
import com.raytheon.uf.viz.collaboration.data.DataUser;
import com.raytheon.uf.viz.collaboration.data.LoginUser;
import com.raytheon.uf.viz.collaboration.data.SessionGroup;
import com.raytheon.uf.viz.collaboration.ui.session.CollaborationSessionView;
import com.raytheon.uf.viz.collaboration.ui.session.SessionView;

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

    private SessionGroup activeSessionGroup = null;

    private TreeViewer usersTreeViewer;

    CollaborationGroup topLevel = new CollaborationGroup("kickstart");

    SessionGroup acitveSessionGroup;

    Map<String, String[]> groupMap;

    private Action collaborateAction;

    private Action linkToEditorAction;

    private Action inviteAction;

    private Action joinAction;

    private Action joinCollaborationAction;

    private Action logoutAction;

    private Action aliasAction;

    private Action addGroupAction;

    private Action addUserAction;

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

        SessionManager manger = CollaborationDataManager.getInstance()
                .getSessionManager();
        if (manger == null) {
            System.err.println("Unable to connect");
            return;
        }
        populateTree();
    }

    /**
     * 
     */
    private void createActions() {

        collaborateAction = new Action("Create Session...") {
            @Override
            public void run() {
                createCollaborationSession();
            }

        };
        collaborateAction.setImageDescriptor(CollaborationUtils
                .getImageDescriptor("add_collaborate.gif"));

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
            };
        };

        logoutAction = new Action("Logout") {
            @Override
            public void run() {
                MessageBox messageBox = new MessageBox(Display.getCurrent()
                        .getActiveShell(), SWT.ICON_WARNING | SWT.OK
                        | SWT.CANCEL);
                messageBox.setText("Log off Collaboration");
                messageBox.setMessage("Logging off will sever your\n"
                        + "connection to the server and\n"
                        + "close all session views.");
                int result = messageBox.open();
                if (result == SWT.OK) {
                    CollaborationDataManager.getInstance().closeManager();
                    // TODO clean up ui here.
                }
            };
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

        addUserAction = new Action("Add User") {
            public void run() {
                System.out.println("Add User");
            };
        };
        addUserAction.setImageDescriptor(CollaborationUtils
                .getImageDescriptor("add_contact.gif"));

        addGroupAction = new Action("Add Group") {
            public void run() {
                System.out.println("Add group");
            };
        };
        addGroupAction.setImageDescriptor(CollaborationUtils
                .getImageDescriptor("add_group.gif"));

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
        for (DataUser.StatusType type : DataUser.StatusType.values()) {
            if (type != DataUser.StatusType.NOT_ON_LINE) {
                Action action = new Action(type.value()) {
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
    }

    private void createToolbar() {
        IToolBarManager mgr = getViewSite().getActionBars().getToolBarManager();
        // mgr.add(joinCollaborationAction);
        mgr.add(collaborateAction);
        mgr.add(collapseAllAction);
        // mgr.add(privateChatAction);
        mgr.add(linkToEditorAction);
    }

    private void createMenubar() {
        IMenuManager mgr = getViewSite().getActionBars().getMenuManager();
        createMenu(mgr);
    }

    private void createMenu(IMenuManager mgr) {
        mgr.add(changeStatusAction);
        mgr.add(changeMessageAction);
        mgr.add(changePasswordAction);
        mgr.add(logoutAction);
    }

    private void createCollaborationSession() {
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

        if (result == null) {
            return;
        }

        String sessionId = null;
        try {

            sessionId = manager.createCollaborationSession(result.getName(),
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

    private void createPrivateChat() {
        IStructuredSelection selection = (IStructuredSelection) usersTreeViewer
                .getSelection();
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
                    .showView(
                            SessionView.ID,
                            null,
                            IWorkbenchPage.VIEW_CREATE
                                    | IWorkbenchPage.VIEW_VISIBLE);
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
        usersTreeViewer.setContentProvider(new UsersTreeContentProvider());
        usersTreeViewer.setLabelProvider(new UsersTreeLabelProvider());
        usersTreeViewer.setSorter(new UsersTreeViewerSorter());

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
                manager.add(collaborateAction);
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
            inviteManager.add(joinAction);
            manager.add(inviteManager);
            if (user.isLocal()) {
                manager.add(addUserAction);
                manager.add(addGroupAction);
                manager.add(removeUserAction);
                manager.add(removeGroupAction);
            }
        } else if (o instanceof CollaborationGroup) {
            CollaborationGroup group = (CollaborationGroup) o;
            manager.add(collaborateAction);
            if (group.isLocal()) {
                manager.add(addUserAction);
                manager.add(addGroupAction);
                manager.add(removeGroupAction);
            }
        }
        manager.add(aliasAction);
    }

    /**
     * 
     */
    private List<String> getChatSessions() {
        List<String> rooms = new ArrayList<String>();
        for (int i = 0; i < 3; i++) {
            rooms.add("Room : " + i);
        }
        return rooms;
    }

    protected void populateTree() {
        topLevel = new CollaborationGroup("kickstart");
        usersTreeViewer.setInput(topLevel);
        CollaborationDataManager manager = CollaborationDataManager
                .getInstance();
        SessionManager sessionManager = manager.getSessionManager();
        LoginUser user = new LoginUser(CollaborationDataManager.getInstance()
                .getLoginId());
        topLevel.addChild(user);
        activeSessionGroup = new SessionGroup("Active Sessions");
        activeSessionGroup.setSessionRoot(true);
        topLevel.addChild(activeSessionGroup);

        populateActiveSessions();

        // TODO get from server.
        for (String g : new String[] { "Mybuddy1", "buddy1" }) {
            CollaborationGroup group = new CollaborationGroup(g);
            group.setLocal(true);
            group.setModifiable(true);
            topLevel.addChild(group);
            for (String u : new String[] { "OAX_user1", "DSM_user3",
                    "LBF_user2" }) {
                CollaborationUser item = new CollaborationUser(u);
                group.addChild(item);
                item.setStatus(DataUser.StatusType.AVAILABLE);
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
                item.setStatus(DataUser.StatusType.AWAY);
            }
        }

        CollaborationUser me = new CollaborationUser("OAX_rferrel");
        me.setStatus(DataUser.StatusType.AVAILABLE);
        for (CollaborationNode node : topLevel.getChildren()) {
            if ("OAX".equals(node.getId())) {
                ((CollaborationGroup) node).addChild(me);
                break;
            }
        }
        usersTreeViewer.setInput(topLevel);
        usersTreeViewer.refresh(topLevel, true);
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
                // TODO add current participants of the venu here.
            }
            activeSessionGroup.addChild(gp);
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
        // TODO Auto-generated method stub

    }

    private void addDoubleClickListeners() {
        usersTreeViewer.addDoubleClickListener(new IDoubleClickListener() {
            @Override
            public void doubleClick(DoubleClickEvent event) {
                TreeSelection selection = (TreeSelection) event.getSelection();
                if (selection.getFirstElement() instanceof SessionGroup) {
                    SessionGroup group = (SessionGroup) selection
                            .getFirstElement();
                    if (!group.isSessionRoot()) {
                        createJoinCollaboration();
                    }
                }
            }
        });
    }
}
