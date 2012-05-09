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
import java.util.Collection;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import org.eclipse.ecf.core.user.User;
import org.eclipse.ecf.presence.IPresence;
import org.eclipse.ecf.presence.IPresence.Mode;
import org.eclipse.ecf.presence.IPresence.Type;
import org.eclipse.ecf.presence.roster.IRoster;
import org.eclipse.ecf.presence.roster.IRosterEntry;
import org.eclipse.ecf.presence.roster.IRosterGroup;
import org.eclipse.ecf.presence.roster.IRosterItem;
import org.eclipse.ecf.presence.roster.RosterEntry;
import org.eclipse.ecf.presence.roster.RosterGroup;
import org.eclipse.jface.action.Action;
import org.eclipse.jface.action.ActionContributionItem;
import org.eclipse.jface.action.IMenuCreator;
import org.eclipse.jface.action.IMenuListener;
import org.eclipse.jface.action.IMenuManager;
import org.eclipse.jface.action.IToolBarManager;
import org.eclipse.jface.action.MenuManager;
import org.eclipse.jface.action.Separator;
import org.eclipse.jface.viewers.ColumnViewerToolTipSupport;
import org.eclipse.jface.viewers.DoubleClickEvent;
import org.eclipse.jface.viewers.IDoubleClickListener;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.viewers.TreeSelection;
import org.eclipse.jface.viewers.TreeViewer;
import org.eclipse.jface.window.ToolTip;
import org.eclipse.swt.SWT;
import org.eclipse.swt.custom.TreeEditor;
import org.eclipse.swt.events.KeyAdapter;
import org.eclipse.swt.events.KeyEvent;
import org.eclipse.swt.events.ModifyEvent;
import org.eclipse.swt.events.ModifyListener;
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
import com.raytheon.uf.viz.collaboration.comm.identity.ISession;
import com.raytheon.uf.viz.collaboration.comm.identity.ISharedDisplaySession;
import com.raytheon.uf.viz.collaboration.comm.identity.IVenueSession;
import com.raytheon.uf.viz.collaboration.comm.identity.event.IRosterChangeEvent;
import com.raytheon.uf.viz.collaboration.comm.identity.info.IVenueInfo;
import com.raytheon.uf.viz.collaboration.comm.identity.invite.SharedDisplayVenueInvite;
import com.raytheon.uf.viz.collaboration.comm.identity.invite.VenueInvite;
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
        // build the necessary actions for the view
        createActions();

        // add some actions to the toolbar
        createToolbar();

        // add some actions to the menubar
        createMenubar();

        // add a part listener so that we can check when things about the view
        // change
        getViewSite().getWorkbenchWindow().getPartService()
                .addPartListener(this);

        createUsersTree(parent);
        addDoubleClickListeners();
        createContextMenu();
        if (CollaborationDataManager.getInstance().isConnected() == false) {
            usersTreeViewer.getTree().setEnabled(false);
        }

        CollaborationConnection connection = CollaborationDataManager
                .getInstance().getCollaborationConnection();
        if (connection != null) {
            connection.registerEventHandler(this);
        }
        populateTree();
        usersTreeViewer.refresh();
    }

    @Override
    public void dispose() {
        super.dispose();
        CollaborationConnection connection = CollaborationDataManager
                .getInstance().getCollaborationConnection();
        if (connection != null) {
            connection.unRegisterEventHandler(this);
        }
        getViewSite().getWorkbenchWindow().getPartService()
                .removePartListener(this);
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
                List<UserId> ids = new ArrayList<UserId>();

                for (IRosterEntry user : getSelectedUsers()) {
                    UserId id = IDConverter.convertFrom(user.getUser());
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
                TreeSelection selection = (TreeSelection) usersTreeViewer
                        .getSelection();
                Object node = selection.getFirstElement();
                if (node instanceof IVenueSession) {
                    // loop through all the views so that we can bring the one
                    // that
                    // was selected to the top...
                    for (IViewReference ref : getViewSite()
                            .getWorkbenchWindow().getActivePage()
                            .getViewReferences()) {
                        IVenueSession session = (IVenueSession) selection
                                .getFirstElement();
                        if (session.getSessionId().equals(ref.getSecondaryId())) {
                            PlatformUI.getWorkbench()
                                    .getActiveWorkbenchWindow().getActivePage()
                                    .activate(ref.getView(true));
                        }
                    }
                }
            }
        };

        peerToPeerChatAction = new Action("Chat") {
            @Override
            public void run() {
                TreeSelection selection = (TreeSelection) usersTreeViewer
                        .getSelection();
                Object node = selection.getFirstElement();
                if (node instanceof IRosterEntry) {
                    IRosterEntry user = (IRosterEntry) node;
                    if (user.getPresence().getType() == Type.AVAILABLE) {
                        UserId loginUserId = CollaborationDataManager
                                .getInstance().getCollaborationConnection()
                                .getUser();
                        if (!loginUserId.equals(user)) {
                            createP2PChat(IDConverter.convertFrom(user
                                    .getUser()));
                        }
                    }
                }
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
        Collection<Object> obs = CollaborationUtils.readAliases();
        for (Object ob : obs) {
            if (ob instanceof IRosterGroup) {
                addGroup((IRosterGroup) ob);
            }
        }
    }

    private void fillStatusMenu(Menu menu) {
        for (int index = 0; index < CollaborationUtils.statusModes.length; ++index) {
            IPresence.Mode mode = CollaborationUtils.statusModes[index];
            Action action = new Action(mode.toString()) {
                public void run() {
                    changeStatusAction.setId(getId());
                    changeStatusAction.run();
                };
            };
            action.setId(mode.toString().toUpperCase());
            ActionContributionItem item = new ActionContributionItem(action);
            action.setImageDescriptor(IconUtil.getImageDescriptor(Activator
                    .getDefault().getBundle(),
                    mode.toString().replaceAll("\\s+", "_").toLowerCase()
                            + ".gif"));
            item.fill(menu, -1);
        }
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
                UserId id = IDConverter.convertFrom(user.getUser());
                // use the fq name so that we know who we need to chat with as a
                // default
                peerToPeerChatAction.setId(id.getFQName());
                manager.add(new Separator());
                manager.add(createSessionAction);
            }
            manager.add(aliasAction);
        } else if (o instanceof IRosterGroup) {
            manager.add(createSessionAction);
        }
    }

    private void addDoubleClickListeners() {
        usersTreeViewer.addDoubleClickListener(new IDoubleClickListener() {
            @Override
            public void doubleClick(DoubleClickEvent event) {
                peerToPeerChatAction.run();
            }
        });
        usersTreeViewer.addDoubleClickListener(new IDoubleClickListener() {
            @Override
            public void doubleClick(DoubleClickEvent event) {
                joinAction.run();
            }
        });
    }

    /**
     * Add the ability to alias items in the group view and then to have that
     * alias be used anywhere else that the UserId is used
     * 
     * Saves to an xml file in localization where the user can edit if desired
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
                    UserId id = IDConverter.convertFrom(entry.getUser());
                    id.setAlias(treeEditor.getItem().getText());
                    ((User) entry.getUser()).setNickname(treeEditor.getItem()
                            .getText());
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
                    ((User) entry.getUser()).setNickname(treeEditor.getItem()
                            .getText());
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
        Mode mode = Mode.fromString(Activator.getDefault().getPreferenceStore()
                .getString(CollabPrefConstants.P_STATUS).toLowerCase());
        String msg = Activator.getDefault().getPreferenceStore()
                .getString(CollabPrefConstants.P_MESSAGE);
        CollaborationDataManager.getInstance().fireModifiedPresence(mode, msg);

        // need to refresh the local tree so that the top user shows up with the
        // current status
        UserId id = (UserId) topLevel.getObjects().get(0);
        usersTreeViewer.refresh(id);
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
                .getCurrent().getActiveShell(), getSelectedUsers().size() > 0);
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
                        usersList.add(IDConverter.convertFrom(user.getUser()));
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
            UserId id = (UserId) peer;
            String name = peer.getFQName();
            TreeSelection selection = (TreeSelection) usersTreeViewer
                    .getSelection();
            IRosterEntry entry = (IRosterEntry) selection.getFirstElement();
            if (id.getAlias() != null && !id.getAlias().isEmpty()) {
                name = id.getAlias();
            } else if (entry.getUser().getName() != null
                    && !entry.getUser().getName().isEmpty()) {
                name = entry.getUser().getName();
            } else {
                name = entry.getName();
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
        ColumnViewerToolTipSupport.enableFor(usersTreeViewer, ToolTip.RECREATE);
        topLevel = new CollaborationGroupContainer();
        usersTreeViewer.setInput(topLevel);

        treeEditor = new TreeEditor(usersTreeViewer.getTree());
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
        for (Object node : groupNode.getEntries()) {
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

    /**
     * Refresh the labels on the View Tree to reflect presence change.
     * 
     * @param rosterEntry
     */
    @Subscribe
    public void handleModifiedPresence(final IRosterEntry rosterEntry) {
        // Only need to update the usersTreeViewer.
        final UserId id = IDConverter.convertFrom(rosterEntry.getUser());
        System.out.println("group view roster entry for:" + id.getName() + "@"
                + id.getHost() + " " + rosterEntry.getPresence().getMode()
                + "/" + rosterEntry.getPresence().getType());

        ((RosterEntry) CollaborationDataManager.getInstance()
                .getCollaborationConnection().getContactsManager()
                .getUsersMap().get(id)).setPresence(rosterEntry.getPresence());
        VizApp.runAsync(new Runnable() {
            @Override
            public void run() {
                for (Object ob : rosterEntry.getGroups()) {
                    IRosterGroup group = (IRosterGroup) ob;
                    refreshUser(id, group);
                }
            }
        });
    }

    @Subscribe
    public void handleRosterChangeEvent(IRosterChangeEvent rosterChangeEvent) {
        final IRosterItem rosterItem = rosterChangeEvent.getItem();
        CollaborationConnection connection = CollaborationDataManager
                .getInstance().getCollaborationConnection();
        switch (rosterChangeEvent.getType()) {
        case MODIFY:
        case ADD:
            if (rosterItem instanceof IRosterEntry) {
                refreshEntry((IRosterEntry) rosterItem, connection);
                VizApp.runAsync(new Runnable() {
                    @Override
                    public void run() {
                        usersTreeViewer.refresh(rosterItem);
                    }
                });
            } else if (rosterItem instanceof IRosterGroup) {
            } else if (rosterItem instanceof IRoster) {
                for (Object ob : ((IRoster) rosterItem).getItems()) {
                    if (ob instanceof IRosterEntry) {
                        refreshEntry((RosterEntry) rosterItem, connection);
                    } else if (ob instanceof IRosterGroup) {
                        for (Object gOb : ((IRosterGroup) ob).getEntries()) {
                            IRosterEntry entry = (IRosterEntry) gOb;
                            refreshEntry(entry, connection);
                        }
                    }
                }
                VizApp.runAsync(new Runnable() {
                    @Override
                    public void run() {
                        usersTreeViewer.refresh(topLevel);
                    }
                });
            }
            break;
        case DELETE:
            if (rosterItem instanceof IRosterEntry) {
                IRosterEntry mainEntry = (IRosterEntry) rosterItem;
                for (final Object ob : topLevel.getObjects()) {
                    if (ob instanceof IRosterEntry) {
                        IRosterEntry entry = (IRosterEntry) ob;
                        if (entry.getUser().getName()
                                .equals(mainEntry.getUser().getName())) {
                            topLevel.removeObject(entry);
                        }
                    } else if (ob instanceof IRosterGroup) {
                        IRosterGroup group = (IRosterGroup) ob;
                        List<IRosterEntry> entries = new ArrayList<IRosterEntry>();
                        entries.addAll(group.getEntries());
                        for (Object entryOb : entries) {
                            IRosterEntry entry = (IRosterEntry) entryOb;
                            if (entry.getUser().getName()
                                    .equals(mainEntry.getUser().getName())) {
                                ((RosterGroup) group).remove(entry);
                            }
                        }
                    }
                    VizApp.runAsync(new Runnable() {
                        @Override
                        public void run() {
                            usersTreeViewer.refresh(ob);
                        }
                    });
                }
            } else if (rosterItem instanceof IRosterGroup) {
                IRosterGroup mainGroup = (IRosterGroup) rosterItem;
                for (final Object ob : topLevel.getObjects()) {
                    IRosterGroup group = (IRosterGroup) ob;
                    if (ob instanceof IRosterGroup) {
                        if (mainGroup.getName().equals(group.getName())) {
                            topLevel.removeObject(group);

                            VizApp.runAsync(new Runnable() {
                                @Override
                                public void run() {
                                    usersTreeViewer.refresh(ob);
                                }
                            });
                            break;
                        }
                    }
                }
            }
            break;
        default:
            statusHandler.handle(Priority.PROBLEM, "Unknown type: "
                    + rosterChangeEvent.getType());
            break;
        }
    }

    /**
     * Adds users to groups if necessary
     * 
     * @param entry
     * @param connection
     */
    private void refreshEntry(IRosterEntry entry,
            CollaborationConnection connection) {
        List<IRosterGroup> groups = new ArrayList<IRosterGroup>();
        for (Object ob : connection.getRosterManager().getRoster().getItems()) {
            if (ob instanceof IRosterGroup) {
                groups.add((IRosterGroup) ob);
            }
        }

        // looping through my groups
        for (final IRosterGroup group : groups) {
            // looping through the other entries groups
            for (Object ob2 : entry.getGroups()) {
                IRosterGroup otherGroup = (IRosterGroup) ob2;
                // check to make sure the groups are the same group...
                // so both users have it (you are showing it, and they
                // are part of it)
                if (group.getName().equals(otherGroup.getName())) {
                    addGroup(otherGroup);
                }
            }
        }
    }

    private synchronized void addGroup(IRosterGroup group) {
        boolean created = false;
        for (Object topOb : topLevel.getObjects()) {
            if (topOb instanceof IRosterGroup) {
                IRosterGroup topGroup = (IRosterGroup) topOb;
                if (topGroup.getName().equals(group.getName())) {
                    System.out.println("Created is true : " + group.getName()
                            + " / " + topGroup.getName());
                    created = true;
                    break;
                } else {
                    System.out.println("Created is false : " + group.getName()
                            + " / " + topGroup.getName());
                }
            }
        }
        if (!created) {
            System.out.println("creating group : " + group.getName());
            topLevel.addObject(group);
        }
    }

    // Does nothing, but necessary due to ViewPart
    @Override
    public void setFocus() {
        // nothing to do in this method
    }

    // the following methods are unused but needed due to implementing the
    // IPartListener interface

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
        }
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

                // compare session ids, if they match then this is the session
                // that was closed and we should remove it from the list of
                // active sessions
                if (sessionId.equals(group.getSessionId())) {
                    activeSessionGroup.removeObject(node);
                    usersTreeViewer.refresh(activeSessionGroup);
                    break;
                }
            }
        }
    }

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

    @Override
    public void partBroughtToTop(IWorkbenchPart part) {
        // nothing to do in this method
    }

    @Override
    public void partDeactivated(IWorkbenchPart part) {
        // nothing to do in this method
    }
}
