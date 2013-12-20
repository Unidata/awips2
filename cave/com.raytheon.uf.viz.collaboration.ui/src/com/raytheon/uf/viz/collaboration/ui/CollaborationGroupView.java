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

import java.util.Arrays;
import java.util.HashSet;
import java.util.Set;

import org.eclipse.jface.action.Action;
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
import org.eclipse.jface.viewers.ViewerFilter;
import org.eclipse.jface.window.ToolTip;
import org.eclipse.swt.SWT;
import org.eclipse.swt.accessibility.ACC;
import org.eclipse.swt.accessibility.AccessibleAdapter;
import org.eclipse.swt.accessibility.AccessibleControlAdapter;
import org.eclipse.swt.accessibility.AccessibleControlEvent;
import org.eclipse.swt.accessibility.AccessibleEvent;
import org.eclipse.swt.custom.TreeEditor;
import org.eclipse.swt.events.DisposeEvent;
import org.eclipse.swt.events.DisposeListener;
import org.eclipse.swt.events.KeyAdapter;
import org.eclipse.swt.events.KeyEvent;
import org.eclipse.swt.events.MouseAdapter;
import org.eclipse.swt.events.MouseEvent;
import org.eclipse.swt.events.MouseMoveListener;
import org.eclipse.swt.events.MouseTrackAdapter;
import org.eclipse.swt.graphics.GC;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.graphics.Point;
import org.eclipse.swt.graphics.Rectangle;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Event;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Listener;
import org.eclipse.swt.widgets.Menu;
import org.eclipse.swt.widgets.Text;
import org.eclipse.swt.widgets.TreeColumn;
import org.eclipse.swt.widgets.TreeItem;
import org.eclipse.ui.IViewPart;
import org.eclipse.ui.IViewReference;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.internal.WorkbenchMessages;
import org.eclipse.ui.plugin.AbstractUIPlugin;
import org.jivesoftware.smack.RosterEntry;
import org.jivesoftware.smack.RosterGroup;
import org.jivesoftware.smack.packet.Presence;
import org.jivesoftware.smack.packet.Presence.Type;
import org.osgi.framework.Bundle;

import com.google.common.eventbus.Subscribe;
import com.raytheon.uf.viz.collaboration.comm.identity.IVenueSession;
import com.raytheon.uf.viz.collaboration.comm.identity.event.IRosterChangeEvent;
import com.raytheon.uf.viz.collaboration.comm.provider.event.ServerDisconnectEvent;
import com.raytheon.uf.viz.collaboration.comm.provider.event.UserNicknameChangedEvent;
import com.raytheon.uf.viz.collaboration.comm.provider.event.UserPresenceChangedEvent;
import com.raytheon.uf.viz.collaboration.comm.provider.session.CollaborationConnection;
import com.raytheon.uf.viz.collaboration.comm.provider.user.ContactsManager;
import com.raytheon.uf.viz.collaboration.comm.provider.user.ContactsManager.LocalGroupListener;
import com.raytheon.uf.viz.collaboration.comm.provider.user.IDConverter;
import com.raytheon.uf.viz.collaboration.comm.provider.user.LocalGroups.LocalGroup;
import com.raytheon.uf.viz.collaboration.comm.provider.user.UserId;
import com.raytheon.uf.viz.collaboration.ui.actions.AddToGroupAction;
import com.raytheon.uf.viz.collaboration.ui.actions.ArchiveViewerAction;
import com.raytheon.uf.viz.collaboration.ui.actions.ChangeFontAction;
import com.raytheon.uf.viz.collaboration.ui.actions.ChangePasswordAction;
import com.raytheon.uf.viz.collaboration.ui.actions.ChangeRoleAction;
import com.raytheon.uf.viz.collaboration.ui.actions.ChangeSiteAction;
import com.raytheon.uf.viz.collaboration.ui.actions.ChangeStatusAction;
import com.raytheon.uf.viz.collaboration.ui.actions.ChangeStatusMessageAction;
import com.raytheon.uf.viz.collaboration.ui.actions.CreateGroupAction;
import com.raytheon.uf.viz.collaboration.ui.actions.CreateSessionAction;
import com.raytheon.uf.viz.collaboration.ui.actions.DeleteGroupAction;
import com.raytheon.uf.viz.collaboration.ui.actions.DisplayFeedAction;
import com.raytheon.uf.viz.collaboration.ui.actions.InviteAction;
import com.raytheon.uf.viz.collaboration.ui.actions.LinkToEditorAction;
import com.raytheon.uf.viz.collaboration.ui.actions.LoginAction;
import com.raytheon.uf.viz.collaboration.ui.actions.LogoutAction;
import com.raytheon.uf.viz.collaboration.ui.actions.PeerToPeerChatAction;
import com.raytheon.uf.viz.collaboration.ui.actions.RemoveFromGroupAction;
import com.raytheon.uf.viz.collaboration.ui.actions.RemoveFromRosterAction;
import com.raytheon.uf.viz.collaboration.ui.actions.ShowVenueAction;
import com.raytheon.uf.viz.collaboration.ui.actions.UserSearchAction;
import com.raytheon.uf.viz.collaboration.ui.data.AlertWordWrapper;
import com.raytheon.uf.viz.collaboration.ui.data.CollaborationGroupContainer;
import com.raytheon.uf.viz.collaboration.ui.data.SessionGroupContainer;
import com.raytheon.uf.viz.collaboration.ui.session.AbstractSessionView;
import com.raytheon.uf.viz.core.VizApp;
import com.raytheon.uf.viz.core.icon.IconUtil;
import com.raytheon.viz.ui.views.CaveFloatingView;

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
 * Oct 22, 2013 #2483      lvenable    Fixed image memory leak.
 * Dec  6, 2013 2561       bclement    removed ECF
 * Dec 19, 2013 2563       bclement    added subscribe method for server disconnection
 * Dec 20, 2013 2563       bclement    fixed support for ungrouped roster items
 * 
 * </pre>
 * 
 * @author rferrel
 * @version 1.0
 */
public class CollaborationGroupView extends CaveFloatingView implements
        LocalGroupListener, IUserSelector {
    public static final String ID = "com.raytheon.uf.viz.collaboration.ui.CollaborationGroupView";

    private TreeViewer usersTreeViewer;

    private UsersTreeFilter usersTreeFilter;

    private CollaborationGroupContainer topLevel;

    private CreateSessionAction createSessionAction;

    private Action aliasAction;

    private DisplayFeedAction displayFeedAction;

    private Action collapseAllAction;

    private TreeEditor treeEditor;

    private Composite parent;

    private Image inactiveImage = null;

    private Image activeImage = null;

    private Image pressedImage = null;

    private LogoutAction logOut;

    /**
     * @param parent
     */
    @Override
    public void createPartControl(Composite parent) {
        super.createPartControl(parent);
        this.parent = parent;
        this.parent.setLayout(new GridLayout());

        createImages();

        // build the necessary actions for the view
        createActions();

        // add some actions to the toolbar
        createToolbar();

        // add some actions to the menubar
        createMenubar();
        openConnection();
    }

    /**
     * Create images.
     */
    private void createImages() {
        inactiveImage = AbstractUIPlugin.imageDescriptorFromPlugin(
                PlatformUI.PLUGIN_ID, "$nl$/icons/full/dtool16/clear_co.gif")
                .createImage();
        activeImage = AbstractUIPlugin.imageDescriptorFromPlugin(
                PlatformUI.PLUGIN_ID, "$nl$/icons/full/etool16/clear_co.gif")
                .createImage();
        pressedImage = new Image(Display.getCurrent(), activeImage,
                SWT.IMAGE_GRAY);
    }

    private void openConnection() {
        CollaborationConnection connection = CollaborationConnection
                .getConnection();
        if (connection == null) {
            new LoginAction().run();
            connection = CollaborationConnection.getConnection();
            if (connection == null) {
                // user cancelled login
                return;
            }
        }

        createFilterText(parent);
        createUsersTree(parent);
        addDoubleClickListeners();
        createContextMenu();

        if (connection != null) {
            connection.registerEventHandler(this);
        }
        connection.getContactsManager().addLocalGroupListener(this);
        populateTree();
        usersTreeViewer.refresh();
        parent.layout();
    }

    @Override
    public void dispose() {
        CollaborationConnection connection = CollaborationConnection
                .getConnection();
        if (connection != null) {
            connection.unregisterEventHandler(this);
            connection.getContactsManager().removeLocalGroupListener(this);
        }
        super.dispose();

        inactiveImage.dispose();
        activeImage.dispose();
        pressedImage.dispose();
    }

    /**
     * 
     */
    private void createActions() {
        Bundle bundle = Activator.getDefault().getBundle();
        final IUserSelector userSelector = this;

        createSessionAction = new CreateSessionAction(userSelector);

        aliasAction = new Action("Alias") {
            @Override
            public void run() {
                aliasItem(getId());
            };
        };

        collapseAllAction = new Action("Collapse All") {
            public void run() {
                if (usersTreeViewer != null) {
                    usersTreeViewer.collapseAll();
                }
            }
        };
        collapseAllAction.setImageDescriptor(IconUtil.getImageDescriptor(
                bundle, "collapseall.gif"));

        // this is either on or off, so it is a toggle
        displayFeedAction = new DisplayFeedAction();

        this.disableOrEnableToolbarActions();
    }

    /**
     * Create the toolbar on top of the group view
     */
    private void createToolbar() {
        IToolBarManager mgr = getViewSite().getActionBars().getToolBarManager();
        mgr.add(createSessionAction);
        mgr.add(displayFeedAction);
        mgr.add(collapseAllAction);
        mgr.add(LinkToEditorAction.getInstance(getViewSite()
                .getWorkbenchWindow()));
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
        mgr.add(new CreateGroupAction());
        mgr.add(new UserSearchAction());
        mgr.add(new Separator());
        mgr.add(new ChangeFontAction());
        mgr.add(new ChangeStatusAction());
        mgr.add(new ChangeStatusMessageAction());
        mgr.add(new ChangePasswordAction());
        mgr.add(new Separator());
        mgr.add(new ChangeRoleAction());
        mgr.add(new ChangeSiteAction());
        mgr.add(new Separator());
        mgr.add(new ArchiveViewerAction());
        mgr.add(new Separator());

        if (CollaborationConnection.getConnection() != null) {
            logOut = new LogoutAction();
            mgr.add(logOut);
        } else {
            mgr.add(new LoginAction());
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
        CollaborationConnection connection = CollaborationConnection
                .getConnection();
        // set all the menu actions to false to start with
        if (connection == null) {
            usersTreeViewer.getTree().setEnabled(false);
            return;
        }

        // add ability to drop new groups as well as drop roster entries into
        // new "custom" groups
        addDragAndDrop();

        // enable the tree, and then refresh it just to be safe
        usersTreeViewer.getTree().setEnabled(true);
        usersTreeViewer.refresh(topLevel, true);
        this.disableOrEnableToolbarActions();
    }

    /**
     * Filling the context menu for the tree depending on whether the item is a
     * group or a user
     * 
     * @paramfillContextMenu manager
     */
    private void fillContextMenu(IMenuManager manager) {
        TreeSelection selection = (TreeSelection) usersTreeViewer
                .getSelection();
        Object o = selection.getFirstElement();

        // handle the session group portion of the group view
        if (o instanceof SessionGroupContainer) {
            manager.add(createSessionAction);
            return;
        } else if (o instanceof IVenueSession) {
            manager.add(new ShowVenueAction((IVenueSession) o));
            manager.add(new ArchiveViewerAction((IVenueSession) o));
            return;
        } else if (o instanceof RosterEntry) {
            // roster entries that are not in a group
            RosterEntry entry = (RosterEntry) o;
            UserId user = IDConverter.convertFrom(entry);
            addOnlineMenuOptions(manager, selection, user);
            addAliasAction(manager, selection, user);
            manager.add(new ArchiveViewerAction(user));
            manager.add(new AddToGroupAction(entry));
            manager.add(new RemoveFromRosterAction(entry));
        } else if (o instanceof UserId) {
            // the user, both the logged in user as well as entries in groups
            UserId user = (UserId) o;
            fillContextMenu(manager, selection, user);
        } else if (o instanceof RosterGroup || o instanceof LocalGroup) {
            manager.add(createSessionAction);
            if (o instanceof LocalGroup) {
                LocalGroup group = (LocalGroup) o;
                manager.add(new DeleteGroupAction(group.getName()));
                aliasAction.setId(group.getName());
                aliasAction.setText("Rename Group");
                manager.add(aliasAction);
            }
        }
    }
    
    /**
     * Populate menu for roster entries. Checks for current user to create
     * appropriate menu.
     * 
     * @param manager
     * @param selection
     * @param user
     */
    private void fillContextMenu(IMenuManager manager, TreeSelection selection,
            UserId user) {
        CollaborationConnection connection = CollaborationConnection
                .getConnection();
        UserId me = connection.getUser();
        if (me.isSameUser(user)) {
            createMenu(manager);
            return;
        }
        addOnlineMenuOptions(manager, selection, user);
        addAliasAction(manager, selection, user);

        manager.add(new ArchiveViewerAction(user));
        manager.add(new AddToGroupAction(getSelectedUsers()));
        String groupName = null;
        Object group = selection.getPaths()[0].getFirstSegment();
        if (group instanceof LocalGroup) {
            groupName = ((LocalGroup) group).getName();
            manager.add(new RemoveFromGroupAction(groupName, getSelectedUsers()));
        }
    }

    /**
     * Add interaction menu options for contact if they are online
     * 
     * @param manager
     * @param selection
     * @param user
     */
    private void addOnlineMenuOptions(IMenuManager manager,
            TreeSelection selection, UserId user) {
        CollaborationConnection connection = CollaborationConnection
                .getConnection();
        Presence presence = connection.getContactsManager().getPresence(user);
        if (presence != null && presence.getType() == Type.available) {
            Action inviteAction = new InviteAction(user);
            if (inviteAction.isEnabled()) {
                manager.add(inviteAction);
            }
            Action p2pAction = new PeerToPeerChatAction(user);
            if (p2pAction.isEnabled()) {
                manager.add(p2pAction);
            }
            manager.add(new Separator());
            manager.add(createSessionAction);
        }
    }

    /**
     * Add menu option for aliasing username
     * 
     * @param manager
     * @param selection
     * @param user
     */
    private void addAliasAction(IMenuManager manager, TreeSelection selection,
            UserId user) {
        CollaborationConnection connection = CollaborationConnection
                .getConnection();
        String name = connection.getContactsManager().getDisplayName(user);
        aliasAction.setId(name);
        aliasAction.setText("Alias");
        manager.add(aliasAction);
    }

    private void addDoubleClickListeners() {
        usersTreeViewer.addDoubleClickListener(new IDoubleClickListener() {
            @Override
            public void doubleClick(DoubleClickEvent event) {
                TreeSelection selection = (TreeSelection) usersTreeViewer
                        .getSelection();
                Object o = selection.getFirstElement();
                if (o instanceof UserId) {
                    new PeerToPeerChatAction((UserId) o).run();
                } else if (o instanceof IVenueSession) {
                    new ShowVenueAction((IVenueSession) o).run();
                }
            }
        });
    }

    /**
     * Add the ability to alias items in the group view and then to have that
     * alias be used anywhere else that the UserId is used
     * 
     * Saves to an xml file in localization where the user can edit if desired
     */
    protected void aliasItem(final String editableText) {
        Control oldEditor = treeEditor.getEditor();
        if (oldEditor != null) {
            oldEditor.dispose();
        }
        TreeSelection selection = (TreeSelection) usersTreeViewer
                .getSelection();
        final Object selectedObj = selection.getFirstElement();

        final TreeItem item = usersTreeViewer.getTree().getSelection()[0];
        final Composite composite = new Composite(usersTreeViewer.getTree(),
                SWT.NONE);
        composite.setBackground(Display.getCurrent().getSystemColor(
                SWT.COLOR_BLACK));
        final Text modText = new Text(composite, SWT.NONE);
        composite.addListener(SWT.Resize, new Listener() {
            public void handleEvent(Event e) {
                Rectangle rect = composite.getClientArea();
                modText.setBounds(rect.x + 1, rect.y + 1, rect.width - 2,
                        rect.height - 2);
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
                    String fullText = modText.getText();
                    composite.dispose();
                    changeText(selectedObj, fullText);
                    refreshUsersTreeViewerAsync(selectedObj);
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
                    treeEditor.minimumWidth = Math.max(size.x, itemRect.width) + 2;
                    int left = itemRect.x,
                    right = rect.x + rect.width;
                    treeEditor.minimumWidth = Math.min(treeEditor.minimumWidth,
                            right - left);
                    treeEditor.minimumHeight = size.y + 2;
                    treeEditor.layout();
                    break;
                }
            }
        };

        modText.addListener(SWT.KeyUp, textListener);
        modText.addListener(SWT.Verify, textListener);
        modText.addListener(SWT.FocusOut, textListener);
        treeEditor.setEditor(composite, item);

        modText.setText(editableText);
        modText.selectAll();
        modText.setFocus();
    }

    protected void changeText(Object selectedObj, String newText) {
        if (selectedObj instanceof UserId) {
            UserId user = (UserId) selectedObj;
            user.setAlias(newText);
            CollaborationConnection.getConnection().getContactsManager()
                    .setNickname(user, newText);
            CollaborationConnection.getConnection().postEvent(user);
            for (LocalGroup group : CollaborationConnection.getConnection()
                    .getContactsManager().getLocalGroups(user)) {
                usersTreeViewer.refresh(group);
            }
        } else if (selectedObj instanceof LocalGroup) {
            LocalGroup group = (LocalGroup) selectedObj;
            CollaborationConnection.getConnection().getContactsManager()
                    .renameLocalGroup(group.getName(), newText);
        }
    }

    /**
     * Code was copied from FilteredTree, as FilteredTree did not offer an
     * advanced enough matching capability
     * 
     * This creates a nice looking text widget with a button embedded in the
     * widget
     * 
     * @param parent
     */
    private void createFilterText(Composite parent) {
        Composite comp = new Composite(parent, SWT.BORDER);
        comp.setBackground(Display.getCurrent().getSystemColor(
                SWT.COLOR_LIST_BACKGROUND));
        GridLayout layout = new GridLayout(2, false);
        layout.marginHeight = 0;
        layout.marginWidth = 0;
        comp.setLayout(layout);
        GridData gd = new GridData(SWT.FILL, SWT.NONE, true, false);
        comp.setLayoutData(gd);

        final Text text = new Text(comp, SWT.SINGLE | SWT.NONE);
        GridData data = new GridData(SWT.FILL, SWT.NONE, true, false);
        text.setLayoutData(data);
        text.addKeyListener(new KeyAdapter() {
            @Override
            public void keyReleased(KeyEvent e) {
                filter(text);
            }
        });

        // only create the button if the text widget doesn't support one
        // natively
        final Label clearButton = new Label(comp, SWT.NONE);
        clearButton.setLayoutData(new GridData(SWT.BEGINNING, SWT.CENTER,
                false, false));
        clearButton.setImage(inactiveImage);
        clearButton.setBackground(parent.getDisplay().getSystemColor(
                SWT.COLOR_LIST_BACKGROUND));
        clearButton.setToolTipText(WorkbenchMessages.FilteredTree_ClearToolTip);
        clearButton.addMouseListener(new MouseAdapter() {
            private MouseMoveListener fMoveListener;

            public void mouseDown(MouseEvent e) {
                clearButton.setImage(pressedImage);
                fMoveListener = new MouseMoveListener() {
                    private boolean fMouseInButton = true;

                    public void mouseMove(MouseEvent e) {
                        boolean mouseInButton = isMouseInButton(e);
                        if (mouseInButton != fMouseInButton) {
                            fMouseInButton = mouseInButton;
                            clearButton.setImage(mouseInButton ? pressedImage
                                    : inactiveImage);
                        }
                    }
                };
                clearButton.addMouseMoveListener(fMoveListener);
            }

            public void mouseUp(MouseEvent e) {
                if (fMoveListener != null) {
                    clearButton.removeMouseMoveListener(fMoveListener);
                    fMoveListener = null;
                    boolean mouseInButton = isMouseInButton(e);
                    clearButton.setImage(mouseInButton ? activeImage
                            : inactiveImage);
                    if (mouseInButton) {
                        text.setText("");
                        filter(text);
                        text.setFocus();
                    }
                }
            }

            private boolean isMouseInButton(MouseEvent e) {
                Point buttonSize = clearButton.getSize();
                return 0 <= e.x && e.x < buttonSize.x && 0 <= e.y
                        && e.y < buttonSize.y;
            }
        });
        clearButton.addMouseTrackListener(new MouseTrackAdapter() {
            public void mouseEnter(MouseEvent e) {
                clearButton.setImage(activeImage);
            }

            public void mouseExit(MouseEvent e) {
                clearButton.setImage(inactiveImage);
            }
        });
        clearButton.addDisposeListener(new DisposeListener() {
            public void widgetDisposed(DisposeEvent e) {
                inactiveImage.dispose();
                activeImage.dispose();
                pressedImage.dispose();
            }
        });
        clearButton.getAccessible().addAccessibleListener(
                new AccessibleAdapter() {
                    public void getName(AccessibleEvent e) {
                        e.result = WorkbenchMessages.FilteredTree_AccessibleListenerClearButton;
                    }
                });
        clearButton.getAccessible().addAccessibleControlListener(
                new AccessibleControlAdapter() {
                    public void getRole(AccessibleControlEvent e) {
                        e.detail = ACC.ROLE_PUSHBUTTON;
                    }
                });
    }

    /**
     * Function to run through the tree and expand all elements if something is
     * typed in the filter field, otherwise collapse them all
     * 
     * @param text
     */
    private void filter(Text text) {
        // call refresh on the tree to get the most up-to-date children
        usersTreeViewer.refresh(false);

        if (usersTreeFilter == null) {
            ViewerFilter[] filters = new ViewerFilter[1];
            usersTreeFilter = new UsersTreeFilter();
            filters[0] = usersTreeFilter;
            usersTreeViewer.setFilters(filters);
        }

        // set the current filter text so that it can be used when refresh is
        // called again
        usersTreeFilter.setCurrentText(text.getText());
        if (text.getText().length() == 0) {
            for (Object ob : topLevel.getObjects()) {
                usersTreeViewer.setExpandedState(ob, false);
            }
        }
        // if text contains anything then expand all items in the tree
        if (text.getText().length() > 0) {
            for (Object ob : topLevel.getObjects()) {
                usersTreeViewer.setExpandedState(ob, true);
            }
        }
        // call refresh on the tree after things are expanded
        usersTreeViewer.refresh(false);
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
        usersTreeViewer = new TreeViewer(child, SWT.VIRTUAL | SWT.MULTI
                | SWT.V_SCROLL | SWT.H_SCROLL | SWT.BORDER);
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
     * Get a unique set of selected users that have a Type of AVAILABLE. This
     * does a recursive search so will work even when groups contain groups.
     * 
     * @return
     */
    public UserId[] getSelectedUsers() {
        Set<UserId> selectedUsers = new HashSet<UserId>();
        IStructuredSelection selection = (IStructuredSelection) usersTreeViewer
                .getSelection();
        Object[] nodes = selection.toArray();

        for (Object node : nodes) {
            if (node instanceof UserId) {
                UserId user = (UserId) node;
                selectedUsers.add(user);
            } else if (node instanceof RosterEntry) {
                UserId user = IDConverter.convertFrom((RosterEntry) node);
                selectedUsers.add(user);
            } else if (node instanceof RosterGroup) {
                selectedUsers.addAll(getSelectedUsers((RosterGroup) node));
            } else if (node instanceof LocalGroup) {
                for (UserId user : ((LocalGroup) node).getUsers()) {
                    Presence presence = CollaborationConnection
                            .getConnection().getContactsManager()
                            .getPresence(user);
                    if (presence.getType() == Presence.Type.available) {
                        selectedUsers.add(user);
                    }
                }
            }
        }

        return selectedUsers.toArray(new UserId[selectedUsers.size()]);
    }

    /**
     * This recursively searches group Nodes and returns all users with Type
     * AVAILABLE.
     * 
     * @param groupNode
     * @return users
     */
    private Set<UserId> getSelectedUsers(RosterGroup groupNode) {
        Set<UserId> selectedUsers = new HashSet<UserId>();
        ContactsManager contacts = CollaborationConnection.getConnection()
                .getContactsManager();
        for (RosterEntry node : groupNode.getEntries()) {
            UserId user = IDConverter.convertFrom(node);
            Presence presence = contacts.getPresence(user);
            if (presence.getType() == Type.available) {
                selectedUsers.add(user);
            }
        }
        return selectedUsers;
    }

    private void refreshUsersTreeViewerAsync(final Object element) {
        VizApp.runAsync(new Runnable() {
            @Override
            public void run() {
                if (usersTreeViewer.getControl().isDisposed() == false) {
                    usersTreeViewer.refresh(element);
                }
            }
        });
    }

    /**
     * Refresh the labels on the View Tree to reflect presence change.
     * 
     * @param rosterEntry
     */
    @Subscribe
    public void handleModifiedPresence(UserPresenceChangedEvent event) {
        refreshUsersTreeViewerAsync(CollaborationConnection.getConnection()
                .getUser());
    }

    @Subscribe
    public void addAlertWords(AlertWordWrapper words) {
        for (IViewReference ref : getViewSite().getWorkbenchWindow()
                .getActivePage().getViewReferences()) {
            IViewPart viewPart = ref.getView(false);
            if (viewPart instanceof AbstractSessionView) {
                ((AbstractSessionView) viewPart).setAlertWords(Arrays
                        .asList(words.getAlertWords()));
            }
        }
    }

    @Subscribe
    public void handleRosterChangeEvent(IRosterChangeEvent rosterChangeEvent) {
        // Refresh the whole tree since there can be instances of the same user
        // elsewhere that might not .equals this one.
        refreshUsersTreeViewerAsync(usersTreeViewer.getInput());
    }

    @Subscribe
    public void handlSessionEvent(IVenueSession rosterChangeEvent) {
        refreshUsersTreeViewerAsync(topLevel.getSessionGroup());
    }

    /**
     * Enables or disables the toolbar buttons based on whether or not the user
     * is connected to the xmpp server.
     */
    private void disableOrEnableToolbarActions() {
        boolean enabled = (CollaborationConnection.getConnection() != null);
        createSessionAction.setEnabled(enabled);
        displayFeedAction.setEnabled(enabled);
        collapseAllAction.setEnabled(enabled);
        LinkToEditorAction.getInstance(getViewSite().getWorkbenchWindow());
    }

    private void addDragAndDrop() {
        // CollaborationGroupDragNDrop dragNDropSource = new
        // CollaborationGroupDragNDrop(
        // usersTreeViewer);
        // usersTreeViewer.addDragSupport(DND.DROP_MOVE | DND.DROP_COPY,
        // new Transfer[] { TextTransfer.getInstance() }, dragNDropSource);
        // usersTreeViewer.addDropSupport(DND.DROP_MOVE | DND.DROP_COPY,
        // new Transfer[] { TextTransfer.getInstance() }, dragNDropSource);
    }

    // Does nothing, but necessary due to ViewPart
    @Override
    public void setFocus() {
        // nothing to do in this method
    }

    @Override
    public void groupCreated(LocalGroup group) {
        refreshUsersTreeViewerAsync(usersTreeViewer.getInput());
    }

    @Override
    public void groupDeleted(LocalGroup group) {
        refreshUsersTreeViewerAsync(usersTreeViewer.getInput());
    }

    @Override
    public void userAdded(LocalGroup group, UserId user) {
        refreshUsersTreeViewerAsync(group);
    }

    @Override
    public void userDeleted(LocalGroup group, UserId user) {
        refreshUsersTreeViewerAsync(group);
    }

    @Subscribe
    public void userNicknameChanged(UserNicknameChangedEvent e) {
        refreshUsersTreeViewerAsync(usersTreeViewer.getInput());
    }

    @Subscribe
    public void serverDisconnected(final ServerDisconnectEvent e) {
        if (logOut == null) {
            // we aren't logged in
            return;
        }
        VizApp.runAsync(new Runnable() {
            @Override
            public void run() {
                logOut.closeCollaboration();
            }
        });
    }
}
