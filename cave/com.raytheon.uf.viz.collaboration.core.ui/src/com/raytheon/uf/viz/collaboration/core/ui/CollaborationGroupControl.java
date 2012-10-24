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
package com.raytheon.uf.viz.collaboration.core.ui;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import javax.swing.tree.TreePath;

import org.eclipse.ecf.presence.roster.IRoster;
import org.eclipse.ecf.presence.roster.IRosterEntry;
import org.eclipse.ecf.presence.roster.IRosterGroup;
import org.eclipse.jface.action.IContributionItem;
import org.eclipse.jface.action.IMenuListener;
import org.eclipse.jface.action.IMenuManager;
import org.eclipse.jface.action.MenuManager;
import org.eclipse.jface.viewers.TreeViewer;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.DisposeEvent;
import org.eclipse.swt.events.DisposeListener;
import org.eclipse.swt.events.KeyAdapter;
import org.eclipse.swt.events.KeyEvent;
import org.eclipse.swt.events.MouseAdapter;
import org.eclipse.swt.events.MouseEvent;
import org.eclipse.swt.events.MouseTrackAdapter;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Menu;
import org.eclipse.swt.widgets.Text;
import org.eclipse.swt.widgets.Tree;
import org.eclipse.swt.widgets.TreeColumn;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.plugin.AbstractUIPlugin;

import com.google.common.eventbus.Subscribe;
import com.raytheon.uf.viz.collaboration.comm.identity.event.IRosterChangeEvent;
import com.raytheon.uf.viz.collaboration.comm.provider.session.CollaborationConnection;
import com.raytheon.uf.viz.collaboration.core.ui.data.ActiveSessionsTreeData;
import com.raytheon.uf.viz.collaboration.core.ui.data.MyUserTreeData;
import com.raytheon.uf.viz.collaboration.core.ui.data.UserGroupTreeData;
import com.raytheon.uf.viz.collaboration.core.ui.tree.UsersTreeContentProvider;
import com.raytheon.uf.viz.collaboration.core.ui.tree.UsersTreeLabelProvider;

/**
 * TODO Add Description
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jun 26, 2012            mschenke     Initial creation
 * 
 * </pre>
 * 
 * @author mschenke
 * @version 1.0
 */

public class CollaborationGroupControl {

    private Text filterText;

    private TreeViewer usersTreeViewer;

    private IMenuManager mainMenu;

    private ICollaborationUIManager manager;

    private ActiveSessionsTreeData activeSessions;

    private MyUserTreeData myUserData;

    private List<UserGroupTreeData> groups = new ArrayList<UserGroupTreeData>();

    public CollaborationGroupControl(Composite compsite,
            ICollaborationUIManager manager) {
        this.manager = manager;
        initializeGroupControl(compsite);
        manager.getConnection().registerEventHandler(this);
    }

    /**
     * @param parent
     */
    private void initializeGroupControl(Composite parent) {
        parent.setLayout(new GridLayout());
        createFilterText(parent);
        createUserTree(parent);

        createMainMenu();
    }

    /**
     * @param parent
     */
    private void createUserTree(Composite parent) {
        Composite treeComp = new Composite(parent, SWT.NONE);
        treeComp.setLayout(new GridLayout(1, false));
        treeComp.setLayoutData(new GridData(SWT.FILL, SWT.FILL, true, true));
        Tree tree = new Tree(treeComp, SWT.VIRTUAL | SWT.MULTI | SWT.V_SCROLL
                | SWT.H_SCROLL | SWT.BORDER);
        tree.setLayoutData(new GridData(SWT.FILL, SWT.FILL, true, true));

        // any width would work
        new TreeColumn(tree, SWT.NONE).setWidth(200);

        MenuManager menuMgr = new MenuManager();
        menuMgr.setRemoveAllWhenShown(true);
        menuMgr.addMenuListener(new IMenuListener() {
            public void menuAboutToShow(IMenuManager mgr) {
                fillContextMenu(mgr);
            }
        });
        Menu menu = menuMgr.createContextMenu(tree);
        tree.setMenu(menu);

        usersTreeViewer = new TreeViewer(tree);
        usersTreeViewer.setContentProvider(new UsersTreeContentProvider());
        usersTreeViewer.setLabelProvider(new UsersTreeLabelProvider());
        populateTree();
    }

    private void populateTree() {
        if (myUserData == null) {
            myUserData = new MyUserTreeData(CollaborationConnection
                    .getConnection().getUser());
            activeSessions = new ActiveSessionsTreeData();
            usersTreeViewer.add(new TreePath(new Object[0]), new Object[] {
                    myUserData, activeSessions });
        }
        usersTreeViewer.remove(groups.toArray(new Object[groups.size()]));
        groups.clear();
        populateGroups();
    }

    /**
     * 
     */
    private void populateGroups() {
        Map<String, UserGroupTreeData> dataMap = new HashMap<String, UserGroupTreeData>();
        IRoster roster = CollaborationConnection.getConnection()
                .getRosterManager().getRoster();
        for (Object item : roster.getItems()) {
            if (item instanceof IRosterGroup) {
                IRosterGroup group = (IRosterGroup) item;
            } else if (item instanceof IRosterEntry) {
                IRosterEntry entry = (IRosterEntry) item;
            }
        }
    }

    private void fillContextMenu(IMenuManager mgr) {
        // TODO: Fill context menu based on selected tree items
    }

    /**
     * 
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

        filterText = new Text(comp, SWT.SINGLE | SWT.NONE);
        GridData data = new GridData(SWT.FILL, SWT.NONE, true, false);
        filterText.setLayoutData(data);
        filterText.addKeyListener(new KeyAdapter() {
            @Override
            public void keyReleased(KeyEvent e) {
                filter();
            }
        });

        // Load icons to be used for label button
        final Image inactiveImage = AbstractUIPlugin.imageDescriptorFromPlugin(
                PlatformUI.PLUGIN_ID, "$nl$/icons/full/dtool16/clear_co.gif")
                .createImage();
        final Image activeImage = AbstractUIPlugin.imageDescriptorFromPlugin(
                PlatformUI.PLUGIN_ID, "$nl$/icons/full/etool16/clear_co.gif")
                .createImage();

        final Label clearButton = new Label(comp, SWT.NONE);
        clearButton.setLayoutData(new GridData(SWT.BEGINNING, SWT.CENTER,
                false, false));
        clearButton.setImage(inactiveImage);
        clearButton.setBackground(parent.getDisplay().getSystemColor(
                SWT.COLOR_LIST_BACKGROUND));
        clearButton.setToolTipText("Clear text");
        // Add listener for enter/exit changing icon
        clearButton.addMouseTrackListener(new MouseTrackAdapter() {
            public void mouseEnter(MouseEvent e) {
                clearButton.setImage(activeImage);
            }

            public void mouseExit(MouseEvent e) {
                clearButton.setImage(inactiveImage);
            }
        });
        // Add mouse listener for clear pressed
        clearButton.addMouseListener(new MouseAdapter() {
            @Override
            public void mouseUp(MouseEvent e) {
                filterText.setText("");
                filter();
            }
        });
        // Add dispose listener
        clearButton.addDisposeListener(new DisposeListener() {
            public void widgetDisposed(DisposeEvent e) {
                inactiveImage.dispose();
                activeImage.dispose();
            }
        });
    }

    private void createMainMenu() {
        // TODO: Populate the main menu
    }

    /**
     * Get the {@link Tree} object that contains user information for the
     * control
     * 
     * @return
     */
    public Tree getUserTree() {
        return usersTreeViewer.getTree();
    }

    /**
     * Gets the contribution items for the group main menu
     * 
     * @return
     */
    public IContributionItem[] getMainMenuItems() {
        return mainMenu.getItems();
    }

    /**
     * Filters the group view based on the text
     * 
     * @param text
     */
    private void filter() {
        String currText = filterText.getText();
        // TODO: Filter the visible tree objects
    }

    @Subscribe
    public void handleRosterChangeEvent(IRosterChangeEvent rosterChangeEvent) {
        // TODO: Handle roster events
        switch (rosterChangeEvent.getType()) {
        case ADD:
            break;
        case MODIFY:
            break;
        case DELETE:
            break;
        case PRESENCE:
            break;
        }
    }

}
