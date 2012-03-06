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

import java.util.Map;

import org.eclipse.jface.viewers.TreeViewer;
import org.eclipse.swt.SWT;
import org.eclipse.swt.custom.SashForm;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.events.SelectionListener;
import org.eclipse.swt.graphics.Color;
import org.eclipse.swt.layout.FillLayout;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Menu;
import org.eclipse.swt.widgets.MenuItem;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.TabFolder;
import org.eclipse.swt.widgets.TabItem;

import com.raytheon.uf.viz.collaboration.data.CollaborationUser;
import com.raytheon.uf.viz.collaboration.ui.session.CollaborationSessionComp;
import com.raytheon.uf.viz.collaboration.ui.session.SessionComp;
import com.raytheon.viz.ui.dialogs.CaveSWTDialogBase;

/**
 * TODO Add Description
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Feb 16, 2012            rferrel     Initial creation
 * 
 * </pre>
 * 
 * @author rferrel
 * @version 1.0
 */
public class CollaborationUsersDlg extends CaveSWTDialogBase {

    private TreeViewer usersTreeViewer;

    private UsersTree usersTree;

    private TabFolder chatTabComp;

    Map<String, String[]> groupMap;

    protected CollaborationUsersDlg(Shell parentShell) {
        super(parentShell, SWT.DIALOG_TRIM | SWT.RESIZE,
                CAVE.PERSPECTIVE_INDEPENDENT | CAVE.MODE_INDEPENDENT
                        | CAVE.INDEPENDENT_SHELL);
        setText("Collaboration Contacts");

        usersTree = new UsersTree("kickstart");
    }

    @Override
    protected void initializeComponents(Shell shell) {
        shell.setLayout(new GridLayout(1, false));
        createMenus(shell);
        createButtonBar(shell);
        createMainArea(shell);
    }

    private void createMenus(Shell parent) {
        Menu bar = new Menu(parent, SWT.BAR);
        parent.setMenuBar(bar);
        MenuItem item = new MenuItem(bar, SWT.CASCADE);
        item.setText("&File");
    }

    private void createButtonBar(Shell parent) {
        Composite bar = new Composite(parent, SWT.NONE);
        bar.setLayout(new GridLayout(3, false));
        Button button = new Button(bar, SWT.DEFAULT);
        button.setText("Invite");

        button = new Button(bar, SWT.DEFAULT);
        button.setText("Collobration Chat");
        button.addSelectionListener(new SelectionListener() {

            @Override
            public void widgetSelected(SelectionEvent e) {
                createCollaborationChat();
            }

            @Override
            public void widgetDefaultSelected(SelectionEvent e) {
                // TODO Auto-generated method stub

            }
        });

        button = new Button(bar, SWT.DEFAULT);
        button.setText("Private Chat");
        button.addSelectionListener(new SelectionListener() {

            @Override
            public void widgetSelected(SelectionEvent e) {
                createPrivateChat();
            }

            @Override
            public void widgetDefaultSelected(SelectionEvent e) {
                // TODO Auto-generated method stub

            }
        });

    }

    private void createCollaborationChat() {
        TabItem item = new TabItem(chatTabComp, SWT.DEFAULT);

        CollaborationSessionComp child = new CollaborationSessionComp(
                chatTabComp);
        item.setControl(child);
        child.setRoom("Collaboration Room");

        item.setText(child.getRoomLabel());
    }

    private void createPrivateChat() {
        TabItem item = new TabItem(chatTabComp, SWT.DEFAULT);
        SessionComp child = new SessionComp(chatTabComp);
        item.setControl(child);
        child.setRoom("Private room");
        item.setText("Private Room");
    }

    private void createMainArea(Shell parent) {
        Composite body = new Composite(parent, SWT.FILL);
        Color red = getParent().getDisplay().getSystemColor(SWT.COLOR_RED);
        body.setBackground(red);
        body.setLayout(new GridLayout());
        body.setLayoutData(new GridData(SWT.FILL, SWT.FILL, true, true));
        SashForm mainForm = new SashForm(body, SWT.HORIZONTAL);
        mainForm.setLayout(new FillLayout());
        mainForm.setLayoutData(new GridData(SWT.FILL, SWT.FILL, true, true));
        Color blue = getParent().getDisplay().getSystemColor(SWT.COLOR_BLUE);
        mainForm.setBackground(blue);
        mainForm.setSashWidth(10);

        createUsersTree(mainForm);
        createChatTabs(mainForm);
        mainForm.setWeights(new int[] { 20, 80 });
    }

    private void createUsersTree(SashForm form) {
        Composite child = new Composite(form, SWT.NONE);
        child.setLayout(new GridLayout(1, false));
        Label label = new Label(child, SWT.NONE);
        label.setText("Groups");
        // usersTreeViewer = new TreeViewer(child, SWT.MULTI | SWT.BORDER
        // | SWT.FILL);
        usersTreeViewer = new TreeViewer(child);
        usersTreeViewer.getTree().setToolTipText("User groups");
        usersTreeViewer.getTree().setLayoutData(
                new GridData(SWT.FILL, SWT.FILL, true, true));
        usersTreeViewer.setContentProvider(new UsersTreeContentProvider(
        /* usersTree */));
        usersTreeViewer.setLabelProvider(new UsersTreeLabelProvider());
        usersTreeViewer.setSorter(new UsersTreeViewerSorter());
    }

    private void createChatTabs(SashForm form) {
        Composite tabComp = new Composite(form, SWT.NONE);
        tabComp.setLayout(new GridLayout(1, false));

        chatTabComp = new TabFolder(tabComp, SWT.NONE);
        chatTabComp.setLayoutData(new GridData(SWT.FILL, SWT.FILL, true, true));
    }

    private void populateUsers() {
        // TODO get from localization
        for (String g : new String[] { "Mybuddy1", "buddy1" }) {
            UsersTree root = usersTree.addChild(g);
            root.setText(g);
            for (String u : new String[] { "OAX_user1", "DSM_user3",
                    "LBF_user2" }) {
                CollaborationUser item = new CollaborationUser(u);
                root.addChild(item);
            }
        }

        // TODO get from server
        for (String g : new String[] { "OAX", "DSM", "LBF", "FSD" }) {
            UsersTree root = usersTree.addChild(g);
            root.setText(g);
            for (String u : new String[] { g + "_user2", g + "_user3",
                    g + "_user1" }) {
                CollaborationUser item = new CollaborationUser(u);
                root.addChild(item);
            }
        }
        usersTreeViewer.setInput(usersTree);
    }

    @Override
    protected void preOpened() {
        super.preOpened();
        populateUsers();
    }

    @Override
    protected void disposed() {
        // TODO Auto-generated method stub
        super.disposed();
        System.err.println("Contact being disposed.");
    }
}
