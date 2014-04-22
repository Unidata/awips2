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
package com.raytheon.uf.viz.collaboration.ui;

import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import org.eclipse.jface.action.Action;
import org.eclipse.jface.action.IMenuListener;
import org.eclipse.jface.action.IMenuManager;
import org.eclipse.jface.action.MenuManager;
import org.eclipse.jface.action.Separator;
import org.eclipse.jface.layout.TableColumnLayout;
import org.eclipse.jface.viewers.ColumnWeightData;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.KeyAdapter;
import org.eclipse.swt.events.KeyEvent;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.layout.RowData;
import org.eclipse.swt.layout.RowLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Combo;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Menu;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Table;
import org.eclipse.swt.widgets.TableColumn;
import org.eclipse.swt.widgets.TableItem;
import org.eclipse.swt.widgets.Text;
import org.jivesoftware.smack.XMPPException;

import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.viz.collaboration.comm.provider.session.CollaborationConnection;
import com.raytheon.uf.viz.collaboration.comm.provider.user.UserId;
import com.raytheon.uf.viz.collaboration.comm.provider.user.UserSearch;
import com.raytheon.uf.viz.collaboration.ui.actions.AddToGroupAction;
import com.raytheon.uf.viz.collaboration.ui.actions.CreateSessionAction;
import com.raytheon.uf.viz.collaboration.ui.actions.InviteAction;
import com.raytheon.uf.viz.collaboration.ui.actions.PeerToPeerChatAction;
import com.raytheon.viz.ui.dialogs.CaveSWTDialog;

/**
 * Provides a dialog for searching for users within collaboration in viz.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jun 27, 2012            bsteffen     Initial creation
 * Dec  6, 2013 2561       bclement    removed ECF
 * 
 * </pre>
 * 
 * @author bsteffen
 * @version 1.0
 */

public class UserSearchDialog extends CaveSWTDialog implements IUserSelector {

    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(UserSearchDialog.class);

    private Text searchText;

    private Combo fieldCombo;

    private Table resultTable;

    public UserSearchDialog(Shell parentShell) {
        super(parentShell, SWT.RESIZE | SWT.DIALOG_TRIM);
        setText("User Search");
    }

    @Override
    protected void initializeComponents(Shell shell) {
        initializeSearchBar(shell);
        initializeResultsTable(shell);
        initializeButtonBar(shell);
    }

    private void initializeSearchBar(Shell shell) {
        Composite entryComp = new Composite(shell, SWT.NONE);
        entryComp
                .setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true, false));
        entryComp.setLayout(new GridLayout(3, false));
        searchText = new Text(entryComp, SWT.BORDER);
        GridData gridData = new GridData(200, SWT.DEFAULT);
        gridData.grabExcessHorizontalSpace = true;
        gridData.horizontalAlignment = SWT.FILL;
        searchText.setLayoutData(gridData);
        searchText.addKeyListener(new KeyAdapter() {

            @Override
            public void keyReleased(KeyEvent e) {
                if (e.keyCode == SWT.CR || e.keyCode == SWT.KEYPAD_CR) {
                    search();
                }
            }

        });

         UserSearch search = CollaborationConnection.getConnection()
                .createSearch();
        fieldCombo = new Combo(entryComp, SWT.DROP_DOWN | SWT.READ_ONLY);
        fieldCombo.add("All");
        try {
            for (String field : search.getUserPropertiesFields()) {
                fieldCombo.add(field);
            }
            fieldCombo.select(0);
        } catch (XMPPException e1) {
            statusHandler
                    .handle(Priority.PROBLEM, e1.getLocalizedMessage(), e1);
        }
        Button searchButton = new Button(entryComp, SWT.PUSH);
        searchButton.setText("Search");
        searchButton.addSelectionListener(new SelectionAdapter() {

            @Override
            public void widgetSelected(SelectionEvent e) {
                search();
            }

        });
    }

    private void initializeResultsTable(Shell shell) {
        Composite tableComp = new Composite(shell, SWT.NONE);
        GridData gridData = new GridData(SWT.DEFAULT, 200);
        gridData.horizontalAlignment = SWT.FILL;
        gridData.verticalAlignment = SWT.FILL;
        gridData.grabExcessHorizontalSpace = true;
        gridData.grabExcessVerticalSpace = true;
        tableComp.setLayoutData(gridData);
        resultTable = new Table(tableComp, SWT.BORDER | SWT.V_SCROLL
                | SWT.H_SCROLL | SWT.MULTI | SWT.FULL_SELECTION);
        resultTable.setHeaderVisible(true);
        resultTable.setLinesVisible(true);
        TableColumn nameColumn = new TableColumn(resultTable, SWT.LEFT);
        nameColumn.setText("Full Name");
        TableColumn idColumn = new TableColumn(resultTable, SWT.LEFT);
        idColumn.setText("User ID");

        TableColumnLayout tcl = new TableColumnLayout();
        tableComp.setLayout(tcl);
        tcl.setColumnData(nameColumn, new ColumnWeightData(40));
        tcl.setColumnData(idColumn, new ColumnWeightData(60));

        MenuManager menuMgr = new MenuManager();
        menuMgr.setRemoveAllWhenShown(true);
        menuMgr.addMenuListener(new IMenuListener() {

            @Override
            public void menuAboutToShow(IMenuManager manager) {
                fillContextMenu(manager);
            }

        });
        Menu menu = menuMgr.createContextMenu(resultTable);
        resultTable.setMenu(menu);
    }

    private void initializeButtonBar(Shell shell) {
        Composite buttonComp = new Composite(shell, SWT.NONE);
        buttonComp.setLayoutData(new GridData(SWT.RIGHT, SWT.NONE, false,
                false, 1, 1));
        RowLayout layout = new RowLayout(SWT.HORIZONTAL);
        layout.pack = false;
        buttonComp.setLayout(layout);

        Button closeButton = new Button(buttonComp, SWT.PUSH);
        closeButton.setText("Close");
        closeButton.addSelectionListener(new SelectionAdapter() {

            @Override
            public void widgetSelected(SelectionEvent e) {
                close();
            }

        });
        closeButton.setLayoutData(new RowData(90, SWT.DEFAULT));
    }

    private void fillContextMenu(IMenuManager manager) {
        TableItem[] selection = resultTable.getSelection();
        if (selection == null || selection.length == 0) {
            return;
        }
        UserId[] users = new UserId[selection.length];
        for (int i = 0; i < users.length; i += 1) {
            users[i] = (UserId) selection[i].getData();
        }
        if (users.length == 1) {
            Action p2pAction = new PeerToPeerChatAction(users[0]);
            if (p2pAction.isEnabled()) {
                manager.add(p2pAction);
            }
        }
        Action inviteAction = new InviteAction(users);
        if (inviteAction.isEnabled()) {
            manager.add(inviteAction);
        }
        if (manager.getItems().length > 0) {
            manager.add(new Separator());
        }

        manager.add(new CreateSessionAction(this));
        manager.add(new AddToGroupAction(users));
    }

    private void search() {
        List<UserId> results = new ArrayList<UserId>();
        List<String> keys = new ArrayList<String>();
        if (fieldCombo.getText().equals("All")) {
            for (String string : fieldCombo.getItems()) {
                if (!string.equals("All")) {
                    keys.add(string);
                }
            }
        } else {
            keys.add(fieldCombo.getText());
        }
        for (String key : keys) {
            UserSearch search = CollaborationConnection.getConnection()
                    .createSearch();
            try {
                List<UserId> users = search.byCriteria(key,
                        searchText.getText());
                results.addAll(users);
            } catch (XMPPException e) {
                statusHandler.handle(Priority.PROBLEM, e.getLocalizedMessage(),
                        e);
            }
        }
        Set<String> uniqueIds = new HashSet<String>();
        resultTable.removeAll();
        if (results.size() > 0) {
            for (UserId user : results) {
                String id = user.getNormalizedId();
                if (!uniqueIds.contains(id)) {
                    TableItem ti = new TableItem(resultTable, SWT.NONE);
                    String fullName = user.getAlias();
                    if (fullName == null || fullName.isEmpty()) {
                        fullName = user.getName();
                    }
                    ti.setText(0, fullName);
                    ti.setText(1, id);
                    ti.setData(user);
                    uniqueIds.add(id);
                }
            }
            resultTable.setEnabled(true);
        } else {
            TableItem ti = new TableItem(resultTable, SWT.NONE);
            ti.setText("No users found.");
            resultTable.setEnabled(false);
        }
    }

    @Override
    public UserId[] getSelectedUsers() {
        Set<UserId> selectedUsers = new HashSet<UserId>();
        TableItem[] selection = resultTable.getSelection();

        if (selection != null && selection.length > 0) {
            UserId[] users = new UserId[selection.length];
            for (int i = 0; i < users.length; i += 1) {
                UserId user = (UserId) selection[i].getData();
                selectedUsers.add(user);
            }
        }
        return selectedUsers.toArray(new UserId[selectedUsers.size()]);
    }

}
