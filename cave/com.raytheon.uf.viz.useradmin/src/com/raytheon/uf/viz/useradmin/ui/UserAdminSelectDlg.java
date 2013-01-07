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
package com.raytheon.uf.viz.useradmin.ui;

import java.util.ArrayList;

import org.eclipse.swt.SWT;
import org.eclipse.swt.events.MouseAdapter;
import org.eclipse.swt.events.MouseEvent;
import org.eclipse.swt.events.MouseListener;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Combo;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.List;
import org.eclipse.swt.widgets.Menu;
import org.eclipse.swt.widgets.MenuItem;
import org.eclipse.swt.widgets.MessageBox;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.TabFolder;
import org.eclipse.swt.widgets.TabItem;

import com.raytheon.uf.common.plugin.nwsauth.xml.PermissionXML;
import com.raytheon.uf.common.plugin.nwsauth.xml.RoleXML;
import com.raytheon.uf.viz.plugin.nwsauth.FileManager;
import com.raytheon.viz.ui.dialogs.CaveSWTDialog;

/**
 * Main User Administration Dialog.
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * May 23, 2012            mpduff     Initial creation.
 * Nov 26, 2012   1347     mpduff     Make resizable.
 *
 * </pre>
 *
 * @author mpduff
 * @version 1.0
 */

public class UserAdminSelectDlg extends CaveSWTDialog {
    private Combo appCombo;

    private TabFolder tabFolder;

    private TabItem userTab;

    private TabItem roleTab;

    private List userList;

    private List roleList;

    private List userPermList;

    private List rolePermList;

    private Button editUserBtn;

    private Button deleteUserBtn;

    private Button editRoleBtn;

    private Button deleteRoleBtn;

    private String selectedApplication;

    private boolean dirty = false;

    /**
     * Constructor.
     *
     * @param parent
     *            The parent shell
     */
    public UserAdminSelectDlg(Shell parent) {
        super(parent, SWT.DIALOG_TRIM | SWT.RESIZE, CAVE.PERSPECTIVE_INDEPENDENT);
        setText("User Admin");
    }

    @Override
    protected void initializeComponents(Shell shell) {
        FileManager man = FileManager.getInstance();
        GridData gd = new GridData(SWT.CENTER, SWT.DEFAULT, true, false);
        GridLayout gl = new GridLayout(1, false);
        shell.setLayout(gl);
        shell.setLayoutData(gd);

        gd = new GridData(SWT.CENTER, SWT.DEFAULT, true, false);
        gl = new GridLayout(2, false);
        Composite c = new Composite(shell, SWT.NONE);
        c.setLayout(gl);
        c.setLayoutData(gd);

        Label label = new Label(c, SWT.NONE);
        label.setText("Component: ");

        gd = new GridData(SWT.CENTER, SWT.DEFAULT, true, false);
        gl = new GridLayout(1, false);
        appCombo = new Combo(c, SWT.DROP_DOWN | SWT.READ_ONLY);
        appCombo.setLayoutData(gd);
        appCombo.setItems(man.getApplications());
        appCombo.select(0);
        appCombo.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                selectedApplication = appCombo.getItem(appCombo.getSelectionIndex());
                populateLists();
            }
        });
        selectedApplication = appCombo.getItem(appCombo.getSelectionIndex());

        gd = new GridData(SWT.FILL, SWT.FILL, true, true);
        gl = new GridLayout(1, false);

        Composite tabComp = new Composite(shell, SWT.NONE);
        tabComp.setLayout(gl);
        tabComp.setLayoutData(gd);

        gd = new GridData(SWT.FILL, SWT.FILL, true, true);
        gl = new GridLayout(1, false);

        tabFolder = new TabFolder(tabComp, SWT.BORDER);
        tabFolder.setLayoutData(gd);

        createTabs(tabFolder);
        tabFolder.pack();

        gd = new GridData(SWT.CENTER, SWT.DEFAULT, true, false);
        gl = new GridLayout(2, false);

        Composite buttonComp = new Composite(shell, SWT.NONE);
        buttonComp.setLayout(gl);
        buttonComp.setLayoutData(gd);

        int buttonWidth = 100;
        GridData btnData = new GridData(buttonWidth, SWT.DEFAULT);

        Button okBtn = new Button(buttonComp, SWT.PUSH);
        okBtn.setText("Save");
        okBtn.setLayoutData(btnData);
        okBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                handleOK();
            }
        });

        btnData = new GridData(buttonWidth, SWT.DEFAULT);

        Button closeBtn = new Button(buttonComp, SWT.PUSH);
        closeBtn.setText("Close");
        closeBtn.setLayoutData(btnData);
        closeBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                if (dirty) {
                    MessageBox messageDialog = new MessageBox(getShell(), SWT.ICON_WARNING | SWT.YES | SWT.NO);
                    messageDialog.setText("Unsaved Changes");
                    messageDialog.setMessage("Unsaved changes are present.\n" +
                    		"Are you sure you want to close without saving?");
                    int answer = messageDialog.open();
                    if (answer == SWT.YES) {
                        FileManager.getInstance().reloadXML();
                        close();
                        return;
                    }

                    return;
                }
                close();
            }
        });

        populateLists();
    }

    /* (non-Javadoc)
     * @see com.raytheon.viz.ui.dialogs.CaveSWTDialog#preOpened()
     */
    @Override
    protected void preOpened() {
        shell.setMinimumSize(550, 405);
        super.preOpened();
    }

    private void createTabs(TabFolder tabFolder) {
        String app = appCombo.getItem(appCombo.getSelectionIndex());
        userTab = new TabItem(tabFolder, SWT.NONE);
        userTab.setText(" " + app + " Users ");

        GridData gd = new GridData(SWT.CENTER, SWT.DEFAULT, true, false);
        GridLayout gl = new GridLayout(3, false);
        gl.verticalSpacing = 2;
        gl.horizontalSpacing = 2;
        gl.marginWidth = 2;

        Composite userComp = new Composite(tabFolder, SWT.NONE);
        userComp.setLayout(gl);
        userComp.setLayoutData(gd);
        userTab.setControl(userComp);

        gd = new GridData(SWT.FILL, SWT.FILL, true, true);
        gl = new GridLayout(1, false);
        Composite listComp = new Composite(userComp, SWT.NONE);
        listComp.setLayout(gl);
        listComp.setLayoutData(gd);

        Label label = new Label(listComp, SWT.NONE);
        label.setText("Selected Users:");

        GridData listData = new GridData(SWT.FILL, SWT.FILL, true, true);
        listData.widthHint = 150;
        listData.heightHint = 175;
        userList = new List(listComp, SWT.BORDER | SWT.MULTI | SWT.V_SCROLL | SWT.H_SCROLL);
        userList.setLayoutData(listData);
        userList.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                if (userList.getSelectionCount() == 0) {
                    deleteUserBtn.setEnabled(false);
                    editUserBtn.setEnabled(false);
                } else {
                    deleteUserBtn.setEnabled(true);
                    editUserBtn.setEnabled(true);
                }

                populateUserRoleList();
            }
        });

        gd = new GridData(SWT.DEFAULT, SWT.CENTER, false, false);
        gl = new GridLayout(1, false);
        Composite userBtnComp = new Composite(userComp, SWT.NONE);
        userBtnComp.setLayout(gl);
        userBtnComp.setLayoutData(gd);

        int buttonWidth = 75;
        GridData btnData = new GridData(buttonWidth, SWT.DEFAULT);
        Button newBtn = new Button(userBtnComp, SWT.PUSH);
        newBtn.setText("New...");
        newBtn.setLayoutData(btnData);
        newBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                handleNewUser();
            }
        });

        btnData = new GridData(buttonWidth, SWT.DEFAULT);
        editUserBtn = new Button(userBtnComp, SWT.PUSH);
        editUserBtn.setText("Edit...");
        editUserBtn.setLayoutData(btnData);
        editUserBtn.setEnabled(false);
        editUserBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                handleEditUser();
                populateUserRoleList();
            }
        });

        btnData = new GridData(buttonWidth, SWT.DEFAULT);
        deleteUserBtn = new Button(userBtnComp, SWT.PUSH);
        deleteUserBtn.setText("Delete");
        deleteUserBtn.setLayoutData(btnData);
        deleteUserBtn.setEnabled(false);
        deleteUserBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                handleDeleteUser();
            }
        });

        gd = new GridData(SWT.FILL, SWT.FILL, true, true);
        gl = new GridLayout(1, false);
        Composite userRoleComp = new Composite(userComp, SWT.NONE);
        userRoleComp.setLayout(gl);
        userRoleComp.setLayoutData(gd);

        listData = new GridData(SWT.FILL, SWT.FILL, true, true);
        listData.widthHint = 200;
        listData.heightHint = 175;
        Label l = new Label(userRoleComp, SWT.NONE);
        l.setText("Defined Roles/Permissions:");

        userPermList = new List(userRoleComp, SWT.BORDER | SWT.MULTI | SWT.V_SCROLL | SWT.H_SCROLL);
        userPermList.setLayoutData(listData);
        userPermList.addMouseListener(new MouseAdapter() {
            @Override
            public void mouseDown(MouseEvent arg0) {
                if (arg0.button == 3) {
                    Menu menu = new Menu(shell, SWT.POP_UP);
                    MenuItem item1 = new MenuItem(menu, SWT.PUSH);
                    item1.setText("Description...");
                    item1.addSelectionListener(new SelectionAdapter() {
                        @Override
                        public void widgetSelected(SelectionEvent arg0) {
                            String selection = userPermList.getItem(userPermList.getSelectionIndex());
                            StringBuilder messageText = new StringBuilder();
                            boolean roleFlag = false;
                            FileManager man = FileManager.getInstance();
                            for (RoleXML role : man.getRoleData(selectedApplication).getRoleList()) {
                                if (selection.equals(role.getRoleId())) {
                                    messageText.append("Role: " + selection);
                                    messageText.append("\n\nDescription: " + role.getRoleDescription().trim());
                                    if (role.getPermissionList().size() > 0) {
                                        messageText.append("\n\nPermissions: ");
                                        for (String perm : role.getPermissionList()) {
                                            messageText.append("\n  " + perm);
                                        }
                                    }
                                    roleFlag = true;
                                    break;
                                }
                            }

                            if (roleFlag == false) {
                                for (PermissionXML perm : man.getRoleData(selectedApplication).getPermissionList()) {
                                    if (perm.getId().equals(selection)) {
                                        messageText.append("Permission: " + selection);
                                        messageText.append("\nDescription: " + perm.getDescription());
                                        break;
                                    }
                                }
                            }
                            if (messageText.length() == 0) {
                                messageText.append("No Description");
                            }
                            MessageBox messageDialog = new MessageBox(shell, SWT.ICON_INFORMATION);
                            if (roleFlag) {
                                messageDialog.setText("Role Description");
                            } else {
                                messageDialog.setText("Permission Description");
                            }
                            messageDialog.setMessage(messageText.toString());
                            messageDialog.open();
                        }
                    });

                    shell.setMenu(menu);
                    menu.setVisible(true);
                }
            }
        });

        /************************************************************************************************/
        /************************************************************************************************/
        /************************************************************************************************/
        /************************************************************************************************/
        /************************************************************************************************/
        /************************************************************************************************/
        /************************************************************************************************/
        /************************************************************************************************/
        /************************************************************************************************/
        /************************************************************************************************/
        /************************************************************************************************/
        /************************************************************************************************/
        /************************************************************************************************/
        /************************************************************************************************/
        /************************************************************************************************/
        // Roles tab
        roleTab = new TabItem(tabFolder, SWT.NONE);
        roleTab.setText(" " + app + " Roles ");

        gd = new GridData(SWT.CENTER, SWT.DEFAULT, true, false);
        gl = new GridLayout(3, false);
        gl.verticalSpacing = 2;
        gl.horizontalSpacing = 2;
        gl.marginWidth = 2;

        Composite roleComp = new Composite(tabFolder, SWT.NONE);
        roleComp.setLayout(gl);
        roleComp.setLayoutData(gd);
        roleTab.setControl(roleComp);

        gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        gl = new GridLayout(1, false);
        Composite listComp2 = new Composite(roleComp, SWT.NONE);
        listComp2.setLayout(gl);
        listComp2.setLayoutData(gd);

        Label roleLabel = new Label(listComp2, SWT.NONE);
        roleLabel.setText("Defined Roles:");

        listData = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        listData.widthHint = 150;
        listData.heightHint = 175;
        roleList = new List(listComp2, SWT.BORDER | SWT.MULTI | SWT.V_SCROLL | SWT.H_SCROLL);
        roleList.setLayoutData(listData);
        roleList.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                if (roleList.getSelectionCount() == 0) {
                    deleteRoleBtn.setEnabled(false);
                    editRoleBtn.setEnabled(false);
                } else {
                    deleteRoleBtn.setEnabled(true);
                    editRoleBtn.setEnabled(true);
                }

                populatePermissionList();
            }
        });
        roleList.addMouseListener(new MouseListener() {

            @Override
            public void mouseUp(MouseEvent arg0) {
                // Not Implemented
            }

            @Override
            public void mouseDown(MouseEvent arg0) {
                if (arg0.button == 3) {
                    Menu menu = new Menu(shell, SWT.POP_UP);
                    MenuItem item1 = new MenuItem(menu, SWT.PUSH);
                    item1.setText("Description...");
                    item1.addSelectionListener(new SelectionAdapter() {
                        @Override
                        public void widgetSelected(SelectionEvent arg0) {
                            String selection = roleList.getItem(roleList.getSelectionIndex());
                            String messageText = null;
                            FileManager man = FileManager.getInstance();
                            String app = appCombo.getItem(appCombo.getSelectionIndex());
                            for (RoleXML role : man.getRoleData(app).getRoleList()) {
                                if (selection.equals(role.getRoleId())) {
                                    messageText = role.getRoleDescription();
                                    break;
                                }
                            }
                            MessageBox messageDialog = new MessageBox(shell, SWT.ICON_INFORMATION);
                            messageDialog.setText("Role Description");
                            messageDialog.setMessage(messageText.toString());
                            messageDialog.open();
                        }
                    });

                    shell.setMenu(menu);
                    menu.setVisible(true);
                }
            }

            @Override
            public void mouseDoubleClick(MouseEvent arg0) {
                // Not implemented

            }
        });

        gd = new GridData(SWT.DEFAULT, SWT.CENTER, false, false);
        gl = new GridLayout(1, false);
        Composite roleBtnComp = new Composite(roleComp, SWT.NONE);
        roleBtnComp.setLayout(gl);
        roleBtnComp.setLayoutData(gd);

        btnData = new GridData(buttonWidth, SWT.DEFAULT);
        Button newRoleBtn = new Button(roleBtnComp, SWT.PUSH);
        newRoleBtn.setText("New...");
        newRoleBtn.setLayoutData(btnData);
        newRoleBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                handleNewRole();
            }
        });

        btnData = new GridData(buttonWidth, SWT.DEFAULT);
        editRoleBtn = new Button(roleBtnComp, SWT.PUSH);
        editRoleBtn.setText("Edit...");
        editRoleBtn.setLayoutData(btnData);
        editRoleBtn.setEnabled(false);
        editRoleBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                handleEditRole();
                populateUserRoleList();
            }
        });

        btnData = new GridData(buttonWidth, SWT.DEFAULT);
        deleteRoleBtn = new Button(roleBtnComp, SWT.PUSH);
        deleteRoleBtn.setText("Delete");
        deleteRoleBtn.setLayoutData(btnData);
        deleteRoleBtn.setEnabled(false);
        deleteRoleBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                handleDeleteRole();
            }
        });

        gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        gl = new GridLayout(1, false);
        Composite permComp = new Composite(roleComp, SWT.NONE);
        permComp.setLayout(gl);
        permComp.setLayoutData(gd);

        listData = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        listData.widthHint = 200;
        listData.heightHint = 175;
        Label l2 = new Label(permComp, SWT.NONE);
        l2.setText("Roles/Permissions:");

        rolePermList = new List(permComp, SWT.BORDER | SWT.MULTI | SWT.V_SCROLL | SWT.H_SCROLL);
        rolePermList.setLayoutData(listData);
    }

    private void populateLists() {
        FileManager man = FileManager.getInstance();
        String app = appCombo.getItem(appCombo.getSelectionIndex());

        userTab.setText(app + " Users");
        roleTab.setText(app + " Roles");
        int selection = 0;

        if (userList.getSelectionIndex() != -1) {
            selection = userList.getSelectionIndex();
        }

        userList.setItems(man.getRoleData(app).getUsers());
        if (selection < userList.getItemCount()) {
            userList.select(selection);
        }
        populateUserRoleList();

        if (roleList.getSelectionIndex() != -1) {
            selection = roleList.getSelectionIndex();
        } else {
            selection = 0;
        }

        roleList.setItems(man.getRoleData(app).getRoles());
        if (selection < roleList.getItemCount()) {
            roleList.select(selection);
        }
        populatePermissionList();
    }

    private void populateUserRoleList() {
        FileManager man = FileManager.getInstance();
        String app = appCombo.getItem(appCombo.getSelectionIndex());

        if (userList.getSelectionIndex() != -1) {
            String user = userList.getItem(userList.getSelectionIndex());
            String[] roles = man.getUserRoles(user, app);
            String[] perms = man.getUserPermissions(user, app);

            ArrayList<String> rp = new ArrayList<String>();
            for (String role : roles) {
                rp.add(role);
            }

            for (String perm : perms) {
                rp.add(perm);
            }

            userPermList.setItems(rp.toArray(new String[rp.size()]));
        }
    }

    private void populatePermissionList() {
        FileManager man = FileManager.getInstance();
        rolePermList.removeAll();
        String app = appCombo.getItem(appCombo.getSelectionIndex());
        if (roleList.getSelectionIndex() != -1) {
            String roleId = roleList.getItem(roleList.getSelectionIndex());
            String[] perms = man.getRolePermissions(roleId, app);

            rolePermList.setItems(perms);
        }
    }

    private void handleNewUser() {
        NewDlg ad = new NewDlg(this.shell, "User", selectedApplication);
        ad.open();

        populateLists();
    }

    private void handleDeleteUser() {
        String user = userList.getItem(userList.getSelectionIndex());

        MessageBox messageDialog = new MessageBox(this.shell, SWT.YES | SWT.NO);
        messageDialog.setText("Title");
        messageDialog.setMessage("Are you sure you wish to delete user " + user);
        int response = messageDialog.open();

        if (response == SWT.YES) {
            FileManager man = FileManager.getInstance();
            String app = appCombo.getItem(appCombo.getSelectionIndex());
            man.deleteUser(user, app);
            dirty = true;
        }

        populateLists();
    }

    private void handleEditRole() {
        String role = roleList.getItem(roleList.getSelectionIndex());
        ManageUserDlg mud = new ManageUserDlg(this.shell, "Role", role, selectedApplication);
        boolean changes = (Boolean) mud.open();
        if (changes) {
            dirty = true;
        }
    }

    private void handleDeleteRole() {
        String role = roleList.getItem(roleList.getSelectionIndex());

        MessageBox messageDialog = new MessageBox(this.shell, SWT.YES | SWT.NO);
        messageDialog.setText("Title");
        messageDialog.setMessage("Are you sure you wish to delete role " + role);
        int response = messageDialog.open();

        if (response == SWT.YES) {
            FileManager man = FileManager.getInstance();
            String app = appCombo.getItem(appCombo.getSelectionIndex());
            man.deleteRole(role, app);
            dirty = true;
        }

        populateLists();
    }

    private void handleNewRole() {
        NewDlg ad;
        if (selectedApplication.equalsIgnoreCase("Localization")) {
            ad = new NewDlg(this.shell, "Permission", selectedApplication);
        } else {
            ad = new NewDlg(this.shell, "Role", selectedApplication);
        }

        ad.open();

        populateLists();
    }

    private void handleEditUser() {
        String user = userList.getItem(userList.getSelectionIndex());
        ManageUserDlg mud = new ManageUserDlg(this.shell, "User", user, selectedApplication);
        boolean changes = (Boolean) mud.open();
        if (changes) {
            dirty = true;
        }
    }

    private void handleOK() {
        FileManager manager = FileManager.getInstance();
        manager.save(selectedApplication);
        dirty = false;
    }
}
