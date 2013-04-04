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
import org.eclipse.swt.custom.StackLayout;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Combo;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Shell;

import com.raytheon.uf.common.plugin.nwsauth.xml.NwsRoleData;
import com.raytheon.uf.common.plugin.nwsauth.xml.RoleXML;
import com.raytheon.uf.common.plugin.nwsauth.xml.UserXML;
import com.raytheon.uf.viz.plugin.nwsauth.NwsRoleDataManager;
import com.raytheon.viz.ui.dialogs.CaveSWTDialog;
import com.raytheon.viz.ui.widgets.duallist.DualList;
import com.raytheon.viz.ui.widgets.duallist.DualListConfig;
import com.raytheon.viz.ui.widgets.duallist.IUpdate;

/**
 * Edit user di
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * May 23, 2012            mpduff     Initial creation
 * Aug 08, 2012    863     jpiatt     Added new interface method.
 * 
 * </pre>
 * 
 * @author mpduff
 * @version 1.0
 */

public class ManageUserDlg extends CaveSWTDialog implements IUpdate {
   
    /** The Stack Layout. */
    private final StackLayout stackLayout = new StackLayout();

    /** Type of permissions */
    private final String type;

    /** Selection */
    private final String selection;

    /** Edit combo box */
    private Combo editCbo;

    /** All role dual list */
    private DualList roleDualList;

    /** Permissions dual list */
    private DualList permDualList;
    
    /** User role dual list */
    private DualList userRoleDualList;
    
    /** Stack composite */
    private Composite stackComp;

    /** The application currently selected.*/
    private final String application;

    /**
     * Constructor.
     * 
     * @param parent parent shell
     * @param type type of data being edited
     * @param selection selection being passed in
     * @param application application working on
     */
    public ManageUserDlg(Shell parent, String type, String selection, String application) {
        super(parent, SWT.DIALOG_TRIM);
        this.selection = selection;
        this.type = type;
        this.application = application;
        setText("Edit " + type + " - " + selection);
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.ui.dialogs.CaveSWTDialogBase#initializeComponents(org
     * .eclipse.swt.widgets.Shell)
     */
    @Override
    protected void initializeComponents(Shell shell) {
        GridData gd = new GridData(SWT.CENTER, SWT.DEFAULT, true, false);
        GridLayout gl = new GridLayout(1, false);
        gl.verticalSpacing = 2;
        gl.horizontalSpacing = 2;
        gl.marginWidth = 2;
        shell.setLayout(gl);
        shell.setLayoutData(gd);

        gd = new GridData(SWT.CENTER, SWT.DEFAULT, true, false);
        gl = new GridLayout(2, false);
        gl.verticalSpacing = 2;
        gl.horizontalSpacing = 2;
        gl.marginWidth = 2;

        Composite labelComp = new Composite(shell, SWT.NONE);
        labelComp.setLayout(gl);
        labelComp.setLayoutData(gd);

        Label editLbl = new Label(labelComp, SWT.NONE);
        editLbl.setText("Edit: ");

        String[] entries;
        if (type.equalsIgnoreCase("Role")) {
            entries = new String[] { "Assigned Roles", "Assigned Permissions", "Assign Role to Users" };
        } else {
            entries = new String[] { "Assigned Roles", "Assigned Permissions" };
        }
        gd = new GridData(SWT.CENTER, SWT.DEFAULT, true, false);
        editCbo = new Combo(labelComp, SWT.DROP_DOWN | SWT.READ_ONLY);
        editCbo.setLayoutData(gd);
     
        editCbo.setItems(entries);
        editCbo.select(0);
        editCbo.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                if (editCbo.getItem(editCbo.getSelectionIndex()).equals("Assigned Roles")) {
                    stackLayout.topControl = roleDualList;
                } else if (editCbo.getItem(editCbo.getSelectionIndex()).equals("Assigned Permissions")) {
                    stackLayout.topControl = permDualList;
                } else if (editCbo.getItem(editCbo.getSelectionIndex()).equals("Assign Role to Users")) {
                    stackLayout.topControl = userRoleDualList;
                }
                
                stackComp.layout();
            }
        });

        stackComp = new Composite(shell, SWT.NONE);
        stackComp.setLayout(stackLayout);

        NwsRoleDataManager manager = NwsRoleDataManager.getInstance();
        ArrayList<String> selectedList = new ArrayList<String>();
        ArrayList<String> fullList = new ArrayList<String>();
        String availableLabel = "Available Roles:";
        String selectedLabel = "Selected Roles:";

        // build roles widgets
        String[] userRoles = manager.getRoleData(application).getUserRoles(this.selection);
        for (String role : userRoles) {
            selectedList.add(role);
        }

        String[] roles = manager.getRoleData(application).getRoles();
        for (String role : roles) {
            fullList.add(role);
        }
        
        DualListMenuData menuData = new DualListMenuData();
        menuData.setApplication(this.application);
        menuData.setMenuText("Details...");
        menuData.setShowMenu(true);

        DualListConfig roleConfig = new DualListConfig();
        roleConfig.setAvailableListLabel(availableLabel);
        roleConfig.setSelectedListLabel(selectedLabel);
        roleConfig.setListHeight(90);
        roleConfig.setListWidth(175);
        roleConfig.setSelectedList(selectedList);
        roleConfig.setFullList(fullList);
        roleConfig.setMenuData(menuData);

        roleDualList = new DualList(stackComp, SWT.NONE, roleConfig, this);

        // Build permissions widgets
        ArrayList<String> selectedPermList = new ArrayList<String>();
        ArrayList<String> fullPermList = new ArrayList<String>();
        String[] userPerms = manager.getRoleData(application).getUserPermissions(this.selection);
        for (String perm : userPerms) {
            selectedPermList.add(perm);
        }
        String[] perms = manager.getRoleData(application).getPermissions();
        for (String perm : perms) {
            fullPermList.add(perm);
        }
        fullList.remove(selection);
        availableLabel = "Available Permissions:";
        selectedLabel = "Selected Permissions:";

        DualListMenuData menuData2 = new DualListMenuData();
        menuData2.setApplication(this.application);
        menuData2.setMenuText("Details...");
        menuData2.setShowMenu(true);

        DualListConfig permConfig = new DualListConfig();
        permConfig.setAvailableListLabel(availableLabel);
        permConfig.setSelectedListLabel(selectedLabel);
        permConfig.setListHeight(90);
        permConfig.setListWidth(175);
        permConfig.setSelectedList(selectedPermList);
        permConfig.setFullList(fullPermList);
        permConfig.setMenuData(menuData2);

        permDualList = new DualList(stackComp, SWT.NONE, permConfig, this);

        // Build Roles to User Widgets
        ArrayList<String> fullUserList = new ArrayList<String>();
        String[] users = manager.getRoleData(application).getUsers();
        for (String user : users) {
            fullUserList.add(user);
        }
        fullUserList.remove(selection);
        availableLabel = "Available Users:";
        selectedLabel = "Assigned Users:";

        DualListConfig userRoleConfig = new DualListConfig();
        userRoleConfig.setAvailableListLabel(availableLabel);
        userRoleConfig.setSelectedListLabel(selectedLabel);
        userRoleConfig.setListHeight(90);
        userRoleConfig.setListWidth(175);
        userRoleConfig.setFullList(fullUserList);
        
        userRoleDualList = new DualList(stackComp, SWT.NONE, userRoleConfig, this);
        
        int buttonWidth = 75;
        GridData btnData = new GridData(buttonWidth, SWT.DEFAULT);
        gd = new GridData(SWT.CENTER, SWT.DEFAULT, false, false);
        gl = new GridLayout(2, false);

        Composite bottomComp = new Composite(shell, SWT.NONE);
        bottomComp.setLayout(gl);
        bottomComp.setLayoutData(gd);

        btnData = new GridData(buttonWidth, SWT.DEFAULT);
        Button okBtn = new Button(bottomComp, SWT.PUSH);
        okBtn.setText("OK");
        okBtn.setLayoutData(btnData);
        okBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                handleOk();
            }
        });

        btnData = new GridData(buttonWidth, SWT.DEFAULT);
        Button cancelBtn = new Button(bottomComp, SWT.PUSH);
        cancelBtn.setText("Cancel");
        cancelBtn.setLayoutData(btnData);
        cancelBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                setReturnValue(false);
                close();
            }
        });

        stackLayout.topControl = roleDualList;
    }

    /**
     * Handle the OK button click.
     */
    private void handleOk() {
        String[] selectedUsers = userRoleDualList.getSelectedListItems();
        String[] permissions = permDualList.getSelectedListItems();
        String[] roles = roleDualList.getSelectedListItems();
        
        NwsRoleDataManager man = NwsRoleDataManager.getInstance();
        NwsRoleData roleData = man.getRoleData(application);
        
        if (type.equalsIgnoreCase("User")) {
            ArrayList<UserXML> userList = (ArrayList<UserXML>) roleData.getUserList();
            for (UserXML user: userList) {
                if (user.getUserId().equals(selection)) {
                    // Update permissions
                    ArrayList<String> permissionList = new ArrayList<String>();
                    for (String perm: permissions) {
                        permissionList.add(perm);
                    }
                    user.setPermissionList(permissionList);
                    
                    // Update roles
                    ArrayList<String> roleList = new ArrayList<String>();
                    for (String role: roles) {
                        roleList.add(role);
                    }
                    user.setRoleList(roleList);
                    break;
                }               
            }            
        } else { // type is role
            ArrayList<RoleXML> roleList = (ArrayList<RoleXML>) roleData.getRoleList();
            for (RoleXML role: roleList) {
                if (role.getRoleId().equals(selection)) {
                    // Update permissions, start by clearing the list
                    role.getPermissionList().clear();
                    for (String perm: permissions) {
                        if (!role.getPermissionList().contains(perm)) {
                            role.addPermission(perm);
                        }
                    }
                    
                    // Add roles
                    for (String r: roles) {
                        if (!role.getPermissionList().contains(r)) {
                            role.addPermission(r);
                        }
                    }
                }
            }
            
            // Add role to selected users
            for (String selectedUser: selectedUsers) {
                for (UserXML user: roleData.getUserList()) {
                    if (selectedUser.equals(user.getUserId())) {
                        if (!user.getRoleList().contains(selection)) {
                            user.addRole(selection);
                            break;
                        }
                    }
                }
            }
        }
        
        close();
    }
    
    @Override
    public void hasEntries(boolean entries) {
        this.setReturnValue(true);
    }

    @Override
    public void selectionChanged() {
        // unused
        
    }
}
