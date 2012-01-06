/*
 *
 * Licensed to the Apache Software Foundation (ASF) under one
 * or more contributor license agreements.  See the NOTICE file
 * distributed with this work for additional information
 * regarding copyright ownership.  The ASF licenses this file
 * to you under the Apache License, Version 2.0 (the
 * "License"); you may not use this file except in compliance
 * with the License.  You may obtain a copy of the License at
 * 
 *   http://www.apache.org/licenses/LICENSE-2.0
 * 
 * Unless required by applicable law or agreed to in writing,
 * software distributed under the License is distributed on an
 * "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
 * KIND, either express or implied.  See the License for the
 * specific language governing permissions and limitations
 * under the License.
 *
 */
package org.apache.qpid.management.ui.views.users;

import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;

import javax.management.MBeanServerConnection;
import javax.management.MBeanServerInvocationHandler;
import javax.management.openmbean.CompositeData;
import javax.management.openmbean.TabularDataSupport;

import org.apache.qpid.management.ui.ApiVersion;
import org.apache.qpid.management.ui.ApplicationRegistry;
import org.apache.qpid.management.ui.ManagedBean;
import org.apache.qpid.management.common.mbeans.UserManagement;
import org.apache.qpid.management.ui.jmx.JMXManagedObject;
import org.apache.qpid.management.ui.jmx.MBeanUtility;
import org.apache.qpid.management.ui.views.TabControl;
import org.apache.qpid.management.ui.views.ViewUtility;
import org.eclipse.jface.dialogs.InputDialog;
import org.eclipse.jface.viewers.ISelectionChangedListener;
import org.eclipse.jface.viewers.IStructuredContentProvider;
import org.eclipse.jface.viewers.ITableLabelProvider;
import org.eclipse.jface.viewers.LabelProvider;
import org.eclipse.jface.viewers.SelectionChangedEvent;
import org.eclipse.jface.viewers.TableViewer;
import org.eclipse.jface.viewers.TableViewerColumn;
import org.eclipse.jface.viewers.Viewer;
import org.eclipse.jface.viewers.ViewerSorter;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.TabFolder;
import org.eclipse.swt.widgets.Table;
import org.eclipse.swt.widgets.TableColumn;
import org.eclipse.swt.widgets.Text;
import org.eclipse.ui.forms.widgets.FormToolkit;
import org.eclipse.ui.forms.widgets.ScrolledForm;


/**
 * Control class for the UserManagement mbean.
 */
public class UserManagementTabControl extends TabControl
{
    private FormToolkit _toolkit;
    private ScrolledForm        _form;
    private Table _table = null;
    private TableViewer _tableViewer = null;

    private TabularDataSupport _userDetails = null;
    private UserManagement _ummb;
    private ApiVersion _ApiVersion;
    
    static final String USERNAME = UserManagement.COMPOSITE_ITEM_NAMES[0];
    static final String RIGHTS_READ_ONLY = UserManagement.COMPOSITE_ITEM_NAMES[1];
    static final String RIGHTS_READ_WRITE = UserManagement.COMPOSITE_ITEM_NAMES[2];
    static final String RIGHTS_ADMIN = UserManagement.COMPOSITE_ITEM_NAMES[3];
    
    public UserManagementTabControl(TabFolder tabFolder, JMXManagedObject mbean, MBeanServerConnection mbsc)
    {
        super(tabFolder);
        _mbean = mbean;
        _ApiVersion = ApplicationRegistry.getServerRegistry(mbean).getManagementApiVersion();
        _ummb = (UserManagement)
                MBeanServerInvocationHandler.newProxyInstance(mbsc, mbean.getObjectName(),
                                                            UserManagement.class, false);
        _toolkit = new FormToolkit(_tabFolder.getDisplay());
        _form = _toolkit.createScrolledForm(_tabFolder);
        _form.getBody().setLayout(new GridLayout());
        createWidgets();
    }

    /**
     * @see TabControl#getControl()
     */
    public Control getControl()
    {
        return _form;
    }
    
    /**
     * @see TabControl#setFocus()
     */
    public void setFocus()
    {
        _table.setFocus();
    }
    
    @Override
    public void refresh(ManagedBean mbean)
    {
        _userDetails = null;
        try
        {
            _userDetails = (TabularDataSupport) _ummb.viewUsers();
        }
        catch(Exception e)
        {
            MBeanUtility.handleException(_mbean, e);
            
        }

        _tableViewer.setInput(_userDetails);
        
        layout();
    }
    
    public void layout()
    {
        _form.layout(true);
        _form.getBody().layout(true, true);
    }
    
    private void createWidgets()
    {
        Composite paramsComposite = _toolkit.createComposite(_form.getBody(), SWT.NONE);
        paramsComposite.setLayoutData(new GridData(SWT.FILL, SWT.FILL, true, true));
        paramsComposite.setLayout(new GridLayout());
        
        Group viewUsersGroup = new Group(paramsComposite, SWT.SHADOW_NONE);
        viewUsersGroup.setBackground(paramsComposite.getBackground());
        viewUsersGroup.setText("Users");
        viewUsersGroup.setLayout(new GridLayout(2,false));
        viewUsersGroup.setLayoutData(new GridData(SWT.FILL, SWT.FILL, true, true));
        
        Composite tableComposite = _toolkit.createComposite(viewUsersGroup);
        GridData gridData = new GridData(SWT.FILL, SWT.FILL, true, true);
        gridData.heightHint = 250;
        gridData.minimumHeight = 250;
        tableComposite.setLayoutData(gridData);
        tableComposite.setLayout(new GridLayout(2,false));
        
        _table = new Table (tableComposite, SWT.MULTI | SWT.SCROLL_LINE | SWT.BORDER | SWT.FULL_SELECTION);
        _table.setLinesVisible (true);
        _table.setHeaderVisible (true);
        gridData = new GridData(SWT.FILL, SWT.FILL, true, true);
        _table.setLayoutData(gridData);
        
        _tableViewer = new TableViewer(_table);
        final TableSorter tableSorter = new TableSorter();
        
        String[] titles = { "Username", "JMX Management Rights" };
        int[] bounds = { 310, 200 };
        for (int i = 0; i < titles.length; i++) 
        {
            final int index = i;
            final TableViewerColumn viewerColumn = new TableViewerColumn(_tableViewer, SWT.NONE);
            final TableColumn column = viewerColumn.getColumn();

            column.setText(titles[i]);
            column.setWidth(bounds[i]);
            column.setResizable(true);

            //Setting the right sorter
            column.addSelectionListener(new SelectionAdapter() 
            {
                @Override
                public void widgetSelected(SelectionEvent e) 
                {
                    tableSorter.setColumn(index);
                    final TableViewer viewer = _tableViewer;
                    int dir = viewer .getTable().getSortDirection();
                    if (viewer.getTable().getSortColumn() == column) 
                    {
                        dir = dir == SWT.UP ? SWT.DOWN : SWT.UP;
                    } 
                    else 
                    {
                        dir = SWT.UP;
                    }
                    viewer.getTable().setSortDirection(dir);
                    viewer.getTable().setSortColumn(column);
                    viewer.refresh();
                }
            });

        }
        
        _tableViewer.setContentProvider(new ContentProviderImpl());
        _tableViewer.setLabelProvider(new LabelProviderImpl());
        _tableViewer.setSorter(tableSorter);
        _table.setSortColumn(_table.getColumn(0));
        _table.setSortDirection(SWT.UP);
        
        Composite buttonsComposite = _toolkit.createComposite(tableComposite);
        gridData = new GridData(SWT.FILL, SWT.TOP, false, false);
        gridData.heightHint = 165;
        buttonsComposite.setLayoutData(gridData);
        buttonsComposite.setLayout(new GridLayout());
        
        final Button addUserButton = _toolkit.createButton(buttonsComposite, "Add New User ...", SWT.PUSH);
        gridData = new GridData(SWT.CENTER, SWT.TOP, false, true);
        gridData.widthHint = 125;
        addUserButton.setLayoutData(gridData);
        addUserButton.addSelectionListener(new SelectionAdapter()
        {
            public void widgetSelected(SelectionEvent e)
            {
                addUser(addUserButton.getShell());
            }
        });
        
        final Button deleteUsersButton = _toolkit.createButton(buttonsComposite, "Delete User(s)", SWT.PUSH);
        gridData = new GridData(SWT.CENTER, SWT.BOTTOM, false, false);
        gridData.widthHint = 125;
        deleteUsersButton.setLayoutData(gridData);
        deleteUsersButton.setEnabled(false);
        deleteUsersButton.addSelectionListener(new SelectionAdapter()
        {
            public void widgetSelected(SelectionEvent e)
            {
                int selectionIndex = _table.getSelectionIndex();

                if (selectionIndex != -1)
                {
                    deleteUsers();
                }
            }
        });
        
        final Button setPasswordButton = _toolkit.createButton(buttonsComposite, "Set Password ...", SWT.PUSH);
        gridData = new GridData(SWT.CENTER, SWT.BOTTOM, false, false);
        gridData.widthHint = 125;
        setPasswordButton.setLayoutData(gridData);
        setPasswordButton.setEnabled(false);
        setPasswordButton.addSelectionListener(new SelectionAdapter()
        {
            public void widgetSelected(SelectionEvent e)
            {
                int selectionIndex = _table.getSelectionIndex();

                if (selectionIndex != -1)
                {
                    final CompositeData selectedLogger = (CompositeData)_table.getItem(
                                                                        selectionIndex).getData();
                    String user = selectedLogger.get(USERNAME).toString();
                    InputDialog id = new InputDialog(setPasswordButton.getShell(),"Set Password",
                                        "Please enter the new password for '" + user + "':",null,null){
                        @Override
                        protected Control createDialogArea(Composite parent)
                        {
                            Control control = super.createDialogArea(parent);
                            //set the Text field echo char to '*' to mask the password 
                            getText().setEchoChar('*');
                            //return the normal result
                            return control;
                        }
                    };
                    
                    int returnValue;
                    while((returnValue = id.open()) == InputDialog.OK)
                    {
                        if (id.getValue() == null || id.getValue().toString().length() == 0)
                        {                            
                            ViewUtility.popupErrorMessage("Set Password", "Please enter a valid password");                       
                        }
                        else
                        {
                            break;
                        }
                    }
                    
                    if (returnValue  == InputDialog.OK)
                    {
                        char[] password = id.getValue().toCharArray();

                        // Qpid JMX API 1.1 and below expects the password to be sent as a hashed value.
                        if (_ApiVersion.lessThanOrEqualTo(1,1))
                        {
                            try
                            {
                                password = ViewUtility.getHash(id.getValue());
                            }
                            catch (Exception hashException)
                            {
                                ViewUtility.popupErrorMessage("Set Password",
                                        "Unable to calculate hash for Password:"
                                        + hashException.getMessage());
                                return;
                            }
                        }

                        try
                        {
                            boolean result = _ummb.setPassword(user, password);
                            ViewUtility.operationResultFeedback(result, "Updated user password", "Failed to update user password");
                        }
                        catch(Exception e2)
                        {
                            ViewUtility.operationFailedStatusBarMessage("Error updating user password");
                            MBeanUtility.handleException(_mbean, e2);
                        }
                    }
                }
            }
        });
        
        final Button setRightsButton = _toolkit.createButton(buttonsComposite, "Set Rights ...", SWT.PUSH);
        gridData = new GridData(SWT.CENTER, SWT.BOTTOM, false, false);
        gridData.widthHint = 125;
        setRightsButton.setLayoutData(gridData);
        setRightsButton.setEnabled(false);
        setRightsButton.addSelectionListener(new SelectionAdapter()
        {
            public void widgetSelected(SelectionEvent e)
            {
                int selectionIndex = _table.getSelectionIndex();

                if (selectionIndex != -1)
                {
                    setRights(setRightsButton.getShell());
                }
            }
        });
        
        _tableViewer.addSelectionChangedListener(new ISelectionChangedListener(){
            public void selectionChanged(SelectionChangedEvent evt)
            {
                int selectionIndex = _table.getSelectionIndex();

                if (selectionIndex == -1)
                {
                    deleteUsersButton.setEnabled(false);
                    setRightsButton.setEnabled(false);
                    setPasswordButton.setEnabled(false);
                    return;
                }
                else
                {
                    deleteUsersButton.setEnabled(true);
                    setRightsButton.setEnabled(true);
                }
                
                if (_table.getSelectionCount() > 1)
                {
                    setPasswordButton.setEnabled(false);
                }
                else
                {
                    setPasswordButton.setEnabled(true);
                }
            }
        });
        
        Group miscGroup = new Group(paramsComposite, SWT.SHADOW_NONE);
        miscGroup.setBackground(paramsComposite.getBackground());
        miscGroup.setText("Misc");
        gridData = new GridData(SWT.LEFT, SWT.TOP, true, true);
        miscGroup.setLayoutData(gridData);
        miscGroup.setLayout(new GridLayout(2,false));

        final Button reloadUserDetails = _toolkit.createButton(miscGroup, 
                                                                "Reload User Details", SWT.PUSH);
        if(_ApiVersion.lessThan(1, 2))
        {
            //this only reloaded the JMX rights file before Qpid JMX API 1.2
            _toolkit.createLabel(miscGroup, " Loads the current management rights file from disk");
        }
        else
        {
            //since Qpid JMX API 1.2 it also reloads the password file
            _toolkit.createLabel(miscGroup, " Loads the current password and management rights files from disk");
        }
        reloadUserDetails.addSelectionListener(new SelectionAdapter()
        {
            public void widgetSelected(SelectionEvent e)
            {
                int response = ViewUtility.popupOkCancelConfirmationMessage("Reload User Data", 
                                                            "Do you want to reload user data ?");
                if (response == SWT.OK)
                {
                    try
                    {
                        boolean result = _ummb.reloadData();
                        ViewUtility.operationResultFeedback(result, "Reloaded user data", "Failed to reload user data");
                    }
                    catch(Exception e3)
                    {
                        ViewUtility.operationFailedStatusBarMessage("Error reloading user data");
                        MBeanUtility.handleException(_mbean, e3);
                    }
                    refresh(_mbean);
                }
            }
        });

    }

    
    /**
     * Content Provider class for the table viewer
     */
    private class ContentProviderImpl  implements IStructuredContentProvider
    {
        
        public void inputChanged(Viewer v, Object oldInput, Object newInput)
        {
            
        }
        
        public void dispose()
        {
            
        }
        
        public Object[] getElements(Object parent)
        {
            Collection<Object> rowCollection = ((TabularDataSupport) parent).values();
           
            return rowCollection.toArray();
        }
    }
    
    /**
     * Label Provider class for the table viewer
     */
    private class LabelProviderImpl extends LabelProvider implements ITableLabelProvider
    {
        @Override
        public String getColumnText(Object element, int columnIndex)
        {
            switch (columnIndex)
            {
                case 0 : // username column 
                    return (String) ((CompositeData) element).get(USERNAME);
                case 1 : // rights column 
                    return classifyUserRights((CompositeData) element);
                default :
                    return "-";
            }
        }
        
        @Override
        public Image getColumnImage(Object element, int columnIndex)
        {
            return null;
        }
        
    }

    /**
     * Sorter class for the table viewer.
     *
     */
    private class TableSorter extends ViewerSorter
    {
        private int column;
        private static final int ASCENDING = 0;
        private static final int DESCENDING = 1;

        private int direction;

        public TableSorter()
        {
            this.column = 0;
            direction = ASCENDING;
        }

        public void setColumn(int column)
        {
            if(column == this.column)
            {
                // Same column as last sort; toggle the direction
                direction = 1 - direction;
            }
            else
            {
                // New column; do an ascending sort
                this.column = column;
                direction = ASCENDING;
            }
        }

        @Override
        public int compare(Viewer viewer, Object e1, Object e2)
        {
            CompositeData user1 = (CompositeData) e1;
            CompositeData user2 = (CompositeData) e2;
            
            int comparison = 0;
            switch(column)
            {
                case 0:
                    comparison = String.valueOf(user1.get(USERNAME)).compareTo(
                                                String.valueOf(user2.get(USERNAME)));
                    break;
                case 1:
                    comparison = classifyUserRights(user1).compareTo(classifyUserRights(user2));
                    break;
                default:
                    comparison = 0;
            }
            // If descending order, flip the direction
            if(direction == DESCENDING)
            {
                comparison = -comparison;
            }
            return comparison;
        }
    }
    
    private String classifyUserRights(CompositeData user)
    {
        Boolean read = (Boolean)user.get(RIGHTS_READ_ONLY);
        Boolean write = (Boolean)user.get(RIGHTS_READ_WRITE);
        Boolean admin = (Boolean)user.get(RIGHTS_ADMIN);
        
        if(admin)
        {
            return "Admin";
        }
        else if(write)
        {
            return "Read & Write";
        }
        else if(read)
        {
            return "Read Only";
        }
        else
        {
            return "No Access";
        }
    }
    
    private void setRights(final Shell parent)
    {
        
        int selectionIndex = _table.getSelectionIndex();

        if (selectionIndex == -1)
        {
            return;
        }

        int[] selectedIndices = _table.getSelectionIndices();

        final ArrayList<String> selectedUsers = new ArrayList<String>();

        for(int index = 0; index < selectedIndices.length ; index++)
        {
            CompositeData selectedUser = (CompositeData)_table.getItem(selectedIndices[index]).getData();
            String user = selectedUser.get(USERNAME).toString();
            selectedUsers.add(user);
        }

        String selectedUsersString = "";
        for(String user : selectedUsers)
        {
            selectedUsersString = selectedUsersString.concat(user + ", ");
        }
        //cut off last ", "
        int lastIndex = selectedUsersString.lastIndexOf(',');
        if (lastIndex != -1)
        {
            selectedUsersString = selectedUsersString.substring(0,lastIndex);
        }


        
        final Shell shell = ViewUtility.createModalDialogShell(parent, "Set Rights");
        
        Label overview = _toolkit.createLabel(shell,"Select rights for user(s): ");
        overview.setBackground(shell.getBackground());
        Label userNamesLabel= _toolkit.createLabel(shell,selectedUsersString);
        userNamesLabel.setBackground(shell.getBackground());

        Composite buttons = _toolkit.createComposite(shell, SWT.NONE);
        buttons.setBackground(shell.getBackground());
        buttons.setLayoutData(new GridData(SWT.FILL, SWT.TOP, true, false));
        buttons.setLayout(new GridLayout(4,false));

        final Button noneButton = new Button(buttons, SWT.RADIO);
        noneButton.setText("No Access");
        noneButton.setSelection(true);
        final Button readButton = new Button(buttons, SWT.RADIO);
        readButton.setText("Read Only");
        final Button writeButton = new Button(buttons, SWT.RADIO);
        writeButton.setText("Read + Write");
        final Button adminButton = new Button(buttons, SWT.RADIO);
        adminButton.setText("Admin");

        Composite okCancelButtonsComp = _toolkit.createComposite(shell);
        okCancelButtonsComp.setBackground(shell.getBackground());
        okCancelButtonsComp.setLayoutData(new GridData(SWT.RIGHT, SWT.FILL, true, true));
        okCancelButtonsComp.setLayout(new GridLayout(2,false));
        
        Button okButton = _toolkit.createButton(okCancelButtonsComp, "OK", SWT.PUSH);
        okButton.setLayoutData(new GridData(SWT.RIGHT, SWT.TOP, false, false));
        Button cancelButton = _toolkit.createButton(okCancelButtonsComp, "Cancel", SWT.PUSH);
        cancelButton.setLayoutData(new GridData(SWT.RIGHT, SWT.TOP, false, false));

        okButton.addSelectionListener(new SelectionAdapter()
        {
            public void widgetSelected(SelectionEvent e)
            {
                boolean read = readButton.getSelection();
                boolean write = writeButton.getSelection();
                boolean admin = adminButton.getSelection();
                
                shell.dispose();

                HashMap<String,Boolean> results = new HashMap<String,Boolean>();
                try
                {
                    //perform the rights updates, save the results.
                    for(String user : selectedUsers)
                    {
                        boolean result = _ummb.setRights(user,read,write,admin);
                        results.put(user, result);
                    }

                    //categorise the overall result
                    boolean overallResult = true;
                    for(boolean result : results.values())
                    {
                        if (!result)
                        {
                            overallResult = false;
                        }
                    }

                    //output the result to status bar if all success, and dialogue if not
                    if(overallResult)
                    {
                        ViewUtility.operationResultFeedback(overallResult, "Updated user rights", null);
                    }
                    else
                    {
                        String failedToUpdateRightsUsers = "";
                        for(String user : results.keySet())
                        {
                            if(!results.get(user))
                            {
                                failedToUpdateRightsUsers = failedToUpdateRightsUsers.concat(user + ", ");
                            }
                        }

                        //cut off last ", "
                        int lastIndex2 = failedToUpdateRightsUsers.lastIndexOf(',');
                        if (lastIndex2 != -1)
                        {
                            failedToUpdateRightsUsers = failedToUpdateRightsUsers.substring(0, lastIndex2);
                        }

                        ViewUtility.operationResultFeedback(overallResult, null, "Failed to update user(s) rights: " + failedToUpdateRightsUsers);
                    }
                }
                catch(Exception e4)
                {
                    ViewUtility.operationFailedStatusBarMessage("Error updating user rights");
                    MBeanUtility.handleException(_mbean, e4);
                }
                refresh(_mbean);
            }
        });

        cancelButton.addSelectionListener(new SelectionAdapter()
        {
            public void widgetSelected(SelectionEvent e)
            {
                shell.dispose();
            }
        });

        shell.setDefaultButton(okButton);
        shell.pack();
        ViewUtility.centerChildInParentShell(parent, shell);
        
        shell.open();
    }
    
    private void addUser(final Shell parent)
    {
        final Shell shell = ViewUtility.createModalDialogShell(parent, "Add New User");
        
        Composite usernameComposite = _toolkit.createComposite(shell, SWT.NONE);
        usernameComposite.setBackground(shell.getBackground());
        usernameComposite.setLayoutData(new GridData(SWT.FILL, SWT.TOP, true, false));
        usernameComposite.setLayout(new GridLayout(2,false));
        
        _toolkit.createLabel(usernameComposite,"Username:").setBackground(shell.getBackground());
        final Text usernameText = new Text(usernameComposite, SWT.BORDER);
        usernameText.setLayoutData(new GridData(SWT.FILL, SWT.TOP, true, false));
        
        Composite passwordComposite = _toolkit.createComposite(shell, SWT.NONE);
        passwordComposite.setBackground(shell.getBackground());
        passwordComposite.setLayoutData(new GridData(SWT.FILL, SWT.TOP, true, false));
        passwordComposite.setLayout(new GridLayout(2,false));
        
        _toolkit.createLabel(passwordComposite,"Password:").setBackground(shell.getBackground());
        final Text passwordText = new Text(passwordComposite, SWT.BORDER | SWT.PASSWORD);
        passwordText.setLayoutData(new GridData(SWT.FILL, SWT.TOP, true, false));
        
        Group buttonGroup = new Group(shell, SWT.NONE);
        buttonGroup.setText("JMX Management Rights");
        buttonGroup.setBackground(shell.getBackground());
        buttonGroup.setLayoutData(new GridData(SWT.FILL, SWT.TOP, true, false));
        buttonGroup.setLayout(new GridLayout(4,false));

        final Button noneButton = new Button(buttonGroup, SWT.RADIO);
        noneButton.setText("No Access");
        noneButton.setSelection(true);
        final Button readButton = new Button(buttonGroup, SWT.RADIO);
        readButton.setText("Read Only");
        final Button writeButton = new Button(buttonGroup, SWT.RADIO);
        writeButton.setText("Read + Write");
        final Button adminButton = new Button(buttonGroup, SWT.RADIO);
        adminButton.setText("Admin");

        Composite okCancelButtonsComp = _toolkit.createComposite(shell);
        okCancelButtonsComp.setBackground(shell.getBackground());
        okCancelButtonsComp.setLayoutData(new GridData(SWT.RIGHT, SWT.FILL, true, true));
        okCancelButtonsComp.setLayout(new GridLayout(2,false));
        
        Button okButton = _toolkit.createButton(okCancelButtonsComp, "OK", SWT.PUSH);
        okButton.setLayoutData(new GridData(SWT.RIGHT, SWT.TOP, false, false));
        Button cancelButton = _toolkit.createButton(okCancelButtonsComp, "Cancel", SWT.PUSH);
        cancelButton.setLayoutData(new GridData(SWT.RIGHT, SWT.TOP, false, false));

        okButton.addSelectionListener(new SelectionAdapter()
        {
            public void widgetSelected(SelectionEvent e)
            {
                String username = usernameText.getText();
                String password = passwordText.getText();
                
                if (username == null || username.length() == 0)
                {                            
                    ViewUtility.popupErrorMessage("Add New User", "Please enter a valid username");
                    return;
                }
                
                if (password == null || password.length() == 0)
                {                            
                    ViewUtility.popupErrorMessage("Add New User", "Please enter a valid password");
                    return;
                }
                
                char[] passwordChars = password.toCharArray();

                // Qpid JMX API 1.1 and below expects the password to be sent as a hashed value.
                if (_ApiVersion.lessThanOrEqualTo(1,1))
                {
                    try
                    {
                        passwordChars = ViewUtility.getHash(password);
                    }
                    catch (Exception hashException)
                    {
                        ViewUtility.popupErrorMessage("Set Password",
                                "Unable to calculate hash for Password:"
                                + hashException.getMessage());
                        return;
                    }
                }
                
                boolean read = readButton.getSelection();
                boolean write = writeButton.getSelection();
                boolean admin = adminButton.getSelection();
                
                shell.dispose();
                try
                {
                    boolean result = _ummb.createUser(username, passwordChars, read, write, admin);
                    ViewUtility.operationResultFeedback(result, "Created user", "Failed to create user");
                }
                catch(Exception e5)
                {
                    ViewUtility.operationFailedStatusBarMessage("Error creating user");
                    MBeanUtility.handleException(_mbean, e5);
                }

                refresh(_mbean);
            }
        });

        cancelButton.addSelectionListener(new SelectionAdapter()
        {
            public void widgetSelected(SelectionEvent e)
            {
                shell.dispose();
            }
        });

        shell.setDefaultButton(okButton);
        shell.pack();
        ViewUtility.centerChildInParentShell(parent, shell);
        
        shell.open();
    }
    
    private void deleteUsers()
    {
        int selectionIndex = _table.getSelectionIndex();
        if (selectionIndex == -1)
        {
            return;
        }
        
        int[] selectedIndices = _table.getSelectionIndices();
        
        ArrayList<String> selectedUsers = new ArrayList<String>();
        
        for(int index = 0; index < selectedIndices.length ; index++)
        {
            CompositeData selectedUser = (CompositeData)_table.getItem(selectedIndices[index]).getData();
            String user = selectedUser.get(USERNAME).toString();
            selectedUsers.add(user);
        }

        String selectedUsersString = "";
        for(String user : selectedUsers)
        {
            selectedUsersString = selectedUsersString.concat(user + ", ");
        }
        //cut off last ", "
        int lastIndex = selectedUsersString.lastIndexOf(',');
        if (lastIndex != -1)
        {
            selectedUsersString = selectedUsersString.substring(0,lastIndex);
        }
        
        int response = ViewUtility.popupOkCancelConfirmationMessage(
                "User Management", "Delete user(s): " + selectedUsersString + " ?");
        
        if (response == SWT.OK)
        {
            HashMap<String,Boolean> results = new HashMap<String,Boolean>();
            try
            {
                //perform the deletes, save the results.
                for(String user : selectedUsers)
                {
                    boolean result = _ummb.deleteUser(user);
                    results.put(user, result);
                }
                
                //categorise the overall result
                boolean overallResult = true;
                for(boolean result : results.values())
                {
                    if (!result)
                    {
                        overallResult = false;
                    }
                }
                
                //output the result to status bar if all success, and dialogue if not
                if(overallResult)
                {
                    ViewUtility.operationResultFeedback(overallResult, "Deleted user(s)", null);
                }
                else
                {
                    String failedToDeleteUsers = "";
                    for(String user : results.keySet())
                    {
                        if(!results.get(user))
                        {
                            failedToDeleteUsers = failedToDeleteUsers.concat(user + ", ");
                        }
                    }
                    
                    //cut off last ", "
                    lastIndex = failedToDeleteUsers.lastIndexOf(',');
                    if (lastIndex != -1)
                    {
                        failedToDeleteUsers = failedToDeleteUsers.substring(0, lastIndex);
                    }
                    
                    ViewUtility.operationResultFeedback(overallResult, null, "Failed to delete user(s): " + failedToDeleteUsers);
                }
                
            }
            catch(Exception e1)
            {
                ViewUtility.operationFailedStatusBarMessage("Error deleting user(s)");
                MBeanUtility.handleException(_mbean, e1);
            }

            refresh(_mbean);;
        }
    }
}
