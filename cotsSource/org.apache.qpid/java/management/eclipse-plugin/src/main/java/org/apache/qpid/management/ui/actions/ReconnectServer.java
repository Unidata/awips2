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
package org.apache.qpid.management.ui.actions;

import static org.apache.qpid.management.ui.Constants.ACTION_LOGIN;
import static org.apache.qpid.management.ui.Constants.CONSOLE_IMAGE;
import static org.apache.qpid.management.ui.Constants.INFO_PASSWORD;
import static org.apache.qpid.management.ui.Constants.INFO_USERNAME;
import static org.apache.qpid.management.ui.Constants.PASSWORD;
import static org.apache.qpid.management.ui.Constants.USERNAME;

import org.apache.qpid.management.ui.ApplicationRegistry;
import org.apache.qpid.management.ui.Constants;
import org.apache.qpid.management.ui.exceptions.InfoRequiredException;
import org.apache.qpid.management.ui.views.TreeObject;
import org.apache.qpid.management.ui.views.ViewUtility;
import org.eclipse.jface.action.IAction;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.KeyAdapter;
import org.eclipse.swt.events.KeyEvent;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Text;
import org.eclipse.ui.IWorkbenchWindowActionDelegate;

public class ReconnectServer extends AbstractAction implements IWorkbenchWindowActionDelegate
{
    private String _title;
    private String _serverName;
    private String _user;
    private String _password;
    private boolean _connect;
    
    public void run(IAction action)
    {
        if(_window == null)
            return;
  
        try
        {
            reset();
            // Check if a server node is selected to be reconnected.
            TreeObject serverNode = getNavigationView().getSelectedServerNode();
            _serverName = serverNode.getName();
            _title = ACTION_LOGIN + " (" + _serverName + ")";

            // Get the login details(username/password)
            createLoginPopup();

            if (_connect)
            {
                // Connect the server
                getNavigationView().reconnect(_user, _password);
            }
        }
        catch(InfoRequiredException ex)
        {
            ViewUtility.popupInfoMessage("Reconnect Qpid server", ex.getMessage());
        }
        catch (Exception ex)
        {
            handleException(ex, null, null);
        }
    }
    
    private void reset()
    {
        _connect = false;
        _user = null;
        _password = null;
    }
    
    // Create the login popup fot th user to enter usernaem and password
    private void createLoginPopup()
    {
        final Shell appShell = _window.getShell();

        Display display = Display.getCurrent();
        final Shell shell = new Shell(display, SWT.BORDER | SWT.CLOSE);
        shell.setText(_title);
        shell.setImage(ApplicationRegistry.getImage(CONSOLE_IMAGE));
        shell.setLayout(new GridLayout());
        
        createWidgets(shell);        
        shell.pack();
        
        //get current size dialog, and application window size and location
        int appWidth = appShell.getBounds().width;
        int appHeight = appShell.getBounds().height;
        int appLocX = appShell.getBounds().x;
        int appLocY = appShell.getBounds().y;
        int currentShellWidth = shell.getSize().x;
        int currentShellHeight = shell.getSize().y;
        
        //default sizes for the dialog
        int minShellWidth = 350;
        int minShellHeight= 200;        
        //ensure this is large enough, increase it if its not
        int newShellWidth =  currentShellWidth > minShellWidth ? currentShellWidth : minShellWidth;
        int newShellHeight = currentShellHeight > minShellHeight ? currentShellHeight : minShellHeight;
        
        //set the final size and centre the dialog within the app window
        shell.setBounds((appWidth - newShellWidth)/2  + appLocX, (appHeight - newShellHeight)/2 + appLocY, newShellWidth, newShellHeight);
        
        shell.open();
        _window.getShell().setEnabled(false);
        
        while (!shell.isDisposed())
        {   
            if (!display.readAndDispatch())
            {
                display.sleep();
            }
        }
        
        // enable the main shell
        _window.getShell().setEnabled(true);
        _window.getShell().open();
    }
    
    // Creates the SWT widgets in the popup shell, to enter username and password.
    // Adds listeners to the widgets to take appropriate action
    private void createWidgets(final Shell shell)
    {
        Composite composite = new Composite(shell, SWT.NONE);
        composite.setLayoutData(new GridData(SWT.FILL, SWT.FILL, true, true));
        GridLayout layout = new GridLayout(2, false);
        layout.horizontalSpacing = 10;
        layout.verticalSpacing = 10;
        layout.marginHeight = 20;
        layout.marginWidth = 20;
        composite.setLayout(layout);
        
        Label user = new Label(composite, SWT.NONE);
        user.setText(USERNAME);
        user.setLayoutData(new GridData(SWT.TRAIL, SWT.TOP, false, false));
        
        final Text textUser = new Text(composite, SWT.BORDER);
        textUser.setText("");
        textUser.setLayoutData(new GridData(SWT.FILL, SWT.TOP, true, false));
        // Put cursor on this field
        textUser.setFocus();
        
        Label password = new Label(composite, SWT.NONE);
        password.setText(PASSWORD);
        password.setLayoutData(new GridData(SWT.TRAIL, SWT.TOP, false, false));
        
        final Text textPwd = new Text(composite, SWT.BORDER | SWT.SINGLE | SWT.PASSWORD);
        textPwd.setText("");
        textPwd.setLayoutData(new GridData(SWT.FILL, SWT.TOP, true, false));
        
        //Get the text widgets
        Control[] widgets = composite.getChildren();
        for (int i=0; i < widgets.length; i++)
        {
            widgets[i].addKeyListener(new KeyAdapter()
            {
                public void keyPressed(KeyEvent event)
                {
                    if (event.character == SWT.ESC)
                    {
                      //Escape key acts as cancel on all widgets
                        shell.dispose();
                    }
                }
            });
        }
        
        Composite buttonsComposite  = new Composite(composite, SWT.NONE);
        buttonsComposite.setLayoutData(new GridData(SWT.FILL, SWT.FILL, true, true, 2, 1));
        buttonsComposite.setLayout(new GridLayout(2, true));
        
        final Button connectButton = new Button(buttonsComposite, SWT.PUSH | SWT.CENTER);       
        connectButton.setText(Constants.BUTTON_CONNECT);
        GridData gridData = new GridData (SWT.TRAIL, SWT.BOTTOM, true, true);
        gridData.widthHint = 100;
        connectButton.setLayoutData(gridData);
        connectButton.setFont(ApplicationRegistry.getFont(Constants.FONT_BUTTON));
        connectButton.addSelectionListener(new SelectionAdapter(){
            public void widgetSelected(SelectionEvent event)
            {
                _user = textUser.getText();
                if ((_user == null) || (_user.trim().length() == 0))
                {
                    ViewUtility.popupInfoMessage(_title, INFO_USERNAME);
                    textUser.setText("");
                    textUser.setFocus();
                    return;
                }
                
                _password = textPwd.getText();
                if (_password == null)
                {
                    ViewUtility.popupInfoMessage(_title, INFO_PASSWORD);
                    textPwd.setText("");
                    textPwd.setFocus();
                    return;
                }
                
                _connect = true;
                shell.dispose();
            }
        });

        final Button cancelButton = new Button(buttonsComposite, SWT.PUSH);
        cancelButton.setText(Constants.BUTTON_CANCEL);
        gridData = new GridData (SWT.LEAD, SWT.BOTTOM, true, true);
        gridData.widthHint = 100;
        cancelButton.setLayoutData(gridData);
        cancelButton.setFont(ApplicationRegistry.getFont(Constants.FONT_BUTTON));
        cancelButton.addSelectionListener(new SelectionAdapter(){
            public void widgetSelected(SelectionEvent event)
            {
                shell.dispose();
            }
        });
        
        //Get the ok/cancel button widgets and add a new key listener
        widgets = buttonsComposite.getChildren();
        for (int i=0; i < widgets.length; i++)
        {
            widgets[i].addKeyListener(new KeyAdapter()
            {
                public void keyPressed(KeyEvent event)
                {
                    if (event.character == SWT.ESC)
                    {
                        //Escape key acts as cancel on all widgets
                        shell.dispose();
                    }
                }
            });
        }
        
        shell.setDefaultButton(connectButton);
    }
    
}
