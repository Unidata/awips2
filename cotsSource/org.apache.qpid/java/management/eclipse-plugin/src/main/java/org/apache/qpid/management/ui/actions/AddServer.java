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

import static org.apache.qpid.management.ui.Constants.*;

import org.apache.qpid.management.ui.ApplicationRegistry;
import org.apache.qpid.management.ui.exceptions.InfoRequiredException;
import org.apache.qpid.management.ui.views.NumberVerifyListener;
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

public class AddServer extends AbstractAction implements IWorkbenchWindowActionDelegate 
{
    private String _host;
    private String _port;
    private String _domain = DEFAULT_DOMAIN;
    private String _user;
    private String _password;
    
    private boolean _addServer;
	
    public AddServer()
    {
        
    }
    
    public void run(IAction action)
    {
        if(_window == null)
            return;
        
        reset();
        createAddServerPopup();
        try
        {
            if (_addServer)
            {
                getNavigationView().addNewServer(_host, Integer.parseInt(_port), _domain, _user, _password);
            }
        }
        catch(InfoRequiredException ex)
        {
            ViewUtility.popupInfoMessage(ACTION_ADDSERVER, ex.getMessage());
        }
        catch (Exception ex)
        {
            handleException(ex, null, null);
        }
    }
    
    private void reset()
    {
        _addServer = false;
        _host = null;
        _port = null;
        _user = null;
        _password = null;
    }
    
    /**
     * Creates the shell and then opens the popup where user can enter new connection details.
     * Connects to the new server and adds the server in the navigation page.
     * Pops up any error occured in connecting to the new server
     */
    private void createAddServerPopup()
    {
        final Shell appShell = _window.getShell();

        Display display = Display.getCurrent();
        final Shell shell = new Shell(display, SWT.BORDER | SWT.CLOSE);
        shell.setText(ACTION_ADDSERVER);
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
        int minShellWidth = 425;
        int minShellHeight= 265;        
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
    
    // Creates SWT widgets for the user to add server connection details.
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
        
        /* Commenting this, as there is only one protocol at the moment.
         * This can be uncommented and enhanced, if more protocols are added in future
        Label name = new Label(composite, SWT.NONE);
        name.setText("Connection Type");
        GridData layoutData = new GridData(SWT.TRAIL, SWT.TOP, false, false);
        name.setLayoutData(layoutData);
        
        final Combo comboTransport = new Combo(composite, SWT.READ_ONLY);
        comboTransport.setItems(CONNECTION_PROTOCOLS);
        comboTransport.setLayoutData(new GridData(SWT.FILL, SWT.TOP, true, false));
        comboTransport.select(0);
        */
        
        Label host = new Label(composite, SWT.NONE);
        host.setText("Host");
        host.setLayoutData(new GridData(SWT.TRAIL, SWT.TOP, false, false));
        
        final Text textHost = new Text(composite, SWT.BORDER);
        textHost.setText("");
        textHost.setLayoutData(new GridData(SWT.FILL, SWT.TOP, true, false));
        textHost.setFocus();
        
        Label port = new Label(composite, SWT.NONE);
        port.setText("Port");
        port.setLayoutData(new GridData(SWT.TRAIL, SWT.TOP, false, false));
        
        final Text textPort = new Text(composite, SWT.BORDER);
        textPort.setText("");
        textPort.setLayoutData(new GridData(SWT.FILL, SWT.TOP, true, false));
        // Verify if the value entered is numeric
        textPort.addVerifyListener(new NumberVerifyListener());
        
        Label user = new Label(composite, SWT.NONE);
        user.setText(USERNAME);
        user.setLayoutData(new GridData(SWT.TRAIL, SWT.TOP, false, false));
        
        final Text textUser = new Text(composite, SWT.BORDER);
        textUser.setText("");
        textUser.setLayoutData(new GridData(SWT.FILL, SWT.TOP, true, false));
        
        Label password = new Label(composite, SWT.NONE);
        password.setText(PASSWORD);
        password.setLayoutData(new GridData(SWT.TRAIL, SWT.TOP, false, false));
        
        final Text textPwd = new Text(composite, SWT.BORDER | SWT.SINGLE | SWT.PASSWORD);
        textPwd.setText("");
        //textPwd.setEchoChar('*');
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
        connectButton.setText(BUTTON_CONNECT);
        GridData gridData = new GridData (SWT.TRAIL, SWT.BOTTOM, true, true);
        gridData.widthHint = 100;
        connectButton.setLayoutData(gridData);
        connectButton.setFont(ApplicationRegistry.getFont(FONT_BUTTON));
        connectButton.addSelectionListener(new SelectionAdapter(){
            public void widgetSelected(SelectionEvent event)
            {                
                _host = textHost.getText();               
                if ((_host == null) || (_host.trim().length() == 0))
                {
                    ViewUtility.popupInfoMessage(ACTION_ADDSERVER, INFO_HOST_ADDRESS);
                    textHost.setText("");
                    textHost.setFocus();
                    return;
                }
                
                _port = textPort.getText();
                if ((_port == null) || (_port.trim().length() == 0))
                {
                    ViewUtility.popupInfoMessage(ACTION_ADDSERVER, INFO_HOST_PORT);
                    textPort.setText("");
                    textPort.setFocus();
                    return;
                }
                
                _user = textUser.getText();
                if ((_user == null) || (_user.trim().length() == 0))
                {
                    ViewUtility.popupInfoMessage(ACTION_ADDSERVER, INFO_USERNAME);
                    textUser.setText("");
                    textUser.setFocus();
                    return;
                }
                
                _password = textPwd.getText();
                if (_password == null)
                {
                    ViewUtility.popupInfoMessage(ACTION_ADDSERVER, INFO_PASSWORD);
                    textPwd.setText("");
                    textPwd.setFocus();
                    return;
                }
                
                _addServer = true;
                shell.dispose();                                     
            }
        });
        
        final Button cancelButton = new Button(buttonsComposite, SWT.PUSH);
        cancelButton.setText(BUTTON_CANCEL);
        gridData = new GridData (SWT.LEAD, SWT.BOTTOM, true, true);
        gridData.widthHint = 100;
        cancelButton.setLayoutData(gridData);
        cancelButton.setFont(ApplicationRegistry.getFont(FONT_BUTTON));
        cancelButton.addSelectionListener(new SelectionAdapter()
        {
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
