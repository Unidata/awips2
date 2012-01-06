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
package org.apache.qpid.management.ui.views;

import static org.apache.qpid.management.ui.Constants.BUTTON_CLEAR;
import static org.apache.qpid.management.ui.Constants.DESCRIPTION;
import static org.apache.qpid.management.ui.Constants.FONT_BOLD;
import static org.apache.qpid.management.ui.Constants.FONT_BUTTON;
import static org.apache.qpid.management.ui.Constants.FONT_ITALIC;
import static org.apache.qpid.management.ui.Constants.SUBSCRIBE_BUTTON;
import static org.apache.qpid.management.ui.Constants.UNSUBSCRIBE_BUTTON;

import java.util.ArrayList;
import java.util.List;

import org.apache.qpid.management.ui.ApplicationRegistry;
import org.apache.qpid.management.ui.ManagedBean;
import org.apache.qpid.management.ui.ServerRegistry;
import org.apache.qpid.management.ui.jmx.MBeanUtility;
import org.apache.qpid.management.ui.model.NotificationInfoModel;
import org.apache.qpid.management.ui.model.NotificationObject;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.events.SelectionListener;
import org.eclipse.swt.layout.FormAttachment;
import org.eclipse.swt.layout.FormData;
import org.eclipse.swt.layout.FormLayout;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Combo;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.TabFolder;

/**
 * Creates control composite for Notifications tab
 * @author Bhupendra Bhardwaj
 */
public class NotificationsTabControl extends VHNotificationsTabControl
{    
    private static final String SELECT_NOTIFICATIONNAME = "Select Notification";
    private static final String SELECT_NOTIFICATIONTYPE = "Select Type";
    private SelectionListener selectionListener;
    private SelectionListener comboListener;    
    
    private Combo _notificationNameCombo;
    private Combo _typesCombo;
    private Label _descriptionLabel;
    private Button _subscribeButton;
    private Button _unsubscribeButton;
    
    public NotificationsTabControl(TabFolder tabFolder, ManagedBean mbean)
    {
        super(tabFolder);
        _mbean = mbean;
        
        populateNotificationInfo();
    }
    
    protected void createWidgets()
    {            
        selectionListener = new SelectionListenerImpl();
        comboListener = new ComboSelectionListener();
        createNotificationInfoComposite();
        addButtons();  
        createTableViewer();
    }
    
    /**
     * Creates composite and populates for displaying Notification Information (name, type, description)
     * and creates buttons for subscribing or unsubscribing for notifications
     */
    private void createNotificationInfoComposite()
    {
        Composite composite = _toolkit.createComposite(_form.getBody(), SWT.NONE);
        composite.setLayoutData(new GridData(SWT.FILL, SWT.TOP, true, false));
        composite.setLayout(new FormLayout());
        
        Label label = _toolkit.createLabel(composite, "Select the notification to subscribe or unsubscribe");
        label.setFont(ApplicationRegistry.getFont(FONT_BOLD));
        FormData formData = new FormData();
        formData.top = new FormAttachment(0, 10);
        formData.left = new FormAttachment(0, 10);
        label.setLayoutData(formData);
        
        _notificationNameCombo = new Combo(composite, SWT.READ_ONLY | SWT.DROP_DOWN);
        formData = new FormData();
        formData.top = new FormAttachment(label, 10);
        formData.left = new FormAttachment(0, 10);
        formData.right = new FormAttachment(40);
        _notificationNameCombo.setLayoutData(formData);
        _notificationNameCombo.addSelectionListener(comboListener);
        
        _typesCombo = new Combo(composite, SWT.READ_ONLY | SWT.DROP_DOWN);
        formData = new FormData();
        formData.top = new FormAttachment(label, 10);
        formData.left = new FormAttachment(_notificationNameCombo, 5);
        formData.right = new FormAttachment(65);
        _typesCombo.setLayoutData(formData);
        _typesCombo.addSelectionListener(comboListener);
        
        _subscribeButton = new Button(composite, SWT.PUSH | SWT.CENTER);
        _subscribeButton.setFont(ApplicationRegistry.getFont(FONT_BUTTON));
        _subscribeButton.setText(SUBSCRIBE_BUTTON);
        formData = new FormData();
        formData.top = new FormAttachment(label, 10);
        formData.left = new FormAttachment(65, 10);
        formData.width = 80;
        _subscribeButton.setLayoutData(formData);
        _subscribeButton.addSelectionListener(selectionListener);
        
        _unsubscribeButton = new Button(composite, SWT.PUSH | SWT.CENTER);
        _unsubscribeButton.setFont(ApplicationRegistry.getFont(FONT_BUTTON));
        _unsubscribeButton.setText(UNSUBSCRIBE_BUTTON);
        formData = new FormData();
        formData.top = new FormAttachment(label, 10);
        formData.left = new FormAttachment(_subscribeButton, 10);
        formData.width = 80;
        _unsubscribeButton.setLayoutData(formData);
        _unsubscribeButton.addSelectionListener(selectionListener);
        
        Label fixedLabel = _toolkit.createLabel(composite, "");
        formData = new FormData();
        formData.top = new FormAttachment(_notificationNameCombo, 5);
        formData.left = new FormAttachment(0, 10);
        fixedLabel.setLayoutData(formData);
        fixedLabel.setText(DESCRIPTION + " : ");
        fixedLabel.setFont(ApplicationRegistry.getFont(FONT_BOLD));
        
        _descriptionLabel = _toolkit.createLabel(composite, "");
        formData = new FormData();
        formData.top = new FormAttachment(_notificationNameCombo, 5);
        formData.left = new FormAttachment(fixedLabel, 10);
        formData.right = new FormAttachment(100);
        _descriptionLabel.setLayoutData(formData);
        _descriptionLabel.setText("      ");
        _descriptionLabel.setFont(ApplicationRegistry.getFont(FONT_ITALIC));
    }
    
    /**
     * Creates clear button
     */
    protected void addButtons()
    {    
        Composite composite = _toolkit.createComposite(_form.getBody(), SWT.NONE);
        composite.setLayoutData(new GridData(SWT.LEFT, SWT.TOP, false, false));
        composite.setLayout(new GridLayout(2,false));
        
        // Add Clear Button
        _clearButton = _toolkit.createButton(composite, BUTTON_CLEAR, SWT.PUSH | SWT.CENTER);
        _clearButton.setFont(ApplicationRegistry.getFont(FONT_BUTTON));
        GridData gridData = new GridData(SWT.LEAD, SWT.TOP, true, false);
        gridData.widthHint = 80;
        _clearButton.setLayoutData(gridData);
        _clearButton.addSelectionListener(new SelectionAdapter()
        {
            public void widgetSelected(SelectionEvent e)
            {    
                if (_mbean == null)
                {
                    return;
                }

                ServerRegistry serverRegistry = ApplicationRegistry.getServerRegistry(MBeanView.getServer());
                IStructuredSelection ss = (IStructuredSelection)_tableViewer.getSelection();
                if(!ss.isEmpty())
                {
                    //clear selected Notifications
                    serverRegistry.clearNotifications(_mbean, ss.toList());
                }
                else if(_notifications != null)
                {
                    //clear all the notifications, if there are any
                    
                    //check the user is certain of this clear-all operation
                    int response = ViewUtility.popupOkCancelConfirmationMessage(
                            "Clear Notifications", "Clear all Notifications for this MBean?");
                    if(response != SWT.OK)
                    {
                        return;
                    }
                    
                    synchronized(this)
                    {
                        List<NotificationObject> newList = new ArrayList<NotificationObject>();
                        newList.addAll(_notifications);
                        serverRegistry.clearNotifications(_mbean, newList);
                    }
                }
                
                refresh();
            }
        });
        //add description
        Label desc = _toolkit.createLabel(composite,"Clears the selected Notifications, or all if none are selected");
        desc.setLayoutData(new GridData(SWT.LEFT,SWT.CENTER, false, false));
    }

    @Override
    public void refresh(ManagedBean mbean)
    {
        refresh();
    }
    
    /**
     * Fills the notification information widgets for selected mbean
     */
    private void populateNotificationInfo()
    {
        _notificationNameCombo.removeAll();
        NotificationInfoModel[] items = MBeanUtility.getNotificationInfo(_mbean);
        if (items.length > 1)
        {
            _notificationNameCombo.add(SELECT_NOTIFICATIONNAME);
        }
        
        for (int i = 0; i < items.length; i++)
        {
            _notificationNameCombo.add(items[i].getName());
            _notificationNameCombo.setData(items[i].getName(), items[i]);
        } 
        _notificationNameCombo.select(0);
        
        _typesCombo.removeAll();
        _typesCombo.add("Select Type", 0);
        _typesCombo.select(0);
        _typesCombo.setEnabled(false);
        
        populateNotificationType(_notificationNameCombo.getItem(0));
        checkForEnablingButtons();
    }
    
    /**
     * Checks and the enabing/disabling of buttons
     */
    private void checkForEnablingButtons()
    {
        int nameIndex = _notificationNameCombo.getSelectionIndex();
        int itemCount = _notificationNameCombo.getItems().length;
        if ((itemCount > 1) && (nameIndex == 0))
        {
            _subscribeButton.setEnabled(false);
            _unsubscribeButton.setEnabled(false);
            _descriptionLabel.setText("");
            return;
        }
        
        int typeIndex = _typesCombo.getSelectionIndex();
        itemCount = _typesCombo.getItems().length;
        if ((itemCount > 1) && (typeIndex == 0))
        {
            _subscribeButton.setEnabled(false);
            _unsubscribeButton.setEnabled(false);
            return;
        }
        
        String type = _typesCombo.getItem(typeIndex);
        String name = _notificationNameCombo.getItem(nameIndex);
        ServerRegistry serverRegistry = ApplicationRegistry.getServerRegistry(_mbean);
        
        if (serverRegistry.hasSubscribedForNotifications(_mbean, name, type))
        {
            _subscribeButton.setEnabled(false);
            _unsubscribeButton.setEnabled(true);
        }
        else
        {
            _subscribeButton.setEnabled(true);
            _unsubscribeButton.setEnabled(false);
        }
    }
    
    /**
     * Selection listener for subscribing or unsubscribing the notifications
     */
    private class SelectionListenerImpl extends SelectionAdapter
    {
        public void widgetSelected(SelectionEvent e)
        {
            if (_mbean == null)
                return;
            
            Button source = (Button)e.getSource();
            String type = _typesCombo.getItem(_typesCombo.getSelectionIndex());
            String name = _notificationNameCombo.getItem(_notificationNameCombo.getSelectionIndex());
            if (source == _unsubscribeButton)
            {
                try
                {
                    MBeanUtility.removeNotificationListener(_mbean, name, type);
                }
                catch(Exception ex)
                {
                    MBeanUtility.handleException(ex);
                }
            }
            else if (source == _subscribeButton)
            {
                try
                {
                    MBeanUtility.createNotificationlistener(_mbean, name, type);
                }
                catch(Exception ex)
                {
                    MBeanUtility.handleException(ex);
                }
            }
            checkForEnablingButtons();
        }
    }
    
    /**
     * Selection listener class for the Notification Name. The notification type and description will be 
     * displayed accordingly
     */
    private class ComboSelectionListener extends SelectionAdapter
    {
        public void widgetSelected(SelectionEvent e)
        {
            if (_mbean == null)
                return;
            
            Combo combo = (Combo)e.getSource();
            if (combo == _notificationNameCombo)
            {
                String selectedItem = combo.getItem(combo.getSelectionIndex());                
                populateNotificationType(selectedItem);
            }
            checkForEnablingButtons();
        }
    }
    
    private void populateNotificationType(String notificationName)
    {
        NotificationInfoModel data = (NotificationInfoModel)_notificationNameCombo.getData(notificationName);
        if (data == null)
        {
            _descriptionLabel.setText("");
            _typesCombo.select(0);
            _typesCombo.setEnabled(false);
            return;
        }
        _descriptionLabel.setText(data.getDescription());
        _typesCombo.removeAll();       
        _typesCombo.setItems(data.getTypes());
        if (_typesCombo.getItemCount() > 1)
        {
            _typesCombo.add(SELECT_NOTIFICATIONTYPE, 0);
        }
        _typesCombo.select(0);
        _typesCombo.setEnabled(true);
    }
    
    /**
     * Updates the table with new notifications received from mbean server for the selected mbean
     */
    protected void updateTableViewer()
    {
        ServerRegistry serverRegistry = ApplicationRegistry.getServerRegistry(_mbean);        
        List<NotificationObject> newList = serverRegistry.getNotifications(_mbean);
        synchronized(this)
        {
            _notifications = newList;
            _tableViewer.setInput(_notifications);
        }
    }
}
