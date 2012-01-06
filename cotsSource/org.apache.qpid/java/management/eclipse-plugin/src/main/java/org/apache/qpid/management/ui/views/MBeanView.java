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

import java.util.LinkedList;

import javax.management.MBeanServerConnection;

import static org.apache.qpid.management.ui.Constants.*;

import org.apache.qpid.management.ui.ApiVersion;
import org.apache.qpid.management.ui.ApplicationRegistry;
import org.apache.qpid.management.ui.ManagedBean;
import org.apache.qpid.management.ui.ManagedServer;
import org.apache.qpid.management.ui.ServerRegistry;
import org.apache.qpid.management.ui.actions.BackAction;
import org.apache.qpid.management.ui.jmx.JMXManagedObject;
import org.apache.qpid.management.ui.jmx.JMXServerRegistry;
import org.apache.qpid.management.ui.jmx.MBeanUtility;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.swt.SWT;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.layout.FormAttachment;
import org.eclipse.swt.layout.FormData;
import org.eclipse.swt.layout.FormLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.TabFolder;
import org.eclipse.swt.widgets.TabItem;
import org.eclipse.ui.IActionBars;
import org.eclipse.ui.ISelectionListener;
import org.eclipse.ui.IWorkbenchPart;
import org.eclipse.ui.forms.widgets.Form;
import org.eclipse.ui.forms.widgets.FormToolkit;
import org.eclipse.ui.part.ViewPart;

/**
 * MBean View create appropriate view based on the user selection on the Navigation View.
 */
public class MBeanView extends ViewPart
{
    public static final String ID = "org.apache.qpid.management.ui.mbeanView";
    
    private FormToolkit  _toolkit = null;
    private Form _form = null;
    private String _formText = APPLICATION_NAME;
    private static ManagedServer _server = null;
    private TreeObject _selectedNode = null;
    private ManagedBean _mbean = null;
    private static String _virtualHostName = null;
    private static MBeanServerConnection _mbsc = null;
    private TabFolder _tabFolder = null;
    private ISelectionListener _selectionListener = new SelectionListenerImpl();

    // TabFolder to list all the mbeans for a given mbeantype(eg Connection, Queue, Exchange)
    private TabFolder _typeTabFolder = null;
    
    private TabFolder _notificationTabFolder = null;

    private LinkedList<Object> _backHistory;
    private BackAction _backAction;
    
    /*
     * Listener for the selection events in the navigation view
     */ 
    private class SelectionListenerImpl implements ISelectionListener
    {
        public void selectionChanged(IWorkbenchPart part, ISelection sel)
        {
            if (!(sel instanceof IStructuredSelection))
                return;

            IStructuredSelection ss = (IStructuredSelection) sel;
            _selectedNode = (TreeObject)ss.getFirstElement();
            
            
            // mbean should be set to null. A selection done on the navigation view can be either an mbean or
            // an mbeantype. For mbeantype selection(eg Connection, Queue, Exchange) _mbean will remain null.
            _mbean = null;
            clearView();
            
            //clear the back history, it is only for use when opening subsequent mbeans not in the nav tree
            _backHistory.clear();
            _backAction.setEnabled(false);
            
            // If a selected node(mbean) gets unregistered from mbean server, mbeanview should 
            // make the tabfolber for that mbean invisible
            if (_selectedNode == null)
            {
                return;
            }
            
            setServer();
            
            if(!ApplicationRegistry.isServerConnected(_server))
            {
                return;
            }
            
            if (MBEAN.equals(_selectedNode.getType()))
            {
                _mbean = (ManagedBean)_selectedNode.getManagedObject();
            }
            
            setFormTitle();                
            showRelevantTabView();
        }
    }
    
    public void openMBean(ManagedBean mbean)
    {
        openMBean(mbean, false);
    }
    
    private void openMBean(ManagedBean mbean, boolean undoing)
    {
        if(mbean == null)
        {
            return;
        }
        
        //if an mbean is about to be opened (but not returning to using back) from the mbean view,
        //then record the current viewed area/object as a means of back history
        if(!undoing)
        {
            if(_backHistory.isEmpty())
            {
                //ensure the button is enabled if this is to be the first history item
                _backAction.setEnabled(true);
            }
            
            if(_mbean == null)
            {
                //queue etc selection area is open, record the tree object
                _backHistory.addLast(_selectedNode);
            }
            else
            {
                _backHistory.addLast(_mbean); 
            }
        }
        
        _mbean = mbean;
        
        try
        {
            clearView();
            
            setFormTitle();
            showMBean(mbean);
            
            _form.layout(true);
            _form.getBody().layout(true, true);
        }
        catch(Exception ex)
        {
            MBeanUtility.handleException(mbean, ex);
        }
    }
    
    private void setFormTitle()
    {
        if (_mbean != null)
        {
            _formText = _mbean.getType();
            if ((_mbean.getVirtualHostName() != null) && (!DEFAULT_VH.equals(_mbean.getVirtualHostName())) )
            {
                _formText = _formText.replaceFirst(VIRTUAL_HOST, _mbean.getVirtualHostName());
                if (_mbean.getName() != null && _mbean.getName().length() != 0)
                {
                    _formText = _formText + ": " + _mbean.getName();
                }
            }
        }
        else if ((_selectedNode.getVirtualHost() != null) && (!DEFAULT_VH.equals(_selectedNode.getVirtualHost())))
        {
            _formText = _selectedNode.getVirtualHost();
        }
        else
        {
            _formText = APPLICATION_NAME;
        }
        _form.setText(_formText);
    }
    
    public void showRelevantTabView()
    {
        try
        {
            if (_selectedNode == null)
            {
                return;
            }
            
            String mbeanType = _selectedNode.getType();
            
            if (NODE_TYPE_TYPEINSTANCE.equals(mbeanType))
            {
                // An virtual host instance is selected
                generateTypeTabFolder();
            }
            else if (NODE_TYPE_MBEANTYPE.equals(mbeanType))
            {
                showTypeTabFolder(_selectedNode.getName());
            } 
            else if (NOTIFICATIONS.equals(mbeanType))
            {
                refreshNotificationPage();
            }
            else if (MBEAN.equals(mbeanType))
            {
                showMBean(_mbean);
            }
            else if(NODE_TYPE_SERVER.equals(mbeanType))
            {
                ServerRegistry serverReg = ApplicationRegistry.getServerRegistry(_server);
                
                //check the server is connected
                if(serverReg != null)
                {
                    //post a message if the server supports a newer API version.
                    ApiVersion serverAPI = serverReg.getManagementApiVersion();
                    int supportedMajor = ApplicationRegistry.SUPPORTED_QPID_JMX_API_MAJOR_VERSION;
                    int supportedMinor = ApplicationRegistry.SUPPORTED_QPID_JMX_API_MINOR_VERSION;
                    
                    if(serverAPI.greaterThan(supportedMajor, supportedMinor))
                    {
                        _form.setText("The server supports an updated management API and may offer " +
                        		"functionality not available with this console. " +
                        		"Please check for an updated console release.");
                    }
                    
                }
            }
            else
            {
                return;
            }
            
            _form.layout(true);
            _form.getBody().layout(true, true);
        }
        catch(Exception ex)
        {
            MBeanUtility.handleException(_mbean, ex);
        }
    }

    /**
     * Sets the managedServer based on the selection in the navigation view
     * At any given time MBeanView will be displaying information for an mbean of mbeantype
     * for a specifiv managed server. This server information will be used by the tab controllers
     * to get server registry.
     */
    private void setServer()
    {
        if (NODE_TYPE_SERVER.equals(_selectedNode.getType()))
        {
            _server = (ManagedServer)_selectedNode.getManagedObject();
            _virtualHostName = null;
        }
        else
        {
            TreeObject parent = _selectedNode.getParent();
            while (parent != null && !parent.getType().equals(NODE_TYPE_SERVER))
            {
                parent = parent.getParent();
            }
            
            if (parent != null && parent.getType().equals(NODE_TYPE_SERVER))
                _server = (ManagedServer)parent.getManagedObject();
            
            _virtualHostName = _selectedNode.getVirtualHost();
        }
        
        JMXServerRegistry serverRegistry = (JMXServerRegistry)ApplicationRegistry.getServerRegistry(_server);
        if(serverRegistry != null){
            _mbsc = serverRegistry.getServerConnection();
        }
    }
    
    public static ManagedServer getServer()
    {
        return _server;
    }
    
    public static String getVirtualHost()
    {
        return _virtualHostName;
    }
    
    private void showMBean(ManagedBean mbean) throws Exception
    {           
        try
        {                
            MBeanUtility.getMBeanInfo(mbean);     
        }
        catch(Exception ex)
        {
            MBeanUtility.handleException(mbean, ex);
            return;
        }

        if (_tabFolder != null && !_tabFolder.isDisposed())
        {
            _tabFolder.dispose();
        }
        
        _tabFolder = MBeanTabFolderFactory.generateMBeanTabFolder(_form.getBody(),(JMXManagedObject)mbean,_mbsc);
        
        int tabIndex = 0;
        if (NOTIFICATIONS.equals(_selectedNode.getType()))
        {
            tabIndex = _tabFolder.getItemCount() -1;
        }
       
        TabItem tab = _tabFolder.getItem(tabIndex);
        // If folder is being set as visible after tab refresh, then the tab 
        // doesn't have the focus.                  
        _tabFolder.setSelection(tabIndex);
        refreshTab(tab);
    }
    
    public void createPartControl(Composite parent)
    {
        // Create the Form
        _toolkit = new FormToolkit(parent.getDisplay());
        _form = _toolkit.createForm(parent);
        _form.getBody().setLayout(new FormLayout());
        _form.setText(APPLICATION_NAME);
        
        // Add selection listener for selection events in the Navigation view
        getSite().getPage().addSelectionListener(NavigationView.ID, _selectionListener); 

        createNotificationsTabFolder();
        
        ViewUtility.setMBeanView(this);
        
        _backAction = new BackAction();
        getViewSite().getActionBars().getToolBarManager().add(_backAction);
        _backAction.setEnabled(false);
        _backHistory = new LinkedList<Object>();
    }
    
    private void refreshTab(TabItem tab)
    {
        if (tab == null)
        {
            return;
        }
        
        TabControl controller = (TabControl)tab.getData(TabControl.CONTROLLER);
        if(controller != null)
        {
            controller.refresh(_mbean);
        }
    }
    
    public void setFocus()
    {   
        //_form.setFocus();
    }

    public void dispose()
    {
        _toolkit.dispose();
        super.dispose();
    }
    
    
    private void createNotificationsTabFolder()
    {
        _notificationTabFolder = new TabFolder(_form.getBody(), SWT.NONE);
        FormData layoutData = new FormData();
        layoutData.left = new FormAttachment(0);
        layoutData.top = new FormAttachment(0);
        layoutData.right = new FormAttachment(100);
        layoutData.bottom = new FormAttachment(100);
        _notificationTabFolder.setLayoutData(layoutData);
        _notificationTabFolder.setVisible(false);
        
        VHNotificationsTabControl controller = new VHNotificationsTabControl(_notificationTabFolder);       
        TabItem tab = new TabItem(_notificationTabFolder, SWT.NONE);
        tab.setText(NOTIFICATIONS);
        tab.setData(TabControl.CONTROLLER, controller);
        tab.setControl(controller.getControl());
    }
    
    private void refreshNotificationPage()
    {        
        TabItem tab = _notificationTabFolder.getItem(0);
        VHNotificationsTabControl controller = (VHNotificationsTabControl)tab.getData(TabControl.CONTROLLER);
        controller.refresh();
        _notificationTabFolder.setVisible(true);
    }
    


    private void generateTypeTabFolder() throws Exception
    {
        if (_typeTabFolder != null && !_typeTabFolder.isDisposed())
        {
            _typeTabFolder.dispose();
        }
        
        //Generates the full Queue/Connection/Exchange selection tab set
        _typeTabFolder = MBeanTabFolderFactory.generateMBeanTypeTabFolder(
                                            _form.getBody(), getServer(), getVirtualHost());
        refreshTab(_typeTabFolder.getItem(0));
    }
    
    private void showTypeTabFolder(String type) throws Exception
    {
        if (_typeTabFolder != null && !_typeTabFolder.isDisposed())
        {
            _typeTabFolder.dispose();
        }
        
        if (CONNECTION.equals(type))
        {
            //Generates the Connection selection tab
            _typeTabFolder = MBeanTabFolderFactory.generateConnectionTypeTabFolder(
                    _form.getBody(), getServer(), getVirtualHost());
            refreshTab(_typeTabFolder.getItem(0));
        }
        else if (EXCHANGE.equals(type))
        {
            //Generates the Exchange selection tab
            _typeTabFolder = MBeanTabFolderFactory.generateExchangeTypeTabFolder(
                    _form.getBody(), getServer(), getVirtualHost());
            refreshTab(_typeTabFolder.getItem(0));
        }
        else if (QUEUE.equals(type))
        {
            //Generates the Queue selection tab
            _typeTabFolder = MBeanTabFolderFactory.generateQueueTypeTabFolder(
                    _form.getBody(), getServer(), getVirtualHost());
            refreshTab(_typeTabFolder.getItem(0));
        }
    }

    private void clearView()
    {
        if (_tabFolder != null && !_tabFolder.isDisposed())
        {
            _tabFolder.setVisible(false);
        }
        
        if (_typeTabFolder != null && !_typeTabFolder.isDisposed())
        {
            _typeTabFolder.setVisible(false);
        }
        
        if (_notificationTabFolder != null && !_notificationTabFolder.isDisposed())
        {
            _notificationTabFolder.setVisible(false);
        }
        
        _form.setText(APPLICATION_NAME);
        populateStatusBar("");
    }
    
    public void mbeanUnregistered(ManagedBean mbean)
    {
        //if the mbean is actually open, clear the view and empty the Back history
        if(mbean == _mbean)
        {
            clearView();
            _backHistory.clear();
            _backAction.setEnabled(false);
            ViewUtility.popupInfoMessage("MBean Unregistered", 
                    "The open MBean was unregistered from the server.");
        }
    }
    
    public void refresh()
    {
        if(!ApplicationRegistry.isServerConnected(_server))
        {
            return;
        }
        
        if (_tabFolder != null && !_tabFolder.isDisposed())
        {
            if(_tabFolder.getVisible())
            {
                int selectedTab = _tabFolder.getSelectionIndex();
                TabItem tab = _tabFolder.getItem(selectedTab);
                TabControl controller = (TabControl) tab.getData(TabControl.CONTROLLER);
                if(controller != null)
                {
                    controller.refresh(_mbean);
                }
                return;
            }
        }
        
        if (_typeTabFolder != null && !_typeTabFolder.isDisposed())
        {
            
            if(_typeTabFolder.getVisible())
            {
                int selectedTab = _typeTabFolder.getSelectionIndex();
                TabItem tab = _typeTabFolder.getItem(selectedTab);
                TabControl controller = (TabControl) tab.getData(TabControl.CONTROLLER);
                if(controller != null)
                {
                    controller.refresh(_mbean);
                }
                return;
            }
        }
        
        if (_notificationTabFolder != null && !_notificationTabFolder.isDisposed())
        {
            if(_notificationTabFolder.getVisible())
            {
                int selectedTab = _notificationTabFolder.getSelectionIndex();
                TabItem tab = _notificationTabFolder.getItem(selectedTab);
                TabControl controller = (TabControl) tab.getData(TabControl.CONTROLLER);
                if(controller != null)
                {
                    controller.refresh(_mbean);
                }
                return;
            }
        }
    }
    
    public void populateStatusBar(Image icon, String message)
    {
        IActionBars bars = getViewSite().getActionBars();
        bars.getStatusLineManager().setMessage(icon, message);
    }
    
    public void populateStatusBar(String message)
    {
        IActionBars bars = getViewSite().getActionBars();
        bars.getStatusLineManager().setMessage(message);
    }

    public void back() throws Exception
    {
        if(_backHistory.isEmpty())
        {
            return;
        }
        
        Object previous = _backHistory.removeLast();
        if(_backHistory.isEmpty())
        {
            //if this is the last history item, disable the action button
            _backAction.setEnabled(false);
        }
        
        if(previous instanceof ManagedBean)
        {
            openMBean((ManagedBean)previous, true);
        }
        else if (previous instanceof TreeObject)
        {
            _mbean = null;
            clearView();
            setFormTitle();
            
            TreeObject node = (TreeObject) previous;
            String mbeanType = node.getType();
            
            if (NODE_TYPE_TYPEINSTANCE.equals(mbeanType))
            {
                generateTypeTabFolder();
            }
            else if (NODE_TYPE_MBEANTYPE.equals(mbeanType))
            {
                showTypeTabFolder(node.getName());
            }
        }
        
        _form.layout(true);
        _form.getBody().layout(true, true);
    }
}
