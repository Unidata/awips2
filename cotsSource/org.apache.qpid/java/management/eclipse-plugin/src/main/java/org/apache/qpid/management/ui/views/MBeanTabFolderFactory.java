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

import static org.apache.qpid.management.ui.Constants.ATTRIBUTES;
import static org.apache.qpid.management.ui.Constants.CONNECTION;
import static org.apache.qpid.management.ui.Constants.EXCHANGE;
import static org.apache.qpid.management.ui.Constants.EXCHANGE_TYPE;
import static org.apache.qpid.management.ui.Constants.NOTIFICATIONS;
import static org.apache.qpid.management.ui.Constants.QUEUE;

import java.util.EnumSet;
import java.util.HashMap;
import java.util.Map;

import javax.management.MBeanServerConnection;

import org.apache.qpid.management.ui.ApplicationRegistry;
import org.apache.qpid.management.ui.ManagedServer;
import org.apache.qpid.management.ui.ServerRegistry;
import org.apache.qpid.management.ui.jmx.JMXManagedObject;
import org.apache.qpid.management.ui.jmx.MBeanUtility;
import org.apache.qpid.management.ui.model.NotificationInfoModel;
import org.apache.qpid.management.ui.model.OperationData;
import org.apache.qpid.management.ui.model.OperationDataModel;
import org.apache.qpid.management.ui.views.queue.QueueOperationsTabControl;
import org.apache.qpid.management.ui.views.type.ConnectionTypeTabControl;
import org.apache.qpid.management.ui.views.type.ExchangeTypeTabControl;
import org.apache.qpid.management.ui.views.type.QueueTypeTabControl;
import org.apache.qpid.management.ui.views.users.UserManagementTabControl;
import org.apache.qpid.management.ui.views.vhost.VHostTabControl;
import org.apache.qpid.management.ui.views.connection.ConnectionOperationsTabControl;
import org.apache.qpid.management.ui.views.exchange.ExchangeOperationsTabControl;
import org.apache.qpid.management.ui.views.exchange.HeadersExchangeOperationsTabControl;
import org.apache.qpid.management.ui.views.logging.ConfigurationFileTabControl;
import org.apache.qpid.management.ui.views.logging.RuntimeTabControl;
import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.FormAttachment;
import org.eclipse.swt.layout.FormData;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Event;
import org.eclipse.swt.widgets.Listener;
import org.eclipse.swt.widgets.TabFolder;
import org.eclipse.swt.widgets.TabItem;

public class MBeanTabFolderFactory
{
    private static final String MBEANTYPE_QUEUE = "VirtualHost.Queue";
    private static final String MBEANTYPE_CONNECTION = "VirtualHost.Connection";
    private static final String MBEANTYPE_EXCHANGE = "VirtualHost.Exchange";
    private static final String MBEANTYPE_VHOST_MANAGER = "VirtualHost.VirtualHostManager";
    private static final String MBEANTYPE_LOGGING_MANAGEMENT = "LoggingManagement";
    private static final String MBEANTYPE_USER_MANAGEMENT = "UserManagement";
    private static final String MBEANTYPE_CONFIGURATION_MANAGEMENT = "ConfigurationManagement";    

    private MBeanTabFolderFactory()
    {
        //no instances
    }

    public static TabFolder generateMBeanTabFolder(final Composite parent, final JMXManagedObject mbean, final MBeanServerConnection mbsc)
    {
        TabFolder tabFolder = new TabFolder(parent, SWT.NONE);
        FormData layoutData = new FormData();
        layoutData.left = new FormAttachment(0);
        layoutData.top = new FormAttachment(0);
        layoutData.right = new FormAttachment(100);
        layoutData.bottom = new FormAttachment(100);
        tabFolder.setLayoutData(layoutData);

        TabItem tab;
        TabControl controller;
        QpidMBeanType mbeanType = QpidMBeanType.get(mbean.getType());

        switch(mbeanType)
        {
            case QUEUE:
                createAttributesTab(tabFolder, mbean);
                
                tab = new TabItem(tabFolder, SWT.NONE);
                tab.setText("Operations");
                controller = new QueueOperationsTabControl(tabFolder, mbean, mbsc);
                tab.setControl(controller.getControl());
                tab.setData(TabControl.CONTROLLER, controller);
                break;
            case CONNECTION:
                createAttributesTab(tabFolder, mbean);
                
                tab = new TabItem(tabFolder, SWT.NONE);
                tab.setText("Operations");
                controller = new ConnectionOperationsTabControl(tabFolder, mbean, mbsc);
                tab.setControl(controller.getControl());
                tab.setData(TabControl.CONTROLLER, controller);
                break;
            case EXCHANGE:
                createAttributesTab(tabFolder, mbean);
                
                if (mbean.getProperty(EXCHANGE_TYPE).equalsIgnoreCase("headers"))
                {
                    tab = new TabItem(tabFolder, SWT.NONE);
                    tab.setText("Operations");
                    controller = new HeadersExchangeOperationsTabControl(tabFolder, mbean, mbsc);
                    tab.setControl(controller.getControl());
                    tab.setData(TabControl.CONTROLLER, controller);
                }
                else
                {
                    tab = new TabItem(tabFolder, SWT.NONE);
                    tab.setText("Operations");
                    controller = new ExchangeOperationsTabControl(tabFolder, mbean, mbsc);
                    tab.setControl(controller.getControl());
                    tab.setData(TabControl.CONTROLLER, controller);
                }
                break;
            case VHOST_MANAGER:
                tab = new TabItem(tabFolder, SWT.NONE);
                tab.setText("Operations");
                controller = new VHostTabControl(tabFolder, mbean, mbsc);
                tab.setControl(controller.getControl());
                tab.setData(TabControl.CONTROLLER, controller);
                break;
            case LOGGING_MANAGEMENT:
                tab = new TabItem(tabFolder, SWT.NONE);
                tab.setText("Runtime Options");
                controller = new RuntimeTabControl(tabFolder, mbean, mbsc);
                tab.setControl(controller.getControl());
                tab.setData(TabControl.CONTROLLER, controller);

                tab = new TabItem(tabFolder, SWT.NONE);
                tab.setText("ConfigurationFile Options");
                controller = new ConfigurationFileTabControl(tabFolder, mbean, mbsc);
                tab.setControl(controller.getControl());
                tab.setData(TabControl.CONTROLLER, controller);
                break;
            case USER_MANAGEMENT:
                tab = new TabItem(tabFolder, SWT.NONE);
                tab.setText("Operations");
                controller = new UserManagementTabControl(tabFolder, mbean, mbsc);
                tab.setControl(controller.getControl());
                tab.setData(TabControl.CONTROLLER, controller);
                break;
            case CONFIGURATION_MANAGEMENT:
                createGenericTabFolder(tabFolder, mbean);
                break;
            case UNKNOWN:
                createGenericTabFolder(tabFolder, mbean);
                break;
        }
        
        createNotificationsTabIfNecessary(tabFolder, mbean);
        
        tabFolder.addListener(SWT.Selection, new Listener()
        {
            public void handleEvent(Event evt)
            {
                TabItem tab = (TabItem)evt.item;        
                TabControl controller = (TabControl)tab.getData(TabControl.CONTROLLER);
                if(controller != null)
                {
                    controller.refresh(mbean);
                }
            }
        });
        
        return tabFolder;
    }
    
    private static void createGenericTabFolder(TabFolder tabFolder, JMXManagedObject mbean)
    {
        createAttributesTab(tabFolder, mbean);
        createOperationTabs(tabFolder, mbean);
    }

    private static void createAttributesTab(TabFolder tabFolder, JMXManagedObject mbean)
    {
        ServerRegistry serverRegistry = ApplicationRegistry.getServerRegistry(mbean);
        if(serverRegistry.getAttributeModel(mbean).getCount() == 0)
        {
            return;
        }
        
        TabItem tab = new TabItem(tabFolder, SWT.NONE);
        tab.setText(ATTRIBUTES);
        AttributesTabControl controller = new AttributesTabControl(tabFolder);
        tab.setControl(controller.getControl());
        tab.setData(TabControl.CONTROLLER, controller);
    }
    
    private static void createOperationTabs(TabFolder tabFolder, JMXManagedObject mbean)
    {
        ServerRegistry serverRegistry = ApplicationRegistry.getServerRegistry(mbean);        
        int operationsCount = serverRegistry.getOperationModel(mbean).getCount();
        if(operationsCount == 0)
        {
            return;
        }
        
        OperationDataModel operationModel = serverRegistry.getOperationModel(mbean);
        for(OperationData operationData : operationModel.getOperations())
        {
            TabItem operationTab = new TabItem(tabFolder, SWT.NONE);
            operationTab.setText(ViewUtility.getDisplayText(operationData.getName()));
            operationTab.setData(operationData);
            OperationTabControl control = new OperationTabControl(tabFolder, operationData);
            operationTab.setData(TabControl.CONTROLLER, control);
            operationTab.setControl(control.getControl());
        }
    }
    
    private static void createNotificationsTabIfNecessary(TabFolder tabFolder, JMXManagedObject mbean)
    {
        NotificationInfoModel[] items = MBeanUtility.getNotificationInfo(mbean);
        if(items == null || items.length == 0)
        {
            //the mbean has no notifications to subscribe for, do not create the tab.
            return;
        }
        
        NotificationsTabControl controller = new NotificationsTabControl(tabFolder, mbean);
        
        TabItem tab = new TabItem(tabFolder, SWT.NONE);
        tab.setText(NOTIFICATIONS);
        tab.setData(TabControl.CONTROLLER, controller);
        tab.setControl(controller.getControl());
    }
    
    /**
     * Creates TabFolder and tabs for all mbeantype (Connection, Queue, and Exchange)
     */
    public static TabFolder generateMBeanTypeTabFolder(final Composite parent, ManagedServer server, String virtualHost)
    {
        TabFolder tabFolder = new TabFolder(parent, SWT.NONE);
        FormData layoutData = new FormData();
        layoutData.left = new FormAttachment(0);
        layoutData.top = new FormAttachment(0);
        layoutData.right = new FormAttachment(100);
        layoutData.bottom = new FormAttachment(100);
        tabFolder.setLayoutData(layoutData);

        
        TabItem tab;
        TabControl controller;
              
        tab = new TabItem(tabFolder, SWT.NONE);
        tab.setText(CONNECTION); 
        controller = new ConnectionTypeTabControl(tabFolder,server,virtualHost);
        tab.setData(TabControl.CONTROLLER, controller);
        tab.setControl(controller.getControl());
        
        tab = new TabItem(tabFolder, SWT.NONE);
        tab.setText(EXCHANGE);      
        controller = new ExchangeTypeTabControl(tabFolder,server,virtualHost);
        tab.setData(TabControl.CONTROLLER, controller);
        tab.setControl(controller.getControl());
        
        tab = new TabItem(tabFolder, SWT.NONE);
        tab.setText(QUEUE);  
        controller = new QueueTypeTabControl(tabFolder,server,virtualHost);
        tab.setData(TabControl.CONTROLLER, controller);
        tab.setControl(controller.getControl());
        
        tabFolder.addListener(SWT.Selection, new Listener()
        {
            public void handleEvent(Event evt)
            {
                TabItem tab = (TabItem)evt.item;        
                TabControl controller = (TabControl)tab.getData(TabControl.CONTROLLER);
                if(controller != null)
                {
                    controller.refresh(null);
                }
            }
        });
        
        return tabFolder;
    }

    /**
     * Creates TabFolder and tab for the Connection selection view
     */
    public static TabFolder generateConnectionTypeTabFolder(final Composite parent, ManagedServer server, String virtualHost)
    {
        TabFolder tabFolder = new TabFolder(parent, SWT.NONE);
        FormData layoutData = new FormData();
        layoutData.left = new FormAttachment(0);
        layoutData.top = new FormAttachment(0);
        layoutData.right = new FormAttachment(100);
        layoutData.bottom = new FormAttachment(100);
        tabFolder.setLayoutData(layoutData);

        TabItem tab;
        TabControl controller;
              
        tab = new TabItem(tabFolder, SWT.NONE);
        tab.setText(CONNECTION); 
        controller = new ConnectionTypeTabControl(tabFolder,server,virtualHost);
        tab.setData(TabControl.CONTROLLER, controller);
        tab.setControl(controller.getControl());
                
        tabFolder.addListener(SWT.Selection, new Listener()
        {
            public void handleEvent(Event evt)
            {
                TabItem tab = (TabItem)evt.item;        
                TabControl controller = (TabControl)tab.getData(TabControl.CONTROLLER);
                if(controller != null)
                {
                    controller.refresh(null);
                }
            }
        });
        
        return tabFolder;
    }
    
    /**
     * Creates TabFolder and tab for the Exchange selection view
     */
    public static TabFolder generateExchangeTypeTabFolder(final Composite parent, ManagedServer server, String virtualHost)
    {
        TabFolder tabFolder = new TabFolder(parent, SWT.NONE);
        FormData layoutData = new FormData();
        layoutData.left = new FormAttachment(0);
        layoutData.top = new FormAttachment(0);
        layoutData.right = new FormAttachment(100);
        layoutData.bottom = new FormAttachment(100);
        tabFolder.setLayoutData(layoutData);

        TabItem tab;
        TabControl controller;

        tab = new TabItem(tabFolder, SWT.NONE);
        tab.setText(EXCHANGE);      
        controller = new ExchangeTypeTabControl(tabFolder,server,virtualHost);
        tab.setData(TabControl.CONTROLLER, controller);
        tab.setControl(controller.getControl());
        
        tabFolder.addListener(SWT.Selection, new Listener()
        {
            public void handleEvent(Event evt)
            {
                TabItem tab = (TabItem)evt.item;        
                TabControl controller = (TabControl)tab.getData(TabControl.CONTROLLER);
                if(controller != null)
                {
                    controller.refresh(null);
                }
            }
        });
        
        return tabFolder;
    }
    
    /**
     * Creates TabFolder and tab for the Queue selection view
     */
    public static TabFolder generateQueueTypeTabFolder(final Composite parent, ManagedServer server, String virtualHost)
    {
        TabFolder tabFolder = new TabFolder(parent, SWT.NONE);
        FormData layoutData = new FormData();
        layoutData.left = new FormAttachment(0);
        layoutData.top = new FormAttachment(0);
        layoutData.right = new FormAttachment(100);
        layoutData.bottom = new FormAttachment(100);
        tabFolder.setLayoutData(layoutData);

        TabItem tab;
        TabControl controller;
        
        tab = new TabItem(tabFolder, SWT.NONE);
        tab.setText(QUEUE);  
        controller = new QueueTypeTabControl(tabFolder,server,virtualHost);
        tab.setData(TabControl.CONTROLLER, controller);
        tab.setControl(controller.getControl());
        
        tabFolder.addListener(SWT.Selection, new Listener()
        {
            public void handleEvent(Event evt)
            {
                TabItem tab = (TabItem)evt.item;        
                TabControl controller = (TabControl)tab.getData(TabControl.CONTROLLER);
                if(controller != null)
                {
                    controller.refresh(null);
                }
            }
        });
        
        return tabFolder;
    }
    
    private enum QpidMBeanType
    {
        QUEUE (MBEANTYPE_QUEUE),
        CONNECTION (MBEANTYPE_CONNECTION),
        EXCHANGE (MBEANTYPE_EXCHANGE),
        VHOST_MANAGER (MBEANTYPE_VHOST_MANAGER),
        LOGGING_MANAGEMENT (MBEANTYPE_LOGGING_MANAGEMENT),
        USER_MANAGEMENT (MBEANTYPE_USER_MANAGEMENT),
        CONFIGURATION_MANAGEMENT (MBEANTYPE_CONFIGURATION_MANAGEMENT),
        UNKNOWN (null);

        private static final Map<String,QpidMBeanType> lookup = new HashMap<String,QpidMBeanType>();

        static
        {
            for(QpidMBeanType m : EnumSet.allOf(QpidMBeanType.class))
            {
                lookup.put(m.getType(), m);
            }
        }

        private String type;
        
        private QpidMBeanType()
        {
            
        }

        private QpidMBeanType(String type)
        {
            this.type = type;
        }

        public String getType()
        {
            return type;
        }

        public static QpidMBeanType get(String type)
        {
            QpidMBeanType t= lookup.get(type);
            if (t != null)
            {
                return t;
            }
            else
            {
                return UNKNOWN;
            }

        }
    }


}
