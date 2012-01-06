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
package org.apache.qpid.management.ui;

import java.io.File;
import java.util.List;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.CopyOnWriteArrayList;

import org.eclipse.jface.resource.FontRegistry;
import org.eclipse.jface.resource.ImageRegistry;
import org.eclipse.swt.SWT;
import org.eclipse.swt.graphics.Font;
import org.eclipse.swt.graphics.FontData;
import org.eclipse.swt.graphics.Image;
import org.eclipse.ui.ISharedImages;
import org.eclipse.ui.PlatformUI;

/**
 * Main Application Registry, which contains shared resources and map to all connected servers.
 * @author Bhupendra Bhardwaj
 */
public abstract class ApplicationRegistry
{
    private static ImageRegistry imageRegistry = new ImageRegistry();
    private static FontRegistry fontRegistry = new FontRegistry();
    public static final boolean debug = Boolean.getBoolean("eclipse.consoleLog");
    public static final long timeout = Long.parseLong(System.getProperty("timeout", "15000"));

    //max supported broker management interface supported by this release of the management console
    public static final int SUPPORTED_QPID_JMX_API_MAJOR_VERSION = 1;
    public static final int SUPPORTED_QPID_JMX_API_MINOR_VERSION = 6;
    
    public static final String DATA_DIR = System.getProperty("user.home") + File.separator + ".qpidmc";
    
    static
    {
        imageRegistry.put(Constants.SUCCESS_IMAGE, 
                org.apache.qpid.management.ui.Activator.getImageDescriptor("/icons/success.gif"));
        imageRegistry.put(Constants.FAILURE_IMAGE, 
                org.apache.qpid.management.ui.Activator.getImageDescriptor("/icons/failure.gif"));
        imageRegistry.put(Constants.CONSOLE_IMAGE, 
                org.apache.qpid.management.ui.Activator.getImageDescriptor("/icons/qpidmc.gif"));
        imageRegistry.put(Constants.CLOSED_FOLDER_IMAGE, 
                org.apache.qpid.management.ui.Activator.getImageDescriptor("/icons/icon_ClosedFolder.gif"));
        imageRegistry.put(Constants.OPEN_FOLDER_IMAGE,
                PlatformUI.getWorkbench().getSharedImages().getImage(ISharedImages.IMG_OBJ_FOLDER));
        imageRegistry.put(Constants.MBEAN_IMAGE,
                PlatformUI.getWorkbench().getSharedImages().getImage(ISharedImages.IMG_OBJ_ELEMENT));
        imageRegistry.put(Constants.NOTIFICATION_IMAGE,
                org.apache.qpid.management.ui.Activator.getImageDescriptor("/icons/notifications.gif"));
        imageRegistry.put(Constants.LOGGING_MANAGEMENT_IMAGE,
                org.apache.qpid.management.ui.Activator.getImageDescriptor("/icons/logging_management.gif"));
        imageRegistry.put(Constants.USER_MANAGEMENT_IMAGE,
                org.apache.qpid.management.ui.Activator.getImageDescriptor("/icons/user_management.gif"));
        imageRegistry.put(Constants.CONFIGURATION_MANAGEMENT_IMAGE,
                org.apache.qpid.management.ui.Activator.getImageDescriptor("/icons/configuration_management.gif"));
        imageRegistry.put(Constants.SERVER_INFO_IMAGE,
                org.apache.qpid.management.ui.Activator.getImageDescriptor("/icons/server_information.gif"));
        imageRegistry.put(Constants.VHOST_MANAGER_IMAGE,
                org.apache.qpid.management.ui.Activator.getImageDescriptor("/icons/virtualhost_manager.gif"));
    }
    
    static
    {
        fontRegistry.put(Constants.FONT_BUTTON, new FontData[]{new FontData("Arial", 8, SWT.BOLD)} );
        fontRegistry.put(Constants.FONT_BOLD, new FontData[]{new FontData("Bold", 9, SWT.BOLD)} );
        fontRegistry.put(Constants.FONT_ITALIC, new FontData[]{new FontData("Italic", 9, SWT.ITALIC)} );
        fontRegistry.put(Constants.FONT_TABLE_CELL, new FontData[]{new FontData("Tablecell", 8, SWT.NORMAL)} );
        fontRegistry.put(Constants.FONT_NORMAL, new FontData[]{new FontData("Normal", 9, SWT.NORMAL)} );
    }
    
    /*
     * This maps all the managed servers to the respective server registry.
     */
    private static ConcurrentHashMap<ManagedServer, ServerRegistry> _serverRegistryMap = new ConcurrentHashMap<ManagedServer, ServerRegistry>();
    
    // This map gets updated when a server connection closes.
    private static List<ManagedServer> _closedServerList = new CopyOnWriteArrayList<ManagedServer>();    
    
    public static Image getImage(String key)
    {
        return imageRegistry.get(key);
    }
    
    public static Font getFont(String key)
    {
        return fontRegistry.get(key);
    }
    
    public static void addServer(ManagedServer server, ServerRegistry registry)
    {
        _serverRegistryMap.put(server, registry);
    }
    
    public static void removeServer(ManagedServer server)
    {
        _serverRegistryMap.remove(server);
    }
    
    public static ServerRegistry getServerRegistry(ManagedServer server)
    {
        return _serverRegistryMap.get(server);
    }
    
    public static ServerRegistry getServerRegistry(ManagedBean mbean)
    {
        ManagedServer server = mbean.getServer();
        return getServerRegistry(server);
    }
    
    public static boolean isServerConnected(ManagedServer server)
    {
        if(server == null)
        {
            //checking for null is not permitted in a CHM
            return false;
        }
        
        ServerRegistry reg = _serverRegistryMap.get(server);
        if(reg !=null)
        {
            return !reg.isServerConnectionClosed();
        }

        return false;
    }
    
    // remove the server from the registry
    public static void serverConnectionClosed(ManagedServer server)
    {
        _closedServerList.add(server);
        removeServer(server);
    }
    
    // remove the server from the registry
    public static void serverConnectionClosedRemotely(ManagedServer server)
    {
        ServerRegistry reg = _serverRegistryMap.get(server);
        if(reg !=null)
        {
            synchronized(server)
            {
                if(reg.isServerConnectionClosed())
                {
                    //the connection closure was already processed
                    return;
                }

                reg.serverConnectionClosed();
            }
        }
        
        serverConnectionClosed(server);
    }
    
    /*
     * Returns the lis of closed servers. The Thread in GUI, which keeps checking for closed connection
     * will check this and will remove the server links from the GUI.
     */
    public static List<ManagedServer> getClosedServers()
    {
        if (_closedServerList.isEmpty())
            return null;
        
        List<ManagedServer> list = new CopyOnWriteArrayList<ManagedServer>(_closedServerList);
        _closedServerList.clear();
        return list;
    }

}
