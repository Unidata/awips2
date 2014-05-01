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


import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.ConcurrentMap;
import java.util.concurrent.atomic.AtomicBoolean;

import org.apache.qpid.management.ui.jmx.ClientListener;
import org.apache.qpid.management.ui.model.ManagedAttributeModel;
import org.apache.qpid.management.ui.model.NotificationObject;
import org.apache.qpid.management.ui.model.OperationDataModel;

public abstract class ServerRegistry
{
    private ManagedServer _managedServer = null;
    
    // API version for the management interface on the broker
    private ApiVersion _managementApiVersion = new ApiVersion(0,0);
    
    // list of virtual hosts for this server
    private List<String> _virtualHosts = new ArrayList<String>();
    // map of all Connection mbeans
    private ConcurrentMap<String,List<ManagedBean>> _connections = new ConcurrentHashMap<String,List<ManagedBean>>();
    // map of all exchange mbeans
    private ConcurrentMap<String,List<ManagedBean>> _exchanges = new ConcurrentHashMap<String,List<ManagedBean>>();
    // map of all queue mbenas
    private ConcurrentMap<String,List<ManagedBean>> _queues = new ConcurrentHashMap<String,List<ManagedBean>>();
    // map of all virtual host manager mbeans
    private ConcurrentMap<String,ManagedBean> _vhostManagers = new ConcurrentHashMap<String,ManagedBean>();
    
    private AtomicBoolean _serverConnectionClosed = new AtomicBoolean(false);
    
    public ServerRegistry()
    {
        
    }
    
    public ServerRegistry(ManagedServer server)
    {
        _managedServer = server;
    }
    
    public void serverConnectionClosed()
    {
        _serverConnectionClosed.set(true);
    }
    
    public boolean isServerConnectionClosed()
    {
        return _serverConnectionClosed.get();
    }
    
    public void setManagementApiVersion(ApiVersion mgmtApiVersion)
    {
        _managementApiVersion = mgmtApiVersion;  
    }
    
    public ApiVersion getManagementApiVersion()
    {
        return _managementApiVersion;  
    }
    
    public ManagedServer getManagedServer()
    {
        return _managedServer;
    }

    public void setManagedServer(ManagedServer server)
    {
        _managedServer = server;
    }
    
    protected void addConnectionMBean(ManagedBean mbean)
    {
        String vHost = mbean.getVirtualHostName();
        _connections.putIfAbsent(vHost, new ArrayList<ManagedBean>());
        _connections.get(vHost).add(mbean);
    }
    
    protected void addExchangeMBean(ManagedBean mbean)
    {
        String vHost = mbean.getVirtualHostName();
        _exchanges.putIfAbsent(vHost, new ArrayList<ManagedBean>());
        _exchanges.get(vHost).add(mbean);
    }
    
    protected void addQueueMBean(ManagedBean mbean)
    {
        String vHost = mbean.getVirtualHostName();
        _queues.putIfAbsent(vHost, new ArrayList<ManagedBean>());
        _queues.get(vHost).add(mbean);
    }
    
    protected void addVirtualHostManagerMBean(ManagedBean mbean)
    {
        String vHost = mbean.getVirtualHostName();
        _vhostManagers.put(vHost, mbean);
    }
    
    protected void removeVirtualHostManagerMBean(ManagedBean mbean)
    {
        _vhostManagers.remove(mbean);
    }
    
    public ManagedBean getVirtualHostManagerMBean(String virtualHost)
    {
        return _vhostManagers.get(virtualHost);
    }
    
    protected void removeConnectionMBean(ManagedBean mbean)
    {
        _connections.get(mbean.getVirtualHostName()).remove(mbean);
    }
    
    protected void removeExchangeMBean(ManagedBean mbean)
    {
        _exchanges.get(mbean.getVirtualHostName()).remove(mbean);
    }
    
    protected void removeQueueMBean(ManagedBean mbean)
    {
        _queues.get(mbean.getVirtualHostName()).remove(mbean);
    }
    
    public List<ManagedBean> getConnections(String virtualHost)
    {
        return _connections.get(virtualHost);
    }
    
    public List<ManagedBean> getExchanges(String virtualHost)
    {
        return _exchanges.get(virtualHost);
    }
    
    public List<ManagedBean> getQueues(String virtualHost)
    {
        return _queues.get(virtualHost);
    }
    
    //returns the requested ManagedBean, or null if it cant be found
    public ManagedBean getQueue(String queueName, String virtualHost)
    {
        ManagedBean requestedQueue = null;
        
        for(ManagedBean queue : _queues.get(virtualHost))
        {
            if(queueName.equals(queue.getName()))
            {
                requestedQueue = queue;
                break;
            }
        }
        
        return requestedQueue;
    }
    
    public void addVirtualHost(String name)
    {
        if (!_virtualHosts.contains(name))
        {
            _virtualHosts.add(name);
        }
    }
    
    public List<String> getVirtualHosts()
    {
        return _virtualHosts;
    }
    
    public abstract void setUserList(List<String> list);
    
    public abstract List<String> getUsernames();
    
    public abstract void addManagedObject(ManagedBean key);
    
    public abstract List<ManagedBean> getMBeans();
    
    public abstract void removeManagedObject(ManagedBean mbean);
   
    public abstract List<ManagedBean> getObjectsToBeRemoved();
    
    public abstract ManagedAttributeModel getAttributeModel(ManagedBean mbean);
    
    public abstract Object getServerConnection();
    
    public abstract void closeServerConnection() throws Exception;
    
    public abstract OperationDataModel getOperationModel(ManagedBean mbean);
    
    public abstract List<String> getQueueNames(String vistualHostName);
    
    public abstract String[] getExchangeNames(String vistualHostName);
    
    public abstract String[] getConnectionNames(String vistualHostName);
    
    public abstract List<NotificationObject> getNotifications(ManagedBean mbean);
    
    public abstract List<NotificationObject> getNotifications(String virtualhost);
    
    public abstract boolean hasSubscribedForNotifications(ManagedBean mbean, String name, String type);
    
    public abstract void clearNotifications(ManagedBean mbean, List<NotificationObject> list);
    
    public ClientListener getNotificationListener()
    {
        return null;
    }

    public ClientListener getClientListener()
    {
        return null;
    }
}
