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
package org.apache.qpid.management.ui.jmx;

import static org.apache.qpid.management.ui.Constants.ALL;

import java.io.IOException;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.concurrent.CopyOnWriteArrayList;

import javax.management.MBeanInfo;
import javax.management.MBeanServerConnection;
import javax.management.Notification;
import javax.management.ObjectName;
import javax.management.remote.JMXConnector;

import org.apache.qpid.management.common.JMXConnnectionFactory;
import org.apache.qpid.management.ui.ApplicationRegistry;
import org.apache.qpid.management.ui.ManagedBean;
import org.apache.qpid.management.ui.ManagedServer;
import org.apache.qpid.management.ui.ServerRegistry;
import org.apache.qpid.management.ui.model.ManagedAttributeModel;
import org.apache.qpid.management.ui.model.NotificationInfoModel;
import org.apache.qpid.management.ui.model.NotificationObject;
import org.apache.qpid.management.ui.model.OperationDataModel;


public class JMXServerRegistry extends ServerRegistry
{
    private ObjectName _serverObjectName = null;
    private JMXConnector _jmxc = null;
    private MBeanServerConnection _mbsc = null;
    private String _securityMechanism = null;
    
    private List<String> _usersList;
    // When an mbean gets removed from mbean server, then the notification listener
    // will add that mbean in this list.
    private List<ManagedBean> _mbeansToBeRemoved = new ArrayList<ManagedBean>();
    
    // Map containing all managed beans and mapped with unique mbean name
    private HashMap<String, ManagedBean>   _mbeansMap    = new HashMap<String, ManagedBean>();
    // Map containing MBeanInfo for all mbeans and mapped with unique mbean name 
    private HashMap<String, MBeanInfo>     _mbeanInfoMap = new HashMap<String, MBeanInfo>();
    // Map containing attribute model for all mbeans and mapped with unique mbean name
    private HashMap<String, ManagedAttributeModel>    _attributeModelMap = new HashMap<String, ManagedAttributeModel>();
    // Map containing operation model for all mbeans and mapped with unique mbean name
    private HashMap<String, OperationDataModel>       _operationModelMap = new HashMap<String, OperationDataModel>();
    // Map containing NotificationInfo for all mbeans and mapped with unique mbean name
    private HashMap<String, List<NotificationInfoModel>> _notificationInfoMap = new HashMap<String, List<NotificationInfoModel>>();
    // Map containing all notifications sent for all mbeans, which are registered for notification
    private HashMap<String, List<NotificationObject>> _notificationsMap  = new HashMap<String, List<NotificationObject>>();
    // For mbeans which have subscribed for a notification type
    // mbean unique name mapped with notification map. Notification map contains list of notification type
    // mapped with notification name. Notification type list contains those notification types,
    // which are subscribed for notification.
    private HashMap<String, HashMap<String, List<String>>> _subscribedNotificationMap = new HashMap<String, HashMap<String, List<String>>>();
    
    // listener for registration or unregistratioj of mbeans on mbean server
    private ClientNotificationListener _notificationListener = null;
    // listener for server connection. Receives notification if server connection goes down
    private ClientListener _clientListener = null;
    
    public JMXServerRegistry(ManagedServer server) throws Exception
    {
        super(server);
        
        _jmxc = JMXConnnectionFactory.getJMXConnection(
                ApplicationRegistry.timeout, server.getHost(),
                server.getPort(), server.getUser(), server.getPassword());
        
        _mbsc = _jmxc.getMBeanServerConnection();

        _clientListener = new ClientListener(server);
        _notificationListener = new ClientNotificationListener(server);

        _jmxc.addConnectionNotificationListener(_clientListener, null, null);
        _serverObjectName = new ObjectName("JMImplementation:type=MBeanServerDelegate");
        _mbsc.addNotificationListener(_serverObjectName, _clientListener, null, null);

    }

    public MBeanServerConnection getServerConnection()
    {
        return _mbsc;
    }


    public String getSecurityMechanism()
    {
        return _securityMechanism;
    }

    /**
     * removes all listeners from the mbean server. This is required when user
     * disconnects the Qpid server connection
     */
    public void closeServerConnection() throws IOException
    {
        if(isServerConnectionClosed())
        {
            //connection was already closed
            return;
        }
        
        try
        {
            //remove the listener from the JMXConnector
            if (_jmxc != null && _clientListener != null)
            {
                _jmxc.removeConnectionNotificationListener(_clientListener);
            }
        }
        catch (Exception e)
        {
            //ignore
        }

        try
        {
            //remove the listener from the MBeanServerDelegate MBean
            if (_mbsc != null && _clientListener != null)
            {
                _mbsc.removeNotificationListener(_serverObjectName, _clientListener);
            }
        }
        catch (Exception e)
        {
            //ignore
        }

        if (_mbsc != null && _clientListener != null)
        {
            //remove any listeners from the Qpid MBeans
            for (String mbeanName : _subscribedNotificationMap.keySet())
            {
                try
                {
                    _mbsc.removeNotificationListener(new ObjectName(mbeanName), _notificationListener);
                }
                catch (Exception e)
                {
                    //ignore
                }
            }
        }

        //close the JMXConnector
        if (_jmxc != null)
        {
            _jmxc.close();
        }
        
        serverConnectionClosed();
    }
    
    public ManagedBean getManagedObject(String uniqueName)
    {
        return _mbeansMap.get(uniqueName);
    }
    
    public void addManagedObject(ManagedBean mbean)
    {
        if (mbean.isQueue())
        {
            addQueueMBean(mbean);
        }
        else if (mbean.isExchange())
        {
            addExchangeMBean(mbean);
        }
        else if (mbean.isConnection())
        {
            addConnectionMBean(mbean);
        }
        else if (mbean.isVirtualHostManager())
        {
            addVirtualHostManagerMBean(mbean);
        }
        
        addVirtualHost(mbean.getVirtualHostName());
        _mbeansMap.put(mbean.getUniqueName(), mbean);
    }

    public void removeManagedObject(ManagedBean mbean)
    {
        if (mbean == null)
        {
            return;
        }
        
        _mbeansMap.remove(mbean.getUniqueName());
        
        if (mbean.isQueue())
        {
            removeQueueMBean(mbean);
        }
        else if (mbean.isExchange())
        {
            removeExchangeMBean(mbean);
        }
        else if (mbean.isConnection())
        {
            removeConnectionMBean(mbean);
        }
        else if (mbean.isVirtualHostManager())
        {
            removeVirtualHostManagerMBean(mbean);
        }
    }
    
    public void putMBeanInfo(ManagedBean mbean, MBeanInfo mbeanInfo)
    {
        _mbeanInfoMap.put(mbean.getUniqueName(), mbeanInfo);
    }    
    public MBeanInfo getMBeanInfo(ManagedBean mbean)
    {
        return _mbeanInfoMap.get(mbean.getUniqueName());
    }
    
    public List<ManagedBean> getMBeans()
    {
        return new ArrayList<ManagedBean>(_mbeansMap.values());
    }
    
    public void setNotificationInfo(ManagedBean mbean, List<NotificationInfoModel>value)
    {
        _notificationInfoMap.put(mbean.getUniqueName(), value);
    }
    
    public List<NotificationInfoModel> getNotificationInfo(ManagedBean mbean)
    {
        return _notificationInfoMap.get(mbean.getUniqueName());
    }
    
    public void addNotification(ObjectName objName, Notification notification)
    {
        List<NotificationObject> list = _notificationsMap.get(objName.toString());
        NotificationObject obj = new NotificationObject(notification.getSequenceNumber(),
                                                        new Date(notification.getTimeStamp()),
                                                        notification.getMessage(),
                                                        notification.getSource(),
                                                        notification.getType());
        
        if (list == null)
        {
            list = new ArrayList<NotificationObject>();
            _notificationsMap.put(objName.toString(), list);
        }
        
        list.add(obj);
    }
    
    /**
     * Returns all the notification objects for a given mbean. If mbean is null, it returns
     * notification objects for all the mbeans.
     */
    public List<NotificationObject> getNotifications(ManagedBean mbean)
    {
        if (mbean == null)
        {
            List<NotificationObject> totalList = new ArrayList<NotificationObject>();
            for (List<NotificationObject> list : _notificationsMap.values())
            {
                totalList.addAll(list);
            }
            return totalList;
        }
        else
        {
            return _notificationsMap.get(mbean.getUniqueName());
        }
    }
    
    public List<NotificationObject> getNotifications(String virtualhost)
    {
        List<NotificationObject> vhostNotificationsList = new ArrayList<NotificationObject>();

        //iterate over all the notification lists for mbeans with subscribed notifications
        for (List<NotificationObject> list : _notificationsMap.values())
        {
            if(list == null || list.isEmpty())
            {
                continue;
            }
            
            //Check the source vhost of the first notification
            NotificationObject notification  = list.get(0);
            
            if (notification != null)
            {
                String sourceVhost = notification.getSourceVirtualHost();
                if(sourceVhost != null)
                {
                    if(sourceVhost.equalsIgnoreCase(virtualhost))
                    {
                        //If it matches, add the entire list as they are from the same vhost (same source mbean)
                        vhostNotificationsList.addAll(list);
                    }
                }
            }
        }

        return vhostNotificationsList;
    }
    
    public void clearNotifications(ManagedBean mbean, List<NotificationObject> list)
    {
        if (mbean == null)
        {
            if (list == null || list.isEmpty())
            {
                // All notifications of all mbeans to be cleared
                _notificationsMap.clear();
            }
            else
            {
                // Clear the selected notifications
                for (NotificationObject obj : list)
                {
                    mbean = _mbeansMap.get(obj.getSource().toString());
                    List<NotificationObject> nList = _notificationsMap.get(mbean.getUniqueName());
                    if (nList != null && !nList.isEmpty())
                    {
                        nList.remove(obj);
                    }
                }
            }
        }
        else 
        {
            if (list == null || list.isEmpty())
            {
                // All notifications of this mbean to be cleared
                List<NotificationObject> nList = _notificationsMap.get(mbean.getUniqueName());
                if (nList != null && !nList.isEmpty())
                {
                    nList.clear();
                }
            }
            else
            {
                // Clear the selected notifications
                for (NotificationObject obj : list)
                {
                    List<NotificationObject> nList = _notificationsMap.get(mbean.getUniqueName());
                    if (nList != null && !nList.isEmpty())
                    {
                        nList.remove(obj);
                    }
                }
            }
        }
    }
    
    
    
    /**
     * Adds notification name and type to the map. The map contains all the notification names,
     * subscribed for an mbean.
     * @param mbean
     * @param name
     * @param type
     */
    public void addNotificationListener(ManagedBean mbean, String name, String type)
    {
        // Get the subscribed notifications map for given mbean. If map is null then create a new one. 
        HashMap<String, List<String>> map = _subscribedNotificationMap.get(mbean.getUniqueName());
        if (map == null)
        {
            map = new HashMap<String, List<String>>();
            _subscribedNotificationMap.put(mbean.getUniqueName(),map);
        }
        
        // Get the list of notification types for given notification name. If null, then create a new list.
        List<String> list = map.get(name);
        if (list == null)
        {
            list = new ArrayList<String>();
            map.put(name, list);
        }
        // Now add the notification type to the list
        if (ALL.equals(type))
        {
            List<NotificationInfoModel> infoList = _notificationInfoMap.get(mbean.getUniqueName());
            for (NotificationInfoModel model : infoList)
            {                
                if (model.getName().equals(name))
                {
                    String[] types = model.getTypes();
                    for (int i = 0; i < types.length; i++)
                    {
                        list.add(types[i]);
                    }
                }
            }
        }
        else
        {
            list.add(type);
        }

        //System.out.println("Subscribed for notification :" + mbean.getUniqueName());
    }
    
    /**
     * Checks if the given notification name and type are subscribed for the mbean.
     */
    public boolean hasSubscribedForNotifications(ManagedBean mbean, String name, String type)
    {
        if (_subscribedNotificationMap.containsKey(mbean.getUniqueName()))
        {
            HashMap<String, List<String>> map = _subscribedNotificationMap.get(mbean.getUniqueName());
            if (map.containsKey(name))
            {
                if (map.get(name).contains(type))
                {
                    return true;
                }
            }
        }
        return false;
    }
    
    /**
     * Clears the notification name and type information from the subscribed notifications map
     * and removes the listener from mbeanserver connection
     * @param mbean
     * @param name
     * @param type
     * @throws Exception
     */
    public void removeNotificationListener(ManagedBean mbean, String name, String type) throws Exception
    {
        //System.out.println("Removed notification listener :" + mbean.getUniqueName() + name +type);
        if (_subscribedNotificationMap.containsKey(mbean.getUniqueName()))
        {            
            // get the notifications map. This map contains the notification name mapped with the notification types
            HashMap<String, List<String>> map = _subscribedNotificationMap.get(mbean.getUniqueName());
            if (map.containsKey(name))
            {
                if (ALL.equals(type))
                {
                    map.remove(name);
                }
                else if (type != null)
                {
                    map.get(name).remove(type);
                    if (map.get(name).isEmpty())
                    {
                        map.remove(name);
                    }
                }
            }
            if (map.size() == 0)
            {
                _subscribedNotificationMap.remove(mbean.getUniqueName());
            }
            
            JMXManagedObject jmxbean = (JMXManagedObject)mbean;
            _mbsc.removeNotificationListener(jmxbean.getObjectName(), _notificationListener);
        }
    }
    
    /**
     * When the mbean registration request is received from the mbean server, then the client listener
     * can use this method.  It will add the mbean to a list, which will be used to add the mbean to
     * the registry and gui
     * @param objName
     */
    public void registerManagedObject(ObjectName objName)
    {
        JMXManagedObject managedObject = new JMXManagedObject(objName);       
        managedObject.setServer(getManagedServer());
        addManagedObject(managedObject);
    }
    
    /**
     * When mbean unregistration notification is received from the mbean server, then client listener
     * can invoke this method. It will add the mbean to the list of mbeans to be removed from registry
     * @param objName
     */
    public void unregisterManagedObject(ObjectName objName)
    {
        ManagedBean mbean = _mbeansMap.get(objName.toString());
        // Check if mbean was not available in the map. It can happen if mbean unregistration
        // notification is received and the mbean is not added in the map.
        if (mbean != null)
        {
            removeManagedObject(mbean);
            _mbeansToBeRemoved.add(mbean);
        }
    }

    public List<ManagedBean> getObjectsToBeRemoved()
    {
        if (_mbeansToBeRemoved.isEmpty())
            return null;
        else
        {
            List<ManagedBean> list = new CopyOnWriteArrayList<ManagedBean>(_mbeansToBeRemoved);
            _mbeansToBeRemoved.clear();
            return list;
        }
    }   
    
    public void setAttributeModel(ManagedBean mbean, ManagedAttributeModel value)
    {
        _attributeModelMap.put(mbean.getUniqueName(), value);
    }
    
    public ManagedAttributeModel getAttributeModel(ManagedBean mbean)
    {
        return _attributeModelMap.get(mbean.getUniqueName());
    }
    
    public void setOperationModel(ManagedBean mbean, OperationDataModel value)
    {
        _operationModelMap.put(mbean.getUniqueName(), value);
    }
    
    public OperationDataModel getOperationModel(ManagedBean mbean)
    {
        return _operationModelMap.get(mbean.getUniqueName());
    }
    
    public List<String> getQueueNames(String virtualHostName)
    {
        List<ManagedBean> list = getQueues(virtualHostName);
        if (list == null)
            return null;
        
        List<String> queueNames = new ArrayList<String>();
        for (ManagedBean mbean : list)
        {
            queueNames.add(mbean.getName());
        }
        return queueNames;
    }
    
    public String[] getExchangeNames(String virtualHostName)
    {
        List<ManagedBean> list = getExchanges(virtualHostName);
        if (list == null)
            return null;
        
        String[] exchanges = new String[list.size()];
        int i = 0;
        for (ManagedBean mbean : list)
        {
            exchanges[i++] = mbean.getName();
        }
        return exchanges;
    }
    
    public String[] getConnectionNames(String virtualHostName)
    {
        List<ManagedBean> list = getExchanges(virtualHostName);
        if (list == null)
            return null;
        
        String[] connections = new String[list.size()];
        int i = 0;
        for (ManagedBean mbean : list)
        {
            connections[i++] = mbean.getName();
        }
        return connections;
    }
    
    public void setUserList(List<String> list)
    {
        _usersList = list;
        Collections.sort(_usersList);
    }
    
    public List<String> getUsernames()
    {
        return _usersList;
    }

    public ClientNotificationListener getNotificationListener()
    {
        return _notificationListener;
    }

    public ClientListener getClientListener()
    {
        return _clientListener;
    }
}
