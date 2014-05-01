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

import java.io.IOException;
import java.math.BigInteger;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Set;

import javax.management.Attribute;
import javax.management.AttributeList;
import javax.management.InstanceNotFoundException;
import javax.management.JMException;
import javax.management.MBeanAttributeInfo;
import javax.management.MBeanException;
import javax.management.MBeanInfo;
import javax.management.MBeanNotificationInfo;
import javax.management.MBeanOperationInfo;
import javax.management.MBeanServerConnection;
import javax.management.MBeanServerInvocationHandler;
import javax.management.MalformedObjectNameException;
import javax.management.ObjectInstance;
import javax.management.ObjectName;
import javax.management.ReflectionException;

import org.apache.qpid.management.common.mbeans.ServerInformation;
import org.apache.qpid.management.ui.ApiVersion;
import org.apache.qpid.management.ui.ApplicationRegistry;
import org.apache.qpid.management.ui.ManagedBean;
import org.apache.qpid.management.ui.ManagedServer;
import org.apache.qpid.management.ui.exceptions.ManagementConsoleException;
import org.apache.qpid.management.ui.model.AttributeData;
import org.apache.qpid.management.ui.model.ManagedAttributeModel;
import org.apache.qpid.management.ui.model.NotificationInfoModel;
import org.apache.qpid.management.ui.model.OperationData;
import org.apache.qpid.management.ui.model.OperationDataModel;
import org.apache.qpid.management.ui.model.ParameterData;
import org.apache.qpid.management.ui.views.ViewUtility;

/**
 * Utility class for all mbeanserver related operations. Keeps all JMX code out from view and model classes
 * @author Bhupendra Bhardwaj
 */
public class MBeanUtility
{
    public static final BigInteger MAX_LONG = BigInteger.valueOf(Long.MAX_VALUE);
    public static final BigInteger MAX_INT = BigInteger.valueOf(Integer.MAX_VALUE);
    /**
     * Retrieves the MBeanInfo from MBeanServer and stores in the application registry
     * @param mbean  managed bean
     * @return MBeanInfo
     * @throws Exception, if server connection is null or if server throws Exception
     */
    public static MBeanInfo getMBeanInfo(ManagedBean mbean) throws Exception
    {
        ManagedServer server = mbean.getServer();
        JMXServerRegistry serverRegistry = (JMXServerRegistry)ApplicationRegistry.getServerRegistry(server);

        MBeanServerConnection mbsc = serverRegistry.getServerConnection();
        if (mbsc == null)
        {
            throw new ManagementConsoleException("Server connection is broken");
        }
        
        JMXManagedObject jmxbean = (JMXManagedObject)mbean;
        MBeanInfo mbeanInfo = mbsc.getMBeanInfo(jmxbean.getObjectName());
        serverRegistry.putMBeanInfo(mbean, mbeanInfo);
        
        // populate the server registry with attribute and operation info
        getAttributes(mbean);
        getOperations(mbean);
        
        return mbeanInfo;
    }
    
    /**
     * executes the MBean operation
     * @param mbean
     * @param opData
     * @return MBean operation return value
     * @throws Exception if server connection is broken or if operation execution fails on the mbean server
     */
    public static Object execute(ManagedBean mbean, OperationData opData) throws Exception
    {
        String opName = opData.getName();
        Object[] values = null;
        String[] signature = null;
        
        List<ParameterData> params = opData.getParameters();
        if (params != null && !params.isEmpty())
        {
            signature = new String[params.size()];;
            values = new Object[params.size()];
            for (int i = 0; i < params.size(); i++)
            {
                signature[i] = params.get(i).getType();
                values[i] = params.get(i).getValue();
            }
        }
        
        ManagedServer server = mbean.getServer();
        JMXServerRegistry serverRegistry = (JMXServerRegistry)ApplicationRegistry.getServerRegistry(server);

        MBeanServerConnection mbsc = serverRegistry.getServerConnection();
        if (mbsc == null)
        {
            throw new ManagementConsoleException("Server connection is broken");
            // TODO
            // try and get the connection again if it was disconnected
        }
        JMXManagedObject jmxbean = (JMXManagedObject)mbean;
        return mbsc.invoke(jmxbean.getObjectName(), opName, values, signature);
    }
    
    /**
     * @see MBeanUtility#handleException(ManagedBean, Exception)
     */
    public static void handleException(Exception ex)
    {
        handleException(null, ex);
    }
    
    /**
     * handels the exception received. Shows the exception to the user in best suitable way
     * @param mbean managed bean
     * @param ex   Exception
     */
    public static void handleException(ManagedBean mbean, Throwable ex)
    {
        if (mbean == null)
        {
            ViewUtility.popupErrorMessage("Error", "Managed Object is null \n" + ex.toString());
            printStackTrace(ex);
        }
        else if (ex instanceof ReflectionException)
        {
            ViewUtility.popupErrorMessage(mbean.getInstanceName(), "Server has thrown error \n" + ex.toString());
            printStackTrace(ex);
        }
        else if (ex instanceof InstanceNotFoundException)
        {
            ViewUtility.popupErrorMessage(mbean.getInstanceName(), "Managed Object Not Found \n" + ex.toString());
            printStackTrace(ex);
        }
        else if (ex instanceof MBeanException)
        {
            String cause = ((MBeanException)ex).getTargetException().getMessage();
            if (cause == null)
                cause = ex.toString();
            ViewUtility.popupInfoMessage(mbean.getInstanceName(), cause);
        }
        else if (ex instanceof JMException)
        {
            ViewUtility.popupErrorMessage(mbean.getInstanceName(), "Management Exception occured \n" + ex.toString());
        }
        else if (ex instanceof ManagementConsoleException)
        {
            ViewUtility.popupErrorMessage(mbean.getInstanceName(), ex.getMessage());
        }
        else if (ex instanceof SecurityException)
        {
            ViewUtility.popupErrorMessage(mbean.getInstanceName(), ex.getMessage());
        }
        else 
        {
            if (ex.getCause() != null)
            {
                handleException(mbean, ex.getCause());
            }
            else
            {
                String msg = ex.getMessage();
                if (msg == null)
                {
                    msg = ex.toString();
                }
                ViewUtility.popupErrorMessage(mbean.getInstanceName(), msg);
                printStackTrace(ex);
            }
        }
        
    }
    
    /**
     * Registers the notification listener with the MBeanServer
     * @param mbean   managed bean
     * @param name    notification name
     * @param type    notification type
     * @throws Exception  if server connection is broken or if listener could not be created 
     */
    public static void createNotificationlistener(ManagedBean mbean, String name, String type)
        throws Exception
    {
        JMXManagedObject jmxbean = (JMXManagedObject)mbean;
        JMXServerRegistry serverRegistry = (JMXServerRegistry)ApplicationRegistry.getServerRegistry(mbean);
        serverRegistry.addNotificationListener(mbean, name, type);
        MBeanServerConnection mbsc = serverRegistry.getServerConnection();
        
        if (mbsc == null)
        {
            throw new ManagementConsoleException("Server connection is broken");
        }
        mbsc.addNotificationListener(jmxbean.getObjectName(), serverRegistry.getNotificationListener(), null, null);
    }
    
    public static void removeNotificationListener(ManagedBean mbean, String name, String type) throws Exception
    {
        JMXServerRegistry serverRegistry = (JMXServerRegistry)ApplicationRegistry.getServerRegistry(mbean);
        serverRegistry.removeNotificationListener(mbean, name, type);
    }
    
    /**
     * Checks if the server registry contains attribute information for this mbean. If not then it queries the
     * mbean server for complete mbean information, else it gets the latest value of the given attribute
     * from mbean server.
     * @return attribute data for the given mbean attribute
     */
    public static AttributeData getAttributeData(ManagedBean mbean, String attribute) throws Exception
    {
        JMXServerRegistry serverRegistry = (JMXServerRegistry)ApplicationRegistry.getServerRegistry(mbean);
        ManagedAttributeModel attributeModel = serverRegistry.getAttributeModel(mbean);
        if (attributeModel == null)
        {
            // If process is here, it means the mbeanInfo is not retrieved from mbean server even once for this mbean
            getMBeanInfo(mbean);
        }
        else
        {
            // refresh attribute value from mbean server
            refreshAttribute(mbean, attribute);
        }
        attributeModel = serverRegistry.getAttributeModel(mbean);
        return attributeModel.getAttribute(attribute);
    }
    
    /**
     * Retrieves the latest attribute value from mbean server for the given mbean attribute
     * and also sets that value in the attribute model in the server registry
     * @return latest attribute value for the given mbean attribute
     */
    public static Object refreshAttribute(ManagedBean mbean, String attribute) throws Exception
    {
        JMXServerRegistry serverRegistry = (JMXServerRegistry)ApplicationRegistry.getServerRegistry(mbean);
        MBeanServerConnection mbsc = serverRegistry.getServerConnection();
        
        if (mbsc == null)
        {
            throw new ManagementConsoleException("Server connection is broken");
        }
        
        Object value = mbsc.getAttribute(((JMXManagedObject)mbean).getObjectName(), attribute);
        // update the attribute data in server registry for this attribute
        ManagedAttributeModel attributeModel = serverRegistry.getAttributeModel(mbean);
        attributeModel.setAttributeValue(attribute, value);
        return value;
    }
    

    /**
     * Returns a List of Object arrays containing the requested attribute values (in the same sequence requested) for each queue in the virtualhost.
     * If a particular attribute cant be found or raises an mbean/reflection exception whilst being gathered its value is substituted with the String "-".
     */
    public static List<List<Object>> getQueueAttributes(List<ManagedBean> mbeans, String[] attributes)
    {
        List<List<Object>> results = new ArrayList<List<Object>>();
        
        MBeanServerConnection mbsc = null;
        if(mbeans.isEmpty())
        {
            return results;
        }
        else
        {
            ManagedBean mbean = mbeans.get(0);
            JMXServerRegistry serverRegistry = (JMXServerRegistry)ApplicationRegistry.getServerRegistry(mbean);
            mbsc = serverRegistry.getServerConnection();
        }
        
        if(mbsc == null)
        {
            return results;
        }
        
        for(ManagedBean mbean : mbeans)
        {
            HashMap<String,Object> tempResults = new HashMap<String,Object>();
            
            ObjectName objName = ((JMXManagedObject)mbean).getObjectName();
            try
            {
                AttributeList list = mbsc.getAttributes(objName, attributes);
                
                for (Attribute attr : list.toArray(new Attribute[0]))
                {
                    tempResults.put(attr.getName(), attr.getValue());
                }
                
                List<Object> attributeValues = new ArrayList<Object>(attributes.length);
                
                for(int i = 0; i <attributes.length; i++)
                {
                    if(tempResults.containsKey(attributes[i]))
                    {
                        attributeValues.add(tempResults.get(attributes[i]));
                    }
                    else
                    {
                        attributeValues.add(new String("-"));
                    }
                }
                
                results.add(attributeValues);
            }
            catch (Exception ignore)
            {
                continue;
            }
        }
        
        return results;
    }
    
    
    /**
     * Retrieves the attribute values from MBeanSever and stores in the server registry.
     * @param mbean
     * @return the attribute model
     * @throws Exception if attributes can not be retrieved from MBeanServer
     */
    public static ManagedAttributeModel getAttributes(ManagedBean mbean) throws Exception
    {
        ObjectName objName = ((JMXManagedObject)mbean).getObjectName();
        String[] attributes = null;
        AttributeList list = null;
        
        JMXServerRegistry serverRegistry = (JMXServerRegistry)ApplicationRegistry.getServerRegistry(mbean);
        MBeanServerConnection mbsc = serverRegistry.getServerConnection();
        MBeanAttributeInfo[] attributesInfo = null;
        ManagedAttributeModel attributeModel = serverRegistry.getAttributeModel(mbean);
        
        if (attributeModel == null)
        {
            // If the process is here, then it means the attribute values are not retrieved from mbean server
            // even once for this mbean. Create attribute model, retrieve values from mbean server and 
            // set the attribute model in server registry for this mbean
            attributeModel = new ManagedAttributeModel();
            attributesInfo = serverRegistry.getMBeanInfo(mbean).getAttributes();
            attributes = new String[attributesInfo.length];
            for (int i = 0; i< attributesInfo.length ; i++)
            {
                attributes[i] = attributesInfo[i].getName();
                attributeModel.setAttributeDescription(attributes[i], attributesInfo[i].getDescription());
                attributeModel.setAttributeWritable(attributes[i], attributesInfo[i].isWritable());
                attributeModel.setAttributeReadable(attributes[i], attributesInfo[i].isReadable());
            }
        }  
        else
        {
            attributes = attributeModel.getAttributeNames().toArray(new String[0]);
        }
        
        if (attributes.length != 0)
        {
            list = mbsc.getAttributes(objName, attributes);
            for (Iterator itr = list.iterator(); itr.hasNext();)
            {
                Attribute attrib = (Attribute)itr.next();
                attributeModel.setAttributeValue(attrib.getName(), attrib.getValue());
            }
        }               
        
        serverRegistry.setAttributeModel(mbean, attributeModel);       
        return attributeModel;
    }
    
    /**
     * Updates the attribute value of an MBean
     * @param mbean
     * @param attribute
     * @param value
     * @throws Exception if MBeanServer throws exception in updating the attribute value
     */
    public static void updateAttribute(ManagedBean mbean, AttributeData attribute, String value) throws Exception
    {
        JMXManagedObject jmxbean = (JMXManagedObject)mbean;
        JMXServerRegistry serverRegistry = (JMXServerRegistry)ApplicationRegistry.getServerRegistry(mbean);

        MBeanServerConnection mbsc = serverRegistry.getServerConnection();
        
        Object newValue = value;
        if (attribute.getDataType().equals(Long.class.getName()))
        {
            if (MAX_LONG.compareTo(new BigInteger(value)) == -1)
            {
                throw new ManagementConsoleException("Entered value is too big for \"" +
                                                     ViewUtility.getDisplayText(attribute.getName()) + "\"");
            }
            newValue = new Long(Long.parseLong(value));
        }
        else if (attribute.getDataType().equals(Integer.class.getName()))
        {
            if (MAX_INT.compareTo(new BigInteger(value)) == -1)
            {
                throw new ManagementConsoleException("Entered value is too big for " + attribute.getName());
            }
            newValue = new Integer(Integer.parseInt(value));
        }
        
        mbsc.setAttribute(jmxbean.getObjectName(), new Attribute(attribute.getName(), newValue));           
        // Update the value in the registry, to avoid refreshing from mbsc
        ManagedAttributeModel attributeModel = serverRegistry.getAttributeModel(mbean);
        attributeModel.setAttributeValue(attribute.getName(), newValue);
    }
    
    /**
     * populates the operation data model in server registry for given mbean
     * @param mbean
     * @return operation data model
     */
    public static OperationDataModel getOperations(ManagedBean mbean)
    {
        JMXServerRegistry serverRegistry = (JMXServerRegistry)ApplicationRegistry.getServerRegistry(mbean);
        OperationDataModel dataModel = serverRegistry.getOperationModel(mbean);
        if (dataModel == null)
        {
            // Create operation model and set it in server registry for this mbean
            MBeanOperationInfo[] operationsInfo = serverRegistry.getMBeanInfo(mbean).getOperations();
            dataModel = new OperationDataModel();
            
            for (int i = 0; i < operationsInfo.length; i++)
            {
                dataModel.addOperation(operationsInfo[i]);
            }
            
            serverRegistry.setOperationModel(mbean, dataModel);
        }
        return dataModel;
    }
    
    /**
     * populates the notification in the server registry for given mbean
     * @param mbean
     * @return notification info model
     */
    public static NotificationInfoModel[] getNotificationInfo(ManagedBean mbean)
    {
        JMXServerRegistry serverRegistry = (JMXServerRegistry)ApplicationRegistry.getServerRegistry(mbean);
        MBeanNotificationInfo[] info = serverRegistry.getMBeanInfo(mbean).getNotifications();
        
        // Check if this mbean sends any notification
        if (info == null || info.length == 0)
            return null;
        
        // Create notification model if not already set in the server registry for this mbean
        List<NotificationInfoModel> list = serverRegistry.getNotificationInfo(mbean);
        if (list != null) 
            return list.toArray(new NotificationInfoModel[0]);
        else
            list = new ArrayList<NotificationInfoModel>();
        
        for (int i = 0; i < info.length; i++)
        {
            list.add(new NotificationInfoModel(info[i].getName(), info[i].getDescription(), info[i].getNotifTypes()));
        }
        
        // Set the notification model in the server registry for this mbean
        serverRegistry.setNotificationInfo(mbean, list);
        return list.toArray(new NotificationInfoModel[0]);
    }
    
    /**
     * Retrieves all the MBeans from mbean server for a given domain
     * @return list of ManagedBeans
     */
    public static List<ManagedBean> getManagedObjectsForDomain(ManagedServer server, String domain) throws Exception
    {
        List<ManagedBean> mbeans = new ArrayList<ManagedBean>();
        JMXServerRegistry serverRegistry = (JMXServerRegistry)ApplicationRegistry.getServerRegistry(server);
        MBeanServerConnection mbsc = serverRegistry.getServerConnection();
        ObjectName objName = new ObjectName(domain + ":*"); 
        Set objectInstances = mbsc.queryMBeans(objName, null);

        for (Iterator itr = objectInstances.iterator(); itr.hasNext();)
        {
            ObjectInstance instance = (ObjectInstance)itr.next();
            ManagedBean obj = new JMXManagedObject(instance.getObjectName());
            mbeans.add(obj);
        }
        
        return mbeans;
    }
    
    /**
     * Classifies the management API version of the given server
     * @return list of ManagedBeans
     * @throws NullPointerException 
     * @throws ManagementConsoleException
     * @throws MalformedObjectNameException 
     * @throws IOException 
     */
    public static void classifyManagementApiVersion(ManagedServer server, JMXServerRegistry serverRegistry) 
         throws MalformedObjectNameException, NullPointerException, IOException
    {
        MBeanServerConnection mbsc = serverRegistry.getServerConnection();
        
        //Detect if the ServerInformation MBean is present, and use it to retrieve the API version.
        ObjectName objName = new ObjectName(server.getDomain() + ":type="+ ServerInformation.TYPE + ",*");
        Set<ObjectName> objectInstances = mbsc.queryNames(objName, null);
        
        if(objectInstances.size() != 0)
        {
            for (Iterator<ObjectName> itr = objectInstances.iterator(); itr.hasNext();)
            {
                ObjectName instance = (ObjectName)itr.next();
                ServerInformation simb = (ServerInformation)
                                         MBeanServerInvocationHandler.newProxyInstance(mbsc, 
                                                     instance, ServerInformation.class, false);

                    int major = simb.getManagementApiMajorVersion();
                    int minor = simb.getManagementApiMinorVersion();
                    
                    serverRegistry.setManagementApiVersion(new ApiVersion(major, minor));
            }
            
            return;
        }
        
        //ServerInformation mbean was not present, so this is a older pre-v1.3 API server.
        
        //Detect the value of the 'version' key property on the UserManagement MBean ObjectName.
        //If present, we have a v1.2 API server. If null, we have a v1.1 API server.
        //Use an ObjectName pattern (the ?) to match the 'type' and allow this to work for non-admin users
        objName = new ObjectName(server.getDomain() + ":type="+ "UserManagemen?" + ",*");
        objectInstances = mbsc.queryNames(objName, null);
        
        if(objectInstances.size() != 0)
        {
            for (Iterator<ObjectName> itr = objectInstances.iterator(); itr.hasNext();)
            {
                ObjectName instance = (ObjectName)itr.next();
                String version = instance.getKeyProperty("version");
                
                if(version != null)
                {
                    serverRegistry.setManagementApiVersion(new ApiVersion(1, 2));
                }
                else
                {
                    serverRegistry.setManagementApiVersion(new ApiVersion(1, 1));
                }
            }
        }
        else
        {
            //UserManagement MBean wasnt present, connected to an old server: classify as v1.0 API
            serverRegistry.setManagementApiVersion(new ApiVersion(1, 0));
        }
    }
    
    public static void printOutput(String statement)
    {
        if (ApplicationRegistry.debug)
        {
            System.out.println(statement);
        }
    }
    
    public static void printStackTrace(Throwable ex)
    {
        if (ApplicationRegistry.debug)
        {
            ex.printStackTrace();
        }
    }
}
