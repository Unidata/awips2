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
package org.apache.qpid.server.management;

import org.apache.qpid.management.common.mbeans.ConfigurationManagement;
import org.apache.qpid.management.common.mbeans.LoggingManagement;
import org.apache.qpid.management.common.mbeans.UserManagement;
import org.apache.qpid.server.logging.actors.CurrentActor;
import org.apache.qpid.server.logging.actors.ManagementActor;
import org.apache.qpid.server.logging.messages.ManagementConsoleMessages;
import org.apache.log4j.Logger;

import javax.management.remote.MBeanServerForwarder;
import javax.management.remote.JMXPrincipal;
import javax.management.remote.JMXConnectionNotification;
import javax.management.MBeanServer;
import javax.management.ObjectName;
import javax.management.MBeanInfo;
import javax.management.MBeanOperationInfo;
import javax.management.JMException;
import javax.management.NotificationListener;
import javax.management.Notification;
import javax.security.auth.Subject;
import java.lang.reflect.InvocationHandler;
import java.lang.reflect.Proxy;
import java.lang.reflect.Method;
import java.security.AccessController;
import java.security.Principal;
import java.security.AccessControlContext;
import java.util.HashSet;
import java.util.Set;
import java.util.Properties;

/**
 * This class can be used by the JMXConnectorServer as an InvocationHandler for the mbean operations. This implements
 * the logic for allowing the users to invoke MBean operations and implements the restrictions for readOnly, readWrite
 * and admin users.
 */
public class MBeanInvocationHandlerImpl implements InvocationHandler, NotificationListener
{
    private static final Logger _logger = Logger.getLogger(MBeanInvocationHandlerImpl.class);

    public final static String ADMIN = "admin";
    public final static String READWRITE = "readwrite";
    public final static String READONLY = "readonly";
    private final static String DELEGATE = "JMImplementation:type=MBeanServerDelegate";
    private MBeanServer _mbs;
    private static Properties _userRoles = new Properties();
    private static ManagementActor  _logActor;

    private static HashSet<String> _adminOnlyMethods = new HashSet<String>();
    {
        _adminOnlyMethods.add(UserManagement.TYPE); 
        _adminOnlyMethods.add(LoggingManagement.TYPE);
        _adminOnlyMethods.add(ConfigurationManagement.TYPE);
    }
    
    public static MBeanServerForwarder newProxyInstance()
    {
        final InvocationHandler handler = new MBeanInvocationHandlerImpl();
        final Class[] interfaces = new Class[]{MBeanServerForwarder.class};


        _logActor = new ManagementActor(CurrentActor.get().getRootMessageLogger());

        Object proxy = Proxy.newProxyInstance(MBeanServerForwarder.class.getClassLoader(), interfaces, handler);
        return MBeanServerForwarder.class.cast(proxy);
    }

    public Object invoke(Object proxy, Method method, Object[] args) throws Throwable
    {
        final String methodName = method.getName();

        if (methodName.equals("getMBeanServer"))
        {
            return _mbs;
        }

        if (methodName.equals("setMBeanServer"))
        {
            if (args[0] == null)
            {
                throw new IllegalArgumentException("Null MBeanServer");
            }
            if (_mbs != null)
            {
                throw new IllegalArgumentException("MBeanServer object already initialized");
            }
            _mbs = (MBeanServer) args[0];
            return null;
        }

        // Retrieve Subject from current AccessControlContext
        AccessControlContext acc = AccessController.getContext();
        Subject subject = Subject.getSubject(acc);

        // Allow operations performed locally on behalf of the connector server itself
        if (subject == null)
        {
            return method.invoke(_mbs, args);
        }

        if (args == null || DELEGATE.equals(args[0]))
        {
            return method.invoke(_mbs, args);
        }

        // Restrict access to "createMBean" and "unregisterMBean" to any user
        if (methodName.equals("createMBean") || methodName.equals("unregisterMBean"))
        {
            _logger.debug("User trying to create or unregister an MBean");
            throw new SecurityException("Access denied");
        }

        // Retrieve JMXPrincipal from Subject
        Set<JMXPrincipal> principals = subject.getPrincipals(JMXPrincipal.class);
        if (principals == null || principals.isEmpty())
        {
            throw new SecurityException("Access denied");
        }

        Principal principal = principals.iterator().next();
        String identity = principal.getName();

        if (isAdminMethod(args))
        {
            if (isAdmin(identity))
            {
                return method.invoke(_mbs, args);
            }
            else
            {
                throw new SecurityException("Access denied");
            }
        }

        // Following users can perform any operation other than "createMBean" and "unregisterMBean"
        if (isAllowedToModify(identity))
        {
            return method.invoke(_mbs, args);
        }

        // These users can only call "getAttribute" on the MBeanServerDelegate MBean
        // Here we can add other fine grained permissions like specific method for a particular mbean
        if (isReadOnlyUser(identity) && isReadOnlyMethod(method, args))
        {
            return method.invoke(_mbs, args);
        }

        throw new SecurityException("Access denied");
    }

    private boolean isAdminMethod(Object[] args)
    {
        if (args[0] instanceof ObjectName)
        {
            ObjectName object = (ObjectName) args[0];
            
            return _adminOnlyMethods.contains(object.getKeyProperty("type"));
        }
        return false;
    }

    // Initialises the user roles
    public static void setAccessRights(Properties accessRights)
    {
        _userRoles = accessRights;
    }

    private boolean isAdmin(String userName)
    {
        if (ADMIN.equals(_userRoles.getProperty(userName)))
        {
            return true;
        }
        return false;
    }

    private boolean isAllowedToModify(String userName)
    {
        if (ADMIN.equals(_userRoles.getProperty(userName))
            || READWRITE.equals(_userRoles.getProperty(userName)))
        {
            return true;
        }
        return false;
    }

    private boolean isReadOnlyUser(String userName)
    {
        if (READONLY.equals(_userRoles.getProperty(userName)))
        {
            return true;
        }
        return false;
    }

    private boolean isReadOnlyMethod(Method method, Object[] args)
    {
        String methodName = method.getName();
        
        //handle standard get/set/query and select 'is' methods from MBeanServer
        if (methodName.startsWith("query") || methodName.startsWith("get")
            ||methodName.startsWith("isInstanceOf") || methodName.startsWith("isRegistered"))
        {
            return true;
        }
        else if (methodName.startsWith("set"))
        {
            return false;
        }

        //handle invocation of other methods on mbeans
        if ((args[0] instanceof ObjectName) && (methodName.equals("invoke")))
        {

            //get invoked method name
            String mbeanMethod = (args.length > 1) ? (String) args[1] : null;
            if (mbeanMethod == null)
            {
                return false;
            }

            try
            {
                //check if the given method is tagged with an INFO impact attribute
                MBeanInfo mbeanInfo = _mbs.getMBeanInfo((ObjectName) args[0]);
                if (mbeanInfo != null)
                {
                    MBeanOperationInfo[] opInfos = mbeanInfo.getOperations();
                    for (MBeanOperationInfo opInfo : opInfos)
                    {
                        if (opInfo.getName().equals(mbeanMethod) && (opInfo.getImpact() == MBeanOperationInfo.INFO))
                        {
                            return true;
                        }
                    }
                }
            }
            catch (JMException ex)
            {
                ex.printStackTrace();
            }
        }

        return false;
    }

    public void handleNotification(Notification notification, Object handback)
    {
        // only RMI Connections are serviced here, Local API atta  
        // rmi://169.24.29.116 guest 3
        String[] connectionData = ((JMXConnectionNotification) notification).getConnectionId().split(" ");
        String user = connectionData[1];

        if (notification.getType().equals(JMXConnectionNotification.OPENED))
        {
            _logActor.message(ManagementConsoleMessages.MNG_OPEN(user));
        }
        else if (notification.getType().equals(JMXConnectionNotification.CLOSED) ||
                 notification.getType().equals(JMXConnectionNotification.FAILED))
        {
            _logActor.message(ManagementConsoleMessages.MNG_CLOSE());
        }
    }
}

