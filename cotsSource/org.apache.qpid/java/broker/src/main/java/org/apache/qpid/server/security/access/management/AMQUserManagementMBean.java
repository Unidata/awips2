/*
 *  Licensed to the Apache Software Foundation (ASF) under one
 *  or more contributor license agreements.  See the NOTICE file
 *  distributed with this work for additional information
 *  regarding copyright ownership.  The ASF licenses this file
 *  to you under the Apache License, Version 2.0 (the
 *  "License"); you may not use this file except in compliance
 *  with the License.  You may obtain a copy of the License at
 *
 *    http://www.apache.org/licenses/LICENSE-2.0
 *
 *  Unless required by applicable law or agreed to in writing,
 *  software distributed under the License is distributed on an
 *  "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
 *  KIND, either express or implied.  See the License for the
 *  specific language governing permissions and limitations
 *  under the License.    
 *
 * 
 */
package org.apache.qpid.server.security.access.management;

import org.apache.qpid.management.common.mbeans.UserManagement;
import org.apache.qpid.management.common.mbeans.annotations.MBeanDescription;
import org.apache.qpid.management.common.mbeans.annotations.MBeanOperation;
import org.apache.qpid.server.management.AMQManagedObject;
import org.apache.qpid.server.management.MBeanInvocationHandlerImpl;
import org.apache.qpid.server.security.auth.database.PrincipalDatabase;
import org.apache.qpid.server.security.auth.sasl.UsernamePrincipal;
import org.apache.qpid.util.FileUtils;
import org.apache.log4j.Logger;
import org.apache.commons.configuration.ConfigurationException;

import javax.management.JMException;
import javax.management.remote.JMXPrincipal;
import javax.management.openmbean.TabularData;
import javax.management.openmbean.TabularDataSupport;
import javax.management.openmbean.TabularType;
import javax.management.openmbean.SimpleType;
import javax.management.openmbean.CompositeType;
import javax.management.openmbean.OpenType;
import javax.management.openmbean.OpenDataException;
import javax.management.openmbean.CompositeData;
import javax.management.openmbean.CompositeDataSupport;
import javax.security.auth.login.AccountNotFoundException;
import javax.security.auth.Subject;
import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.FileOutputStream;
import java.util.Properties;
import java.util.List;
import java.util.Enumeration;
import java.util.Random;
import java.util.Set;
import java.util.concurrent.locks.ReentrantLock;
import java.security.Principal;
import java.security.AccessControlContext;
import java.security.AccessController;

/** MBean class for AMQUserManagementMBean. It implements all the management features exposed for managing users. */
@MBeanDescription("User Management Interface")
public class AMQUserManagementMBean extends AMQManagedObject implements UserManagement
{

    private static final Logger _logger = Logger.getLogger(AMQUserManagementMBean.class);

    private PrincipalDatabase _principalDatabase;
    private Properties _accessRights;
    private File _accessFile;

    private ReentrantLock _accessRightsUpdate = new ReentrantLock();

    // Setup for the TabularType
    static TabularType _userlistDataType; // Datatype for representing User Lists
    static CompositeType _userDataType; // Composite type for representing User

    static
    {
        OpenType[] userItemTypes = new OpenType[4]; // User item types.
        userItemTypes[0] = SimpleType.STRING;  // For Username
        userItemTypes[1] = SimpleType.BOOLEAN; // For Rights - Read
        userItemTypes[2] = SimpleType.BOOLEAN; // For Rights - Write
        userItemTypes[3] = SimpleType.BOOLEAN; // For Rights - Admin

        try
        {
            _userDataType =
                    new CompositeType("User", "User Data", COMPOSITE_ITEM_NAMES, COMPOSITE_ITEM_DESCRIPTIONS, userItemTypes);

            _userlistDataType = new TabularType("Users", "List of users", _userDataType, TABULAR_UNIQUE_INDEX);
        }
        catch (OpenDataException e)
        {
            _logger.error("Tabular data setup for viewing users incorrect.");
            _userlistDataType = null;
        }
    }


    public AMQUserManagementMBean() throws JMException
    {
        super(UserManagement.class, UserManagement.TYPE, UserManagement.VERSION);
    }

    public String getObjectInstanceName()
    {
        return UserManagement.TYPE;
    }

    public boolean setPassword(String username, char[] password)
    {
        try
        {
            //delegate password changes to the Principal Database
            return _principalDatabase.updatePassword(new UsernamePrincipal(username), password);
        }
        catch (AccountNotFoundException e)
        {
            _logger.warn("Attempt to set password of non-existant user'" + username + "'");
            return false;
        }
    }

    public boolean setRights(String username, boolean read, boolean write, boolean admin)
    {

        Object oldRights = null;
        if ((oldRights =_accessRights.get(username)) == null)
        {
            // If the user doesn't exist in the access rights file check that they at least have an account.
            if (_principalDatabase.getUser(username) == null)
            {
                return false;
            }
        }

        try
        {
            _accessRightsUpdate.lock();

            // Update the access rights
            if (admin)
            {
                _accessRights.put(username, MBeanInvocationHandlerImpl.ADMIN);
            }
            else
            {
                if (read | write)
                {
                    if (read)
                    {
                        _accessRights.put(username, MBeanInvocationHandlerImpl.READONLY);
                    }
                    if (write)
                    {
                        _accessRights.put(username, MBeanInvocationHandlerImpl.READWRITE);
                    }
                }
                else
                {
                    _accessRights.remove(username);
                }
            }
            
            //save the rights file
            try
            {
                saveAccessFile();
            }
            catch (IOException e)
            {
                _logger.warn("Problem occured saving '" + _accessFile + "', the access right changes will not be preserved: " + e);

                //the rights file was not successfully saved, restore user rights to previous value
                _logger.warn("Reverting attempted rights update for user'" + username + "'");
                if (oldRights != null)
                {
                    _accessRights.put(username, oldRights);
                }
                else
                {
                    _accessRights.remove(username);
                }

                return false;
            }
        }
        finally
        {
            if (_accessRightsUpdate.isHeldByCurrentThread())
            {
                _accessRightsUpdate.unlock();
            }
        }

        return true;
    }

    public boolean createUser(String username, char[] password, boolean read, boolean write, boolean admin)
    {
        if (_principalDatabase.createPrincipal(new UsernamePrincipal(username), password))
        {
            if (!setRights(username, read, write, admin))
            {
                //unable to set rights for user, remove account
                try
                {
                    _principalDatabase.deletePrincipal(new UsernamePrincipal(username));
                }
                catch (AccountNotFoundException e)
                {
                    //ignore
                }
                return false;
            }
            else
            {
                return true;
            }
        }

        return false;
    }

    public boolean deleteUser(String username)
    {
        try
        {
            if (_principalDatabase.deletePrincipal(new UsernamePrincipal(username)))
            {
                try
                {
                    _accessRightsUpdate.lock();

                    _accessRights.remove(username);
                    
                    try
                    {
                        saveAccessFile();
                    }
                    catch (IOException e)
                    {
                        _logger.warn("Problem occured saving '" + _accessFile + "', the access right changes will not be preserved: " + e);
                        return false;
                    }
                }
                finally
                {
                    if (_accessRightsUpdate.isHeldByCurrentThread())
                    {
                        _accessRightsUpdate.unlock();
                    }
                }
            }
        }
        catch (AccountNotFoundException e)
        {
            _logger.warn("Attempt to delete user (" + username + ") that doesn't exist");
            return false;
        }

        return true;
    }

    public boolean reloadData()
    {
            try
            {
                loadAccessFile();
                _principalDatabase.reload();
            }
            catch (ConfigurationException e)
            {
                _logger.warn("Reload failed due to:" + e);
                return false;
            }
            catch (IOException e)
            {
                _logger.warn("Reload failed due to:" + e);
                return false;
            }
            // Reload successful
            return true;
    }


    @MBeanOperation(name = "viewUsers", description = "All users with access rights to the system.")
    public TabularData viewUsers()
    {
        // Table of users
        // Username(string), Access rights Read,Write,Admin(bool,bool,bool)

        if (_userlistDataType == null)
        {
            _logger.warn("TabluarData not setup correctly");
            return null;
        }

        List<Principal> users = _principalDatabase.getUsers();

        TabularDataSupport userList = new TabularDataSupport(_userlistDataType);

        try
        {
            // Create the tabular list of message header contents
            for (Principal user : users)
            {
                // Create header attributes list

                String rights = (String) _accessRights.get(user.getName());

                Boolean read = false;
                Boolean write = false;
                Boolean admin = false;

                if (rights != null)
                {
                    read = rights.equals(MBeanInvocationHandlerImpl.READONLY)
                           || rights.equals(MBeanInvocationHandlerImpl.READWRITE);
                    write = rights.equals(MBeanInvocationHandlerImpl.READWRITE);
                    admin = rights.equals(MBeanInvocationHandlerImpl.ADMIN);
                }

                Object[] itemData = {user.getName(), read, write, admin};
                CompositeData messageData = new CompositeDataSupport(_userDataType, COMPOSITE_ITEM_NAMES, itemData);
                userList.put(messageData);
            }
        }
        catch (OpenDataException e)
        {
            _logger.warn("Unable to create user list due to :" + e);
            return null;
        }

        return userList;
    }

    /*** Broker Methods **/

    /**
     * setPrincipalDatabase
     *
     * @param database set The Database to use for user lookup
     */
    public void setPrincipalDatabase(PrincipalDatabase database)
    {
        _principalDatabase = database;
    }

    /**
     * setAccessFile
     *
     * @param accessFile the file to use for updating.
     *
     * @throws java.io.IOException If the file cannot be accessed
     * @throws org.apache.commons.configuration.ConfigurationException
     *                             if checks on the file fail.
     */
    public void setAccessFile(String accessFile) throws IOException, ConfigurationException
    {
        if (accessFile != null)
        {
            _accessFile = new File(accessFile);
            if (!_accessFile.exists())
            {
                throw new ConfigurationException("'" + _accessFile + "' does not exist");
            }

            if (!_accessFile.canRead())
            {
                throw new ConfigurationException("Cannot read '" + _accessFile + "'.");
            }

            if (!_accessFile.canWrite())
            {
                _logger.warn("Unable to write to access rights file '" + _accessFile + "', changes will not be preserved.");
            }

            loadAccessFile();
        }
        else
        {
            _logger.warn("Access rights file specified is null. Access rights not changed.");
        }
    }

    private void loadAccessFile() throws IOException, ConfigurationException
    {
        if(_accessFile == null)
        {
            _logger.error("No jmx access rights file has been specified.");
            return;
        }
        
        if(_accessFile.exists())
        {
            try
            {
                _accessRightsUpdate.lock();

                Properties accessRights = new Properties();
                accessRights.load(new FileInputStream(_accessFile));
                checkAccessRights(accessRights);
                setAccessRights(accessRights);
            }
            finally
            {
                if (_accessRightsUpdate.isHeldByCurrentThread())
                {
                    _accessRightsUpdate.unlock();
                }
            }
        }
        else
        {
            _logger.error("Specified jmxaccess rights file '" + _accessFile + "' does not exist.");
        }
    }

    private void checkAccessRights(Properties accessRights)
    {
        Enumeration values = accessRights.propertyNames();

        while (values.hasMoreElements())
        {
            String user = (String) values.nextElement();

            if (_principalDatabase.getUser(user) == null)
            {
                _logger.warn("Access rights contains user '" + user + "' but there is no authentication data for that user");
            }
        }
    }

    private void saveAccessFile() throws IOException
    {
        try
        {
            _accessRightsUpdate.lock();

            // Create temporary file
            Random r = new Random();
            File tmp;
            do
            {
                tmp = new File(_accessFile.getPath() + r.nextInt() + ".tmp");
            }
            while(tmp.exists());
            
            tmp.deleteOnExit();

            FileOutputStream output = new FileOutputStream(tmp);
            _accessRights.store(output, "Generated by AMQUserManagementMBean Console : Last edited by user:" + getCurrentJMXUser());
            output.close();

            // Swap temp file to main rights file.
            File old = new File(_accessFile.getAbsoluteFile() + ".old");
            if (old.exists())
            {
                old.delete();
            }
            
            if(!_accessFile.renameTo(old))
            {
                //unable to rename the existing file to the backup name 
                _logger.error("Could not backup the existing management rights file");
                throw new IOException("Could not backup the existing management rights file");
            }

            if(!tmp.renameTo(_accessFile))
            {
                //failed to rename the new file to the required filename
                
                if(!old.renameTo(_accessFile))
                {
                    //unable to return the backup to required filename
                    _logger.error("Could not rename the new management rights file into place, and unable to restore original file");
                    throw new IOException("Could not rename the new management rights file into place, and unable to restore original file");
                }
                
                _logger.error("Could not rename the new management rights file into place");
                throw new IOException("Could not rename the new management rights file into place");
            }
        }
        finally
        {
            if (_accessRightsUpdate.isHeldByCurrentThread())
            {
                _accessRightsUpdate.unlock();
            }
        }
        
    }

    private String getCurrentJMXUser()
    {
        AccessControlContext acc = AccessController.getContext();
        
        Subject subject = Subject.getSubject(acc);
        if (subject == null)
        {
            return "Unknown user, authentication Subject was null";
        }

        // Retrieve JMXPrincipal from Subject
        Set<JMXPrincipal> principals = subject.getPrincipals(JMXPrincipal.class);
        if (principals == null || principals.isEmpty())
        {
            return "Unknown user principals were null";
        }

        Principal principal = principals.iterator().next();
        return principal.getName();
    }

    /**
     * user=read user=write user=readwrite user=admin
     *
     * @param accessRights The properties list of access rights to process
     */
    private void setAccessRights(Properties accessRights)
    {
        _logger.debug("Setting Access Rights:" + accessRights);
        _accessRights = accessRights;
        MBeanInvocationHandlerImpl.setAccessRights(_accessRights);
    }
}
