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
package org.apache.qpid.server.security.auth.database;

import org.apache.log4j.Logger;
import org.apache.qpid.server.security.access.management.AMQUserManagementMBean;
import org.apache.qpid.server.security.auth.sasl.AuthenticationProviderInitialiser;
import org.apache.qpid.server.security.auth.sasl.UsernamePrincipal;
import org.apache.qpid.server.security.auth.sasl.crammd5.CRAMMD5HexInitialiser;
import org.apache.qpid.server.security.auth.sasl.crammd5.CRAMMD5HashedInitialiser;
import org.apache.qpid.util.FileUtils;

import javax.security.auth.callback.PasswordCallback;
import javax.security.auth.login.AccountNotFoundException;
import java.io.BufferedReader;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.IOException;
import java.io.PrintStream;
import java.security.Principal;
import java.util.HashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Random;
import java.util.concurrent.locks.ReentrantLock;
import java.util.regex.Pattern;

/**
 * Represents a user database where the account information is stored in a simple flat file.
 *
 * The file is expected to be in the form: username:password username1:password1 ... usernamen:passwordn
 *
 * where a carriage return separates each username/password pair. Passwords are assumed to be in plain text.
 */
public class Base64MD5PasswordFilePrincipalDatabase implements PrincipalDatabase
{
    private static final Logger _logger = Logger.getLogger(Base64MD5PasswordFilePrincipalDatabase.class);

    private File _passwordFile;

    private Pattern _regexp = Pattern.compile(":");

    private Map<String, AuthenticationProviderInitialiser> _saslServers;

    AMQUserManagementMBean _mbean;
    public static final String DEFAULT_ENCODING = "utf-8";
    private Map<String, HashedUser> _users = new HashMap<String, HashedUser>();
    private ReentrantLock _userUpdate = new ReentrantLock();

    public Base64MD5PasswordFilePrincipalDatabase()
    {
        _saslServers = new HashMap<String, AuthenticationProviderInitialiser>();

        /**
         *  Create Authenticators for MD5 Password file.
         */

        // Accept Plain incomming and hash it for comparison to the file.
        CRAMMD5HashedInitialiser cram = new CRAMMD5HashedInitialiser();
        cram.initialise(this);
        _saslServers.put(cram.getMechanismName(), cram);

        //Add the Hex initialiser
        CRAMMD5HexInitialiser cramHex = new CRAMMD5HexInitialiser();
        cramHex.initialise(this);
        _saslServers.put(cramHex.getMechanismName(), cramHex);

        //fixme The PDs should setup a PD Mangement MBean
//        try
//        {
//            _mbean = new AMQUserManagementMBean();
//            _mbean.setPrincipalDatabase(this);
//        }
//        catch (JMException e)
//        {
//            _logger.warn("User management disabled as unable to create MBean:" + e);
//        }
    }

    public void setPasswordFile(String passwordFile) throws IOException
    {
        File f = new File(passwordFile);
        _logger.info("PasswordFilePrincipalDatabase using file " + f.getAbsolutePath());
        _passwordFile = f;
        if (!f.exists())
        {
            throw new FileNotFoundException("Cannot find password file " + f);
        }
        if (!f.canRead())
        {
            throw new FileNotFoundException("Cannot read password file " + f +
                                            ". Check permissions.");
        }

        loadPasswordFile();
    }

    /**
     * SASL Callback Mechanism - sets the Password in the PasswordCallback based on the value in the PasswordFile
     * If you want to change the password for a user, use updatePassword instead.
     *
     * @param principal The Principal to set the password for
     * @param callback  The PasswordCallback to call setPassword on
     *
     * @throws AccountNotFoundException If the Principal cannont be found in this Database
     */
    public void setPassword(Principal principal, PasswordCallback callback) throws AccountNotFoundException
    {
        if (_passwordFile == null)
        {
            throw new AccountNotFoundException("Unable to locate principal since no password file was specified during initialisation");
        }
        if (principal == null)
        {
            throw new IllegalArgumentException("principal must not be null");
        }

        char[] pwd = lookupPassword(principal.getName());

        if (pwd != null)
        {
            callback.setPassword(pwd);
        }
        else
        {
            throw new AccountNotFoundException("No account found for principal " + principal);
        }
    }

    /**
     * Used to verify that the presented Password is correct. Currently only used by Management Console
     *
     * @param principal The principal to authenticate
     * @param password  The password to check
     *
     * @return true if password is correct
     *
     * @throws AccountNotFoundException if the principal cannot be found
     */
    public boolean verifyPassword(String principal, char[] password) throws AccountNotFoundException
    {
        char[] pwd = lookupPassword(principal);
        
        if (pwd == null)
        {
            throw new AccountNotFoundException("Unable to lookup the specfied users password");
        }
        
        byte[] byteArray = new byte[password.length];
        int index = 0;
        for (char c : password)
        {
            byteArray[index++] = (byte) c;
        }
        
        byte[] MD5byteArray;
        try
        {
            MD5byteArray = HashedUser.getMD5(byteArray);
        }
        catch (Exception e1)
        {
            _logger.warn("Unable to hash password for user '" + principal + "' for comparison");
            return false;
        }
        
        char[] hashedPassword = new char[MD5byteArray.length];

        index = 0;
        for (byte c : MD5byteArray)
        {
            hashedPassword[index++] = (char) c;
        }

        return compareCharArray(pwd, hashedPassword);
    }
    
    private boolean compareCharArray(char[] a, char[] b)
    {
        boolean equal = false;
        if (a.length == b.length)
        {
            equal = true;
            int index = 0;
            while (equal && index < a.length)
            {
                equal = a[index] == b[index];
                index++;
            }
        }
        return equal;
    }

    /**
     * Changes the password for the specified user
     * 
     * @param principal to change the password for
     * @param password plaintext password to set the password too
     */
    public boolean updatePassword(Principal principal, char[] password) throws AccountNotFoundException
    {
        HashedUser user = _users.get(principal.getName());

        if (user == null)
        {
            throw new AccountNotFoundException(principal.getName());
        }

        try
        {
            try
            {
                _userUpdate.lock();
                char[] orig = user.getPassword();
                user.setPassword(password,false);

                try
                {
                    savePasswordFile();
                }
                catch (IOException e)
                {
                    _logger.error("Unable to save password file, password change for user'"
                                  + principal + "' will revert at restart");
                    //revert the password change
                    user.setPassword(orig,true);
                    return false;
                }
                return true;
            }
            finally
            {
                if (_userUpdate.isHeldByCurrentThread())
                {
                    _userUpdate.unlock();
                }
            }
        }
        catch (Exception e)
        {
            return false;
        }
    }

    public boolean createPrincipal(Principal principal, char[] password)
    {
        if (_users.get(principal.getName()) != null)
        {
            return false;
        }

        HashedUser user;
        try
        {
            user = new HashedUser(principal.getName(), password);
        }
        catch (Exception e1)
        {
            _logger.warn("Unable to create new user '" + principal.getName() + "'");
            return false;
        }


        try
        {
            _userUpdate.lock();
            _users.put(user.getName(), user);

            try
            {
                savePasswordFile();
                return true;
            }
            catch (IOException e)
            {
                //remove the use on failure.
                _users.remove(user.getName());
                return false;
            }
        }
        finally
        {
            if (_userUpdate.isHeldByCurrentThread())
            {
                _userUpdate.unlock();
            }
        }
    }

    public boolean deletePrincipal(Principal principal) throws AccountNotFoundException
    {
        HashedUser user = _users.get(principal.getName());

        if (user == null)
        {
            throw new AccountNotFoundException(principal.getName());
        }

        try
        {
            _userUpdate.lock();
            user.delete();

            try
            {
                savePasswordFile();
            }
            catch (IOException e)
            {
                _logger.warn("Unable to remove user '" + user.getName() + "' from password file.");
                return false;
            }

            _users.remove(user.getName());
        }
        finally
        {
            if (_userUpdate.isHeldByCurrentThread())
            {
                _userUpdate.unlock();
            }
        }

        return true;
    }

    public Map<String, AuthenticationProviderInitialiser> getMechanisms()
    {
        return _saslServers;
    }

    public List<Principal> getUsers()
    {
        return new LinkedList<Principal>(_users.values());
    }

    public Principal getUser(String username)
    {
        if (_users.containsKey(username))
        {
            return new UsernamePrincipal(username);
        }
        return null;
    }

    /**
     * Looks up the password for a specified user in the password file. Note this code is <b>not</b> secure since it
     * creates strings of passwords. It should be modified to create only char arrays which get nulled out.
     *
     * @param name The principal name to lookup
     *
     * @return a char[] for use in SASL.
     */
    private char[] lookupPassword(String name)
    {
        HashedUser user = _users.get(name);
        if (user == null)
        {
            return null;
        }
        else
        {
            return user.getPassword();
        }
    }

    private void loadPasswordFile() throws IOException
    {
        try
        {
            _userUpdate.lock();
            _users.clear();

            BufferedReader reader = null;
            try
            {
                reader = new BufferedReader(new FileReader(_passwordFile));
                String line;

                while ((line = reader.readLine()) != null)
                {
                    String[] result = _regexp.split(line);
                    if (result == null || result.length < 2 || result[0].startsWith("#"))
                    {
                        continue;
                    }

                    HashedUser user = new HashedUser(result);
                    _logger.info("Created user:" + user);
                    _users.put(user.getName(), user);
                }
            }
            finally
            {
                if (reader != null)
                {
                    reader.close();
                }
            }
        }
        finally
        {
            if (_userUpdate.isHeldByCurrentThread())
            {
                _userUpdate.unlock();
            }
        }
    }

    private void savePasswordFile() throws IOException
    {
        try
        {
            _userUpdate.lock();

            BufferedReader reader = null;
            PrintStream writer = null;
            
            Random r = new Random();
            File tmp;
            do
            {
                tmp = new File(_passwordFile.getPath() + r.nextInt() + ".tmp");
            }
            while(tmp.exists());
            
            tmp.deleteOnExit();

            try
            {
                writer = new PrintStream(tmp);
                reader = new BufferedReader(new FileReader(_passwordFile));
                String line;

                while ((line = reader.readLine()) != null)
                {
                    String[] result = _regexp.split(line);
                    if (result == null || result.length < 2 || result[0].startsWith("#"))
                    {
                        writer.write(line.getBytes(DEFAULT_ENCODING));
                        writer.println();
                        continue;
                    }

                    HashedUser user = _users.get(result[0]);

                    if (user == null)
                    {
                        writer.write(line.getBytes(DEFAULT_ENCODING));
                        writer.println();
                    }
                    else if (!user.isDeleted())
                    {
                        if (!user.isModified())
                        {
                            writer.write(line.getBytes(DEFAULT_ENCODING));
                            writer.println();
                        }
                        else
                        {
                            try
                            {
                                byte[] encodedPassword = user.getEncodedPassword();

                                writer.write((user.getName() + ":").getBytes(DEFAULT_ENCODING));
                                writer.write(encodedPassword);
                                writer.println();

                                user.saved();
                            }
                            catch (Exception e)
                            {
                                _logger.warn("Unable to encode new password reverting to old password.");
                                writer.write(line.getBytes(DEFAULT_ENCODING));
                                writer.println();
                            }
                        }
                    }
                }

                for (HashedUser user : _users.values())
                {
                    if (user.isModified())
                    {
                        byte[] encodedPassword;
                        try
                        {
                            encodedPassword = user.getEncodedPassword();
                            writer.write((user.getName() + ":").getBytes(DEFAULT_ENCODING));
                            writer.write(encodedPassword);
                            writer.println();
                            user.saved();
                        }
                        catch (Exception e)
                        {
                            _logger.warn("Unable to get Encoded password for user'" + user.getName() + "' password not saved");
                        }
                    }
                }
            }
            catch(IOException e)
            {
                _logger.error("Unable to create the new password file: " + e);
                throw new IOException("Unable to create the new password file" + e);
            }
            finally
            {
                if (reader != null)
                {
                    reader.close();
                }

                if (writer != null)
                {
                    writer.close();
                }
            }
            
            // Swap temp file to main password file.
            File old = new File(_passwordFile.getAbsoluteFile() + ".old");
            if (old.exists())
            {
                old.delete();
            }
            
            if(!_passwordFile.renameTo(old))
            {
                //unable to rename the existing file to the backup name 
                _logger.error("Could not backup the existing password file");
                throw new IOException("Could not backup the existing password file");
            }

            if(!tmp.renameTo(_passwordFile))
            {
                //failed to rename the new file to the required filename
                
                if(!old.renameTo(_passwordFile))
                {
                    //unable to return the backup to required filename
                    _logger.error("Could not rename the new password file into place, and unable to restore original file");
                    throw new IOException("Could not rename the new password file into place, and unable to restore original file");
                }
                
                _logger.error("Could not rename the new password file into place");
                throw new IOException("Could not rename the new password file into place");
            }
        }
        finally
        {
            if (_userUpdate.isHeldByCurrentThread())
            {
                _userUpdate.unlock();
            }
        }
    }

    public void reload() throws IOException
    {
        loadPasswordFile();
    }

}
