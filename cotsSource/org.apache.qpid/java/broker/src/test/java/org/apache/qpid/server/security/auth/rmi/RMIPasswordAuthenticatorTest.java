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
package org.apache.qpid.server.security.auth.rmi;

import java.io.BufferedWriter;
import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.util.Collections;

import javax.management.remote.JMXPrincipal;
import javax.security.auth.Subject;

import org.apache.qpid.server.security.auth.database.Base64MD5PasswordFilePrincipalDatabase;
import org.apache.qpid.server.security.auth.database.PlainPasswordFilePrincipalDatabase;

import junit.framework.TestCase;

public class RMIPasswordAuthenticatorTest extends TestCase
{
    private final String USERNAME = "guest";
    private final String PASSWORD = "guest";
    private final String B64_MD5HASHED_PASSWORD = "CE4DQ6BIb/BVMN9scFyLtA==";
    private RMIPasswordAuthenticator _rmipa;
    
    private Base64MD5PasswordFilePrincipalDatabase _md5Pd;
    private File _md5PwdFile;
    
    private PlainPasswordFilePrincipalDatabase _plainPd;
    private File _plainPwdFile;

    private Subject testSubject;

    protected void setUp() throws Exception
    {
        _rmipa = new RMIPasswordAuthenticator();
        
        _md5Pd = new Base64MD5PasswordFilePrincipalDatabase();
        _md5PwdFile = createTempPasswordFile(this.getClass().getName()+"md5pwd", USERNAME, B64_MD5HASHED_PASSWORD);
        _md5Pd.setPasswordFile(_md5PwdFile.getAbsolutePath());
        
        _plainPd = new PlainPasswordFilePrincipalDatabase();
        _plainPwdFile = createTempPasswordFile(this.getClass().getName()+"plainpwd", USERNAME, PASSWORD);
        _plainPd.setPasswordFile(_plainPwdFile.getAbsolutePath());
        
        testSubject = new Subject(true,
                Collections.singleton(new JMXPrincipal(USERNAME)),
                Collections.EMPTY_SET,
                Collections.EMPTY_SET);
    }
    
    private File createTempPasswordFile(String filenamePrefix, String user, String password)
    {
        try
        {
            File testFile = File.createTempFile(filenamePrefix,"tmp");
            testFile.deleteOnExit();

            BufferedWriter writer = new BufferedWriter(new FileWriter(testFile));

            writer.write(user + ":" + password);
            writer.newLine();

            writer.flush();
            writer.close();

            return testFile;
        }
        catch (IOException e)
        {
            fail("Unable to create temporary test password file." + e.getMessage());
        }

        return null;
    }

    
    //********** Test Methods *********//
    
    
    public void testAuthenticate()
    {
        String[] credentials;
        Subject newSubject;

        // Test when no PD has been set
        try
        {
            credentials = new String[]{USERNAME, PASSWORD};
            newSubject = _rmipa.authenticate(credentials);
            fail("SecurityException expected due to lack of principal database");
        }
        catch (SecurityException se)
        {
            assertEquals("Unexpected exception message",
                    RMIPasswordAuthenticator.UNABLE_TO_LOOKUP, se.getMessage());
        }

        //The PrincipalDatabase's are tested primarily by their own tests, but
        //minimal tests are done here to exercise their usage in this area.
        
        // Test correct passwords are verified with an MD5 PD
        try
        {
            _rmipa.setPrincipalDatabase(_md5Pd);
            credentials = new String[]{USERNAME, PASSWORD};
            newSubject = _rmipa.authenticate(credentials);
            assertTrue("Returned subject does not equal expected value", 
                    newSubject.equals(testSubject));
        }
        catch (Exception e)
        {
            fail("Unexpected Exception:" + e.getMessage());
        }

        // Test incorrect passwords are not verified with an MD5 PD
        try
        {
            credentials = new String[]{USERNAME, PASSWORD+"incorrect"};
            newSubject = _rmipa.authenticate(credentials);
            fail("SecurityException expected due to incorrect password");
        }
        catch (SecurityException se)
        {
            assertEquals("Unexpected exception message",
                    RMIPasswordAuthenticator.INVALID_CREDENTIALS, se.getMessage());
        }
        
        // Test non-existent accounts are not verified with an MD5 PD
        try
        {
            credentials = new String[]{USERNAME+"invalid", PASSWORD};
            newSubject = _rmipa.authenticate(credentials);
            fail("SecurityException expected due to non-existant account");
        }
        catch (SecurityException se)
        {
            assertEquals("Unexpected exception message",
                    RMIPasswordAuthenticator.INVALID_CREDENTIALS, se.getMessage());
        }

        // Test correct passwords are verified with a Plain PD
        try
        {
            _rmipa.setPrincipalDatabase(_plainPd);
            credentials = new String[]{USERNAME, PASSWORD};
            newSubject = _rmipa.authenticate(credentials);
            assertTrue("Returned subject does not equal expected value", 
                    newSubject.equals(testSubject));
        }
        catch (Exception e)
        {
            fail("Unexpected Exception");
        }

        // Test incorrect passwords are not verified with a Plain PD
        try
        {
            credentials = new String[]{USERNAME, PASSWORD+"incorrect"};
            newSubject = _rmipa.authenticate(credentials);
            fail("SecurityException expected due to incorrect password");
        }
        catch (SecurityException se)
        {
            assertEquals("Unexpected exception message",
                    RMIPasswordAuthenticator.INVALID_CREDENTIALS, se.getMessage());
        }
        
        // Test non-existent accounts are not verified with an Plain PD
        try
        {
            credentials = new String[]{USERNAME+"invalid", PASSWORD};
            newSubject = _rmipa.authenticate(credentials);
            fail("SecurityException expected due to non existant account");
        }
        catch (SecurityException se)
        {
            assertEquals("Unexpected exception message",
                    RMIPasswordAuthenticator.INVALID_CREDENTIALS, se.getMessage());
        }

        // Test handling of non-string credential's
        try
        {
            Object[] objCredentials = new Object[]{USERNAME, PASSWORD};
            newSubject = _rmipa.authenticate(objCredentials);
            fail("SecurityException expected due to non string[] credentials");
        }
        catch (SecurityException se)
        {
            assertEquals("Unexpected exception message",
                    RMIPasswordAuthenticator.SHOULD_BE_STRING_ARRAY, se.getMessage());
        }
        
        // Test handling of incorrect number of credential's
        try
        {
            credentials = new String[]{USERNAME, PASSWORD, PASSWORD};
            newSubject = _rmipa.authenticate(credentials);
            fail("SecurityException expected due to supplying wrong number of credentials");
        }
        catch (SecurityException se)
        {
            assertEquals("Unexpected exception message",
                    RMIPasswordAuthenticator.SHOULD_HAVE_2_ELEMENTS, se.getMessage());
        }
        
        // Test handling of null credential's
        try
        {
            //send a null array
            credentials = null;
            newSubject = _rmipa.authenticate(credentials);
            fail("SecurityException expected due to not supplying an array of credentials");
        }
        catch (SecurityException se)
        {
            assertEquals("Unexpected exception message",
                    RMIPasswordAuthenticator.CREDENTIALS_REQUIRED, se.getMessage());
        }
        
        try
        {
            //send a null password
            credentials = new String[]{USERNAME, null};
            newSubject = _rmipa.authenticate(credentials);
            fail("SecurityException expected due to sending a null password");
        }
        catch (SecurityException se)
        {
            assertEquals("Unexpected exception message",
                    RMIPasswordAuthenticator.SHOULD_BE_NON_NULL, se.getMessage());
        }
        
        try
        {
            //send a null username
            credentials = new String[]{null, PASSWORD};
            newSubject = _rmipa.authenticate(credentials);
            fail("SecurityException expected due to sending a null username");
        }
        catch (SecurityException se)
        {
            assertEquals("Unexpected exception message",
                    RMIPasswordAuthenticator.SHOULD_BE_NON_NULL, se.getMessage());
        }
    }

}
