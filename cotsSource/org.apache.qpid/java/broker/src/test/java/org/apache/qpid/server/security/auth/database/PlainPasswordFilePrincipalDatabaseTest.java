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
package org.apache.qpid.server.security.auth.database;

import junit.framework.TestCase;

import javax.security.auth.login.AccountNotFoundException;

import org.apache.qpid.server.security.auth.sasl.UsernamePrincipal;

import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.FileWriter;
import java.io.IOException;
import java.security.Principal;
import java.util.ArrayList;
import java.util.List;
import java.util.regex.Pattern;

public class PlainPasswordFilePrincipalDatabaseTest extends TestCase
{

    private static final String TEST_COMMENT = "# Test Comment";
    private static final String TEST_PASSWORD = "testPassword";
    private static final char[] TEST_PASSWORD_CHARS = TEST_PASSWORD.toCharArray();
    private static final String TEST_USERNAME = "testUser";
    
    private Principal _principal = new UsernamePrincipal(TEST_USERNAME);
    private PlainPasswordFilePrincipalDatabase _database;
    private List<File> _testPwdFiles = new ArrayList<File>();

    public void setUp() throws Exception
    {
        _database = new PlainPasswordFilePrincipalDatabase();
        _testPwdFiles.clear();
    }

    public void tearDown() throws Exception
    {
        //clean up any additional files and their backups
        for(File f : _testPwdFiles)
        {
            File oldPwdFile = new File(f.getAbsolutePath() + ".old");
            if(oldPwdFile.exists())
            {
                oldPwdFile.delete();
            }
            
            f.delete();
        }
    }
    
    // ******* Test Methods ********** //

    public void testCreatePrincipal()
    {
        File testFile = createPasswordFile(1, 0);

        loadPasswordFile(testFile);

        final String CREATED_PASSWORD = "guest";
        final String CREATED_USERNAME = "createdUser";

        Principal principal = new Principal()
        {
            public String getName()
            {
                return CREATED_USERNAME;
            }
        };

        assertTrue("New user not created.", _database.createPrincipal(principal, CREATED_PASSWORD.toCharArray()));

        loadPasswordFile(testFile);

        assertNotNull("Created User was not saved", _database.getUser(CREATED_USERNAME));

        assertFalse("Duplicate user created.", _database.createPrincipal(principal, CREATED_PASSWORD.toCharArray()));

        testFile.delete();
    }

    public void testCreatePrincipalIsSavedToFile()
    {

        File testFile = createPasswordFile(1, 0);

        loadPasswordFile(testFile);

        Principal principal = new Principal()
        {
            public String getName()
            {
                return TEST_USERNAME;
            }
        };

        _database.createPrincipal(principal, TEST_PASSWORD_CHARS);

        try
        {
            BufferedReader reader = new BufferedReader(new FileReader(testFile));

            assertTrue("File has no content", reader.ready());

            assertEquals("Comment line has been corrupted.", TEST_COMMENT, reader.readLine());

            assertTrue("File is missing user data.", reader.ready());

            String userLine = reader.readLine();

            String[] result = Pattern.compile(":").split(userLine);

            assertEquals("User line not complete '" + userLine + "'", 2, result.length);

            assertEquals("Username not correct,", TEST_USERNAME, result[0]);
            assertEquals("Password not correct,", TEST_PASSWORD, result[1]);

            assertFalse("File has more content", reader.ready());

        }
        catch (IOException e)
        {
            fail("Unable to valdate file contents due to:" + e.getMessage());
        }
        testFile.delete();
    }
    
    public void testDeletePrincipal()
    {
        File testFile = createPasswordFile(1, 1);

        loadPasswordFile(testFile);

        Principal user = _database.getUser(TEST_USERNAME + "0");
        assertNotNull("Generated user not present.", user);

        try
        {
            _database.deletePrincipal(user);
        }
        catch (AccountNotFoundException e)
        {
            fail("User should be present" + e.getMessage());
        }

        try
        {
            _database.deletePrincipal(user);
            fail("User should not be present");
        }
        catch (AccountNotFoundException e)
        {
            //pass
        }

        loadPasswordFile(testFile);

        try
        {
            _database.deletePrincipal(user);
            fail("User should not be present");
        }
        catch (AccountNotFoundException e)
        {
            //pass
        }

        assertNull("Deleted user still present.", _database.getUser(TEST_USERNAME + "0"));

        testFile.delete();
    }

    public void testGetUsers()
    {
        int USER_COUNT = 10;
        File testFile = createPasswordFile(1, USER_COUNT);

        loadPasswordFile(testFile);

        Principal user = _database.getUser("MISSING_USERNAME");
        assertNull("Missing user present.", user);

        List<Principal> users = _database.getUsers();

        assertNotNull("Users list is null.", users);

        assertEquals(USER_COUNT, users.size());

        boolean[] verify = new boolean[USER_COUNT];
        for (int i = 0; i < USER_COUNT; i++)
        {
            Principal principal = users.get(i);

            assertNotNull("Generated user not present.", principal);

            String name = principal.getName();

            int id = Integer.parseInt(name.substring(TEST_USERNAME.length()));

            assertFalse("Duplicated username retrieve", verify[id]);
            verify[id] = true;
        }

        for (int i = 0; i < USER_COUNT; i++)
        {
            assertTrue("User " + i + " missing", verify[i]);
        }

        testFile.delete();
    }

    public void testUpdatePasswordIsSavedToFile()
    {

        File testFile = createPasswordFile(1, 1);

        loadPasswordFile(testFile);

        Principal testUser = _database.getUser(TEST_USERNAME + "0");

        assertNotNull(testUser);

        String NEW_PASSWORD = "NewPassword";
        try
        {
            _database.updatePassword(testUser, NEW_PASSWORD.toCharArray());
        }
        catch (AccountNotFoundException e)
        {
            fail(e.toString());
        }

        try
        {
            BufferedReader reader = new BufferedReader(new FileReader(testFile));

            assertTrue("File has no content", reader.ready());

            assertEquals("Comment line has been corrupted.", TEST_COMMENT, reader.readLine());

            assertTrue("File is missing user data.", reader.ready());

            String userLine = reader.readLine();

            String[] result = Pattern.compile(":").split(userLine);

            assertEquals("User line not complete '" + userLine + "'", 2, result.length);

            assertEquals("Username not correct,", TEST_USERNAME + "0", result[0]);
            assertEquals("New Password not correct,", NEW_PASSWORD, result[1]);

            assertFalse("File has more content", reader.ready());

        }
        catch (IOException e)
        {
            fail("Unable to valdate file contents due to:" + e.getMessage());
        }
        testFile.delete();
    }

    public void testSetPasswordFileWithMissingFile()
    {
        try
        {
            _database.setPasswordFile("DoesntExist");
        }
        catch (FileNotFoundException fnfe)
        {
            assertTrue(fnfe.getMessage(), fnfe.getMessage().startsWith("Cannot find password file"));
        }
        catch (IOException e)
        {
            fail("Password File was not created." + e.getMessage());
        }

    }

    public void testSetPasswordFileWithReadOnlyFile()
    {

        File testFile = createPasswordFile(0, 0);

        testFile.setReadOnly();

        try
        {
            _database.setPasswordFile(testFile.toString());
        }
        catch (FileNotFoundException fnfe)
        {
            assertTrue(fnfe.getMessage().startsWith("Cannot read password file "));
        }
        catch (IOException e)
        {
            fail("Password File was not created." + e.getMessage());
        }

        testFile.delete();
    }
    
    private void createUserPrincipal() throws IOException
    {
        File testFile = createPasswordFile(0, 0);
        loadPasswordFile(testFile);
        
        _database.createPrincipal(_principal, TEST_PASSWORD_CHARS);
        Principal newPrincipal = _database.getUser(TEST_USERNAME);
        assertNotNull(newPrincipal);
        assertEquals(_principal.getName(), newPrincipal.getName());
    }
    
    public void testVerifyPassword() throws IOException, AccountNotFoundException
    {
        createUserPrincipal();
        assertFalse(_database.verifyPassword(TEST_USERNAME, new char[]{}));
        assertFalse(_database.verifyPassword(TEST_USERNAME, "massword".toCharArray()));
        assertTrue(_database.verifyPassword(TEST_USERNAME, TEST_PASSWORD_CHARS));
        
        try
        {
            _database.verifyPassword("made.up.username", TEST_PASSWORD_CHARS);
            fail("Should not have been able to verify this non-existant users password.");
        }
        catch (AccountNotFoundException e)
        {
            // pass
        }
    }
    
    public void testUpdatePassword() throws IOException, AccountNotFoundException 
    {
        createUserPrincipal();
        char[] newPwd = "newpassword".toCharArray();
        _database.updatePassword(_principal, newPwd);
        assertFalse(_database.verifyPassword(TEST_USERNAME, TEST_PASSWORD_CHARS));
        assertTrue(_database.verifyPassword(TEST_USERNAME, newPwd));
    }
 
    
    
    // *********** Utility Methods ******** //
    
    private File createPasswordFile(int commentLines, int users)
    {
        try
        {
            File testFile = File.createTempFile(this.getClass().getName(),"tmp");
            testFile.deleteOnExit();

            BufferedWriter writer = new BufferedWriter(new FileWriter(testFile));

            for (int i = 0; i < commentLines; i++)
            {
                writer.write(TEST_COMMENT);
                writer.newLine();
            }

            for (int i = 0; i < users; i++)
            {
                writer.write(TEST_USERNAME + i + ":" + TEST_PASSWORD);
                writer.newLine();
            }

            writer.flush();
            writer.close();
            
            _testPwdFiles.add(testFile);

            return testFile;

        }
        catch (IOException e)
        {
            fail("Unable to create test password file." + e.getMessage());
        }

        return null;
    }

    private void loadPasswordFile(File file)
    {
        try
        {
            _database.setPasswordFile(file.toString());
        }
        catch (IOException e)
        {
            fail("Password File was not created." + e.getMessage());
        }
    }
    
    
}
