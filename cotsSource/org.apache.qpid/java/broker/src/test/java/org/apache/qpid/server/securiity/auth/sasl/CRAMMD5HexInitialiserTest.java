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
package org.apache.qpid.server.securiity.auth.sasl;

import junit.framework.TestCase;
import org.apache.qpid.server.security.auth.database.PropertiesPrincipalDatabase;
import org.apache.qpid.server.security.auth.sasl.crammd5.CRAMMD5HexInitialiser;

import javax.security.auth.callback.Callback;
import javax.security.auth.callback.NameCallback;
import javax.security.auth.callback.PasswordCallback;
import javax.security.auth.callback.UnsupportedCallbackException;
import java.io.IOException;
import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;
import java.util.Properties;

/**
 * These tests ensure that the Hex wrapping that the initialiser performs does actually operate when the handle method is called.
 */
public class CRAMMD5HexInitialiserTest extends TestCase
{

    public void testHex()
    {

        //Create User details for testing
        String user = "testUser";
        String password = "testPassword";

        perform(user, password);
    }

    public void testHashedHex()
    {

        //Create User details for testing
        String user = "testUser";
        String password = "testPassword";

        //Create a hashed password that we then attempt to put through the call back mechanism.
        try
        {
            password = new String(MessageDigest.getInstance("MD5").digest(password.getBytes()));
        }
        catch (NoSuchAlgorithmException e)
        {
            fail(e.getMessage());
        }

        perform(user, password);
    }

    public void perform(String user, String password)
    {
        CRAMMD5HexInitialiser initialiser = new CRAMMD5HexInitialiser();

        //Use properties to create a PrincipalDatabase
        Properties users = new Properties();
        users.put(user, password);

        PropertiesPrincipalDatabase db = new PropertiesPrincipalDatabase(users);

        initialiser.initialise(db);

        //setup the callbacks
        PasswordCallback passwordCallback = new PasswordCallback("password:", false);
        NameCallback usernameCallback = new NameCallback("user:", user);

        Callback[] callbacks = new Callback[]{usernameCallback, passwordCallback};

        //Check the
        try
        {
            assertNull("The password was not null before the handle call.", passwordCallback.getPassword());
            initialiser.getCallbackHandler().handle(callbacks);
        }
        catch (IOException e)
        {
            fail(e.getMessage());
        }
        catch (UnsupportedCallbackException e)
        {
            fail(e.getMessage());
        }

        //Hex the password we initialised with and compare it with the passwordCallback
        assertArrayEquals(toHex(password.toCharArray()), passwordCallback.getPassword());

    }

    private void assertArrayEquals(char[] expected, char[] actual)
    {        
        assertEquals("Arrays are not the same length", expected.length, actual.length);

        for (int index = 0; index < expected.length; index++)
        {
            assertEquals("Characters are not equal", expected[index], actual[index]);
        }
    }

    private char[] toHex(char[] password)
    {
        StringBuilder sb = new StringBuilder();
        for (char c : password)
        {
            //toHexString does not prepend 0 so we have to
            if (((byte) c > -1) && (byte) c < 10)
            {
                sb.append(0);
            }

            sb.append(Integer.toHexString(c & 0xFF));
        }

        //Extract the hex string as char[]
        char[] hex = new char[sb.length()];

        sb.getChars(0, sb.length(), hex, 0);

        return hex;
    }
    

}
