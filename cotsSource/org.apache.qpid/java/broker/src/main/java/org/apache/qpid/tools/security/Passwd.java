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
package org.apache.qpid.tools.security;

import org.apache.commons.codec.binary.Base64;

import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;
import java.security.DigestException;
import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.io.PrintStream;

public class Passwd
{
    public static void main(String args[]) throws NoSuchAlgorithmException, DigestException, IOException
    {
        if (args.length != 2)
        {
            System.out.println("Passwd <username> <password>");
            System.exit(0);
        }

        byte[] data = args[1].getBytes("utf-8");

        MessageDigest md = MessageDigest.getInstance("MD5");

        for (byte b : data)
        {
            md.update(b);
        }

        byte[] digest = md.digest();

        Base64 b64 = new Base64();

        byte[] encoded = b64.encode(digest);

        output(args[0], encoded);
    }

    private static void output(String user, byte[] encoded) throws IOException
    {

//        File passwdFile = new File("qpid.passwd");

        PrintStream ps = new PrintStream(System.out);

        user += ":";
        ps.write(user.getBytes("utf-8"));

        for (byte b : encoded)
        {
            ps.write(b);
        }

        ps.println();

        ps.flush();
        ps.close();
    }
}
