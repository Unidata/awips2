/*
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
 */
package org.apache.qpid.example.shared;

import java.io.*;

/**
 * Class that provides file related utility methods for utility use
 */
public class FileUtils {


    //Reads file content into String
    public static String getFileContent(String filePath) throws IOException
    {

        BufferedReader reader = null;
        String tempData = "";
        String eol = "\n\r";

        try
        {
            String line;
            reader = new BufferedReader(new FileReader(filePath));
            while ((line = reader.readLine()) != null)
            {
                if (!tempData.equals(""))
                {
                    tempData = tempData + eol + line;
                }
                else
                {
                    tempData = line;
                }
            }
        }
        finally
        {
            if (reader != null)
            {
                reader.close();
            }
        }
        return tempData;
    }

     /*
     * Reads xml from a file and returns it as an array of chars
     */
    public static char[] getFileAsCharArray(String filePath) throws IOException
    {
        BufferedReader reader = null;
        char[] tempChars = null;
        String tempData = "";

        try
        {
            String line;
            reader = new BufferedReader(new FileReader(filePath));
            while ((line = reader.readLine()) != null)
            {
                tempData = tempData + line;
            }
            tempChars = tempData.toCharArray();
        }
        finally
        {
            if (reader != null)
            {
                reader.close();
            }
        }
        return tempChars;
    }

    /*
    * Write String content to filename provided
    */
    public static void writeStringToFile(String content, String path) throws IOException
    {

        BufferedWriter writer = new BufferedWriter(new FileWriter(new File(path)));
        writer.write(content);
        writer.flush();
        writer.close();
    }

    /*
    * Allows moving of files to a new dir and preserves the last bit of the name only
    */
    public static void moveFileToNewDir(String path, String newDir) throws IOException
    {
        //get file name from current path
        //while more files in dir publish them
        File pathFile = new File(path);
        if (pathFile.isDirectory())
        {
            File[] files = pathFile.listFiles();
            for (File file : files)
            {
                moveFileToNewDir(file,newDir);
            }
        }
    }

    /*
    * Allows moving of a file to a new dir and preserves the last bit of the name only
    */
    public static void moveFileToNewDir(File fileToMove, String newDir) throws IOException
    {
        moveFile(fileToMove,getArchiveFileName(fileToMove,newDir));
    }

    /*
    * Moves file from a given path to a new path with String params
    */
    public static void moveFile(String fromPath, String dest) throws IOException
    {
        moveFile(new File(fromPath),new File(dest));
    }

    /*
    * Moves file from a given path to a new path with mixed params
    */
    public static void moveFile(File fileToMove, String dest) throws IOException
    {
        moveFile(fileToMove,new File(dest));
    }

    /*
    * Moves file from a given path to a new path with File params
    */
    public static void moveFile(File fileToMove, File dest) throws IOException
    {
        fileToMove.renameTo(dest);
    }

    /*
    * Deletes a given file
    */
    public static void deleteFile(String filePath) throws IOException
    {
        new File(filePath).delete();
    }

    private static String getArchiveFileName(File fileToMove, String archiveDir)
    {
         //get file name from current path
        String fileName = fileToMove.getName();
        return archiveDir + File.separator + fileName;
    }
}
