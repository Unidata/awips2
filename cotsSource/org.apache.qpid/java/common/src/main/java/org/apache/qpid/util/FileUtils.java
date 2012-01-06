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
package org.apache.qpid.util;

import java.io.BufferedInputStream;
import java.io.BufferedReader;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.FileReader;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.util.LinkedList;
import java.util.List;

/**
 * FileUtils provides some simple helper methods for working with files. It follows the convention of wrapping all
 * checked exceptions as runtimes, so code using these methods is free of try-catch blocks but does not expect to
 * recover from errors.
 *
 * <p/><table id="crc"><caption>CRC Card</caption>
 * <tr><th> Responsibilities <th> Collaborations
 * <tr><td> Read a text file as a string.
 * <tr><td> Open a file or default resource as an input stream.
 * </table>
 */
public class FileUtils
{
    /**
     * Reads a text file as a string.
     *
     * @param filename The name of the file.
     *
     * @return The contents of the file.
     */
    public static String readFileAsString(String filename)
    {
        BufferedInputStream is = null;

        try
        {
            try
            {
                is = new BufferedInputStream(new FileInputStream(filename));
            }
            catch (FileNotFoundException e)
            {
                throw new RuntimeException(e);
            }

            return readStreamAsString(is);
        }
        finally
        {
            if (is != null)
            {
                try
                {
                    is.close();
                }
                catch (IOException e)
                {
                    throw new RuntimeException(e);
                }
            }
        }
    }

    /**
     * Reads a text file as a string.
     *
     * @param file The file.
     *
     * @return The contents of the file.
     */
    public static String readFileAsString(File file)
    {
        BufferedInputStream is = null;

        try
        {
            is = new BufferedInputStream(new FileInputStream(file));
        }
        catch (FileNotFoundException e)
        {
            throw new RuntimeException(e);
        }

        return readStreamAsString(is);
    }

    /**
     * Reads the contents of a reader, one line at a time until the end of stream is encountered, and returns all
     * together as a string.
     *
     * @param is The reader.
     *
     * @return The contents of the reader.
     */
    private static String readStreamAsString(BufferedInputStream is)
    {
        try
        {
            byte[] data = new byte[4096];

            StringBuffer inBuffer = new StringBuffer();

            String line;
            int read;

            while ((read = is.read(data)) != -1)
            {
                String s = new String(data, 0, read);
                inBuffer.append(s);
            }

            return inBuffer.toString();
        }
        catch (IOException e)
        {
            throw new RuntimeException(e);
        }
    }

    /**
     * Either opens the specified filename as an input stream, or uses the default resource loaded using the
     * specified class loader, if opening the file fails or no file name is specified.
     *
     * @param filename        The name of the file to open.
     * @param defaultResource The name of the default resource on the classpath if the file cannot be opened.
     * @param cl              The classloader to load the default resource with.
     *
     * @return An input stream for the file or resource, or null if one could not be opened.
     */
    public static InputStream openFileOrDefaultResource(String filename, String defaultResource, ClassLoader cl)
    {
        InputStream is = null;

        // Flag to indicate whether the default resource should be used. By default this is true, so that the default
        // is used when opening the file fails.
        boolean useDefault = true;

        // Try to open the file if one was specified.
        if (filename != null)
        {
            try
            {
                is = new BufferedInputStream(new FileInputStream(new File(filename)));

                // Clear the default flag because the file was succesfully opened.
                useDefault = false;
            }
            catch (FileNotFoundException e)
            {
                // Ignore this exception, the default will be used instead.
            }
        }

        // Load the default resource if a file was not specified, or if opening the file failed.
        if (useDefault)
        {
            is = cl.getResourceAsStream(defaultResource);
        }

        return is;
    }

    /**
     * Copies the specified source file to the specified destintaion file. If the destinationst file does not exist,
     * it is created.
     *
     * @param src The source file name.
     * @param dst The destination file name.
     */
    public static void copy(File src, File dst)
    {
        try
        {
            copyCheckedEx(src, dst);
        }
        catch (IOException e)
        {
            throw new RuntimeException(e);
        }
    }

    /**
     * Copies the specified source file to the specified destination file. If the destination file does not exist,
     * it is created.
     *
     * @param src The source file name.
     * @param dst The destination file name.
     * @throws IOException
     */
    public static void copyCheckedEx(File src, File dst) throws IOException
    {
        InputStream in = new FileInputStream(src);
        if (!dst.exists())
        {
            dst.createNewFile();
        }

        OutputStream out = new FileOutputStream(dst);

        // Transfer bytes from in to out
        byte[] buf = new byte[1024];
        int len;
        while ((len = in.read(buf)) > 0)
        {
            out.write(buf, 0, len);
        }

        in.close();
        out.close();
    }

    /*
     * Deletes a given file
     */
    public static boolean deleteFile(String filePath)
    {
        return delete(new File(filePath), false);
    }

    /*
     * Deletes a given empty directory 
     */
    public static boolean deleteDirectory(String directoryPath)
    {
        File directory = new File(directoryPath);

        if (directory.isDirectory())
        {
            if (directory.listFiles().length == 0)
            {
                return delete(directory, true);
            }
        }

        return false;
    }

    /**
     * Delete a given file/directory,
     * A directory will always require the recursive flag to be set.
     * if a directory is specified and recursive set then delete the whole tree
     *
     * @param file      the File object to start at
     * @param recursive boolean to recurse if a directory is specified.
     *
     * @return <code>true</code> if and only if the file or directory is
     *         successfully deleted; <code>false</code> otherwise
     */
    public static boolean delete(File file, boolean recursive)
    {
        boolean success = true;

        if (file.isDirectory())
        {
            if (recursive)
            {
                File[] files = file.listFiles();

                // This can occur if the file is deleted outside the JVM
                if (files == null)
                {
                    return false;
                }

                for (int i = 0; i < files.length; i++)
                {
                    success = delete(files[i], true) && success;
                }

                return success && file.delete();
            }

            return false;
        }

        return file.delete();
    }

    public static class UnableToCopyException extends Exception
    {
        UnableToCopyException(String msg)
        {
            super(msg);
        }
    }

    public static void copyRecursive(File source, File dst) throws FileNotFoundException, UnableToCopyException
    {

        if (!source.exists())
        {
            throw new FileNotFoundException("Unable to copy '" + source.toString() + "' as it does not exist.");
        }

        if (dst.exists() && !dst.isDirectory())
        {
            throw new IllegalArgumentException("Unable to copy '" + source.toString() + "' to '" + dst + "' a file with same name exists.");
        }

        if (source.isFile())
        {
            copy(source, dst);
        }

        //else we have a source directory
        if (!dst.isDirectory() && !dst.mkdir())
        {
            throw new UnableToCopyException("Unable to create destination directory");
        }

        for (File file : source.listFiles())
        {
            if (file.isFile())
            {
                copy(file, new File(dst.toString() + File.separator + file.getName()));
            }
            else
            {
                copyRecursive(file, new File(dst + File.separator + file.getName()));
            }
        }

    }

    /**
     * Checks the specified file for instances of the search string.
     *
     * @param file the file to search
     * @param search the search String
     *
     * @throws java.io.IOException
     * @return the list of matching entries
     */
    public static List<String> searchFile(File file, String search)
            throws IOException
    {

        List<String> results = new LinkedList<String>();

        BufferedReader reader = new BufferedReader(new FileReader(file));
        while (reader.ready())
        {
            String line = reader.readLine();
            if (line.contains(search))
            {
                results.add(line);
            }
        }

        return results;
    }
}
