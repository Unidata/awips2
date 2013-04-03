/**
 * This software was developed and / or modified by Raytheon Company,
 * pursuant to Contract DG133W-05-CQ-1067 with the US Government.
 * 
 * U.S. EXPORT CONTROLLED TECHNICAL DATA
 * This software product contains export-restricted data whose
 * export/transfer/disclosure is restricted by U.S. law. Dissemination
 * to non-U.S. persons whether in the United States or abroad requires
 * an export license or other authorization.
 * 
 * Contractor Name:        Raytheon Company
 * Contractor Address:     6825 Pine Street, Suite 340
 *                         Mail Stop B8
 *                         Omaha, NE 68106
 *                         402.291.0100
 * 
 * See the AWIPS II Master Rights File ("Master Rights File.pdf") for
 * further licensing information.
 **/
package com.raytheon.uf.common.util;

import java.io.File;
import java.io.IOException;
import java.net.URL;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Enumeration;
import java.util.List;
import java.util.jar.JarEntry;
import java.util.jar.JarFile;

/**
 * Utilities for working with Jar files.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jan 28, 2011            njensen     Initial creation
 * Nov 09, 2012 1322       djohnson    Add getResResourcePath.
 * 
 * </pre>
 * 
 * @author njensen
 * @version 1.0
 */

public class JarUtil {

    /**
     * Recursive method to return all jar files in a directory structure.
     * 
     * @param file
     * @return An array of Files
     */
    public static File[] getJarFiles(File file) {

        File[] files = null;
        List<File> list = new ArrayList<File>();
        Filter filter = new Filter();
        filter.setFileType("jar");

        if (file.exists()) {
            files = file.listFiles(filter);

            for (int counter = 0; counter < files.length; counter++) {
                if (files[counter].isDirectory()) {
                    Collections.addAll(list, getJarFiles(files[counter]));
                } else {
                    list.add(files[counter]);
                }
            }
        }
        return list.toArray(new File[] {});
    }

    /**
     * Determines if a file name is present in a jar
     * 
     * @param file
     *            The file to inspect
     * @param name
     *            The file name to find
     * @return True, if the file is present in the jar
     * @throws IOException
     */
    public static boolean isFileInJar(File file, String name)
            throws IOException {

        JarFile jar = null;

        try {
            jar = new JarFile(file);
            if (jar.getJarEntry(name) == null) {
                return false;
            }
        } catch (Exception e) {
            throw new IOException("Unable to open jar file at: "
                    + file.getPath(), e);
        } finally {
            if (jar != null) {
                try {
                    jar.close();
                } catch (IOException e) {
                    throw new IOException("Unable to close jar file", e);
                }
            }
        }
        return true;
    }

    /**
     * Gets the list of files from a jar based on a regular expression
     * 
     * @param fileLocation
     *            The location of the jar
     * @param regEx
     *            The regular expression to use as a filter
     * @return The list of files
     * @throws IOException
     */
    public static String[] getFileListFromJar(String fileLocation, String regEx)
            throws IOException {
        File file = new File(fileLocation);
        return JarUtil.getFileListFromJar(file, regEx);
    }

    /**
     * Obtains a list of files within the specified JAR that match the specified
     * regular expression. The matching is case insensitive. In the event of an
     * error, {@code null} is returned.
     * 
     * @param file
     *            the JAR to examine
     * @param regex
     *            the match pattern.
     * @return a list of jar contents matching the regex.
     * @throws IOException
     */
    public static String[] getFileListFromJar(File file, String regex)
            throws IOException {
        return getFileListFromJar(file, regex, true);
    }

    public static String[] getFileListFromJar(File file, String regex,
            boolean caseInsensitive) throws IOException {

        JarFile jar = null;
        try {
            ArrayList<String> retVal = new ArrayList<String>();
            jar = new JarFile(file);
            Enumeration<JarEntry> enm = jar.entries();
            while (enm.hasMoreElements()) {
                if (caseInsensitive) {
                    String name = enm.nextElement().getName()
                            .replaceAll("\\\\", "/").toLowerCase();
                    if (name.matches(regex.toLowerCase())) {
                        retVal.add(name);
                    }
                } else {
                    String name = enm.nextElement().getName()
                            .replaceAll("\\\\", "/");
                    if (name.matches(regex)) {
                        retVal.add(name);
                    }
                }
            }
            return retVal.toArray(new String[] {});
        } catch (Exception e) {
            throw new IOException("Unable to retrieve file list from jar", e);
        } finally {
            if (jar != null) {
                try {
                    jar.close();
                } catch (IOException e) {
                    throw new IOException("Unable to close jar file", e);
                }
            }
        }
    }

    /**
     * Attempts to find the specified resource on the classpath. First it
     * searches for the provided string resource path, if that fails, returns
     * the string prepended with "/res" since that is where the "res" resources
     * are placed in the jar files. This is required because in Eclipse the
     * Spring files are not prepended with "res" as they are in jar files.
     * 
     * @param resourcePath
     *            the resource path
     * @return the String resource path to use
     */
    public static String getResResourcePath(String resourcePath) {
        URL url = JarUtil.class.getResource(resourcePath);
        if (url != null) {
            return resourcePath;
        } else {
            return "/res" + resourcePath;
        }
    }

}
