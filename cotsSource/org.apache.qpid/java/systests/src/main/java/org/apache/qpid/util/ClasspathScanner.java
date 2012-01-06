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

import java.io.File;
import java.util.*;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import org.apache.log4j.Logger;

/**
 * An ClasspathScanner scans the classpath for classes that implement an interface or extend a base class and have names
 * that match a regular expression.
 *
 * <p/>In order to test whether a class implements an interface or extends a class, the class must be loaded (unless
 * the class files were to be scanned directly). Using this collector can cause problems when it scans the classpath,
 * because loading classes will initialize their statics, which in turn may cause undesired side effects. For this
 * reason, the collector should always be used with a regular expression, through which the class file names are
 * filtered, and only those that pass this filter will be tested. For example, if you define tests in classes that
 * end with the keyword "Test" then use the regular expression "Test$" to match this.
 *
 * <p><table id="crc"><caption>CRC Card</caption>
 * <tr><th> Responsibilities <th> Collaborations
 * <tr><td> Find all classes matching type and name pattern on the classpath.
 * </table>
 *
 * @todo Add logic to scan jars as well as directories.
 */
public class ClasspathScanner
{
    private static final Logger log = Logger.getLogger(ClasspathScanner.class);

    /**
     * Scans the classpath and returns all classes that extend a specified class and match a specified name.
     * There is an flag that can be used to indicate that only Java Beans will be matched (that is, only those classes
     * that have a default constructor).
     *
     * @param matchingClass  The class or interface to match.
     * @param matchingRegexp The regular expression to match against the class name.
     * @param beanOnly       Flag to indicate that onyl classes with default constructors should be matched.
     *
     * @return All the classes that match this collector.
     */
    public static <T> Collection<Class<? extends T>> getMatches(Class<T> matchingClass, String matchingRegexp,
        boolean beanOnly)
    {
        log.debug("public static <T> Collection<Class<? extends T>> getMatches(Class<T> matchingClass = " + matchingClass
            + ", String matchingRegexp = " + matchingRegexp + ", boolean beanOnly = " + beanOnly + "): called");

        // Build a compiled regular expression from the pattern to match.
        Pattern matchPattern = Pattern.compile(matchingRegexp);

        String classPath = System.getProperty("java.class.path");
        Map<String, Class<? extends T>> result = new HashMap<String, Class<? extends T>>();

        log.debug("classPath = " + classPath);

        // Find matching classes starting from all roots in the classpath.
        for (String path : splitClassPath(classPath))
        {
            gatherFiles(new File(path), "", result, matchPattern, matchingClass);
        }

        return result.values();
    }

    /**
     * Finds all matching classes rooted at a given location in the file system. If location is a directory it
     * is recursively examined.
     *
     * @param classRoot     The root of the current point in the file system being examined.
     * @param classFileName The name of the current file or directory to examine.
     * @param result        The accumulated mapping from class names to classes that match the scan.
     *
     * @todo Recursion ok as file system depth is not likely to exhaust the stack. Might be better to replace with
     *       iteration.
     */
    private static <T> void gatherFiles(File classRoot, String classFileName, Map<String, Class<? extends T>> result,
        Pattern matchPattern, Class<? extends T> matchClass)
    {
        log.debug("private static <T> void gatherFiles(File classRoot = " + classRoot + ", String classFileName = "
            + classFileName + ", Map<String, Class<? extends T>> result, Pattern matchPattern = " + matchPattern
            + ", Class<? extends T> matchClass = " + matchClass + "): called");

        File thisRoot = new File(classRoot, classFileName);

        // If the current location is a file, check if it is a matching class.
        if (thisRoot.isFile())
        {
            // Check that the file has a matching name.
            if (matchesName(thisRoot.getName(), matchPattern))
            {
                String className = classNameFromFile(thisRoot.getName());

                // Check that the class has matching type.
                try
                {
                    Class<?> candidateClass = Class.forName(className);

                    Class matchedClass = matchesClass(candidateClass, matchClass);

                    if (matchedClass != null)
                    {
                        result.put(className, matchedClass);
                    }
                }
                catch (ClassNotFoundException e)
                {
                    // Ignore this. The matching class could not be loaded.
                    log.debug("Got ClassNotFoundException, ignoring.", e);
                }
            }

            return;
        }
        // Otherwise the current location is a directory, so examine all of its contents.
        else
        {
            String[] contents = thisRoot.list();

            if (contents != null)
            {
                for (String content : contents)
                {
                    gatherFiles(classRoot, classFileName + File.separatorChar + content, result, matchPattern, matchClass);
                }
            }
        }
    }

    /**
     * Checks if the specified class file name corresponds to a class with name matching the specified regular expression.
     *
     * @param classFileName The class file name.
     * @param matchPattern  The regular expression pattern to match.
     *
     * @return <tt>true</tt> if the class name matches, <tt>false</tt> otherwise.
     */
    private static boolean matchesName(String classFileName, Pattern matchPattern)
    {
        String className = classNameFromFile(classFileName);
        Matcher matcher = matchPattern.matcher(className);

        return matcher.matches();
    }

    /**
     * Checks if the specified class to compare extends the base class being scanned for.
     *
     * @param matchingClass The base class to match against.
     * @param toMatch       The class to match against the base class.
     *
     * @return The class to check, cast as an instance of the class to match if the class extends the base class, or
     *         <tt>null</tt> otherwise.
     */
    private static <T> Class<? extends T> matchesClass(Class<?> matchingClass, Class<? extends T> toMatch)
    {
        try
        {
            return matchingClass.asSubclass(toMatch);
        }
        catch (ClassCastException e)
        {
            return null;
        }
    }

    /**
     * Takes a classpath (which is a series of paths) and splits it into its component paths.
     *
     * @param classPath The classpath to split.
     *
     * @return A list of the component paths that make up the class path.
     */
    private static List<String> splitClassPath(String classPath)
    {
        List<String> result = new LinkedList<String>();
        String separator = System.getProperty("path.separator");
        StringTokenizer tokenizer = new StringTokenizer(classPath, separator);

        while (tokenizer.hasMoreTokens())
        {
            result.add(tokenizer.nextToken());
        }

        return result;
    }

    /**
     * Translates from the filename of a class to its fully qualified classname. Files are named using forward slash
     * seperators and end in ".class", whereas fully qualified class names use "." sperators and no ".class" ending.
     *
     * @param classFileName The filename of the class to translate to a class name.
     *
     * @return The fully qualified class name.
     */
    private static String classNameFromFile(String classFileName)
    {
        log.debug("private static String classNameFromFile(String classFileName = " + classFileName + "): called");

        // Remove the .class ending.
        String s = classFileName.substring(0, classFileName.length() - ".class".length());

        // Turn / seperators in . seperators.
        String s2 = s.replace(File.separatorChar, '.');

        // Knock off any leading . caused by a leading /.
        if (s2.startsWith("."))
        {
            return s2.substring(1);
        }

        return s2;
    }
}
