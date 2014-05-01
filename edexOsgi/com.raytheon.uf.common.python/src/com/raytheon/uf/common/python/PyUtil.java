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
package com.raytheon.uf.common.python;

import java.io.File;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Set;

/**
 * Utilities for the python bridge
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Feb 25, 2008            njensen     Initial creation
 * Jun  3, 2008 1164       jelkins     listToList fixes for null and empty list
 * Jun 13, 2008 1164       jelkins     mapToDictionary fixes for dict, list, and tuple values
 * Jun 16, 2008 1164       jelkins     mapToDictionary support for integer and float values
 * Jun 19, 2008 1164       jelkins     sanitize Strings before being added to dict() and list()
 * 
 * </pre>
 * 
 * @author njensen
 * @version 1.0
 */

public class PyUtil {

    private static final String SEPARATOR = File.pathSeparator;

    /**
     * Transforms a Java map into a String declaring a python dictionary that
     * can be used by a python interpreter
     * 
     * @param map
     * @return
     */
    public static String mapToDictionary(Map<String, Object> map) {
        Set<String> keys = map.keySet();
        Iterator<String> itr = keys.iterator();

        StringBuffer s = new StringBuffer();
        s.append("{");
        while (itr.hasNext()) {
            s.append("'");
            String key = itr.next();
            s.append(key);
            s.append("':");

            String value = map.get(key).toString();
            if (value != null && !value.isEmpty()) {
                switch (value.charAt(0)) {
                case '[':
                case '{':
                case '(':
                    s.append(value);
                    break;
                default:
                    try {
                        Double.parseDouble(value);
                        // its a number
                        s.append(value);
                    } catch (NumberFormatException e) {
                        // its not a number
                        s.append("'");
                        s.append(sanitize(value));
                        s.append("'");
                    }
                    break;
                }
            } else {
                s.append("None");
            }
            if (itr.hasNext()) {
                s.append(",");
            }
        }
        s.append("}");

        return s.toString();
    }

    /**
     * Transforms a Java List of Strings to a Python list of Strings that can be
     * used by a python interpreter
     * 
     * @param list
     *            the list of strings
     * @return a python statement in string format
     */
    public static String listToList(List<String> list) {
        StringBuffer s = new StringBuffer();
        s.append("[");
        if (list != null) {

            for (String str : list) {
                s.append("'");
                s.append(sanitize(str));
                s.append("', ");
            }

            if (!list.isEmpty()) {
                s.deleteCharAt(s.length() - 1);
                s.deleteCharAt(s.length() - 1);
            }
        }
        s.append("]");

        return s.toString();
    }

    /**
     * Transforms a Java List of Strings to a Python tuple of Strings that can
     * be used by a python interpreter
     * 
     * @param list
     *            the list of strings
     * @return a python statement in string format
     */
    public static String listToTuple(List<String> list) {
        StringBuffer s = new StringBuffer();
        s.append("(");
        if (list != null) {

            for (String str : list) {
                s.append("'");
                s.append(sanitize(str));
                s.append("', ");
            }

            if (!list.isEmpty()) {
                s.deleteCharAt(s.length() - 1);
                s.deleteCharAt(s.length() - 1);
            }
        }
        s.append(")");

        return s.toString();
    }

    /**
     * Transforms a Java List of Objects to a Python list of Strings that can be
     * used by a python interpreter
     * 
     * @param list
     *            the list of objects
     * @return a python statement in string format
     */
    public static String objListToList(List<? extends Object> list) {
        StringBuffer s = new StringBuffer();
        s.append("[");
        if (list != null) {

            for (Object obj : list) {
                s.append("'");
                s.append(sanitize(obj.toString()));
                s.append("', ");
            }

            if (!list.isEmpty()) {
                s.deleteCharAt(s.length() - 1);
                s.deleteCharAt(s.length() - 1);
            }
        }
        s.append("]");

        return s.toString();
    }

    /**
     * This method prevents ' characters from corrupting the python data
     * structures
     * 
     * @param str
     *            contains possible ' characters
     * @return a new string with all ' characters escaped
     */
    private static String sanitize(String str) {

        // escape backslahes
        str = str.replaceAll("\\\\", "\\\\\\\\");

        // escape single quotes
        str = str.replaceAll("'", "\\\\'");

        return str;
    }

    /**
     * Builds a jep python include path, separating directories with a :
     * 
     * @param includeDirs
     *            the dirs that should be added to the python path
     * @return a string representing the include path
     */
    public static String buildJepIncludePath(String... includeDirs) {
        return buildJepIncludePath(false, includeDirs);
    }

    /**
     * Builds a jep python include path, separating directories with a :
     * 
     * @param createDirs
     *            if true create any non-existent directories
     * 
     * @param includeDirs
     *            the dirs that should be added to the python path
     * @return a string representing the include path
     */
    public static String buildJepIncludePath(boolean createDirs,
            String... includeDirs) {
        StringBuffer sb = new StringBuffer();
        for (int i = 0; i < includeDirs.length; i++) {
            sb.append(includeDirs[i]);
            if (i < includeDirs.length - 1) {
                sb.append(SEPARATOR);
            }
        }

        String s = sb.toString();
        if (createDirs) {
            createDirs(s);
        }
        return s;
    }

    private static void createDirs(String includeString) {
        String[] includeDirs = includeString.split(SEPARATOR);

        for (String s : includeDirs) {
            if (!s.isEmpty()) {
                File dir = new File(s);
                if (!dir.exists()) {
                    dir.mkdirs();
                }
            }
        }
    }

}
