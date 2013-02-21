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
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import jep.JepException;

/**
 * A Java class to extract the first Python error for a source file from a
 * JepException caused by an error in loading a script. This is used primarily
 * to give users useful error messages when scripts fail to import due to syntax
 * errors (including indentation errors) or missing imports.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jun 2, 2011             wldougher   Initial creation
 * Jan 8, 2013  1486       dgilling    Move to common python plugin.
 * </pre>
 * 
 * @author wldougher
 * @version 1.0
 */

public class PythonErrorExtractor {

    /** Regular expression string for syntax errors and indentation errors */
    private static final String SYNTAX_ERROR_STRING = "('|\")exceptions\\.(\\w+)\\1"
            + ".*?('|\")(.*?)\\3" // quoted error message
            + "\\, \\(('|\")(.*?)\\5" // quoted path
            + "\\, (\\d+)" // line
            + "\\,.*?\\, " // skip ", None, " in message (??)
            + "('|\")(.*?)\\8" // quoted sample of error
    ;

    /** Pattern generated from SYNTAX_ERROR_STRING */
    private static final Pattern SYNTAX_ERROR_PATTERN = Pattern
            .compile(SYNTAX_ERROR_STRING);

    /**
     * A pattern for import errors and other non-Syntax errors. module is
     * substituted for %s before Pattern is created.
     */
    private static final String IMPORT_ERROR_STRING = "('|\")exceptions\\.(\\w+)\\1"
            + ">: (.*?) >>>" // primary error message
            + ".*?(%s\\.py)" // script name (sometimes far from the start)
            + "\"(, line \\d+, in .*?)\n" // location in the script
            + "(.*?)\n"; // sample of error

    /**
     * @param exc
     *            The JepException to get the Python exception from
     * @param module
     *            The module that the exception is related to
     * @return A simplified one-line Python error, or null if a simplified error
     *         could not be extracted.
     */
    public static String getPythonError(JepException exc, String module) {
        String excMsg = exc.getLocalizedMessage();
        String result = null;

        Matcher feMatcher = SYNTAX_ERROR_PATTERN.matcher(excMsg);
        if (feMatcher.find()) {
            String errClass = feMatcher.group(2);
            String errText = feMatcher.group(4);
            String errPath = feMatcher.group(6);
            String errLine = feMatcher.group(7);
            String errSample = feMatcher.group(9);
            File errF = new File(errPath);
            String modName = errF.getName();
            result = errClass + ":" + errText + ". " + modName + " line "
                    + errLine + ", \"" + errSample + "\"";
        } else {
            String moduleImportString = String.format(IMPORT_ERROR_STRING,
                    module);
            Pattern importPattern = Pattern.compile(moduleImportString,
                    Pattern.MULTILINE | Pattern.DOTALL);
            Matcher importMatcher = importPattern.matcher(excMsg);

            if (importMatcher.find()) {
                String errClass = importMatcher.group(2);
                String errText = importMatcher.group(3);
                String modName = importMatcher.group(4);
                String errLoc = importMatcher.group(5);
                String errSample = importMatcher.group(6);

                result = errClass + ":" + errText + ". " + modName + errLoc
                        + ", \"" + errSample.trim() + "\"";
            }
        }

        return result;
    }
}
