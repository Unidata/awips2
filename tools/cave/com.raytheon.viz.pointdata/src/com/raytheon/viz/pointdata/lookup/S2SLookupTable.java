package com.raytheon.viz.pointdata.lookup;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.IOException;
import java.util.Arrays;
import java.util.Iterator;
import java.util.LinkedList;

import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;

/**
 * 
 * Implementation of a String to String (S2S) lookup table
 * 
 * <h4>4.1) String to string lookup table</h4> (from AWIPS1
 * adaptivePlanViewPlotting.html)
 * 
 * <p>
 * The first line of a string to string lookup table is "s2s". A string to
 * string lookup table is used to convert some arbitrary string to some other
 * string. Most entries are a line with a lookup string followed by a result
 * string to which to translate it. Sometimes, there is a need to have
 * characters in the result string that are unprintable in ASCII. In that case,
 * the result string can be expressed with a colon followed by a list of
 * integers designating the character codes (all space delimited). It is also
 * possible to make an entry with only a lookup string, which means that the
 * result of translating that string will be an empty string.
 * </p>
 * 
 * <p>
 * There are four special keywords that are recognized in a string to string
 * table. Once a line has been encountered that does not have one of these
 * keywords on it, further occurences of these keywords will be treated as a
 * regular lookup string. Normally, if an attempt is made to translate an input
 * string that does not exist in the table as a lookup string, the output will
 * be some constant default string. The keyword `pass,' if present, indicates
 * that if the input string is not available as a lookup string, then the
 * resulting translation should be the input string. The usual default string is
 * an empty string, but using the keyword `default' allows one to supply a
 * different default string. There is a special type of translation available
 * for the string to string table that will attempt to translate all possible
 * substrings in the input string, as well as the entire string. The keywords
 * `left' and `right' allow one to perform an edit operation on the input string
 * before it is translated in this case. The arguments after the `left' or
 * `right' keyword are a lookup and result string expressed exactly as in a
 * regular entry. For `left,' the first occurence of the corresponding lookup
 * string is located, and all text up to that occurence is replaced with the
 * corresponding result text. `right' works analogously with the last occurence
 * and the end of the string to translate.
 * </p>
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Apr 21, 2011            ekladstrup     Initial creation
 * 
 * </pre>
 * 
 * @author ekladstrup
 * @version 1.0
 */
public class S2SLookupTable implements IAbstractLookupTable {

    private class S2SPair {
        private String key;

        private String value;

        public S2SPair(String key, String value) {
            this.setKey(key);
            this.setValue(value);
        }

        public void setKey(String key) {
            this.key = key;
        }

        public String getKey() {
            return key;
        }

        public void setValue(String value) {
            this.value = value;
        }

        public String getValue() {
            return value;
        }
    }

    /**
     * mode for lookup, e.g. recursive_table means use recursiveTranslation
     * instead of the simple lookup method
     */
    private String mode = "";

    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(S2SLookupTable.class);

    /**
     * if pass is true then return back the lookup key if the table does not
     * have a match
     */
    private boolean pass = false;

    private boolean foundDefault = false;

    private boolean foundLeft = false;

    private boolean foundRight = false;

    private boolean foundPass = false;

    private String leftKey = null;

    private String leftValue = "";

    private String rightKey = null;

    private String rightValue = "";

    private String tablePath = "";

    private String defaultValue = "";

    private LinkedList<S2SPair> lookupList = null;

    public S2SLookupTable(File table) {
        lookupList = new LinkedList<S2SPair>();
        tablePath = table.getAbsolutePath();
        try {
            BufferedReader input = new BufferedReader(new FileReader(table));
            String line = null;
            int lineNumber = 0;
            while ((line = input.readLine()) != null) {
                lineNumber++;
                if (!line.isEmpty() && !line.equals("s2s")
                        && !line.startsWith("//") && !line.startsWith("#")) {
                    String[] columns = LookupUtils.splitString(line);
                    handleLine(columns, lineNumber, line);
                }
            }
        } catch (FileNotFoundException e) {
            statusHandler.handle(Priority.CRITICAL, e.getLocalizedMessage(), e);
        } catch (IOException e) {
            statusHandler.handle(Priority.CRITICAL, e.getLocalizedMessage(), e);
        }
    }

    /**
     * decide what to do for each line
     * 
     * @param columns
     *            line split on spaces
     * @param lineNumber
     *            the line number in the file
     * @param line
     *            the entire line
     */
    private void handleLine(String[] columns, int lineNumber, String line) {
        String key = columns[0];
        if (key.equals("default")) {
            handleDefault(columns, lineNumber, line);
        } else if (key.equals("left")) {
            handleLeft(columns, lineNumber, line);
        } else if (key.equals("pass")) {
            handlePass(columns, lineNumber, line);
        } else if (key.equals("right")) {
            handleRight(columns, lineNumber, line);
        } else {
            addPair(columns, lineNumber, line);
        }
    }

    /**
     * handle when the right keyword is found, if right was already set will
     * then treat as normal lookup pair
     * 
     * @param columns
     * @param lineNumber
     * @param line
     */
    private void handleRight(String[] columns, int lineNumber, String line) {
        if (foundRight) {
            addPair(columns, lineNumber, line);
        } else {
            if (columns.length > 1) {
                if (columns.length == 2) {
                    rightKey = columns[1];
                } else if (columns.length > 2) {
                    if (columns.length > 3) {
                        // warn
                        statusHandler
                                .handle(Priority.PROBLEM,
                                        "Unexpected line length for line \""
                                                + line
                                                + "\" on line number "
                                                + lineNumber
                                                + " in s2s table "
                                                + tablePath
                                                + ", ignoring the rest of the line past the first three columns");
                    }
                    rightKey = columns[1];
                    rightValue = columns[2];
                }
            } else {
                // warn that this line was too short, expected 2 to 3 elements
                statusHandler.handle(Priority.SIGNIFICANT,
                        "ERROR line length too short for line \"" + line
                                + "\" on line number " + lineNumber
                                + " in s2s table " + tablePath
                                + ", line must be at least two columns");
            }
        }
    }

    /**
     * handle pass keyword, if pass was already defined then the line is treated
     * as a normal lookup pair
     * 
     * @param columns
     * @param lineNumber
     * @param line
     */
    private void handlePass(String[] columns, int lineNumber, String line) {
        if (foundPass) {
            addPair(columns, lineNumber, line);
        } else {
            pass = true;
            foundPass = true;
        }
    }

    /**
     * handle left keyword, if left was already defined it's treated as a normal
     * lookup pair
     * 
     * @param columns
     * @param lineNumber
     * @param line
     */
    private void handleLeft(String[] columns, int lineNumber, String line) {
        if (foundLeft) {
            addPair(columns, lineNumber, line);
        } else {
            if (columns.length > 1) {
                if (columns.length == 2) {
                    leftKey = columns[1];
                } else if (columns.length > 2) {
                    if (columns.length > 3) {
                        // warn
                        statusHandler
                                .handle(Priority.PROBLEM,
                                        "Unexpected line length for line \""
                                                + line
                                                + "\" on line number "
                                                + lineNumber
                                                + " in s2s table "
                                                + tablePath
                                                + ", ignoring the rest of the line past the first three columns");
                    }
                    leftKey = columns[1];
                    leftValue = columns[2];
                }
            } else {
                // warn that this line was too short, expected 2 to 3 elements
                statusHandler.handle(Priority.SIGNIFICANT,
                        "ERROR line length too short for line \"" + line
                                + "\" on line number " + lineNumber
                                + " in s2s table " + tablePath
                                + ", line must be at least two columns");
            }
        }
    }

    /**
     * handle default keyword, treats as a normal lookup pair if it was already
     * defined
     * 
     * @param columns
     * @param lineNumber
     * @param line
     */
    private void handleDefault(String[] columns, int lineNumber, String line) {
        if (foundDefault) {
            addPair(columns, lineNumber, line);
        } else {
            // set the default
            if (columns.length > 1) {
                if (columns[1].equals(":")) {
                    String value = LookupUtils.handleAsciiToString(columns, 2);
                    defaultValue = value;
                } else if (columns.length != 2) {
                    // unexpected length
                    // warn then carry on
                    statusHandler
                            .handle(Priority.PROBLEM,
                                    "Unexpected line length for line \""
                                            + line
                                            + "\" on line number "
                                            + lineNumber
                                            + " in s2s table "
                                            + tablePath
                                            + ", ignoring the rest of the line past the first two columns");
                    defaultValue = columns[1];
                } else {
                    defaultValue = columns[1];
                }
            }
            foundDefault = true;
        }
    }

    /**
     * add a lookup pair
     * 
     * @param columns
     * @param lineNumber
     * @param line
     */
    private void addPair(String[] columns, int lineNumber, String line) {
        if (columns.length < 1) {
            return;
        }

        if (columns.length == 1) {
            lookupList.add(new S2SPair(columns[0], null));
        } else if (columns[1].equals(":")) {
            String value = LookupUtils.handleAsciiToString(columns, 2);
            lookupList.add(new S2SPair(columns[0], value));
        } else {
            if (columns.length > 2) {
                // warn
                statusHandler
                        .handle(Priority.PROBLEM,
                                "Unexpected line length for line "
                                        + lineNumber
                                        + " in s2s table "
                                        + tablePath
                                        + ", ignoring the rest of the line past the first two columns");
            }
            lookupList.add(new S2SPair(columns[0], columns[1]));
        }
    }

    @Override
    public String lookup(String key) {
        if (key == null) {
            return defaultValue;
        }
        String result = find(key);

        if (result != null) {
            return result;
        } else if (mode.toLowerCase().equals("recursive_translation")) {
            result = recursiveTranslation(key);
            return result;
        } else {
            if (pass) {
                return key;
            } else {
                return defaultValue;
            }
        }
    }

    /**
     * find the key in the lookup list
     * 
     * @param key
     * @return
     */
    private String find(String key) {
        Iterator<S2SPair> iter = lookupList.iterator();
        while (iter.hasNext()) {
            S2SPair pair = iter.next();
            if (key.equals(pair.getKey())) {
                if (pair.getValue() == null) {
                    return defaultValue;
                } else {
                    return pair.getValue();
                }
            }
        }
        return null;
    }

    /**
     * perform a recursive translation
     * 
     * @param key
     * @return
     */
    // adapted from StringLookup.java
    private String recursiveTranslation(String key) {
        String rval = null;

        String field = key;
        int j, k, c, n, m;
        String txtstr;

        // If required, perform an edit operation where we replace everthing
        // from the substring _left to the beginning of the string with the
        // string _leftTrans.
        n = key.length();
        if (foundLeft) {
            field = replaceLeft(field);
        }
        if (foundRight) {
            field = replaceRight(field);
        }

        // Try to translate the whole thing again.
        rval = find(field);
        if (rval != null) {
            return rval;
        }

        // Initialize the output with an invisible string the same length as
        // the input, loop through each possible substring size.
        int nLeft = n; // number of untranslated characters
        boolean none = true; // true if nothing has been translated.
        String output = this.invisibleString(n);
        for (m = n - 1; m >= 1; m--) {

            // If we have enough untranslated characters to translate a
            // substring
            // of this size, loop through each possible substring.
            if (nLeft < m || m > n - 1)
                continue;
            for (c = 0; c <= n - m; c++) {

                // If a translation for this substring is not found, go on.
                if (find(this.mid(field, c, m)) == null) {
                    continue;
                } else {
                    txtstr = find(this.mid(field, c, m));
                }

                // Replace input substring with unprintable characters, put
                // the translation in the output string.
                j = txtstr.length();
                k = n - (c + m);
                field = this.left(field, c) + this.invisibleString(j)
                        + this.right(field, k);
                output = this.left(output, c) + txtstr + this.right(output, k);

                // Update next substring position, number untranslated and total
                // string length.
                c += j - 1;
                nLeft -= m;
                none = false;
                n = field.length();
                if (nLeft < m)
                    break;
            }
        }

        // Make anything that is all unprintable an empty string.
        if (nLeft == 0)
            field = "";
        else if (none)
            return defaultValue;
        rval = output;

        return rval;
    }

    /**
     * create a string of spaces of a certain length
     * 
     * @param length
     * @return
     */
    // copied from StringLookup.java
    private String invisibleString(int length) {
        char character = ' ';
        char[] toReturn = new char[length];
        Arrays.fill(toReturn, character);
        return new String(toReturn);
    }

    /**
     * replace left if the left keyword was found
     * 
     * @param key
     * @return
     */
    private String replaceLeft(String key) {
        String rval = key;
        if (foundLeft) {
            int pos = rval.indexOf(leftKey);
            if (pos >= 0) {
                rval = leftValue + rval.substring(pos);
            }
        }
        return rval;
    }

    /**
     * replace right if the right keyword was found
     * 
     * @param key
     * @return
     */
    private String replaceRight(String key) {
        String rval = key;
        if (foundRight) {
            int pos = rval.lastIndexOf(rightKey);
            if (pos >= 0) {
                rval = rval.substring(0, pos + rightKey.length()) + rightValue;
            }
        }
        return rval;
    }

    /**
     * copied from StringLookup.java
     * 
     * @param stringToParse
     * @param start
     * @param length
     * @return
     */
    // copied from StringLookup.java
    private String mid(String stringToParse, int start, int length) {
        int lStart = start;
        int lLength = length;
        if (lStart >= stringToParse.length()) {
            lStart = stringToParse.length();
        }
        lLength = Math.min(stringToParse.length() - lStart, lLength);
        String returnString = null;
        try {
            returnString = stringToParse.substring(lStart, lStart + lLength);
        } catch (Exception e) {
            statusHandler.handle(Priority.EVENTA, e.getLocalizedMessage(), e);
        }
        return returnString;

    }

    /**
     * copied from StringLookup.java
     * 
     * @param stringToParse
     * @param length
     * @return
     */
    // /copied from StringLookup.java
    private String left(String stringToParse, int length) {
        if (length >= stringToParse.length()) {
            return stringToParse;
        } else {
            return stringToParse.substring(0, length);
        }
    }

    /**
     * copied from StringLookup.java
     * 
     * @param stringToParse
     * @param length
     * @return
     */
    // copied from StringLookup.java
    private String right(String stringToParse, int length) {
        if (length >= stringToParse.length()) {
            return stringToParse;
        } else {
            return stringToParse.substring(stringToParse.length() - length,
                    stringToParse.length());
        }
    }

    @Override
    public void setMode(String mode) {
        this.mode = mode;
    }
}
