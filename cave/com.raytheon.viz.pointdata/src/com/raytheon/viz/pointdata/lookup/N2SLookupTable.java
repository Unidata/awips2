package com.raytheon.viz.pointdata.lookup;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.IOException;
import java.util.Iterator;
import java.util.LinkedList;

import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;

/**
 * 
 * Implementation of a number to number lookup table
 * 
 * <h4>4.3) Number to string lookup table</h4> (from AWIPS1
 * adaptivePlanViewPlotting.html)
 * 
 * <p>
 * The first line of a number to string lookup table is "n2s". An n2s lookup
 * table is used to convert some arbitrary numeric value to a string. Most
 * commonly, entries are a single number or a pair of numbers (representing a
 * range), space delimited, followed by a result string. If there are
 * overlapping ranges, order is important because the first range that matches
 * will be used. Sometimes, there is a need to have characters in the result
 * string that are unprintable in ASCII. In that case, the result string can be
 * expressed with a colon followed by a list of integers designating the
 * character codes (all space delimited). It is also possible to make an entry
 * with only one or a pair of numbers, which means that the result of that
 * lookup will be an empty string. Internally, numeric values in a number to
 * string table are held as doubles.
 * </p>
 * 
 * <p>
 * There is one special keyword that is recognized in a number to string table.
 * Normally, if an attempt is made to look up a number that does not exist in
 * the table or is outside all specified ranges, the output will be an empty
 * string. Using the keyword `default' allows one to supply a different string
 * to be output in the case of a failed lookup.
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
public class N2SLookupTable implements IAbstractLookupTable {

    private class N2SPair {
        private Double key;

        private String value;

        public N2SPair(Double key, String value) {
            this.key = key;
            this.value = value;
        }

        public Double getKey() {
            return key;
        }

        public void setKey(Double key) {
            this.key = key;
        }

        public String getValue() {
            return value;
        }

        public void setValue(String value) {
            this.value = value;
        }

        @Override
        public boolean equals(Object obj) {
            if (obj == null) {
                return false;
            }

            if (!obj.getClass().equals(this.getClass())) {
                return false;
            }

            N2SPair rhs = (N2SPair) obj;

            if (key == null) {
                if (rhs.getKey() != null) {
                    return false;
                }
            } else if (!key.equals(rhs.getKey())) {
                return false;
            }

            if (value == null) {
                if (rhs.getValue() != null) {
                    return false;
                }
            } else if (!value.equals(rhs.getValue())) {
                return false;
            }

            return true;
        }
    }

    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(N2SLookupTable.class);

    private String mode = "";

    /**
     * default value, for keys that do not specify a value in the table or other
     * special cases as described in the Awips1 documentation
     */
    private String defaultValue = "";

    private LinkedList<N2SPair> lookupList = null;

    private boolean foundDefault = false;

    private String tablePath = "";

    public N2SLookupTable(File table) {
        lookupList = new LinkedList<N2SPair>();
        tablePath = table.getAbsolutePath();
        try {
            BufferedReader input = new BufferedReader(new FileReader(table));
            String line = null;
            int lineNumber = 0;
            while ((line = input.readLine()) != null) {
                lineNumber++;
                // skip lines that specify this as a n2s file and commented
                // lines
                if (!line.isEmpty() && !line.equals("n2s")
                        && !line.startsWith("//") && !line.startsWith("#")) {
                    // split the line on spaces
                    String[] split = LookupUtils.splitString(line);

                    if (split.length > 0) {
                        handleLine(split, lineNumber, line);
                    }
                }
            }
        } catch (FileNotFoundException e) {
            statusHandler.handle(Priority.CRITICAL, e.getLocalizedMessage(), e);
        } catch (IOException e) {
            statusHandler.handle(Priority.CRITICAL, e.getLocalizedMessage(), e);
        }
    }

    private void handleLine(String[] columns, int lineNumber, String line) {
        if (columns.length < 1) {
            return;
        }

        String key = columns[0];

        if (key.equals("default")) {
            handleDefault(columns, lineNumber, line);
        } else {
            addPair(columns, lineNumber, line);
        }
    }

    private void handleDefault(String[] columns, int lineNumber, String line) {
        if (foundDefault) {
            // notify of the error and ignore
            statusHandler.handle(Priority.EVENTB,
                    "Found \"default\" key word in line \"" + line
                            + "\" on line number " + lineNumber
                            + " of n2s table " + tablePath
                            + " after a default was already defined");
        } else {
            if (columns.length == 1) {
                // ignore, default is already a blank string
            } else if (columns[1].equals(":")) {
                String value = LookupUtils.handleAsciiToString(columns, 2);
                defaultValue = value;
            } else {
                if (columns.length > 2) {
                    // notify
                    statusHandler
                            .handle(Priority.PROBLEM,
                                    "line \""
                                            + line
                                            + "\" on line number "
                                            + lineNumber
                                            + " of n2s table "
                                            + tablePath
                                            + " it too long, ignoring columns after the first two");
                }
                foundDefault = true;
                defaultValue = columns[1];
            }
        }
    }

    private void addPair(String[] columns, int lineNumber, String line) {
        try {
            if (columns.length == 1) {
                Double key = Double.parseDouble(columns[0]);
                lookupList.add(new N2SPair(key, null));
            } else if (columns[1].equals(":")) {
                Double key = Double.parseDouble(columns[0]);
                String value = LookupUtils.handleAsciiToString(columns, 2);
                lookupList.add(new N2SPair(key, value));
            } else {
                if (columns.length > 2) {
                    // notify
                    statusHandler
                            .handle(Priority.PROBLEM,
                                    "line \""
                                            + line
                                            + "\" on line number "
                                            + lineNumber
                                            + " of n2s table "
                                            + tablePath
                                            + " it too long, ignoring columns after the first two");
                }
                Double key = Double.parseDouble(columns[0]);
                lookupList.add(new N2SPair(key, columns[1]));
            }
        } catch (NumberFormatException e) {
            statusHandler.handle(Priority.CRITICAL, e.getLocalizedMessage(), e);
        }
    }

    /**
     * Returns a string representation of the lookup value for the passed in key
     * 
     * @param key
     *            a string representation of the lookup key
     * @return a string representation of the lookup value
     */
    @Override
    public String lookup(String key) {
        if (key == null) {
            return defaultValue;
        }
        // key should be a double, if not return the default
        try {
            Double dKey = Double.parseDouble(key);
            // look for the key
            Iterator<N2SPair> iter = lookupList.iterator();
            while (iter.hasNext()) {
                N2SPair pair = iter.next();

                if (dKey.equals(pair.getKey())) {
                    if (pair.getValue() == null) {
                        return defaultValue;
                    }
                    return pair.getValue();
                }
            }
            statusHandler.handle(Priority.VERBOSE, "key: " + key
                    + " not in lookup table, returning default");
            return defaultValue;
        } catch (NumberFormatException e) {
            statusHandler.handle(Priority.SIGNIFICANT,
                    "Returning the default, key for lookup was not a valid Double: "
                            + key);
            return defaultValue;
        }
    }

    @Override
    public void setMode(String mode) {
        this.mode = mode;
    }

}
