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
package com.raytheon.viz.pointdata.lookup;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileReader;
import java.util.Iterator;
import java.util.LinkedList;

import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;

/**
 * Range lookup table for SVG plots
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * ???                      ???         Initial creation
 * Sep 16, 2014 2707       bclement    removed warnings, closed input
 * 
 * </pre>
 * 
 * @author bclement
 * @version 1.0
 */
public class R2SLookupTable implements IAbstractLookupTable {

    private class R2SPair {
        private final Double low;

        private final Double high;

        private final String value;

        public R2SPair(Double low, Double high, String value) {
            this.low = low;
            this.high = high;
            this.value = value;
        }

        /**
         * get the low value of the range
         * 
         * @return
         */
        public Double getLow() {
            return low;
        }

        /**
         * get the high value of the range
         * 
         * @return
         */
        public Double getHigh() {
            return high;
        }

        /**
         * get the string the range maps to
         * 
         * @return
         */
        public String getValue() {
            return value;
        }

        @Override
        public boolean equals(Object obj) {
            if (obj == null) {
                return false;
            }

            if (!this.getClass().equals(obj.getClass())) {
                return false;
            }

            R2SPair rhs = (R2SPair) obj;

            // check low
            if (getLow() == null) {
                if (rhs.getLow() != null) {
                    return false;
                }
            } else if (!getLow().equals(rhs.getLow())) {
                return false;
            }

            // check high
            if (getHigh() == null) {
                if (rhs.getHigh() != null) {
                    return false;
                }
            } else if (!getHigh().equals(rhs.getHigh())) {
                return false;
            }

            // check value
            if (getValue() == null) {
                if (rhs.getValue() != null) {
                    return false;
                }
            } else if (!getValue().equals(rhs.getValue())) {
                return false;
            }

            return true;
        }
    }

    LinkedList<R2SPair> lookupList = null;

    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(R2SLookupTable.class);

    private String defaultValue = "";

    private boolean foundDefault = false;

    String tablePath = "";

    public R2SLookupTable(File table) {
        lookupList = new LinkedList<R2SPair>();
        tablePath = table.getAbsolutePath();

        try (BufferedReader input = new BufferedReader(new FileReader(table))) {
            String line = null;
            int lineNumber = 0;
            while ((line = input.readLine()) != null) {
                lineNumber++;
                if (!line.isEmpty() && !line.equals("r2s")
                        && !line.startsWith("//") && !line.startsWith("#")) {
                    String[] columns = LookupUtils.splitString(line);

                    if (columns.length == 1) {
                        handleLineLength1(columns, lineNumber, line);
                    } else if (columns.length == 2) {
                        handleLineLength2(columns, lineNumber, line);
                    } else if (columns.length > 2) {
                        handleLineLengthGreater2(columns, lineNumber, line);
                    }
                }
            }
        } catch (Exception e) {
            statusHandler.error("Unable to parse SVG table: " + table, e);
        }
    }

    private void handleLineLength1(String[] columns, int lineNumber, String line) {
        // check for default
        if (columns[0].equals("default")) {
            if (foundDefault) {
                // send notification that we found a duplicate
                // default
                statusHandler.handle(Priority.EVENTB,
                        "Found \"default\" key word on line " + lineNumber
                                + " of r2s table " + tablePath
                                + " after a default was already defined");
            }
            // do nothing because the default is already a blank
            // string
            foundDefault = true;
        } else {
            // invalid line with single item
            statusHandler.handle(Priority.EVENTA,
                    "Found and skipped invalid line \"" + line + "\" on line "
                            + lineNumber + " of r2s table " + tablePath);
        }
    }

    private void handleLineLength2(String[] columns, int lineNumber, String line) {
        if (columns[0].equals("default")) {
            if (foundDefault) {
                // send notice
                statusHandler.handle(Priority.EVENTB,
                        "Found \"default\" key word on line " + lineNumber
                                + " of r2s table " + tablePath
                                + " after a default was already defined");
            } else {
                foundDefault = true;
                defaultValue = columns[1];
            }
        } else {
            // range that maps to the default
            // check that the two numbers are valid
            try {
                Double low = Double.parseDouble(columns[0]);
                Double high = Double.parseDouble(columns[1]);
                lookupList.add(new R2SPair(low, high, null));
            } catch (NumberFormatException e) {
                statusHandler.handle(Priority.PROBLEM,
                        "(Skipped invalid line): Number format error in line \""
                                + line + "\" on line " + lineNumber
                                + " of r2s table " + tablePath, e);
            }
        }
    }

    private void handleLineLengthGreater2(String[] columns, int lineNumber,
            String line) {
        // if the third column is a colon then expect ascii values
        if (columns[2].equals(":")) {
            // start with null because if there is nothing after the colon then
            // null will trigger using default
            try {
                String value = LookupUtils.handleAsciiToString(columns, 2);
                Double low = Double.parseDouble(columns[0]);
                Double high = Double.parseDouble(columns[1]);
                lookupList.add(new R2SPair(low, high, value));
            } catch (NumberFormatException e) {
                statusHandler.handle(Priority.PROBLEM,
                        "(Skipped invalid line): Number format error in line \""
                                + line + "\" on line " + lineNumber
                                + " of r2s table " + tablePath, e);
            }
        }
        // otherwise then there should only be three sections
        else if (columns.length == 3) {
            try {
                Double low = Double.parseDouble(columns[0]);
                Double high = Double.parseDouble(columns[1]);
                lookupList.add(new R2SPair(low, high, columns[2]));
            } catch (NumberFormatException e) {
                statusHandler.handle(Priority.PROBLEM,
                        "(Skipped invalid line): Number format error in line \""
                                + line + "\" on line " + lineNumber
                                + " of r2s table " + tablePath, e);
            }
        } else {
            // invalid line
            statusHandler.handle(Priority.EVENTA,
                    "Found and skipped invalid line \"" + line + "\" on line "
                            + lineNumber + " of r2s table " + tablePath);
        }
    }

    @Override
    public String lookup(String key) {
        if (key == null) {
            return defaultValue;
        }
        // convert key to double
        try {
            Double lookupKey = Double.parseDouble(key);
            Iterator<R2SPair> iter = lookupList.iterator();
            while (iter.hasNext()) {
                R2SPair pair = iter.next();
                if (lookupKey.compareTo(pair.getLow()) >= 0
                        && lookupKey.compareTo(pair.getHigh()) <= 0) {
                    // we found it!
                    if (pair.getValue() == null) {
                        return defaultValue;
                    } else {
                        String s = pair.getValue();
                        s = s.replace("~", " ");
                        // this will match float format strings.
                        if (s.matches("%\\d\\.\\df")) {
                            s = String.format(s, Float.parseFloat(key));
                        }
                        return s;
                    }
                }
            }
            return defaultValue;
        } catch (NumberFormatException e) {
            // notify and return default
            statusHandler
                    .handle(Priority.SIGNIFICANT, "passed in invalid key \""
                            + key
                            + "\", must be a string representation of a double");
            return defaultValue;
        }
    }

    @Override
    public void setMode(String mode) {
    }

}
