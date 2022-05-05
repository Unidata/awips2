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
import java.io.IOException;
import java.util.ArrayList;
import java.util.List;
import java.util.regex.Pattern;

import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;

/**
 * 
 * provides utility functions related to lookup tables
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Apr 21, 2011            ekladstrup     Initial creation
 * Sep 16, 2014 2707       bclement     added splitGroupedString()
 * 
 * </pre>
 * 
 * @author ekladstrup
 * @version 1.0
 */
public class LookupUtils {

    private static final Pattern WHITESPACE = Pattern.compile("\\s+");

    private static enum QuotedState {
        OUTSIDE_GROUP, IN_SINGLE_QUOTES, IN_DOUBLE_QUOTES, IN_REGULAR_BLOCK
    }

    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(LookupUtils.class);

    /**
     * 
     * <UL>
     * <LI><B>N2N</B> - a Number to Number table</LI>
     * <LI><B>S2S</B> - a String to String table</LI>
     * <LI><B>N2S</B> - a Number to String table</LI>
     * <LI><B>S2N</B> - a String to Number table</LI>
     * <LI><B>R2S</B> - a Range to String table</LI>
     * <LI><B>UNKNOWN</B> - an unknown table type</LI>
     * </UL>
     */
    public enum TableTypes {
        N2N, S2S, N2S, S2N, R2S, UNKNOWN
    }

    /**
     * gets the type of the table from the passed in file. The first line is
     * expected to be a three character code that matches a known type.
     * 
     * @param table
     *            the File to open
     * @return a TableTypes object of the type of the table or unknown for
     *         non-critical errors
     */
    public static TableTypes getTableType(File table) {
        try (BufferedReader input = new BufferedReader(new FileReader(table))) {
            String line = "";
                boolean stillLoop = true;
                while (stillLoop && (line = input.readLine()) != null) {
                    line = line.trim();
                    if (!line.startsWith("//") && !line.startsWith("#")) {
                        stillLoop = false;
                    }
                }
                if (line == null) {
                    line = "";
                }
                if (line.length() != 3) {
                    statusHandler
                            .handle(Priority.SIGNIFICANT,
                                    "Lookup table does not contain a three character type code on the first line.  File: "
                                            + table.getPath());
                    return TableTypes.UNKNOWN;
                } else {
                    if (line.equals("n2n")) {
                        return TableTypes.N2N;
                    } else if (line.equals("s2s")) {
                        return TableTypes.S2S;
                    } else if (line.equals("n2s")) {
                        return TableTypes.N2S;
                    } else if (line.equals("s2n")) {
                        return TableTypes.S2N;
                    } else if (line.equals("r2s")) {
                        return TableTypes.R2S;
                    } else {
                        return TableTypes.UNKNOWN;
                    }
                }
        } catch (IOException e) {
            statusHandler
                    .error("Problem parsing SVG config table: " + table, e);
            return TableTypes.UNKNOWN;
        }
    }

    /**
     * creates and returns an IAbstractLookupTable
     * 
     * @param table
     *            file with table information
     * @return a lookup table
     */
    public static IAbstractLookupTable buildLookupTable(File table) {
        TableTypes type = getTableType(table);

        IAbstractLookupTable rval = null;
        switch (type) {
        case N2N:
            break;
        case S2S:
            rval = new S2SLookupTable(table);
            break;
        case N2S:
            rval = new N2SLookupTable(table);
            break;
        case S2N:
            break;
        case R2S:
            rval = new R2SLookupTable(table);
            break;
        case UNKNOWN:
            break;
        }

        if (rval == null) {
            statusHandler.handle(Priority.CRITICAL,
                    "Unsupported lookup table type, returning null");
        }
        return rval;
    }

    /**
     * convert a list from ascii codes to a string
     * 
     * @param columns
     *            the array of ascii codes in string format
     * @param start
     *            the index in the array to start parsing
     * @return
     */
    public static String handleAsciiToString(String[] columns, int start) {
        String rval = "";
        for (int i = start; i < columns.length; ++i) {
            if (!rval.isEmpty()) {
                rval += " ";
            }
            rval += columns[i];
        }
        return rval;
    }

    /**
     * Split string on whitespaces
     * 
     * @param line
     * @return
     */
    public static String[] splitString(String line) {
        return WHITESPACE.split(line);
    }

    /**
     * Split string on whitespaces with double or single quoted groups. Any
     * character (including whitespaces) inside a quoted group will be a single
     * string in the returned array. Double quotes can be nested inside of
     * single quotes and vice versa.
     * 
     * @param line
     * @return empty array if line is empty
     */
    public static String[] splitGroupedString(String line) {
        QuotedState state = QuotedState.OUTSIDE_GROUP;
        List<String> rval = new ArrayList<String>();
        StringBuilder sb = new StringBuilder();
        /* iterate over string; splitting out groups */
        for (int i = 0; i < line.length(); ++i) {
            char c = line.charAt(i);
            if (state == QuotedState.OUTSIDE_GROUP) {
                /* outside of a group all whitespace is ignored */
                if (!Character.isWhitespace(c)) {
                    /* any non-whitespace transitions inside group */
                    if (c == '\'') {
                        state = QuotedState.IN_SINGLE_QUOTES;
                    } else if (c == '"') {
                        state = QuotedState.IN_DOUBLE_QUOTES;
                    } else {
                        state = QuotedState.IN_REGULAR_BLOCK;
                        sb.append(c);
                    }
                }
            } else {
                /* inside a group, look for transition out of group */
                QuotedState newState = null;
                switch (state) {
                case IN_REGULAR_BLOCK:
                    if (Character.isWhitespace(c)) {
                        newState = QuotedState.OUTSIDE_GROUP;
                    } else if (c == '\'') {
                        newState = QuotedState.IN_SINGLE_QUOTES;
                    } else if (c == '"') {
                        newState = QuotedState.IN_DOUBLE_QUOTES;
                    }
                    break;
                case IN_SINGLE_QUOTES:
                    if (c == '\'') {
                        newState = QuotedState.OUTSIDE_GROUP;
                    }
                    break;
                case IN_DOUBLE_QUOTES:
                    if (c == '"') {
                        newState = QuotedState.OUTSIDE_GROUP;
                    }
                    break;
                default:
                }
                if (newState != null) {
                    /* state changed, leave group */
                    state = newState;
                    rval.add(sb.toString());
                    sb = new StringBuilder();
                } else {
                    sb.append(c);
                }
            }
        }
        /* add last group */
        if (sb.length() > 0) {
            rval.add(sb.toString());
        }
        return rval.toArray(new String[rval.size()]);
    }

}
