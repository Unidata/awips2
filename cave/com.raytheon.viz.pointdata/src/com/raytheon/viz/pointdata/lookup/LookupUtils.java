package com.raytheon.viz.pointdata.lookup;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.IOException;

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
 * 
 * </pre>
 * 
 * @author ekladstrup
 * @version 1.0
 */
public class LookupUtils {
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
        try {
            BufferedReader input = new BufferedReader(new FileReader(table));
            String line = "";
            try {
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
                statusHandler.handle(Priority.SIGNIFICANT,
                        e.getLocalizedMessage(), e);
                return TableTypes.UNKNOWN;
            }
        } catch (FileNotFoundException e) {
            statusHandler.handle(Priority.CRITICAL, e.getLocalizedMessage(), e);
            return null;
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

    public static String[] splitString(String line) {
        String[] rval = line.split("\\s+");
        return rval;
    }
}
