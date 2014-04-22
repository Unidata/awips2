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
package com.raytheon.uf.common.nc.bufr.tables;

import java.util.regex.Matcher;
import java.util.regex.Pattern;

import com.raytheon.uf.common.nc.bufr.tables.TranslationTable.TableType;

/**
 * Parsed representation of unit value for fields that are code or flag table
 * mappings.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Mar 27, 2014 2905       bclement     Initial creation
 * 
 * </pre>
 * 
 * @author bclement
 * @version 1.0
 */
public class ParsedTableUnit {

    public static final Pattern TABLE_UNIT_PATTERN = Pattern
            .compile("^\\s*(\\S+)\\s+(\\d+)-(\\d+)-(\\d+).*$");

    public static final String TABLE_ID_FORMAT = "%d %02d %03d";

    public static final String CODE_TABLE_TYPE = "CodeTable";

    public static final String FLAG_TABLE_TYPE = "FlagTable";

    private final TableType type;

    private final String tableId;

    /**
     * @param type
     * @param tableId
     */
    public ParsedTableUnit(TableType type, String tableId) {
        this.type = type;
        this.tableId = tableId;
    }

    /**
     * Parse unit string from BUFR file into new object
     * 
     * @param tableUnitString
     * @return
     * @throws IllegalArgumentException
     */
    public static ParsedTableUnit parse(String tableUnitString)
            throws IllegalArgumentException {
        Matcher m = TABLE_UNIT_PATTERN.matcher(tableUnitString);
        if (!m.matches()) {
            throw new IllegalArgumentException(
                    "Invalid BUFR table unit string: " + tableUnitString);
        }
        TableType type = matchType(m.group(1));
        int f = Integer.valueOf(m.group(2));
        int x = Integer.valueOf(m.group(3));
        int y = Integer.valueOf(m.group(4));
        return new ParsedTableUnit(type, String.format(TABLE_ID_FORMAT, f, x, y));
    }

    /**
     * @param typeStr
     * @return type of BUFR table
     * @throws IllegalArgumentException
     *             if table type is unknown
     */
    private static TableType matchType(String typeStr)
            throws IllegalArgumentException {
        TableType rval;
        if (typeStr.equalsIgnoreCase(CODE_TABLE_TYPE)) {
            rval = TableType.CODE;
        } else if (typeStr.equalsIgnoreCase(FLAG_TABLE_TYPE)) {
            rval = TableType.FLAG;
        } else {
            throw new IllegalArgumentException("Unknown BUFR table type: "
                    + typeStr);
        }
        return rval;
    }

    /**
     * @return the type
     */
    public TableType getType() {
        return type;
    }

    /**
     * @return the tableId
     */
    public String getTableId() {
        return tableId;
    }

}
