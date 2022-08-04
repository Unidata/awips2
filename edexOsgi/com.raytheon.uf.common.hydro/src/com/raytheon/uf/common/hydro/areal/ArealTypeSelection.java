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
package com.raytheon.uf.common.hydro.areal;

import java.util.EnumSet;
import java.util.HashMap;
import java.util.Map;

/**
 * Hydro's different areal types.
 * 
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Aug 31, 2018   6979     mpduff      Initial creation
 * Mar 11, 2020  19533  mgamazaychikov Added lookup map
 *
 * </pre>
 *
 * @author mpduff
 */

public enum ArealTypeSelection {
    ZONES("Zones", "ZONE"),
    COUNTIES("Counties", "COUNTY"),
    BASINS("Basins", "BASIN"),
    RESERVOIRS("Reservoirs", "RESRVR");

    private static final Map<Integer, ArealTypeSelection> lookup = new HashMap<>();

    static {
        int ordinal = 0;
        for (ArealTypeSelection ats : EnumSet.allOf(ArealTypeSelection.class)) {
            lookup.put(ordinal, ats);
            ordinal += 1;
        }
    }

    private String displayString;

    private String dataName;

    private ArealTypeSelection(String displayString, String dataName) {
        this.displayString = displayString;
        this.dataName = dataName;
    }

    public String toDisplayString() {
        return this.displayString;
    }

    /**
     * @return the data name for this enum
     */
    public String getDataName() {
        return this.dataName;
    }

    public static ArealTypeSelection fromOrdinal(int ordinal) {
        return lookup.get(ordinal);
    }
}
