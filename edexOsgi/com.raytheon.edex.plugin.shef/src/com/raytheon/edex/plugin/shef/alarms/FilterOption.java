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
package com.raytheon.edex.plugin.shef.alarms;

import java.util.ArrayList;

/**
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * June 15, 2011    9377     jnjanga     Initial creation
 * 
 * 
 * </pre>
 * 
 * @author jnjanga
 * @version 1.0
 */

public enum FilterOption {

    OBSERVED('O'), FORECAST('F'), ALERTS('T'), ALARMS('M'), RATE_OF_CHANGE('R'), LOWER_LIMIT(
            'L'), UPPER_LIMIT('U'), DIFF_LIMIT('D');

    char arg;

    FilterOption(char arg) {
        this.arg = arg;
    }

    public String toString() {
        return Character.toString(arg);
    }

    public char getArg() {
        return arg;
    }

    public static ArrayList<Character> asList() {
        ArrayList<Character> vals = new ArrayList<Character>();
        for (FilterOption opt : values())
            vals.add(opt.getArg());
        return vals;
    }
}