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
package com.raytheon.uf.common.stats.util;

import java.util.Collections;
import java.util.HashMap;
import java.util.Map;

import com.raytheon.uf.common.units.DataSizeUnit;

/**
 * Data view enumeration.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jan 18, 2013    1357    mpduff      Moved to its own file.
 * 
 * </pre>
 * 
 * @author djohnson
 * @version 1.0
 */

public enum DataView {
    AVG("Average"), MIN("Minimum"), MAX("Maximum"), SUM("Sum"), COUNT("Count");

    private final String view;

    private DataView(String view) {
        this.view = view;
    }

    public String getView() {
        return view;
    }

    private static final Map<String, DataView> LOOKUP_MAP;
    static {
        Map<String, DataView> map = new HashMap<String, DataView>();
        for (DataView view : DataView.values()) {
            map.put(view.getView(), view);
        }
        LOOKUP_MAP = Collections.unmodifiableMap(map);
    }

    /**
     * Retrieve the {@link DataSizeUnit} for its string representation.
     * 
     * @param asString
     * @return
     */
    public static DataView fromString(String asString) {
        return LOOKUP_MAP.get(asString);
    }

}