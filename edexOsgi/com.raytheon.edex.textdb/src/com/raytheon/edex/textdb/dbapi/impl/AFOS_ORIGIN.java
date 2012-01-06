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
package com.raytheon.edex.textdb.dbapi.impl;

import java.util.HashMap;

/**
 * TODO Add Description
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Nov 13, 2008            jkorman     Initial creation
 * </pre>
 * 
 * @author jkorman
 * @version 1.0
 */

public enum AFOS_ORIGIN {
    Regional("R"), East("E"), Central("C"), West("W"), International("I");

    private final String originKey;

    private static final HashMap<String, AFOS_ORIGIN> ORIGIN_MAP = new HashMap<String, AFOS_ORIGIN>();
    static {
        for (AFOS_ORIGIN origin : AFOS_ORIGIN.values()) {
            ORIGIN_MAP.put(origin.originKey, origin);
        }
    }

    private AFOS_ORIGIN(String key) {
        originKey = key;
    }

    /**
     * 
     * @return
     */
    public String getOriginKey() {
        return originKey;
    }

    /**
     * 
     * @param key
     * @return
     */
    public static AFOS_ORIGIN getFromOriginKey(String key) {
        return ORIGIN_MAP.get(key);
    }

}
