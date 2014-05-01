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
/**
 * 
 */
package com.raytheon.viz.gfe.rsc.zones;

import java.util.Comparator;

public class ZoneNameComp implements Comparator<DbData> {

    /**
     * The database record sometimes has nulls. This is just a string compare,
     * guarded by a couple of branches that handle nulls in either parameter.
     * 
     * @param s1
     *            The first string to compare (can be null).
     * @param s2
     *            The second string to compare (can be null).
     * @return The comparison of the two strings. The comparison sorts nulls as
     *         coming "before" any non-null string.
     */
    private int nullSafeCmp(String s1, String s2) {
        int val;
        if (s1 == null && s2 == null) {
            val = 0;
        } else if (s1 == null) {
            val = -1;
        } else if (s2 == null) {
            val = 1;
        } else {
            val = s1.compareTo(s2);
        }
        return val;
    }

    @Override
    public int compare(DbData o1, DbData o2) {
        int val = 0;
        if (o1 == null && o2 == null) {
            val = 0;
        } else if (o1 == null) {
            // put nulls at the head of the list so iterations that begin by
            // seeking a zone name automatically skip them. That way, the
            // iteration code doesn't have to watch for nulls.
            val = -1;
        } else if (o2 == null) {
            val = 1;
        } else {
            val = nullSafeCmp(o1.zone, o2.zone);

            // tableName is sometimes empty (for dummy search-key values),
            // but should never be null, so use String compare for speed.
            if (val == 0) {
                val = o1.tableName.compareTo(o2.tableName);
            }

            if (val == 0) {
                val = nullSafeCmp(o1.cwa, o2.cwa);
            }

            if (val == 0 && o1.wkb != o2.wkb) {
                if (o1.wkb == null) {
                    val = -1;
                } else if (o2.wkb == null) {
                    val = 1;
                } else {
                    val = o1.wkb.length - o2.wkb.length;
                    if (val == 0) {
                        for (int wkbIdx = 0; wkbIdx < o1.wkb.length && val == 0; wkbIdx++) {
                            val = o1.wkb[wkbIdx] - o2.wkb[wkbIdx];
                        }
                    }
                }
            }
        }
        return val;
    }
}
