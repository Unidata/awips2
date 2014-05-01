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

/**
 * A comparator for sorting DbData objects by table name.
 * 
 * @author wldougher
 * 
 */
public class TabNameComp implements Comparator<DbData> {

    @Override
    public int compare(DbData o1, DbData o2) {
        int val = 0;
        if (o1 == null && o2 == null) {
            val = 0;
        } else if (o1 == null) {
            // put nulls at the head of the list so iterations that begin by
            // seeking a table name automatically skip them. That way, the
            // iteration code doesn't have to watch for nulls.
            val = -1;
        } else if (o2 == null) {
            val = 1;
        } else {
            val = o1.tableName.compareTo(o2.tableName);
            if (val == 0) {
                if (o1.cwa == null && o2.cwa == null) {
                    val = 0;
                } else if (o1.cwa == null) {
                    val = -1;
                } else if (o2.cwa == null) {
                    val = 1;
                } else {
                    val = o1.cwa.compareTo(o2.cwa);
                }
            }
            if (val == 0) {
                if (o1.zone == null && o2.zone == null) {
                    val = 0;
                } else if (o1.zone == null) {
                    val = -1;
                } else if (o2.zone == null) {
                    val = 1;
                } else {
                    val = o1.zone.compareTo(o2.zone);
                }
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
