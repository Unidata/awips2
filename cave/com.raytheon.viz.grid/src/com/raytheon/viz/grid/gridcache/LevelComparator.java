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
package com.raytheon.viz.grid.gridcache;

import java.util.Comparator;

import com.raytheon.uf.common.dataplugin.grib.GribRecord;
import com.raytheon.uf.common.dataplugin.level.CompareType;

/**
 * Grib layer comparator.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Nov 12, 2009 3579       mpduff      Initial creation
 * 
 * </pre>
 * 
 * @author mpduff
 * @version 1.0
 */

public class LevelComparator implements Comparator<GribRecord> {
    /**
     * Compare the two objects.
     * 
     * @return 1 if above, -1 if below, 0 if equal
     */
    public int compare(GribRecord record, GribRecord otherRecord) {
        CompareType ct = record.getModelInfo().getLevel().compare(
                otherRecord.getModelInfo().getLevel());

        if (ct == CompareType.ABOVE) {
            return 1;
        } else if (ct == CompareType.BELOW) {
            return -1;
        } else {
            return 0;
        }
    }
}
