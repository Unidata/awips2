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
package com.raytheon.edex.util;

import java.util.Arrays;
import java.util.HashMap;

import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.common.time.DataTime.SortKey;

/**
 * Utility methods pertaining to time
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Nov 20, 2007            njensen     Initial creation
 * 
 * </pre>
 * 
 * @author njensen
 * @version 1.0
 */

public class TimeSeriesUtil {

    public static PluginDataObject[] sortByTime(PluginDataObject[] unsorted,
            SortKey majorKey, SortKey minorKey) {
        int size = unsorted.length;
        PluginDataObject[] sorted = new PluginDataObject[size];
        DataTime[] dt = new DataTime[size];
        HashMap<DataTime, PluginDataObject> map = new HashMap<DataTime, PluginDataObject>();
        for (int i = 0; i < size; i++) {
            DataTime time = unsorted[i].getDataTime();
            time.setSortKeys(majorKey, minorKey, false);
            dt[i] = time;
            map.put(time, unsorted[i]);
        }

        Arrays.sort(dt);
        for (int i = 0; i < size; i++) {
            sorted[i] = map.get(dt[i]);
        }
        return sorted;
    }

}
