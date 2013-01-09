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
package com.raytheon.uf.viz.datadelivery.utils;

import java.util.List;

/**
 * Data Set Frequency Enumeration.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jan 8, 2013    1420     mpduff      Initial creation.
 * 
 * </pre>
 * 
 * @author mpduff
 * @version 1.0
 */

public enum DataSetFrequency {
    HOURLY, SIX_HOURLY;

    private DataSetFrequency() {

    }

    public static DataSetFrequency fromCycleTimes(List<Integer> cycleTimes) {
        if (cycleTimes.size() > 1) {
            if ((cycleTimes.get(1) - cycleTimes.get(0)) == 1) {
                return DataSetFrequency.HOURLY;
            } else {
                return DataSetFrequency.SIX_HOURLY;
            }
        }

        return DataSetFrequency.SIX_HOURLY;
    }
}
