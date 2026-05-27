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
package com.raytheon.uf.edex.plugin.mpe.dqcpreprocessor.output;

import java.util.Collection;
import java.util.LinkedList;
import java.util.List;

/**
 * Enumeration representing the synoptic hours associated with level 1
 * temperature data.
 * 
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Feb 15, 2018 7184       bkowal      Initial creation
 *
 * </pre>
 *
 * @author bkowal
 */

public enum SynopticHour {
    HOUR0, HOUR6, HOUR12, HOUR18;

    public static final Collection<SynopticHour> determineSynopticHourOrder(
            final TemperatureBasetime temperatureBasetime) {
        final List<SynopticHour> synopticHourOrder = new LinkedList<>();
        switch (temperatureBasetime) {
        case BASETIME_00Z:
            synopticHourOrder.add(HOUR0);
            synopticHourOrder.add(HOUR6);
            synopticHourOrder.add(HOUR12);
            synopticHourOrder.add(HOUR18);
            break;
        case BASETIME_06Z:
            synopticHourOrder.add(HOUR6);
            synopticHourOrder.add(HOUR12);
            synopticHourOrder.add(HOUR18);
            synopticHourOrder.add(HOUR0);
            break;
        case BASETIME_12Z:
            synopticHourOrder.add(HOUR12);
            synopticHourOrder.add(HOUR18);
            synopticHourOrder.add(HOUR0);
            synopticHourOrder.add(HOUR6);
            break;
        case BASETIME_18Z:
            synopticHourOrder.add(HOUR18);
            synopticHourOrder.add(HOUR0);
            synopticHourOrder.add(HOUR6);
            synopticHourOrder.add(HOUR12);
            break;
        }
        return synopticHourOrder;
    }
}