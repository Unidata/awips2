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
package com.raytheon.uf.viz.d2d.gfe.rsc;

import java.time.Instant;
import java.time.temporal.ChronoUnit;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Date;
import java.util.List;

import com.raytheon.uf.common.dataplugin.gfe.db.objects.GFERecord;
import com.raytheon.uf.common.dataquery.requests.TimeQueryRequest;
import com.raytheon.uf.common.inventory.exception.DataCubeException;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.common.time.SimulatedTime;
import com.raytheon.uf.common.time.util.TimeUtil;
import com.raytheon.uf.viz.datacube.DefaultDataCubeAdapter;

/**
 * DataCube adapter for GFE data.
 * 
 * <pre>
 *
 * SOFTWARE HISTORY
 * 
 * Date          Ticket#  Engineer  Description
 * ------------- -------- --------- ------------------
 * May 29, 2019  6613     bhurley   Initial creation.
 * 
 * </pre>
 *
 * @author bhurley
 */

public class GFEDataCubeAdapter extends DefaultDataCubeAdapter {

    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(GFEDataCubeAdapter.class);

    public GFEDataCubeAdapter() {
        super(GFERecord.PLUGIN_NAME);
    }

    @Override
    public List<List<DataTime>> timeQuery(List<TimeQueryRequest> requests)
            throws DataCubeException {
        List<List<DataTime>> queryResult = super.timeQuery(requests);
        List<List<DataTime>> retVal = new ArrayList<>();

        for (List<DataTime> times : queryResult) {
            // We want to return times starting with the current hour. So remove
            // times that occur before the current hour. If no time exists for
            // the current hour, then we'll use the closest time that occurs
            // after the current hour.
            Collections.sort(times);
            Instant timeAtCurrentHour = SimulatedTime.getSystemTime().getTime()
                    .toInstant().truncatedTo(ChronoUnit.HOURS);
            long timeAtCurrentHourMilli = timeAtCurrentHour.toEpochMilli();
            times.removeIf(i -> i.getMatchRef() < timeAtCurrentHourMilli);

            long matchedTimes = times.stream()
                    .filter(i -> i.getMatchRef() == timeAtCurrentHourMilli)
                    .count();
            if (matchedTimes < 1) {
                statusHandler.warn("A time for the current hour ("
                        + timeAtCurrentHour.toString()
                        + ") is not available; using the closest time that occurs after the current hour.");
            }

            // Now create new modified times that all have the same reference
            // time and an appropriate forecast value. This needs to be done so
            // time matching is performed in the same way as for regular model
            // data.
            List<DataTime> newTimes = new ArrayList<>(times.size());
            if (!times.isEmpty()) {
                Date firstRefTime = times.get(0).getRefTime();
                long firstRefTimeMillis = firstRefTime.getTime();
                for (DataTime time : times) {
                    long refTimeMillis = time.getRefTime().getTime();
                    Long numberOfSeconds = (refTimeMillis - firstRefTimeMillis)
                            / TimeUtil.MILLIS_PER_SECOND;
                    DataTime newTime = time.clone();
                    newTime.setFcstTime(numberOfSeconds.intValue());
                    newTime.setRefTime(firstRefTime);
                    newTimes.add(newTime);
                }
            }
            retVal.add(newTimes);
        }

        return retVal;
    }
}
