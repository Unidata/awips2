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
package com.raytheon.uf.common.time.util;

import java.util.LinkedHashMap;
import java.util.Map.Entry;

import com.raytheon.uf.common.status.IPerformanceStatusHandler;
import com.raytheon.uf.common.time.domain.Durations;
import com.raytheon.uf.common.time.domain.TimePoints;
import com.raytheon.uf.common.time.domain.api.IDuration;
import com.raytheon.uf.common.time.domain.api.ITimePoint;

/**
 * Base implementation of {@link IPerformanceTimer}. Intentionally
 * package-private, all access should be through the public API method of
 * {@link TimeUtil#getPerformanceTimer()}.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jun 14, 2013 2095       djohnson     Initial creation
 * 
 * </pre>
 * 
 * @author djohnson
 * @version 1.0
 */
// @NotThreadSafe
class PerformanceTimerImpl extends AbstractTimer implements IPerformanceTimer {

    private ITimePoint lastLapTime;

    private final LinkedHashMap<String, IDuration> laps = new LinkedHashMap<String, IDuration>(
            5);

    PerformanceTimerImpl() {
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public IDuration lap() {
        return lap("lap-" + (laps.size() + 1));
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public IDuration lap(String lapName) {
        ITimePoint startOrLastLapTime = (lastLapTime == null) ? start
                : lastLapTime;
        // The new last lap time is the current time
        lastLapTime = getCurrentTime();
        IDuration currentLap = Durations.between(startOrLastLapTime,
                lastLapTime);
        laps.put(lapName, currentLap);
        return currentLap;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public long lapMillis() {
        return lap().getMillis();
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public long lapMillis(String lapName) {
        return lap(lapName).getMillis();
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void logLaps(String prefix, IPerformanceStatusHandler statusHandler) {
        StringBuilder sb = new StringBuilder();
        sb.append(prefix);
        sb.append(" total [").append(getElapsedTime()).append(" ms]");
        for (Entry<String, IDuration> entry : laps.entrySet()) {
            sb.append(" ").append(entry.getKey()).append(" [")
                    .append(entry.getValue().getMillis()).append(" ms]");
        }
        statusHandler.log(sb.toString());
    }

    /**
     * {@inheritDoc}
     */
    @Override
    protected ITimePoint getCurrentTime() {
        // Always uses the wall clock
        return TimePoints.fromMillis(System.currentTimeMillis());
    }

}
