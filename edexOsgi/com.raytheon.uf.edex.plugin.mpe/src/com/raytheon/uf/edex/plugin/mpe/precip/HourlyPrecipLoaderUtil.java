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
package com.raytheon.uf.edex.plugin.mpe.precip;

import java.util.List;

import org.apache.commons.collections.CollectionUtils;

import java.util.Calendar;
import java.util.Collection;

import com.raytheon.uf.common.dataplugin.shef.tables.Hourlypc;
import com.raytheon.uf.common.dataplugin.shef.tables.Hourlypp;
import com.raytheon.uf.common.time.util.TimeUtil;
import com.raytheon.uf.edex.plugin.mpe.dao.impl.AbstractHourlyPC_PPDao;
import com.raytheon.uf.edex.plugin.mpe.dao.impl.HourlyPCDao;
import com.raytheon.uf.edex.plugin.mpe.dao.impl.HourlyPPDao;

/**
 * Utility to retrieve {@link Hourlypc} and {@link Hourlypp} records. Based on:
 * /rary.ohd.whfs/src/PrecipUtil/TEXT/load_PCPP_data.c.
 * 
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Sep 7, 2016  5631       bkowal      Initial creation
 *
 * </pre>
 *
 * @author bkowal
 */

public class HourlyPrecipLoaderUtil {

    private static final HourlyPCDao hourlyPCDao = new HourlyPCDao();

    private static final HourlyPPDao hourlyPPDao = new HourlyPPDao();

    protected HourlyPrecipLoaderUtil() {
    }

    /**
     * Retrieves the {@link Hourlypp} records between the specified begin and
     * end times limited by the specified lid (optional) and the specified ts to
     * include or exclude as indicated by the specified boolean flag.
     * 
     * @param beginTime
     *            the specified beginning date/time
     * @param endTime
     *            the specified ending date/time
     * @param lid
     *            the specified lid (optional - {@code null} is allowed)
     * @param ts
     *            a {@link Collection} of ts to filter the retrieved records
     *            based on exclusion or inclusion as indicated by the excludeTS
     *            flag
     * @param excludeTS
     *            flag indicating whether the specified ts should be used to
     *            filter the results based on inclusion or exclusion. When
     *            {@code true}, the filter will be based on exclusion.
     * @return a {@link List} of the {@link Hourlypp} records that were
     *         retrieved
     */
    public static List<Hourlypp> loadPPForTimePeriod(final Calendar beginTime,
            final Calendar endTime, final String lid,
            final Collection<String> ts, final boolean excludeTS) {
        return loadHourlyData(hourlyPPDao, beginTime, endTime, lid, ts,
                excludeTS);
    }

    /**
     * Retrieves the {@link Hourlypc} records between the specified begin and
     * end times limited by the specified lid (optional) and the specified ts to
     * include or exclude as indicated by the specified boolean flag.
     * 
     * @param beginTime
     *            the specified beginning date/time
     * @param endTime
     *            the specified ending date/time
     * @param lid
     *            the specified lid (optional - {@code null} is allowed)
     * @param ts
     *            a {@link Collection} of ts to filter the retrieved records
     *            based on exclusion or inclusion as indicated by the excludeTS
     *            flag
     * @param excludeTS
     *            flag indicating whether the specified ts should be used to
     *            filter the results based on inclusion or exclusion. When
     *            {@code true}, the filter will be based on exclusion.
     * @return a {@link List} of the {@link Hourlypc} records that were
     *         retrieved
     */
    public static List<Hourlypc> loadPCForTimePeriod(final Calendar beginTime,
            final Calendar endTime, final String lid,
            final Collection<String> ts, final boolean excludeTS) {
        return loadHourlyData(hourlyPCDao, beginTime, endTime, lid, ts,
                excludeTS);
    }

    /**
     * Retrieves hourly PC or PP records between the specified begin and end
     * times using the specified {@link AbstractHourlyPC_PPDao}. The retrieved
     * records can further be limited by specifying a lid and/or a
     * {@link Collection} of ts to include or exclude (at least one of the two,
     * if not both, must be specified).
     * 
     * @param dao
     *            the specified {@link AbstractHourlyPC_PPDao}
     * @param beginTime
     *            the specified begin time
     * @param endTime
     *            the specified end time
     * @param lid
     *            the specified lid
     * @param ts
     *            the specified {@link Collection} of ts to include or exclude
     * @param excludeTS
     *            boolean flag indicating whether or not the specified ts should
     *            be included or excluded. When {@code true}, the specfied ts
     *            will be excluded
     * @return a {@link List} of retrieved hourly records
     */
    private static <T> List<T> loadHourlyData(
            final AbstractHourlyPC_PPDao<T, ?> dao, final Calendar beginTime,
            final Calendar endTime, final String lid,
            final Collection<String> ts, final boolean excludeTS) {
        if (dao == null) {
            throw new IllegalArgumentException(
                    "Required argument 'dao' cannot be NULL.");
        }
        if (beginTime == null) {
            throw new IllegalArgumentException(
                    "Required argument 'beginTime' cannot be NULL.");
        }
        if (endTime == null) {
            throw new IllegalArgumentException(
                    "Required argument 'endTime' cannot be NULL.");
        }
        if (lid == null && CollectionUtils.isEmpty(ts)) {
            throw new IllegalArgumentException(
                    "Both arguments: 'lid' and 'ts' cannot be NULL.");
        }

        /*
         * Adjusts the specified {@link Calendar} to account for accumulation
         * intervals that start at 00Z. This is because the 00Z PC value is
         * actually placed in the 24 hour slot of the previous day.
         */
        final Calendar adjustedBeginTime = TimeUtil.newCalendar(beginTime);
        if (adjustedBeginTime.get(Calendar.HOUR_OF_DAY) == 0) {
            adjustedBeginTime.add(Calendar.HOUR_OF_DAY, -1);
        }

        if (CollectionUtils.isEmpty(ts) && lid == null) {
            return dao.getHourlyForObstime(adjustedBeginTime.getTime(),
                    endTime.getTime());
        }

        if (CollectionUtils.isEmpty(ts) && lid != null) {
            return dao.getHourlyForLidObstime(lid, adjustedBeginTime.getTime(),
                    endTime.getTime());
        }

        if (CollectionUtils.isNotEmpty(ts) && lid == null) {
            if (ts.size() == 1 && excludeTS) {
                return dao.getHourlyForTsSglNOTObstime(ts.iterator().next(),
                        adjustedBeginTime.getTime(), endTime.getTime());
            } else if (ts.size() == 1 && !excludeTS) {
                return dao.getHourlyForTsSglEQObstime(ts.iterator().next(),
                        adjustedBeginTime.getTime(), endTime.getTime());
            } else if (excludeTS) {
                return dao.getHourlyForTsMultiNOTObstime(ts,
                        adjustedBeginTime.getTime(), endTime.getTime());
            } else if (!excludeTS) {
                return dao.getHourlyForTsMultiEQObstime(ts,
                        adjustedBeginTime.getTime(), endTime.getTime());
            }
        }

        if (CollectionUtils.isNotEmpty(ts) && lid != null) {
            if (ts.size() == 1 && excludeTS) {
                return dao.getHourlyForTsSglNOTLidObstime(lid,
                        ts.iterator().next(), adjustedBeginTime.getTime(),
                        endTime.getTime());
            } else if (ts.size() == 1 && !excludeTS) {
                return dao.getHourlyForTsSglEQLidObstime(lid,
                        ts.iterator().next(), adjustedBeginTime.getTime(),
                        endTime.getTime());
            } else if (excludeTS) {
                return dao.getHourlyForTsMultiNOTLidObstime(lid, ts,
                        adjustedBeginTime.getTime(), endTime.getTime());
            } else if (!excludeTS) {
                return dao.getHourlyForTsMultiEQLidObstime(lid, ts,
                        adjustedBeginTime.getTime(), endTime.getTime());
            }
        }

        /*
         * should never reach this point.
         */
        throw new IllegalStateException(
                "An unexpected sequence of input parameters has been encountered. Neither lid or ts have been specified.");
    }
}