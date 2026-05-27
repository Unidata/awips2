package com.raytheon.uf.edex.plugin.mpe.dao.impl;

import java.util.Calendar;
import java.util.Collection;
import java.util.Collections;
import java.util.Date;
import java.util.List;

import com.raytheon.uf.common.dataplugin.shef.tables.Hourlypp;
import com.raytheon.uf.common.dataplugin.shef.tables.HourlyppId;
import com.raytheon.uf.common.time.util.TimeUtil;
import com.raytheon.uf.common.util.CollectionUtil;

/**
 * IHFS Database DAO for interacting with the {@link Hourlypp} entity.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jul 01, 2016 4623       skorolev    Initial creation
 * Sep 09, 2016 5631       bkowal      Abstract common aspects for Hourly PC and Hourly PP.
 * 
 * </pre>
 * 
 * @author skorolev
 */

public class HourlyPPDao extends AbstractHourlyPC_PPDao<Hourlypp, HourlyppId> {

    public HourlyPPDao() {
        super(HOURLY_ENTITY.PP);
    }

    /**
     * Retrieves the {@link Hourlypp} data within the specified start date and
     * end date. The data is retrieved by day (YYYY-MM-DD) and any time fields
     * are ignored.
     * 
     * @param startObsDate
     *            the specified start date
     * @param endObsDate
     *            the specified end date
     * @return the retrieved {@link Hourlypp} data
     */
    public List<Hourlypp> retrieveDataWithinObsDateRange(
            final Calendar startObsDate, final Calendar endObsDate) {
        Calendar queryStartDate = TimeUtil
                .newGmtCalendar(startObsDate.getTime());
        Calendar queryEndDate = TimeUtil.newGmtCalendar(endObsDate.getTime());
        if (queryStartDate.get(Calendar.HOUR_OF_DAY) == 0) {
            /*
             * Need special logic to account for accumulation intervals which
             * start at 00Z. This is because the 00Z PC value is actually placed
             * in the 24 hour slot of the previous day.
             */
            queryStartDate.add(Calendar.HOUR_OF_DAY, -1);
        }
        final String[] names = { "startObsDate", "endObsDate" };
        final Object[] parameters = { queryStartDate.getTime(),
                queryEndDate.getTime() };
        List<Hourlypp> results = findByNamedQueryAndNamedParams(
                Hourlypp.SELECT_DATA_BETWEEN_OBS_DATE_RANGE, names, parameters);
        if (CollectionUtil.isNullOrEmpty(results)) {
            return Collections.emptyList();
        }
        return results;
    }

    @Override
    public List<Hourlypp> getHourlyForTsSglEQLidObstime(String lid, String ts,
            Date start, Date finish) {
        return findByNamedQueryAndNamedParams(
                Hourlypp.SELECT_HOURLYPP_FOR_TS_SNGL_EQ_LID_OBSTIME,
                paramsLidTsObsTime, new Object[] { lid, ts, start, finish });
    }

    @Override
    public List<Hourlypp> getHourlyForTsSglNOTLidObstime(String lid, String ts,
            Date start, Date finish) {
        return findByNamedQueryAndNamedParams(
                Hourlypp.SELECT_HOURLYPP_FOR_TS_SNGL_NOT_LID_OBSTIME,
                paramsLidTsObsTime, new Object[] { lid, ts, start, finish });
    }

    @Override
    public List<Hourlypp> getHourlyForTsMultiEQLidObstime(String lid,
            Collection<String> ts, Date start, Date finish) {
        return findByNamedQueryAndNamedParams(
                Hourlypp.SELECT_HOURLYPP_FOR_TS_MULTI_EQ_LID_OBSTIME,
                paramsLidTsObsTime, new Object[] { lid, ts, start, finish });
    }

    @Override
    public List<Hourlypp> getHourlyForTsMultiNOTLidObstime(String lid,
            Collection<String> ts, Date start, Date finish) {
        return findByNamedQueryAndNamedParams(
                Hourlypp.SELECT_HOURLYPP_FOR_TS_MULTI_NOT_LID_OBSTIME,
                paramsLidTsObsTime, new Object[] { lid, ts, start, finish });
    }

    @Override
    public List<Hourlypp> getHourlyForTsSglEQObstime(String ts, Date start,
            Date finish) {
        return findByNamedQueryAndNamedParams(
                Hourlypp.SELECT_HOURLYPP_FOR_TS_SNGL_EQ_OBSTIME,
                paramsTsObsTime, new Object[] { ts, start, finish });
    }

    @Override
    public List<Hourlypp> getHourlyForTsSglNOTObstime(String ts, Date start,
            Date finish) {
        return findByNamedQueryAndNamedParams(
                Hourlypp.SELECT_HOURLYPP_FOR_TS_SNGL_NOT_OBSTIME,
                paramsTsObsTime, new Object[] { ts, start, finish });
    }

    @Override
    public List<Hourlypp> getHourlyForTsMultiEQObstime(Collection<String> ts,
            Date start, Date finish) {
        return findByNamedQueryAndNamedParams(
                Hourlypp.SELECT_HOURLYPP_FOR_TS_MULTI_EQ_OBSTIME,
                paramsTsObsTime, new Object[] { ts, start, finish });
    }

    @Override
    public List<Hourlypp> getHourlyForTsMultiNOTObstime(Collection<String> ts,
            Date start, Date finish) {
        return findByNamedQueryAndNamedParams(
                Hourlypp.SELECT_HOURLYPP_FOR_TS_MULTI_NOT_OBSTIME,
                paramsTsObsTime, new Object[] { ts, start, finish });
    }

    @Override
    public List<Hourlypp> getHourlyForLidObstime(String lid, Date start,
            Date finish) {
        return findByNamedQueryAndNamedParams(
                Hourlypp.SELECT_HOURLYPP_FOR_LID_OBSTIME,
                new String[] { "lid", "start", "finish" },
                new Object[] { lid, start, finish });
    }

    @Override
    public List<Hourlypp> getHourlyForObstime(Date start, Date finish) {
        return findByNamedQueryAndNamedParams(
                Hourlypp.SELECT_HOURLYPP_FOR_OBSTIME,
                new String[] { "start", "finish" },
                new Object[] { start, finish });
    }
}