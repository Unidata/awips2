package com.raytheon.uf.edex.plugin.mpe.dao.impl;

import java.util.Calendar;
import java.util.Collection;
import java.util.Collections;
import java.util.Date;
import java.util.List;

import com.raytheon.uf.common.dataplugin.shef.tables.Hourlypc;
import com.raytheon.uf.common.dataplugin.shef.tables.HourlypcId;
import com.raytheon.uf.common.time.util.TimeUtil;
import com.raytheon.uf.common.util.CollectionUtil;

/**
 * IHFS Database DAO for interacting with the {@link Hourlypc} entity.
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

public class HourlyPCDao extends AbstractHourlyPC_PPDao<Hourlypc, HourlypcId> {

    public HourlyPCDao() {
        super(HOURLY_ENTITY.PC);
    }
    
    /**
     * Retrieves the {@link Hourlypc} data within the specified start date and
     * end date. The data is retrieved by day (YYYY-MM-DD) and any time fields
     * are ignored.
     * 
     * @param startObsDate
     *            the specified start date
     * @param endObsDate
     *            the specified end date
     * @return the retrieved {@link Hourlypc} data
     */
    public List<Hourlypc> retrieveDataWithinObsDateRange(
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
        List<Hourlypc> results = findByNamedQueryAndNamedParams(
                Hourlypc.SELECT_DATA_BETWEEN_OBS_DATE_RANGE, names, parameters);
        if (CollectionUtil.isNullOrEmpty(results)) {
            return Collections.emptyList();
        }
        return results;
    }

    @Override
    public List<Hourlypc> getHourlyForTsSglEQLidObstime(String lid, String ts,
            Date start, Date finish) {
        return findByNamedQueryAndNamedParams(
                Hourlypc.SELECT_HOURLYPC_FOR_TS_SNGL_EQ_LID_OBSTIME,
                paramsLidTsObsTime, new Object[] { lid, ts, start, finish });
    }

    @Override
    public List<Hourlypc> getHourlyForTsSglNOTLidObstime(String lid, String ts,
            Date start, Date finish) {
        return findByNamedQueryAndNamedParams(
                Hourlypc.SELECT_HOURLYPC_FOR_TS_SNGL_NOT_LID_OBSTIME,
                paramsLidTsObsTime, new Object[] { lid, ts, start, finish });
    }

    @Override
    public List<Hourlypc> getHourlyForTsMultiEQLidObstime(String lid,
            Collection<String> ts, Date start, Date finish) {
        return findByNamedQueryAndNamedParams(
                Hourlypc.SELECT_HOURLYPC_FOR_TS_MULTI_EQ_LID_OBSTIME,
                paramsLidTsObsTime, new Object[] { lid, ts, start, finish });
    }

    @Override
    public List<Hourlypc> getHourlyForTsMultiNOTLidObstime(String lid,
            Collection<String> ts, Date start, Date finish) {
        return findByNamedQueryAndNamedParams(
                Hourlypc.SELECT_HOURLYPC_FOR_TS_MULTI_NOT_LID_OBSTIME,
                paramsLidTsObsTime, new Object[] { lid, ts, start, finish });
    }

    @Override
    public List<Hourlypc> getHourlyForTsSglEQObstime(String ts, Date start,
            Date finish) {
        return findByNamedQueryAndNamedParams(
                Hourlypc.SELECT_HOURLYPC_FOR_TS_SNGL_EQ_OBSTIME,
                paramsTsObsTime, new Object[] { ts, start, finish });
    }

    @Override
    public List<Hourlypc> getHourlyForTsSglNOTObstime(String ts, Date start,
            Date finish) {
        return findByNamedQueryAndNamedParams(
                Hourlypc.SELECT_HOURLYPC_FOR_TS_SNGL_NOT_OBSTIME,
                paramsTsObsTime, new Object[] { ts, start, finish });
    }

    @Override
    public List<Hourlypc> getHourlyForTsMultiEQObstime(Collection<String> ts,
            Date start, Date finish) {
        return findByNamedQueryAndNamedParams(
                Hourlypc.SELECT_HOURLYPC_FOR_TS_MULTI_EQ_OBSTIME,
                paramsTsObsTime, new Object[] { ts, start, finish });
    }

    @Override
    public List<Hourlypc> getHourlyForTsMultiNOTObstime(Collection<String> ts,
            Date start, Date finish) {
        return findByNamedQueryAndNamedParams(
                Hourlypc.SELECT_HOURLYPC_FOR_TS_MULTI_NOT_OBSTIME,
                paramsTsObsTime, new Object[] { ts, start, finish });
    }

    @Override
    public List<Hourlypc> getHourlyForLidObstime(String lid, Date start,
            Date finish) {
        return findByNamedQueryAndNamedParams(
                Hourlypc.SELECT_HOURLYPC_FOR_LID_OBSTIME,
                new String[] { "lid", "start", "finish" },
                new Object[] { lid, start, finish });
    }

    @Override
    public List<Hourlypc> getHourlyForObstime(Date start, Date finish) {
        return findByNamedQueryAndNamedParams(
                Hourlypc.SELECT_HOURLYPC_FOR_OBSTIME,
                new String[] { "start", "finish" },
                new Object[] { start, finish });
    }
}