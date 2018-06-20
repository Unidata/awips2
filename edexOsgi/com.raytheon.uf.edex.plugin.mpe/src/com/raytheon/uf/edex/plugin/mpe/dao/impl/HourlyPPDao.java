package com.raytheon.uf.edex.plugin.mpe.dao.impl;

import java.util.Collection;
import java.util.Date;
import java.util.List;

import com.raytheon.uf.common.dataplugin.shef.tables.Hourlypp;
import com.raytheon.uf.common.dataplugin.shef.tables.HourlyppId;

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
