package com.raytheon.uf.edex.plugin.mpe.dao.impl;

import java.util.Collection;
import java.util.Date;
import java.util.List;

import com.raytheon.uf.common.dataplugin.shef.tables.Hourlypc;
import com.raytheon.uf.common.dataplugin.shef.tables.HourlypcId;

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
