package com.raytheon.uf.edex.plugin.mpe.dao.impl;

import java.util.Date;
import java.util.List;

import com.raytheon.uf.common.dataplugin.shef.tables.Hourlypc;
import com.raytheon.uf.common.dataplugin.shef.tables.HourlypcId;
import com.raytheon.uf.edex.plugin.mpe.dao.AbstractIHFSDbDao;

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
 * 
 * </pre>
 * 
 * @author skorolev
 */

public class HourlyPCDao extends AbstractIHFSDbDao<Hourlypc, HourlypcId> {

    public HourlyPCDao() {
        super(Hourlypc.class);
    }

    private String[] paramsLidTsObsTime = new String[] { "lid", "ts", "start",
            "finish" };

    private String[] paramsTsObsTime = { "ts", "start", "finish" };

    /**
     * Retrieves {@link Hourlypc} records using lid and ts = "ts" between start
     * and finish dates.
     * 
     * @param lid
     *            local ID
     * @param ts
     *            type-source
     * @param start
     *            obsTime
     * @param finish
     *            obsTime
     * @return a {@link List} of {@link Hourlypc} records
     */
    public List<Hourlypc> getHourlyPC_for_Ts_SglEQ_Lid_Obstime(String lid,
            String ts, Date start, Date finish) {
        return findByNamedQueryAndNamedParams(
                Hourlypc.SELECT_HOURLYPC_FOR_TS_SNGL_EQ_LID_OBSTIME,
                paramsLidTsObsTime, new Object[] { lid, ts, start, finish });
    }

    /**
     * Retrieves {@link Hourlypc} records using lid and ts != "ts" between start
     * and finish dates.
     * 
     * @param lid
     *            local ID
     * @param ts
     *            type-source
     * @param start
     *            obsTime
     * @param finish
     *            obsTime
     * @return a {@link List} of {@link Hourlypc} records
     */
    public List<Hourlypc> getHourlyPC_for_Ts_SglNOT_Lid_Obstime(String lid,
            String ts, Date start, Date finish) {
        return findByNamedQueryAndNamedParams(
                Hourlypc.SELECT_HOURLYPC_FOR_TS_SNGL_NOT_LID_OBSTIME,
                paramsLidTsObsTime, new Object[] { lid, ts, start, finish });
    }

    /**
     * Retrieves {@link Hourlypc} records using lid and ts from list of "ts"
     * between start and finish dates.
     * 
     * @param lid
     *            local ID
     * @param ts
     *            type-source
     * @param start
     *            obsTime
     * @param finish
     *            obsTime
     * @return a {@link List} of {@link Hourlypc} records
     */
    public List<Hourlypc> getHourlyPC_for_Ts_MultiEQ_Lid_Obstime(String lid,
            List<String> ts, Date start, Date finish) {
        return findByNamedQueryAndNamedParams(
                Hourlypc.SELECT_HOURLYPC_FOR_TS_MULTI_EQ_LID_OBSTIME,
                paramsLidTsObsTime, new Object[] { lid, ts, start, finish });
    }

    /**
     * Retrieves {@link Hourlypc} records using lid and ts not from list of "ts"
     * between start and finish dates.
     * 
     * @param lid
     *            local ID
     * @param ts
     *            type-source
     * @param start
     *            obsTime
     * @param finish
     *            obsTime
     * @return a {@link List} of {@link Hourlypc} records
     */
    public List<Hourlypc> getHourlyPC_for_Ts_MultiNOT_Lid_Obstime(String lid,
            List<String> ts, Date start, Date finish) {
        return findByNamedQueryAndNamedParams(
                Hourlypc.SELECT_HOURLYPC_FOR_TS_MULTI_NOT_LID_OBSTIME,
                paramsLidTsObsTime, new Object[] { lid, ts, start, finish });
    }

    /**
     * Retrieves {@link Hourlypc} records using ts = "ts" between start and
     * finish dates.
     * 
     * @param ts
     *            type-source
     * @param start
     *            obsTime
     * @param finish
     *            obsTime
     * @return
     */
    public List<Hourlypc> getHourlyPC_for_Ts_SglEQ_Obstime(String ts,
            Date start, Date finish) {
        return findByNamedQueryAndNamedParams(
                Hourlypc.SELECT_HOURLYPC_FOR_TS_SNGL_EQ_OBSTIME,
                paramsTsObsTime, new Object[] { ts, start, finish });
    }

    /**
     * Retrieves {@link Hourlypc} records using ts != "ts" between start and
     * finish dates.
     * 
     * @param ts
     *            type-source
     * @param start
     *            obsTime
     * @param finish
     *            obsTime
     * @return
     */
    public List<Hourlypc> getHourlyPC_for_Ts_SglNOT_Obstime(String ts,
            Date start, Date finish) {
        return findByNamedQueryAndNamedParams(
                Hourlypc.SELECT_HOURLYPC_FOR_TS_SNGL_NOT_OBSTIME,
                paramsTsObsTime, new Object[] { ts, start, finish });
    }

    /**
     * Retrieves {@link Hourlypc} records using ts from list of "ts" between
     * start and finish dates.
     * 
     * @param ts
     *            type-source
     * @param start
     *            obsTime
     * @param finish
     *            obsTime
     * @return
     */
    public List<Hourlypc> getHourlyPC_for_Ts_MultiEQ_Obstime(List<String> ts,
            Date start, Date finish) {
        return findByNamedQueryAndNamedParams(
                Hourlypc.SELECT_HOURLYPC_FOR_TS_MULTI_EQ_OBSTIME,
                paramsTsObsTime, new Object[] { ts, start, finish });
    }

    /**
     * Retrieves {@link Hourlypc} records using ts not from list of "ts" between
     * start and finish dates.
     * 
     * @param ts
     *            type-source
     * @param start
     *            obsTime
     * @param finish
     *            obsTime
     * @return
     */
    public List<Hourlypc> getHourlyPC_for_Ts_MultiNOT_Obstime(List<String> ts,
            Date start, Date finish) {
        return findByNamedQueryAndNamedParams(
                Hourlypc.SELECT_HOURLYPC_FOR_TS_MULTI_NOT_OBSTIME,
                paramsTsObsTime, new Object[] { ts, start, finish });
    }

    /**
     * Retrieves {@link Hourlypc} records using lid between start and finish
     * dates.
     * 
     * @param lid
     *            local ID
     * @param start
     *            obsTime
     * @param finish
     *            obsTime
     * @return a {@link List} of {@link Hourlypc} records
     */
    public List<Hourlypc> getHourlyPC_for_Lid_Obstime(String lid, Date start,
            Date finish) {
        return findByNamedQueryAndNamedParams(
                Hourlypc.SELECT_HOURLYPC_FOR_LID_OBSTIME, new String[] { "lid",
                        "start", "finish" },
                new Object[] { lid, start, finish });
    }

    /**
     * Retrieves {@link Hourlypc} records between start and finish dates.
     * 
     * @param start
     *            obsTime
     * @param finish
     *            obsTime
     * @return a {@link List} of {@link Hourlypc} records
     */
    public List<Hourlypc> getHourlyPC_for_Obstime(Date start, Date finish) {
        return findByNamedQueryAndNamedParams(
                Hourlypc.SELECT_HOURLYPC_FOR_OBSTIME, new String[] { "start",
                        "finish" }, new Object[] { start, finish });
    }

}
