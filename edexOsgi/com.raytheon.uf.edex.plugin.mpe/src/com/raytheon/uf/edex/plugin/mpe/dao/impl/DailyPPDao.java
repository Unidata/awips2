package com.raytheon.uf.edex.plugin.mpe.dao.impl;

import java.util.Date;
import java.util.List;

import com.raytheon.uf.common.dataplugin.shef.tables.Dailypp;
import com.raytheon.uf.common.dataplugin.shef.tables.DailyppId;
import com.raytheon.uf.edex.plugin.mpe.dao.AbstractIHFSDbDao;

/**
 * IHFS Database DAO for interacting with the {@link Dailypp} entity.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jun 16, 2016 4623       skorolev    Initial creation
 * 
 * </pre>
 * 
 * @author skorolev
 */
public class DailyPPDao extends AbstractIHFSDbDao<Dailypp, DailyppId> {

    public DailyPPDao() {
        super(Dailypp.class);
    }

    /**
     * Retrieves all of the {@link Dailypp} records between start and finish
     * date.
     * 
     * @param start
     * @param finish
     * @return the {@link List} of {@link Dailypp} records
     * @throws Exception
     */
    public List<Dailypp> getRecordList(Date start, Date finish)
            throws Exception {

        return findByNamedQueryAndNamedParams(
                Dailypp.SELECT_DAILYPP_BY_START_AND_FINISH, new String[] {
                        "start", "finish" }, new Object[] { start, finish });
    }
}
