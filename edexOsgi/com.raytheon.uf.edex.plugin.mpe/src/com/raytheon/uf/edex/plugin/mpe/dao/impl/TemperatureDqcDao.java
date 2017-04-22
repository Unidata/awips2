package com.raytheon.uf.edex.plugin.mpe.dao.impl;

import java.util.Date;
import java.util.List;

import com.raytheon.uf.common.dataplugin.shef.tables.Temperature;
import com.raytheon.uf.common.dataplugin.shef.tables.TemperatureId;
import com.raytheon.uf.edex.plugin.mpe.dao.AbstractIHFSDbDao;

/**
 * IHFS Database DAO for interacting with the {@link Temperature} entity.
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

public class TemperatureDqcDao extends
        AbstractIHFSDbDao<Temperature, TemperatureId> {

    public TemperatureDqcDao() {
        super(Temperature.class);
    }

    /**
     * Retrieves {@link Temperature} records between start and finish date.
     * 
     * @param start
     *            obsTime
     * @param finish
     *            obsTime
     * @return a {@link List} of {@link Temperature} records
     * @throws Exception
     */
    public List<Temperature> getRecordList(Date start, Date finish)
            throws Exception {

        return findByNamedQueryAndNamedParams(
                Temperature.SELECT_TEMP_BY_START_AND_FINISH, new String[] {
                        "start", "finish" }, new Object[] { start, finish });
    }

}
