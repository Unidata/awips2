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

package com.raytheon.uf.edex.stats.dao;

import java.util.Calendar;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import com.raytheon.uf.common.stats.StatsRecord;
import com.raytheon.uf.edex.database.DataAccessLayerException;
import com.raytheon.uf.edex.database.dao.SessionManagedDao;

/**
 * Stats object data access object
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 3/18/2013    1082       bphillip     Modified to extend sessionmanagedDao and use spring injection
 * 
 * </pre>
 * 
 * @author bphillip
 * @version 1.0
 */
public class StatsDao extends SessionManagedDao<Integer, StatsRecord> {
    /**
     * Creates a new data access object
     */
    public StatsDao() {

    }

    /**
     * Retrieves stat records that has a date before the limit.
     * 
     * @param limit
     * @param eventType
     * @param maxResults
     *            if greater than 0 will limit database results to maxResults
     * @return an array of stat records. If an error occurs, then an array of
     *         size 0 will be returned.
     * @throws DataAccessLayerException
     */
    public List<StatsRecord> retrieveRecords(Calendar limit, String eventType,
            int maxResults) throws DataAccessLayerException {
        Map<String, Object> params = new HashMap<String, Object>();
        String hql = "from StatsRecord rec where rec.eventType = :eventType and rec.date < :date order by rec.date asc";
        params.put("eventType", eventType);
        params.put("date", limit);
        return this.query(hql, params, maxResults);
    }

    @Override
    public StatsRecord getById(Integer id) {
        return super.getById(id);
    }

    @Override
    protected Class<StatsRecord> getEntityClass() {
        return StatsRecord.class;
    }
}
