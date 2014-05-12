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
import java.util.List;

import com.raytheon.uf.common.stats.StatsRecord;
import com.raytheon.uf.edex.database.DataAccessLayerException;
import com.raytheon.uf.edex.database.dao.SessionManagedDao;

/**
 * Data access object for raw statistics.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Aug 21, 2012            jsanchez    Initial creation
 * Mar 18, 2013 1082       bphillip    Modified to extend sessionmanagedDao and use spring injection
 * May 22, 2013 1917       rjpeter     Added reclaimSpace.
 * Apr 18, 2014 2681       rjpeter     Added retrieveMinTime.
 * May 12, 2014 3154       rjpeter     Remove reclaimSpace, postgres 9.2 autovacuum sufficient.
 * </pre>
 * 
 * @author jsanchez
 * @version 1.0
 */
public class StatsDao extends SessionManagedDao<Integer, StatsRecord> {
    /**
     * Creates a new data access object
     */
    public StatsDao() {

    }

    /**
     * Retrieves the earliest time in the stats table for a given data type.
     * 
     * @param eventType
     * @return
     * @throws DataAccessLayerException
     */
    public Calendar retrieveMinTime(String eventType)
            throws DataAccessLayerException {
        String hql = "select min(rec.date) from StatsRecord rec where rec.eventType = :eventType";
        List<Object> results = this
                .executeHQLQuery(hql, "eventType", eventType);
        if ((results != null) && !results.isEmpty()) {
            Object time = results.get(0);
            if (time != null) {
                return (Calendar) time;
            }
        }

        return null;
    }

    /**
     * Retrieves stat records that has a date in the time range.
     * 
     * @param limit
     * @param eventType
     * @param maxResults
     *            if greater than 0 will limit database results to maxResults
     * @return an array of stat records. If an error occurs, then an array of
     *         size 0 will be returned.
     * @throws DataAccessLayerException
     */
    public List<StatsRecord> retrieveRecords(String eventType,
            Calendar minTime, Calendar maxTime) throws DataAccessLayerException {
        String hql = "from StatsRecord rec where rec.eventType = :eventType and rec.date >= :minDate and rec.date < :maxDate order by rec.date asc";
        return this.query(hql, "eventType", eventType, "minDate", minTime,
                "maxDate", maxTime);
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
