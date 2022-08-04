package com.raytheon.uf.edex.plugin.mpe.dpa.dao;

import java.util.Date;

import com.raytheon.uf.common.dataplugin.shef.tables.Dparadar;
import com.raytheon.uf.common.dataplugin.shef.tables.DparadarId;
import com.raytheon.uf.common.dataplugin.shef.tables.Radarresp;
import com.raytheon.uf.edex.plugin.mpe.dao.AbstractIHFSDbDao;

/**
 * IHFS Database Dao for interacting with the {@link Radarresp} entity.
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * May 13, 2016 4662       jschmid     Initial creation
 * Sep 27, 2016 5631       bkowal      Fix identifier type specification.
 * Apr 04, 2019  6140      tgurney     Removed recordExists() (no longer used)
 *
 * </pre>
 *
 * @author jschmid
 */

public class DpaRadarDao extends AbstractIHFSDbDao<Dparadar, DparadarId> {

    public DpaRadarDao() {
        super(Dparadar.class);
    }

    /**
     * Query 'dparadar' table for the max of the 'obstime' timestamps matching
     * the current record's Id. Must be within the lower half of the
     * filter-window surrounding the top of the hour.
     *
     * @param radarId
     *            Three letter String identifying current record's station.
     * @param lowerTimeBound
     *            Do not consider records with timestamp before this point in
     *            time.
     * @param topOfHourMark
     *            Do not consider records with timestamp at or after this point
     *            in time.
     * @return String timestamp of query-max or null.
     */
    public Date getMaxWindowObstimeForId(String radarId, Date lowerTimeBound,
            Date topOfHourMark) {

        Date maxRegionDate = getDateByNamedQueryAndNamedParam(
                Dparadar.SELECT_MAX_OBSTIME_FROM_DPARADARID,
                new String[] { "radarId", "lowerTimeBound", "topOfHourMark" },
                new Object[] { radarId, lowerTimeBound, topOfHourMark });
        return maxRegionDate;
    }

    /**
     * Query 'dparadar' table for the min of the 'obstime' timestamps matching
     * the current record's Id. Must be within the upper half of the
     * filter-window surrounding the top of the hour.
     *
     * @param radarId
     *            Three letter String identifying current record's station.
     * @param lowerTimeBound
     *            Do not consider records with timestamp before this point in
     *            time.
     * @param topOfHourMark
     *            Do not consider records with timestamp at or after this point
     *            in time.
     * @return String timestamp of query-min or null.
     */
    public Date getMinWindowObstimeForId(String radarId, Date topOfHourMark,
            Date upperTimeBound) {

        Date minRegionDate = getDateByNamedQueryAndNamedParam(
                Dparadar.SELECT_MIN_OBSTIME_FROM_DPARADARID,
                new String[] { "radarId", "topOfHourMark", "upperTimeBound" },
                new Object[] { radarId, topOfHourMark, upperTimeBound });
        return minRegionDate;
    }
}