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

package com.raytheon.edex.plugin.radar.dao;

import java.util.ArrayList;
import java.util.List;

import org.hibernate.SQLQuery;
import org.hibernate.Session;
import org.hibernate.criterion.DetachedCriteria;
import org.hibernate.criterion.Disjunction;
import org.hibernate.criterion.Restrictions;
import org.springframework.transaction.TransactionStatus;
import org.springframework.transaction.support.TransactionCallback;

import com.raytheon.uf.common.dataplugin.radar.RadarStation;
import com.raytheon.uf.common.dataquery.db.QueryResult;
import com.raytheon.uf.edex.database.DataAccessLayerException;
import com.raytheon.uf.edex.database.dao.CoreDao;
import com.raytheon.uf.edex.database.dao.DaoConfig;
import com.vividsolutions.jts.geom.Coordinate;

/**
 * The dao implementation associated with the RadarStation class used for all
 * database interaction.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#     Engineer    Description
 * ------------ ----------  ----------- --------------------------
 * 7/24/07      353         bphillip    Initial Check in
 * 10/16/2014   3454        bphillip    Upgrading to Hibernate 4
 * 10/28/2014   3454        bphillip    Fix usage of getSession()
 * Jul 09, 2015 4500        rjpeter     Add setSridOnAllRadarStation.
 * </pre>
 * 
 * @author bphillip
 * @version 1
 */
public class RadarStationDao extends CoreDao {

    /**
     * Creates a new RadarStationDao
     */
    public RadarStationDao() {
        super(DaoConfig.forClass(RadarStation.class));
    }

    /**
     * Retrieves a radar station with the provided rda id
     * 
     * @param rdaId
     *            The rda id
     * @return The radar station with the corresponding rda id
     */
    public RadarStation queryByRdaId(String rdaId)
            throws DataAccessLayerException {
        List<?> stations = queryBySingleCriteria("rdaId", rdaId);
        if (stations.isEmpty()) {
            return null;
        } else {
            return (RadarStation) stations.get(0);
        }
    }

    /**
     * Retrieves a radar station with the provided rpgID
     * 
     * @param rpgIdDec
     *            The rpgID
     * @return The radar station with the corresponding rpgID
     */
    public RadarStation queryByRpgIdDec(String rpgIdDec)
            throws DataAccessLayerException {
        List<?> stations = queryBySingleCriteria("rpgIdDec", rpgIdDec);
        if (stations.isEmpty()) {
            return null;
        } else {
            return (RadarStation) stations.get(0);
        }
    }

    /**
     * Retrieves a radar station with the provided icao
     * 
     * @param wfo
     *            The ICAO
     * @return The radar station with the given icao
     */
    @SuppressWarnings("unchecked")
    public List<RadarStation> queryByWfo(String wfo)
            throws DataAccessLayerException {
        List<?> stations = queryBySingleCriteria("wfoId", wfo);
        if ((stations == null) || stations.isEmpty()) {
            return null;
        } else {
            return (List<RadarStation>) stations;
        }
    }

    /**
     * Retrieves all radar stations bounded by the polygon defined by the
     * provided list of coordinates. <br>
     * The polygon defined by the points does not need to be closed.
     * 
     * @param coords
     *            A list of coordinates defining an open polygon
     * @return List of radar stations bounded by the polygon defined by the
     *         coordinates
     */
    @SuppressWarnings("unchecked")
    public List<RadarStation> queryBySpatialPolygon(List<Coordinate> coords) {

        // Coordinates must form a polygon ie more than two points
        if (coords.size() > 2) {

            /*
             * Execute a spatial query to get all the icaos which reside in the
             * bounding polygon. Necessary because Hibernate does not support
             * postGIS spatial queries
             */
            StringBuffer sql = new StringBuffer();
            sql.append("SELECT wfo_id,rda_id FROM awips.radar_spatial WHERE the_geom && GeomFromText('POLYGON((");

            Coordinate currentCoord = null;
            for (int i = 0; i < coords.size(); i++) {
                currentCoord = coords.get(i);
                sql.append(currentCoord.x).append(" ").append(currentCoord.y)
                        .append(",");
            }
            sql.append(coords.get(0).x).append(" ").append(coords.get(0).y)
                    .append("))',4326);");

            Object[] names = executeSQLQuery(sql.toString());

            /* Exit early if no stations are identified */
            if (names.length == 0) {
                return new ArrayList<RadarStation>();
            }

            /*
             * Create a criteria query so Hibernate can map icaos retrieved
             * above to objects
             */
            DetachedCriteria crit = DetachedCriteria
                    .forClass(RadarStation.class);

            Disjunction stationEq = Restrictions.disjunction();
            for (Object name : names) {
                if (((Object[]) name)[0] != null) {
                    stationEq.add(Restrictions.eq("wfoId",
                            ((Object[]) name)[0].toString()));
                }
            }
            crit.add(stationEq);
            Session session = getSession();
            try {
                return crit.getExecutableCriteria(session).list();
            } finally {
                if (session != null) {
                    session.close();
                }
            }
        } else {
            logger.warn("Cannot execute spatial query with less than 3 points");
            return new ArrayList<RadarStation>();
        }

    }

    /**
     * Retrieves all radar stations bounded by a specified box. <br>
     * The box is defined by an upper left lon/lat pair and a lower right
     * lon/lat pair. The other two points of the box are calculated.
     * 
     * @param upperLeftLat
     *            The upper left latitude
     * @param upperLeftLon
     *            The upper left longitude
     * @param lowerRightLat
     *            The lower right latitude
     * @param lowerRightLon
     *            The lower right longitude
     * @return List of radar stations bounded by the box
     */
    public List<RadarStation> queryBySpatialBox(double upperLeftLat,
            double upperLeftLon, double lowerRightLat, double lowerRightLon) {

        List<Coordinate> coords = new ArrayList<Coordinate>();
        coords.add(new Coordinate(upperLeftLon, upperLeftLat));
        coords.add(new Coordinate(upperLeftLon, lowerRightLat));
        coords.add(new Coordinate(lowerRightLon, lowerRightLat));
        coords.add(new Coordinate(lowerRightLon, upperLeftLat));
        return queryBySpatialPolygon(coords);
    }

    public List<String> getRDA_IDs() {
        String buf = "select rdaId from " + daoClass.getName();
        QueryResult result = this.executeHQLQuery(buf);
        List<String> rdaIds = new ArrayList<String>();
        for (int i = 0; i < result.getResultCount(); i++) {
            rdaIds.add((String) result.getRowColumnValue(i, 0));
        }
        return rdaIds;
    }

    /**
     * Sets the station field on all radar spatial entries to setsrid of the
     * geometry.
     */
    public void setSridOnAllRadarStation() {
        txTemplate.execute(new TransactionCallback<Integer>() {
            @Override
            public Integer doInTransaction(TransactionStatus status) {
                Session sess = getCurrentSession();
                SQLQuery query = sess
                        .createSQLQuery("update radar_spatial set station=st_setsrid(the_geom, 4326)");
                return query.executeUpdate();
            }
        });

    }
}
