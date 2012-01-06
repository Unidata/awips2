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

package com.raytheon.edex.db.dao.spatial;

import java.util.ArrayList;
import java.util.List;

import org.hibernate.criterion.Disjunction;
import org.hibernate.criterion.Expression;

import com.raytheon.uf.common.pointdata.spatial.ObStation;
import com.raytheon.uf.edex.database.DataAccessLayerException;
import com.raytheon.uf.edex.database.dao.CoreDao;
import com.raytheon.uf.edex.database.dao.DaoConfig;
import com.vividsolutions.jts.geom.Coordinate;

/**
 * The dao implementation associated with the ObsStation class used for all
 * database interaction.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#     Engineer    Description
 * ------------ ----------  ----------- --------------------------
 * 7/24/07      353         bphillip    Initial Check in    
 * 10/12/07     391         jkorman     Modified queryByIcao and queryByWmoIndex
 *                                      to guard for an empty list.
 * </pre>
 * 
 * @author bphillip
 * @version 1
 */
public class ObStationDao extends CoreDao {

	/**
	 * Creates a new ObsStationDao
	 */
	public ObStationDao() {
		super(DaoConfig.forClass(ObStation.class));
		
	}

	/**
	 * 
	 * @param gid
	 * @return
	 * @throws DataAccessLayerException
	 */
	public ObStation queryByGid(String gid) throws DataAccessLayerException{
        ObStation station = null;
        List<?> obs = queryBySingleCriteria("gid", gid);
        if((obs != null)&&(obs.size() > 0)) {
            station = (ObStation) obs.get(0);
        }
        return station;
	}	
	
	/**
	 * Retrieves an obs station with the specified icao
	 * 
	 * @param icao
	 *            The icao
	 * @return The obs station with the specified icao
	 */
	public ObStation queryByIcao(String icao) throws DataAccessLayerException{
        ObStation station = null;
		List<?> obs = queryBySingleCriteria("gid", ObStation.createGID(ObStation.CAT_TYPE_ICAO,icao));
        if((obs != null)&&(obs.size() > 0)) {
            station = (ObStation) obs.get(0);
        }
        return station;
	}

	/**
	 * Retrieves an obs station with the specified wmo index
	 * 
	 * @param wmoIndex
	 *            The wmo index
	 * @return The obs station with the specified wmo index
	 */
	public ObStation queryByWmoIndex(Integer wmoIndex) throws DataAccessLayerException{
	    ObStation station = null;
	    if(wmoIndex != null) {
	        List<?> obs = queryBySingleCriteria("gid", ObStation.createGID(ObStation.CAT_TYPE_SFC_FXD,wmoIndex.toString()));
	        if((obs != null)&&(obs.size() > 0)) {
	            station = (ObStation) obs.get(0);
	        }
	    }
		return station;
	}

	/**
	 * Retrieves all obs stations in a specified country
	 * 
	 * @param country
	 *            The country
	 * @return List of all obs stations in a specified country
	 */
	@SuppressWarnings("unchecked")
	public List<ObStation> queryByCountry(String country) throws DataAccessLayerException{
		return (List<ObStation>) queryBySingleCriteria("country", country);
	}

	/**
	 * Retrieves all obs stations in a specified state
	 * 
	 * @param state
	 *            The state
	 * @return List of all obs stations in a specified state
	 */
	@SuppressWarnings("unchecked")
	public List<ObStation> queryByState(String state) throws DataAccessLayerException{
		return (List<ObStation>) queryBySingleCriteria("state", state);
	}

    /**
     * Retrieves all obs stations with a specified stationid. 
     * 
     * @param stationId
     *            A stationId to retrieve.
     * @return List of all obs stations with the specified stationId.
     */
    @SuppressWarnings("unchecked")
    public List<ObStation> queryByStationId(String stationId) throws DataAccessLayerException{
        return (List<ObStation>) queryBySingleCriteria("stationid", stationId);
    }
	
	/**
	 * Retrieves all obs stations bounded by the polygon defined by the provided
	 * list of coordinates. <br>
	 * The polygon defined by the points does not need to be closed.
	 * 
	 * @param coords
	 *            A list of coordinates defining an open polygon
	 * @return List of obs stations bounded by the polygon defined by the
	 *         coordinates
	 */
	@SuppressWarnings("unchecked")
	public List<ObStation> queryBySpatialPolygon(List<Coordinate> coords) {

		// Coordinates must form a polygon ie more than two points
		if (coords.size() > 2) {

			/*
			 * Execute a spatial query to get all the icaos which reside in the
			 * bounding polygon
			 */
			StringBuffer sql = new StringBuffer();
			sql
					.append("SELECT icao,wmoindex FROM awips.spatial_obs_stations WHERE stationgeom && GeomFromText('POLYGON((");

			Coordinate currentCoord = null;
			for (int i = 0; i < coords.size(); i++) {
				currentCoord = coords.get(i);
				sql.append(currentCoord.x).append(" ").append(currentCoord.y)
						.append(",");
			}
			sql.append(coords.get(0).x).append(" ").append(coords.get(0).y)
					.append("))',-1);");

			Object[] names = executeSQLQuery(sql.toString());

			/* Exit early if no stations are identified */
			if (names.length == 0) {
				return new ArrayList<ObStation>();
			}

			Disjunction stationEq = Expression.disjunction();
			for (int i = 0; i < names.length; i++) {
				if (((Object[]) names[i])[0] == null) {

					stationEq.add(Expression.eq("wmoIndex",
							((Object[]) names[i])[1]));
				} else {
					stationEq.add(Expression.eq("icao",
							((Object[]) names[i])[0].toString()));
				}
			}

			return (List<ObStation>)executeCriteriaQuery(stationEq);
		} else {
			logger.warn("Cannot execute spatial query with less than 3 points");
			return new ArrayList<ObStation>();
		}

	}

	/**
	 * Retrieves all obs stations bounded by a specified box. <br>
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
	 * @return
	 */
	public List<ObStation> queryBySpatialBox(double upperLeftLat,
			double upperLeftLon, double lowerRightLat, double lowerRightLon) {

		List<Coordinate> coords = new ArrayList<Coordinate>();
		coords.add(new Coordinate(upperLeftLon, upperLeftLat));
		coords.add(new Coordinate(upperLeftLon, lowerRightLat));
		coords.add(new Coordinate(lowerRightLon, lowerRightLat));
		coords.add(new Coordinate(lowerRightLon, upperLeftLat));
		return queryBySpatialPolygon(coords);
	}

}
