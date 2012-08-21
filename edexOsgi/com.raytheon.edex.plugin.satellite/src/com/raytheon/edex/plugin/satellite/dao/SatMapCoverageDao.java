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

package com.raytheon.edex.plugin.satellite.dao;

import java.util.ArrayList;
import java.util.List;

import com.raytheon.uf.common.dataplugin.satellite.SatMapCoverage;
import com.raytheon.uf.edex.database.DataAccessLayerException;
import com.raytheon.uf.edex.database.dao.CoreDao;
import com.raytheon.uf.edex.database.dao.DaoConfig;
import com.raytheon.uf.edex.database.query.DatabaseQuery;

/**
 * The dao implementation associated with the SatelliteMapCoverage class used
 * for all database interaction.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#     Engineer    Description
 * ------------ ----------  ----------- --------------------------
 * 7/24/07      353         bphillip    Initial Check in    
 * - AWIPS2 Baseline Repository --------
 * 06/27/2012    798        jkorman     Corrected id query type.
 *  
 * </pre>
 * 
 * @author bphillip
 * @version 1
 */
public class SatMapCoverageDao extends CoreDao {

    /**
     * Creates a new SatelliteMapCoverageDao
     */
    public SatMapCoverageDao() {
        super(DaoConfig.forClass(SatMapCoverage.class));
    }

    /**
     * Convenience method to retrieve a SatelliteMapCoverage from the database
     * given a map ID
     * 
     * @param mapId
     *            The Map ID
     * @return A SatelliteMapCoverage object with the corresponding ID. Null if
     *         not found.
     */
    public SatMapCoverage queryByMapId(Integer mapId) {
        return (SatMapCoverage) this.queryById(mapId);
    }

    /**
     * Retrieves all satellite map coverage windows for a given type. <br>
     * These types include IR, Visible, and Water Vapor
     * 
     * @param type
     *            The type of satellite image
     * @return A list of satellite map coverage areas for the given type
     */
    @SuppressWarnings("unchecked")
    public List<SatMapCoverage> queryByType(String type) throws DataAccessLayerException{
        return (List<SatMapCoverage>) queryBySingleCriteria("type", type);
    }

    /**
     * Retrieves all satellite map coverage windows with the given dimensions
     * 
     * @param nx
     *            The horizontal dimension in number of points
     * @param ny
     *            The vertical dimension in number of points
     * @return A list of satellite maps matching the given dimensions
     */
    @SuppressWarnings("unchecked")
    public List<SatMapCoverage> queryByDimension(Integer nx, Integer ny) throws DataAccessLayerException{
        List<String> fields = new ArrayList<String>();
        List<Object> values = new ArrayList<Object>();

        fields.add("nx");
        values.add(String.valueOf(nx));
        fields.add("ny");
        values.add(String.valueOf(ny));

        return (List<SatMapCoverage>) queryByCriteria(fields, values);
    }

    /**
     * Retrieves a map projection based on the given criteria
     * 
     * @param mapProjection
     *            The map projection 1=Mercator 3=Lambert Conformal 5=Polar
     *            Stereographic
     * @param nx
     *            Number of points along the x-axis
     * @param ny
     *            Number of points along the y-axis
     * @param dx
     *            The horizontal resolution of the grid
     * @param dy
     *            The vertical resolution of the grid
     * @param lov
     *            The orientation of the grid
     * @param latin
     *            The tangent latitude
     * @param la1
     *            The latitude of the first grid point
     * @param lo1
     *            The longitude of the first grid point
     * @param la2
     *            The latitude of the last grid point (only used with Mercator
     *            projection)
     * @param lo2
     *            The longitude of the last grid opint (only used with Mercaotr
     *            projection)
     * @return The SatMapCoverage object matching the given criteria
     */
    @SuppressWarnings("unchecked")
    public SatMapCoverage getSatCoverage(Integer mapProjection, Integer nx,
            Integer ny, Float dx, Float dy, Float lov, Float latin, Float la1,
            Float lo1, Float la2, Float lo2) throws DataAccessLayerException{
        List<SatMapCoverage> queryResults = null;
        List<String> fields = new ArrayList<String>();
        List<Object> values = new ArrayList<Object>();
        
        DatabaseQuery query = new DatabaseQuery(daoClass.getName());
        query.addQueryParam("projection", mapProjection);
        query.addQueryParam("nx",nx);
        query.addQueryParam("ny",ny);
        query.addQueryParam("dx",dx);
        query.addQueryParam("dy",dy);
        query.addQueryParam("lov",lov);
        query.addQueryParam("latin",latin);
        query.addQueryParam("la1",la1);
        query.addQueryParam("lo1",lo1);

        if (mapProjection == SatMapCoverage.PROJ_MERCATOR) {
            query.addQueryParam("la2",la2);
            query.addQueryParam("lo2",lo2);
        }

        queryResults = (List<SatMapCoverage>) queryByCriteria(query);
        if (queryResults != null) {
            if (queryResults.size() > 1) {
                StringBuffer out = new StringBuffer();
                out
                        .append("Multiple map coverages return using the following criteria: [");
                for (int i = 0; i < fields.size(); i++) {
                    out.append(fields.get(i)).append("=").append(values.get(i))
                            .append(" ");
                }
                out.append("] -- Using first result");
                logger.debug(out.toString());
            }
            if (queryResults.size() >= 1) {
                return queryResults.get(0);
            } else {
                return null;
            }
        } else {
            return null;
        }
    }

}
