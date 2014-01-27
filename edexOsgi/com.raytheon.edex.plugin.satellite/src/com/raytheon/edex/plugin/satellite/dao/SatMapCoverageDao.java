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
 * 06/27/2012   798         jkorman     Corrected id query type.
 * 10/02/2013   2333        mschenke    Removed unused code
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

}
