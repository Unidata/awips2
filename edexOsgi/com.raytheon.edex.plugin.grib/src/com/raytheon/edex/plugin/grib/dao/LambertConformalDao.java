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

package com.raytheon.edex.plugin.grib.dao;

import java.util.List;

import com.raytheon.uf.common.dataplugin.grib.spatial.projections.GridCoverage;
import com.raytheon.uf.common.dataplugin.grib.spatial.projections.LambertConformalGridCoverage;
import com.raytheon.uf.edex.database.DataAccessLayerException;
import com.raytheon.uf.edex.database.dao.DaoConfig;
import com.raytheon.uf.edex.database.query.DatabaseQuery;

/**
 * Data Access Object for retrieving LambertConforamlGridCoverage objects
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#     Engineer    Description
 * ------------ ----------  ----------- --------------------------
 * 4/7/09       1994        bphillip    Initial Creation
 * 
 * </pre>
 * 
 * @author bphillip
 * @version 1
 */
public class LambertConformalDao extends GridCoverageDao implements
        IGridCoverageDao {

    /**
     * Creates new MercatorDao
     */
    public LambertConformalDao() {
        super(DaoConfig.forClass(LambertConformalGridCoverage.class));
    }

    /**
     * Checks that database to see if a grid exists in the database which very
     * closely resembles the provided coverage
     * 
     * @param coverage
     *            The PolarStereoGridCoverage to check against
     * @return The Grid from the database or null if not found
     * @throws DataAccessLayerException
     *             If problems during query
     */
    @SuppressWarnings("unchecked")
    public GridCoverage checkGrid(GridCoverage grid)
            throws DataAccessLayerException {

        LambertConformalGridCoverage coverage = (LambertConformalGridCoverage) grid;
        DatabaseQuery query = new DatabaseQuery(this.daoClass);
        query.addQueryParam(addQueryTolerance("dx", coverage.getDx()));
        query.addQueryParam(addQueryTolerance("dy", coverage.getDy()));
        query.addQueryParam(addQueryTolerance("la1", coverage.getLa1()));
        query.addQueryParam(addQueryTolerance("lo1", coverage.getLo1()));
        query.addQueryParam(addQueryTolerance("latin1", coverage.getLatin1()));
        query.addQueryParam(addQueryTolerance("latin2", coverage.getLatin2()));
        query.addQueryParam(addQueryTolerance("lov", coverage.getLov()));
        query.addQueryParam("nx", coverage.getNx());
        query.addQueryParam("ny", coverage.getNy());
        List<LambertConformalGridCoverage> result = (List<LambertConformalGridCoverage>) queryByCriteria(query);
        return selectKnownGrid(result);
    }
}
