/*****************************************************************************************
 * COPYRIGHT (c), 2009, RAYTHEON COMPANY
 * ALL RIGHTS RESERVED, An Unpublished Work 
 *
 * RAYTHEON PROPRIETARY
 * If the end user is not the U.S. Government or any agency thereof, use
 * or disclosure of data contained in this source code file is subject to
 * the proprietary restrictions set forth in the Master Rights File.
 *
 * U.S. GOVERNMENT PURPOSE RIGHTS NOTICE
 * If the end user is the U.S. Government or any agency thereof, this source
 * code is provided to the U.S. Government with Government Purpose Rights.
 * Use or disclosure of data contained in this source code file is subject to
 * the "Government Purpose Rights" restriction in the Master Rights File.
 *
 * U.S. EXPORT CONTROLLED TECHNICAL DATA
 * Use or disclosure of data contained in this source code file is subject to
 * the export restrictions set forth in the Master Rights File.
 ******************************************************************************************/

package gov.noaa.nws.ncep.edex.plugin.ncgrib.dao;

import java.util.List;

import com.raytheon.uf.edex.database.DataAccessLayerException;
import com.raytheon.uf.edex.database.dao.DaoConfig;
import com.raytheon.uf.edex.database.query.DatabaseQuery;

import gov.noaa.nws.ncep.common.dataplugin.ncgrib.spatial.projections.NcgridCoverage;
import gov.noaa.nws.ncep.common.dataplugin.ncgrib.spatial.projections.LambertConformalNcgridCoverage;

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
public class NcLambertConformalDao extends NcgridCoverageDao implements INcgridCoverageDao {

    /**
     * Creates new MercatorDao
     */
    public NcLambertConformalDao() {
        super(DaoConfig.forClass(LambertConformalNcgridCoverage.class));
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
    public NcgridCoverage checkGrid(NcgridCoverage grid)
            throws DataAccessLayerException {
        
        LambertConformalNcgridCoverage coverage = (LambertConformalNcgridCoverage)grid;
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
        List<LambertConformalNcgridCoverage> result = (List<LambertConformalNcgridCoverage>) queryByCriteria(query);

        if (result.isEmpty()) {
            return null;
        } else {
            return result.get(0);
        }
    }
}
