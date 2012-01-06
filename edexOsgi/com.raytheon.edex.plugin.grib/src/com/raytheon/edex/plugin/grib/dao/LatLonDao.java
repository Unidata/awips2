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

import java.util.ArrayList;
import java.util.List;

import com.raytheon.edex.plugin.grib.Grib1Decoder;
import com.raytheon.uf.common.dataplugin.grib.spatial.projections.GridCoverage;
import com.raytheon.uf.common.dataplugin.grib.spatial.projections.LatLonGridCoverage;
import com.raytheon.uf.edex.database.DataAccessLayerException;
import com.raytheon.uf.edex.database.dao.DaoConfig;
import com.raytheon.uf.edex.database.query.DatabaseQuery;

/**
 * Data Access Object for retrieving LatLonGridCoverage objects
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
public class LatLonDao extends GridCoverageDao implements IGridCoverageDao {

    /**
     * Creates a new LatLonDao
     */
    public LatLonDao() {
        super(DaoConfig.forClass(LatLonGridCoverage.class));
    }

    /**
     * Checks that database to see if a grid exists in the database which very
     * closely resembles the provided coverage
     * 
     * @param coverage
     *            The LatLonGridCoverage to check against
     * @return The Grid from the database or null if not found
     * @throws DataAccessLayerException
     *             If problems during query
     */
    @SuppressWarnings("unchecked")
    public GridCoverage checkGrid(GridCoverage grid)
            throws DataAccessLayerException {

        LatLonGridCoverage coverage = (LatLonGridCoverage) grid;
        DatabaseQuery query = new DatabaseQuery(this.daoClass);

        // If dx and dy are present, use them
        if (coverage.getDx() != 65.535) {
            query.addQueryParam(addQueryTolerance("dx", coverage.getDx()));
        }
        if (coverage.getDy() != 65.535) {
            query.addQueryParam(addQueryTolerance("dy", coverage.getDy()));
        }
        query.addQueryParam(addQueryTolerance("la1", coverage.getLa1()));
        query.addQueryParam(addQueryTolerance("lo1", coverage.getLo1()));
        query.addQueryParam("nx", coverage.getNx());
        query.addQueryParam("ny", coverage.getNy());
        query.addQueryParam("firstGridPointCorner",coverage.getFirstGridPointCorner());
        List<LatLonGridCoverage> result = (List<LatLonGridCoverage>) queryByCriteria(query);

        if (result.isEmpty()) {
            return manualCheck(coverage);
        } else {
            return selectKnownGrid(result);
        }
    }

    @SuppressWarnings("unchecked")
    private GridCoverage manualCheck(LatLonGridCoverage coverage)
            throws DataAccessLayerException {

        DatabaseQuery query = new DatabaseQuery(this.daoClass);
        query.addQueryParam("nx", coverage.getNx());
        query.addQueryParam("ny", coverage.getNy());
        query.addQueryParam(addQueryTolerance("dx", coverage.getDx()));
        query.addQueryParam(addQueryTolerance("dy", coverage.getDy()));
        List<LatLonGridCoverage> result = (List<LatLonGridCoverage>) queryByCriteria(query);

        List<LatLonGridCoverage> matchingCoverages = new ArrayList<LatLonGridCoverage>();
        for (LatLonGridCoverage gridToCheck : result) {
            if (checkLat(coverage.getLa1(), gridToCheck.getLa1())
                    && checkLon(coverage.getLo1(), gridToCheck.getLo1())) {
                matchingCoverages.add(gridToCheck);
            }

        }

        return selectKnownGrid(matchingCoverages);
    }

    private boolean checkLat(double reference, double latToCheck) {
        double correctedRef = Grib1Decoder.correctLat((float) reference);
        double correctedLat = Grib1Decoder.correctLat((float) latToCheck);

        if (Math.abs(correctedRef - correctedLat) < QUERY_TOLERANCE
                || Math.abs(correctedLat - correctedRef) < QUERY_TOLERANCE) {
            return true;
        }
        return false;
    }

    private boolean checkLon(double reference, double lonToCheck) {
        double correctedRef = Grib1Decoder.correctLon((float) reference);
        double correctedLon = Grib1Decoder.correctLon((float) lonToCheck);

        if (Math.abs(correctedRef - correctedLon) < QUERY_TOLERANCE
                || Math.abs(correctedLon - correctedRef) < QUERY_TOLERANCE) {
            return true;
        }
        return false;
    }
}
