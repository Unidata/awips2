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
package com.raytheon.uf.common.datadelivery.registry;

import java.util.Random;

import com.raytheon.uf.common.gridcoverage.Corner;
import com.raytheon.uf.common.gridcoverage.LatLonGridCoverage;
import com.raytheon.uf.common.gridcoverage.exception.GridCoverageException;
import com.raytheon.uf.common.util.AbstractFixture;

/**
 * Fixture for {@link GriddedCoverage}s.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jan 30, 2013 1543       djohnson     Initial creation
 * 
 * </pre>
 * 
 * @author djohnson
 * @version 1.0
 */

public class GriddedCoverageFixture extends AbstractFixture<GriddedCoverage> {

    public static final GriddedCoverageFixture INSTANCE = new GriddedCoverageFixture();

    /**
     * Prevent construction.
     */
    private GriddedCoverageFixture() {

    }

    /**
     * {@inheritDoc}
     */
    @Override
    public GriddedCoverage getInstance(long seedValue, Random random) {
        LatLonGridCoverage gridCoverage = new LatLonGridCoverage();
        gridCoverage.setCrsWKT("Polygon");
        gridCoverage.setLa1(10 + seedValue);
        gridCoverage.setLo1(-10 - seedValue);
        gridCoverage.setDx(1.0);
        gridCoverage.setDy(1.0);
        gridCoverage.setNx(21);
        gridCoverage.setNy(21);
        gridCoverage.setSpacingUnit("degree");
        gridCoverage.setFirstGridPointCorner(Corner.UpperLeft);
        try {
            gridCoverage.initialize();
        } catch (GridCoverageException e) {
            throw new RuntimeException(e);
        }

        GriddedCoverage coverage = new GriddedCoverage();
        coverage.setGridCoverage(gridCoverage);
        return coverage;
    }

}
