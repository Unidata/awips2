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
package com.raytheon.uf.edex.datadelivery.retrieval.opendap;

import static org.junit.Assert.assertEquals;

import org.geotools.geometry.jts.ReferencedEnvelope;
import org.junit.Test;

import com.raytheon.uf.common.datadelivery.registry.GriddedCoverage;
import com.raytheon.uf.common.geospatial.MapUtil;
import com.raytheon.uf.common.gridcoverage.Corner;
import com.raytheon.uf.common.gridcoverage.GridCoverage;
import com.raytheon.uf.common.gridcoverage.LatLonGridCoverage;
import com.raytheon.uf.common.gridcoverage.exception.GridCoverageException;

/**
 * Test {@link OpenDAPRequestBuilder}.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Aug 14, 2012 1022       djohnson     Initial creation
 * Dec 10, 2012 1259       bsteffen     Switch Data Delivery from LatLon to referenced envelopes.
 * 
 * </pre>
 * 
 * @author djohnson
 * @version 1.0
 */

public class OpenDAPRequestBuilderTest {

    @Test
    public void testProperGridCoordinatesResultsInCorrectOpenDapString() {
        GridCoverage coverage = new LatLonGridCoverage();
        coverage.setDx(1.0);
        coverage.setDy(1.0);
        coverage.setNx(61);
        coverage.setNy(61);
        coverage.setLa1(30);
        coverage.setLo1(-30);
        coverage.setSpacingUnit("degree");
        coverage.setFirstGridPointCorner(Corner.UpperLeft);

        try {
            coverage.initialize();
        } catch (GridCoverageException e) {
            e.printStackTrace();
        }

        GriddedCoverage griddedCoverage = new GriddedCoverage();
        griddedCoverage.setGridCoverage(coverage);

        griddedCoverage.setRequestEnvelope(new ReferencedEnvelope(0, 10, -20,
                -10, MapUtil.LATLON_PROJECTION));

        String coverageString = OpenDAPRequestBuilder
                .getCoverageString(griddedCoverage);
        assertEquals("Improper OpenDAP grid coordinate string specified!",
                "[10:1:20][30:1:40]", coverageString);
    }
}
