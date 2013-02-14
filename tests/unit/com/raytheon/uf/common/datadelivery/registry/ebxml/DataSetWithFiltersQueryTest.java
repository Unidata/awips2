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
package com.raytheon.uf.common.datadelivery.registry.ebxml;

import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;

import java.util.HashMap;
import java.util.Map;

import org.geotools.geometry.jts.ReferencedEnvelope;
import org.junit.Test;

import com.raytheon.uf.common.datadelivery.registry.DataLevelType;
import com.raytheon.uf.common.datadelivery.registry.DataLevelType.LevelType;
import com.raytheon.uf.common.datadelivery.registry.DataSet;
import com.raytheon.uf.common.datadelivery.registry.GriddedCoverage;
import com.raytheon.uf.common.datadelivery.registry.OpenDapGriddedDataSet;
import com.raytheon.uf.common.datadelivery.registry.Parameter;
import com.raytheon.uf.common.geospatial.MapUtil;
import com.raytheon.uf.common.gridcoverage.Corner;
import com.raytheon.uf.common.gridcoverage.LatLonGridCoverage;
import com.raytheon.uf.common.gridcoverage.exception.GridCoverageException;
import com.raytheon.uf.common.util.CollectionUtil;
import com.vividsolutions.jts.geom.Coordinate;
import com.vividsolutions.jts.geom.Envelope;

/**
 * Test {@link DataSetWithFiltersQuery}.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jul 31, 2012 955        djohnson     Initial creation
 * Aug 16, 2012 1022       djohnson     Use concrete implementation of DataSet for test.
 * Nov 19, 2012 1166       djohnson     Clean up JAXB representation of registry objects.
 * Dec 10, 2012 1259       bsteffen     Switch Data Delivery from LatLon to referenced envelopes.
 * Jan 02, 2012 1345       djohnson     Fix broken code from referenced envelope switch.
 * 
 * </pre>
 * 
 * @author djohnson
 * @version 1.0
 */

public class DataSetWithFiltersQueryTest {

    @Test
    public void testSatisfiesFilterCriteriaReturnsTrueIfNoLevelsOrAreaSpecified() {
        assertTrue("Should return true when no filters specified!",
                DataSetWithFiltersQuery.satisfiesFilterCriteria(null, null,
                        null));
    }

    @Test
    public void testSatisfiesFilterCriteriaReturnsTrueWhenOnlySatisfiableLevelsSpecified() {
        DataLevelType dataLevelType = new DataLevelType(LevelType.SFC);
        
        Parameter parameter = new Parameter();
        parameter.setLevelType(java.util.Arrays.asList(dataLevelType));

        Map<String, Parameter> paramMap = new HashMap<String, Parameter>();
        paramMap.put("whatShouldThisKeyBe?", parameter);

        DataSet dataSet = new OpenDapGriddedDataSet();
        dataSet.setParameters(paramMap);

        assertTrue("Should return true when satisfiable levels specified!",
                DataSetWithFiltersQuery.satisfiesFilterCriteria(dataSet,
                        CollectionUtil.asSet(LevelType.SFC), null));
    }

    @Test
    public void testSatisfiesFilterCriteriaReturnsFalseWhenLevelsSpecifiedThatAreUnsatisfiable() {
        DataLevelType dataLevelType = new DataLevelType(LevelType.SFC);

        Parameter parameter = new Parameter();
        parameter.setLevelType(java.util.Arrays.asList(dataLevelType));

        Map<String, Parameter> paramMap = new HashMap<String, Parameter>();
        paramMap.put("whatShouldThisKeyBe?", parameter);

        DataSet dataSet = new OpenDapGriddedDataSet();
        dataSet.setParameters(paramMap);

        assertFalse("Should return false when unsatisfiable levels specified!",
                DataSetWithFiltersQuery.satisfiesFilterCriteria(dataSet,
                        CollectionUtil.asSet(LevelType.CBL), null));
    }

    @Test
    public void testSatisfiesFilterCriteriaReturnsTrueWhenOnlySatisfiableAreaSpecified()
            throws GridCoverageException {

        LatLonGridCoverage gridCoverage = new LatLonGridCoverage();
        gridCoverage.setCrsWKT("BoundingBox");
        gridCoverage.setLa1(89);
        gridCoverage.setLo1(-179);
        gridCoverage.setDx(1.0);
        gridCoverage.setDy(1.0);
        gridCoverage.setNx(360);
        gridCoverage.setNy(180);
        gridCoverage.setSpacingUnit("degree");
        gridCoverage.setFirstGridPointCorner(Corner.UpperLeft);
        gridCoverage.initialize();

        GriddedCoverage coverage = new GriddedCoverage();
        coverage.setGridCoverage(gridCoverage);

        DataSet dataSet = new OpenDapGriddedDataSet();
        dataSet.setCoverage(coverage);

        // Choose a smaller bounding box inside the dataset's
        ReferencedEnvelope selectedAreaCoordinates = new ReferencedEnvelope(
                MapUtil.LATLON_PROJECTION);
        final Coordinate upperLeft = coverage.getUpperLeft();
        final Coordinate lowerRight = coverage.getLowerRight();
        selectedAreaCoordinates.expandToInclude(upperLeft.x + 1,
                upperLeft.y - 1);
        selectedAreaCoordinates.expandToInclude(lowerRight.x - 1,
                lowerRight.y + 1);
        assertTrue("Should return true when satisfiable area is specified!",
                DataSetWithFiltersQuery.satisfiesFilterCriteria(dataSet, null,
                        selectedAreaCoordinates));
    }

    @Test
    public void testSatisfiesFilterCriteriaReturnsFalseWhenAreaIsSpecifiedThatIsUnsatisfiable()
            throws GridCoverageException {

        LatLonGridCoverage gridCoverage = new LatLonGridCoverage();
        gridCoverage.setCrsWKT("Polygon");
        gridCoverage.setLa1(10);
        gridCoverage.setLo1(-10);
        gridCoverage.setDx(1.0);
        gridCoverage.setDy(1.0);
        gridCoverage.setNx(21);
        gridCoverage.setNy(21);
        gridCoverage.setSpacingUnit("degree");
        gridCoverage.setFirstGridPointCorner(Corner.UpperLeft);
        gridCoverage.initialize();

        GriddedCoverage coverage = new GriddedCoverage();
        coverage.setGridCoverage(gridCoverage);

        DataSet dataSet = new OpenDapGriddedDataSet();
        dataSet.setCoverage(coverage);

        // Choose a bounding box lying outside of the dataset's
        ReferencedEnvelope selectedAreaCoordinates = new ReferencedEnvelope(
                new Envelope(new Coordinate(-15, -14), new Coordinate(-14, -13)),
                        MapUtil.LATLON_PROJECTION);

        assertFalse(
                "Should return false when unsatisfiable area is specified!",
                DataSetWithFiltersQuery.satisfiesFilterCriteria(dataSet, null,
                        selectedAreaCoordinates));
    }
}
