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
package com.raytheon.uf.common.dataplugin.grib;

import org.opengis.metadata.spatial.PixelOrientation;

import com.raytheon.uf.common.geospatial.MapUtil;
import com.raytheon.uf.common.gridcoverage.Corner;
import com.raytheon.uf.common.gridcoverage.LambertConformalGridCoverage;
import com.raytheon.uf.common.gridcoverage.exception.GridCoverageException;
import com.vividsolutions.jts.geom.Coordinate;

/**
 * TODO Add Description
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jan 4, 2011            randerso     Initial creation
 * 
 * </pre>
 * 
 * @author randerso
 * @version 1.0
 */

public class CoverageTest {

    /**
     * @param args
     */
    public static void main(String[] args) {

        LambertConformalGridCoverage cov = new LambertConformalGridCoverage();
        cov.setNx(93);
        cov.setNy(65);
        cov.setFirstGridPointCorner(Corner.LowerLeft);
        cov.setDx(81.2705);
        cov.setDy(81.2705);
        cov.setLatin1(25);
        cov.setLatin2(25);
        cov.setLov(-95);
        cov.setLa1(12.19);
        cov.setLo1(-133.459);
        cov.setMajorAxis(6371229.0);
        cov.setMinorAxis(6371229.0);
        cov.setSpacingUnit("km");

        try {
            cov.initialize();

            Coordinate c = new Coordinate(0, 0);
            PixelOrientation orient = PixelOrientation.UPPER_LEFT;
            Coordinate ll = MapUtil.gridCoordinateToLatLon(c, orient, cov);
            System.out.println("grid cell: " + c + " lonLat: " + ll);

            c.x = cov.getNx();
            ll = MapUtil.gridCoordinateToLatLon(c, orient, cov);
            System.out.println("grid cell: " + c + " lonLat: " + ll);

            c.y = cov.getNy();
            ll = MapUtil.gridCoordinateToLatLon(c, orient, cov);
            System.out.println("grid cell: " + c + " lonLat: " + ll);

            c.x = 0;
            ll = MapUtil.gridCoordinateToLatLon(c, orient, cov);
            System.out.println("grid cell: " + c + " lonLat: " + ll);

            c.x = 64 - 1;
            c.y = cov.getNy() - 11;
            ll = MapUtil.gridCoordinateToLatLon(c, orient, cov);
            System.out.println("grid cell: " + c + " lonLat: " + ll);

            c.x = 73 - 1;
            ll = MapUtil.gridCoordinateToLatLon(c, orient, cov);
            System.out.println("grid cell: " + c + " lonLat: " + ll);

            c.y = cov.getNy() - 20;
            ll = MapUtil.gridCoordinateToLatLon(c, orient, cov);
            System.out.println("grid cell: " + c + " lonLat: " + ll);

            c.x = 64 - 1;
            ll = MapUtil.gridCoordinateToLatLon(c, orient, cov);
            System.out.println("grid cell: " + c + " lonLat: " + ll);

        } catch (GridCoverageException e) {
            e.printStackTrace();
        }
    }

}
