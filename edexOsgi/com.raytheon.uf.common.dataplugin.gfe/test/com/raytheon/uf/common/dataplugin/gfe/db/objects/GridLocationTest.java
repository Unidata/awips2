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
package com.raytheon.uf.common.dataplugin.gfe.db.objects;

import java.awt.Point;

import org.geotools.coverage.grid.GridGeometry2D;
import org.opengis.metadata.spatial.PixelOrientation;

import com.raytheon.uf.common.dataplugin.gfe.config.ProjectionData;
import com.raytheon.uf.common.dataplugin.gfe.config.ProjectionData.ProjectionType;
import com.raytheon.uf.common.dataplugin.gfe.db.objects.GridLocation.PythonNumpyLatLonGrid;
import com.raytheon.uf.common.geospatial.MapUtil;
import com.vividsolutions.jts.geom.Coordinate;

/**
 * GridLocation unit test
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * May 20, 2014  #3069     randerso    Initial creation
 * 
 * </pre>
 * 
 * @author randerso
 * @version 1.0
 */

public class GridLocationTest {

    /**
     * @param args
     */
    public static void main(String[] args) {
        ProjectionData grid211 = new ProjectionData("Grid211",
                ProjectionType.LAMBERT_CONFORMAL, new Coordinate(-133.459,
                        12.190), new Coordinate(-49.385, 57.290),
                new Coordinate(-95.0, 25.0), 25.0f, 25.0f, new Point(1, 1),
                new Point(93, 65), 0.0f, 0.0f, 0.0f);

        // GridLocation gloc = new GridLocation("ABR", grid211,
        // new Point(145, 145), new Coordinate(45.0, 35.0),
        // new Coordinate(9, 9), "CST6CDT");

        GridLocation gloc = new GridLocation("OAX", grid211,
                new Point(417, 289), new Coordinate(41.0, 29.0),
                new Coordinate(13, 9), "CST6CDT");

        System.out.println(gloc.getSiteId());
        Coordinate gridCoord = new Coordinate();
        Coordinate latLon = new Coordinate();

        System.out.println("geometry: " + gloc.getGeometry());

        try {
            gridCoord.x = 0;
            gridCoord.y = 0;
            latLon = MapUtil.gridCoordinateToLatLon(gridCoord,
                    PixelOrientation.CENTER, gloc);
            System.out.println(gridCoord.x + "," + gridCoord.y + "  " + latLon);

            gridCoord.x = 0;
            gridCoord.y = gloc.getNy() - 1;
            latLon = MapUtil.gridCoordinateToLatLon(gridCoord,
                    PixelOrientation.CENTER, gloc);
            System.out.println(gridCoord.x + "," + gridCoord.y + "  " + latLon);

            gridCoord.x = gloc.getNx() - 1;
            gridCoord.y = gloc.getNy() - 1;
            latLon = MapUtil.gridCoordinateToLatLon(gridCoord,
                    PixelOrientation.CENTER, gloc);
            System.out.println(gridCoord.x + "," + gridCoord.y + "  " + latLon);

            gridCoord.x = gloc.getNx() - 1;
            gridCoord.y = 0;
            latLon = MapUtil.gridCoordinateToLatLon(gridCoord,
                    PixelOrientation.CENTER, gloc);
            System.out.println(gridCoord.x + "," + gridCoord.y + "  " + latLon);

            GridGeometry2D gridGeometry = MapUtil.getGridGeometry(gloc);
            System.out.println(gridGeometry.getEnvelope2D().toString());
            System.out.println(gridGeometry.toString());

            PythonNumpyLatLonGrid latLonGrid = gloc.getLatLonGrid();
            float[] data = (float[]) latLonGrid.getNumpy()[0];
            for (int x = 0; x < gloc.getNx(); x++) {
                for (int y = 0; y < gloc.getNy(); y++) {
                    int idx = 2 * ((x * gloc.ny) + y);
                    float lon = data[idx];
                    float lat = data[idx + 1];
                    System.out.println(x + "," + y + "  " + lon + ", " + lat);
                }
            }
        } catch (Exception e) {
            e.printStackTrace();
        }
    }

}
