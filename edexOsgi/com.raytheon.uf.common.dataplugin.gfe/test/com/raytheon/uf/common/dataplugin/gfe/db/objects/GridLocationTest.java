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

import org.opengis.metadata.spatial.PixelOrientation;

import com.raytheon.uf.common.dataplugin.gfe.config.ProjectionData;
import com.raytheon.uf.common.dataplugin.gfe.config.ProjectionData.ProjectionType;
import com.raytheon.uf.common.geospatial.MapUtil;
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
 * Jan 6, 2011            randerso     Initial creation
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
                ProjectionType.LAMBERT_CONFORMAL.ordinal(), new Coordinate(
                        -133.459, 12.190), new Coordinate(-49.385, 57.290),
                new Coordinate(-95.0, 25.0), 25.0f, 25.0f, new Point(1, 1),
                new Point(93, 65), 0, 0, 0);
        GridLocation tbw = new GridLocation("TBW", grid211,
                new Point(145, 145), new Coordinate(64.00, 11.00),
                new Coordinate(9.0, 9.0), "EST5EDT");

        GridLocation gloc = tbw;
        PixelOrientation orient = PixelOrientation.UPPER_LEFT;
        Coordinate c = new Coordinate();
        Coordinate ll;

        ll = MapUtil.gridCoordinateToLatLon(c, orient, gloc);
        System.out.println("grid cell: " + c + " lonlat: " + ll);

        c.x = gloc.getNx();
        ll = MapUtil.gridCoordinateToLatLon(c, orient, gloc);
        System.out.println("grid cell: " + c + " lonlat: " + ll);

        c.y = gloc.getNy();
        ll = MapUtil.gridCoordinateToLatLon(c, orient, gloc);
        System.out.println("grid cell: " + c + " lonlat: " + ll);

        c.x = 0;
        ll = MapUtil.gridCoordinateToLatLon(c, orient, gloc);
        System.out.println("grid cell: " + c + " lonlat: " + ll);
    }
}
