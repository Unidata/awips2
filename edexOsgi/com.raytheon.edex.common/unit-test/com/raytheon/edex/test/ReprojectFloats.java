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

package com.raytheon.edex.test;

import java.awt.image.DataBuffer;

import org.geotools.coverage.grid.GridCoverage2D;

import com.raytheon.uf.common.geospatial.MapUtil;
import com.vividsolutions.jts.geom.Coordinate;
import com.vividsolutions.jts.geom.GeometryFactory;
import com.vividsolutions.jts.geom.Point;

public class ReprojectFloats {

    public static void main(String[] arg) {

        try {
            GeometryFactory gf = new GeometryFactory();

            Point[] p = new Point[4];

            p[0] = gf.createPoint(new Coordinate(-100, 30)); // upper left
            p[1] = gf.createPoint(new Coordinate(-100, 20)); // lower left
            p[2] = gf.createPoint(new Coordinate(-90, 30)); // upper right
            p[3] = gf.createPoint(new Coordinate(-90, 20)); // lower right

            float[][] f = new float[1024][1024]; // my grid data

            for (int i = 250; i < 750; i++) {
                f[i][512] = 1.0f;
            }

            GridCoverage2D gc = MapUtil.constructGridCoverage(
                    "My First Grid Coverage", f,
                    MapUtil.AWIPS_LAMBERT_NORTHAMERICA, p);

            GridCoverage2D projectedGC = MapUtil.reprojectCoverage(gc,
                    MapUtil.LATLON_PROJECTION);

            float[][] projectedData = new float[1024][1024];

            DataBuffer db = projectedGC.getRenderedImage().getData()
                    .getDataBuffer();
            for (int i = 0; i < 1024; i++) {
                for (int j = 0; j < 1024; j++) {
                    projectedData[i][j] = db.getElemFloat(j + (1024 * i));
                }
            }

        } catch (Exception e) {
            // TODO Auto-generated catch block
            e.printStackTrace();
        }

    }
}
