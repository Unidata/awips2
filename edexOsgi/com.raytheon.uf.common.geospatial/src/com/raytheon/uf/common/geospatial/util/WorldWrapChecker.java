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
package com.raytheon.uf.common.geospatial.util;

import org.geotools.coverage.grid.GeneralGridGeometry;
import org.geotools.referencing.CRS;
import org.geotools.referencing.operation.DefaultMathTransformFactory;
import org.geotools.referencing.operation.projection.MapProjection;
import org.geotools.referencing.operation.projection.MapProjection.AbstractProvider;
import org.opengis.parameter.ParameterValueGroup;
import org.opengis.referencing.operation.MathTransform;

import com.raytheon.uf.common.geospatial.MapUtil;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;

/**
 * Given a descriptor of a map, this class will check line segments for wrapping
 * around the world (crossing the inverse central meridian of the projection)
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jun 6, 2011            mschenke     Initial creation
 * 
 * </pre>
 * 
 * @author mschenke
 * @version 1.0
 */

public class WorldWrapChecker {

    private double inverseCentralMeridian = Double.NaN;

    private boolean low = false;

    private boolean checkForWrapping = false;

    public WorldWrapChecker(GeneralGridGeometry worldGeometry) {
        MapProjection worldProjection = CRS.getMapProjection(worldGeometry
                .getCoordinateReferenceSystem());
        double centralMeridian = 0.0;
        if (worldProjection != null) {
            ParameterValueGroup group = worldProjection.getParameterValues();
            centralMeridian = group.parameter(
                    AbstractProvider.CENTRAL_MERIDIAN.getName().getCode())
                    .doubleValue();
        }
        inverseCentralMeridian = centralMeridian + 180.0;
        if (inverseCentralMeridian > 180.0) {
            inverseCentralMeridian -= 360.0;
            low = true;
        }

        double l1 = inverseCentralMeridian - .1;
        double l2 = inverseCentralMeridian - .2;
        double r1 = inverseCentralMeridian - 359.9;
        double r2 = inverseCentralMeridian - 359.8;

        try {
            MathTransform latLonToGrid = new DefaultMathTransformFactory()
                    .createConcatenatedTransform(MapUtil
                            .getTransformFromLatLon(worldGeometry
                                    .getCoordinateReferenceSystem()),
                            worldGeometry.getGridToCRS().inverse());

            double[] in = new double[] { l1, 0.0, l2, 0.0, r1, 0.0, r2, 0.0 };
            double[] out = new double[in.length];
            latLonToGrid.transform(in, 0, out, 0, 4);

            double xl1 = out[0];
            double xl2 = out[2];
            double xr1 = out[4];
            double xr2 = out[6];

            checkForWrapping = Math.abs(xl1 - xr1) > Math.abs(xl2 - xr2);
        } catch (Throwable t) {
            UFStatus.getHandler().handle(Priority.PROBLEM,
                    "Error determing world wrap checking", t);
        }
    }

    /**
     * Checks to see if the line between point a and b wrap around the world
     * 
     * @param a
     *            lat/lon starting coordinate
     * @param b
     *            lat/lon ending coordinate
     * @return true if wraps, false otherwise
     */
    public boolean check(double aLon, double bLon) {
        if (!checkForWrapping) {
            return false;
        }

        aLon = toProjectionRange(aLon);
        bLon = toProjectionRange(bLon);

        return Math.abs(aLon - bLon) > 180.0;
    }

    public double getInverseCentralMeridian() {
        return inverseCentralMeridian;
    }

    public double toProjectionRange(double aLon) {
        if (low && aLon < inverseCentralMeridian) {
            aLon += 360;
        } else if (!low && aLon > inverseCentralMeridian) {
            aLon -= 360;
        }
        return aLon;
    }

    public boolean needsChecking() {
        return checkForWrapping;
    }
}
