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
import org.geotools.referencing.operation.projection.MapProjection;
import org.geotools.referencing.operation.projection.MapProjection.AbstractProvider;
import org.opengis.geometry.Envelope;
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

    private double lowInverseCentralMeridian = Double.NaN;

    private double highInverseCentralMeridian = Double.NaN;

    private boolean checkForWrapping = false;

    public WorldWrapChecker(Envelope worldEnvelope) {
        MapProjection worldProjection = CRS.getMapProjection(worldEnvelope
                .getCoordinateReferenceSystem());
        double centralMeridian = 0.0;
        if (worldProjection != null) {
            ParameterValueGroup group = worldProjection.getParameterValues();
            centralMeridian = group.parameter(
                    AbstractProvider.CENTRAL_MERIDIAN.getName().getCode())
                    .doubleValue();
        }

        highInverseCentralMeridian = centralMeridian + 180.0;
        lowInverseCentralMeridian = centralMeridian - 180.0;

        double l1 = highInverseCentralMeridian - .1;
        double l2 = highInverseCentralMeridian - .2;
        double r1 = highInverseCentralMeridian - 359.9;
        double r2 = highInverseCentralMeridian - 359.8;

        try {
            MathTransform latLonToCRS = MapUtil
                    .getTransformFromLatLon(worldEnvelope
                            .getCoordinateReferenceSystem());

            double[] in = new double[] { l1, 0.0, l2, 0.0, r1, 0.0, r2, 0.0 };
            double[] out = new double[in.length];
            latLonToCRS.transform(in, 0, out, 0, 4);

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

    public WorldWrapChecker(GeneralGridGeometry worldGeometry) {
        this(worldGeometry.getEnvelope());
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

    /**
     * @return the lowInverseCentralMeridian
     */
    public double getLowInverseCentralMeridian() {
        return lowInverseCentralMeridian;
    }

    /**
     * @return the highInverseCentralMeridian
     */
    public double getHighInverseCentralMeridian() {
        return highInverseCentralMeridian;
    }

    public double toProjectionRange(double aLon) {
        while (aLon < lowInverseCentralMeridian) {
            aLon += 360.0;
        }
        while (aLon > highInverseCentralMeridian) {
            aLon -= 360.0;
        }
        return aLon;
    }

    public boolean needsChecking() {
        return checkForWrapping;
    }
}
