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
package com.raytheon.uf.common.dataplugin.radar.projection;

import java.awt.geom.Point2D;

import org.geotools.parameter.DefaultParameterDescriptorGroup;
import org.geotools.referencing.operation.projection.ObliqueStereographic;
import org.geotools.referencing.operation.projection.ProjectionException;
import org.opengis.parameter.InvalidParameterNameException;
import org.opengis.parameter.InvalidParameterValueException;
import org.opengis.parameter.ParameterDescriptor;
import org.opengis.parameter.ParameterDescriptorGroup;
import org.opengis.parameter.ParameterNotFoundException;
import org.opengis.parameter.ParameterValueGroup;
import org.opengis.referencing.FactoryException;
import org.opengis.referencing.operation.MathTransform;

/**
 * Coordinates in the AzimuthRangeProjection represent the azimuth and range
 * relative to the point defined by the intersection of the central meridian and
 * the latitude of origin. The x coordinate is the azimuth angle in degrees and
 * the the y coordinate is the distance from the point in meters.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jun 6, 2012            bsteffen     Initial creation
 * 
 * </pre>
 * 
 * @author bsteffen
 * @version 1.0
 */

public class AzimuthRangeMapProjection extends ObliqueStereographic {

    private static final long serialVersionUID = 4469133725910219322L;

    protected AzimuthRangeMapProjection(ParameterValueGroup values)
            throws ParameterNotFoundException {
        super(values);
        this.globalScale = 1.0;
    }

    @Override
    public ParameterDescriptorGroup getParameterDescriptors() {
        return Provider.PARAMETERS;
    }

    @Override
    protected Point2D inverseTransformNormalized(double azimuth, double range,
            Point2D dest) throws ProjectionException {
        range = range / (scaleFactor * semiMajor);
        azimuth = Math.toRadians(azimuth);
        double x = range * Math.sin(azimuth);
        double y = range * Math.cos(azimuth);
        return super.inverseTransformNormalized(x, y, dest);
    }

    @Override
    protected Point2D transformNormalized(double lon, double lat, Point2D dest)
            throws ProjectionException {
        Point2D tmp = new Point2D.Double();
        super.transformNormalized(lon, lat, tmp);
        double x = tmp.getX();
        double y = tmp.getY();
        double azimuth = Math.toDegrees(Math.atan2(x, y));
        // Math.hypot is more accurate than the sqrt approach but much much
        // slower.
        double range = scaleFactor * semiMajor * Math.sqrt(x * x + y * y);
        if (dest != null) {
            dest.setLocation(azimuth, range);
            return dest;
        }
        return new Point2D.Double(azimuth, range);
    }

    public static class Provider extends AbstractProvider {

        private static final long serialVersionUID = 6736153862347982008L;

        static final ParameterDescriptorGroup PARAMETERS = new DefaultParameterDescriptorGroup(
                "Azimuth_Range", new ParameterDescriptor[] { SEMI_MAJOR,
                        SEMI_MINOR, CENTRAL_MERIDIAN, LATITUDE_OF_ORIGIN,
                        SCALE_FACTOR, FALSE_EASTING, FALSE_NORTHING });

        public Provider() {
            super(PARAMETERS);
        }

        @Override
        protected MathTransform createMathTransform(ParameterValueGroup values)
                throws InvalidParameterNameException,
                ParameterNotFoundException, InvalidParameterValueException,
                FactoryException {
            return new AzimuthRangeMapProjection(values);
        }

    }

}
