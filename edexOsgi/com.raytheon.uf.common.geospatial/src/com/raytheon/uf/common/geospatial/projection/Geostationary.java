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
 * further licenMath.sing information.
 **/
package com.raytheon.uf.common.geospatial.projection;

import java.awt.geom.Point2D;

import javax.measure.unit.SI;
import javax.measure.unit.Unit;

import org.geotools.parameter.DefaultParameterDescriptor;
import org.geotools.parameter.DefaultParameterDescriptorGroup;
import org.geotools.referencing.operation.MathTransformProvider;
import org.geotools.referencing.operation.projection.MapProjection;
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
 * Geostationary map projection. Earth as viewed from space.
 * 
 * TODO Add support latitude of origin != 0.0
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jun 27, 2013            mschenke     Initial creation
 * 
 * </pre>
 * 
 * @author mschenke
 * @version 1.0
 */

public class Geostationary extends MapProjection {

    private static final long serialVersionUID = 4747155239658791357L;

    public static final String PROJECTION_NAME = "Geostationary";

    public static final String PERSPECTIVE_HEIGHT = "perspective_height";

    public static final String SWEEP_AXIS = "sweep_axis";

    static final double DEFAULT_PERSPECTIVE_HEIGHT = 35800000.0;

    private double perspectiveHeight = DEFAULT_PERSPECTIVE_HEIGHT;

    private boolean swapAxis = false;

    private double radius_g, radius_g_1, radius_p, radius_p2, radius_p_inv2, C;

    /**
     * @param values
     * @throws ParameterNotFoundException
     */
    protected Geostationary(ParameterValueGroup values)
            throws ParameterNotFoundException {
        super(values);
        this.perspectiveHeight = Provider.getValue(Provider.PERSPECTIVE_HEIGHT,
                values);
        double sweepValue = Provider.getValue(Provider.SWEEP_AXIS, values);
        this.swapAxis = sweepValue == 1.0;
        double h = perspectiveHeight;
        double a = semiMajor;
        double b = semiMinor;
        double es = 1.0 - (b * b) / (a * a);
        double one_es = 1.0 - es;
        double rone_es = 1.0 / one_es;

        radius_g_1 = h / a;
        radius_g = 1 + radius_g_1;
        radius_p2 = one_es;
        radius_p_inv2 = rone_es;
        radius_p = Math.sqrt(radius_p2);
        C = radius_g * radius_g - 1.0;
    }

    /*
     * (non-Javadoc)
     * 
     * @see org.geotools.referencing.operation.projection.MapProjection#
     * getParameterDescriptors()
     */
    @Override
    public ParameterDescriptorGroup getParameterDescriptors() {
        return Provider.PARAMETERS;
    }

    /*
     * (non-Javadoc)
     * 
     * @see org.geotools.referencing.operation.projection.MapProjection#
     * inverseTransformNormalized(double, double, java.awt.geom.Point2D)
     */
    @Override
    protected Point2D inverseTransformNormalized(double x, double y,
            Point2D ptDst) throws ProjectionException {
        double lam, phi;
        double Vx, Vy, Vz, a, b, det, k;

        /* Setting three components of vector from satellite to position. */
        Vx = -1.0;
        if (swapAxis) {
            Vz = Math.tan(y / radius_g_1);
            Vy = Math.tan(x / radius_g_1) * Math.hypot(1.0, Vz);
        } else {
            Vy = Math.tan(x / radius_g_1);
            Vz = Math.tan(y / radius_g_1) * Math.hypot(1.0, Vy);
        }
        /* Calculation of terms in cubic equation and determinant. */
        a = Vz / radius_p;
        a = Vy * Vy + a * a + Vx * Vx;
        b = 2 * radius_g * Vx;
        if ((det = (b * b) - 4 * a * C) < 0.) {
            lam = phi = Double.NaN;
        } else {
            /*
             * Calculation of three components of vector from satellite to
             * position.
             */
            k = (-b - Math.sqrt(det)) / (2. * a);
            Vx = radius_g + k * Vx;
            Vy *= k;
            Vz *= k;
            /* Calculation of longitude and latitude. */
            lam = Math.atan2(Vy, Vx);
            phi = Math.atan(Vz * Math.cos(lam) / Vx);
            phi = Math.atan(radius_p_inv2 * Math.tan(phi));
        }

        if (ptDst == null) {
            ptDst = new Point2D.Double();
        }
        ptDst.setLocation(lam, phi);
        return ptDst;
    }

    /*
     * (non-Javadoc)
     * 
     * @see org.geotools.referencing.operation.projection.MapProjection#
     * transformNormalized(double, double, java.awt.geom.Point2D)
     */
    @Override
    protected Point2D transformNormalized(double lam, double phi, Point2D ptDst)
            throws ProjectionException {
        double x, y;
        double r, Vx, Vy, Vz, tmp;

        /* Calculation of geocentric latitude. */
        phi = Math.atan(radius_p2 * Math.tan(phi));
        /*
         * Calculation of the three components of the vector from satellite to*
         * position on earth surface (lon,lat).
         */
        r = (radius_p) / Math.hypot(radius_p * Math.cos(phi), Math.sin(phi));
        Vx = r * Math.cos(lam) * Math.cos(phi);
        Vy = r * Math.sin(lam) * Math.cos(phi);
        Vz = r * Math.sin(phi);
        /* Check visibility. */
        if (((radius_g - Vx) * Vx - Vy * Vy - Vz * Vz * radius_p_inv2) < 0.) {
            x = y = Double.NaN;
        } else {
            /* Calculation based on view angles from satellite. */
            tmp = radius_g - Vx;
            if (swapAxis) {
                x = radius_g_1 * Math.atan(Vy / Math.hypot(Vz, tmp));
                y = radius_g_1 * Math.atan(Vz / tmp);
            } else {
                x = radius_g_1 * Math.atan(Vy / tmp);
                y = radius_g_1 * Math.atan(Vz / Math.hypot(Vy, tmp));
            }
        }

        if (ptDst == null) {
            ptDst = new Point2D.Double();
        }
        ptDst.setLocation(x, y);
        return ptDst;
    }

    public static class Provider extends AbstractProvider {

        private static final long serialVersionUID = 3868187206568280453L;

        static final ParameterDescriptor<Double> PERSPECTIVE_HEIGHT = DefaultParameterDescriptor
                .create(Geostationary.PERSPECTIVE_HEIGHT,
                        DEFAULT_PERSPECTIVE_HEIGHT, 0, Double.MAX_VALUE,
                        SI.METER);

        static final ParameterDescriptor<Double> SWEEP_AXIS = DefaultParameterDescriptor
                .create(Geostationary.SWEEP_AXIS, 0.0, 0.0, 1.0, Unit.ONE);

        static final ParameterDescriptorGroup PARAMETERS = new DefaultParameterDescriptorGroup(
                PROJECTION_NAME, new ParameterDescriptor[] {
                        SEMI_MAJOR, SEMI_MINOR, CENTRAL_MERIDIAN,
                        LATITUDE_OF_ORIGIN, FALSE_EASTING, FALSE_NORTHING,
                        PERSPECTIVE_HEIGHT, SWEEP_AXIS });

        public Provider() {
            super(PARAMETERS);
        }

        /*
         * (non-Javadoc)
         * 
         * @see org.geotools.referencing.operation.MathTransformProvider#
         * createMathTransform (org.opengis.parameter.ParameterValueGroup)
         */
        @Override
        protected MathTransform createMathTransform(ParameterValueGroup values)
                throws InvalidParameterNameException,
                ParameterNotFoundException, InvalidParameterValueException,
                FactoryException {
            return new Geostationary(values);
        }

        static <T> T getValue(ParameterDescriptor<T> descriptor,
                ParameterValueGroup group) {
            return MathTransformProvider.value(descriptor, group);
        }

    }

}
