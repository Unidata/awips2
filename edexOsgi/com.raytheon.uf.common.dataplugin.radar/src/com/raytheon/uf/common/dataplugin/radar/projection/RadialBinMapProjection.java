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
import java.util.Arrays;

import org.geotools.parameter.DefaultParameterDescriptor;
import org.geotools.parameter.DefaultParameterDescriptorGroup;
import org.geotools.referencing.operation.MathTransformProvider;
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
 * Coordinates in the RadialBinMapProjection represent the radial and bin
 * relative to the point defined by the intersection of the central meridian and
 * the latitude of origin. The x coordinate is the radial index into the
 * angleData, the y coordinate is the bin number, which is computed using the
 * binWidth and the tilt angle.
 * 
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date          Ticket#  Engineer  Description
 * ------------- -------- --------- --------------------------------------------
 * Jun 06, 2012           bsteffen  Initial creation
 * Feb 08, 2012  2672     bsteffen  Fix projection of points between the last
 *                                  and first radial.
 * Jan 24, 2018  6907     bsteffen  Fix inaccuracies when comparing floats and
 *                                  doubles.
 * 
 * </pre>
 * 
 * @author bsteffen
 */
public class RadialBinMapProjection extends AzimuthRangeMapProjection {

    private static final long serialVersionUID = 6392115431894559801L;

    private float[] normalAngleData;

    private double flatBinLength;

    protected RadialBinMapProjection(ParameterValueGroup values)
            throws ParameterNotFoundException {
        super(values);
        float[] angleData = Provider.getValue(Provider.ANGLE_DATA, values);
        normalAngleData = new float[angleData.length];
        normalAngleData[0] = angleData[0];
        for (int i = 1; i < angleData.length; i += 1) {
            if (angleData[i] <= angleData[0]) {
                normalAngleData[i] = angleData[i] + 360;
            } else {
                normalAngleData[i] = angleData[i];
            }
        }
        double tiltAngle = Provider.getValue(Provider.TILT_ANGLE, values);
        double binLength = Provider.getValue(Provider.BIN_LENGTH, values);
        this.flatBinLength = binLength * Math.cos(Math.toRadians(tiltAngle));
    }

    @Override
    public ParameterDescriptorGroup getParameterDescriptors() {
        return Provider.PARAMETERS;
    }

    @Override
    protected Point2D inverseTransformNormalized(double radial, double bin,
            Point2D dest) throws ProjectionException {
        double ran = bin * flatBinLength;
        while (radial < 0) {
            radial += normalAngleData.length;
        }
        int prevRadial = (int) Math.floor(radial) % normalAngleData.length;
        int nextRadial = (int) Math.ceil(radial) % normalAngleData.length;
        float prevAngle = normalAngleData[prevRadial];
        float nextAngle = normalAngleData[nextRadial];
        if (nextAngle + 180 < prevAngle) {
            nextAngle += 360;
        } else if (nextAngle - 180 > prevAngle) {
            nextAngle -= 360;
        }
        double az = prevAngle + (radial - prevRadial) * (nextAngle - prevAngle);
        return super.inverseTransformNormalized(az, ran, dest);
    }

    @Override
    protected Point2D transformNormalized(double lon, double lat, Point2D dest)
            throws ProjectionException {
        if (dest == null) {
            dest = new Point2D.Double();
        }
        Point2D tmp = new Point2D.Double();
        tmp = super.transformNormalized(lon, lat, tmp);
        double az = tmp.getX();
        double ran = tmp.getY();
        double bin = ran / flatBinLength;
        double firstAngle = normalAngleData[0];
        double lastAngle = normalAngleData[normalAngleData.length - 1];
        // normalize the range for az
        while (az < firstAngle) {
            az += 360;
        }
        while (az > firstAngle + 360) {
            az -= 360;
        }
        if (az >= lastAngle) {
            // special case of az is not between two normalizedAngles
            double radial = normalAngleData.length - 1
                    + (az - lastAngle) / (firstAngle + 360 - lastAngle);
            dest.setLocation(radial, bin);
        } else {
            // start off with a guess for the index
            int index = (int) ((az - firstAngle) * (normalAngleData.length - 1)
                    / (lastAngle - firstAngle));
            // increase index if we guessed to low
            double nextAngle = normalAngleData[index];
            while (nextAngle <= az) {
                index += 1;
                nextAngle = normalAngleData[index];
            }
            index -= 1;
            // decrease index if we guessed to high.
            double prevAngle = normalAngleData[index];
            while (prevAngle > az) {
                nextAngle = prevAngle;
                index -= 1;
                prevAngle = normalAngleData[index];
            }
            // interpolate a result.
            double radial = index + (az - prevAngle) / (nextAngle - prevAngle);
            dest.setLocation(radial, bin);
        }
        return dest;
    }

    @Override
    public int hashCode() {
        final int prime = 31;
        int result = super.hashCode();
        long temp;
        temp = Double.doubleToLongBits(flatBinLength);
        result = prime * result + (int) (temp ^ (temp >>> 32));
        result = prime * result + Arrays.hashCode(normalAngleData);
        return result;
    }

    @Override
    public boolean equals(Object obj) {
        if (this == obj) {
            return true;
        }
        if (!super.equals(obj)) {
            return false;
        }
        if (getClass() != obj.getClass()) {
            return false;
        }
        RadialBinMapProjection other = (RadialBinMapProjection) obj;
        if (Double.doubleToLongBits(flatBinLength) != Double
                .doubleToLongBits(other.flatBinLength)) {
            return false;
        }
        if (!Arrays.equals(normalAngleData, other.normalAngleData)) {
            return false;
        }
        return true;
    }

    public static class Provider extends AbstractProvider {

        private static final long serialVersionUID = 4990986103949071193L;

        public static final ParameterDescriptor<float[]> ANGLE_DATA = DefaultParameterDescriptor
                .create("angle_data", "The angle in degrees of each radial",
                        float[].class, null, true);

        public static final ParameterDescriptor<Double> TILT_ANGLE = DefaultParameterDescriptor
                .create("tilt_angle", "The angle in degrees above ground level",
                        Double.class, null, true);

        public static final ParameterDescriptor<Double> BIN_LENGTH = DefaultParameterDescriptor
                .create("bin_length", "The length of bins in meters",
                        Double.class, null, true);

        public static final ParameterDescriptorGroup PARAMETERS = new DefaultParameterDescriptorGroup(
                "Radial_Bin",
                new ParameterDescriptor[] { SEMI_MAJOR, SEMI_MINOR,
                        CENTRAL_MERIDIAN, LATITUDE_OF_ORIGIN, ANGLE_DATA,
                        TILT_ANGLE, BIN_LENGTH, SCALE_FACTOR, FALSE_EASTING,
                        FALSE_NORTHING });

        public Provider() {
            super(PARAMETERS);
        }

        @Override
        protected MathTransform createMathTransform(ParameterValueGroup values)
                throws InvalidParameterNameException,
                ParameterNotFoundException, InvalidParameterValueException,
                FactoryException {
            return new RadialBinMapProjection(values);
        }

        static <T> T getValue(ParameterDescriptor<T> descriptor,
                ParameterValueGroup group) {
            return MathTransformProvider.value(descriptor, group);
        }

    }

}
