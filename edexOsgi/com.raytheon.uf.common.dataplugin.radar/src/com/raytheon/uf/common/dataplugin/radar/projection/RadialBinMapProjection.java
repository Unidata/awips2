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
import java.util.List;

import org.geotools.parameter.DefaultParameterDescriptor;
import org.geotools.parameter.DefaultParameterDescriptorGroup;
import org.geotools.referencing.operation.MathTransformProvider;
import org.geotools.referencing.operation.projection.ProjectionException;
import org.geotools.referencing.operation.transform.AbstractMathTransform;
import org.geotools.api.parameter.InvalidParameterNameException;
import org.geotools.api.parameter.InvalidParameterValueException;
import org.geotools.api.parameter.ParameterDescriptor;
import org.geotools.api.parameter.ParameterDescriptorGroup;
import org.geotools.api.parameter.ParameterNotFoundException;
import org.geotools.api.parameter.ParameterValueGroup;
import org.geotools.api.referencing.FactoryException;
import org.geotools.api.referencing.operation.MathTransform;
import org.geotools.api.referencing.operation.TransformException;

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
 * Mar 14, 2023  9029     mapeters  Support splitting this projection into 2 math
 *                                  transforms for performance (the results of the
 *                                  first can often be cached)
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
        return transformAzRanToRadialBin(az, ran, dest);
    }

    Point2D transformAzRanToRadialBin(double az, double ran, Point2D dest) {
        if (dest == null) {
            dest = new Point2D.Double();
        }

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

    private Point2D transformLonLatRadiansToAzRan(double lonRadians,
            double latRadians, Point2D dest) throws ProjectionException {
        return super.transformNormalized(lonRadians, latRadians, dest);
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

    protected int superHashCode() {
        return super.hashCode();
    }

    protected boolean superEquals(Object obj) {
        return super.equals(obj);
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
                "Radial_Bin", SEMI_MAJOR, SEMI_MINOR, CENTRAL_MERIDIAN,
                LATITUDE_OF_ORIGIN, ANGLE_DATA, TILT_ANGLE, BIN_LENGTH,
                SCALE_FACTOR, FALSE_EASTING, FALSE_NORTHING);

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

    /**
     * Split this transform into a list of 2 transforms: one that just does the
     * superclass transformation (lon/lat -> azimuth/range), and a second that
     * does the transformation in this class (azimuth/range -> radial/bin).
     *
     * This is useful for caching transformation results for performance, as the
     * lon/lat -> azimuth/range transformation is shared among different radar
     * records and can be cached. The azimuth/range->radial/bin transformation
     * is typically not shared and can't be cached effectively, due to each
     * radar record having slightly different angle data.
     *
     * The split transforms are not invertible.
     *
     * @return a list of the two split transforms that make up this transform.
     */
    public List<MathTransform> splitTransform() {
        return List.of(new LonLatToAzRanTransform(this),
                new AzRanToRadialBinTransform(this));
    }

    /**
     * Abstract math transform class for a portion of the transformation done by
     * a RadialBinMapProjection.
     */
    public static abstract class AbstractRadialBinMapProjectionSplitTransform
            extends AbstractMathTransform {

        protected final RadialBinMapProjection proj;

        protected AbstractRadialBinMapProjectionSplitTransform(
                RadialBinMapProjection proj) {
            this.proj = proj;
        }

        @Override
        public abstract Point2D transform(Point2D srcPt, Point2D destPt)
                throws ProjectionException;

        @Override
        public void transform(final double[] srcPts, int srcOff,
                final double[] dstPts, int dstOff, int numPts)
                throws TransformException {
            /*
             * This method is copied from the corresponding MapProjection method
             */
            final boolean reverse = (srcPts == dstPts && srcOff < dstOff
                    && srcOff + (2 * numPts) > dstOff);
            if (reverse) {
                srcOff += 2 * numPts;
                dstOff += 2 * numPts;
            }
            final Point2D.Double point = new Point2D.Double();
            ProjectionException firstException = null;
            while (--numPts >= 0) {
                try {
                    point.x = srcPts[srcOff++];
                    point.y = srcPts[srcOff++];
                    transform(point, point);
                    dstPts[dstOff++] = point.x;
                    dstPts[dstOff++] = point.y;
                } catch (ProjectionException exception) {
                    dstPts[dstOff++] = Double.NaN;
                    dstPts[dstOff++] = Double.NaN;
                    if (firstException == null) {
                        firstException = exception;
                    }
                }
                if (reverse) {
                    srcOff -= 4;
                    dstOff -= 4;
                }
            }
            if (firstException != null) {
                throw firstException;
            }
        }

        @Override
        public int getSourceDimensions() {
            return 2;
        }

        @Override
        public int getTargetDimensions() {
            return 2;
        }
    }

    /**
     * Math transform class for the first half of a RadialBinMapProjection's
     * transformation: the lon/lat->azimuth/range transformation done within the
     * superclasses. This transform can often be shared by different radar
     * records.
     */
    public static class LonLatToAzRanTransform
            extends AbstractRadialBinMapProjectionSplitTransform {

        protected LonLatToAzRanTransform(RadialBinMapProjection proj) {
            super(proj);
        }

        @Override
        public Point2D transform(Point2D ptSrc, Point2D ptDst)
                throws ProjectionException {
            double lon = ptSrc.getX();
            double lat = ptSrc.getY();
            /*
             * This conversion is copied from the corresponding MapProjection
             * method
             */
            lon = proj.centralMeridian != 0
                    ? rollLongitude(Math.toRadians(lon) - proj.centralMeridian)
                    : Math.toRadians(lon);
            lat = Math.toRadians(lat);

            ptDst = proj.transformLonLatRadiansToAzRan(lon, lat, ptDst);
            return ptDst;
        }

        @Override
        public int hashCode() {
            final int prime = 31;
            int result = super.hashCode();
            // Only hash things in RadialBinMapProjection's parent classes
            result = prime * result + proj.superHashCode();
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
            LonLatToAzRanTransform other = (LonLatToAzRanTransform) obj;
            // Only compare things in RadialBinMapProjection's parent classes
            return proj.superEquals(other.proj);
        }
    }

    /**
     * Math transform class for the second half of a RadialBinMapProjection's
     * transformation: the azimuth/range->radial/bin transformation done within
     * the RadialBinMapProjection class itself. This transform is usually
     * specific to a particular radar record due to records having data at
     * slightly different angles.
     */
    public class AzRanToRadialBinTransform
            extends AbstractRadialBinMapProjectionSplitTransform {

        protected AzRanToRadialBinTransform(RadialBinMapProjection proj) {
            super(proj);
        }

        @Override
        public Point2D transform(Point2D ptSrc, Point2D ptDst) {
            double az = ptSrc.getX();
            double ran = ptSrc.getY();
            ptDst = proj.transformAzRanToRadialBin(az, ran, ptDst);

            /*
             * This conversion is copied from the corresponding MapProjection
             * method
             */
            ptDst.setLocation(
                    proj.globalScale * ptDst.getX() + proj.falseEasting,
                    proj.globalScale * ptDst.getY() + proj.falseNorthing);
            return ptDst;
        }

        @Override
        public int hashCode() {
            final int prime = 31;
            int result = super.hashCode();
            result = prime * result + proj.hashCode();
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
            AzRanToRadialBinTransform other = (AzRanToRadialBinTransform) obj;
            return proj.equals(other.proj);
        }
    }
}
