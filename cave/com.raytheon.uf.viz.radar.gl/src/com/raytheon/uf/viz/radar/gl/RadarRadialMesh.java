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
package com.raytheon.uf.viz.radar.gl;

import java.util.Arrays;
import java.util.HashMap;
import java.util.Map;

import javax.media.opengl.GL;

import org.geotools.coverage.grid.GeneralGridGeometry;
import org.geotools.coverage.grid.GridGeometry2D;
import org.opengis.referencing.operation.MathTransform;
import org.opengis.referencing.operation.TransformException;

import com.raytheon.uf.common.dataplugin.radar.RadarRecord;
import com.raytheon.uf.common.dataplugin.radar.util.RadarUtil;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.viz.core.gl.AbstractGLMesh;
import com.raytheon.viz.core.gl.SharedCoordMap.SharedCoordinateKey;

/**
 * Mesh for radar data. Uses GL_TRIANGLE_STRIPS. density is every 2^n-th bin,
 * depending on the projection, and every single radial to get best resolution.
 * 
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY Date Ticket# Engineer Description ------------ ----------
 * ----------- -------------------------- Jun 10, 2010 mschenke Initial creation
 * 
 * </pre>
 * 
 * @author mschenke
 * @version 1.0
 */

public class RadarRadialMesh extends AbstractGLMesh {
    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(RadarRadialMesh.class);

    private static class CacheKey {
        private final float latitude;

        private final float longitude;

        private final int hashCode;

        private final int numBins;

        private final int numRadials;

        private final int gateResolution;

        private final float trueElevationAngle;

        private final int jStart;

        private final float[] angleData;

        private final GeneralGridGeometry gridGeometry;

        public CacheKey(float latitude, float longitude, int numBins,
                int numRadials, int gateResolution, float trueElevationAngle,
                int jStart, float[] angleData, GeneralGridGeometry gridGeometry) {
            this.latitude = latitude;
            this.longitude = longitude;
            this.numBins = numBins;
            this.numRadials = numRadials;
            this.gateResolution = gateResolution;
            this.trueElevationAngle = trueElevationAngle;
            this.jStart = jStart;
            this.angleData = angleData;
            this.gridGeometry = gridGeometry;
            final int prime = 31;
            int hashCode = 1;
            hashCode = prime * hashCode + Arrays.hashCode(angleData);
            hashCode = prime * hashCode + gateResolution;
            hashCode = prime * hashCode + jStart;
            hashCode = prime * hashCode + Float.floatToIntBits(latitude);
            hashCode = prime * hashCode + Float.floatToIntBits(longitude);
            hashCode = prime * hashCode + numBins;
            hashCode = prime * hashCode + numRadials;
            hashCode = prime * hashCode
                    + Float.floatToIntBits(trueElevationAngle);
            hashCode = prime * hashCode + gridGeometry.hashCode();
            this.hashCode = hashCode;
        }

        @Override
        public int hashCode() {
            return hashCode;
        }

        @Override
        public boolean equals(Object obj) {
            if (this == obj)
                return true;
            if (obj == null)
                return false;
            if (getClass() != obj.getClass())
                return false;
            CacheKey other = (CacheKey) obj;
            if (hashCode != other.hashCode)
                return false;
            if (gateResolution != other.gateResolution)
                return false;
            if (jStart != other.jStart)
                return false;
            if (latitude != other.latitude)
                return false;
            if (longitude != other.longitude)
                return false;
            if (numBins != other.numBins)
                return false;
            if (numRadials != other.numRadials)
                return false;
            if (Float.floatToIntBits(trueElevationAngle) != Float
                    .floatToIntBits(other.trueElevationAngle))
                return false;
            if (!Arrays.equals(angleData, other.angleData))
                return false;
            if (gridGeometry != null && other.gridGeometry == null) {
                return false;
            }
            if (gridGeometry == null && other.gridGeometry != null) {
                return false;
            }
            if (gridGeometry != null
                    && !gridGeometry.equals(other.gridGeometry)) {
                return false;
            }
            return true;
        }
    }

    private static Map<CacheKey, RadarRadialMesh> cache = new HashMap<CacheKey, RadarRadialMesh>();

    /** The record to build the mesh for */
    private RadarRecord record;

    private CacheKey cacheKey;

    private int refCount;

    public RadarRadialMesh(RadarRecord record,
            GeneralGridGeometry targetGeometry, CacheKey cacheKey)
            throws VizException {
        super(GL.GL_TRIANGLE_STRIP);
        this.record = record;
        this.cacheKey = cacheKey;
        initialize(
                RadarUtil.constructGridGeometry(record.getCRS(),
                        RadarUtil.calculateExtent(record),
                        Math.max(record.getNumBins(), record.getNumRadials())),
                targetGeometry);
        refCount = 0;
    }

    @Override
    protected double[][][] generateWorldCoords(GridGeometry2D imageGeometry,
            MathTransform mt) throws TransformException {

        int horizontalDivisions = key.horizontalDivisions;
        int verticalDivisions = key.verticalDivisions + 1;

        // get dx and dy for texture points
        float dX = (1.0f / (key.horizontalDivisions));

        // set up our angle data for the radials
        float[] angles = record.getAngleData();
        int height = 2 * verticalDivisions;
        int width = horizontalDivisions;

        float totalDiff = angles[angles.length - 1] - angles[0];
        if (totalDiff < 0) {
            totalDiff += 360;
        }

        float average = totalDiff / (angles.length - 1);

        // determine if average dist is close enough to dist at [0].
        float actualDiff = 360 - totalDiff;

        // if the difference between actual and calculated is less than
        // half a degree, we can assume the radial data does in fact
        // wrap all the way in a circle and we can use the starting
        // angle as our ending angle. Otherwise, we must assume the data
        // is a slice of a circle and we should use our calculated end
        // location
        float lastAngle = angles[angles.length - 1] + average;
        if (Math.abs(actualDiff - average) < 0.5) {
            lastAngle = angles[0] + 360.0f;
        }

        // Precalculate angles used in mesh
        double[] cosAngles = new double[verticalDivisions];
        double[] sinAngles = new double[verticalDivisions];
        for (int j = 0; j < angles.length; j++) {
            cosAngles[j] = Math.cos(Math.toRadians(angles[j]));
            sinAngles[j] = Math.sin(Math.toRadians(angles[j]));
        }

        cosAngles[cosAngles.length - 1] = Math.cos(Math.toRadians(lastAngle));
        sinAngles[sinAngles.length - 1] = Math.sin(Math.toRadians(lastAngle));

        // precalculated bin value
        double calculatedVal = record.getNumBins() * record.getGateResolution()
                * Math.cos(Math.toRadians(record.getTrueElevationAngle()));

        double startRange = 0;
        if (record.getJstart() != null) {
            startRange = record.getJstart() * record.getGateResolution()
                    * Math.cos(Math.toRadians(record.getTrueElevationAngle()));
        }

        double rangeHigh = startRange, rangeLow;

        double[] in = new double[3];
        double[] out = new double[3];

        double[][][] worldCoordinates = new double[width][height][2];

        for (int i = 0; i < width; ++i) {
            rangeLow = rangeHigh;
            rangeHigh = startRange + (i + 1) * dX * calculatedVal;

            for (int j = 0; j < height;) {
                int idx = (j / 2);
                double sin = sinAngles[idx];
                double cos = cosAngles[idx];

                double range = rangeLow;
                in[0] = range * sin;
                in[1] = range * cos;

                mt.transform(in, 0, out, 0, 1);

                worldCoordinates[i][j][0] = out[0];
                worldCoordinates[i][j++][1] = out[1];

                range = rangeHigh;
                in[0] = range * sin;
                in[1] = range * cos;

                mt.transform(in, 0, out, 0, 1);

                worldCoordinates[i][j][0] = out[0];
                worldCoordinates[i][j++][1] = out[1];
            }
        }
        return worldCoordinates;
    }

    @Override
    protected SharedCoordinateKey generateKey(GridGeometry2D imageGeometry,
            MathTransform mt) {
        try {
            return new SharedCoordinateKey(record.getNumRadials(),
                    getNumVerticalDivisions(mt, record));
        } catch (Exception e) {
            statusHandler
                    .handle(Priority.PROBLEM,
                            "Error calculating divisions needed for radar, defaulting to 10",
                            e);
            return new SharedCoordinateKey(record.getNumRadials(), 10);
        }
    }

    private static final double THRESHOLD = 0.1;

    private int getNumVerticalDivisions(MathTransform toLatLon,
            RadarRecord record) throws Exception {
        float[] angles = record.getAngleData();
        int numBins = record.getNumBins();
        int startBin = 0;
        if (record.getJstart() != null) {
            startBin = record.getJstart();
        }

        double calculatedVal = record.getGateResolution()
                * Math.cos(Math.toRadians(record.getTrueElevationAngle()));

        double[] in = new double[2];
        double[] out = new double[3];

        int pow2 = (int) Math.floor(Math.log(numBins / 10) / Math.log(2));

        in[0] = 0;
        in[1] = 0;
        toLatLon.transform(in, 0, out, 0, 1);

        double[] start = worldToPixel(out);

        for (int i = 0; i < angles.length; ++i) {
            // grab end
            float az = (float) Math.toRadians(angles[i]);
            double sinAz = Math.sin(az);
            double cosAz = Math.cos(az);

            double range = (startBin + numBins) * calculatedVal;

            in[0] = range * sinAz;
            in[1] = range * cosAz;
            toLatLon.transform(in, 0, out, 0, 1);
            out = worldToPixel(out);

            int[] curPow2 = new int[] { (int) Math.floor(Math.log(numBins)
                    / Math.log(2)) };

            getDivisionsNeeded(toLatLon, 0, range, start, out, sinAz, cosAz,
                    curPow2, pow2);

            if (curPow2[0] > pow2) {
                pow2 = curPow2[0];
            }
        }

        return numBins / (int) Math.pow(2, pow2);
    }

    /**
     * Recursive function to figure out the number of vertical divisions needed
     * 
     * @param toLatLon
     * @param startRange
     * @param endRange
     * @param startLoc
     * @param endLoc
     * @param sinAz
     * @param cosAz
     * @param curPow2
     */
    private void getDivisionsNeeded(MathTransform toLatLon, double startRange,
            double endRange, double[] startLoc, double[] endLoc, double sinAz,
            double cosAz, int[] curPow2, int minPow2) throws Exception {
        if (curPow2[0] == minPow2) {
            return;
        }

        // Get actual center point:
        double rangeToTry = (startRange + endRange) / 2;

        double[] in = new double[] { rangeToTry * sinAz, rangeToTry * cosAz };
        double[] actual = new double[3];
        toLatLon.transform(in, 0, actual, 0, 1);
        actual = worldToPixel(actual);

        // Get linear interpolated point
        double[] interp = new double[] { (endLoc[0] + startLoc[0]) / 2,
                (endLoc[1] + startLoc[1]) / 2 };

        double dist = Math.sqrt(Math.pow(Math.abs(actual[0] - interp[0]), 2)
                + Math.pow(Math.abs(actual[1] - interp[1]), 2));

        if (dist > THRESHOLD) {
            // decrement division and try the left side, try the right side
            curPow2[0]--;

            getDivisionsNeeded(toLatLon, startRange, rangeToTry, startLoc,
                    actual, sinAz, cosAz, curPow2, minPow2);
            getDivisionsNeeded(toLatLon, rangeToTry, endRange, actual, endLoc,
                    sinAz, cosAz, curPow2, minPow2);
        }
    }

    private void use() {
        refCount += 1;
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.viz.core.gl.AbstractGLMesh#dispose()
     */
    @Override
    public synchronized void dispose() {
        refCount -= 1;
        synchronized (cache) {
            if (refCount == 0) {
                super.dispose();
                cache.remove(cacheKey);
            }
        }
    }

    public static RadarRadialMesh getMesh(RadarRecord radarData,
            GeneralGridGeometry targetGeometry) throws VizException {
        float latitude = radarData.getLatitude();
        float longitude = radarData.getLongitude();
        int numBins = radarData.getNumBins();
        int numRadials = radarData.getNumRadials();
        int gateResolution = radarData.getGateResolution();
        float trueElevationAngle = radarData.getTrueElevationAngle();
        Integer jStart = radarData.getJstart();
        if (jStart == null) {
            jStart = 0;
        }
        float[] angleData = radarData.getAngleData();
        CacheKey key = new CacheKey(latitude, longitude, numBins, numRadials,
                gateResolution, trueElevationAngle, jStart, angleData,
                targetGeometry);
        synchronized (cache) {
            RadarRadialMesh mesh = cache.get(key);
            if (mesh == null) {
                // System.out.println("Mesh Cache miss");
                mesh = new RadarRadialMesh(radarData, targetGeometry, key);
                cache.put(key, mesh);
            } else {
                // System.out.println("Mesh Cache hit");
            }
            mesh.use();
            return mesh;
        }
    }
}
