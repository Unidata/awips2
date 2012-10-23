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
package com.raytheon.viz.grid.util;

import java.lang.ref.Reference;
import java.lang.ref.SoftReference;
import java.util.HashMap;
import java.util.Map;

import org.geotools.coverage.grid.GridEnvelope2D;
import org.geotools.coverage.grid.GridGeometry2D;
import org.geotools.referencing.GeodeticCalculator;
import org.opengis.referencing.operation.MathTransform;
import org.opengis.referencing.operation.TransformException;

import com.raytheon.uf.common.dataplugin.radar.RadarStation;
import com.raytheon.uf.common.datastorage.records.FloatDataRecord;
import com.raytheon.uf.common.geospatial.MapUtil;
import com.raytheon.uf.common.gridcoverage.GridCoverage;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.viz.radar.util.StationUtils;
import com.vividsolutions.jts.geom.Coordinate;

/**
 * Utility for generating grids of tilt heights for a Radar.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Dec 17, 2009            rjpeter     Initial creation
 * 
 * </pre>
 * 
 * @author rjpeter
 * @version 1.0
 */
public class TiltUtils {
    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(TiltUtils.class);

    private static class CacheKey {

        private final Integer coverageId;

        private final double lat;

        private final double lon;

        public CacheKey(Integer coverageId, double lon, double lat) {
            this.coverageId = coverageId;
            this.lat = lat;
            this.lon = lon;
        }

        @Override
        public int hashCode() {
            final int prime = 31;
            int result = 1;
            result = prime * result
                    + ((coverageId == null) ? 0 : coverageId.hashCode());
            long temp;
            temp = Double.doubleToLongBits(lat);
            result = prime * result + (int) (temp ^ (temp >>> 32));
            temp = Double.doubleToLongBits(lon);
            result = prime * result + (int) (temp ^ (temp >>> 32));
            return result;
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
            if (coverageId == null) {
                if (other.coverageId != null)
                    return false;
            } else if (!coverageId.equals(other.coverageId))
                return false;
            if (Double.doubleToLongBits(lat) != Double
                    .doubleToLongBits(other.lat))
                return false;
            if (Double.doubleToLongBits(lon) != Double
                    .doubleToLongBits(other.lon))
                return false;
            return true;
        }

    }

    private Map<CacheKey, Reference<double[]>> gridRadiusCache = new HashMap<CacheKey, Reference<double[]>>();

    private static TiltUtils instance;

    public static synchronized TiltUtils getInstance() {
        if (instance == null) {
            instance = new TiltUtils();
        }

        return instance;
    }

    private TiltUtils() {

    }

    /**
     * 
     * @param coverage
     * @param tilt
     *            in degrees
     */
    public FloatDataRecord getHeightGrid(GridCoverage coverage, double tilt) {

        RadarStation homeRadar = StationUtils.getInstance()
                .getHomeRadarStation();
        return getHeightGrid(homeRadar, coverage, tilt);
    }

    public FloatDataRecord getHeightGrid(Coordinate latLon,
            GridCoverage coverage, double tilt) {

        RadarStation radar = StationUtils.getInstance().getClosestRadarStation(
                latLon.x, latLon.y);
        return getHeightGrid(radar, coverage, tilt);
    }

    private FloatDataRecord getHeightGrid(RadarStation radar,
            GridCoverage coverage, double tilt) {
        if (radar != null) {
            CacheKey cacheKey = new CacheKey(coverage.getId(), radar.getLon(),
                    radar.getLat());
            GridGeometry2D geometry = MapUtil.getGridGeometry(coverage);
            GridEnvelope2D gridRange = geometry.getGridRange2D();
            double[] radius = null;
            Reference<double[]> radiusRef = gridRadiusCache.get(cacheKey);
            if (radiusRef != null) {
                radius = radiusRef.get();
            }
            if (radius == null) {
                try {
                    MathTransform gridToCrs = geometry.getGridToCRS();
                    MathTransform fromLatLon = MapUtil
                            .getTransformFromLatLon(coverage.getCrs());

                    double[] radarLonLat = new double[] { radar.getLon(),
                            radar.getLat() };
                    double[] radarCrsCoord = new double[2];
                    fromLatLon.transform(radarLonLat, 0, radarCrsCoord, 0, 1);
                    int numPoints = gridRange.height * gridRange.width;
                    double[] gridCoordGrid = new double[numPoints * 2];
                    int offset = 0;
                    for (int j = 0; j < gridRange.height; j++) {
                        for (int i = 0; i < gridRange.width; i++) {
                            gridCoordGrid[offset++] = i;
                            gridCoordGrid[offset++] = j;
                        }
                    }
                    gridToCrs.transform(gridCoordGrid, 0, gridCoordGrid, 0,
                            numPoints);
                    radius = new double[numPoints];
                    offset = 0;
                    for (int i = 0; i < numPoints; i++) {
                        double xDist = radarCrsCoord[0]
                                - gridCoordGrid[offset++];
                        double yDist = radarCrsCoord[1]
                                - gridCoordGrid[offset++];
                        radius[i] = Math.sqrt(xDist * xDist + yDist * yDist);
                    }
                    gridRadiusCache.put(cacheKey, new SoftReference<double[]>(
                            radius));
                } catch (Exception e) {
                    statusHandler
                            .handle(Priority.PROBLEM,
                                    "Error occurred generating height grid for radar tilt",
                                    e);
                    return null;
                }
            }
            return getHeightGrid(radar, gridRange, radius, tilt);
        } else {
            return null;
        }
    }

    public FloatDataRecord getHeightGrid(GridEnvelope2D gridRange,
            MathTransform gridToLatLon, double tilt) {
        RadarStation homeRadar = StationUtils.getInstance()
                .getHomeRadarStation();

        try {
            double[] radius = getRadius(homeRadar, gridRange, gridToLatLon);
            return getHeightGrid(homeRadar, gridRange, radius, tilt);
        } catch (TransformException e) {
            statusHandler.handle(Priority.PROBLEM,
                    "Error occurred generating height grid for radar tilt", e);
        }
        return null;
    }

    private FloatDataRecord getHeightGrid(RadarStation radar,
            GridEnvelope2D gridRange, double[] radius, double tilt) {
        float elevMeter = radar.getElevMeter();
        double tanTilt = Math.tan(Math.toRadians(tilt));
        float[] floatData = new float[radius.length];
        for (int i = 0; i < radius.length; i++) {
            floatData[i] = (float) (elevMeter + tanTilt * radius[i]);
        }
        FloatDataRecord fdr = new FloatDataRecord();
        fdr.setFloatData(floatData);
        fdr.setDimension(2);
        long[] sizes = { gridRange.width, gridRange.height };
        fdr.setSizes(sizes);
        return fdr;
    }

    private double[] getRadius(RadarStation radar, GridEnvelope2D gridRange,
            MathTransform gridToLatLon) throws TransformException {
        int numPoints = gridRange.height * gridRange.width;
        int offset = 0;
        double[] gridCoordGrid = new double[numPoints * 2];
        for (int j = 0; j < gridRange.height; j++) {
            for (int i = 0; i < gridRange.width; i++) {
                gridCoordGrid[offset++] = i;
                gridCoordGrid[offset++] = j;
            }
        }

        gridToLatLon.transform(gridCoordGrid, 0, gridCoordGrid, 0, numPoints);

        double[] radius = new double[numPoints];
        GeodeticCalculator gc = new GeodeticCalculator();
        gc.setStartingGeographicPoint(radar.getLon(), radar.getLat());
        offset = 0;

        for (int i = 0; i < numPoints; i++) {
            gc.setDestinationGeographicPoint(gridCoordGrid[offset],
                    gridCoordGrid[offset + 1]);
            offset += 2;
            radius[i] = gc.getOrthodromicDistance();
        }
        return radius;
    }
}
