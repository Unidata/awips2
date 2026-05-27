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
package com.raytheon.uf.viz.grid.radar;

import java.lang.ref.Reference;
import java.lang.ref.SoftReference;
import java.util.HashMap;
import java.util.Map;

import org.geotools.coverage.grid.GridEnvelope2D;
import org.geotools.coverage.grid.GridGeometry2D;
import org.geotools.referencing.GeodeticCalculator;
import org.locationtech.jts.geom.Coordinate;
import org.geotools.api.referencing.operation.MathTransform;
import org.geotools.api.referencing.operation.TransformException;

import com.raytheon.uf.common.dataplugin.radar.RadarStation;
import com.raytheon.uf.common.datastorage.records.FloatDataRecord;
import com.raytheon.uf.common.geospatial.MapUtil;
import com.raytheon.uf.common.gridcoverage.GridCoverage;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.common.wxmath.Hgt2Pres;
import com.raytheon.viz.radar.util.StationUtils;

/**
 * Utility for generating grids of tilt heights for a Radar.
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date          Ticket#  Engineer  Description
 * ------------- -------- --------- ------------------------------
 * Dec 17, 2009           rjpeter   Initial creation
 * Aug 15, 2017  6332     bsteffen  Move to viz.grid.radar plugin
 * Feb 03, 2020 DR21007   kshrestha Resolve Incorrect height readings when sampling radar data.
 * Jul 17, 2020 17574     smoorthy  Account for curvature of earth in height equation
 * Jul 14, 2021  8576     randerso  Changed RadarAdapter to support multiple
 *                                  local radars as defined in radarsInUse.txt
 * Sep 07, 2021  8652     njensen   Change cache to cache height grid instead of radius
 *                                  Added hgt2pres methods and cache
 *
 * </pre>
 *
 * @author rjpeter
 */
public class TiltUtils {
    private static final IUFStatusHandler statusHandler = UFStatus
            .getHandler(TiltUtils.class);

    private static final double EFFECTIVE_EARTH_RADIUS = 1.21 * 6_371_000; // meters

    private static class CacheKey {

        private final Integer coverageId;

        private final double lat;

        private final double lon;

        private final double tilt;

        public CacheKey(Integer coverageId, double lon, double lat,
                double tilt) {
            this.coverageId = coverageId;
            this.lat = lat;
            this.lon = lon;
            this.tilt = tilt;
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
            temp = Double.doubleToLongBits(tilt);
            result = prime * result + (int) (temp ^ (temp >>> 32));
            return result;
        }

        @Override
        public boolean equals(Object obj) {
            if (this == obj) {
                return true;
            }
            if (obj == null) {
                return false;
            }
            if (getClass() != obj.getClass()) {
                return false;
            }
            CacheKey other = (CacheKey) obj;
            if (coverageId == null) {
                if (other.coverageId != null) {
                    return false;
                }
            } else if (!coverageId.equals(other.coverageId)) {
                return false;
            }
            if (Double.doubleToLongBits(lat) != Double
                    .doubleToLongBits(other.lat)) {
                return false;
            }
            if (Double.doubleToLongBits(lon) != Double
                    .doubleToLongBits(other.lon)) {
                return false;
            }
            if (Double.doubleToLongBits(tilt) != Double
                    .doubleToLongBits(other.tilt)) {
                return false;
            }
            return true;
        }

    }

    private Map<CacheKey, Reference<FloatDataRecord>> gridHeightCache = new HashMap<>();

    private Map<CacheKey, Reference<FloatDataRecord>> gridHgt2PresCache = new HashMap<>();

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
     * @param icao
     *            ICAO of desired radar station. If null radar nearest home
     *            cursor location will be used.
     * @param coverage
     * @param tilt
     *            in degrees
     */
    public FloatDataRecord getHeightGrid(String icao, GridCoverage coverage,
            double tilt) {
        RadarStation radarStation;
        if (icao == null) {
            radarStation = StationUtils.getInstance().getHomeRadarStation();
        } else {
            radarStation = StationUtils.getInstance().getRadarStation(icao);
        }
        return getHeightGrid(radarStation, coverage, tilt);
    }

    public FloatDataRecord getHeightGrid(Coordinate latLon,
            GridCoverage coverage, double tilt) {

        RadarStation radar = StationUtils.getInstance()
                .getClosestRadarStation(latLon.x, latLon.y);
        return getHeightGrid(radar, coverage, tilt);
    }

    private FloatDataRecord getHeightGrid(RadarStation radar,
            GridCoverage coverage, double tilt) {
        if (radar != null) {
            CacheKey cacheKey = new CacheKey(coverage.getId(), radar.getLon(),
                    radar.getLat(), tilt);
            GridGeometry2D geometry = MapUtil.getGridGeometry(coverage);
            GridEnvelope2D gridRange = geometry.getGridRange2D();
            FloatDataRecord height = null;
            Reference<FloatDataRecord> heightRef = gridHeightCache
                    .get(cacheKey);
            if (heightRef != null) {
                height = heightRef.get();
            }
            if (height == null) {
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
                            gridCoordGrid[offset] = i;
                            offset += 1;
                            gridCoordGrid[offset] = j;
                            offset += 1;
                        }
                    }
                    gridToCrs.transform(gridCoordGrid, 0, gridCoordGrid, 0,
                            numPoints);
                    double[] radius = new double[numPoints];
                    offset = 0;
                    for (int i = 0; i < numPoints; i++) {
                        double xDist = radarCrsCoord[0] - gridCoordGrid[offset];
                        offset += 1;
                        double yDist = radarCrsCoord[1] - gridCoordGrid[offset];
                        offset += 1;
                        radius[i] = Math.sqrt(xDist * xDist + yDist * yDist);
                    }
                    height = getHeightGrid(radar, gridRange, radius, tilt);
                    gridHeightCache.put(cacheKey, new SoftReference<>(height));
                } catch (Exception e) {
                    statusHandler.handle(Priority.PROBLEM,
                            "Error occurred generating height grid for radar tilt",
                            e);
                    return null;
                }
            }
            return height;
        } else {
            return null;
        }
    }

    public FloatDataRecord getHeightGrid(String icao, GridEnvelope2D gridRange,
            MathTransform gridToLatLon, double tilt) {
        RadarStation radarStation;
        if (icao == null) {
            radarStation = StationUtils.getInstance().getHomeRadarStation();
        } else {
            radarStation = StationUtils.getInstance().getRadarStation(icao);
        }

        try {
            double[] radius = getRadius(radarStation, gridRange, gridToLatLon);
            return getHeightGrid(radarStation, gridRange, radius, tilt);
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

            double range = Math.sqrt(
                    Math.pow(radius[i], 2) + Math.pow(tanTilt * radius[i], 2));
            floatData[i] = (float) (elevMeter + tanTilt * radius[i]
                    + (range * range) / (2 * EFFECTIVE_EARTH_RADIUS));
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
                gridCoordGrid[offset] = i;
                offset += 1;
                gridCoordGrid[offset] = j;
                offset += 1;
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

    public FloatDataRecord getHgt2PresGrid(String icao, GridCoverage coverage,
            double tilt) {
        if (icao == null) {
            throw new IllegalArgumentException(
                    "icao cannot be null for calculating pressure at tilt heights");
        }
        RadarStation radarStation = StationUtils.getInstance()
                .getRadarStation(icao);
        return getHgt2PresGrid(radarStation, coverage, tilt);
    }

    public FloatDataRecord getHgt2PresGrid(RadarStation radar,
            GridCoverage coverage, double tilt) {
        CacheKey cacheKey = new CacheKey(coverage.getId(), radar.getLon(),
                radar.getLat(), tilt);
        FloatDataRecord retVal = null;
        Reference<FloatDataRecord> presRef = gridHgt2PresCache.get(cacheKey);
        if (presRef != null) {
            retVal = presRef.get();
        }

        if (retVal == null) {
            FloatDataRecord heightFdr = getHeightGrid(radar, coverage, tilt);
            float[] height = heightFdr.getFloatData();
            float[] pres = new float[height.length];
            for (int i = 0; i < height.length; i++) {
                pres[i] = Hgt2Pres.hgt2pres(height[i]);
            }
            retVal = new FloatDataRecord();
            retVal.setFloatData(pres);
            retVal.setDimension(heightFdr.getDimension());
            retVal.setSizes(heightFdr.getSizes());
            gridHgt2PresCache.put(cacheKey, new SoftReference<>(retVal));
        }

        return retVal;
    }
}
