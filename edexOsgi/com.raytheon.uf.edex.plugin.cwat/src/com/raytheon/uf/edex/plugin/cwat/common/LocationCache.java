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
package com.raytheon.uf.edex.plugin.cwat.common;

import java.io.File;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.apache.commons.collections.map.LRUMap;
import org.geotools.coverage.grid.GridGeometry2D;
import org.geotools.referencing.GeodeticCalculator;
import org.opengis.referencing.datum.PixelInCell;
import org.opengis.referencing.operation.MathTransform;

import com.raytheon.uf.common.dataplugin.radar.RadarRecord;
import com.raytheon.uf.common.geospatial.TransformFactory;
import com.raytheon.uf.common.localization.IPathManager;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationLevel;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationType;
import com.raytheon.uf.common.localization.LocalizationFile;
import com.raytheon.uf.common.localization.PathManagerFactory;
import com.raytheon.uf.common.monitor.scan.ScanUtils;
import com.raytheon.uf.common.monitor.scan.VILReport;
import com.raytheon.uf.common.serialization.SerializationUtil;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.common.util.FileUtil;
import com.raytheon.uf.edex.database.cluster.ClusterLockUtils;
import com.raytheon.uf.edex.database.cluster.ClusterLockUtils.LockState;
import com.raytheon.uf.edex.database.cluster.ClusterTask;
import com.vividsolutions.jts.geom.Coordinate;

/**
 * TODO Add Description
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Nov 17, 2010            rjpeter     Initial creation
 * Aug 26, 2014 3503       bclement    removed warning
 * Jan 27, 2016 5237       tgurney     Remove deprecated LocalizationFile
 *                                     method call
 * 
 * </pre>
 * 
 * @author rjpeter
 * @version 1.0
 */
public class LocationCache {
    private static final String TASK_NAME = "CWAT Location Gen";

    private static LocationCache instance = new LocationCache();

    // keep last 10 radars in memory
    // String site, to value of LocationBinList
    private LRUMap cachedSites = new LRUMap(10);

    private final IUFStatusHandler handler = UFStatus
            .getHandler(LocationCache.class);

    public static LocationCache getInstance() {
        return instance;
    }

    private LocationCache() {

    }

    public Coordinate[] getVilLatLons(VILReport report, RadarRecord vilRec,
            double distance) {
        String site = report.getSiteName();
        String cell = report.getCellName();

        LocationBinList cachedLocations = (LocationBinList) cachedSites
                .get(site);

        if (cachedLocations == null
                || !cachedLocations.getSiteCoord().equals(report.getSiteCoor())) {
            // attempt to read from file
            cachedLocations = getLocationsFromFile(site, cachedLocations, false);

            if (cachedLocations == null
                    || !cachedLocations.getSiteCoord().equals(
                            report.getSiteCoor())) {
                // file did not exist or radar changed sites
                cachedLocations = new LocationBinList();
                cachedLocations.setSiteCoord(report.getSiteCoor());
                cachedLocations
                        .setLocationBins(new HashMap<String, LocationBin>(250));
                cachedLocations.setLastModifiyTime(System.currentTimeMillis());
            }

            cachedSites.put(site, cachedLocations);
        }

        Map<String, LocationBin> cachedCellLocations = cachedLocations
                .getLocationBins();
        String cellKey = cell + "_" + distance;
        LocationBin location = cachedCellLocations.get(cellKey);

        if (location == null
                || !report.getCellCoor().equals(location.getCellCoord())) {
            ClusterTask ct = null;

            // need exclusive lock for generating location
            do {
                ct = ClusterLockUtils.lock(TASK_NAME, site, 60000, true);
            } while (!ct.getLockState().equals(LockState.SUCCESSFUL));

            try {
                // verify location file has not been updated by another process
                LocationBinList tmp = getLocationsFromFile(site,
                        cachedLocations, true);
                if (tmp != null) {
                    cachedLocations = tmp;
                    cachedSites.put(site, cachedLocations);
                    cachedCellLocations = cachedLocations.getLocationBins();
                    location = cachedCellLocations.get(cellKey);

                    if (location != null
                            && report.getCellCoor().equals(
                                    location.getCellCoord())) {
                        return location.getLatLons();
                    }
                }

                if (location == null) {
                    location = new LocationBin();
                    location.setCellCoord(report.getCellCoor());
                    cachedCellLocations.put(cellKey, location);
                }

                Coordinate[] points = generateLocationBin(report, vilRec,
                        distance);
                location.setLatLons(points);
                saveLocationToFile(site, cachedLocations);
            } catch (Exception e) {
                handler.handle(
                        Priority.ERROR,
                        "Error generating cwat location [" + site + ", " + cell
                                + ", " + distance + "], Error: "
                                + e.getMessage());
                return new Coordinate[0];
            } finally {
                ClusterLockUtils.unlock(ct, false);
            }
        }

        return location.getLatLons();
    }

    private LocationBinList getLocationsFromFile(String site,
            LocationBinList currentLocationList, boolean haveLock) {
        IPathManager pm = PathManagerFactory.getPathManager();
        LocalizationFile localizationFile = pm.getLocalizationFile(pm
                .getContext(LocalizationType.EDEX_STATIC,
                        LocalizationLevel.SITE), "/cwat/locations/" + site
                + ".bin");
        File file = localizationFile.getFile();
        LocationBinList rval = null;

        if (file.exists()
                && (currentLocationList == null || currentLocationList
                        .getLastModifiyTime() < file.lastModified())) {
            // technically should be a shared lock for maximum concurrency
            ClusterTask ct = null;
            if (!haveLock) {
                do {
                    ct = ClusterLockUtils.lock(TASK_NAME, site, 60000, true);
                } while (!ct.getLockState().equals(LockState.SUCCESSFUL));
            }

            try {
                long start = System.currentTimeMillis();
                rval = SerializationUtil.transformFromThrift(
                        LocationBinList.class, FileUtil.file2bytes(file));
                long end = System.currentTimeMillis();
                rval.setLastModifiyTime(file.lastModified());
                handler.handle(Priority.INFO, "Read CWAT location file ["
                        + file.getAbsolutePath() + "] in " + (end - start)
                        + " ms.");
            } catch (Exception e) {
                // error'd deserializing file, delete file and regen it
                handler.handle(
                        Priority.ERROR,
                        "Error processing cwat location file ["
                                + file.getAbsolutePath() + "], Error: "
                                + e.getMessage()
                                + ".  Attempting to delete file.", e);
                try {
                    file.delete();
                } catch (Exception e2) {
                    handler.handle(
                            Priority.WARN,
                            "Error deleting cwat location file ["
                                    + localizationFile.getPath() + "], "
                                    + e2.getMessage());
                }
            } finally {
                if (ct != null) {
                    ClusterLockUtils.unlock(ct, false);
                }
            }
        }
        return rval;
    }

    private void saveLocationToFile(String site, LocationBinList location) {
        IPathManager pm = PathManagerFactory.getPathManager();
        LocalizationFile locationFile = pm.getLocalizationFile(pm.getContext(
                LocalizationType.EDEX_STATIC, LocalizationLevel.SITE),
                "/cwat/locations/" + site + ".bin");
        try {
            File file = locationFile.getFile();
            FileUtil.bytes2File(SerializationUtil.transformToThrift(location),
                    file);
            locationFile.save();
            location.setLastModifiyTime(file.lastModified());
        } catch (Exception e) {
            handler.handle(Priority.ERROR, "Error saving cwat location file ["
                    + locationFile.getPath() + "], Error: " + e.getMessage());
        }
    }

    private Coordinate[] generateLocationBin(VILReport report,
            RadarRecord vilRec, double distance) throws Exception {
        handler.handle(Priority.INFO, "Generating CWAT location for site ["
                + report.getSiteName() + "] cell [" + report.getCellName()
                + "] dist [" + distance + "]");
        List<Coordinate> latLonList = new ArrayList<Coordinate>(
                ScanUtils.SCAN_GRID_DIM * ScanUtils.SCAN_GRID_DIM / 2);
        GridGeometry2D geometry = ScanUtils.getStationGeometry(
                report.getSiteCoor(), ScanUtils.SCAN_GRID_DIM_RESOLUTION,
                ScanUtils.SCAN_GRID_DIM);
        GeodeticCalculator gc = new GeodeticCalculator(vilRec.getCRS());
        double compareDistance = distance / ScanUtils.meterToNM;

        // go over the x, y scan grid finding lightning within
        // 10 and 30 nm of each point in grid
        // determine coordinate in X,Y relative to the Radar
        MathTransform mt = TransformFactory.gridToLatLon(geometry,
                PixelInCell.CELL_CENTER);
        int points = ScanUtils.SCAN_GRID_DIM * ScanUtils.SCAN_GRID_DIM;
        double[] nativePoints = new double[points * 2];
        int index = 0;

        for (int i = 0; i < ScanUtils.SCAN_GRID_DIM; i++) {
            for (int j = 0; j < ScanUtils.SCAN_GRID_DIM; j++) {
                nativePoints[index++] = i;
                nativePoints[index++] = j;
            }
        }
        double[] latLons = new double[nativePoints.length];
        mt.transform(nativePoints, 0, latLons, 0, points);

        for (int i = 0; i < points; i++) {
            index = i * 2;
            Coordinate compareCoor = new Coordinate(latLons[index],
                    latLons[index + 1]);
            double myDistance = ScanUtils.getDistance(compareCoor,
                    report.getCellCoor(), gc);

            if (myDistance <= compareDistance) {
                latLonList.add(compareCoor);
            }
        }

        return latLonList.toArray(new Coordinate[latLonList.size()]);
    }
}
