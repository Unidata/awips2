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
package com.raytheon.uf.viz.volumebrowser.dataplugin.point;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.Comparator;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.concurrent.ArrayBlockingQueue;
import java.util.concurrent.BlockingQueue;

import com.raytheon.uf.common.dataplugin.level.Level;
import com.raytheon.uf.common.dataplugin.level.mapping.LevelMappingFactory;
import com.raytheon.uf.common.dataquery.requests.RequestConstraint;
import com.raytheon.uf.common.dataquery.requests.RequestConstraint.ConstraintType;
import com.raytheon.uf.common.pointdata.spatial.SurfaceObsLocation;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.viz.core.catalog.DbQuery;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.points.PointsDataManager;
import com.raytheon.uf.viz.volumebrowser.dataplugin.AbstractInventoryDataCatalog;
import com.raytheon.viz.awipstools.ToolsDataManager;
import com.raytheon.viz.pointdata.util.AbstractPointDataInventory;
import com.raytheon.viz.volumebrowser.datacatalog.AvailableDataRequest;
import com.raytheon.viz.volumebrowser.datacatalog.DataCatalogEntry;
import com.raytheon.viz.volumebrowser.datacatalog.IDataCatalogEntry;
import com.raytheon.viz.volumebrowser.util.PointLineUtil;
import com.raytheon.viz.volumebrowser.vbui.SelectedData;
import com.raytheon.viz.volumebrowser.vbui.VBMenuBarItemsMgr.ViewMenu;
import com.raytheon.viz.volumebrowser.vbui.VolumeBrowserAction;
import com.vividsolutions.jts.geom.Coordinate;
import com.vividsolutions.jts.geom.LineString;

/**
 * 
 * Default catalog for point data types
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date          Ticket#  Engineer  Description
 * ------------- -------- --------- --------------------------
 * Dec 01, 2009           bsteffen  Initial creation
 * May 08, 2013  14824    mgamazay  Added alterProductParameters method
 * May 09, 2013  1869     bsteffen  Modified D2D time series of point data to
 *                                  work without dataURI.
 * Aug 15, 2013  2258     bsteffen  Convert profiler sounding to var height
 *                                  with hodo.
 * Aug 15, 2013  2260     bsteffen  Switch poessounding to NSharp.
 * Jul 23, 2014  3410     bclement  location changed to floats
 * Sep 09, 2014  3356     njensen   Remove CommunicationException
 * Aug 03, 2015  3861     bsteffen  Move resource creation to ProductCreators
 * Oct 22, 2015  4970     bsteffen  Add ldadmesonet to supported plugins.
 * Sep 19, 2017  ----     mjames    Remove ldadmesonet, profiler.
 * 
 * </pre>
 * 
 * @author bsteffen
 * @version 1.0
 */
public class PointDataCatalog extends AbstractInventoryDataCatalog {
    protected static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(PointDataCatalog.class);

    protected static final Comparator<SurfaceObsLocation> locComparator = new Comparator<SurfaceObsLocation>() {

        @Override
        public int compare(SurfaceObsLocation o1, SurfaceObsLocation o2) {
            return Double.compare(o1.getLatitude(), o2.getLatitude());
        }

    };

    protected Map<String, SurfaceObsLocation[]> availableStations = new HashMap<String, SurfaceObsLocation[]>();

    @Override
    public void getAvailableData(AvailableDataRequest request) {
        if ((request.getSelectedFields() != null)
                && Arrays.asList(request.getSelectedFields()).contains("Snd")) {
            for (String source : getSupportedSources()) {
                request.addAvailableSource(source);
            }
        }
        super.getAvailableData(new PointAvailableDataRequest(request));
    }

    @Override
    public List<String> getSupportedSources() {
        List<String> supportedSources = new ArrayList<String>();
        for (String supportedSource : super.getSupportedSources()) {
            supportedSources.add(supportedSource);
            supportedSources.add(supportedSource + "OA");
        }
        return supportedSources;
    }

    @Override
    protected void addProductParameters(IDataCatalogEntry catalogEntry,
            HashMap<String, RequestConstraint> productParameters) {
        String sourceKey = catalogEntry.getSelectedData().getSourcesKey();
        productParameters.put("pluginName", new RequestConstraint(
                getPlugin(sourceKey)));
        addLineOrPointStationParameter(catalogEntry, productParameters,
                "location.stationId");
        String type = getType(sourceKey);
        if ((type != null) && !type.isEmpty()) {
            productParameters.put(getTypeKey(sourceKey), new RequestConstraint(
                    type));
        }
    }

    protected SurfaceObsLocation[] getStationLocations(String sourceKey) {
        // no need to sync if we already have it.
        if (availableStations.containsKey(sourceKey)) {
            return availableStations.get(sourceKey);
        }
        if (!Arrays.asList(getPlugins()).contains(getPlugin(sourceKey))) {
            availableStations.put(sourceKey, null);
            return null;
        }
        synchronized (availableStations) {
            if (availableStations.containsKey(sourceKey)) {
                return availableStations.get(sourceKey);
            }
            String type = getType(sourceKey);
            DbQuery query = new DbQuery(getPlugin(sourceKey));
            query.setDistinctField("location.stationId");
            query.addColumn("location.latitude");
            query.addColumn("location.longitude");
            query.addOrderBy("location.latitude");
            if ((type != null) && !type.isEmpty()) {
                query.addConstraint(getTypeKey(sourceKey),
                        new RequestConstraint(type));
            }
            try {
                List<Object[]> result = query.performQuery();
                SurfaceObsLocation[] locs = new SurfaceObsLocation[result
                        .size()];
                int i = 0;
                for (Object row : result) {
                    Object[] cols = (Object[]) row;
                    SurfaceObsLocation loc = new SurfaceObsLocation();
                    loc.setStationId(cols[0].toString());
                    loc.setLatitude(((Number) cols[1]).floatValue());
                    loc.setLongitude(((Number) cols[2]).floatValue());
                    locs[i++] = loc;
                }
                availableStations.put(sourceKey, locs);
                return locs;
            } catch (VizException e) {
                e.printStackTrace();
                return null;
            }
        }
    }

    public SurfaceObsLocation getClosestStation(Coordinate coordinate,
            String sourceKey) {
        return getClosestStation(coordinate, sourceKey, null);
    }

    public SurfaceObsLocation getClosestStation(Coordinate coordinate,
            String sourceKey, Collection<String> ignore) {
        SurfaceObsLocation[] availableStations = getStationLocations(sourceKey);
        if ((availableStations == null) || (availableStations.length == 0)
                || (coordinate == null)) {
            return null;
        }

        SurfaceObsLocation target = new SurfaceObsLocation();
        target.setLatitude((float) coordinate.y);
        target.setLongitude((float) coordinate.x);
        int index = Arrays.binarySearch(availableStations, target,
                locComparator);
        if (index < 1) {
            index = -index;
        }
        while ((index + 1) > availableStations.length) {
            index -= 1;
        }
        double bestDistance = Double.MAX_VALUE;
        SurfaceObsLocation bestLoc = null;
        for (int i = index; i >= 0; i--) {
            SurfaceObsLocation loc = availableStations[i];
            if ((ignore != null) && ignore.contains(loc.getStationId())) {
                continue;
            }
            double latD = target.getLatitude() - loc.getLatitude();
            if (latD > bestDistance) {
                break;
            }
            double lonD = target.getLongitude() - loc.getLongitude();
            double distance = Math.sqrt((latD * latD) + (lonD * lonD));
            if (distance < bestDistance) {
                bestLoc = loc;
                bestDistance = distance;
            }
        }
        for (int i = index; i < availableStations.length; i++) {
            SurfaceObsLocation loc = availableStations[i];
            if ((ignore != null) && ignore.contains(loc.getStationId())) {
                continue;
            }
            double latD = target.getLatitude() - loc.getLatitude();
            if (latD > bestDistance) {
                break;
            }
            double lonD = target.getLongitude() - loc.getLongitude();
            double distance = Math.sqrt((latD * latD) + (lonD * lonD));
            if (distance < bestDistance) {
                bestLoc = loc;
                bestDistance = distance;
            }
        }

        if (bestDistance > 2) {
            return null;
        }

        return bestLoc;

    }

    protected void addLineOrPointStationParameter(
            IDataCatalogEntry catalogEntry,
            HashMap<String, RequestConstraint> productParameters,
            String constraintKey) {
        if (!PointLineUtil.isPointLine(catalogEntry.getSelectedData()
                .getPlanesKey())
                && (catalogEntry.getDialogSettings().getViewSelection() != ViewMenu.TIMESERIES)) {
            return;
        }
        if (catalogEntry.getSelectedData().getSourcesKey().endsWith("OA")) {
            Coordinate current = PointLineUtil.getPointCoordinate(catalogEntry);
            double lon0 = current.x - 5;
            double lon1 = current.y + 5;
            double lat0 = current.y - 5;
            double lat1 = current.y + 5;

            RequestConstraint longitudeRequestConstraint = new RequestConstraint();
            longitudeRequestConstraint.setBetweenValueList(new String[] {
                    lon0 + "", lon1 + "" });
            longitudeRequestConstraint
                    .setConstraintType(ConstraintType.BETWEEN);

            RequestConstraint latitudeRequestConstraint = new RequestConstraint();
            latitudeRequestConstraint.setBetweenValueList(new String[] {
                    lat0 + "", lat1 + "" });
            latitudeRequestConstraint.setConstraintType(ConstraintType.BETWEEN);

            productParameters.put("location.latitude",
                    latitudeRequestConstraint);
            productParameters.put("location.longitude",
                    longitudeRequestConstraint);
            return;
        }
        if (catalogEntry.getSelectedData().getPlanesKey().startsWith("Line")) {
            String letter = catalogEntry.getSelectedData().getPlanesKey()
                    .replace("Line", "");
            LineString line = ToolsDataManager.getInstance()
                    .getBaseline(letter);
            RequestConstraint stationRC = new RequestConstraint();
            stationRC.setConstraintType(RequestConstraint.ConstraintType.IN);
            String sourceKey = catalogEntry.getSelectedData().getSourcesKey();
            Collection<String> closest = new ArrayList<String>();
            for (Coordinate c : line.getCoordinates()) {
                SurfaceObsLocation loc = getClosestStation(c, sourceKey,
                        closest);
                if (loc == null) {
                    break;
                }
                closest.add(loc.getStationId());
                stationRC.addToConstraintValueList(loc.getStationId());
            }
            productParameters.put(constraintKey, stationRC);
        } else {
            Coordinate coordinate = PointLineUtil
                    .getPointCoordinate(catalogEntry);

            if (coordinate != null) {
                String sourceKey = catalogEntry.getSelectedData()
                        .getSourcesKey();
                SurfaceObsLocation closestStation = getClosestStation(
                        coordinate, sourceKey);
                if ((closestStation != null)
                        && (closestStation.getStationId() != null)) {
                    productParameters.put(constraintKey, new RequestConstraint(
                            closestStation.getStationId()));
                } else {
                    productParameters.put(constraintKey, new RequestConstraint(
                            null, ConstraintType.ISNULL));
                }
            }
        }
    }

    @Override
    protected String[] getPlugins() {
        return new String[] { "goessounding", "poessounding", "bufrua", "obs", "sfcobs", "bufrmosLAMP" };
    }

    /**
     * Get the type of the point data from a source key, by default this will be
     * added as a request constraint to all data requests
     * 
     * @param sourceKey
     * @return
     */
    protected String getType(String sourceKey) {
        String plugin = getPlugin(sourceKey);
        String type = sourceKey.replace(plugin, "");
        if (type.endsWith("OA")) {
            type = type.substring(0, type.length() - 2);
        }
        return type;
    }

    /**
     * Get the type key to use when making any data queries
     * 
     * @param sourceKey
     * @return
     */
    protected String getTypeKey(String sourceKey) {
        String plugin = getPlugin(sourceKey);
        return getPointDataInventory().getTypeKey(plugin);
    }

    protected String getPlugin(String sourceKey) {
        for (String plugin : getPlugins()) {
            if (sourceKey.startsWith(plugin)) {
                return plugin;
            }
        }
        return sourceKey;
    }

    /**
     * @return
     */
    private AbstractPointDataInventory getPointDataInventory() {
        return (AbstractPointDataInventory) getInventory();
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.volumebrowser.datacatalog.IDataCatalog#getCatalogEntry
     * (com.raytheon.viz.volumebrowser.vbui.SelectedData)
     */
    @Override
    public IDataCatalogEntry getCatalogEntry(SelectedData selData) {
        if (!isValidSelection(selData)) {
            return null;
        }
        return new DataCatalogEntry(selData);

    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.volumebrowser.datacatalog.AbstractInventoryDataCatalog
     * #get3DLevels()
     */
    @Override
    protected Collection<? extends Level> get3DLevels() {
        return LevelMappingFactory
                .getInstance(
                        LevelMappingFactory.VOLUMEBROWSER_LEVEL_MAPPING_FILE)
                .getLevelMappingForKey("Station").getLevels();
    }

    @Override
    protected boolean isValidSelection(SelectedData selData) {
        try {
            Set<Level> levels = getLevels(
                    new String[] { selData.getPlanesKey() },
                    new String[] { selData.getSourcesKey() });
            if ((levels == null) || levels.isEmpty()) {
                return false;
            }
            List<String> params = Arrays.asList(selData.getFieldsKey());
            if (selData.getFieldsKey().equals("Snd")) {
                return true;
            }
            BlockingQueue<String> returnQueue = new ArrayBlockingQueue<String>(
                    levels.size());
            String sourceKey = selData.getSourcesKey();
            if (sourceKey.endsWith("OA")) {
                sourceKey = sourceKey.substring(0, sourceKey.length() - 2);
            }

            getInventory().checkLevels(Arrays.asList(sourceKey), params,
                    levels, returnQueue);

            if (returnQueue.isEmpty()) {
                return false;
            }
        } catch (InterruptedException e) {
            statusHandler.handle(Priority.PROBLEM,
                    "Error occured checking data availability", e);
            return false;
        }
        return true;
    }

    @Override
    protected Collection<String> get3DSources(String[] planes) {
        List<String> lineLetters = new ArrayList<String>();
        List<Double> lats = new ArrayList<Double>();
        List<Double> lons = new ArrayList<Double>();
        List<String> pointLetters = new ArrayList<String>();

        ViewMenu viewSelection = VolumeBrowserAction.getVolumeBrowserDlg()
                .getDialogSettings().getViewSelection();
        if (viewSelection == ViewMenu.PLANVIEW) {
            // no-op
        } else if (viewSelection == ViewMenu.TIMESERIES) {
            String name = VolumeBrowserAction.getVolumeBrowserDlg()
                    .getDialogSettings().getPointsSelection().getName();
            pointLetters.add(name);
        } else {
            if (planes == null) {
                return null;
            }
            for (String plane : planes) {
                if (plane.equals("LATS") || plane.equals("LONS")) {
                    return Collections.emptyList();
                } else if (plane.startsWith("Lat")) {
                    lats.add(Double.parseDouble(plane.replace("Lat", "")));
                } else if (plane.startsWith("Lon")) {
                    lons.add(Double.parseDouble(plane.replace("Lon", "")));
                } else if (plane.startsWith("Line")) {
                    lineLetters.add(plane.replace("Line", ""));
                } else if (plane.startsWith("Point")) {
                    pointLetters.add(plane.replace("Point", ""));
                }
            }
        }
        if (pointLetters.isEmpty() && lineLetters.isEmpty() && lats.isEmpty()
                && lons.isEmpty()) {
            return null;
        }
        Set<String> fileredSources = new HashSet<String>();
        for (String source : getSupportedSourcesInternal()) {
            if (source.endsWith("OA")) {
                fileredSources.add(source);
                continue;
            }
            for (String letter : pointLetters) {
                Coordinate c = PointsDataManager.getInstance().getCoordinate(
                        letter);
                if (getClosestStation(c, source) != null) {
                    fileredSources.add(source);
                    break;
                }
            }
            for (String letter : lineLetters) {
                LineString ls = ToolsDataManager.getInstance().getBaseline(
                        letter);
                boolean failed = false;
                for (Coordinate c : ls.getCoordinates()) {
                    if (getClosestStation(c, source) == null) {
                        failed = true;
                    }
                }
                if (!failed) {
                    fileredSources.add(source);
                    break;
                }
            }
        }
        return fileredSources;
    }

    @Override
    protected Collection<String> get3DPlanes(Collection<String> sources) {
        ViewMenu viewSelection = VolumeBrowserAction.getVolumeBrowserDlg()
                .getDialogSettings().getViewSelection();
        if ((sources == null) || sources.isEmpty()
                || (viewSelection == ViewMenu.PLANVIEW)
                || (viewSelection == ViewMenu.TIMESERIES)) {
            Set<String> results = new HashSet<String>();
            results.addAll(PointLineUtil.getPointLineKeys());
            return results;
        }
        ToolsDataManager tdm = ToolsDataManager.getInstance();
        PointsDataManager pdm = PointsDataManager.getInstance();
        Set<String> validPlanes = new HashSet<String>(sources.size());
        for (String source : sources) {
            for (String letter : pdm.getPointNames()) {
                Coordinate c = pdm.getCoordinate(letter);
                if (getClosestStation(c, source) != null) {
                    validPlanes.add("Point" + letter);
                }
            }
            for (String letter : tdm.getBaselineNames()) {
                LineString ls = tdm.getBaseline(letter);
                boolean failed = false;
                for (Coordinate c : ls.getCoordinates()) {
                    if (getClosestStation(c, source) == null) {
                        failed = true;
                    }
                }
                if (!failed) {
                    validPlanes.add("Line" + letter);
                }
            }
        }
        return validPlanes;
    }

    /**
     * Alter product parameters
     * 
     * @param selectedKey
     * @param selectedValue
     * @param productParameters
     */
    @Override
    public void alterProductParameters(String selectedKey,
            String selectedValue,
            HashMap<String, RequestConstraint> productParameters) {
        if (selectedKey.equalsIgnoreCase("line")) {
            LineString line = ToolsDataManager.getInstance().getBaseline(
                    selectedValue);
            RequestConstraint stationRC = new RequestConstraint();
            stationRC.setConstraintType(RequestConstraint.ConstraintType.IN);
            String sourceKey = productParameters.get("pluginName")
                    .getConstraintValue();
            Collection<String> closest = new ArrayList<String>();
            for (Coordinate c : line.getCoordinates()) {
                SurfaceObsLocation loc = getClosestStation(c, sourceKey,
                        closest);
                if (loc == null) {
                    break;
                }
                closest.add(loc.getStationId());
                stationRC.addToConstraintValueList(loc.getStationId());
            }
            productParameters.put("location.stationId", stationRC);
        } else if (selectedKey.equalsIgnoreCase("point")) {
            Coordinate point = PointsDataManager.getInstance().getCoordinate(
                    selectedValue);
            String sourceKey = productParameters.get("pluginName")
                    .getConstraintValue();

            SurfaceObsLocation closestStation = getClosestStation(point,
                    sourceKey);
            System.out.println();
            productParameters.put("location.stationId", new RequestConstraint(
                    closestStation.getStationId()));
            return;
        }
        return;
    }
}
