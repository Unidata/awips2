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
package com.raytheon.viz.awipstools;

import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.File;
import java.io.FileReader;
import java.io.FileWriter;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Date;
import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.concurrent.BlockingQueue;
import java.util.concurrent.LinkedBlockingQueue;

import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.ListenerList;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.jobs.Job;
import org.eclipse.jface.preference.IPersistentPreferenceStore;
import org.eclipse.jface.util.IPropertyChangeListener;
import org.eclipse.jface.util.PropertyChangeEvent;

import com.raytheon.uf.common.localization.FileUpdatedMessage;
import com.raytheon.uf.common.localization.ILocalizationFileObserver;
import com.raytheon.uf.common.localization.IPathManager;
import com.raytheon.uf.common.localization.LocalizationContext;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationLevel;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationType;
import com.raytheon.uf.common.localization.LocalizationFile;
import com.raytheon.uf.common.localization.PathManagerFactory;
import com.raytheon.uf.common.localization.exception.LocalizationException;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.common.time.SimulatedTime;
import com.raytheon.uf.viz.core.catalog.DirectDbQuery;
import com.raytheon.uf.viz.core.catalog.DirectDbQuery.QueryLanguage;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.localization.HierarchicalPreferenceStore;
import com.raytheon.uf.viz.core.localization.LocalizationManager;
import com.raytheon.uf.viz.points.PointsDataManager;
import com.raytheon.viz.awipstools.common.RangeRing;
import com.raytheon.viz.awipstools.common.RangeRing.RangeRingType;
import com.raytheon.viz.awipstools.common.StormTrackData;
import com.raytheon.viz.core.CorePlugin;
import com.vividsolutions.jts.geom.Coordinate;
import com.vividsolutions.jts.geom.GeometryFactory;
import com.vividsolutions.jts.geom.LineSegment;
import com.vividsolutions.jts.geom.LineString;

/**
 * A Single class responsible for generating, storing, and loading any data used
 * by various tools. This includes baselines, range rings and distance bearings.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 10-21-09	    #1711      bsteffen    Initial Creation
 * 04-07-10     #4614      randerso    Reworked to use localization files
 * 07-11-12     #875       rferrel     Move points to PointsDataManager.
 * 01-29-14     DR 16351   D. Friedman Fix updates to storm track from preferences.
 * 
 * </pre>
 * 
 * @author bsteffen
 * @version 1.0
 */
public class ToolsDataManager implements ILocalizationFileObserver,
        IPropertyChangeListener {
    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(ToolsDataManager.class);

    private static final String BASELINE_PREFIX = "baseline_";

    private static final String BASELINE_EXT = ".txt";

    private static final String P_RANGERING_LOCATIONS = "rangeRingLocations";

    private static final String P_STORMTRACK_SPEED = "stormSpeed";

    private static final String P_STORMTRACK_ANGLE = "stormAngle";

    private static final String P_STORMTRACK_POINTS = "stormCoordinates";

    private static final String P_STORMTRACK_DATE = "stormDate";

    private static final int[] DEFAULT_LINE_RADIUS = { 120, 120, 120, 120, 240,
            240, 216, 216, 360, 360 };

    private static final int[] DEFAULT_LINE_STARTDIR = { 225, 315, 0, 270, 225,
            315, 315, 45, 225, 315 };

    private static final int[] DEFAULT_LINE_ENDDIR = { 45, 135, 180, 90, 135,
            45, 225, 135, 135, 45 };

    private static final GeometryFactory GF = new GeometryFactory();

    private static ToolsDataManager theManager = null;

    private Map<String, LineString> baselines;

    private ListenerList baselineListeners = new ListenerList();

    private PointsDataManager pointsManager;

    private Collection<RangeRing> rangeRings;

    private StormTrackData stormData;

    private ListenerList stormListeners = new ListenerList();

    private Object stormLock = new Object();

    private boolean stormTrackDirty = false;

    private String site;

    private LocalizationFile userToolsDir;

    private IPathManager pathMgr;

    private BlockingQueue<String> baselineStoreQueue = new LinkedBlockingQueue<String>();

    public static synchronized ToolsDataManager getInstance() {
        if (theManager == null) {
            theManager = new ToolsDataManager();
        }
        return theManager;
    }

    private ToolsDataManager() {
        site = LocalizationManager.getInstance().getCurrentSite();

        pathMgr = PathManagerFactory.getPathManager();
        pointsManager = PointsDataManager.getInstance();
        LocalizationContext userCtx = pathMgr.getContext(
                LocalizationType.CAVE_STATIC, LocalizationLevel.USER);

        userToolsDir = pathMgr.getLocalizationFile(userCtx, "awipsTools"
                + File.separator + site);
        userToolsDir.addFileUpdatedObserver(this);

        CorePlugin.getDefault().getPreferenceStore()
                .addPropertyChangeListener(this);
    }

    public Collection<String> getBaselineNames() {
        return getBaselines().keySet();
    }

    /**
     * 
     * @param name
     * @return the baseline associated with the given name, null if the given
     *         name isn't associated with any baseline
     */
    public LineString getBaseline(String name) {
        LineString baseline = getBaselines().get(name);
        if (baseline == null) {
            return null;
        }
        return (LineString) baseline.clone();
    }

    public void setBaseline(String name, LineString baseline) {
        baseline = (LineString) baseline.clone();
        getBaselines().put(name, baseline);
        storeBaseline(name);
    }

    public Collection<LineSegment> getDistanceBearings() {
        Collection<LineSegment> distanceBearings = new ArrayList<LineSegment>();
        Coordinate center = pointsManager.getHome();
        for (int i = 0; i < 6; i++) {
            Coordinate start = pointsManager.getCoordinateOnCircle(center,
                    DEFAULT_LINE_RADIUS[i], DEFAULT_LINE_STARTDIR[i]);
            Coordinate end = pointsManager.getCoordinateOnCircle(center,
                    DEFAULT_LINE_RADIUS[i], DEFAULT_LINE_ENDDIR[i]);
            LineSegment line = new LineSegment(start, end);
            distanceBearings.add(line);
        }
        return distanceBearings;
    }

    public Collection<RangeRing> getRangeRings() {
        if (rangeRings == null) {
            IPersistentPreferenceStore prefStore = CorePlugin.getDefault()
                    .getPreferenceStore();

            if (prefStore.getString(P_RANGERING_LOCATIONS).isEmpty()) {
                createDefaultRangeRings();
            } else {
                loadRangeRings();
            }
        }
        Collection<RangeRing> copy = new ArrayList<RangeRing>(rangeRings.size());
        copy.addAll(rangeRings);
        return copy;
    }

    public void setRangeRings(Collection<RangeRing> rangeRings) {
        if (this.rangeRings == null) {
            this.rangeRings = new ArrayList<RangeRing>(rangeRings.size());
        }
        this.rangeRings.clear();
        this.rangeRings.addAll(rangeRings);
        storeRangeRings();
    }

    public StormTrackData getStormTrackData() {
        synchronized (stormLock) {
            if (stormData == null || stormTrackDirty) {
                loadStormData();
            }
            return new StormTrackData(stormData);
        }
    }

    public void setStormTrackData(StormTrackData data) {
        synchronized (stormLock) {
            if (data != null) {
                stormData = new StormTrackData(data);
                storeStormData();
            }
        }
    }

    private void loadStormData() {
        stormData = new StormTrackData();
        HierarchicalPreferenceStore store = (HierarchicalPreferenceStore) CorePlugin
                .getDefault().getPreferenceStore();
        store.setDefault(P_STORMTRACK_SPEED, 35.0);
        double speed = store.getDouble(P_STORMTRACK_SPEED);
        stormData.setMotionSpeed(speed);

        store.setDefault(P_STORMTRACK_ANGLE, 60.0);
        double angle = store.getDouble(P_STORMTRACK_ANGLE);
        stormData.setMotionDirection(angle);

        long date = store.getLong(P_STORMTRACK_DATE);
        if (date > 0) {
            stormData.setDate(new Date(date));
        }
        String[] points = store.getStringArray(P_STORMTRACK_POINTS);
        if (points != null) {
            setCoordinates(stormData, points);
        }
        stormTrackDirty = false;
    }

    private void setCoordinates(StormTrackData data, String[] points) {
        Coordinate[] coords = new Coordinate[points.length];
        for (int i = 0; i < points.length; ++i) {
            String[] latLon = points[i].split("[ ]");
            try {
                coords[i] = new Coordinate(Double.parseDouble(latLon[0]),
                        Double.parseDouble(latLon[1]));
            } catch (NumberFormatException e) {
                statusHandler.handle(Priority.PROBLEM,
                        "Error reading storm track coordinates", e);
                coords = new Coordinate[0];
                break;
            }
        }
        data.setCoordinates(coords);
    }

    private void storeStormData() {
        synchronized (stormLock) {
            // Update the store time
            stormData.setDate(SimulatedTime.getSystemTime().getTime());
            HierarchicalPreferenceStore store = (HierarchicalPreferenceStore) CorePlugin
                    .getDefault().getPreferenceStore();
            store.setValue(P_STORMTRACK_SPEED, stormData.getMotionSpeed());
            store.setValue(P_STORMTRACK_ANGLE, stormData.getMotionDirection());
            Coordinate[] coordinates = stormData.getCoordinates();
            if (coordinates != null) {
                String[] coords = new String[coordinates.length];
                for (int i = 0; i < coordinates.length; ++i) {
                    coords[i] = coordinates[i].x + " " + coordinates[i].y;
                }
                store.setValue(P_STORMTRACK_POINTS, coords);
            }
            store.setValue(P_STORMTRACK_DATE, stormData.getDate().getTime());
            try {
                store.save();
            } catch (IOException e) {
                statusHandler.handle(Priority.PROBLEM,
                        "Error saving storm track data", e);
            }
        }
    }

    private Map<String, LineString> getBaselines() {
        if (baselines == null || baselines.isEmpty()) {
            loadBaselines();
            if (baselines == null || baselines.isEmpty()) {
                createDefaultBaselines();
            }
        }
        return baselines;
    }

    private void createDefaultBaselines() {
        baselines = new LinkedHashMap<String, LineString>();
        Coordinate center = pointsManager.getHome();
        for (int i = 0; i < 10; i++) {
            String label = String.valueOf((char) ('A' + i));
            Coordinate start = pointsManager.getCoordinateOnCircle(center,
                    DEFAULT_LINE_RADIUS[i], DEFAULT_LINE_STARTDIR[i]);
            Coordinate end = pointsManager.getCoordinateOnCircle(center,
                    DEFAULT_LINE_RADIUS[i], DEFAULT_LINE_ENDDIR[i]);
            LineString baseline = GF.createLineString(new Coordinate[] { start,
                    end });
            baselines.put(label, baseline);
            storeBaseline(label);
        }
    }

    private void createDefaultRangeRings() {
        LocalizationManager loc = LocalizationManager.getInstance();

        String id = loc.getCurrentSite();
        String query = "SELECT lat, lon, rda_id FROM radar_spatial WHERE wfo_id = '"
                + id.toUpperCase() + "';";
        List<Object[]> rows = null;
        try {
            rows = DirectDbQuery.executeQuery(query, "metadata",
                    QueryLanguage.SQL);
        } catch (VizException e) {
            e.printStackTrace();
            rows = new ArrayList<Object[]>();
        }
        float[] result = new float[3];
        rangeRings = new ArrayList<RangeRing>();
        for (int r = 0; r < rows.size(); r++) {
            String rda_id = "";
            if (rows.size() > 0) {
                Object[] row = rows.get(r);
                for (int i = 0; i < result.length; ++i) {
                    if (row[i] instanceof String) {
                        rda_id = (String) row[i];
                    } else {
                        result[i] = ((Number) row[i]).floatValue();
                    }
                }
            }
            double lon = result[0];
            double lat = result[1];
            RangeRing sample = new RangeRing(rda_id, new Coordinate(lat, lon),
                    new int[] { 5, 0, 0 }, "None", true);
            rangeRings.add(sample);
        }
    }

    private Job baselineStoreJob = new Job("Storing Baselines") {

        @Override
        protected IStatus run(IProgressMonitor monitor) {
            String name = baselineStoreQueue.poll();
            while (name != null && !monitor.isCanceled()) {
                LineString baseline = baselines.get(name);

                if (baseline == null) {
                    statusHandler.handle(Priority.PROBLEM,
                            "Attempting to store non-existent baseline \""
                                    + name + "\"");
                }

                String fileName = userToolsDir.getName() + File.separator
                        + BASELINE_PREFIX + name + BASELINE_EXT;

                LocalizationFile lf = pathMgr.getLocalizationFile(
                        userToolsDir.getContext(), fileName);
                File file = lf.getFile();

                // create the local directory if necessary
                if (!file.getParentFile().exists()) {
                    file.getParentFile().mkdirs();
                }

                BufferedWriter out = null;
                try {
                    out = new BufferedWriter(new FileWriter(file));

                    for (Coordinate point : baseline.getCoordinates()) {
                        out.write(String.format("%f %f\n", point.y, point.x));
                    }
                } catch (IOException e) {
                    statusHandler.handle(Priority.PROBLEM,
                            "Error writing to file: " + file.getAbsolutePath(),
                            e);
                } finally {
                    if (out != null) {
                        try {
                            out.close();
                            lf.save();
                        } catch (IOException e) {
                            statusHandler.handle(
                                    Priority.PROBLEM,
                                    "Error writing to file: "
                                            + file.getAbsolutePath(), e);
                        } catch (LocalizationException e) {
                            statusHandler.handle(Priority.PROBLEM,
                                    "Error storing locatization file to server: "
                                            + lf, e);
                        }
                    }
                }
                name = baselineStoreQueue.poll();
            }
            return Status.OK_STATUS;
        }

    };

    private void storeBaseline(String name) {
        baselineStoreQueue.add(name);
        baselineStoreJob.schedule();
    }

    private void storeRangeRings() {
        StringBuilder pref = new StringBuilder();
        for (RangeRing ring : rangeRings) {
            pref.append(ring.getId());
            pref.append(":");
            Coordinate c = ring.getCenterCoordinate();
            pref.append(c.x);
            pref.append("|");
            pref.append(c.y);
            pref.append(":");
            pref.append(ring.isVisible());
            pref.append(":");
            pref.append(ring.getLabel());
            pref.append(":");
            if (ring.getType() == RangeRingType.MOVABLE) {
                pref.append(ring.getRadius());
            } else {
                int[] radii = ring.getRadii();
                for (int radius : radii) {
                    pref.append(radius);
                    pref.append("|");
                }
            }
            pref.append("_");
        }
        IPersistentPreferenceStore prefStore = CorePlugin.getDefault()
                .getPreferenceStore();
        prefStore.setValue(P_RANGERING_LOCATIONS, pref.toString());
        try {
            prefStore.save();
        } catch (IOException e) {
            statusHandler.handle(Priority.PROBLEM,
                    "Error occured saving range rings");
        }
    }

    private void loadBaselines() {
        baselines = new HashMap<String, LineString>();

        LocalizationFile[] files = pathMgr.listFiles(userToolsDir.getContext(),
                userToolsDir.getName(), new String[] { BASELINE_EXT }, false,
                true);
        for (LocalizationFile lf : files) {
            String fileName = lf.getFile().getName();
            if (fileName.startsWith(BASELINE_PREFIX)) {
                LineString baseline = loadBaseline(fileName);
                String name = fileName.substring(BASELINE_PREFIX.length())
                        .replace(BASELINE_EXT, "");
                baselines.put(name, baseline);
            }
        }
    }

    private LineString loadBaseline(String fileName) {
        LineString baseline = null;

        LocalizationFile lf = pathMgr.getLocalizationFile(
                userToolsDir.getContext(), userToolsDir.getName()
                        + File.separator + fileName);
        File file = lf.getFile();

        BufferedReader in = null;
        try {
            in = new BufferedReader(new FileReader(file));

            List<Coordinate> coords = new ArrayList<Coordinate>();
            String line = null;
            while ((line = in.readLine()) != null) {
                line = line.trim();
                int p = line.indexOf(' ');
                double lat = Double.parseDouble(line.substring(0, p));
                double lon = Double.parseDouble(line.substring(p));

                if (lat > 90.0 || lat < -90.0 || lon > 180.0 || lon < -180.0) {
                    statusHandler
                            .handle(Priority.PROBLEM,
                                    "Invalid lat/lon in wfo center point file, using default");
                } else {
                    coords.add(new Coordinate(lon, lat));
                }
            }
            GeometryFactory gf = new GeometryFactory();
            baseline = gf.createLineString(coords.toArray(new Coordinate[coords
                    .size()]));

        } catch (NumberFormatException e) {
            statusHandler
                    .handle(Priority.PROBLEM,
                            "Invalid number in wfo center point file, using default",
                            e);
        } catch (IOException e) {
            statusHandler.handle(Priority.PROBLEM, "Error writing to file: "
                    + file.getAbsolutePath(), e);
        } finally {
            if (in != null) {
                try {
                    in.close();
                } catch (IOException e) {
                    // nothing to do
                }
            }
        }

        return baseline;
    }

    private void loadRangeRings() {
        rangeRings = new ArrayList<RangeRing>();

        IPersistentPreferenceStore prefStore = CorePlugin.getDefault()
                .getPreferenceStore();

        String pref = prefStore.getString(P_RANGERING_LOCATIONS);
        String[] ringEntries = pref.split("_");
        for (String ringStr : ringEntries) {
            String[] fields = ringStr.split(":");
            String id = fields[0];
            String[] latlon = fields[1].split("\\|");
            Boolean visible = Boolean.parseBoolean(fields[2]);
            String label = fields[3];
            String[] radiiStr = fields[4].split("\\|");
            Double lon = Double.parseDouble(latlon[0]);
            Double lat = Double.parseDouble(latlon[1]);
            Coordinate center = new Coordinate(lon, lat);
            RangeRing ring = null;
            if (radiiStr.length == 1) {
                int radius = Integer.parseInt(radiiStr[0]);
                ring = new RangeRing(id, center, radius, label, visible);
            } else {
                int[] radii = new int[radiiStr.length];
                for (int i = 0; i < radii.length; i++) {
                    radii[i] = Integer.parseInt(radiiStr[i]);
                }
                ring = new RangeRing(id, center, radii, label, visible);
            }
            rangeRings.add(ring);
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.common.localization.ILocalizationFileObserver#fileUpdated
     * (com.raytheon.uf.common.localization.FileUpdatedMessage)
     */
    @Override
    public void fileUpdated(FileUpdatedMessage message) {
        String fileName = new File(message.getFileName()).getName();
        if (fileName.startsWith(BASELINE_PREFIX)) {
            baselineFileUpdated(fileName);
        } else {
            pointsManager.fileUpdated(message);
        }
    }

    private void baselineFileUpdated(String fileName) {
        if (baselines != null) {
            LineString baseline = loadBaseline(fileName);
            String name = fileName.substring(BASELINE_PREFIX.length()).replace(
                    BASELINE_EXT, "");
            baselines.put(name, baseline);

            for (Object listener : baselineListeners.getListeners()) {
                ((IToolChangedListener) listener).toolChanged();
            }
        }
    }

    /**
     * @param listener
     */
    public void addBaselinesChangedListener(IToolChangedListener listener) {
        baselineListeners.add(listener);
    }

    /**
     * @param listener
     */
    public void removeBaselinesChangedListener(IToolChangedListener listener) {
        baselineListeners.remove(listener);
    }

    /**
     * @param listener
     */
    public void addStormTrackChangedListener(IToolChangedListener listener) {
        stormListeners.add(listener);
    }

    /**
     * @param listener
     */
    public void removeStormTrackChangedListener(IToolChangedListener listener) {
        stormListeners.remove(listener);
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * org.eclipse.jface.util.IPropertyChangeListener#propertyChange(org.eclipse
     * .jface.util.PropertyChangeEvent)
     */
    @Override
    public void propertyChange(PropertyChangeEvent event) {
        String key = event.getProperty();
        if ((P_STORMTRACK_ANGLE.equals(key) || P_STORMTRACK_DATE.equals(key)
                || P_STORMTRACK_POINTS.equals(key) || P_STORMTRACK_SPEED
                .equals(key)) && stormData != null) {
            synchronized (stormLock) {
                Object value = event.getNewValue();
                if (P_STORMTRACK_ANGLE.equals(key) && value instanceof Double) {
                    stormData.setMotionDirection((Double) value);
                } else if (P_STORMTRACK_DATE.equals(key)
                        && value instanceof Long) {
                    stormData.setDate(new Date((Long) value));
                } else if (P_STORMTRACK_POINTS.equals(key)
                        && value instanceof String[]) {
                    setCoordinates(stormData, (String[]) value);
                } else if (P_STORMTRACK_SPEED.equals(key)
                        && value instanceof Double) {
                    stormData.setMotionSpeed((Double) value);
                } else {
                    /* Incompatible value indicates update from preference
                     * store.  We will want to reload.
                     */
                    stormTrackDirty = true;
                }
            }

            // fire listeners
            for (Object listener : stormListeners.getListeners()) {
                ((IToolChangedListener) listener).toolChanged();
            }
        }
    }
}
