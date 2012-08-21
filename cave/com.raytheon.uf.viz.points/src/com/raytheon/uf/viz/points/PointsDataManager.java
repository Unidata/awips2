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
package com.raytheon.uf.viz.points;

import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.FileWriter;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.Map;

import javax.measure.converter.UnitConverter;
import javax.measure.unit.NonSI;
import javax.measure.unit.SI;

import org.eclipse.core.runtime.ListenerList;
import org.eclipse.jface.util.IPropertyChangeListener;
import org.eclipse.jface.util.PropertyChangeEvent;
import org.geotools.referencing.GeodeticCalculator;
import org.geotools.referencing.datum.DefaultEllipsoid;

import com.raytheon.uf.common.awipstools.GetWfoCenterPoint;
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
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.localization.LocalizationManager;
import com.raytheon.uf.viz.core.requests.ThriftClient;
import com.raytheon.viz.core.CorePlugin;
import com.vividsolutions.jts.geom.Coordinate;

/**
 * This manages maintaining localized files that contain information on points
 * including the special point home.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jul 10, 2012 #875       rferrel     Initial creation
 * 
 * </pre>
 * 
 * @author rferrel
 * @version 1.0
 */

public class PointsDataManager implements ILocalizationFileObserver,
        IPropertyChangeListener {
    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(PointsDataManager.class);

    private static final GeodeticCalculator GC = new GeodeticCalculator(
            DefaultEllipsoid.WGS84);

    private static final UnitConverter NM_TO_METERS = NonSI.NAUTICAL_MILE
            .getConverterTo(SI.METER);

    private static PointsDataManager theManager;

    private static final String MOVABLE_POINT_PREFIX = "movablePoint";

    private static final String MOVABLE_POINT_EXT = ".txt";

    private static final String HOME_LOCATION_FILE = "HomeLocation.dat";

    private Map<String, Coordinate> points;

    private ListenerList pointsListeners = new ListenerList();

    private Coordinate home;

    private ListenerList homeListeners = new ListenerList();

    private Coordinate wfoCenter;

    private String site;

    private LocalizationFile userToolsDir;

    private IPathManager pathMgr;

    public static synchronized PointsDataManager getInstance() {
        if (theManager == null) {
            theManager = new PointsDataManager();
        }
        return theManager;
    }

    private PointsDataManager() {
        site = LocalizationManager.getInstance().getCurrentSite();

        pathMgr = PathManagerFactory.getPathManager();
        LocalizationContext userCtx = pathMgr.getContext(
                LocalizationType.CAVE_STATIC, LocalizationLevel.USER);

        userToolsDir = pathMgr.getLocalizationFile(userCtx, "awipsTools"
                + File.separator + site);
        userToolsDir.addFileUpdatedObserver(this);

        CorePlugin.getDefault().getPreferenceStore()
                .addPropertyChangeListener(this);
    }

    public Collection<String> getPointNames() {
        return getPoints().keySet();
    }

    /**
     * 
     * @param name
     * @return the point coordinate associated with the given name, null if no
     *         point exists by that name
     */
    public Coordinate getPoint(String name) {
        if (points == null) {
            getPoints();
        }
        Coordinate pointCoordinate = points.get(name);
        if (pointCoordinate == null) {
            return null;
        }
        return new Coordinate(pointCoordinate);
    }

    public void setPoint(String name, Coordinate point) {
        points.put(name, new Coordinate(point));
        storePoint(userToolsDir, point, MOVABLE_POINT_PREFIX + name
                + MOVABLE_POINT_EXT);
    }

    /**
     * A convince method to get the current WFO's coordinates from the server.
     * 
     * @return
     */
    public Coordinate getWfoCenter() {
        if (wfoCenter == null) {
            loadWfoCenter();
        }
        return new Coordinate(wfoCenter);
    }

    public Coordinate getHome() {
        if (home == null) {
            loadHome();
        }
        return new Coordinate(home);
    }

    public void setHome(Coordinate home) {
        if (home == null) {
            return;
        }
        this.home = new Coordinate(home);
        storeHome();
    }

    private void storeHome() {
        storePoint(userToolsDir, home, HOME_LOCATION_FILE);
    }

    private void storePoint(LocalizationFile dir, Coordinate point,
            String fileName) {

        LocalizationFile lf = pathMgr.getLocalizationFile(dir.getContext(),
                dir.getName() + File.separator + fileName);
        File file = lf.getFile();

        // create the local directory if necessary
        if (!file.getParentFile().exists()) {
            file.getParentFile().mkdirs();
        }

        BufferedWriter out = null;
        try {
            out = new BufferedWriter(new FileWriter(file));
            out.write(String.format("%f %f\n", point.y, point.x));
        } catch (IOException e) {
            statusHandler.handle(Priority.PROBLEM, "Error writing to file: "
                    + file.getAbsolutePath(), e);
        } finally {
            if (out != null) {
                try {
                    out.close();
                    lf.save();
                } catch (IOException e) {
                    statusHandler.handle(Priority.PROBLEM,
                            "Error writing to file: " + file.getAbsolutePath(),
                            e);
                } catch (LocalizationException e) {
                    statusHandler.handle(Priority.PROBLEM,
                            "Error storing locatization file to server: " + lf,
                            e);
                }
            }
        }
    }

    private Map<String, Coordinate> getPoints() {
        if (points == null || points.isEmpty()) {
            loadPoints();
            if (points == null || points.isEmpty()) {
                createDefaultPoints();
            }
        }
        return points;
    }

    private void createDefaultPoints() {
        points = new HashMap<String, Coordinate>();
        Coordinate center = getHome();
        int baseRingSize = 120;
        int startAngle = -90;
        for (char label = 'A'; label <= 'J'; label++) {
            Coordinate point = getCoordinateOnCircle(center, baseRingSize,
                    startAngle);
            setPoint(String.valueOf(label), point);
            startAngle += 36;
        }
    }

    private void loadWfoCenter() {
        try {
            // Request WFO center point from server
            wfoCenter = (Coordinate) ThriftClient
                    .sendLocalizationRequest(new GetWfoCenterPoint(site));
        } catch (VizException e) {
            statusHandler.handle(Priority.PROBLEM,
                    "Unable to get WFO center for \"" + site + "\"", e);
        }
        if (wfoCenter == null) {
            wfoCenter = new Coordinate(-96.2, 41.2);
        }
    }

    private Coordinate loadHome() {
        home = loadPoint(userToolsDir, HOME_LOCATION_FILE);
        if (home == null) {
            home = getWfoCenter();
            storeHome();
        }
        return home;
    }

    private void loadPoints() {
        points = new HashMap<String, Coordinate>();

        LocalizationFile[] files = pathMgr.listFiles(userToolsDir.getContext(),
                userToolsDir.getName(), new String[] { MOVABLE_POINT_EXT },
                false, true);
        for (LocalizationFile lf : files) {
            String fileName = lf.getFile().getName();
            if (fileName.startsWith(MOVABLE_POINT_PREFIX)) {
                Coordinate point = loadPoint(userToolsDir, fileName);
                String name = fileName.substring(MOVABLE_POINT_PREFIX.length())
                        .replace(MOVABLE_POINT_EXT, "");
                points.put(name, point);
            }
        }
    }

    public Coordinate getCoordinateOnCircle(Coordinate coor, double radius,
            int angle) {

        if (angle > 180) {
            angle = angle - 360;
        }
        GC.setStartingGeographicPoint(coor.x, coor.y);
        GC.setDirection(angle, NM_TO_METERS.convert(radius));

        return new Coordinate(GC.getDestinationGeographicPoint().getX(), GC
                .getDestinationGeographicPoint().getY());

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

        if (fileName.startsWith(MOVABLE_POINT_PREFIX)) {
            pointsFileUpdated(fileName);
        } else if (fileName.startsWith(HOME_LOCATION_FILE)) {
            homeLocationFileUpdated(fileName);
        }
    }

    private void pointsFileUpdated(String fileName) {
        if (points != null) {
            Coordinate point = loadPoint(userToolsDir, fileName);
            String name = fileName.substring(MOVABLE_POINT_PREFIX.length())
                    .replace(MOVABLE_POINT_EXT, "");
            points.put(name, point);

            for (Object listener : pointsListeners.getListeners()) {
                ((IPointChangedListener) listener).pointChanged();
            }
        }
    }

    private void homeLocationFileUpdated(String fileName) {
        if (home != null) {
            loadHome();

            ArrayList<Thread> threads = new ArrayList<Thread>(
                    homeListeners.size());
            for (final Object listener : homeListeners.getListeners()) {
                // fire listeners in separate threads to avoid waiting to draw
                // the updated location while waiting on another listener to
                // finish
                Thread t = new Thread(new Runnable() {

                    @Override
                    public void run() {
                        ((IPointChangedListener) listener).pointChanged();
                    }

                });
                threads.add(t);
                t.start();
            }

            // join all threads before continuing so we can't fire listeners
            // again until all have finished
            for (Thread t : threads) {
                try {
                    t.join();
                } catch (InterruptedException e) {
                    statusHandler.handle(
                            Priority.SIGNIFICANT,
                            "Unexpected Interruption from: "
                                    + e.getLocalizedMessage(), e);
                }
            }
        }
    }

    private Coordinate loadPoint(LocalizationFile dir, String fileName) {
        Coordinate point = null;

        LocalizationFile lf = pathMgr.getLocalizationFile(dir.getContext(),
                dir.getName() + File.separator + fileName);
        File file = lf.getFile();
        BufferedReader in = null;
        try {
            in = new BufferedReader(new FileReader(file));
            String line = in.readLine();
            line = line.trim();
            int p = line.indexOf(' ');
            double lat = Double.parseDouble(line.substring(0, p));
            double lon = Double.parseDouble(line.substring(p));

            if (lat > 90.0 || lat < -90.0 || lon > 180.0 || lon < -180.0) {
                statusHandler
                        .handle(Priority.PROBLEM,
                                "Invalid lat/lon in wfo center point file, using default");
            } else {
                point = new Coordinate(lon, lat);
            }
        } catch (NumberFormatException e) {
            statusHandler
                    .handle(Priority.PROBLEM,
                            "Invalid number in wfo center point file, using default",
                            e);
        } catch (FileNotFoundException e) {
            statusHandler.handle(Priority.EVENTA,
                    "No wfo center point file found, creating default.");
        } catch (IOException e) {
            statusHandler.handle(Priority.PROBLEM,
                    "Error reading wfo center point file, using default", e);
        } finally {
            if (in != null) {
                try {
                    in.close();
                } catch (IOException e) {
                    // nothing to do
                }
            }
        }
        return point;
    }

    /**
     * @param listener
     */
    public void addPointsChangedListener(IPointChangedListener listener) {
        pointsListeners.add(listener);
    }

    /**
     * @param listener
     */
    public void removePointsChangedListener(IPointChangedListener listener) {
        pointsListeners.remove(listener);
    }

    /**
     * @param listener
     */
    public void addHomeChangedListener(IPointChangedListener listener) {
        homeListeners.add(listener);
    }

    /**
     * @param listener
     */
    public void removeHomeChangedListener(IPointChangedListener listener) {
        homeListeners.remove(listener);
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * org.eclipse.core.runtime.Preferences.IPropertyChangeListener#propertyChange
     * (org.eclipse.core.runtime.Preferences.PropertyChangeEvent)
     */
    @Override
    public void propertyChange(PropertyChangeEvent event) {
        // TODO Auto-generated method stub

    }
}
