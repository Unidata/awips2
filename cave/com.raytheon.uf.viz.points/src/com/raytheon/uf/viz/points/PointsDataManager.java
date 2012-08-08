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
import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.Comparator;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.StringTokenizer;
import java.util.concurrent.atomic.AtomicBoolean;

import javax.measure.converter.UnitConverter;
import javax.measure.unit.NonSI;
import javax.measure.unit.SI;

import org.eclipse.core.runtime.Assert;
import org.eclipse.core.runtime.ListenerList;
import org.eclipse.jface.util.IPropertyChangeListener;
import org.eclipse.jface.util.PropertyChangeEvent;
import org.eclipse.swt.graphics.RGB;
import org.geotools.referencing.GeodeticCalculator;
import org.geotools.referencing.datum.DefaultEllipsoid;

import com.raytheon.uf.common.awipstools.GetWfoCenterPoint;
import com.raytheon.uf.common.localization.FileUpdatedMessage;
import com.raytheon.uf.common.localization.FileUpdatedMessage.FileChangeType;
import com.raytheon.uf.common.localization.ILocalizationFileObserver;
import com.raytheon.uf.common.localization.IPathManager;
import com.raytheon.uf.common.localization.LocalizationContext;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationLevel;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationType;
import com.raytheon.uf.common.localization.LocalizationFile;
import com.raytheon.uf.common.localization.PathManagerFactory;
import com.raytheon.uf.common.localization.exception.LocalizationException;
import com.raytheon.uf.common.localization.exception.LocalizationOpFailedException;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.localization.LocalizationManager;
import com.raytheon.uf.viz.core.requests.ThriftClient;
import com.raytheon.uf.viz.points.data.GroupNode;
import com.raytheon.uf.viz.points.data.IPointNode;
import com.raytheon.uf.viz.points.data.Point;
import com.raytheon.uf.viz.points.data.PointNameChangeException;
import com.raytheon.uf.viz.points.data.PointSize;
import com.raytheon.viz.core.CorePlugin;
import com.vividsolutions.jts.geom.Coordinate;

/**
 * This manages maintaining localized files that contain information on points
 * including the special point home. The home is also managed in localization
 * preferences, however the two should match.
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

    private static final String GROUP_INFO = "group.dat";

    private static final String GROUP_TEMP_PREFIX = "TEMP_";

    private static final String D2D_POINTS_GROUP = "D2D Points";

    private static final String POINT_FILENAME_PREFIX = "map-point-";

    private static final String POINT_FILENAME_EXT = ".txt";

    private static final String POINTS_DIR = "points";

    private static final String AWIPSTOOLS = "awipsTools";

    private Map<String, Coordinate> coordinates;

    private ListenerList pointsListeners = new ListenerList();

    private final Map<String, Point> points = new HashMap<String, Point>();

    private Coordinate home;

    private ListenerList homeListeners = new ListenerList();

    private Coordinate wfoCenter;

    private String site;

    private LocalizationContext userCtx;

    private LocalizationFile userToolsDir;

    private LocalizationFile pointsDir;

    private LocalizationFile prevLFile;

    private Point prevPoint;

    private int cntPointsToUpdate;

    private int cntPointsUpdated;

    private AtomicBoolean groupUpdate;

    private IPathManager pathMgr;

    public static synchronized PointsDataManager getInstance() {
        if (theManager == null) {
            theManager = new PointsDataManager();
        }
        return theManager;
    }

    private PointsDataManager() {
        cntPointsToUpdate = 0;
        cntPointsUpdated = 0;
        groupUpdate = new AtomicBoolean(false);
        site = LocalizationManager.getInstance().getCurrentSite();

        pathMgr = PathManagerFactory.getPathManager();
        userCtx = pathMgr.getContext(LocalizationType.CAVE_STATIC,
                LocalizationLevel.USER);

        pointsDir = pathMgr.getLocalizationFile(userCtx, AWIPSTOOLS
                + File.separator + site + File.separator + POINTS_DIR);
        userToolsDir = pathMgr.getLocalizationFile(userCtx, AWIPSTOOLS
                + File.separator + site);
        userToolsDir.addFileUpdatedObserver(this);

        CorePlugin.getDefault().getPreferenceStore()
                .addPropertyChangeListener(this);
    }

    /**
     * 
     * @param name
     * @return the point coordinate associated with the given name, null if no
     *         point exists by that name
     */
    public Coordinate getCoordinate(String name) {
        if (points.isEmpty()) {
            getPointsMap();
        }
        Point point = points.get(name);
        if (point == null) {
            return null;
        }
        return point.getCoordinate();
    }

    public Point getPoint(String name) {
        return getPointsMap().get(name);
    }

    public void setCoordinate(String name, Coordinate coordinate) {
        Point point = points.get(name);
        Assert.isNotNull(point, "Point not found for " + name);
        point.setCoordinate(coordinate);
        String path = getPointDirName(point);
        LocalizationFile dir = pathMgr.getLocalizationFile(userCtx, path);
        storePoint(dir, point);
    }

    public void setPoint(Point point) {
        points.put(point.getName(), point);
        String path = getPointDirName(point);
        LocalizationFile dir = pathMgr.getLocalizationFile(userCtx, path);
        storePoint(dir, point);
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

    public Collection<String> getVisiblePointNames() {
        Collection<String> visiblePoints = new ArrayList<String>();
        for (String name : getPointsMap().keySet()) {
            Point point = points.get(name);
            if (!point.isHidden() && !point.isGroup()) {
                visiblePoints.add(name);
            }
        }
        return visiblePoints;
    }

    public Collection<String> getPointNames() {
        Collection<String> pointNames = new ArrayList<String>();
        for (String key : getPointsMap().keySet()) {
            if (!points.get(key).isGroup()) {
                pointNames.add(key);
            }
        }
        return pointNames;
    }

    private void storeHome() {
        Point point = new Point("home", 0.0, 0.0, true, false, false, new RGB(
                0, 0, 0), "");
        point.setCoordinate(home);
        storePoint(pointsDir, point, HOME_LOCATION_FILE);
    }

    /**
     * Add a new Point file to the persistent store; fires an
     * ILocalizationFileObserver event
     * 
     * @param dir
     * @param point
     */
    private void storePoint(LocalizationFile dir, Point point) {

        String fileName = getPointFileName(point.getName());
        storePoint(dir, point, fileName);
    }

    /**
     * @param dir
     * @param point
     * @param fileName
     */
    private void storePoint(LocalizationFile dir, Point point, String fileName) {
        LocalizationFile lFile = pathMgr.getLocalizationFile(userCtx, dir
                .getName().trim() + File.separator + fileName);

        File file = lFile.getFile();

        // create the local directory if necessary
        if (!file.getParentFile().exists()) {
            file.getParentFile().mkdirs();
        }

        String name = point.getName().replace(' ', PointUtilities.DELIM_CHAR);
        Coordinate location = point.getCoordinate();

        boolean colorActive = point.isColorActive();
        RGB color = point.getColor();
        int r = -1;
        int g = -1;
        int b = -1;
        if (color != null) {
            r = color.red;
            g = color.green;
            b = color.blue;
        }

        boolean isHidden = point.isHidden();
        boolean isMovable = point.isMovable();

        PointSize fontSize = point.getFontSize();

        BufferedWriter out = null;

        try {
            out = new BufferedWriter(new FileWriter(file));
            out.write(String.format("%s %f %f %b %d %d %d %b %b %d\n", name,
                    location.y, location.x, colorActive, r, g, b, isHidden,
                    isMovable, fontSize.getOrdinal()));
        } catch (IOException e) {
            statusHandler.handle(Priority.PROBLEM, "Error writing to file: "
                    + file.getAbsolutePath());
        } finally {
            if (out != null) {
                try {
                    out.close();
                    lFile.save();
                    prevLFile = null;
                    prevPoint = null;
                } catch (IOException e) {
                    statusHandler.handle(Priority.PROBLEM,
                            "Error writing to file: " + file.getAbsolutePath());
                } catch (LocalizationException e) {
                    statusHandler.handle(Priority.PROBLEM,
                            "Error storing locatization file to server: "
                                    + lFile);
                }
            }
        }
    }

    /**
     * Given a Point original name (e.g. Brooklyn Bridge), return the
     * conventional file name (e.g.map-point-Brooklyn~Bridge.txt)
     * 
     * @param name
     * @return
     */
    private String getPointFileName(String name) {
        name = PointUtilities.convertSpaceToDelimiter(name);
        String fileName = POINT_FILENAME_PREFIX + name + POINT_FILENAME_EXT;
        return fileName;
    }

    public Collection<Point> getPoints() {
        return getPointsMap().values();
    }

    private Map<String, Point> getPointsMap() {
        if (points.isEmpty()) {
            loadPoints();
            String dirPath = pointsDir.getName().trim();
            String name = D2D_POINTS_GROUP.replace(' ',
                    PointUtilities.DELIM_CHAR);
            String path = dirPath + File.separator + name;
            LocalizationFile d2dDir = pathMgr
                    .getLocalizationFile(userCtx, path);
            boolean createPoints = true;
            if (d2dDir.exists()) {
                if (!d2dDir.isDirectory()) {
                    try {
                        d2dDir.delete();
                        createGroup(dirPath, name);
                    } catch (LocalizationOpFailedException e) {
                        statusHandler.handle(Priority.PROBLEM,
                                "Unable to create group: " + D2D_POINTS_GROUP);
                        createPoints = false;
                    }
                } else {
                    LocalizationFile[] files = pathMgr.listFiles(userCtx, path,
                            new String[] { POINT_FILENAME_EXT }, true, false);
                    for (LocalizationFile lf : files) {
                        if (!lf.isDirectory()) {
                            createPoints = false;
                            break;
                        }
                    }
                }
            } else {
                createGroup(dirPath, name);
            }
            if (createPoints) {
                createDefaultPoints();
            }
        }
        return points;
    }

    private void createDefaultPoints() {
        Coordinate center = getHome();
        int baseRingSize = 120;
        int startAngle = -90;
        for (char label = 'A'; label <= 'J'; label++) {
            String name = String.valueOf(label);

            Coordinate coordinate = getCoordinateOnCircle(center, baseRingSize,
                    startAngle);
            Point point = new Point(name, coordinate, false, new RGB(0, 0, 0),
                    false, true, PointSize.DEFAULT, D2D_POINTS_GROUP);
            setPoint(point);
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
        Point point = loadPoint(pointsDir, HOME_LOCATION_FILE);
        if (point == null) {
            home = getWfoCenter();
            storeHome();
        } else {
            home = point.getCoordinate();
        }
        return home;
    }

    private void loadPoints() {
        points.clear();

        String path = pointsDir.getName().trim();
        LocalizationFile[] files = pathMgr.listFiles(userCtx, path,
                new String[] { POINT_FILENAME_EXT }, true, false);
        String name = null;

        if (files.length == 0) {
            pointsDir.getFile().mkdirs();
            Point point = new GroupNode("point");
            points.put(point.getGroup(), point);
        } else {

            for (LocalizationFile lf : files) {
                if (lf.isDirectory()) {
                    prevPoint = loadPoint(lf);
                } else {
                    String fileName = lf.getFile().getName().trim();
                    if (fileName.startsWith(POINT_FILENAME_PREFIX)) {
                        Point point = loadPoint(lf);
                        name = point.getName();
                        points.put(name, point);
                    }
                }
            }
        }
    }

    /**
     * Create a Point from a file; does NOT fire an ILocalizationFileObserver
     * event.
     * 
     * @param dir
     * @param fileName
     * @return point
     */
    private Point loadPoint(LocalizationFile dir, String fileName) {

        LocalizationFile lf = pathMgr.getLocalizationFile(dir.getContext(), dir
                .getName().trim() + File.separator + fileName);
        return loadPoint(lf);
    }

    public Point loadPoint(LocalizationFile lFile) {
        if (prevLFile == lFile) {
            return prevPoint;
        }

        if (lFile.isDirectory()) {
            String key = getPointKey(lFile);
            prevPoint = points.get(key);
            if (prevPoint == null) {
                prevPoint = new GroupNode(getPointName(lFile));
                prevPoint.setGroup(key);
                points.put(key, prevPoint);
            }
            prevLFile = lFile;
            return prevPoint;
        }

        StringBuffer sb = new StringBuffer(lFile.toString());
        sb.replace(0, pointsDir.toString().length(), "");
        Point point = null;
        Coordinate coordinate = null;
        File file = lFile.getFile();
        sb.setLength(sb.length() - file.getName().length() - 1);
        String group = sb.toString();
        BufferedReader in = null;
        try {
            in = new BufferedReader(new FileReader(file));
            String line = in.readLine();
            line = line.trim();
            StringTokenizer st = new StringTokenizer(line);
            String pointName = st.nextToken();
            pointName = pointName.replace(PointUtilities.DELIM_CHAR, ' ');
            String token = st.nextToken();
            double lat = Double.parseDouble(token);
            token = st.nextToken();
            double lon = Double.parseDouble(token);

            if (lat > 90.0 || lat < -90.0 || lon > 180.0 || lon < -180.0) {
                statusHandler.handle(Priority.PROBLEM,
                        "Invalid lat/lon in wfo center point file; Ignoring file: "
                                + file.getName());
            } else {
                coordinate = new Coordinate(lon, lat);
            }
            boolean colorActive = Boolean.parseBoolean(st.nextToken());
            int red = Integer.parseInt(st.nextToken());
            int green = Integer.parseInt(st.nextToken());
            int blue = Integer.parseInt(st.nextToken());
            RGB color = new RGB(red, green, blue);
            boolean isHidden = Boolean.parseBoolean(st.nextToken());
            boolean isMovable = Boolean.parseBoolean(st.nextToken());
            int fontSizeOrdinal = Integer.parseInt(st.nextToken());
            PointSize fontSize = PointSize.getPointSize(fontSizeOrdinal);

            point = new Point(pointName, coordinate, colorActive, color,
                    isHidden, isMovable, fontSize, group);

        } catch (NumberFormatException e) {
            statusHandler.handle(Priority.PROBLEM,
                    "Invalid number in wfo center point file, using default");
        } catch (FileNotFoundException e) {
            statusHandler.handle(Priority.PROBLEM,
                    "No wfo center point file found, creating default.");
        } catch (IOException e) {
            statusHandler.handle(Priority.PROBLEM,
                    "Error reading wfo center point file, using default");
        } finally {
            if (in != null) {
                try {
                    in.close();
                } catch (IOException e) {
                    // nothing to do
                }
            }
        }

        if (point != null) {
            prevLFile = lFile;
            prevPoint = point;
        }
        return point;
    }

    private LocalizationFile getGroupDir(Point point) {
        String path = (pointsDir.getName() + point.getGroup()).replace(' ',
                PointUtilities.DELIM_CHAR);
        LocalizationFile dir = pathMgr.getLocalizationFile(userCtx, path);
        return dir;
    }

    public List<IPointNode> getChildren(IPointNode parent) {
        if (parent == null) {
            getPoints();
            return getChildrenPoints(pointsDir);
        }
        Point point = (Point) parent;
        String path = getPointDirName(point);
        LocalizationFile lFile = pathMgr.getLocalizationFile(userCtx, path);
        return getChildrenPoints(lFile);
    }

    private List<IPointNode> getChildrenPoints(LocalizationFile lFile) {
        List<IPointNode> children = new ArrayList<IPointNode>();
        LocalizationFile[] files = pathMgr.listFiles(userCtx, lFile.getName()
                .trim(), new String[] { POINT_FILENAME_EXT }, false, false);
        for (int index = 1; index < files.length; ++index) {
            LocalizationFile lf = files[index];
            Point point = null;
            point = points.get(getPointKey(lf));
            if (point != null) {
                children.add(point);
            }
        }
        sort(children);
        return children;
    }

    private void sort(List<IPointNode> nodes) {
        Collections.sort(nodes, new Comparator<IPointNode>() {

            @Override
            public int compare(IPointNode o1, IPointNode o2) {
                return o1.compareTo(o2);
            }
        });
    }

    /**
     * Given a Point file name (e.g.map-point-Brooklyn~Bridge.txt) return the
     * user's original name (e.g. Brooklyn Bridge)
     * 
     * @param point
     * @return fileName
     */
    private String getPointFilename(Point point) {
        StringBuilder sb = new StringBuilder();
        sb.append(pointsDir.getName().trim()).append(File.separator)
                .append(point.getGroup()).append(File.separator)
                .append(POINT_FILENAME_PREFIX).append(point.getName())
                .append(POINT_FILENAME_EXT);
        String name = sb.toString().replace(' ', PointUtilities.DELIM_CHAR);
        return name;
    }

    private String getPointDirName(Point point) {
        String group = point.getGroup();
        StringBuilder sb = new StringBuilder();
        sb.append(pointsDir.getName().trim());
        if (group.length() > 0) {
            if (group.charAt(0) != File.separatorChar) {
                sb.append(File.separator);
            }
            sb.append(group);
        }
        String name = sb.toString().replace(' ', PointUtilities.DELIM_CHAR);
        return name;
    }

    private String getPointName(LocalizationFile lFile) {
        String name = null;
        StringBuilder sb = new StringBuilder();
        sb.append(lFile.toString());
        sb.replace(0, sb.lastIndexOf(File.separator) + 1, "");
        if (!lFile.isDirectory()) {
            sb.replace(0, POINT_FILENAME_PREFIX.length(), "");
            sb.replace(sb.length() - POINT_FILENAME_EXT.length(), sb.length(),
                    "");
        }
        name = sb.toString().replace(PointUtilities.DELIM_CHAR, ' ');
        return name;
    }

    private String getPointKey(LocalizationFile lFile) {
        String key = null;
        if (!lFile.isDirectory()) {
            key = getPointName(lFile);
        } else {
            StringBuilder sb = new StringBuilder();
            sb.append(lFile.toString());
            sb.replace(0, pointsDir.toString().length(), "");
            key = sb.toString().replace(PointUtilities.DELIM_CHAR, ' ');
        }
        return key;
    }

    public IPointNode getParent(IPointNode node) {
        Point child = (Point) node;
        StringBuilder sb = new StringBuilder(child.getGroup());
        if (child.isGroup()) {
            if (child.getGroup().length() == 0) {
                return null;
            }
            sb.replace(sb.lastIndexOf(File.separator), sb.length(), "");
        }
        Point parent = points.get(sb.toString().replace(
                PointUtilities.DELIM_CHAR, ' '));
        return new GroupNode(parent);
    }

    /**
     * This generates a temporary name for new group node under the desired
     * parent. This can serves as a place holder in a dialog's tree structure;
     * allowing a user to type in the new name for the group or keep the
     * temporary name.
     * 
     * @param parentNode
     * @return groupNode
     * @throws LocalizationOpFailedException
     */
    public IPointNode createTempGroup(IPointNode parentNode)
            throws LocalizationOpFailedException {
        Point parent = (Point) parentNode;
        String path = getPointDirName(parent);
        StringBuilder sb = new StringBuilder(path);

        LocalizationFile[] dirs = pathMgr.listStaticFiles(sb.toString(),
                new String[0], false, false);
        sb.append(File.separator).append(GROUP_TEMP_PREFIX);
        int end = sb.length();

        List<String> names = new ArrayList<String>();
        for (LocalizationFile lf : dirs) {
            names.add(lf.getName().trim());
        }

        int cnt = 0;
        while (true) {
            sb.setLength(end);
            sb.append(++cnt);
            if (!names.contains(sb.toString())) {
                break;
            }
        }
        String name = GROUP_TEMP_PREFIX + cnt;
        return createGroup(path, name);
    }

    private void moveGroup(IPointNode srcNode, IPointNode destNode) {
        Point srcPoint = (Point) srcNode;
        Point destPoint = (Point) destNode;
        String srcPath = getPointDirName(srcPoint);
        String destPath = getPointDirName(destPoint);
        String destGroupName = destPoint.getGroup();
        LocalizationFile[] files = pathMgr.listFiles(userCtx, srcPath,
                new String[] { POINT_FILENAME_EXT }, false, false);
        Point point = null;
        for (int index = 1; index < files.length; ++index) {
            LocalizationFile lf = files[index];
            point = loadPoint(lf);
            if (lf.isDirectory()) {
                IPointNode child = createGroup(destPath, point.getName());
                moveGroup(point, child);
            } else {
                ++cntPointsToUpdate;
                // System.out.println("moveGroup delete point \""
                // + point.getName() + "\" cntPointsToUpdate "
                // + cntPointsToUpdate);
                deletePoint(point);
                point.setGroup(destGroupName);
                ++cntPointsToUpdate;
                // System.out.println("moveGroup add point \"" + point.getName()
                // + "\" cntPointsToUpdate " + cntPointsToUpdate);
                addPoint(point);
            }
        }
        ++cntPointsToUpdate;
        // System.out.println("moveGroup delete group \"" + srcPoint.getName()
        // + "\" cntPointsToUpdate " + cntPointsToUpdate);
        deletePoint(srcPoint);
        firePointChangeListeners();
    }

    public boolean renameGroup(IPointNode srcNode, String destName) {
        if (srcNode.getName().equals(destName)) {
            return false;
        }

        Point parent = (Point) getParent(srcNode);
        String path = getPointDirName(parent);
        IPointNode destNode = createGroup(path, destName);

        if (destNode == null) {
            return false;
        }

        synchronized (groupUpdate) {
            groupUpdate.set(true);
            moveGroup(srcNode, destNode);
            waitForUpdate();
            groupUpdate.set(false);
            // System.out.print("renameGroup=>");
            // System.out.flush();
            firePointChangeListeners();
        }
        return true;
    }

    /**
     * Create a new group
     * 
     * @param parent
     *            group to add new group to
     * @param name
     *            of the new group.
     * @return node The new group or null if group already exists.
     */
    private IPointNode createGroup(String parent, String name) {
        StringBuilder sb = new StringBuilder();
        sb.append(parent);
        sb.replace(0, pointsDir.getName().trim().length(), "");
        sb.append(File.separator).append(name);
        Point gPoint = new GroupNode(name);
        gPoint.setGroup(sb.toString().replace(PointUtilities.DELIM_CHAR, ' '));
        String groupPath = getPointDirName(gPoint);
        LocalizationFile lFile = pathMgr
                .getLocalizationFile(userCtx, groupPath);

        // lFile.exits() can return true even when directory doesn't exist.
        // Get a list of the parent's sub-directories and see if group's
        // directory is in it.
        LocalizationFile[] files = pathMgr.listFiles(userCtx, parent,
                new String[] {}, false, false);
        for (LocalizationFile lf : files) {
            if (groupPath.equals(lf.getName().trim())) {
                return null;
            }
        }

        try {
            // Must create a file in the directory to force its creation.
            String p = lFile.getName().trim() + File.separator + GROUP_INFO;
            LocalizationFile lf = pathMgr.getLocalizationFile(userCtx, p);
            File f = lf.getFile();
            BufferedWriter out = null;
            try {
                out = new BufferedWriter(new FileWriter(f));
                out.write("0");
                out.close();
                lf.save();
            } catch (IOException e) {
                statusHandler.handle(Priority.PROBLEM,
                        "Error writing to file: " + f.getAbsolutePath());
            } finally {
                if (out != null) {
                    try {
                        out.close();
                    } catch (IOException e) {
                        // Ignore
                    }
                    lf.save();
                }
            }
            String key = gPoint.getGroup();
            points.put(key, gPoint);
        } catch (LocalizationException e) {
            statusHandler.handle(Priority.PROBLEM,
                    "Unable to create the group: " + gPoint.getGroup(), e);
            return null;
        }
        return new GroupNode(gPoint);
    }

    public boolean exists(String name) {
        return getPointNames().contains(name);
    }

    /**
     * Obtains a sorted list of groups
     * 
     * @return groups
     */
    public String[] getGroupList() {
        LocalizationFile[] files = pathMgr.listFiles(userCtx, pointsDir
                .getName().trim(), new String[] {}, true, false);
        String[] groups = new String[files.length - 1];
        int start = files[0].getName().trim().length();
        for (int index = 1; index < files.length; ++index) {
            LocalizationFile lf = files[index];
            groups[index - 1] = lf.getName().trim().substring(start)
                    .replace(PointUtilities.DELIM_CHAR, ' ');
        }
        Arrays.sort(groups);
        return groups;
    }

    private void loadCoordinates() {
        coordinates = new HashMap<String, Coordinate>();

        LocalizationFile[] files = pathMgr.listFiles(userToolsDir.getContext(),
                userToolsDir.getName(), new String[] { MOVABLE_POINT_EXT },
                false, true);
        for (LocalizationFile lf : files) {
            String fileName = lf.getFile().getName();
            if (fileName.startsWith(MOVABLE_POINT_PREFIX)) {
                Coordinate point = loadCoordinate(userToolsDir, fileName);
                String name = fileName.substring(MOVABLE_POINT_PREFIX.length())
                        .replace(MOVABLE_POINT_EXT, "");
                coordinates.put(name, point);
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

        if (fileName.startsWith(POINT_FILENAME_PREFIX)) {
            pointsFileUpdated(message.getFileName(), message.getChangeType());
        } else if (fileName.startsWith(HOME_LOCATION_FILE)) {
            homeLocationFileUpdated(fileName);
        }
    }

    private void pointsFileUpdated(String filename, FileChangeType type) {
        LocalizationFile lFile = pathMgr.getLocalizationFile(userCtx, filename);

        if (type == FileChangeType.DELETED) {
            // System.out.println("pointsFileUpdated remove: " + filename);
            points.remove(getPointKey(lFile));
        } else {
            Point point = loadPoint(lFile);
            // System.out.println("pointsFileUpdated: " + point.getName());
            points.put(point.getName(), point);
        }
        firePointChangeListeners();
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

    private Coordinate loadCoordinate(LocalizationFile dir, String fileName) {
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

    private void firePointChangeListeners() {
        // Wait for group update to finish
        // if (groupCnt.get() == 0) {
        if (groupUpdate.compareAndSet(false, false)) {
            // System.out.println("firePointChangeListeners notify listeners ");
            for (Object listener : pointsListeners.getListeners()) {
                ((IPointChangedListener) listener).pointChanged();
            }
            // }
        } else {
            synchronized (groupUpdate) {
                ++cntPointsUpdated;
                // System.out
                // .println("firePointChangeListeners cntPointsUpdated: "
                // + cntPointsUpdated);
                try {
                    groupUpdate.notify();
                } catch (IllegalMonitorStateException ex) {
                    ex.printStackTrace();
                }
            }
        }
    }

    /**
     * Try to add a new Point to the persistent store; the boolean flag just
     * let's user know e.g. if Point was a duplicate. The server will return
     * asynchronously via ILocalizationFileObserver::fileUpdated and a
     * FileUpdatedMessage status of ADDED if file was successfully created.
     * 
     * @param point
     * @return returns true if point was successfully added, false otherwise,
     *         for example when a duplicate point name exists and forceOverwrite
     *         was false
     */
    public boolean addPoint(Point point) {
        boolean pointAdded;
        String name = point.getName();
        name = PointUtilities.trimAll(name);
        Point foundPoint = getPoint(name);
        // already exists?
        if (foundPoint != null) {
            pointAdded = false;
        } else {
            pointAdded = true;
        }
        LocalizationFile dir = getGroupDir(point);
        storePoint(dir, point);
        points.put(point.getName(), point);
        return pointAdded;
    }

    /**
     * Try to delete a Point from the persistent store; the boolean flag just
     * let's user know of (otherwise impossible?) situation where the Point
     * somehow disappeared; the server will return asynchronously via
     * ILocalizationFileObserver::fileUpdated and a FileUpdatedMessage status of
     * DELETE if file removal is successful.
     * 
     * @param point
     * @return
     */
    public boolean deletePoint(Point point) {
        boolean pointRemoved = false;
        String key = null;
        if (point.isGroup()) {
            key = point.getGroup();
        } else {
            key = point.getName();
        }

        Point foundPoint = getPoint(key);
        if (foundPoint != null) {
            if (foundPoint.isGroup()) {
                List<IPointNode> children = getChildren(foundPoint);
                for (IPointNode child : children) {
                    deletePoint((Point) child);
                }
                // System.out.println("Delete group " + foundPoint.getGroup());
                pointRemoved = removePoint(foundPoint);
                points.remove(key);
                firePointChangeListeners();
            } else {
                // System.out.println("deletePoint found: " + point.getName());
                pointRemoved = removePoint(foundPoint);
                points.remove(key);
            }
        }
        return pointRemoved;
    }

    /***
     * Try to remove a Point file from the persistent store; fires an
     * ILocalizationFileObserver event.
     * 
     * @param m
     * @return
     */
    private boolean removePoint(Point point) {
        LocalizationFile lFile = null;
        if (point.isGroup()) {
            lFile = getGroupDir(point);
            String path = lFile.getName().trim() + File.separator + GROUP_INFO;
            LocalizationFile lf = pathMgr.getLocalizationFile(userCtx, path);
            try {
                lf.delete();
            } catch (LocalizationOpFailedException e) {
                // ignore
            }
        } else {
            String name = getPointFilename(point);
            lFile = pathMgr.getLocalizationFile(userCtx, name);
        }
        try {
            lFile.delete();
        } catch (LocalizationOpFailedException e1) {
            statusHandler.handle(Priority.PROBLEM,
                    "Error deleting locatization file from server: " + lFile);
        }
        return true;
    }

    public void updatePoint(Point point) throws PointNameChangeException {
        Point oldPoint = getPoint(point.getName());
        if (oldPoint == null) {
            throw new PointNameChangeException("Point does not exist");
        }
        LocalizationFile dir = getGroupDir(point);
        storePoint(dir, point);
    }

    public void updateChildrenHidden(IPointNode node, boolean state) {
        if (!node.isGroup()) {
            return;
        }

        synchronized (groupUpdate) {
            groupUpdate.set(true);
            doChildrenHidden(node, state);
            waitForUpdate();
            groupUpdate.set(false);
            // System.out.print("updateChildrenHidden==>");
            // System.out.flush();
            firePointChangeListeners();
        }
    }

    private void doChildrenHidden(IPointNode node, boolean state) {
        String path = getPointDirName((Point) node);
        LocalizationFile[] files = pathMgr.listFiles(userCtx, path,
                new String[] { POINT_FILENAME_EXT }, true, false);
        Point point = null;
        for (LocalizationFile lf : files) {
            point = loadPoint(lf);
            boolean oldState = point.isHidden();
            point.setHidden(state);
            if (!lf.isDirectory() && state != oldState) {
                try {
                    ++cntPointsToUpdate;
                    // System.out.println("update point: " + point.getName()
                    // + ", cntPointsToUpdate: " + cntPointsToUpdate);
                    updatePoint(point);
                } catch (PointNameChangeException e) {
                    statusHandler.handle(
                            Priority.PROBLEM,
                            "Unable up point's hidden state: "
                                    + point.getName());
                }
            }
        }
    }

    public void updateChildrenMovable(IPointNode node, boolean state) {
        if (!node.isGroup()) {
            return;
        }

        synchronized (groupUpdate) {
            groupUpdate.set(true);
            doChildrenMovable(node, state);
            waitForUpdate();
            groupUpdate.set(false);
            // System.out.print("updateChildrenMoveable==>");
            // System.out.flush();
            firePointChangeListeners();
        }
    }

    private void doChildrenMovable(IPointNode node, boolean state) {
        String path = getPointDirName((Point) node);
        LocalizationFile[] files = pathMgr.listFiles(userCtx, path,
                new String[] { POINT_FILENAME_EXT }, true, false);
        Point point = null;
        for (LocalizationFile lf : files) {
            point = loadPoint(lf);
            boolean oldState = point.isMovable();
            point.setMovable(state);
            if (!lf.isDirectory() && oldState != state) {
                try {
                    ++cntPointsToUpdate;
                    updatePoint(point);
                } catch (PointNameChangeException e) {
                    statusHandler.handle(
                            Priority.PROBLEM,
                            "Unable up point's movable state: "
                                    + point.getName());
                }
            }
        }
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

    private void waitForUpdate() {
        int cntDown = 5;
        int oldUpdated = cntPointsUpdated;
        while (cntPointsToUpdate != cntPointsUpdated && cntDown != 0) {
            try {
                groupUpdate.wait(100L);
                if (oldUpdated == cntPointsToUpdate) {
                    --cntDown;
                } else {
                    oldUpdated = cntPointsUpdated;
                }
            } catch (InterruptedException e) {
                statusHandler.handle(Priority.PROBLEM, e.getLocalizedMessage(),
                        e);
            }
        }
        if (cntDown == 0) {
            statusHandler.debug("waitForUpdate count down hit zero");
        }
        cntPointsToUpdate = 0;
        cntPointsUpdated = 0;
    }
}
