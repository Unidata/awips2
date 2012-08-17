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

import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.Comparator;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.concurrent.atomic.AtomicBoolean;

import javax.measure.converter.UnitConverter;
import javax.measure.unit.NonSI;
import javax.measure.unit.SI;
import javax.xml.bind.JAXB;

import org.eclipse.core.runtime.Assert;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.ListenerList;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.jobs.Job;
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

    private static final String HOME_LOCATION_FILE = "HomeLocation.dat";

    private static final String GROUP_INFO = "group.dat";

    private static final String GROUP_TEMP_PREFIX = "TEMP_";

    private static final String D2D_POINTS_GROUP = "D2D Points";

    private static final String POINT_FILENAME_PREFIX = "map-point-";

    private static final String POINT_FILENAME_EXT = ".txt";

    private static final String POINTS_DIR = "points";

    private static final String AWIPSTOOLS = "awipsTools";

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

    /**
     * Prior to deleting a group's directory we must delete all points and
     * sub-groups and finally the GROUP_INFO file. The order the file names are
     * received in a message to fileUpdatedFile() may not be in the same order
     * the deletes are sent. This mapping allows tracking the pending deletes so
     * we know when the directory can be deleted. The String is the group's key
     * and the list is the files and sub-directories that need to be deleted.
     * when a message comes in for a given file it is removed from the list.
     * When the list is empty the group's directory is deleted.
     */
    private Map<String, List<String>> pendingDeletes;

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

        pendingDeletes = new HashMap<String, List<String>>();

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

        try {
            marshalPointToXmlFile(point, lFile);
            prevLFile = null;
            prevPoint = null;
        } catch (Exception e) {
            statusHandler.handle(Priority.PROBLEM, e.getLocalizedMessage(), e);
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
        LocalizationFile lFile = pathMgr.getLocalizationFile(
                pointsDir.getContext(), pointsDir.getName().trim()
                        + File.separator + HOME_LOCATION_FILE);
        Point point = null;
        if (lFile.exists()) {
            point = loadPoint(pointsDir, HOME_LOCATION_FILE);
        }

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

        if (files.length == 0) {
            pointsDir.getFile().mkdirs();
            Point point = new GroupNode("point");
            put(point.getGroup(), point);
        } else {

            for (LocalizationFile lf : files) {
                prevPoint = loadPoint(lf);
                put(getPointKey(lf), prevPoint);
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

    private Point loadPoint(LocalizationFile lFile) {
        if (prevLFile == lFile) {
            return prevPoint;
        }

        if (lFile.isDirectory()) {
            String key = getPointKey(lFile);
            prevPoint = points.get(key);
            if (prevPoint == null) {
                prevPoint = new GroupNode(getPointName(lFile));
                prevPoint.setGroup(key);
            }
            prevLFile = lFile;
            return prevPoint;
        }

        Point point = null;
        try {
            point = unmarshalPointFromXmlFile(lFile);
        } catch (IOException ex) {
            StringBuffer sb = new StringBuffer(lFile.toString());
            sb.replace(0, pointsDir.toString().length(), "");
            int index = sb.lastIndexOf(File.separator);
            sb.setLength(index);
            point.setGroup(sb.toString());

            statusHandler.handle(Priority.PROBLEM,
                    "Unable to open localized file: " + lFile, ex);
        } catch (LocalizationException e) {
            statusHandler.handle(Priority.PROBLEM, e.getLocalizedMessage(), e);
        }

        if (point != null) {
            prevLFile = lFile;
            prevPoint = point;
            StringBuffer sb = new StringBuffer(lFile.toString());
            sb.replace(0, pointsDir.toString().length(), "");
            int index = sb.lastIndexOf(File.separator);
            sb.setLength(index);
            point.setGroup(sb.toString());
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
        sb.append(pointsDir.getName().trim());
        String group = point.getGroup();
        if (!group.startsWith(File.separator)) {
            sb.append(File.separator);
        }
        sb.append(point.getGroup()).append(File.separator)
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

    /**
     * @param node
     * @return
     */
    public IPointNode getParent(IPointNode node) {
        Point child = (Point) node;
        StringBuilder sb = new StringBuilder(child.getGroup());
        if (child.isGroup()) {
            // In a group node the getGroup() returns the full path to the
            // group. It is used as the key to retrieve the node from the
            // points map. Thus to get the parent key for the group node,
            // /Towers/alt/sub, you strip off the last part of the node's
            // key to get /Towers/alt. If the nodes key is empty this is the
            // root node and there is no parent.
            if (sb.length() > 0) {
                sb.replace(sb.lastIndexOf(File.separator), sb.length(), "");
            }
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

    private void doMoveGroup(IPointNode srcNode, IPointNode destNode) {
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
                doMoveGroup(point, child);
            } else {
                ++cntPointsToUpdate;
                doDeletePoint(point);
                point.setGroup(destGroupName);
                doAddPoint(point);
            }
        }
    }

    public void moveNode(final IPointNode node, final IPointNode destNode) {
        if (!node.isGroup()) {
            if (getParent(node).compareTo(destNode) != 0) {
                synchronized (groupUpdate) {
                    groupUpdate.set(true);
                    Point point = (Point) node;
                    ++cntPointsToUpdate;
                    doDeletePoint(point);
                    point.setGroup(destNode.getGroup());
                    doAddPoint(point);
                    waitForUpdate();
                    groupUpdate.set(false);
                    firePointChangeListeners();
                }
            }
        } else {
            Job job = new Job("moveNode Group") {

                @Override
                protected IStatus run(IProgressMonitor monitor) {
                    synchronized (groupUpdate) {
                        groupUpdate.set(true);
                        String parentPath = getPointDirName((Point) destNode);
                        IPointNode destGroup = createGroup(parentPath,
                                node.getName());
                        doMoveGroup(node, destGroup);
                        doDeletePoint((Point) node);
                        waitForUpdate();
                        firePointChangeListeners();
                        groupUpdate.set(false);
                    }
                    return Status.OK_STATUS;
                }
            };
            job.schedule();
        }
    }

    public boolean renameGroup(final IPointNode srcNode, final String destName) {
        if (srcNode.getName().equals(destName)) {
            return false;
        }

        Job job = new Job("RENAME_GROUP") {

            @Override
            protected IStatus run(IProgressMonitor monitor) {
                IPointNode parent = getParent(srcNode);
                String path = getPointDirName((Point) parent);
                synchronized (groupUpdate) {
                    groupUpdate.set(true);
                    IPointNode destNode = createGroup(path, destName);

                    if (destNode != null) {
                        doMoveGroup(srcNode, destNode);
                        doDeletePoint((Point) srcNode);
                        waitForUpdate();
                        firePointChangeListeners();
                        groupUpdate.set(false);
                    }
                    return Status.OK_STATUS;
                }
            }

        };
        job.schedule();
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
            OutputStream outStream = lf.openOutputStream();
            outStream.write(gPoint.getGroup().getBytes());
            outStream.close();
            lf.save();
            if (groupUpdate.get()) {
                ++cntPointsToUpdate;
            }
        } catch (LocalizationException e) {
            statusHandler.handle(Priority.PROBLEM,
                    "Unable to create the group: " + gPoint.getGroup(), e);
            return null;
        } catch (IOException e) {
            statusHandler.handle(Priority.PROBLEM, e.getLocalizedMessage(), e);
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

        if (fileName.startsWith(HOME_LOCATION_FILE)) {
            homeLocationFileUpdated(fileName);
            return;
        }

        synchronized (groupUpdate) {
            if (groupUpdate.get()) {
                ++cntPointsUpdated;
                pointsFileUpdated(message);
                try {
                    // Let waitForUpdate know we updated the count.
                    groupUpdate.notify();
                } catch (IllegalMonitorStateException e) {
                    // Ignore
                }
                return;
            }
        }

        pointsFileUpdated(message);
        firePointChangeListeners();
    }

    private void checkDelete(String filename) {
        if (!filename.endsWith(GROUP_INFO)) {
            LocalizationFile lFile = pathMgr.getLocalizationFile(userCtx,
                    filename);
            remove(getPointKey(lFile));
        }

        // Decouple keys set from the mapping to allow removal without throwing
        // ConcurrentModificationException.
        List<String> keys = new ArrayList<String>(pendingDeletes.keySet());
        for (String key : keys) {
            List<String> pendList = pendingDeletes.get(key);

            if (pendList.contains(filename)) {
                pendList.remove(filename);
                // The group's directory is now empty and can be removed.
                if (pendList.size() == 0) {
                    pendingDeletes.remove(key);
                    removePoint(points.get(key));
                }
                return;
            }
        }
    }

    private void pointsFileUpdated(FileUpdatedMessage message) {
        String filename = message.getFileName();
        LocalizationFile lFile = pathMgr.getLocalizationFile(userCtx, filename);
        if (!lFile.getName().startsWith(pointsDir.getName())) {
            return;
        }

        FileChangeType type = message.getChangeType();

        if (filename.endsWith(GROUP_INFO)) {
            if (type == FileChangeType.DELETED) {
                checkDelete(filename);
            } else if (type == FileChangeType.ADDED) {
                String groupFilename = filename.substring(0,
                        filename.lastIndexOf(File.separatorChar));
                LocalizationFile glFile = pathMgr.getLocalizationFile(userCtx,
                        groupFilename);
                Point point = loadPoint(glFile);
                put(getPointKey(glFile), point);
            }
        } else {

            if (type == FileChangeType.DELETED) {
                checkDelete(filename);
            } else {
                Point point = loadPoint(lFile);
                put(getPointKey(lFile), point);
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
                Job job = new Job("Home Location Update") {

                    @Override
                    protected IStatus run(IProgressMonitor monitor) {
                        ((IPointChangedListener) listener).pointChanged();
                        return Status.OK_STATUS;
                    }

                };
                job.schedule();
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
        for (final Object listener : pointsListeners.getListeners()) {
            // fire listeners in separate threads to avoid waiting to draw
            // the updated location while waiting on another listener to
            // finish
            Job job = new Job("Point Changed") {

                @Override
                protected IStatus run(IProgressMonitor monitor) {
                    ((IPointChangedListener) listener).pointChanged();
                    return Status.OK_STATUS;
                }

            };
            job.schedule();
        }
    }

    public void addPoint(final Point point) {
        Assert.isTrue(!point.isGroup());

        Job job = new Job("ADD POINT") {
            @Override
            public IStatus run(IProgressMonitor monitor) {
                synchronized (groupUpdate) {
                    groupUpdate.set(true);
                    String name = point.getName();
                    name = PointUtilities.trimAll(name);
                    Point foundPoint = getPoint(name);
                    if (foundPoint != null
                            && !foundPoint.getGroup().equals(point.getGroup())) {
                        ++cntPointsToUpdate;
                        doDeletePoint(foundPoint);
                    }
                    doAddPoint(point);
                    waitForUpdate();
                    firePointChangeListeners();
                    groupUpdate.set(false);
                }
                return Status.OK_STATUS;
            }
        };
        job.schedule();
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
    private boolean doAddPoint(Point point) {
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
        if (groupUpdate.get()) {
            ++cntPointsToUpdate;
        }
        storePoint(dir, point);
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
    public void deletePoint(final Point point) {
        if (!point.isGroup()) {
            doDeletePoint(point);
            return;
        }

        Job job = new Job("DELETE_GROUP") {

            @Override
            protected IStatus run(IProgressMonitor monitor) {
                synchronized (groupUpdate) {
                    groupUpdate.set(true);
                    ++cntPointsToUpdate;
                    doDeletePoint(point);
                    waitForUpdate();
                    firePointChangeListeners();
                    groupUpdate.set(false);
                }
                return Status.OK_STATUS;
            }
        };
        job.schedule();
    }

    private void doDeletePoint(Point point) {
        String key = null;
        if (point.isGroup()) {
            key = point.getGroup();
        } else {
            key = point.getName();
        }

        Point foundPoint = getPoint(key);
        if (foundPoint != null) {
            if (!foundPoint.isGroup()) {
                removePoint(foundPoint);
            } else {
                // When removing a group node we need to delete the directory
                // that represents the node. That can not be done until all
                // entries in the directory are removed. This determines
                // what we need to have deleted and adds the list to the
                // pendingDeletes map so the fileUpdated() method can determine
                // when it is safe to remove the directory..
                List<String> deleteList = new ArrayList<String>();
                String group = getPointDirName(foundPoint) + File.separator
                        + GROUP_INFO;
                LocalizationFile gplf = pathMgr.getLocalizationFile(userCtx,
                        group);
                if (gplf.exists()) {
                    ++cntPointsToUpdate;
                    deleteList.add(group);
                }
                List<IPointNode> children = getChildren(foundPoint);
                for (IPointNode child : children) {
                    Point childPoint = (Point) child;
                    ++cntPointsToUpdate;
                    if (child.isGroup()) {
                        deleteList.add(getPointDirName(childPoint));
                    } else {
                        deleteList.add(getPointFilename(childPoint));
                    }
                }

                if (deleteList.size() > 0) {
                    // Delete children and wait for verification before
                    // deleting the directory.
                    pendingDeletes.put(key, deleteList);
                    for (IPointNode child : children) {
                        doDeletePoint((Point) child);
                    }

                    if (gplf.exists()) {
                        try {
                            gplf.delete();
                        } catch (LocalizationOpFailedException e) {
                            statusHandler.handle(Priority.PROBLEM,
                                    "unable to delete: " + group);
                        }
                    }
                } else {
                    // Directory is empty and safe to delete.
                    String dirName = getPointDirName(foundPoint);
                    LocalizationFile lf = pathMgr.getLocalizationFile(userCtx,
                            dirName);
                    try {
                        lf.delete();
                    } catch (LocalizationOpFailedException e) {
                        statusHandler.error("Unable to remove: " + group);
                    }
                }
            }
        }
    }

    /***
     * Try to remove a Point file from the persistent store; fires an
     * ILocalizationFileObserver event.
     * 
     * @param m
     * @return
     */
    private void removePoint(Point point) {
        LocalizationFile lFile = null;
        if (point.isGroup()) {
            lFile = getGroupDir(point);
        } else {
            String name = getPointFilename(point);
            lFile = pathMgr.getLocalizationFile(userCtx, name);
        }
        try {
            if (!lFile.delete()) {
                statusHandler.error("Unable to remove file: " + lFile);
            }
        } catch (LocalizationOpFailedException e1) {
            statusHandler.handle(Priority.PROBLEM,
                    "Error deleting locatization file from server: " + lFile);
        }
    }

    public void updatePoint(final Point oldPoint, final Point newPoint) {
        Job job = new Job("POINT UPDATE") {

            @Override
            protected IStatus run(IProgressMonitor monitor) {
                synchronized (groupUpdate) {
                    groupUpdate.set(true);
                    if (!oldPoint.getName().equals(newPoint.getName())
                            || !oldPoint.getGroup().equals(newPoint.getGroup())) {
                        ++cntPointsToUpdate;
                        doDeletePoint(oldPoint);
                    }
                    doAddPoint(newPoint);
                    waitForUpdate();
                    firePointChangeListeners();
                    groupUpdate.set(false);
                }
                return Status.OK_STATUS;
            }

        };
        job.schedule();
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
            if (state != oldState) {
                if (lf.isDirectory()) {
                    put(getPointKey(lf), point);
                } else {
                    try {
                        ++cntPointsToUpdate;
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
            if (oldState != state) {
                if (lf.isDirectory()) {
                    put(getPointKey(lf), point);
                } else {
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
    }

    /**
     * This method is used to catch a number of calls to fileUpdated without
     * calling the firePointChangedListeners. This allows moving, updating, and
     * deleting of several points all at once.
     * <p>
     * This method should only be called when in a synchronized block for
     * groupUpdate and groupUpdate is set to true. It is assumed that
     * cntPointsToUpdate is the number of times we need to wait for fileUpdated
     * to be called. fileUpdated will increment cntPointsUpdated and perform a
     * notify. This will return once the cntPointsUpdated equals
     * cntPointsToUpdate.
     * </p>
     * <p>
     * It is the responsibility of the calling method to perform a
     * firePointChangeListeners to update displays.
     * </p>
     */
    private void waitForUpdate() {
        if (cntPointsToUpdate == 0) {
            // Nothing to wait for
            cntPointsUpdated = 0;
            return;
        }

        // Since we cannot fully control what is happening with localized files
        // and directories the cntDown allows up to break out of the loop if no
        // update is received in 0.5 seconds. Otherwise this could freeze the
        // display.
        int cntDown = 5;
        int oldUpdated = cntPointsUpdated;
        while (cntPointsToUpdate != cntPointsUpdated && cntDown != 0) {
            try {
                groupUpdate.wait(100L);
                if (oldUpdated == cntPointsUpdated) {
                    --cntDown;
                } else {
                    oldUpdated = cntPointsUpdated;
                    cntDown = 5;
                }
            } catch (InterruptedException e) {
                statusHandler.handle(Priority.PROBLEM, e.getLocalizedMessage(),
                        e);
            }
        }

        cntPointsToUpdate = 0;
        cntPointsUpdated = 0;
    }

    /**
     * Use JAXB to convert a point to xml and save it in the localized file.
     * 
     * @param point
     * @param lFile
     * @throws LocalizationException
     * @throws IOException
     */
    private void marshalPointToXmlFile(Point point, LocalizationFile lFile)
            throws LocalizationException, IOException {
        OutputStream stream = lFile.openOutputStream();
        JAXB.marshal(point, stream);
        stream.close();
        lFile.save();
    }

    /**
     * Use JAXB to read the xml in the localized file and convert to a point.
     * 
     * @param lFile
     * @return point
     * @throws LocalizationException
     * @throws IOException
     */
    private Point unmarshalPointFromXmlFile(LocalizationFile lFile)
            throws LocalizationException, IOException {
        InputStream stream = lFile.openInputStream();
        Point point = JAXB.unmarshal(stream, Point.class);
        stream.close();
        return point;
    }

    private void put(String key, Point point) {
        points.put(key, point);
    }

    private Point remove(String key) {
        return points.remove(key);
    }
}
