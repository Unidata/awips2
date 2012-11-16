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
import java.util.LinkedList;
import java.util.List;
import java.util.Map;

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
import com.raytheon.uf.viz.points.PointRequest.RequestType;
import com.raytheon.uf.viz.points.data.GroupNode;
import com.raytheon.uf.viz.points.data.IPointNode;
import com.raytheon.uf.viz.points.data.Point;
import com.raytheon.uf.viz.points.data.PointFieldState;
import com.raytheon.uf.viz.points.data.PointNameChangeException;
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

public class PointsDataManager implements ILocalizationFileObserver {
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

    private static final String ROOT_NODE_KEY = "";

    private static final String AWIPSTOOLS = "awipsTools";

    private ListenerList pointsListeners = new ListenerList();

    private final Map<String, Point> points = new HashMap<String, Point>();

    private Coordinate home;

    private final ListenerList homeListeners = new ListenerList();

    private Coordinate wfoCenter;

    private String site;

    private final LocalizationContext userCtx;

    private final LocalizationFile userToolsDir;

    private final LocalizationFile pointsDir;

    private IPathManager pathMgr;

    private LinkedList<PointRequest> requestList;

    private final Map<String, List<String>> childrenKeyMap;

    /**
     * Prior to deleting a group's directory we must get the acknowledgment that
     * all its children have been deleted otherwise the delete of the directory
     * may fail. The key is the directory name to delete and the list is the
     * files in the directory that we need to get acknowledgment they have been
     * deleted.
     */
    private final Map<String, List<String>> groupDeleteMap;

    /**
     * Job to handle performing changes to the Localization files to maintain
     * points and groups.
     */
    private Job processRequestJob;

    public static synchronized PointsDataManager getInstance() {
        if (theManager == null) {
            theManager = new PointsDataManager();
        }
        return theManager;
    }

    /**
     * Private constructor to maintain singleton instance.
     */
    private PointsDataManager() {
        site = LocalizationManager.getInstance().getCurrentSite();

        pathMgr = PathManagerFactory.getPathManager();
        userCtx = pathMgr.getContext(LocalizationType.CAVE_STATIC,
                LocalizationLevel.USER);

        pointsDir = pathMgr.getLocalizationFile(userCtx, AWIPSTOOLS
                + File.separator + site + File.separator + POINTS_DIR);
        userToolsDir = pathMgr.getLocalizationFile(userCtx, AWIPSTOOLS
                + File.separator + site);
        userToolsDir.addFileUpdatedObserver(this);

        childrenKeyMap = new HashMap<String, List<String>>();

        groupDeleteMap = new HashMap<String, List<String>>();

        requestList = new LinkedList<PointRequest>();

    }

    /**
     * Queue a request to be perform latter by processRequests.
     * 
     * @param request
     */
    private void queueRequest(PointRequest request) {
        synchronized (requestList) {
            requestList.add(request);
        }
    }

    /**
     * This starts a job to process the request queue.
     */
    private void processRequests() {
        firePointChangeListeners();

        if (processRequestJob == null) {
            processRequestJob = new Job("Point Requests") {

                @Override
                protected IStatus run(IProgressMonitor monitor) {
                    PointRequest request = null;
                    while (true) {
                        synchronized (requestList) {
                            if (requestList.isEmpty()) {
                                processRequestJob = null;
                                break;
                            }
                            request = requestList.remove();
                        }

                        Point point = (Point) request.getPoint();
                        switch (request.getType()) {
                        case ADD:
                            if (!point.isGroup()) {
                                storePoint(point);
                            } else {
                                Point parent = points.get(getParentKey(point));
                                String parentPath = getPointDirName(parent);
                                createGroup(parentPath, point.getName());
                            }
                            break;
                        case UPDATE:
                            Assert.isTrue(!point.isGroup());
                            storePoint(point);
                            break;
                        case DELETE:
                            removePoint(point);
                            break;
                        default:
                            statusHandler.handle(Priority.ERROR,
                                    "Unknown PointChangeType");
                        }
                    }
                    return Status.OK_STATUS;
                }
            };
            processRequestJob.schedule();
        }
    }

    /**
     * 
     * @param name
     * @return the point coordinate associated with the given name, null if no
     *         point exists by that name
     */
    public Coordinate getCoordinate(String name) {
        Point point = getPointsMap().get(name);
        if (point == null) {
            return null;
        }
        return point.getCoordinate();
    }

    /**
     * Obtain the point for the given name.
     * 
     * @param name
     * @return point
     */
    public Point getPoint(String name) {
        return getPointsMap().get(name);
    }

    /**
     * Change the coordinate for the point associated with the name.
     * 
     * @param name
     * @param coordinate
     */
    public void setCoordinate(String name, Coordinate coordinate) {
        Point point = getPointsMap().get(name);
        Assert.isNotNull(point, "Point not found for " + name);
        point.setCoordinate(coordinate);
        PointRequest request = new PointRequest(RequestType.UPDATE, point);
        queueRequest(request);
        processRequests();
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

    /**
     * Obtain the home point's coordinate.
     * 
     * @return coordinate.
     */
    public Coordinate getHome() {
        if (home == null) {
            loadHome();
        }
        return new Coordinate(home);
    }

    /**
     * Update and save the home point's new coordinate.
     * 
     * @param home
     */
    public void setHome(Coordinate home) {
        if (home == null) {
            return;
        }
        this.home = new Coordinate(home);
        storeHome();
    }

    /**
     * Obtain a list of all points that are visible.
     * 
     * @return visiblePoints
     */
    public Collection<String> getVisiblePointNames() {
        Collection<String> visiblePoints = new ArrayList<String>();
        for (String name : getPointsMap().keySet()) {
            Point point = points.get(name);
            if (!point.isGroup() && point.getHidden() == PointFieldState.FALSE) {
                visiblePoints.add(name);
            }
        }
        return visiblePoints;
    }

    /**
     * Get a list of all point names (no groups).
     * 
     * @return pointNames
     */
    public Collection<String> getPointNames() {
        Collection<String> pointNames = new ArrayList<String>();
        for (String key : getPointsMap().keySet()) {
            if (!points.get(key).isGroup()) {
                pointNames.add(key);
            }
        }
        return pointNames;
    }

    /**
     * Store home coordinates in the root directory; fires an
     * ILocalizationFileObserver event
     */
    private void storeHome() {
        Point point = new Point("", home.y, home.x, PointFieldState.FALSE,
                PointFieldState.FALSE, false, new RGB(0, 0, 0), ROOT_NODE_KEY);
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

    /**
     * Get all points including groups.
     * 
     * @return
     */
    public Collection<Point> getPoints() {
        return getPointsMap().values();
    }

    /**
     * This checks and if needed loads points. It will also check and create the
     * default points and the group they belong to.
     * 
     * @return points
     */
    private Map<String, Point> getPointsMap() {
        boolean doRequest = false;
        if (points.isEmpty()) {
            doRequest = loadPoints();
            String d2dKey = File.separator + D2D_POINTS_GROUP;
            Point d2dPoint = points.get(d2dKey);
            if (d2dPoint != null) {
                if (childrenKeyMap.get(d2dKey).size() == 0) {
                    createDefaultPoints();
                    processRequests();
                }
            } else {
                String dirPath = pointsDir.getName().trim();
                String name = D2D_POINTS_GROUP.replace(' ',
                        PointUtilities.DELIM_CHAR);
                String path = dirPath + File.separator + name;
                LocalizationFile d2dDir = pathMgr.getLocalizationFile(userCtx,
                        path);
                if (!d2dDir.isDirectory()) {
                    try {
                        d2dDir.delete();
                    } catch (LocalizationOpFailedException e) {
                        statusHandler.handle(Priority.PROBLEM,
                                "Unable to create group: " + D2D_POINTS_GROUP);
                        return points;
                    }
                }
                d2dPoint = new GroupNode();
                d2dPoint.setName(D2D_POINTS_GROUP);
                d2dPoint.setGroup(d2dKey);
                PointRequest request = new PointRequest(RequestType.ADD,
                        d2dPoint);
                queueRequest(request);
                String parentKey = ROOT_NODE_KEY;
                childrenKeyMap.get(parentKey).add(d2dKey);
                childrenKeyMap.put(d2dKey, new ArrayList<String>());
                put(d2dKey, d2dPoint);
                createDefaultPoints();
                doRequest = true;
            }

            if (doRequest) {
                processRequests();
            }
        }
        return points;
    }

    /**
     * This creates D2D Point group and the default A-J points for the group.
     */
    private void createDefaultPoints() {
        Coordinate center = getHome();
        int baseRingSize = 120;
        int startAngle = -90;
        String group = File.separator + D2D_POINTS_GROUP;
        List<String> d2dChildren = childrenKeyMap.get(group);
        for (char label = 'A'; label <= 'J'; label++) {
            String name = String.valueOf(label);

            Coordinate coordinate = getCoordinateOnCircle(center, baseRingSize,
                    startAngle);
            Point point = new Point(name, coordinate.y, coordinate.x,
                    PointFieldState.FALSE, PointFieldState.TRUE, false,
                    new RGB(0, 0, 0), group);
            PointRequest request = new PointRequest(RequestType.ADD, point);
            queueRequest(request);
            put(name, point);
            d2dChildren.add(name);
            startAngle += 36;
        }
    }

    /**
     * Set wfoCenter to the sites center point.
     */
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

    /**
     * Loads home point from the localize file and returns its coordinate.
     * 
     * @return home
     */
    private Coordinate loadHome() {
        LocalizationFile lFile = pathMgr.getLocalizationFile(
                pointsDir.getContext(), pointsDir.getName().trim()
                        + File.separator + HOME_LOCATION_FILE);
        Point point = null;
        if (lFile.exists()) {
            point = loadPoint(lFile);
        }

        if (point == null) {
            home = getWfoCenter();
            storeHome();
        } else {
            home = point.getCoordinate();
        }
        return home;
    }

    /**
     * Clear mapping of points and children and create all points and group
     * nodes from the localization files and directory structure.
     */
    private boolean loadPoints() {
        boolean doRequest = false;
        points.clear();
        childrenKeyMap.clear();

        String path = pointsDir.getName().trim();
        LocalizationFile[] files = pathMgr.listFiles(userCtx, path,
                new String[] { POINT_FILENAME_EXT }, true, false);

        if (files.length == 0) {
            pointsDir.getFile().mkdirs();
            Point point = new GroupNode(ROOT_NODE_KEY);
            String key = point.getGroup();
            PointRequest request = new PointRequest(RequestType.ADD, point);
            queueRequest(request);
            put(key, point);
            childrenKeyMap.put(key, new ArrayList<String>());
            doRequest = true;
        } else {
            Point point = null;
            for (LocalizationFile lf : files) {
                point = loadPoint(lf);
                String key = getPointKey(lf);
                put(key, point);
                if (point.isGroup()) {
                    childrenKeyMap.put(key, new ArrayList<String>());
                }
                if (key.length() > 0) {
                    // p
                    String parentKey = getParentKey(point);
                    childrenKeyMap.get(parentKey).add(key);
                }
            }
        }
        doNodeState(points.get(ROOT_NODE_KEY));
        return doRequest;
    }

    private void doNodeState(IPointNode node) {

        // Make sure all the children are in proper state.
        for (IPointNode child : getChildren(node, true)) {
            if (child.isGroup()) {
                doNodeState(child);
            }
        }

        // The children list now have the proper state.
        List<IPointNode> children = getChildren(node, true);
        if (children.size() == 0) {
            ((Point) node).setHidden(PointFieldState.FALSE);
            ((Point) node).setMovable(PointFieldState.TRUE);
            put(getPointKey((Point) node), (Point) node);
            return;
        }

        IPointNode child = children.remove(0);
        PointFieldState hidden = child.getHidden();
        PointFieldState movable = child.getMovable();
        int unknownCnt = 0;

        if (hidden == PointFieldState.UNKNOWN) {
            ++unknownCnt;
        }

        if (movable == PointFieldState.UNKNOWN) {
            ++unknownCnt;
        }

        while (children.size() > 0 && unknownCnt < 2) {
            child = children.remove(0);
            if (hidden != PointFieldState.UNKNOWN
                    && hidden != child.getHidden()) {
                hidden = PointFieldState.UNKNOWN;
                ++unknownCnt;
            }
            if (movable != PointFieldState.UNKNOWN
                    && movable != child.getMovable()) {
                movable = PointFieldState.UNKNOWN;
                ++unknownCnt;
            }
        }
        ((Point) node).setHidden(hidden);
        ((Point) node).setMovable(movable);
        put(getPointKey((Point) node), (Point) node);
    }

    /**
     * Parse a localization file and return the point it contains; does NOT fire
     * an ILocalizationFileObserver event.
     * 
     * @param lFile
     * @return point
     */
    private Point loadPoint(LocalizationFile lFile) {
        Point point = null;

        if (lFile.isDirectory()) {
            String key = getPointKey(lFile);
            point = points.get(key);
            if (point == null) {
                point = new GroupNode(getPointName(lFile));
                point.setGroup(key);
            }
            return point;
        }

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
            StringBuffer sb = new StringBuffer(lFile.toString());
            sb.replace(0, pointsDir.toString().length(), "");
            int index = sb.lastIndexOf(File.separator);
            sb.setLength(index);
            point.setGroup(sb.toString());
        }
        return point;
    }

    /**
     * Get the point's group localized file.
     * 
     * @param point
     * @return lFile
     */
    private LocalizationFile getGroupDir(Point point) {
        String path = (pointsDir.getName() + point.getGroup().replace(' ',
                PointUtilities.DELIM_CHAR));
        LocalizationFile dir = pathMgr.getLocalizationFile(userCtx, path);
        return dir;
    }

    /**
     * Get the points and non-empty group nodes that are the children of the
     * node.
     * 
     * @param node
     * @return children
     */
    public List<IPointNode> getChildren(IPointNode node) {
        return getChildren(node, false);
    }

    /**
     * Get children of node. When node is null the children of the root node are
     * returned.
     * 
     * @param node
     * @param allGroups
     *            - When true include all groups otherwise only non-empty groups
     *            are included.
     * @return children
     */
    public List<IPointNode> getChildren(IPointNode node, boolean allGroups) {
        // Make sure point maps are are loaded.
        getPointsMap();

        String parentKey = null;
        if (node == null) {
            parentKey = ROOT_NODE_KEY;
        } else {
            parentKey = getPointKey((Point) node);
        }

        List<String> childrenKeyList = childrenKeyMap.get(parentKey);
        List<IPointNode> children = new ArrayList<IPointNode>();
        if (childrenKeyList != null) {
            if (allGroups) {
                for (String key : childrenKeyList) {
                    children.add(points.get(key));
                }
            } else {
                for (String key : childrenKeyList) {
                    IPointNode child = points.get(key);
                    if (!child.isGroup()
                            || childrenKeyMap.get(getPointKey((Point) child))
                                    .size() > 0) {
                        children.add(child);
                    }
                }
            }
            sort(children);
        }
        return children;
    }

    /**
     * Sort the collection of nodes placing group before points.
     * 
     * @param nodes
     */
    private void sort(List<IPointNode> nodes) {
        Collections.sort(nodes, new Comparator<IPointNode>() {

            @Override
            public int compare(IPointNode o1, IPointNode o2) {
                return o1.compareTo(o2);
            }
        });
    }

    /**
     * Get the localization file name for the point.
     * 
     * @param point
     * @return filename
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
        String filename = sb.toString().replace(' ', PointUtilities.DELIM_CHAR);
        return filename;
    }

    /**
     * Get the localization path for the group node.
     * 
     * @param node
     * @return path
     */
    private String getPointDirName(Point node) {
        String group = node.getGroup();
        StringBuilder sb = new StringBuilder();
        sb.append(pointsDir.getName().trim());
        if (group.length() > 0) {
            if (group.charAt(0) != File.separatorChar) {
                sb.append(File.separator);
            }
            sb.append(group);
        }
        String path = sb.toString().replace(' ', PointUtilities.DELIM_CHAR);
        return path;
    }

    /**
     * Get the name for the point/node for the localization file.
     * 
     * @param lFile
     * @return name
     */
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

    /**
     * Obtain the unique key for the point represented by the localization file.
     * 
     * @param lFile
     * @return key
     */
    private String getPointKey(LocalizationFile lFile) {
        String key = null;
        if (!lFile.isDirectory()) {
            key = getPointName(lFile);
        } else {
            StringBuilder sb = new StringBuilder();
            sb.append(lFile.toString());
            sb.replace(0, pointsDir.toString().length(), "");
            key = sb.toString();
        }
        return key.replace(PointUtilities.DELIM_CHAR, ' ');
    }

    /**
     * Determine the key for the point.
     * 
     * @param point
     * @return key
     */
    private String getPointKey(Point point) {
        String key = null;
        if (!point.isGroup()) {
            key = point.getName();
        } else {
            key = point.getGroup();
        }
        return key;
    }

    /**
     * Get the key for the parent of the point.
     * 
     * @param point
     * @return parentKey
     */
    private String getParentKey(Point point) {
        String parentKey = point.getGroup();
        if (point.isGroup() && parentKey.length() > 0) {
            parentKey = parentKey.substring(0,
                    parentKey.lastIndexOf(File.separator));
        }
        return parentKey;
    }

    /**
     * @param node
     * @return
     */
    public IPointNode getParent(IPointNode node) {
        Point parent = points.get(getParentKey((Point) node));
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
        Point node = new GroupNode();
        node.setName(name);
        String parentKey = getPointKey(parent);
        node.setGroup(parentKey + File.separator + name);
        String nodeKey = getPointKey(node);
        PointRequest request = new PointRequest(RequestType.ADD, node);
        queueRequest(request);
        childrenKeyMap.get(parentKey).add(nodeKey);
        put(nodeKey, node);
        childrenKeyMap.put(nodeKey, new ArrayList<String>());
        processRequests();
        return node;
    }

    /**
     * Moves a node to the new location.
     * 
     * @param node
     * @param destNode
     */
    public void moveNode(final IPointNode node, final IPointNode destNode) {
        String oldParentKey = getParentKey((Point) node);
        String destKey = getPointKey((Point) destNode);
        if (oldParentKey.equals(destKey)) {
            return;
        }
        doMoveNode(node, destNode);
        checkGroupState(points.get(oldParentKey));
        checkGroupState(destNode);
        processRequests();
    }

    /**
     * Recursive method to handle moving a group node.
     * 
     * @param node
     * @param destNode
     */
    private void doMoveNode(IPointNode node, IPointNode destNode) {
        String key = getPointKey((Point) node);
        Point point = points.get(key);
        String newParentKey = getPointKey((Point) destNode);
        if (!point.isGroup()) {
            doDeletePoint(point);
            point.setGroup(newParentKey);
            doAddPoint(point);
            put(key, point);
            childrenKeyMap.get(newParentKey).add(key);
        } else {
            Point newGroup = new GroupNode((Point) point);
            String newGroupKey = newParentKey + File.separator
                    + point.getName();
            newGroup.setGroup(newGroupKey);
            PointRequest request = new PointRequest(RequestType.ADD, newGroup);
            queueRequest(request);
            put(newGroupKey, newGroup);
            childrenKeyMap.put(newGroupKey, new ArrayList<String>());
            childrenKeyMap.get(newParentKey).add(newGroupKey);

            for (IPointNode child : getChildren(node)) {
                doMoveNode(child, newGroup);
            }
            doDeletePoint(point);
        }
    }

    /**
     * Renames a group node and handles moving its childern to the new
     * localtion.
     * 
     * @param srcNode
     * @param destName
     * @return
     */
    public boolean renameGroup(final IPointNode srcNode, final String destName) {
        if (srcNode.getName().equals(destName)) {
            return false;
        }

        String parentKey = getParentKey((Point) srcNode);
        Point parent = points.get(parentKey);
        Point destNode = new GroupNode(parent);
        String destKey = parentKey + File.separator + destName;
        destNode.setName(destName);
        destNode.setGroup(destKey);
        PointRequest request = new PointRequest(RequestType.ADD, destNode);
        queueRequest(request);
        put(destKey, destNode);
        childrenKeyMap.get(parentKey).add(destKey);
        childrenKeyMap.put(destKey, new ArrayList<String>());
        for (IPointNode child : getChildren(srcNode)) {
            doMoveNode(child, destNode);
        }
        doDeletePoint((Point) srcNode);
        processRequests();
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
        } catch (LocalizationException e) {
            statusHandler.handle(Priority.PROBLEM,
                    "Unable to create the group: " + gPoint.getGroup(), e);
            return null;
        } catch (IOException e) {
            statusHandler.handle(Priority.PROBLEM, e.getLocalizedMessage(), e);
        }
        return new GroupNode(gPoint);
    }

    /**
     * Determine if a point exits for the given name.
     * 
     * @param name
     * @return true when point exits otherwise false
     */
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

    /**
     * Determine the coordinate of point on a circle.
     * 
     * @param coor
     *            - center of the circle
     * @param radius
     *            - Distance to the point
     * @param angle
     *            - Angle from the center
     * @return coordinate
     */
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

        // Assume message is for a directory (group), GROUP_INFO or point map
        // file. If the changes are already in this instance of class then the
        // message can be ignored. Otherwise we need to update this instance and
        // notify listeners.
        boolean stateChange = false;
        if (fileName.startsWith(POINT_FILENAME_PREFIX)
                && fileName.endsWith(POINT_FILENAME_EXT)) {
            stateChange = checkPoint(message, fileName);
        } else if (fileName.equals(GROUP_INFO)) {
            stateChange = checkGroup(message);
        } else if (message.getChangeType() == FileChangeType.DELETED) {
            // Assume group directory that may be on the pending delete list.
            checkGroupDelete(message);
        }

        if (stateChange) {
            firePointChangeListeners();
        }
    }

    /**
     * Determine if the message needs to be acted upon by this instance of CAVE.
     * 
     * @param message
     * @param fileName
     * @return
     */
    private boolean checkPoint(FileUpdatedMessage message, String fileName) {
        boolean stateChange = false;

        StringBuilder sb = new StringBuilder(message.getFileName());
        sb.replace(0, pointsDir.getName().length(), "");
        sb.replace(sb.lastIndexOf(File.separator), sb.length(), "");
        String groupKey = sb.toString().replace(PointUtilities.DELIM_CHAR, ' ');

        sb.setLength(0);
        sb.append(fileName);
        sb.replace(0, POINT_FILENAME_PREFIX.length(), "");
        sb.replace(sb.length() - POINT_FILENAME_EXT.length(), sb.length(), "");
        String key = sb.toString().replace(PointUtilities.DELIM_CHAR, ' ');

        Point foundPoint = points.get(key);
        Point point = null;
        LocalizationFile lFile = null;
        switch (message.getChangeType()) {
        case ADDED:
            lFile = pathMgr.getLocalizationFile(userCtx, message.getFileName());
            point = loadPoint(lFile);
            if (foundPoint == null) {
                put(key, point);
                childrenKeyMap.get(groupKey).add(key);
                checkGroupState(getParent(point));
                stateChange = true;
            } else if (!groupKey.equals(foundPoint.getGroup())) {
                // Finishing up moving the point to a different group.
                childrenKeyMap.get(foundPoint.getGroup()).remove(key);
                childrenKeyMap.get(groupKey).add(key);
                put(key, point);
                checkGroupState(getParent(point));
                stateChange = true;
            }
            break;
        case UPDATED:
            lFile = pathMgr.getLocalizationFile(userCtx, message.getFileName());
            point = loadPoint(lFile);
            if (foundPoint == null) {
                statusHandler.handle(Priority.PROBLEM,
                        "Unable to find point to update: " + key);
            } else if (!groupKey.equals(foundPoint.getGroup())) {
                statusHandler.handle(Priority.PROBLEM,
                        "Updated point in wrong group: " + key);
            } else if (point.differentContent(foundPoint)) {
                put(key, point);
                checkGroupState(getParent(point));
                stateChange = true;
            }
            break;
        case DELETED:
            if (foundPoint != null) {
                // When groups are different assume in the middle of move
                // (delete/add) do nothing and let the ADDED handle it.
                if (groupKey.equals(foundPoint.getGroup())) {
                    IPointNode parent = getParent(foundPoint);
                    childrenKeyMap.get(groupKey).remove(key);
                    remove(key);
                    checkGroupState(parent);
                    stateChange = true;
                }
            } else {
                checkGroupDelete(message);
            }
            break;
        default:
            statusHandler.handle(Priority.DEBUG, "Unhandled change type: "
                    + message.getChangeType());
        }
        return stateChange;
    }

    /**
     * Check to see if the node's movable and hidden state and if changed check
     * its parent node.
     * 
     * @param node
     *            - group node to check
     * @return true - when either movable or hidden have changed otherwise false
     */
    private boolean checkGroupState(IPointNode node) {
        List<IPointNode> children = getChildren(node, true);
        if (children.size() == 0) {
            return false;
        }

        IPointNode child = children.remove(0);
        PointFieldState hidden = child.getHidden();
        PointFieldState movable = child.getMovable();

        int unknownCnt = 0;
        if (hidden == PointFieldState.UNKNOWN) {
            ++unknownCnt;
        }

        if (movable == PointFieldState.UNKNOWN) {
            ++unknownCnt;
        }

        while (children.size() > 0 && unknownCnt < 2) {
            child = children.remove(0);
            if (hidden != PointFieldState.UNKNOWN
                    && hidden != child.getHidden()) {
                hidden = PointFieldState.UNKNOWN;
                ++unknownCnt;
            }
            if (movable != PointFieldState.UNKNOWN
                    && movable != child.getMovable()) {
                movable = PointFieldState.UNKNOWN;
                ++unknownCnt;
            }
        }
        boolean value = false;
        if (hidden != node.getHidden()) {
            value = true;
        }
        if (movable != node.getMovable()) {
            value = true;
        }

        if (value) {
            Point point = points.get(getPointKey((Point) node));
            point.setHidden(hidden);
            point.setMovable(movable);
            if (!getParent(node).equals(ROOT_NODE_KEY)) {
                checkGroupState(getParent(node));
            }
        }
        return value;
    }

    /**
     * Determine if this group message needs to be acted upoin by this instance
     * of CAVE.
     * 
     * @param message
     * @return
     */
    private boolean checkGroup(FileUpdatedMessage message) {
        boolean stateChange = false;
        StringBuilder sb = new StringBuilder(message.getFileName());
        sb.setLength(sb.lastIndexOf(File.separator));
        sb.replace(0, pointsDir.getName().length(), "");
        String key = sb.toString().replace(PointUtilities.DELIM_CHAR, ' ');
        String parentKey = null;
        int index = key.lastIndexOf(File.separator);
        if (index >= 0) {
            parentKey = key.substring(0, index);
        }
        Point foundGroup = points.get(key);

        switch (message.getChangeType()) {
        case ADDED:
            if (foundGroup == null) {
                sb.replace(0, sb.lastIndexOf(File.separator) + 1, "");
                String name = sb.toString().replace(PointUtilities.DELIM_CHAR,
                        ' ');
                Point point = new GroupNode();
                point.setName(name);
                point.setGroup(key);
                put(key, point);
                childrenKeyMap.get(parentKey).add(key);
                childrenKeyMap.put(key, new ArrayList<String>());
                checkGroupState(getParent(point));
                stateChange = true;
            }
            break;
        case DELETED:
            if (foundGroup != null) {
                if (childrenKeyMap.get(key).size() > 0) {
                    statusHandler.handle(Priority.PROBLEM, "Removing group \""
                            + key + "\" while it contains points or groups");
                }
                childrenKeyMap.remove(key);
                childrenKeyMap.get(parentKey).remove(key);
                remove(key);
                checkGroupState(points.get(parentKey));
                stateChange = true;
            } else {
                checkGroupDelete(message);
            }
            break;
        default:
            statusHandler.handle(Priority.DEBUG, "Unexepected change type "
                    + message.getChangeType() + " for group \"" + key + "\"");
        }
        return stateChange;
    }

    /**
     * Check the message's file name to see if it needs to be removed from a
     * pending delete list and see if it is time to delete the parent's
     * directory.
     * 
     * @param message
     */
    private void checkGroupDelete(FileUpdatedMessage message) {
        String filename = message.getFileName();
        String deleteKey = filename.substring(0,
                filename.lastIndexOf(File.separator));
        List<String> childList = groupDeleteMap.get(deleteKey);
        if (childList != null) {
            childList.remove(filename);
            if (childList.size() == 0) {
                LocalizationFile lFile = pathMgr.getLocalizationFile(userCtx,
                        deleteKey);
                try {
                    lFile.delete();
                } catch (LocalizationOpFailedException e) {
                    statusHandler.handle(Priority.PROBLEM,
                            e.getLocalizedMessage(), e);
                }
                groupDeleteMap.remove(deleteKey);
            }
        }
    }

    /**
     * Let everyone know about the update to the home point.
     * 
     * @param fileName
     */
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

    /**
     * Let listners no about changes to point or groups.
     */
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

    /**
     * Add the point (never a group) and create/update its file.
     * 
     * @param point
     */
    public void addPoint(final Point point) {
        Assert.isTrue(!point.isGroup());

        String key = getPointKey(point);
        Point foundPoint = getPointsMap().get(key);

        PointRequest request = null;

        if (foundPoint == null) {
            request = new PointRequest(RequestType.ADD, point);
            childrenKeyMap.get(getParentKey(point)).add(key);
        } else {
            request = new PointRequest(RequestType.UPDATE, point);
        }
        queueRequest(request);
        put(key, point);
        checkGroupState(getParent(point));
        processRequests();
    }

    /**
     * Create the quest to add a point and adds it to the queue.
     * 
     * @param point
     */
    private void doAddPoint(Point point) {
        PointRequest request = new PointRequest(RequestType.ADD, point);
        queueRequest(request);
    }

    /**
     * The server will return asynchronously via
     * ILocalizationFileObserver::fileUpdated and a FileUpdatedMessage status of
     * ADDED if file was successfully created.
     * 
     * @param point
     * @return returns true if point was successfully added, false otherwise,
     *         for example when a duplicate point name exists and forceOverwrite
     *         was false
     */
    private void storePoint(Point point) {
        LocalizationFile dir = getGroupDir(point);
        storePoint(dir, point);
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
        IPointNode parentNode = getParent(point);
        doDeletePoint(point);
        checkGroupState(parentNode);
        processRequests();
    }

    /**
     * Queues request to delete a point and if it is a group recursive deletes
     * its children.
     * 
     * @param point
     */
    private void doDeletePoint(Point point) {
        String key = getPointKey(point);

        Point foundPoint = getPoint(key);
        PointRequest request = null;
        String pointKey = getPointKey(foundPoint);
        String parentKey = getParentKey(foundPoint);
        String deleteKey = getPointDirName(foundPoint);

        if (foundPoint.isGroup()) {
            // When removing a group node we need to delete the directory
            // that represents the node. That can not be done until all
            // entries in the directory are removed. This determines
            // what we need to have deleted and adds requests to the queue.
            String groupInfo = deleteKey + File.separator + GROUP_INFO;
            groupDeleteMap.put(deleteKey, new ArrayList<String>());
            groupDeleteMap.get(deleteKey).add(groupInfo);

            List<IPointNode> children = getChildren(foundPoint, true);
            for (IPointNode child : children) {
                doDeletePoint((Point) child);
            }
            childrenKeyMap.remove(pointKey);
        } else {
            List<String> childList = groupDeleteMap.get(deleteKey);
            if (childList != null) {
                childList.add(getPointFilename(foundPoint));
            }
        }
        request = new PointRequest(RequestType.DELETE, foundPoint);
        queueRequest(request);
        childrenKeyMap.get(parentKey).remove(pointKey);
        remove(pointKey);
    }

    /***
     * Try to remove a Point file from the persistent store; fires an
     * ILocalizationFileObserver event.
     * 
     * @param point
     */
    private void removePoint(Point point) {
        LocalizationFile lFile = null;
        if (point.isGroup()) {
            String name = getPointDirName(point) + File.separator + GROUP_INFO;
            lFile = pathMgr.getLocalizationFile(userCtx, name);
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

    /**
     * Updates a point by remove/add if name is changed else by a update
     * request.
     * 
     * @param oldPoint
     * @param newPoint
     */
    public void updatePoint(final Point oldPoint, final Point newPoint) {
        PointRequest request = null;

        if (!oldPoint.getName().equals(newPoint.getName())
                || !oldPoint.getGroup().equals(newPoint.getGroup())) {
            // Points name or group changed need to remove old point and add
            // new.
            request = new PointRequest(RequestType.DELETE, oldPoint);
            queueRequest(request);
            String oldParentKey = getParentKey(oldPoint);
            String oldKey = getPointKey(oldPoint);
            childrenKeyMap.get(oldParentKey).remove(oldKey);
            request = new PointRequest(RequestType.ADD, newPoint);
            queueRequest(request);
            String newParentKey = getParentKey(newPoint);
            String newKey = getPointKey(newPoint);
            childrenKeyMap.get(newParentKey).add(newKey);
            remove(oldKey);
            put(newKey, newPoint);
        } else {
            request = new PointRequest(RequestType.UPDATE, newPoint);
            queueRequest(request);
            put(getPointKey(newPoint), newPoint);
        }
        checkGroupState(getParent(newPoint));
        processRequests();
    }

    /**
     * Update a non-group point.
     * 
     * @param point
     * @throws PointNameChangeException
     */
    public void updatePoint(Point point) throws PointNameChangeException {
        Assert.isTrue(point != null && !point.isGroup());
        Point oldPoint = getPoint(point.getName());
        if (oldPoint == null) {
            throw new PointNameChangeException("Point does not exist");
        }
        addPoint(point);
    }

    /**
     * change node's and all it chilren's hidden to the desired state.
     * 
     * @param node
     * @param state
     */
    public void updateChildrenHidden(IPointNode node, PointFieldState state) {
        if (!node.isGroup()) {
            return;
        }
        doChildrenHidden(node, state);
        checkGroupState(getParent(node));
        processRequests();
    }

    /**
     * Does the recursive work for updateChildrenHidden.
     * 
     * @param node
     * @param state
     */
    private void doChildrenHidden(IPointNode node, PointFieldState state) {
        String key = getPointKey((Point) node);
        Point point = points.get(key);
        PointRequest request = null;

        if (!point.isGroup()) {
            if (point.getHidden() != state) {
                point.setHidden(state);
                request = new PointRequest(RequestType.UPDATE, point);
                queueRequest(request);
            }
        } else {
            point.setHidden(state);
            List<IPointNode> children = getChildren(point);
            for (IPointNode child : children) {
                doChildrenHidden(child, state);
            }
        }
    }

    /**
     * change node's and all it chilren's movable to the desired state.
     * 
     * @param node
     * @param state
     */
    public void updateChildrenMovable(IPointNode node, PointFieldState state) {
        if (!node.isGroup()) {
            return;
        }
        doChildrenMovable(node, state);
        processRequests();
    }

    /**
     * Does the recursive work for updateChildrenMovable.
     * 
     * @param node
     * @param state
     */
    private void doChildrenMovable(IPointNode node, PointFieldState state) {
        String key = getPointKey((Point) node);
        Point point = points.get(key);
        PointRequest request = null;

        if (!point.isGroup()) {
            if (point.getMovable() != state) {
                point.setMovable(state);
                request = new PointRequest(RequestType.UPDATE, point);
                queueRequest(request);
            }
        } else {
            point.setMovable(state);
            List<IPointNode> children = getChildren(point);
            for (IPointNode child : children) {
                doChildrenMovable(child, state);
            }
        }
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
        InputStream stream = null;
        try {
            stream = lFile.openInputStream();
            return JAXB.unmarshal(stream, Point.class);
        } finally {
            if (stream != null) {
                stream.close();
            }
        }
    }

    /**
     * All put to points should use this in case additional work needs to be
     * done.
     * 
     * @param key
     * @param point
     */
    private void put(String key, Point point) {
        points.put(key, point);
    }

    /**
     * All remove to points should use this in case addtional work needs to be
     * done.
     * 
     * @param key
     * @return
     */
    private Point remove(String key) {
        return points.remove(key);
    }
}
