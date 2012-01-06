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
package com.raytheon.viz.mpe.ui.dialogs.polygon;

import java.awt.Point;
import java.awt.Rectangle;
import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.File;
import java.io.FileReader;
import java.io.FileWriter;
import java.io.IOException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.StringTokenizer;

import javax.measure.converter.UnitConverter;
import javax.measure.unit.NonSI;
import javax.measure.unit.SI;

import com.raytheon.uf.common.hydro.spatial.HRAPCoordinates;
import com.raytheon.uf.common.mpe.util.XmrgFile;
import com.raytheon.uf.common.ohd.AppsDefaults;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.common.util.FileUtil;
import com.raytheon.viz.mpe.MPEConstants;
import com.raytheon.viz.mpe.ui.DisplayFieldData;
import com.raytheon.viz.mpe.ui.MPEDisplayManager;
import com.raytheon.viz.mpe.ui.rsc.XmrgResource;

/**
 * Utility class for Precipitation Polygons.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jul 7, 2009  2685       mpduff      Initial creation
 * Aug 21, 2009 2685       mpduff      Fixed problem with Scale option
 * 
 * </pre>
 * 
 * @author mpduff
 * @version 1.0
 */

public class PrecipPolyUtils {
    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(PrecipPolyUtils.class);

    private static final String POLY_DIR_TOKEN = "rfcwide_drawpre_dir";

    private static final String ACTION_TOKEN = "mpe_polygon_action_order";

    private static final int REGION_SCALE_FACTOR = 10;

    private static final String valueFormat = "%6.2f";

    private static final String[] ACTION_NAMES = { "Sub", "Set", "Snow",
            "Raise", "Lower", "Scale" };

    private static final UnitConverter converter = NonSI.INCH
            .getConverterTo(SI.MILLIMETER);

    private static String polygonDir = null;

    /**
     * Specifies the precedence of the actions that can be performed on a
     * polygon. The -1 is a sentinel that indicates that no precedence was
     * specified for the given action.
     */
    private static Map<String, Integer> actionPrecedenceMap;

    /**
     * Contains the draw precedence of each field that can be substituted in a
     * polygon. The -1 is a sentinel that indicates that no precedence was
     * specified for the given field.
     */
    private static List<String> subPrecedence;

    /** First time through? */
    private static boolean first = true;

    /** Action order */
    private static int actionOrder = 0;

    /** Substitution order */
    private static int subOrder = 0;

    /**
     * Write polygon info from draw precip mode into file.
     * 
     * @param data
     *            The polygon data
     */
    public static synchronized void writeDrawPrecipData(RubberPolyData data,
            boolean write) {
        MPEDisplayManager dispMgr = MPEDisplayManager.getCurrent();
        XmrgResource xmrgRsc = (XmrgResource) dispMgr.getDisplayedResource();

        short[] xmrgData = xmrgRsc.getData();

        if (xmrgData != null) {
            /* Apply the polygon to the MPE product here. */
            if (data.isVisible()
                    && (data.isSub_flag() || data.isSet_flag()
                            || data.isLower_flag() || data.isRaise_flag()
                            || data.isScale_flag() || data.isSnow_flag())) {
                xmrgData = applyEditPolygon(data, xmrgData);
            }

            /* write polygon info to DrawPrecip file */
            /* polygon points are stored in HRAP coord */
            if (write) {
                writePolygons();
            }

            xmrgRsc.setData(xmrgData);
            xmrgRsc.updateXmrg(false);
        }
    }

    /**
     * When an MPE product is loaded, this routine determines if there are
     * Polygons which must be loaded onto the field. If there are, then it loads
     * the polygons in order of their polygon number in the polygon file.
     * 
     * @param polyData
     * @param xmrgData
     * @return
     */
    public static synchronized short[] applyEditPolygon(
            RubberPolyData polyData, short[] xmrgData) {
        short[] tempData = null;
        float precipVal;

        // Get the substitute type if needed
        try {
            if (polyData.getAction().equalsIgnoreCase("Sub")) {
                tempData = getXmrgData(polyData.getSubDrawSource());
            }
        } catch (IOException ioe) {
            ioe.printStackTrace();
        }

        /*
         * determine HRAP coordinates bounded by polygon update field array
         */
        precipVal = (float) polyData.getPrecipValue();

        List<Point> hrapList = polyData.getHrap();
        Rectangle extent = null;
        try {
            extent = HRAPCoordinates.getHRAPCoordinates();
        } catch (Exception e) {
            e.printStackTrace();
        }

        for (Point p : hrapList) {
            int x = p.x - extent.x;
            int y = extent.y + extent.height - 1 - p.y;

            int idx = y * extent.width + x;
            if ((x >= 0) && (x < extent.width) && (y >= 0)
                    && (y < extent.height)) {
                switch (polyData.getDrawSource()) {
                case avgrMosaic:
                case bMosaic:
                case gageOnly:
                case lMosaic:
                case lqmosaic:
                case lsatPre:
                case maxrMosaic:
                case mlMosaic:
                case mlqmosaic:
                case mMosaic:
                case p3lMosaic:
                case qmosaic:
                case rfcbMosaic:
                case rfcmMosaic:
                case rfcMosaic:
                case Xmrg:
                case rMosaic:
                case satPre:
                case sgMosaic:
                case srgMosaic:
                case srMosaic:
                case subValue:
                    if (tempData != null) {
                        xmrgData[idx] = tempData[idx];
                    } else {
                        /*
                         * Updated to make the converter a double. The check for
                         * < 50 is used if the precipVal is 1 When the precipVal
                         * is 1 (actually 0.01 but not divided by 100 since it
                         * is stored a short) the converted value end up being
                         * 25.4 25.4 gets rounded down to 25 and the loss of
                         * precision causes the value of "0.01" to be colored as
                         * 0.00.
                         */
                        double converted = converter.convert((precipVal));
                        if (converted < 50) {
                            converted = Math.ceil(converted);
                        }

                        if (polyData.isRaise_flag()) {
                            xmrgData[idx] = (short) Math.max(converted,
                                    xmrgData[idx]);
                        } else if (polyData.isLower_flag()) {
                            xmrgData[idx] = (short) Math.min(converted,
                                    xmrgData[idx]);
                        } else if (polyData.isScale_flag()) {
                            xmrgData[idx] *= (precipVal / 100.0);
                        } else if (polyData.isSnow_flag()) {
                            if (xmrgData[idx] < converted) {
                                xmrgData[idx] = -9999;
                            }
                        } else if (polyData.isSet_flag()) {
                            xmrgData[idx] = (short) converted;
                        }
                    }
                    break;

                default:

                }
            }
        }

        return xmrgData;
    }

    /**
     * Get the polygon file's directory.
     * 
     * @return The directory containing the polygon files
     */
    public static String getPolygonDir() {
        if (polygonDir == null) {
            AppsDefaults appsDefaults = AppsDefaults.getInstance();
            polygonDir = appsDefaults.getToken(POLY_DIR_TOKEN);
        }
        return polygonDir;
    }

    /**
     * Read the poly data from the polygon file.
     * 
     * @param file
     *            The polygon file
     * @param persistent
     *            true if persistent file
     */
    public static List<RubberPolyData> readPolyData(File file,
            List<RubberPolyData> polygonList, DisplayFieldData fieldData,
            boolean persistent) {
        RubberPolyData polyNode = new RubberPolyData();

        try {
            BufferedReader in = new BufferedReader(new FileReader(file));
            String str;
            int numberPoints = 0;
            boolean firstRecord = true;

            while ((str = in.readLine()) != null) {
                String[] pieces = str.trim().split("\\s+");

                // Add the previous data set
                if (pieces.length == 5) {
                    if (!firstRecord) {
                        polygonList.add(polyNode);
                        polyNode = new RubberPolyData();
                    }
                    firstRecord = false;

                    /*
                     * Copy the header information to the rubber_poly_data
                     * structure.
                     */
                    numberPoints = Integer.parseInt(pieces[3]);

                    polyNode.setPolygonNumber(Integer.parseInt(pieces[0]));
                    polyNode.setNumberPoints(numberPoints);

                    if (!pieces[1].equalsIgnoreCase(ACTION_NAMES[0])) {
                        polyNode.setPrecipValue(Float.parseFloat(pieces[2]));
                    }

                    polyNode.setDrawSource(DisplayFieldData.subValue);
                    for (int i = 0; i < ACTION_NAMES.length; i++) {
                        if (pieces[1].equalsIgnoreCase(ACTION_NAMES[0])) { // Sub
                            polyNode.setSub_flag(true);

                            // loop through and find the right displayFieldData
                            DisplayFieldData[] dataTypes = DisplayFieldData
                                    .values();
                            for (int j = 0; j < dataTypes.length; j++) {
                                if (dataTypes[j].getCv_use().equalsIgnoreCase(
                                        pieces[2])) {
                                    polyNode.setSubDrawSource(dataTypes[j]);
                                    break;
                                }
                            }

                            break;
                        } else if (pieces[1].equalsIgnoreCase(ACTION_NAMES[1])) { // Set
                            polyNode.setSet_flag(true);
                            polyNode.setDrawSource(fieldData);
                            break;
                        } else if (pieces[1].equalsIgnoreCase(ACTION_NAMES[2])) { // Snow
                            polyNode.setSnow_flag(true);
                            polyNode.setDrawSource(fieldData);
                            break;
                        } else if (pieces[1].equalsIgnoreCase(ACTION_NAMES[3])) { // Raise
                            polyNode.setRaise_flag(true);
                            polyNode.setDrawSource(fieldData);
                            break;
                        } else if (pieces[1].equalsIgnoreCase(ACTION_NAMES[4])) { // Lower
                            polyNode.setLower_flag(true);
                            polyNode.setDrawSource(fieldData);
                            break;
                        } else if (pieces[1].equalsIgnoreCase(ACTION_NAMES[5])) { // Scale
                            polyNode.setScale_flag(true);
                            polyNode.setDrawSource(fieldData);
                            break;
                        }
                    }

                    if (pieces[4].equalsIgnoreCase("0")) {
                        polyNode.setVisible(false);
                    } else {
                        polyNode.setVisible(true);
                    }

                    polyNode.setPersistent(persistent);
                } else if (pieces.length == 2) {
                    // Reading in polygon vertices
                    int hrapx = Integer.parseInt(pieces[0]);
                    int hrapy = Integer.parseInt(pieces[1]);

                    Point p = new Point(hrapx, hrapy);
                    polyNode.addHrapPoint(p);

                    if (p.x < polyNode.getMinx()) {
                        polyNode.setMinx(p.x);
                    }

                    if (p.y < polyNode.getMiny()) {
                        polyNode.setMiny(p.y);
                    }

                    if (p.x > polyNode.getMaxx()) {
                        polyNode.setMaxx(p.x);
                    }

                    if (p.y > polyNode.getMaxy()) {
                        polyNode.setMaxy(p.y);
                    }
                }
            }

            // Add the last data set
            polygonList.add(polyNode);

            in.close();
        } catch (IOException e) {
            e.printStackTrace();
        }

        return polygonList;
    }

    /**
     * Order the polygons.
     * 
     * @param dataList
     *            The list of polygons
     * @return The ordered list of polygons
     */
    public static List<RubberPolyData> orderPolygons(
            List<RubberPolyData> dataList) {
        int nodeNumber;
        int action;
        List<RubberPolyData> orderedList = new ArrayList<RubberPolyData>();
        RubberPolyData pNode = null;
        RubberPolyData pNodeN = null;
        if (first) {
            first = false;
            actionOrder = initActionOrder();
            subOrder = initSubstituteOrder();
        }

        if ((actionOrder == 1) || (subOrder == 1)) {
            /* Polygon precedence does matter ... */
            if (actionOrder == 1) {
                /* Some action ordering exists. */
                nodeNumber = 1;

                for (int i = 0; i < ACTION_NAMES.length; i++) {
                    action = actionPrecedenceMap.get(ACTION_NAMES[i]);

                    if (action == -1) {
                        break;
                    }

                    /* Walk through the polygon list looking for this element */
                    for (int j = 0; j < dataList.size(); j++) {
                        pNode = dataList.get(j);

                        if (ACTION_NAMES[action].equalsIgnoreCase("Sub")) {
                            if (pNode.isSub_flag()) {
                                pNodeN = dataList.get(nodeNumber);

                                if (pNodeN != pNode) {
                                    orderedList.add(pNodeN);
                                } else {
                                    orderedList.add(pNode);
                                }
                                nodeNumber++;
                            }
                        } else if (ACTION_NAMES[action].equalsIgnoreCase("Set")) {
                            if (pNode.isSet_flag()) {
                                pNodeN = dataList.get(nodeNumber);

                                if (pNodeN != pNode) {
                                    orderedList.add(pNodeN);
                                } else {
                                    orderedList.add(pNode);
                                }
                                nodeNumber++;
                            }
                        } else if (ACTION_NAMES[action]
                                .equalsIgnoreCase("Snow")) {
                            if (pNode.isSnow_flag()) {
                                pNodeN = dataList.get(nodeNumber);

                                if (pNodeN != pNode) {
                                    orderedList.add(pNodeN);
                                } else {
                                    orderedList.add(pNode);
                                }
                                nodeNumber++;
                            }
                        } else if (ACTION_NAMES[action]
                                .equalsIgnoreCase("Raise")) {
                            if (pNode.isRaise_flag()) {
                                pNodeN = dataList.get(nodeNumber);

                                if (pNodeN != pNode) {
                                    orderedList.add(pNodeN);
                                } else {
                                    orderedList.add(pNode);
                                }
                                nodeNumber++;
                            }
                        } else if (ACTION_NAMES[action]
                                .equalsIgnoreCase("Lower")) {
                            if (pNode.isLower_flag()) {
                                pNodeN = dataList.get(nodeNumber);

                                if (pNodeN != pNode) {
                                    orderedList.add(pNodeN);
                                } else {
                                    orderedList.add(pNode);
                                }
                                nodeNumber++;
                            }
                        } else if (ACTION_NAMES[action]
                                .equalsIgnoreCase("Scale")) {
                            if (pNode.isScale_flag()) {
                                pNodeN = dataList.get(nodeNumber);

                                if (pNodeN != pNode) {
                                    orderedList.add(pNodeN);
                                } else {
                                    orderedList.add(pNode);
                                }
                                nodeNumber++;
                            }
                        } else {
                            // TODO log this message
                            // Unrecognized action %d in order "
                            // "polygons.\n", action
                        }
                    }

                }
                dataList = orderedList;
            }
            /* Action ordering complete. */

            List<RubberPolyData> subOrderedList = new ArrayList<RubberPolyData>();

            if (subOrder == 1) {
                int listSize = dataList.size();

                for (int i = 0; i < subPrecedence.size(); i++) {
                    for (int j = 0; j < listSize; j++) {
                        RubberPolyData node = dataList.get(j);

                        if (node.getDrawSource().getCv_use()
                                .equalsIgnoreCase(subPrecedence.get(i))) {
                            subOrderedList.add(node);
                        }
                    }
                }
                dataList = subOrderedList;
            }
        }

        return dataList;
    }

    /**
     * Initialize the action order.
     * 
     * @return
     */
    private static int initActionOrder() {
        AppsDefaults appsDefaults = AppsDefaults.getInstance();
        int order = 0;
        int status = 0;
        actionPrecedenceMap = new HashMap<String, Integer>();

        for (int i = 0; i < ACTION_NAMES.length; i++) {
            actionPrecedenceMap.put(ACTION_NAMES[i], -1);
        }

        String value = appsDefaults.getToken(ACTION_TOKEN);

        if (value != null) {
            if (!value.equalsIgnoreCase("NONE")) {
                StringTokenizer st = new StringTokenizer(value, ",");
                DisplayFieldData[] fieldNames = DisplayFieldData.values();

                while (st.hasMoreTokens()) {
                    String token = st.nextToken();
                    for (int i = 0; i < ACTION_NAMES.length; i++) {
                        if (token.equalsIgnoreCase(ACTION_NAMES[i])) {
                            actionPrecedenceMap.put(fieldNames[i].getCv_use(),
                                    order);
                            order++;
                        }
                    }
                }
            }
        }

        if (order > 0) {
            status = 1;
        } else {
            status = 0;
        }

        return status;
    }

    /**
     * Initialize the substitution order.
     * 
     * @return
     */
    private static int initSubstituteOrder() {
        AppsDefaults appsDefaults = AppsDefaults.getInstance();
        int status = 0;
        subPrecedence = new ArrayList<String>();

        String value = appsDefaults.getToken(ACTION_TOKEN);

        if (value != null) {
            if (!value.equalsIgnoreCase("NONE")) {

                StringTokenizer st = new StringTokenizer(value, ",");
                while (st.hasMoreTokens()) {
                    String token = st.nextToken();
                    subPrecedence.add(token);
                }
            }
        }

        if (subPrecedence.size() > 0) {
            status = 1;
        } else {
            status = 0;
        }

        return status;
    }

    /**
     * Write the polygon file.
     */
    public static void writePolygons() {
        StringBuilder hourlyBuffer = new StringBuilder();
        StringBuilder persistentBuffer = new StringBuilder();
        MPEDisplayManager manager = MPEDisplayManager.getCurrent();
        PolygonDataManager polyManager = PolygonDataManager.getInstance();

        List<RubberPolyData> polygonList = polyManager.getPolygonList();

        String fieldname = manager.getDisplayFieldType().getCv_use()
                .toUpperCase();
        String persistentFilename = getPolygonDir() + "/DrawPoly" + fieldname;
        String hourlyFilename = getPolygonDir() + "/DrawPoly" + fieldname
                + MPEConstants.DATE_FORMAT.format(manager.getCurrentDate())
                + "z";

        // If the list is empty then return, nothing to do
        if (polygonList.size() > 0) {

            int persistOrder = 0;
            int hourlyOrder = 0;
            int visible = 0;
            System.out.println(polygonList.size() + " polygons in the list");
            for (RubberPolyData data : polygonList) {
                if (data.isPersistent()) {
                    /* Increment the polygon number and record number. */
                    persistOrder++;
                    if (data.isVisible()) {
                        visible = 1;
                    } else {
                        visible = 0;
                    }

                    /* Write the header to the file buffer. */
                    if (data.getAction().equalsIgnoreCase(ACTION_NAMES[0])) {
                        persistentBuffer
                                .append(persistOrder + " " + data.getAction()
                                        + " "
                                        + data.getSubDrawSource().getCv_use()
                                        + " " + data.getNumberPoints() + " "
                                        + visible + "\n");

                    } else {
                        persistentBuffer
                                .append(persistOrder
                                        + " "
                                        + data.getAction()
                                        + " "
                                        + String.format(valueFormat,
                                                data.getPrecipValue()) + " "
                                        + data.getNumberPoints() + " "
                                        + visible + "\n");
                    }

                    /* Write the points to the file buffer. */
                    List<Point> pointList = data.getHrap();
                    for (int i = 0; i < data.getNpoints(); i++) {
                        Point p = pointList.get(i);
                        persistentBuffer.append(p.x + " " + p.y + "\n");
                    }
                } else {
                    hourlyOrder++;
                    if (data.isVisible()) {
                        visible = 1;
                    } else {
                        visible = 0;
                    }

                    /* Write the header to the file buffer. */
                    if (data.getAction().equalsIgnoreCase(ACTION_NAMES[0])) {
                        hourlyBuffer.append(hourlyOrder + " "
                                + data.getAction() + " "
                                + data.getSubDrawSource().getCv_use() + " "
                                + data.getNpoints() + " " + visible + "\n");

                    } else {
                        hourlyBuffer.append(hourlyOrder
                                + " "
                                + data.getAction()
                                + " "
                                + String.format(valueFormat,
                                        data.getPrecipValue()) + " "
                                + data.getNpoints() + " " + visible + "\n");
                    }

                    /* Write the points to the file buffer. */
                    List<Point> pointList = data.getHrap();
                    for (int i = 0; i < data.getNpoints(); i++) {
                        Point p = pointList.get(i);
                        hourlyBuffer.append(p.x + " " + p.y + "\n");
                    }
                }
            }
        }

        // Write the buffers to disk if needed
        File file = new File(persistentFilename);
        if (persistentBuffer.length() > 0) {
            if (file.exists()) {
                file.delete();
            }
            try {
                BufferedWriter out = new BufferedWriter(new FileWriter(file));
                out.write(persistentBuffer.toString());
                out.close();
            } catch (IOException e) {
                // TODO - Log error message here
                e.printStackTrace();
            }
        } else {
            if (file.exists()) {
                file.delete();
            }
        }

        file = new File(hourlyFilename);
        System.out.println("File = " + file.toString());
        if (hourlyBuffer.length() > 0) {
            if (file.exists()) {
                file.delete();
            }
            BufferedWriter out;
            try {
                out = new BufferedWriter(new FileWriter(file));
                out.write(hourlyBuffer.toString());
                out.flush();
                out.close();
            } catch (IOException e) {
                statusHandler.handle(Priority.PROBLEM, e.getLocalizedMessage(),
                        e);
            }
        } else {
            if (file.exists()) {
                file.delete();
            }
        }
    }

    /**
     * Gets the xmrg data from the file.
     * 
     * @param dataType
     *            DisplayFieldData type
     * @return short[][] of data
     * @throws IOException
     */
    public static short[] getXmrgData(DisplayFieldData dataType)
            throws IOException {
        String cv_use = dataType.getCv_use();
        String fname = "";
        if (cv_use.equals("XMRG")) {
            cv_use = cv_use.toLowerCase();
        }
        String dirname = AppsDefaults.getInstance().getToken(
                dataType.getDirToken());

        if (!cv_use.equals("xmrg")) {
            fname = FileUtil.join(
                    dirname,
                    cv_use
                            + MPEConstants.DATE_FORMAT.format(MPEDisplayManager
                                    .getCurrent().getCurrentDate()) + "z");

        } else {
            fname = FileUtil.join(
                    dirname,
                    cv_use
                            + MPEConstants.XMRG_DATE_FORMAT
                                    .format(MPEDisplayManager.getCurrent()
                                            .getCurrentDate()) + "z");
        }
        XmrgFile file = new XmrgFile(fname);
        file.load();
        short[] data = file.getData();

        return data;
    }
}
