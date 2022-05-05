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
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jan 7, 2015  16954      cgobs      Fix for cv_use issue - using getFieldName() in certain parts.
 * Mar 10, 2015 14554      snaples    Made getHourlyEditFile public to be accessed from Save Best Estimate.
 * </pre>
 **/
package com.raytheon.viz.mpe.ui.dialogs.polygon;

import java.awt.Point;
import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.File;
import java.io.FileReader;
import java.io.FileWriter;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.Comparator;
import java.util.Date;
import java.util.HashMap;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.stream.Collectors;

import com.raytheon.uf.common.ohd.AppsDefaults;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.viz.mpe.MPEDateFormatter;
import com.raytheon.viz.mpe.ui.DisplayFieldData;
import com.raytheon.viz.mpe.ui.MPEDisplayManager;
import com.raytheon.viz.mpe.ui.dialogs.polygon.RubberPolyData.PolygonEditAction;

/**
 * 
 * Utility functions for retrieving, applying, and saving polygone edits
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Dec 3, 2012             mschenke    Initial creation
 * July 9, 2013     #2172  bkowal      Set a polygon edit flag whenever new polygon
 *                                     files are written.
 * Jul 15, 2013  15963     snaples     Removed polygon edit flag, and 
 *                                     removed unneeded Constant for Backward compatibility.
 * Feb 15, 2016  5338      bkowal      Keep track of the remaining persistent polygons just
 *                                     in case a refresh is required.
 * Oct 06, 2017  6407      bkowal      Cleanup. Updates to support GOES-R SATPRE.
 * 
 * 
 * </pre>
 * 
 * @author mschenke
 */
public class PolygonEditManager {

    private static final IUFStatusHandler statusHandler = UFStatus
            .getHandler(PolygonEditManager.class);
    
    private PolygonEditManager() {
    }

    /**
     * Comparator class that looks at mpe_polygon_action_order for ordering
     * preferences
     */
    private static class ActionOrderingComparator
            implements Comparator<RubberPolyData> {

        private static List<PolygonEditAction> actionPrecedence = new ArrayList<>();

        static {
            String none = "None";
            String actionOrder = AppsDefaults.getInstance()
                    .getToken("mpe_polygon_action_order", none);
            if (actionOrder != null && !actionOrder.equalsIgnoreCase(none)) {
                String[] actions = actionOrder.split("[,]");
                for (String action : actions) {
                    try {
                        actionPrecedence.add(PolygonEditAction.valueOf(action));
                    } catch (Throwable t) {
                        // Ignore, bad action in file
                    }
                }
            }
        }

        @Override
        public int compare(RubberPolyData o1, RubberPolyData o2) {
            return actionPrecedence.indexOf(o1.getEditAction())
                    - actionPrecedence.indexOf(o2.getEditAction());
        }

    }

    /**
     * Comparator class that looks at mpe_polygon_field_order for substitution
     * ordering preferences
     */
    private static class SubOrderingComparator
            implements Comparator<RubberPolyData> {

        private static Map<DisplayFieldData, Integer> subPrecedence = new HashMap<>();

        static {
            String none = "None";
            String fieldOrder = AppsDefaults.getInstance()
                    .getToken("mpe_polygon_field_order", none);
            if (fieldOrder != null && !fieldOrder.equalsIgnoreCase(none)) {
                int order = 0;
                String[] fields = fieldOrder.split("[,]");
                for (String field : fields) {
                    DisplayFieldData fieldData = DisplayFieldData
                            .fromString(field);
                    if (fieldData != null) {
                        subPrecedence.put(fieldData, order);
                        order += 1;
                    }
                }
            }
        }

        @Override
        public int compare(RubberPolyData o1, RubberPolyData o2) {
            Integer subPrecedence1 = subPrecedence.get(o1.getSubDrawSource());
            if (subPrecedence1 == null) {
                subPrecedence1 = -1;
            }
            Integer subPrecedence2 = subPrecedence.get(o2.getSubDrawSource());
            if (subPrecedence2 == null) {
                subPrecedence2 = -1;
            }

            return subPrecedence1 - subPrecedence2;
        }
    }

    private static final Set<IPolygonEditsChangedListener> listeners = new LinkedHashSet<>();

    public static void registerListener(IPolygonEditsChangedListener listener) {
        synchronized (listeners) {
            listeners.add(listener);
        }
    }

    public static void unregisterListener(
            IPolygonEditsChangedListener listener) {
        synchronized (listeners) {
            listeners.remove(listener);
        }
    }

    /**
     * Reads persisted polygon edit objects for the fieldData for the specified
     * date
     * 
     * @param fieldData
     * @param date
     * @return
     */
    public static List<RubberPolyData> getPolygonEdits(
            DisplayFieldData fieldData, Date date) {
        List<RubberPolyData> polygonEdits = new ArrayList<>();

        /* Check to see if the hourly polygon file exists. */
        File hourlyFile = getHourlyEditFile(fieldData, date);
        if (hourlyFile.exists()) {
            polygonEdits.addAll(readPolygonEdits(hourlyFile, false));
        }

        /* Check to see if the persistent polygon file exists. */
        File persistentFile = getPersistentEditFile(fieldData, date);
        if (persistentFile.exists()) {
            polygonEdits.addAll(readPolygonEdits(persistentFile, true));
        }

        orderPolygonEdits(polygonEdits);

        return polygonEdits;
    }

    /**
     * Sorts the polygon edits based on action and substitution preferences
     * 
     * @param polygonEdits
     */
    private static void orderPolygonEdits(List<RubberPolyData> polygonEdits) {
        Collections.sort(polygonEdits, new ActionOrderingComparator());
        Collections.sort(polygonEdits, new SubOrderingComparator());
    }

    public static void writePolygonEdits(DisplayFieldData fieldData, Date date,
            List<RubberPolyData> polygonEdits, boolean persistentRemoved) {
        orderPolygonEdits(polygonEdits);
        List<RubberPolyData> notifyPolygonEdits = new ArrayList<>(polygonEdits);
        File hourlyFile = getHourlyEditFile(fieldData, date);
        StringBuilder hourlyBuffer = new StringBuilder();

        List<RubberPolyData> persistentRemaining = new ArrayList<>();
        File persistentFile = getPersistentEditFile(fieldData, date);
        StringBuilder persistentBuffer = new StringBuilder();

        if (!polygonEdits.isEmpty()) {
            int persistOrder = 0;
            int hourlyOrder = 0;

            for (RubberPolyData polyEdit : polygonEdits) {
                StringBuilder toUse = null;
                int idx = 0;
                if (polyEdit.isPersistent()) {
                    persistentRemaining.add(polyEdit);
                    persistOrder++;
                    toUse = persistentBuffer;
                    idx = persistOrder;
                } else {
                    hourlyOrder++;
                    toUse = hourlyBuffer;
                    idx = hourlyOrder;
                }

                PolygonEditAction editAction = polyEdit.getEditAction();
                DisplayFieldData subDrawSource = polyEdit.getSubDrawSource();
                double precipValue = polyEdit.getPrecipValue();
                Point[] editPoints = polyEdit.getEditPoints();
                boolean visible = polyEdit.isVisible();

                String polyEditStr = editAction.toPrettyName() + " "
                        + (subDrawSource != null ? subDrawSource.getFieldName()
                                : String.format("%6.2f", precipValue))
                        + " " + editPoints.length + " " + (visible ? "1" : "0");
                toUse.append(idx + " " + polyEditStr + "\n");
                for (Point p : editPoints) {
                    toUse.append(p.x + " " + p.y + "\n");
                }
            }
        }

        Path containingPath = null;
        // Write persistent file
        if (persistentFile.exists()) {
            persistentFile.delete();
        }
        if (persistentBuffer.length() > 0) {
            try {
                containingPath = persistentFile.toPath().getParent();
                if (!Files.exists(containingPath)) {
                    Files.createDirectories(containingPath);
                }
            } catch (IOException e) {
                statusHandler.error("Failed to create directory: "
                        + containingPath.toString() + ".", e);
                return;
            }
            if (Files.exists(containingPath)) {
                /*
                 * Only write the file if the containing directory exists (was
                 * successfully created).
                 */
                try (BufferedWriter out = new BufferedWriter(
                        new FileWriter(persistentFile))) {
                    out.write(persistentBuffer.toString());
                } catch (IOException e) {
                    statusHandler.error(
                            "Failed to write persistent polygon file: "
                                    + persistentFile.getAbsolutePath() + ".",
                            e);
                    return;
                }
            }
        }

        // Write hourly file
        if (hourlyFile.exists()) {
            hourlyFile.delete();
        }

        if (hourlyBuffer.length() > 0) {
            boolean written = false;
            try {
                containingPath = hourlyFile.toPath().getParent();
                if (!Files.exists(containingPath)) {
                    Files.createDirectories(containingPath);
                }
            } catch (IOException e) {
                statusHandler.error("Failed to create directory: "
                        + containingPath.toString() + ".", e);
                if (persistentBuffer.length() <= 0) {
                    /*
                     * No persistent polygons have been written.
                     */
                    return;
                }
                /*
                 * Cannot just exit on failure here due to the subset of
                 * polygons that have successfully been written.
                 */
            }
            if (Files.exists(containingPath)) {
                /*
                 * Only write the file if the containing directory exists (was
                 * successfully created).
                 */
                try (BufferedWriter out = new BufferedWriter(
                        new FileWriter(hourlyFile))) {
                    out.write(hourlyBuffer.toString());
                    written = true;
                } catch (IOException e) {
                    statusHandler.error(
                            "Failed to write hourly polygon file: "
                                    + persistentFile.getAbsolutePath() + ".",
                            e);
                    if (persistentBuffer.length() <= 0) {
                        /*
                         * No persistent polygons have been written.
                         */
                        return;
                    }
                    /*
                     * Cannot just exit on failure here due to the subset of
                     * polygons that have successfully been written.
                     */
                }
            }
            if (!written) {
                /*
                 * Remove all of the non-persistent polygons from the
                 * notification list.
                 */
                notifyPolygonEdits = notifyPolygonEdits.stream()
                        .filter((p) -> !p.isPersistent())
                        .collect(Collectors.toList());
            }
        }

        // Notify listeners of the polygon edit changes
        Collection<IPolygonEditsChangedListener> toNotify;
        synchronized (listeners) {
            toNotify = new LinkedHashSet<>(listeners);
        }
        if (!persistentRemoved) {
            persistentRemaining = null;
        }
        for (IPolygonEditsChangedListener listener : toNotify) {
            listener.polygonEditsChanged(fieldData, date,
                    new ArrayList<>(notifyPolygonEdits), persistentRemaining);
        }
    }

    public static File getHourlyEditFile(DisplayFieldData fieldData,
            Date date) {
        String fieldname = fieldData.getFieldName();
        String polygonDir = MPEDisplayManager.getPolygonEditDir();

        /* Build the polygon filename. */
        String filename = String.format("%s/DrawPoly%s%sz", polygonDir,
                fieldname, MPEDateFormatter.format_yyyyMMddHH(date));
        return new File(filename);
    }

    private static File getPersistentEditFile(DisplayFieldData fieldData,
            Date date) {
        String fieldname = fieldData.getFieldName();
        String polygonDir = MPEDisplayManager.getPolygonEditDir();
        /* Build the persistent polygon filename. */
        String persistentFilename = String.format("%s/DrawPoly%s", polygonDir,
                fieldname);
        return new File(persistentFilename);
    }

    /**
     * @param polygonEditFile
     * @return
     */
    private static List<RubberPolyData> readPolygonEdits(File polygonEditFile,
            boolean persistent) {
        List<RubberPolyData> polygonEdits = new ArrayList<>();
        String delimeter = "\\s+";
        try (BufferedReader in = new BufferedReader(
                new FileReader(polygonEditFile))) {
            String line = null;
            while ((line = in.readLine()) != null) {
                String[] pieces = line.trim().split(delimeter);
                if (pieces.length == 5) {
                    // Create new poly data and set current data
                    PolygonEditAction editAction = PolygonEditAction
                            .valueOf(pieces[1].toUpperCase());
                    int numPoints = Integer.parseInt(pieces[3]);
                    Point[] editPoints = new Point[numPoints];
                    for (int i = 0; i < numPoints; ++i) {
                        String pointLine = in.readLine();
                        String[] xy = pointLine.trim().split(delimeter);
                        if (xy.length == 2) {
                            // Add point to polygonEditPoints
                            int hrapx = Integer.parseInt(xy[0]);
                            int hrapy = Integer.parseInt(xy[1]);

                            editPoints[i] = new Point(hrapx, hrapy);
                        }
                    }
                    boolean visible = (!"0".equals(pieces[4]));

                    if (editAction == PolygonEditAction.SUB) {
                        String subCvUse = pieces[2];
                        DisplayFieldData subData = null;
                        for (DisplayFieldData fieldData : DisplayFieldData
                                .values()) {
                            if (fieldData.getFieldName()
                                    .equalsIgnoreCase(subCvUse)) {
                                subData = fieldData;
                                break;
                            }
                        }
                        polygonEdits.add(new RubberPolyData(editAction, subData,
                                -999.0, editPoints, visible, persistent));
                    } else {
                        double precipValue = Double.parseDouble(pieces[2]);
                        polygonEdits.add(new RubberPolyData(editAction, null,
                                precipValue, editPoints, visible, persistent));
                    }
                }
            }
        } catch (IOException e) {
            statusHandler.error("Failed to read the polygon edits from: "
                    + polygonEditFile + ".", e);

        }
        return polygonEdits;
    }
}