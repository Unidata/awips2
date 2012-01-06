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

import java.io.File;
import java.util.ArrayList;
import java.util.Date;
import java.util.List;

import com.raytheon.viz.mpe.MPEConstants;
import com.raytheon.viz.mpe.ui.DisplayFieldData;
import com.raytheon.viz.mpe.ui.MPEDisplayManager;

/**
 * Holds the polygon data. TODO: Look into notifying listeners when new one
 * added. Find out if we need to store time for edit (Figure out how
 * DeletePolygonDlg gets the RubberPolyData)
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jul 10, 2009 2685       mpduff      Initial creation
 * 
 * </pre>
 * 
 * @author mpduff
 * @version 1.0
 */

public class PolygonDataManager {
    /** Instance of this class. */
    private static PolygonDataManager instance = null;

    /** List of polygon data objects. */
    private List<RubberPolyData> polygonList = null;

    /** Private constructor. */
    private PolygonDataManager() {

    }

    /**
     * Returns the instance of this class.
     * 
     * @return instance The instance of this class
     */
    public final synchronized static PolygonDataManager getInstance() {
        if (instance == null) {
            instance = new PolygonDataManager();
        }

        return instance;
    }

    /**
     * Add a polygon to the list.
     * 
     * @param data
     *            RubberPolyData object to add to the list
     */
    public void addPolygon(RubberPolyData data) {
        if (polygonList == null) {
            polygonList = new ArrayList<RubberPolyData>();
        }
        polygonList.add(data);
    }

    /**
     * Get the list of polygons.
     * 
     * @param fieldData
     *            The DisplayFieldData type
     * @param date
     *            The date and hour of the data
     * @return List<RubberPolyData> The list of polygons
     */
    public List<RubberPolyData> getPolygons(DisplayFieldData fieldData,
            Date date) {
        MPEDisplayManager dispMgr = MPEDisplayManager.getCurrent();
        PolygonDataManager polyManager = PolygonDataManager.getInstance();

        List<RubberPolyData> dataList = polyManager.getPolygonList();
        String polygonDir = PrecipPolyUtils.getPolygonDir();

        /* Get the fieldname corresponding to the field. */
        String fieldname = dispMgr.getDisplayFieldType().getCv_use();

        /* Build the polygon filename. */
        String filename = String.format("%s/DrawPoly%s%sz", polygonDir,
                fieldname, MPEConstants.DATE_FORMAT.format(date));

        /* Build the persistent polygon filename. */
        String persistentFilename = String.format("%s/DrawPoly%s", polygonDir,
                fieldname);

        /* Check to see if the hourly polygon file exists. */
        File hourlyFile = new File(filename);

        dataList.clear();
        if (hourlyFile.exists()) {
            dataList = PrecipPolyUtils.readPolyData(hourlyFile, dataList,
                    fieldData, false);
        }

        /* Check to see if the persistent polygon file exists. */
        File persistentFile = new File(persistentFilename);

        if (persistentFile.exists()) {
            dataList = PrecipPolyUtils.readPolyData(persistentFile, dataList,
                    fieldData, true);
        }

        /*
         * If the user has specified any polygon order preferences, reorder the
         * list to reflect these preferences.
         */
        if (dataList.size() > 0) {
            dataList = PrecipPolyUtils.orderPolygons(dataList);
        }

        /*
         * Set the polygons so that they are numbered sequentially from 1 to N,
         * where N is the number of polygons in the linked list.
         */
        for (int i = 0; i < dataList.size(); i++) {
            dataList.get(i).setPolygonNumber(i + 1);
            PrecipPolyUtils.writeDrawPrecipData(dataList.get(i), false);
        }

        return dataList;
    }

    /**
     * @return the polygonList
     */
    public List<RubberPolyData> getPolygonList() {
        if (polygonList == null) {
            polygonList = new ArrayList<RubberPolyData>();
        }
        return polygonList;
    }

    /**
     * @param polygonList
     *            the polygonList to set
     */
    public void setPolygonList(List<RubberPolyData> polygonList) {
        this.polygonList = polygonList;
    }

    /**
     * Get the last polygon in the list.
     * 
     * @return the last polygon in the list
     */
    public RubberPolyData getLastPolygon() {
        return polygonList.get(polygonList.size() - 1);
    }
}
