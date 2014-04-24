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
package com.raytheon.viz.ghg.monitor;

import java.util.ArrayList;
import java.util.List;

import com.raytheon.uf.common.activetable.ActiveTableRecord;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.viz.gfe.core.DataManager;
import com.raytheon.viz.ghg.Activator;
import com.raytheon.viz.ghg.constants.StatusConstants;
import com.raytheon.viz.ghg.monitor.constants.GhgMenuConstants;
import com.raytheon.viz.ghg.monitor.data.GhgConfigData.DataEnum;
import com.raytheon.viz.ghg.monitor.data.GhgConfigData;
import com.raytheon.viz.ghg.monitor.data.GhgData;
import com.raytheon.viz.ghg.monitor.event.GhgMonitorFilterChangeEvent;
import com.raytheon.viz.ghg.monitor.event.GhgMonitorTableSelectionEvent;
import com.raytheon.viz.ghg.monitor.event.GhgMonitorZoneSelectionEvent;
import com.raytheon.viz.ghg.monitor.listener.GhgMonitorFilterChangeListener;
import com.raytheon.viz.ghg.monitor.listener.GhgMonitorTableSelectionListener;
import com.raytheon.viz.ghg.monitor.listener.GhgMonitorZoneSelectionListener;

/**
 * The GHG Monitor display manager.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * May 10, 2010            mpduff     Initial creation
 * Apr 9, 2014  15769      ryu        Moved attribute identifyTestData to configuration, as in A1.
 * 
 * </pre>
 * 
 * @author mpduff
 * @version 1.0
 */

public class GhgDisplayManager {
    private static final transient IUFStatusHandler statusHandler = UFStatus.getHandler(GhgDisplayManager.class);
    /**
     * Single instance of this class type.
     */
    private static GhgDisplayManager instance = null;

    /**
     * Zoom level of the map.
     */
    private int zoomLevel = 0;

    /**
     * Zone type to display.
     */
    private GhgMenuConstants.ShowMap zoneDisplay;

    /**
     * Show label flag.
     */
    private boolean showLabels = false;

    /**
     * List of GhgData records
     */
    private List<GhgData> dataList = new ArrayList<GhgData>();

    private DataEnum sortColumn;

    /**
     * Flag for changes to filter
     */
    private boolean filterChanged = false;

    /**
     * Filter change listener list
     */
    private ArrayList<GhgMonitorFilterChangeListener> filterChangeListenerList = new ArrayList<GhgMonitorFilterChangeListener>();

    /**
     * Map zone selection listener list
     */
    private ArrayList<GhgMonitorZoneSelectionListener> zoneSelectionListenerList = new ArrayList<GhgMonitorZoneSelectionListener>();

    /**
     * Table selection listener list
     */
    private ArrayList<GhgMonitorTableSelectionListener> tableSelectionListenerList = new ArrayList<GhgMonitorTableSelectionListener>();

    /**
     * Private constructor for this singleton.
     */
    private GhgDisplayManager() {

    }

    /**
     * Get the singleton instance of this class.
     * 
     * @return The one instance of this class
     */
    public static synchronized GhgDisplayManager getInstance() {
        if (instance == null) {
            instance = new GhgDisplayManager();
        }

        return instance;
    }

    /**
     * Gets the ActiveTable records and returns them as a List<GhgData>. The
     * list is based on the configuration selections.
     * 
     * @return List<GhgData> list of GhgData objects
     */
    public List<GhgData> getTableData() {
        List<ActiveTableRecord> activeTableList = null;

        dataList.clear();
        try {
            activeTableList = DataManager.getCurrentInstance().getActiveTable();
            for (ActiveTableRecord rec : activeTableList) {
                GhgData data = new GhgData(rec);
                dataList.add(data);
            }
        } catch (VizException e) {
            statusHandler.handle(Priority.PROBLEM,
                    "Error getting ActiveTable", e);
            return new ArrayList<GhgData>();
        }

        return new ArrayList<GhgData>(dataList);
    }

    /**
     * @return the zoomLevel
     */
    public int getZoomLevel() {
        return zoomLevel;
    }

    /**
     * @param zoomLevel
     *            the zoomLevel to set
     */
    public void setZoomLevel(int zoomLevel) {
        this.zoomLevel = zoomLevel;
    }

    /**
     * @return the zoneDisplay
     */
    public GhgMenuConstants.ShowMap getZoneDisplay() {
        return zoneDisplay;
    }

    /**
     * @param zoneDisplay
     *            the zoneDisplay to set
     */
    public void setZoneDisplay(GhgMenuConstants.ShowMap zoneDisplay) {
        this.zoneDisplay = zoneDisplay;
    }

    /**
     * @return the showLabels
     */
    public boolean isShowLabels() {
        return showLabels;
    }

    /**
     * @param showLabels
     *            the showLabels to set
     */
    public void setShowLabels(boolean showLabels) {
        this.showLabels = showLabels;
    }

    /**
     * @return the sortColumn
     */
    public DataEnum getSortColumn() {
        return sortColumn;
    }

    /**
     * @param sortColumn
     *            the sortColumn to set
     */
    public void setSortColumn(DataEnum sortColumn) {
        this.sortColumn = sortColumn;
    }

    /**
     * @return the filterChanged
     */
    public boolean isFilterChanged() {
        return filterChanged;
    }

    /**
     * @param filterChanged
     *            the filterChanged to set
     */
    public void setFilterChanged(boolean filterChanged) {
        this.filterChanged = filterChanged;
        GhgMonitorFilterChangeEvent evt = new GhgMonitorFilterChangeEvent(
                "Filter");
        evt.setFilterChanged(filterChanged);
        fireFilterChangeEvent(evt);
    }

    /**
     * Fire event so listeners are aware of change.
     * 
     * @param evt
     *            The GhgMonitorFilterChangeEvent
     */
    private void fireFilterChangeEvent(GhgMonitorFilterChangeEvent evt) {
        for (GhgMonitorFilterChangeListener listener : filterChangeListenerList) {
            listener.notifyUpdate(evt);
        }
    }

    /**
     * Add a listener to the list.
     * 
     * @param gmcl
     */
    public void addGhgMonitorFilterChangeListener(
            GhgMonitorFilterChangeListener gmcl) {
        filterChangeListenerList.add(gmcl);
    }

    /**
     * Remove a listener from the list.
     * 
     * @param gmcl
     */
    public void removeGhgMonitorChangeListener(
            GhgMonitorFilterChangeListener gmcl) {
        if (filterChangeListenerList.contains(gmcl)) {
            filterChangeListenerList.remove(gmcl);
        }
    }

    /**
     * Add a listener to the list.
     * 
     * @param gmzsl
     */
    public void addGhgMonitorZoneSelectionListener(
            GhgMonitorZoneSelectionListener gmzsl) {
        zoneSelectionListenerList.add(gmzsl);
    }

    /**
     * Remove a listener from the list.
     * 
     * @param gmzsl
     */
    public void removeGhgMonitorZoneSelectionListener(
            GhgMonitorZoneSelectionListener gmzsl) {
        if (zoneSelectionListenerList.contains(gmzsl)) {
            zoneSelectionListenerList.remove(gmzsl);
        }
    }

    /**
     * Add a listener to the list.
     * 
     * @param gmtsl
     */
    public void addGhgMonitorTableSelectionListener(
            GhgMonitorTableSelectionListener gmtsl) {
        tableSelectionListenerList.add(gmtsl);
    }

    /**
     * Remove a listener from the list.
     * 
     * @param gmtsl
     */
    public void removeGhgMonitorTableSelectionListener(
            GhgMonitorTableSelectionListener gmtsl) {
        if (tableSelectionListenerList.contains(gmtsl)) {
            tableSelectionListenerList.remove(gmtsl);
        }
    }

    /**
     * Fire the table change event.
     * 
     * @param evt
     *            The GhgMonitorTableSelectionEvent object
     */
    public void fireTableSelectionEvent(GhgMonitorTableSelectionEvent evt) {
        for (GhgMonitorTableSelectionListener listener : tableSelectionListenerList) {
            listener.notifyUpdate(evt);
        }
    }

    /**
     * Fire the map change event.
     * 
     * @param evt
     *            The GhgMonitorZoneSelectionEvent object
     */
    public void fireMapChangeEvent(GhgMonitorZoneSelectionEvent evt) {
        for (GhgMonitorZoneSelectionListener listener : zoneSelectionListenerList) {
            listener.notifyUpdate(evt);
        }
    }
}
