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
import java.util.Arrays;
import java.util.HashMap;
import java.util.HashSet;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;

import org.eclipse.swt.graphics.Point;
import org.eclipse.swt.graphics.RGB;
import org.eclipse.swt.widgets.Composite;

import com.raytheon.uf.common.dataplugin.gfe.db.objects.GridLocation;
import com.raytheon.uf.viz.core.IDisplayPane;
import com.raytheon.uf.viz.core.RGBColors;
import com.raytheon.uf.viz.zoneselector.AbstractZoneSelector;
import com.raytheon.uf.viz.zoneselector.ZoneSelectorResource;
import com.raytheon.viz.ghg.monitor.event.AbstractGhgMonitorEvent.GhgEventListener;
import com.raytheon.viz.ghg.monitor.event.GhgMonitorZoneSelectionEvent;
import com.raytheon.viz.ui.input.InputAdapter;
import com.raytheon.viz.ui.input.PanHandler;

/**
 * GHG Spatial Viewer
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date          Ticket#  Engineer  Description
 * ------------- -------- --------- --------------------------------------------
 * Aug 23, 2011           randerso  Initial creation
 * Feb 05, 2016  5316     randerso  Fleshed out implementation
 * Mar 03, 2016  5316     randerso  Added code to switch to appropriate map when
 *                                  product selected from table
 * Jun 23, 2016  5674     randerso  Change to use mouse-base pan and zoom
 * Sep 03, 2019  7919     randerso  Code cleanup.
 *
 * </pre>
 *
 * @author randerso
 */

public class GHGSpatialViewer extends AbstractZoneSelector {

    /**
     * Enumeration defining the available zoom level.
     */
    public enum ZoomLevel {
        /** 1x zoom (No Zoom) */
        ZOOM_1("No Zoom", 1),

        /** 2x zoom */
        ZOOM_2("x2", 2),

        /** 4x zoom */
        ZOOM_4("x4", 4),

        /** 6x zoom */
        ZOOM_6("x6", 6),

        /** 8x zoom */
        ZOOM_8("x8", 8),

        /** 12x zoom */
        ZOOM_12("x12", 12),

        /** 16x zoom */
        ZOOM_16("x16", 16);

        private String displayName;

        private int level;

        private ZoomLevel(String displayName, int level) {
            this.displayName = displayName;
            this.level = level;
        }

        @Override
        public String toString() {
            return displayName;
        }

        /**
         * @return the current zoom level
         */
        public int getZoomLevel() {
            return level;
        }
    }

    private static final Map<String, List<String>> mapConfigure;
    static {
        mapConfigure = new LinkedHashMap<>();
        mapConfigure.put("FIPS", Arrays.asList("Counties", "Marine_Zones"));
        mapConfigure.put("Public", Arrays.asList("Zones"));
        mapConfigure.put("FireWx", Arrays.asList("FireWxZones"));
        mapConfigure.put("Marine",
                Arrays.asList("Marine_Zones", "Offshore_Marine_Zones"));
    }

    private String myWfo;

    private String mapName;

    private RGB fgColor;

    private RGB myWfoOutlineColor;

    private RGB otherWfoOutlineColor;

    private Point pressInfo;

    private List<GhgEventListener> zoneSelectionListenerList = new ArrayList<>();

    private Map<String, Set<String>> mapDataDict;

    /**
     * @param parent
     * @param myWfo
     * @param gloc
     */
    public GHGSpatialViewer(Composite parent, String myWfo, GridLocation gloc) {
        super(parent, gloc, null);
        this.myWfo = myWfo;

        this.backColor = RGBColors.getRGBColor("gray90");
        this.fgColor = RGBColors.getRGBColor("gray40");
        this.myWfoOutlineColor = RGBColors.getRGBColor("yellow");
        this.otherWfoOutlineColor = RGBColors.getRGBColor("black");

        setupMapData();
    }

    private void setupMapData() {
        mapDataDict = new HashMap<>(mapConfigure.size(), 1.0f);
        for (Entry<String, List<String>> entry : mapConfigure.entrySet()) {
            setMap(entry.getKey());
            List<String> zones = getZoneNames();
            if (!zones.isEmpty()) {
                mapDataDict.put(entry.getKey(), new HashSet<>(zones));
            }
        }
    }

    /**
     * @param x
     * @param y
     */
    protected void button1Press(int x, int y) {
        this.pressInfo = new Point(x, y);
    }

    /**
     * @param x
     * @param y
     */
    protected void button1Release(int x, int y) {
        if (this.pressInfo == null) {
            return;
        }
        int diff = Math.abs(x - this.pressInfo.x)
                + Math.abs(y - this.pressInfo.y);
        // click?
        if (diff < 10) {
            List<String> zones = this.selectedZones(x, y);
            if ((this.selectCB != null) && !zones.isEmpty()) {
                this.selectCB.zoneSelected(zones.get(0));
            }
        }
        this.pressInfo = null;
    }

    public void setMap(String mapName) {
        this.mapName = mapName;
        List<String> mapList = mapConfigure.get(mapName);
        super.setMap(mapList, true);

        for (ZoneSelectorResource rsc : mapRscList) {
            rsc.setMyWfo(this.myWfo);
            rsc.setDefaultFillColor(this.fgColor);
            rsc.setWfoOutlineColor(this.myWfoOutlineColor);
            rsc.setOutlineColor(otherWfoOutlineColor);
        }

        setMapInternal(mapRscList);
    }

    public List<String> knownMaps() {
        return new ArrayList<>(mapDataDict.keySet());
    }

    public String getCurrentMap() {
        return this.mapName;
    }

    @Override
    protected void registerHandlers(IDisplayPane pane) {
        super.registerHandlers(pane);
        registerMouseHandler(new InputAdapter() {
            private boolean drag = false;

            @Override
            public boolean handleMouseUp(int x, int y, int mouseButton) {
                if (mouseButton == 1) {
                    if (!drag) {
                        List<String> zones = selectedZones(x, y);
                        GhgMonitorZoneSelectionEvent evt = new GhgMonitorZoneSelectionEvent();
                        evt.setHighlightedZones(zones);
                        fireZoneSelectionEvent(evt);
                    }
                    this.drag = false;
                }
                return false;
            }

            @Override
            public boolean handleMouseDownMove(int x, int y, int mouseButton) {
                if (mouseButton == 1) {
                    this.drag = true;
                }
                return false;
            }
        });
        registerMouseHandler(new PanHandler(this));
    }

    /**
     * @param selectionColor
     * @param highlightedZones
     */
    public void setHighlightedZones(RGB selectionColor,
            String... highlightedZones) {
        List<String> highlightedList = Arrays.asList(highlightedZones);

        // user current map as initial guess
        String mapName = this.mapName;
        Set<String> intersection = new HashSet<>(highlightedList);
        intersection.retainAll(mapDataDict.get(mapName));
        int maxCount = intersection.size();

        // determine which map to use
        int count;
        for (Entry<String, Set<String>> entry : mapDataDict.entrySet()) {
            if (entry.getKey().equals(this.mapName)) {
                continue;
            }
            intersection.clear();
            intersection.addAll(highlightedList);
            intersection.retainAll(entry.getValue());

            count = intersection.size();
            if (count > maxCount) {
                mapName = entry.getKey();
                maxCount = count;
            }
        }

        // update the map if changed
        if (!mapName.equals(this.mapName)) {
            setMap(mapName);
        }

        for (ZoneSelectorResource rsc : mapRscList) {
            rsc.clearZones();
            rsc.setZone(selectionColor, highlightedZones);
        }
    }

    /**
     * Add a listener to the list.
     *
     * @param listener
     */
    public void addSelectionListener(GhgEventListener listener) {
        if (!zoneSelectionListenerList.contains(listener)) {
            zoneSelectionListenerList.add(listener);
        }
    }

    /**
     * Remove a listener from the list.
     *
     * @param listener
     */
    public void removeSelectionListener(GhgEventListener listener) {
        zoneSelectionListenerList.remove(listener);
    }

    /**
     * Fire the map change event.
     *
     * @param event
     *            The GhgMonitorZoneSelectionEvent object
     */
    private void fireZoneSelectionEvent(GhgMonitorZoneSelectionEvent event) {
        for (GhgEventListener listener : zoneSelectionListenerList) {
            listener.notifyUpdate(event);
        }
    }
}
