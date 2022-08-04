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
package com.raytheon.uf.viz.zoneselector;

import java.util.ArrayList;
import java.util.List;

import org.eclipse.swt.SWT;
import org.eclipse.swt.events.DisposeEvent;
import org.eclipse.swt.events.DisposeListener;
import org.eclipse.swt.graphics.RGB;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.ScrollBar;
import org.geotools.coverage.grid.GeneralGridEnvelope;
import org.geotools.coverage.grid.GridGeometry2D;
import org.geotools.geometry.GeneralEnvelope;
import org.geotools.referencing.operation.builder.GridToEnvelopeMapper;
import org.locationtech.jts.geom.Coordinate;
import org.locationtech.jts.geom.Envelope;
import org.opengis.metadata.spatial.PixelOrientation;
import org.opengis.referencing.datum.PixelInCell;
import org.opengis.referencing.operation.MathTransform;

import com.raytheon.uf.common.dataplugin.gfe.db.objects.GridLocation;
import com.raytheon.uf.common.geospatial.MapUtil;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.viz.core.IDisplayPane;
import com.raytheon.uf.viz.core.IExtent;
import com.raytheon.uf.viz.core.datastructure.LoopProperties;
import com.raytheon.uf.viz.core.drawables.IRenderableDisplay;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.map.MapDescriptor;
import com.raytheon.uf.viz.core.maps.MapStore;
import com.raytheon.uf.viz.core.maps.display.MapRenderableDisplay;
import com.raytheon.uf.viz.core.maps.rsc.DbMapResourceData;
import com.raytheon.uf.viz.core.rsc.LoadProperties;
import com.raytheon.viz.ui.panes.PaneManager;

/**
 * Abstract base class for ZoneSelector and GHGSpatialViewer
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date          Ticket#  Engineer  Description
 * ------------- -------- --------- --------------------------------------------
 * Aug 23, 2011           randerso  Initial creation
 * May 30, 2013  2028     randerso  Fixed date line issue with map display
 * Jan 07, 2014  2662     randerso  Fixed limitZones (subDomainUGCs) support
 * Aug 31, 2015  4749     njensen   Set selectCB to null on dispose
 * Feb 05, 2016  5316     randerso  Moved into separate package. Additional
 *                                  changes to support use in GHG Monitor,
 *                                  MakeHazard, and ZoneCombiner
 * Jun 23, 2016  5674     randerso  Change to use mouse-base pan and zoom
 * Sep 03, 2019  7919     randerso  Code cleanup.
 *
 * </pre>
 *
 * @author randerso
 */

public abstract class AbstractZoneSelector extends PaneManager {
    /**
     * Interface for zone selection listener
     */
    public static interface IZoneSelectionListener {
        /**
         * Called when a zone is selected
         *
         * @param zone
         *            selected zone
         */
        public void zoneSelected(String zone);
    }

    private static final IUFStatusHandler statusHandler = UFStatus
            .getHandler(AbstractZoneSelector.class);

    protected static final int SCROLL_MAX = 10_000;

    private static final double ZOOM_LIMIT = 1.0 / 16.0;

    protected Composite parent;

    protected IZoneSelectionListener selectCB;

    protected final GridLocation gloc;

    private LoopProperties loopProperties;

    protected RGB noZoneColor;

    protected RGB backColor;

    protected List<String> mapNames;

    protected List<ZoneSelectorResource> mapRscList;

    // list of zones in the map
    protected List<String> zoneNames;

    // list of only valid zones, or None (sub)
    private List<String> limitZoneNames;

    // state flag, whether to label the zones
    private boolean labelZones;

    protected Composite map;

    protected ScrollBar hBar;

    protected ScrollBar vBar;

    protected double currShiftX = 0;

    protected double currShiftY = 0;

    /**
     * @param parent
     * @param gloc
     * @param selectCB
     */
    public AbstractZoneSelector(Composite parent, GridLocation gloc,
            IZoneSelectionListener selectCB) {
        super();
        this.parent = parent;
        this.selectCB = selectCB;

        this.gloc = gloc;

        this.loopProperties = new LoopProperties();
        this.mapRscList = new ArrayList<>();
        this.zoneNames = new ArrayList<>(0);

        setupCanvas();
        this.parent.addDisposeListener(new DisposeListener() {

            @Override
            public void widgetDisposed(DisposeEvent e) {
                AbstractZoneSelector.this.selectCB = null;
                dispose();
            }
        });
    }

    /**
     * Set the zoomLevel
     *
     * @param zoomLevel
     */
    public void setZoomLevel(double zoomLevel) {
        updateZoom(zoomLevel);
    }

    /**
     * Get the zoomLevel
     *
     * @return the zoomLevel
     */
    public double getZoomLevel() {
        IDisplayPane pane = getActiveDisplayPane();
        IRenderableDisplay rDisplay = pane.getRenderableDisplay();
        IExtent extent = rDisplay.getExtent();
        return extent.getScale();
    }

    /**
     * Set labelZones
     *
     * @param labelZones
     */
    public void setLabelZones(boolean labelZones) {
        if (labelZones == this.labelZones) {
            return;
        }
        this.labelZones = labelZones;
        for (ZoneSelectorResource mapRsc : this.mapRscList) {
            mapRsc.setLabelZones(labelZones);
        }
    }

    /**
     * Get labelZones
     *
     * @return labelZones
     */
    public boolean isLabelZones() {
        return this.labelZones;
    }

    /**
     * Command to limit the set of zones that are allowed to be part of the zone
     * combinations and maps. If set to None, then all items in the maps are
     * allowed.
     *
     * @param limitZones
     *            list of allowable zones
     */
    public void setLimitZones(List<String> limitZones) {
        if ((this.limitZoneNames == null) && (limitZones == null)) {
            return;
        }

        if ((this.limitZoneNames != null)
                && this.limitZoneNames.equals(limitZones)) {
            return;
        }
        this.limitZoneNames = limitZones;
        for (ZoneSelectorResource mapRsc : this.mapRscList) {
            mapRsc.setLimitZones(limitZones);
        }
    }

    @Override
    public LoopProperties getLoopProperties() {
        return this.loopProperties;
    }

    private void setupCanvas() {

        map = new Composite(parent, SWT.BORDER);
        map.setLayout(new GridLayout());
        map.setLayoutData(new GridData(SWT.FILL, SWT.FILL, true, true));
        initializeComponents(this, map);
    }

    /**
     *
     */
    protected void updateZoom(double zoomLevel) {
        // Resets the map to be full view before performing zoom
        IDisplayPane pane = getActiveDisplayPane();

        if (zoomLevel < ZOOM_LIMIT) {
            zoomLevel = ZOOM_LIMIT;
        }

        if (zoomLevel > 1) {
            zoomLevel = 1;
        }

        IRenderableDisplay rDisplay = pane.getRenderableDisplay();
        IExtent extent = rDisplay.getExtent();

        if (zoomLevel == 1.0) {
            pane.scaleToClientArea();
            currShiftX = 0;
            currShiftY = 0;
        } else {
            extent.scale(zoomLevel / extent.getScale());
        }

        this.refresh();
    }

    protected void setMap(List<String> mapNames, boolean clipToDomain) {
        this.mapNames = mapNames;
        List<ZoneSelectorResource> mapRscList = new ArrayList<>();
        for (String mapName : mapNames) {
            String bundlePath = MapStore.findBundlePath(mapName);
            if (bundlePath == null) {
                statusHandler.handle(Priority.PROBLEM,
                        "No map bundle found for: " + mapName);
                continue;
            }
            try {
                DbMapResourceData rscData = (DbMapResourceData) MapStore
                        .retrieveMap(bundlePath).getResourceData();

                ZoneSelectorResource rsc = new ZoneSelectorResource(rscData,
                        new LoadProperties(), gloc, this.limitZoneNames,
                        noZoneColor, clipToDomain);
                mapRscList.add(rsc);
            } catch (VizException e) {
                statusHandler.handle(Priority.PROBLEM,
                        "Error loading map: " + mapName, e);
            }
        }
        this.mapRscList = mapRscList;
    }

    // main routine, mapdata is a MapData object, comboDict is a dictionary
    // of zoneIDs for keys, and group numbers for values, colors is a list
    // of colors to use for the groups. Sets the zone combiner map and
    // current combinations.
    protected void setMapInternal(List<ZoneSelectorResource> mapRscList) {
        this.mapRscList = mapRscList;

        try {
            Envelope env = getBoundingEnvelope();

            Coordinate llCrs = new Coordinate(env.getMinX(), env.getMinY());
            Coordinate urCrs = new Coordinate(env.getMaxX(), env.getMaxY());

            Coordinate llGrid = MapUtil.nativeToGridCoordinate(llCrs,
                    PixelOrientation.CENTER, gloc);
            Coordinate urGrid = MapUtil.nativeToGridCoordinate(urCrs,
                    PixelOrientation.CENTER, gloc);

            GeneralEnvelope ge = new GeneralEnvelope(2);
            ge.setCoordinateReferenceSystem(gloc.getCrs());
            ge.setRange(0, Math.min(llCrs.x, urCrs.x),
                    Math.max(llCrs.x, urCrs.x));
            ge.setRange(1, Math.min(llCrs.y, urCrs.y),
                    Math.max(llCrs.y, urCrs.y));

            int nx = (int) Math.abs(Math.ceil(urGrid.x - llGrid.x));
            int ny = (int) Math.abs(Math.ceil(urGrid.y - llGrid.y));

            GeneralGridEnvelope gr = new GeneralGridEnvelope(new int[] { 0, 0 },
                    new int[] { nx, ny }, false);

            GridToEnvelopeMapper mapper = new GridToEnvelopeMapper();
            mapper.setEnvelope(ge);
            mapper.setGridRange(gr);
            mapper.setPixelAnchor(PixelInCell.CELL_CENTER);
            mapper.setReverseAxis(new boolean[] { false, true });
            MathTransform mt = mapper.createTransform();

            GridGeometry2D gridGeom = new GridGeometry2D(
                    PixelInCell.CELL_CORNER, mt, ge, null);

            MapDescriptor md = new MapDescriptor(gridGeom);
            MapRenderableDisplay mapRenderableDisplay = new MapRenderableDisplay(
                    md);
            mapRenderableDisplay.setBackgroundColor(this.backColor);
            IDisplayPane pane = this.getActiveDisplayPane();
            if (pane == null) {
                pane = this.addPane(mapRenderableDisplay);
            } else {
                pane.setRenderableDisplay(mapRenderableDisplay);
            }
            pane.scaleToClientArea();

            for (ZoneSelectorResource mapRsc : mapRscList) {
                mapRsc.setLabelZones(this.labelZones);
                md.getResourceList().add(mapRsc);
            }
        } catch (Exception e) {
            statusHandler.handle(Priority.PROBLEM,
                    "Error creating map descriptor", e);
        }

        // extract out the zone names from the map
        this.initZoneNames();
        this.refresh();
    }

    private Envelope getBoundingEnvelope() {
        Envelope env = new Envelope();
        for (ZoneSelectorResource mapRsc : this.mapRscList) {
            env.expandToInclude(mapRsc.getBoundingEnvelope());
        }
        double delta = Math.max(env.getWidth(), env.getHeight()) * 0.02;
        env.expandBy(delta);
        return env;
    }

    // creates list of zone names from shapefile
    private void initZoneNames() {
        List<String> zoneNames = new ArrayList<>();
        for (ZoneSelectorResource mapRsc : mapRscList) {
            for (String ean : mapRsc.getZoneNames()) {
                if ((ean != null) && !ean.isEmpty()) {
                    // ensure we only add a zone once, and that it should
                    // be included (limitZoneNames is none or is in list)
                    if (!zoneNames.contains(ean)
                            && ((this.limitZoneNames == null)
                                    || this.limitZoneNames.contains(ean))) {
                        zoneNames.add(ean);
                    }
                }
            }
        }
        this.zoneNames = zoneNames;
    }

    /**
     * @return all zone names
     */
    public List<String> getZoneNames() {
        return this.zoneNames;
    }

    protected List<String> selectedZones(int x, int y) {
        Coordinate c = translateClick(x, y);

        List<String> retZones = new ArrayList<>();
        for (ZoneSelectorResource mapRsc : mapRscList) {
            List<String> zones = mapRsc.getSelectedZones(c);
            retZones.addAll(zones);
        }
        return retZones;
    }
}
