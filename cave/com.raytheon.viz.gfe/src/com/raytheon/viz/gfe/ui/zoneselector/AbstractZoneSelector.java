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
package com.raytheon.viz.gfe.ui.zoneselector;

import java.util.ArrayList;
import java.util.List;

import org.eclipse.swt.SWT;
import org.eclipse.swt.events.DisposeEvent;
import org.eclipse.swt.events.DisposeListener;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.events.SelectionListener;
import org.eclipse.swt.graphics.RGB;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.ScrollBar;
import org.geotools.coverage.grid.GeneralGridEnvelope;
import org.geotools.coverage.grid.GridGeometry2D;
import org.geotools.geometry.GeneralEnvelope;
import org.geotools.referencing.operation.builder.GridToEnvelopeMapper;
import org.opengis.coverage.grid.GridEnvelope;
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
import com.raytheon.uf.viz.core.drawables.IDescriptor;
import com.raytheon.uf.viz.core.drawables.IRenderableDisplay;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.map.MapDescriptor;
import com.raytheon.uf.viz.core.maps.MapStore;
import com.raytheon.uf.viz.core.maps.display.MapRenderableDisplay;
import com.raytheon.uf.viz.core.maps.rsc.DbMapResourceData;
import com.raytheon.uf.viz.core.rsc.LoadProperties;
import com.raytheon.viz.gfe.ui.zoneselector.ZoneSelector.IZoneSelectionListener;
import com.raytheon.viz.ui.panes.PaneManager;
import com.vividsolutions.jts.geom.Coordinate;
import com.vividsolutions.jts.geom.Envelope;

/**
 * Abstract base class for ZoneSelector and GHGSpatialViewer
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Aug 23, 2011            randerso    Initial creation
 * May 30, 2013 #2028      randerso    Fixed date line issue with map display
 * Jan 07, 2014 #2662      randerso    Fixed limitZones (subDomainUGCs) support
 * 
 * </pre>
 * 
 * @author randerso
 * @version 1.0
 */

public abstract class AbstractZoneSelector extends PaneManager {
    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(AbstractZoneSelector.class);

    protected static final int SCROLL_MAX = 10000;

    private static final double ZOOM_LIMIT = 0.1;

    protected Composite parent;

    protected IZoneSelectionListener selectCB;

    protected GridLocation gloc;

    private LoopProperties loopProperties;

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

    protected double zoomLevel = 1;

    protected double currShiftX = 0;

    protected double currShiftY = 0;

    /**
     * @param parent
     * @param gloc
     * @param selectCB
     */
    public AbstractZoneSelector(Composite parent, GridLocation gloc,
            IZoneSelectionListener selectCB) {
        this.parent = parent;
        this.gloc = gloc;
        this.selectCB = selectCB;

        this.loopProperties = new LoopProperties();
        this.mapRscList = new ArrayList<ZoneSelectorResource>();
        this.zoneNames = new ArrayList<String>(0);

        setupCanvas();
        this.parent.addDisposeListener(new DisposeListener() {

            @Override
            public void widgetDisposed(DisposeEvent e) {
                dispose();
            }
        });
    }

    public void setZoomLevel(double zoomLevel) {
        this.zoomLevel = zoomLevel;
        updateZoom();
    }

    public double getZoomLevel() {
        return this.zoomLevel;
    }

    public void setLabelZones(boolean labelZones) {
        if (labelZones == this.labelZones) {
            return;
        }
        this.labelZones = labelZones;
        for (ZoneSelectorResource mapRsc : this.mapRscList) {
            mapRsc.setLabelZones(labelZones);
        }
    }

    // command to limit the set of zones that are allowed to be part of the
    // zone combinations and maps. If set to None, then all items in
    // the maps are allowed.
    public void setLimitZones(List<String> limitZones) {
        if (this.limitZoneNames == null && limitZones == null) {
            return;
        }

        if (this.limitZoneNames != null
                && this.limitZoneNames.equals(limitZones)) {
            return;
        }
        this.limitZoneNames = limitZones;
        for (ZoneSelectorResource mapRsc : this.mapRscList) {
            mapRsc.setLimitZones(limitZones);
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.viz.ui.panes.PaneManager#getLoopProperties()
     */
    @Override
    public LoopProperties getLoopProperties() {
        return this.loopProperties;
    }

    private void setupCanvas() {

        map = new Composite(parent, SWT.BORDER | SWT.H_SCROLL | SWT.V_SCROLL);
        map.setLayout(new GridLayout());
        map.setLayoutData(new GridData(SWT.FILL, SWT.FILL, true, true));
        initializeComponents(this, map);

        hBar = map.getHorizontalBar();
        vBar = map.getVerticalBar();

        hBar.setValues(0, 0, SCROLL_MAX, SCROLL_MAX, SCROLL_MAX / 10,
                SCROLL_MAX);
        vBar.setValues(0, 0, SCROLL_MAX, SCROLL_MAX, SCROLL_MAX / 10,
                SCROLL_MAX);

        // Move the map when the user changes the scrollbars
        SelectionListener scrollbarListener = new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                updateZoom();
            }

        };
        hBar.addSelectionListener(scrollbarListener);
        vBar.addSelectionListener(scrollbarListener);

    }

    /**
     * 
     */
    protected void updateZoom() {
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
        IDescriptor descriptor = rDisplay.getDescriptor();
        GridEnvelope gridRange = descriptor.getGridGeometry().getGridRange();
        int mapWidth = gridRange.getSpan(0);
        int mapHeight = gridRange.getSpan(1);

        int hSel = hBar.getSelection() + hBar.getThumb() / 2;
        int vSel = vBar.getSelection() + vBar.getThumb() / 2;

        int hThumb = (int) (SCROLL_MAX * zoomLevel);
        int vThumb = (int) (SCROLL_MAX * zoomLevel);

        hSel -= hThumb / 2;
        vSel -= vThumb / 2;

        hBar.setValues(hSel, 0, SCROLL_MAX, hThumb, hThumb / 10, hThumb);
        vBar.setValues(vSel, 0, SCROLL_MAX, vThumb, vThumb / 10, vThumb);

        hSel = hBar.getSelection() + hBar.getThumb() / 2;
        vSel = vBar.getSelection() + vBar.getThumb() / 2;

        double xShift = ((double) hSel / SCROLL_MAX - 0.5) * mapWidth;
        double yShift = ((double) vSel / SCROLL_MAX - 0.5) * mapHeight;

        extent.shift(xShift - currShiftX, yShift - currShiftY);
        extent.scale(zoomLevel / extent.getScale());

        currShiftX = xShift;
        currShiftY = yShift;

        this.refresh();
    }

    protected void setMap(List<String> mapNames) {
        this.mapNames = mapNames;
        List<ZoneSelectorResource> mapRscList = new ArrayList<ZoneSelectorResource>();
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
                        new LoadProperties(), gloc, this.limitZoneNames);
                mapRscList.add(rsc);
            } catch (VizException e) {
                statusHandler.handle(Priority.PROBLEM, "Error loading map: "
                        + mapName, e);
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

            GeneralGridEnvelope gr = new GeneralGridEnvelope(
                    new int[] { 0, 0 }, new int[] { nx, ny }, false);

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
            this.zoomLevel = 1;

            for (ZoneSelectorResource mapRsc : mapRscList) {
                mapRsc.setLabelZones(this.labelZones);
                md.getResourceList().add(mapRsc);
            }
        } catch (Exception e) {
            statusHandler.handle(Priority.PROBLEM,
                    "Error creating map descriptor", e);
        }

        this.initZoneNames(); // extract out the zone names from the map
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
        this.zoneNames.clear();
        for (ZoneSelectorResource mapRsc : mapRscList) {
            for (String ean : mapRsc.getZoneNames()) {
                if (ean != null && !ean.isEmpty()) {
                    // ensure we only add a zone once, and that it should
                    // be included (limitZoneNames is none or is in list)
                    if (!this.zoneNames.contains(ean)
                            && (this.limitZoneNames == null || this.limitZoneNames
                                    .contains(ean))) {
                        this.zoneNames.add(ean);
                    }
                }
            }
        }
    }

    /**
     * @return all zone names
     */
    public List<String> getZoneNames() {
        return this.zoneNames;
    }

    protected List<String> selectedZones(int x, int y) {
        Coordinate c = translateClick(x, y);

        List<String> retZones = new ArrayList<String>();
        for (ZoneSelectorResource mapRsc : mapRscList) {
            List<String> zones = mapRsc.getSelectedZones(c);
            retZones.addAll(zones);
        }
        return retZones;
    }
}
