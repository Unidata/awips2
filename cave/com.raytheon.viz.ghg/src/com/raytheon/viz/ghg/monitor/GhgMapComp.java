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

import org.eclipse.swt.SWT;
import org.eclipse.swt.events.DisposeEvent;
import org.eclipse.swt.events.DisposeListener;
import org.eclipse.swt.graphics.RGB;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.opengis.referencing.FactoryException;
import org.opengis.referencing.operation.TransformException;

import com.raytheon.uf.common.dataplugin.gfe.db.objects.GridLocation;
import com.raytheon.uf.common.geospatial.MapUtil;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.viz.core.IDisplayPane;
import com.raytheon.uf.viz.core.drawables.IRenderableDisplay;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.localization.LocalizationManager;
import com.raytheon.uf.viz.core.map.MapDescriptor;
import com.raytheon.uf.viz.core.maps.display.PlainMapRenderableDisplay;
import com.raytheon.uf.viz.core.rsc.LoadProperties;
import com.raytheon.viz.gfe.rsc.ZoneDbResourceData;
import com.raytheon.viz.gfe.rsc.zones.ZoneDbResource;
import com.raytheon.viz.ghg.monitor.constants.GhgMenuConstants;
import com.raytheon.viz.ghg.monitor.data.GhgConfigData;
import com.raytheon.viz.ui.panes.VizDisplayPane;
import com.vividsolutions.jts.geom.Envelope;

/**
 * Contains the GHG Monitor Map and handles all interactions with the map.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 30May2008    1157       MW Fegan    Initial Creation.
 * 17Nov2008               wdougherty  Extra refresh() in paint handler removed.
 * Dec 16, 2015  #5184     dgilling    Remove viz.gfe dependencies.
 * 
 * </pre>
 * 
 * @author mfegan
 * @version 1.0
 */

public class GhgMapComp extends Composite implements IMenuSelectionClient {
    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(GhgMapComp.class);

    private final GhgDisplayManager displayMgr;

    private final GridLocation gloc;

    /**
     * The zoom level.
     */
    private int zoomLevel = 0;

    /**
     * The Zone File resource.
     */
    private ZoneDbResource zoneDbResource;

    /** Map Action Handler */
    private GhgMapManager mapManager;

    /**
     * Constructor.
     * 
     * @param parent
     *            parent of this component.
     */
    public GhgMapComp(Composite parent, GhgDisplayManager displayMgr,
            GridLocation gloc) {
        super(parent, SWT.NONE);
        this.displayMgr = displayMgr;
        this.gloc = gloc;
        init();
    }

    /**
     * Performs steps needed to create the widget.
     */
    private void init() {
        GridData gd = new GridData(SWT.FILL, SWT.FILL, true, true);
        GridLayout gl = new GridLayout(1, false);
        gl.verticalSpacing = 1;
        gl.marginHeight = 1;
        setLayout(gl);
        setLayoutData(gd);

        try {
            initializeComponents();
        } catch (Exception e) {
            statusHandler.handle(Priority.PROBLEM, e.getLocalizedMessage(), e);
        }

        this.pack();

        addDisposeListener(new DisposeListener() {
            @Override
            public void widgetDisposed(DisposeEvent e) {

            }
        });
    }

    /**
     * Performs steps needed to initialize components.
     * 
     * @throws FactoryException
     * @throws TransformException
     * @throws VizException
     */
    private void initializeComponents() throws TransformException,
            FactoryException, VizException {
        // First thing, give the zone resource a bounding geometry.
        Envelope env = MapUtil.getBoundingEnvelope(gloc);

        // Create a plain map renderable display
        IRenderableDisplay renderableDisplay = new PlainMapRenderableDisplay(
                new MapDescriptor(MapUtil.getGridGeometry(gloc)));

        // Create resource data for the zone resource
        ZoneDbResourceData resourceData = new ZoneDbResourceData();
        List<String> allTables = new ArrayList<String>(4);
        allTables.add(ZoneDbResource.COUNTY_TABLE);
        allTables.add(ZoneDbResource.FIREWX_TABLE);
        allTables.add(ZoneDbResource.MARINE_TABLE);
        allTables.add(ZoneDbResource.PUBLIC_TABLE);
        allTables.add(ZoneDbResource.OFFSHORE_TABLE);
        resourceData.setQueryColumns(ZoneDbResource.COUNTY_TABLE,
                ZoneDbResource.FIPS_FIELDS);
        resourceData.setQueryColumns(ZoneDbResource.FIREWX_TABLE,
                ZoneDbResource.FIREWX_FIELDS);
        resourceData.setQueryColumns(ZoneDbResource.MARINE_TABLE,
                ZoneDbResource.MARINE_FIELDS);
        resourceData.setQueryColumns(ZoneDbResource.PUBLIC_TABLE,
                ZoneDbResource.PUBLIC_FIELDS);
        resourceData.setQueryColumns(ZoneDbResource.OFFSHORE_TABLE,
                ZoneDbResource.OFFSHORE_FIELDS);
        resourceData.setAllTables(allTables);
        resourceData.setMainTable(ZoneDbResource.PUBLIC_TABLE);

        RGB selectionColor = GhgConfigData.getInstance()
                .getMapSelectionsColors().getBackgroundRgb();
        resourceData.setSelectionColor(selectionColor);
        resourceData.setMapName("GHG Monitor Map");

        // Create the zone resource
        zoneDbResource = resourceData.construct(new LoadProperties(),
                renderableDisplay.getDescriptor());
        zoneDbResource.setBoundingEnvelope(env);
        // Tell the zone resource which value is the local CWA/site id
        zoneDbResource.setCwaId(LocalizationManager.getInstance()
                .getCurrentSite());
        zoneDbResource.fitToCWA();
        zoneDbResource.setGHGType();

        // Create the map manager
        mapManager = new GhgMapManager(this, displayMgr);
        mapManager.createComponent(this);

        // Add the zone resource to the display
        renderableDisplay.getDescriptor().getResourceList().add(zoneDbResource);

        // Add the renderable display
        mapManager.addPane(renderableDisplay);
    }

    @Override
    public void notifyMenuSelection(String menu, Object event, Object data) {
        ZoneDbResourceData zdrd = zoneDbResource.getResourceData();
        switch ((GhgMenuConstants.MapMenuActions) event) {
        case SHOW_LABELS:
            zoneDbResource.setLabelZones((Boolean) data);
            break;
        case SHOW_MAP:
            switch ((GhgMenuConstants.ShowMap) data) {
            case SHOW_FIPS:
                // FIPS shows both counties and marine zones
                List<String> tables = new ArrayList<String>(2);
                tables.add(ZoneDbResource.COUNTY_TABLE);
                tables.add(ZoneDbResource.MARINE_TABLE);
                zdrd.setTables(tables);
                zdrd.setQueryColumns(ZoneDbResource.COUNTY_TABLE,
                        ZoneDbResource.FIPS_FIELDS);
                zdrd.setQueryColumns(ZoneDbResource.MARINE_TABLE,
                        ZoneDbResource.MARINE_FIELDS);
                break;
            case SHOW_FIRE:
                zoneDbResource.getResourceData().setMainTable(
                        ZoneDbResource.FIREWX_TABLE);
                zdrd.setQueryColumns(ZoneDbResource.FIREWX_TABLE,
                        ZoneDbResource.FIREWX_FIELDS);
                break;
            case SHOW_MARINE:
                zoneDbResource.getResourceData().setMainTable(
                        ZoneDbResource.MARINE_TABLE);
                zdrd.setQueryColumns(ZoneDbResource.MARINE_TABLE,
                        ZoneDbResource.MARINE_FIELDS);
                break;
            case SHOW_PUBLIC:
                zoneDbResource.getResourceData().setMainTable(
                        ZoneDbResource.PUBLIC_TABLE);
                zdrd.setQueryColumns(ZoneDbResource.PUBLIC_TABLE,
                        ZoneDbResource.PUBLIC_FIELDS);
                break;
            }
            zoneDbResource.setDirty(true);
            break;
        case ZOOM_MAP:
            zoomLevel = ((GhgMenuConstants.ZoomLevel) data).getZoomLevel();
            displayMgr.setZoomLevel(zoomLevel);
        }
    }

    /**
     * Update the selected zoom.
     */
    public void updateZoom() {
        double zLev = displayMgr.getZoomLevel();
        for (IDisplayPane dspPane : mapManager.getDisplayPanes()) {
            // Reset the zoom
            dspPane.setZoomLevel(1.0f);
            dspPane.scaleToClientArea();

            // Zoom into selected zoom level
            ((VizDisplayPane) dspPane).zoom(1 / zLev);
        }

        mapManager.refresh();
    }

    public void updateZone() {
        GhgMenuConstants.ShowMap zoneType = displayMgr.getZoneDisplay();
        ZoneDbResourceData zdrd = zoneDbResource.getResourceData();

        switch (zoneType) {
        case SHOW_FIPS:
            // FIPS shows both counties and marine zones
            List<String> tables = new ArrayList<String>(2);
            tables.add(ZoneDbResource.COUNTY_TABLE);
            tables.add(ZoneDbResource.MARINE_TABLE);
            zdrd.setTables(tables);
            zdrd.setQueryColumns(ZoneDbResource.COUNTY_TABLE,
                    ZoneDbResource.FIPS_FIELDS);
            zdrd.setQueryColumns(ZoneDbResource.MARINE_TABLE,
                    ZoneDbResource.MARINE_FIELDS);
            break;
        case SHOW_FIRE:
            zdrd.setMainTable(ZoneDbResource.FIREWX_TABLE);
            zdrd.setQueryColumns(ZoneDbResource.FIREWX_TABLE,
                    ZoneDbResource.FIREWX_FIELDS);
            break;
        case SHOW_MARINE:
            zdrd.setMainTable(ZoneDbResource.MARINE_TABLE);
            zdrd.setQueryColumns(ZoneDbResource.MARINE_TABLE,
                    ZoneDbResource.MARINE_FIELDS);
            break;
        case SHOW_PUBLIC:
            zdrd.setMainTable(ZoneDbResource.PUBLIC_TABLE);
            zdrd.setQueryColumns(ZoneDbResource.PUBLIC_TABLE,
                    ZoneDbResource.PUBLIC_FIELDS);
            break;
        }

        zoneDbResource.setFilthy(true);
    }

    public void updateLabels() {
        zoneDbResource.setLabelZones(displayMgr.isShowLabels());
        zoneDbResource.setDirty(true);
    }

    public void updateMapColors() {
        GhgConfigData config = GhgConfigData.getInstance();
        ZoneDbResourceData rData = zoneDbResource.getResourceData();
        rData.setSelectionColor(config.getMapSelectionsColors()
                .getBackgroundRgb());
    }

    public void refresh() {
        mapManager.refresh();
    }

    @Override
    public void dispose() {
        super.dispose();
        mapManager.dispose();
    }

    @Override
    public boolean setFocus() {
        mapManager.setFocus();
        return super.setFocus();
    }
}
