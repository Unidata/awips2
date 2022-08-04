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
package com.raytheon.uf.viz.monitor.scan.resource;

import java.text.DecimalFormat;
import java.util.ArrayList;
import java.util.Comparator;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;

import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.PlatformUI;
import org.geotools.referencing.GeodeticCalculator;
import org.opengis.referencing.FactoryException;
import org.opengis.referencing.operation.TransformException;

import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.dataplugin.scan.ScanRecord;
import com.raytheon.uf.common.dataplugin.scan.data.CellTableDataRow;
import com.raytheon.uf.common.dataplugin.scan.data.DMDTableDataRow;
import com.raytheon.uf.common.dataplugin.scan.data.ScanTableData;
import com.raytheon.uf.common.dataplugin.scan.data.ScanTableDataRow;
import com.raytheon.uf.common.geospatial.ReferencedCoordinate;
import com.raytheon.uf.common.monitor.scan.config.SCANConfigEnums.ScanTables;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.common.util.StringUtil;
import com.raytheon.uf.viz.core.DrawableString;
import com.raytheon.uf.viz.core.IGraphicsTarget;
import com.raytheon.uf.viz.core.IGraphicsTarget.HorizontalAlignment;
import com.raytheon.uf.viz.core.IGraphicsTarget.TextStyle;
import com.raytheon.uf.viz.core.IGraphicsTarget.VerticalAlignment;
import com.raytheon.uf.viz.core.PixelCoverage;
import com.raytheon.uf.viz.core.VizApp;
import com.raytheon.uf.viz.core.drawables.PaintProperties;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.map.MapDescriptor;
import com.raytheon.uf.viz.core.rsc.AbstractVizResource;
import com.raytheon.uf.viz.core.rsc.IResourceDataChanged;
import com.raytheon.uf.viz.core.rsc.LoadProperties;
import com.raytheon.uf.viz.core.rsc.capabilities.ColorableCapability;
import com.raytheon.uf.viz.core.rsc.capabilities.IMiddleClickCapableResource;
import com.raytheon.uf.viz.core.rsc.capabilities.MagnificationCapability;
import com.raytheon.uf.viz.core.rsc.capabilities.OutlineCapability;
import com.raytheon.uf.viz.monitor.scan.ScanMonitor;
import com.raytheon.uf.viz.monitor.scan.ScanSampleConfigManager;
import com.raytheon.uf.viz.monitor.scan.config.SCANConfig;
import com.raytheon.uf.viz.monitor.scan.listeners.IScanRadarListener;
import com.raytheon.uf.viz.monitor.scan.xml.CellXML;
import com.raytheon.uf.viz.monitor.scan.xml.DmdXML;
import com.raytheon.uf.viz.monitor.scan.xml.ParameterXML;
import org.locationtech.jts.geom.Coordinate;

/**
 * Resource overrider for SCAN of RadarResource
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Oct 13, 2009            dhladky     Initial creation
 *
 * Jul 24  2012  12996     Xiaochuan   Compare with MidVal()
 * Feb 28, 2013 1731       bsteffen    Allow ScanResource to work better with
 *                                     D2DTimeMatcher.
 * Apr 02, 2013 1731       mpduff      Fix problem with DMD updates.
 * Apr 22, 2013   1926       njensen     Faster rendering
 * Mar  3, 2014 2804       mschenke    Set back up clipping pane
 * May 09, 2014   3145     mpduff      Dispose the ScanDrawer font
 * Aug 14, 2014 3523       mapeters    Updated deprecated {@link DrawableString#textStyle}
 *                                     assignments.
 * Nov 05, 2015 5070       randerso    Adjust font sizes for dpi scaling
 * Jan 22, 2018 6854       mduff       Don't filter the D2D display based on cwa filter selection.
 * Jan 30, 2018 6575       mduff       Click on the ident column to unzoom after it's been zoomed.
 * Jan 31, 2018 5863       mapeters    Change dataTimes to a NavigableSet
 * 
 * </pre>
 *
 * @author dhladky
 */

public class ScanResource
        extends AbstractVizResource<ScanResourceData, MapDescriptor> implements
        IScanRadarListener, IMiddleClickCapableResource, IResourceDataChanged {

    private static final IUFStatusHandler statusHandler = UFStatus
            .getHandler(ScanResource.class);

    private static final String TVS = "tvs";

    private static final String RANK = "rank";

    private static final String IDENT = "ident";

    private static final String SPACE = "  ";

    /** offsets **/
    private static final int titleOffset = 50;

    private static final int titleXOffset = 50;

    /** class that draws the scan hexagons and such **/
    private ScanDrawer drawer = null;

    /** type of resource **/
    private ScanTables table = null;

    /** record (used by DMD) **/
    private ScanRecord rec = null;

    /** mouse handler **/
    private ScanMouseAdapter inspectAdapter = null;

    /** trends graphs **/
    private boolean isTrend = false;

    private GeodeticCalculator gc = null;

    private DataTime paintTime = null;

    private DataTime previousTime = null;

    private double tilt = 0.0;

    private boolean isOverlap = true;

    private Date volScanTime = null;

    private ScanTableData<?> std = null;

    private String dmdSampleText = null;

    private String cellSampleText = null;

    private String dmdId = null;

    private String cellId = null;

    protected Map<String, PixelCoverage> drawables = new HashMap<>();

    private boolean zoomed;

    protected ScanResource(ScanResourceData srd, LoadProperties loadProps)
            throws VizException {
        super(srd, loadProps, false);
        this.setTable(srd.tableType);
        srd.addChangeListener(this);
    }

    @Override
    public void resourceChanged(ChangeType type, Object object) {
        VizApp.runAsync(new Runnable() {
            @Override
            public void run() {
                if (gc != null && getScanDrawer().font != null) {
                    getScanDrawer().font.setMagnification(
                            getCapability(MagnificationCapability.class)
                                    .getMagnification().floatValue());
                }
            }
        });
        if (type.equals(ChangeType.DATA_UPDATE)) {
            if (getTable() != null) {
                dmdSampleText = null;
                cellSampleText = null;
                dmdId = null;

                PluginDataObject[] pdos = (PluginDataObject[]) object;
                ScanRecord scan = null;
                for (PluginDataObject pdo : pdos) {
                    try {
                        scan = (ScanRecord) pdo;
                        if (scan.getIcao().equals(resourceData.icao)
                                && scan.getType().equals(getTable().name())) {
                            addRecord(scan);
                        }
                    } catch (Exception e) {
                        statusHandler.handle(Priority.PROBLEM,
                                "Error updating SCAN resource", e);
                    }
                }
                // Handle link-to-frame for DMD, CELL, TVS, and MESO tables.
                if (getScan().getDialog(getTable(),
                        resourceData.icao) != null) {
                    if ((!getScan().getDialog(getTable(), resourceData.icao)
                            .getLinkToFrame(getTable().name()))
                            && (scan != null)) {
                        getScan().updateDialog(getTable(), resourceData.icao,
                                getScan().getDialogTime(getTable(),
                                        resourceData.icao),
                                scan.getDataTime().getRefTime(),
                                scan.getTilt());
                    }
                }
            }

            try {
                getScan().purgeSCANData(resourceData.icao, getTable(),
                        getOldestDate());
            } catch (VizException e) {
                statusHandler.handle(Priority.PROBLEM,
                        "Error purging SCAN resource", e);
            }
        }
    }

    @Override
    protected void disposeInternal() {
        if (drawer != null) {
            if (drawer.font != null) {
                drawer.getFont().dispose();
                drawer.setFont(null);
            }
            drawer = null;
        }
        getScan().setInstantiated(false);
        closeDialog();
        removeScanRadarListener(this);
        getResourceContainer().unregisterMouseHandler(inspectAdapter);
    }

    /**
     * Initialize the internal resources
     */
    @Override
    protected void initInternal(IGraphicsTarget target) throws VizException {
        // Enable the mouse inspect adapter
        inspectAdapter = new ScanMouseAdapter(getResourceContainer());
        getResourceContainer().registerMouseHandler(inspectAdapter);
        addScanRadarListener(this);
        gc = new GeodeticCalculator(descriptor.getCRS());
        initialCenter(getScan().getStationCoordinate(resourceData.icao));

        final ScanMonitor scan = ScanMonitor.getInstance();
        final String icao = resourceData.icao;
        // Open the Monitor for this resource just before we've completed the
        // resource initialization. That is, when
        // status = ResourceStatus.INITIALIZED in AbstractVizResource.java
        VizApp.runAsync(new Runnable() {

            @Override
            public void run() {
                Shell shell = PlatformUI.getWorkbench()
                        .getActiveWorkbenchWindow().getShell();
                scan.launchDialog(shell, icao, table);
            }
        });
    }

    @Override
    protected void paintInternal(IGraphicsTarget target,
            PaintProperties paintProps) throws VizException {
        getScan().setFrames(paintProps.getFramesInfo().getFrameCount());

        this.paintTime = paintProps.getDataTime();

        if (paintTime != null) {
            if (getTable().equals(ScanTables.CELL)) {
                this.std = getScan().getTableData(getTable(), resourceData.icao,
                        paintTime.getRefTime());
                if (std != null) {
                    setTilt(getTableData().getTrueAngle());
                }

            } else {
                this.rec = resourceData.dataObjectMap.get(paintTime);
                if (rec != null) {
                    this.std = rec.getTableData();
                    setTilt(rec.getTilt());
                }
            }
            // true to primary conversions
            setMonitorTilt();
        }

        if ((previousTime == null) || (paintTime == null)
                || !paintTime.equals(previousTime)
                || getScan().isDataUpdated()) {
            getScan().setDataUpdated(false);

            drawables.clear();
            if ((getTableData() != null) && (paintTime != null)) {
                updateDialogTime(paintTime.getValidTime().getTime(),
                        getTableData().getVolScanTime());
            }
        }

        if (getTableData() != null) {
            if (paintProps.getDataTime() == null) {
                return;
            }

            this.volScanTime = paintProps.getDataTime().getRefTime();
            // draw
            if ((getScan().getDialog(getTable(), resourceData.icao) != null)
                    && !getScan().getDialog(getTable(), resourceData.icao)
                            .getCurrentShell().isDisposed()) {
                getScanDrawer().setResourceColor(this
                        .getCapability(ColorableCapability.class).getColor());

                // allow changing of width
                getScanDrawer().setOutlineWidth(
                        this.getCapability(OutlineCapability.class)
                                .getOutlineWidth());

                // draw the cells
                getScanDrawer().setScreenToWorldRatio(
                        paintProps.getCanvasBounds().width
                                / paintProps.getView().getExtent().getWidth());

                if (getScanDrawer().font == null) {
                    getScanDrawer()
                            .setFont(target.initializeFont("Dialog", 9, null));
                }

                if (getScan().getTableKeys(getTable(), resourceData.icao,
                        volScanTime) != null) {
                    for (String id : getScan().getTableKeys(getTable(),
                            resourceData.icao, volScanTime)) {
                        if (getTable().equals(ScanTables.CELL)) {
                            CellTableDataRow ctdr = (CellTableDataRow) std
                                    .getRow(id);
                            if (ctdr != null) {
                                getScanDrawer().drawHexagon(ctdr, descriptor,
                                        target);
                                drawables.put(id,
                                        getScanDrawer().getPixelCoverage());
                            }
                        } else if (getTable().equals(ScanTables.DMD)) {
                            // draw the DMD circles and stuff
                            DMDTableDataRow dtdr = (DMDTableDataRow) std
                                    .getRow(id);
                            if (dtdr == null) {
                                continue;
                            }

                            String rank = dtdr.getRank();
                            Double d = new Double(0);
                            if (!"N/A".equals(rank)) {
                                d = Double.valueOf(rank);
                            }

                            if (d >= getScanDrawer().ddfc.getMidVal()) {
                                if (!getScanDrawer().ddfc.isOverlap()) {
                                    if (!dtdr.getOverlap()) {
                                        isOverlap = false;
                                    }
                                } else {
                                    isOverlap = false;
                                }

                                if (!isOverlap) {
                                    getScanDrawer().drawDMD(dtdr, descriptor,
                                            target);
                                    drawables.put(id,
                                            getScanDrawer().getPixelCoverage());
                                }
                            }

                        }

                        isOverlap = true;
                    }
                }

                target.clearClippingPlane();
                if (getTable().equals(ScanTables.CELL)) {
                    paintCellProductString(target, paintProps);
                } else if (getTable().equals(ScanTables.DMD)) {
                    paintElevationAngle(target, paintProps);
                }
                target.setupClippingPlane(paintProps.getClippingPane());
            }
        }

        this.previousTime = paintTime;
    }

    /**
     * Draws the field text string
     *
     * @param target
     * @param paintProps
     * @throws VizException
     */
    private void paintCellProductString(IGraphicsTarget target,
            PaintProperties paintProps) throws VizException {
        double[] pixel = paintProps.getView().getDisplayCoords(
                new double[] { titleXOffset, titleOffset }, target);
        float mag = getCapability(MagnificationCapability.class)
                .getMagnification().floatValue();

        DrawableString[] strings = new DrawableString[4];
        strings[0] = new DrawableString(getScanDrawer().sdc.getAttrName(),
                getCapability(ColorableCapability.class).getColor());
        strings[0].basics.x = pixel[0];
        strings[0].basics.y = pixel[1];

        double[] pixel1 = paintProps.getView().getDisplayCoords(
                new double[] { titleOffset * 2 * mag, titleOffset }, target);
        strings[1] = new DrawableString(
                String.valueOf(getScanDrawer().sdc.getUpperVal()),
                ScanDrawer.red);
        strings[1].basics.x = pixel1[0];
        strings[1].basics.y = pixel1[1];

        double[] pixel2 = paintProps.getView().getDisplayCoords(
                new double[] { titleOffset * 3 * mag, titleOffset }, target);
        strings[2] = new DrawableString(
                String.valueOf(getScanDrawer().sdc.getMidVal()),
                ScanDrawer.yellow);
        strings[2].basics.x = pixel2[0];
        strings[2].basics.y = pixel2[1];

        double[] pixel3 = paintProps.getView().getDisplayCoords(
                new double[] { titleOffset * 4 * mag, titleOffset }, target);

        strings[3] = new DrawableString(
                String.valueOf(getScanDrawer().sdc.getLowerVal()),
                ScanDrawer.white);
        strings[3].basics.x = pixel3[0];
        strings[3].basics.y = pixel3[1];

        for (DrawableString string : strings) {
            string.font = getScanDrawer().font;
            string.addTextStyle(TextStyle.BLANKED);
            string.horizontalAlignment = HorizontalAlignment.LEFT;
            string.verticallAlignment = VerticalAlignment.MIDDLE;
        }
        target.drawStrings(strings);
    }

    private void paintElevationAngle(IGraphicsTarget target,
            PaintProperties paintProps) throws VizException {

        if (getScan().getTableData(getTable(), resourceData.icao,
                volScanTime) != null) {
            double[] pixel = paintProps.getView()
                    .getDisplayCoords(new double[] { titleXOffset,
                            paintProps.getCanvasBounds().height - titleOffset },
                            target);
            DrawableString string = new DrawableString(
                    "DMD's at Elevation:  " + String.format("%3.1f",
                            getScan().getTableData(getTable(),
                                    resourceData.icao, volScanTime)
                                    .getTrueAngle())
                            + " deg",
                    getCapability(ColorableCapability.class).getColor());
            string.font = getScanDrawer().font;
            string.horizontalAlignment = HorizontalAlignment.LEFT;
            string.verticallAlignment = VerticalAlignment.MIDDLE;
            string.addTextStyle(TextStyle.BLANKED);
            string.basics.x = pixel[0];
            string.basics.y = pixel[1];
            target.drawStrings(string);
        }
    }

    /**
     * Register this resource with scan
     *
     * @param rrd
     */
    private void addScanRadarListener(ScanResource sr) {
        getScan().addScanRadarListener(sr);
    }

    /**
     * Register this resource with scan
     *
     * @param rrd
     */
    private void removeScanRadarListener(ScanResource sr) {
        getScan().removeScanRadarListener(sr);
    }

    /**
     * close our dialog(s)
     */
    private void closeDialog() {
        getScan().closeDialog(table, resourceData.icao);
        getScan().nullifyMonitor(resourceData.icao);
    }

    @Override
    public void paintScan() {
        this.issueRefresh();
    }

    @Override
    public void recenter(Coordinate coor) {
        if (!getScan().getDialog(getTable(), resourceData.icao)
                .getCurrentShell().isDisposed()) {
            // Reset the display
            getDescriptor().getRenderableDisplay().getExtent().reset();

            // If not zoomed then zoom and recenter on the cell.
            if (!zoomed) {
                getDescriptor().getRenderableDisplay().getExtent().reset();
                int mapWidth = getDescriptor().getMapWidth() / 1000;

                // Force the recenter and zoom to zoom to 137 km
                double zoomLevel = (double) 137 / mapWidth;
                getDescriptor().getRenderableDisplay().zoom(zoomLevel);
                getDescriptor().getRenderableDisplay()
                        .recenter(new double[] { coor.x, coor.y });
            }

            zoomed = !zoomed;
            paintScan();
        }
    }

    /**
     * Sets the initial center point
     *
     * @param center
     */
    public void initialCenter(Coordinate center) {
        // do not reset the center
        // getDescriptor().getRenderableDisplay().getExtent().reset();
        // double[] point = new double[] { center.x, center.y };
        // getDescriptor().getRenderableDisplay().recenter(point);

        paintScan();
    }

    /**
     * get the ScanDrawer
     *
     * @return
     */
    public ScanDrawer getScanDrawer() {
        if (drawer == null && gc != null) {
            if (getTable().equals(ScanTables.CELL)) {
                drawer = new ScanDrawer(
                        SCANConfig.getInstance().getStormCellConfig(), gc);
            } else if (getTable().equals(ScanTables.DMD)) {
                drawer = new ScanDrawer(
                        SCANConfig.getInstance().getDmdDisplayFilterConfig(),
                        gc);
            }
        }
        return drawer;
    }

    /**
     * resets the scan drawer to new config settings
     */
    @Override
    public void updateDrawingConfig() {

        if (getTable().equals(ScanTables.CELL)) {
            getScanDrawer().setCellDisplayConfig(
                    getScan().getScanConfig().getStormCellConfig());
        } else if (getTable().equals(ScanTables.DMD)) {
            getScanDrawer().setDmdDisplayConfig(
                    getScan().getScanConfig().getDmdDisplayFilterConfig());
        }

        paintScan();
    }

    /** grab monitor instance **/
    public ScanMonitor getScan() {
        return ScanMonitor.getInstance();
    }

    /**
     * Gets the type from the bundle called
     *
     * @param stype
     */
    public void setTable(String stype) {
        if (stype.equals(ScanTables.CELL.name())) {
            table = ScanTables.CELL;
        } else if (stype.equals(ScanTables.DMD.name())) {
            table = ScanTables.DMD;
        }
    }

    /**
     * get the type
     *
     * @return
     */
    public ScanTables getTable() {
        return table;
    }

    @Override
    public String getName() {
        if (paintTime == null) {
            return "No Data Available";
        }

        StringBuilder prefix = new StringBuilder();
        prefix = new StringBuilder();
        prefix.append(resourceData.icao);
        prefix.append(" ");
        prefix.append("SCAN " + getTable() + " Display");
        if (isTrend()) {
            prefix.append(" ");
            prefix.append("(Editable) ");
        }
        return prefix.toString();
    }

    @Override
    public String inspect(ReferencedCoordinate latLon) throws VizException {
        StringBuilder inspect = new StringBuilder();
        String idToUse = null;
        int maxStRank = 0;

        if ((drawables.size() > 0) && (getScan() != null)) {
            for (Entry<String, PixelCoverage> entry : drawables.entrySet()) {
                // for (String id : drawables.keySet()) {
                try {
                    if (contains(entry.getValue(), latLon.asLatLon())) {
                        ScanTableDataRow stdr = getScan()
                                .getTableData(getTable(), resourceData.icao,
                                        volScanTime)
                                .getRow(entry.getKey());

                        if (stdr != null) {
                            if (stdr instanceof CellTableDataRow) {
                                inspect.append(getCellSampleString(
                                        (CellTableDataRow) stdr));
                                return inspect.toString();
                            } else {
                                int rank = Integer.parseInt(
                                        ((DMDTableDataRow) stdr).getRank());
                                if (rank > maxStRank) {
                                    maxStRank = rank;
                                    idToUse = stdr.getIdent();
                                }
                            }
                        }
                    }
                } catch (TransformException | FactoryException e) {
                    statusHandler.warn("Error transforming coordinates for "
                            + entry.getKey(), e);
                }
            }
        }

        if (idToUse != null) {
            ScanTableDataRow dataRow = getScan()
                    .getTableData(getTable(), resourceData.icao, volScanTime)
                    .getRow(idToUse);
            inspect.append(getDmdSampleString((DMDTableDataRow) dataRow));
        }

        return inspect.toString();
    }

    /**
     * Generate the Cell Sample Text
     *
     * @param data
     *            CellTableDataRow
     * @return the sample text
     */
    private String getCellSampleString(CellTableDataRow data) {

        if ((cellId != null) && cellId.equals(data.getIdent())
                && (cellSampleText != null)) {
            return cellSampleText;
        }

        cellId = data.getIdent();

        List<String> sampleLines = new ArrayList<>();
        DecimalFormat format = new DecimalFormat();
        format.setMaximumFractionDigits(1);
        int line = -999;
        CellXML config = ScanSampleConfigManager.getInstance()
                .getCellSampleConfig();
        ArrayList<ParameterXML> paramList = config.getParameterList();

        for (ParameterXML parm : paramList) {
            if ("true".equalsIgnoreCase(parm.getDisplay())) {
                if (parm.getLine() != null) {
                    try {
                        line = Integer.parseInt(parm.getLine());
                    } catch (NumberFormatException e) {
                        statusHandler
                                .error("Error parsing line in Scan Sample Config for "
                                        + parm.getName(), e);
                    }
                } else {
                    // Reset the line
                    line = -1;
                }

                String text = null;
                if (parm.getKey().equalsIgnoreCase(IDENT)) {
                    text = parm.getName() + ": " + data.getIdent() + SPACE;
                } else if (parm.getKey().equalsIgnoreCase(RANK)) {
                    text = parm.getName() + ": " + data.getRank() + SPACE;
                } else if (parm.getKey().equalsIgnoreCase(TVS)) {
                    text = parm.getName() + ": " + data.getTvs() + SPACE;
                } else if ("mdaSr".equalsIgnoreCase(parm.getKey())) {
                    text = parm.getName() + ": " + data.getMdaSR() + SPACE;
                } else if ("posh".equalsIgnoreCase(parm.getKey())) {
                    text = parm.getName() + ": " + data.getPosh() + SPACE;
                } else if ("poh".equalsIgnoreCase(parm.getKey())) {
                    text = parm.getName() + ": " + data.getPoh() + SPACE;
                } else if ("polh".equalsIgnoreCase(parm.getKey())) {
                    text = parm.getName() + ": " + data.getPolh() + SPACE;
                } else if ("hsize".equalsIgnoreCase(parm.getKey())) {
                    text = parm.getName() + ": "
                            + format.format(data.getHsize()) + SPACE;
                } else if ("dbz".equalsIgnoreCase(parm.getKey())) {
                    text = parm.getName() + ": " + format.format(data.getDbz())
                            + SPACE;
                } else if ("dbzHt".equalsIgnoreCase(parm.getKey())) {
                    text = parm.getName() + ": "
                            + format.format(data.getDbzHt()) + SPACE;
                } else if ("top".equalsIgnoreCase(parm.getKey())) {
                    text = parm.getName() + ": " + format.format(data.getTop())
                            + SPACE;
                } else if ("azm15".equalsIgnoreCase(parm.getKey())) {
                    text = parm.getName() + ": "
                            + format.format(data.getAzm15()) + SPACE;
                } else if ("rng15".equalsIgnoreCase(parm.getKey())) {
                    text = parm.getName() + ": "
                            + format.format(data.getRng15()) + SPACE;
                } else if ("azm30".equalsIgnoreCase(parm.getKey())) {
                    text = parm.getName() + ": "
                            + format.format(data.getAzm30()) + SPACE;
                } else if ("rng30".equalsIgnoreCase(parm.getKey())) {
                    text = parm.getName() + ": "
                            + format.format(data.getRng30()) + SPACE;
                } else if ("azm45".equalsIgnoreCase(parm.getKey())) {
                    text = parm.getName() + ": "
                            + format.format(data.getAzm45()) + SPACE;
                } else if ("rng45".equalsIgnoreCase(parm.getKey())) {
                    text = parm.getName() + ": "
                            + format.format(data.getRng45()) + SPACE;
                } else if ("azm60".equalsIgnoreCase(parm.getKey())) {
                    text = parm.getName() + ": "
                            + format.format(data.getAzm60()) + SPACE;
                } else if ("rng60".equalsIgnoreCase(parm.getKey())) {
                    text = parm.getName() + ": "
                            + format.format(data.getRng60()) + SPACE;
                } else if ("mvtErr".equalsIgnoreCase(parm.getKey())) {
                    text = parm.getName() + ": "
                            + format.format(data.getMvtErr()) + SPACE;
                } else if ("mvtMn".equalsIgnoreCase(parm.getKey())) {
                    text = parm.getName() + ": "
                            + format.format(data.getMvtMn()) + SPACE;
                } else if ("svrwx".equalsIgnoreCase(parm.getKey())) {
                    text = parm.getName() + ": " + data.getSvrwx() + SPACE;
                } else if ("heavyPr".equalsIgnoreCase(parm.getKey())) {
                    text = parm.getName() + ": " + data.getHvyPr() + SPACE;
                } else if ("pPos".equalsIgnoreCase(parm.getKey())) {
                    text = parm.getName() + ": " + format.format(data.getPos())
                            + SPACE;
                } else if ("cgRate".equalsIgnoreCase(parm.getKey())) {
                    text = parm.getName() + ": "
                            + format.format(data.getCgRate()) + SPACE;
                } else if ("vcp".equalsIgnoreCase(parm.getKey())) {
                    text = parm.getName() + ": " + data.getVcp() + SPACE;
                } else if ("cape".equalsIgnoreCase(parm.getKey())) {
                    text = parm.getName() + ": " + format.format(data.getCape())
                            + SPACE;
                } else if ("sreh".equalsIgnoreCase(parm.getKey())) {
                    text = parm.getName() + ": " + format.format(data.getSreh())
                            + SPACE;
                }

                if (line > 0) {
                    if (sampleLines.size() > line - 1) {
                        String s = sampleLines.get(line - 1);
                        s = s.concat(text);
                        sampleLines.remove(line - 1);
                        sampleLines.add(line - 1, s);
                    } else {
                        sampleLines.add(line - 1, text);
                    }
                } else {
                    sampleLines.add(text);
                }
            }
        }

        StringBuilder sb = new StringBuilder();
        for (String s : sampleLines) {
            sb.append(s).append(StringUtil.NEWLINE);
        }

        cellSampleText = sb.toString();

        return cellSampleText;
    }

    /**
     * Generate the sample text
     *
     * @param data
     * @return
     */
    private String getDmdSampleString(DMDTableDataRow data) {
        if ((dmdId != null) && dmdId.equals(data.getIdent())
                && (dmdSampleText != null)) {
            return dmdSampleText;
        }

        dmdId = data.getIdent();

        List<String> sampleLines = new ArrayList<>();
        DecimalFormat format = new DecimalFormat();
        format.setMaximumFractionDigits(1);
        int line = -999;
        DmdXML config = ScanSampleConfigManager.getInstance()
                .getDmdSampleConfig();
        ArrayList<ParameterXML> paramList = config.getParameterList();

        for (ParameterXML parm : paramList) {
            if ("true".equalsIgnoreCase(parm.getDisplay())) {
                if (parm.getLine() != null) {
                    try {
                        line = Integer.parseInt(parm.getLine());
                    } catch (NumberFormatException e) {
                        statusHandler
                                .error("Error parsing line in Scan Sample Config for "
                                        + parm.getName(), e);
                    }
                } else {
                    // Reset line number
                    line = -1;
                }

                String text = null;
                if (parm.getKey().equalsIgnoreCase(IDENT)) {
                    text = parm.getName() + ": " + data.getIdent() + SPACE;
                } else if ("strmID".equalsIgnoreCase(parm.getKey())) {
                    text = parm.getName() + ": " + data.getStrmID() + SPACE;
                } else if ("status".equalsIgnoreCase(parm.getKey())) {
                    text = parm.getName() + ": " + data.getStatus() + SPACE;
                } else if ("base".equalsIgnoreCase(parm.getKey())) {
                    text = parm.getName() + ": " + format.format(data.getBase())
                            + SPACE;
                } else if ("depth".equalsIgnoreCase(parm.getKey())) {
                    text = parm.getName() + ": "
                            + format.format(data.getDepth()) + SPACE;
                } else if (parm.getKey().equalsIgnoreCase(RANK)) {
                    text = parm.getName() + ": " + data.getRank() + SPACE;
                } else if ("msi".equalsIgnoreCase(parm.getKey())) {
                    text = parm.getName() + ": " + data.getMsi() + SPACE;
                } else if ("llVr".equalsIgnoreCase(parm.getKey())) {
                    text = parm.getName() + ": " + format.format(data.getLlVr())
                            + SPACE;
                } else if ("llgtg".equalsIgnoreCase(parm.getKey())) {
                    text = parm.getName() + ": "
                            + format.format(data.getLlgtg()) + SPACE;
                } else if ("llConv".equalsIgnoreCase(parm.getKey())) {
                    text = parm.getName() + ": "
                            + format.format(data.getLlConv()) + SPACE;
                } else if ("llShear".equalsIgnoreCase(parm.getKey())) {
                    text = parm.getName() + ": "
                            + format.format(data.getLlShear()) + SPACE;
                } else if ("mlConv".equalsIgnoreCase(parm.getKey())) {
                    text = parm.getName() + ": "
                            + format.format(data.getMlConv()) + SPACE;
                } else if ("llDiam".equalsIgnoreCase(parm.getKey())) {
                    text = parm.getName() + ": "
                            + format.format(data.getLlDiam()) + SPACE;
                } else if ("relDepth".equalsIgnoreCase(parm.getKey())) {
                    text = parm.getName() + ": "
                            + format.format(data.getRelDepth()) + SPACE;
                } else if ("maxVr".equalsIgnoreCase(parm.getKey())) {
                    text = parm.getName() + ": "
                            + format.format(data.getMaxVr()) + SPACE;
                } else if ("htMxVr".equalsIgnoreCase(parm.getKey())) {
                    text = parm.getName() + ": "
                            + format.format(data.getHtMxVr()) + SPACE;
                } else if (parm.getKey().equalsIgnoreCase(TVS)) {
                    text = parm.getName() + ": " + data.getTvs() + SPACE;
                } else if ("age".equalsIgnoreCase(parm.getKey())) {
                    text = parm.getName() + ": " + data.getAge() + SPACE;
                } else if ("elev0".equalsIgnoreCase(parm.getKey())) {
                    text = parm.getName() + ": " + data.getElev0() + SPACE;
                } else if ("rankType".equalsIgnoreCase(parm.getKey())) {
                    text = parm.getName() + ": " + data.getRankType() + SPACE;
                }

                if (line > 0) {
                    if (sampleLines.size() > line - 1) {
                        String s = sampleLines.get(line - 1);
                        s = s.concat(text);
                        sampleLines.remove(line - 1);
                        sampleLines.add(line - 1, s);
                    } else {
                        sampleLines.add(line - 1, text);
                    }
                } else {
                    sampleLines.add(text);
                }

            }
        }

        StringBuilder sb = new StringBuilder();
        for (String s : sampleLines) {
            sb.append(s).append(StringUtil.NEWLINE);
        }

        dmdSampleText = sb.toString();

        return dmdSampleText;
    }

    /**
     * updates the dialog
     */
    @Override
    public void updateDialogTime(Date drawTime, Date recordTime) {
        if ((drawTime != null) && (getTableData() != null)) {
            if (getTable().equals(ScanTables.CELL)) {
                getScan().updateDialog(getTable(), resourceData.icao, drawTime,
                        recordTime, getTableData().getTrueAngle());
            } else {
                if (rec != null) {
                    getScan().updateDialog(getTable(), resourceData.icao,
                            recordTime, drawTime, rec.getTilt());
                }
            }
        }
    }

    /**
     * Sets the tilt angle
     *
     * @param tilt
     */
    public void setTilt(double tilt) {
        this.tilt = tilt;
    }

    /**
     * get tilted
     *
     * @return
     */
    public double getTilt() {
        return tilt;
    }

    public void setMonitorTilt() {
        if (getTable() == ScanTables.CELL) {
            getScan().setCellTilt(getTilt(), resourceData.icao);
        } else if (getTable() == ScanTables.DMD) {
            getScan().setDmdTilt(getTilt(), resourceData.icao);
        }
    }

    /**
     * See if you are in the coverage of this feature
     *
     * @param c
     * @return
     */
    private boolean contains(PixelCoverage pc, Coordinate c) {
        boolean inside = false;
        double[] mousePt = descriptor.worldToPixel(new double[] { c.x, c.y });
        double centerX = (pc.getMinX() + pc.getMaxX()) / 2;
        double centerY = (pc.getMinY() + pc.getMaxY()) / 2;
        double radius = pc.getMaxX() - centerX;

        double distX = Math.abs(centerX - mousePt[0]);
        double distY = Math.abs(centerY - mousePt[1]);

        double dist = Math.sqrt((distX * distX) + (distY * distY));
        if (descriptor.getRenderableDisplay().getZoom() < 0.02) {
            radius *= 0.6;
        }
        if (dist < radius) {
            return true;
        }

        return inside;
    }

    /**
     * Sort of misnomer, not really "editable" in this case
     *
     */
    @Override
    public void middleClicked() throws VizException {
        if (isTrend) {
            setTrend(false);
        } else {
            setTrend(true);
        }
        issueRefresh();
    }

    /**
     * Set trend select
     *
     * @param isTrend
     */
    public void setTrend(boolean isTrend) {
        this.isTrend = isTrend;
    }

    /**
     * get trend on/off
     *
     * @return
     */
    public boolean isTrend() {
        return isTrend;
    }

    /**
     * Fires a trend graph when requested
     *
     * @param mouseCoords
     * @return boolean
     */
    protected boolean trendClick(double[] mouseCoord) {
        boolean trend = false;
        if ((drawables.size() > 0) && (getScan() != null)) {
            for (Entry<String, PixelCoverage> entry : drawables.entrySet()) {
                if (contains(drawables.get(entry.getKey()),
                        getResourceContainer().translateClick(mouseCoord[0],
                                mouseCoord[1]))) {
                    getScan().launchTrendGraphs(getTable(), resourceData.icao,
                            entry.getKey());
                    trend = true;
                }
            }
        }
        return trend;
    }

    /**
     * Sort by Date
     *
     * @author dhladky
     *
     */
    public class SortDates implements Comparator<Date> {

        @Override
        public int compare(Date o1, Date o2) {

            return o1.compareTo(o2);
        }
    }

    /**
     * set and populate the record to the monitor
     *
     * @param newrecord
     */
    public void addRecord(ScanRecord newrecord) {
        try {
            if (!getScan()
                    .getTimeOrderedKeys(getScan(), newrecord.getType(),
                            resourceData.icao)
                    .contains(newrecord.getDataTime().getRefTime())
                    || "DMD".equals(newrecord.getType())) {

                newrecord = resourceData.populateRecord(newrecord);

                if ((newrecord.getTableData() != null)
                        && (newrecord.getDataTime() != null) && (newrecord
                                .getTableData().getVolScanTime() != null)) {

                    getScan().setTableData(resourceData.icao,
                            newrecord.getTableData(),
                            /*
                             * TODO: This should be the volume scan time, but
                             * {Radar,Scan}Record.getVolScanTime is actually the
                             * radar product generation time.
                             */
                            newrecord.getDataTime().getRefTime(),
                            newrecord.getTilt(),
                            newrecord.getDataTime().getRefTime(),
                            newrecord.getType());
                    if (getTable().equals(ScanTables.DMD)) {
                        resourceData.dataObjectMap.put(newrecord.getDataTime(),
                                newrecord);
                        getScan().setLatestElevation(newrecord.getTilt());
                        getScan().setDmdTilt(newrecord.getTilt(),
                                resourceData.icao);
                    }
                }
            }

        } catch (Exception e) {
            statusHandler.warn("Error adding record: " + newrecord.getId(), e);
        }
    }

    /**
     * table data object
     *
     * @return
     */
    public ScanTableData<?> getTableData() {
        return std;
    }

    /**
     * Gets the oldest available date
     *
     * @return
     * @throws VizException
     */
    private Date getOldestDate() throws VizException {
        Date oldDate = null;
        for (DataTime time : resourceData.getAvailableTimes()) {
            if ((oldDate == null) || time.getRefTime().before(oldDate)) {
                oldDate = time.getRefTime();
            }
        }

        return oldDate;
    }

    @Override
    public DataTime[] getDataTimes() {
        ScanMonitor scan = getScan();
        List<Date> dates = scan.getTimeOrderedKeys(scan, resourceData.tableType,
                resourceData.icao);
        DataTime[] dataTimes = new DataTime[dates.size()];
        for (int i = 0; i < dataTimes.length; i += 1) {
            dataTimes[i] = new DataTime(dates.get(i));
        }
        return dataTimes;
    }

}
