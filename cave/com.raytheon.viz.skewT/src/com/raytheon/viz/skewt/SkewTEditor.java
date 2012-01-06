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

package com.raytheon.viz.skewt;

import java.util.ArrayList;
import java.util.List;

import javax.measure.converter.UnitConverter;
import javax.measure.unit.NonSI;
import javax.measure.unit.SI;

import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.swt.SWT;
import org.eclipse.swt.custom.SashForm;
import org.eclipse.swt.events.FocusAdapter;
import org.eclipse.swt.events.FocusEvent;
import org.eclipse.swt.events.MouseEvent;
import org.eclipse.swt.events.MouseTrackAdapter;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.graphics.Point;
import org.eclipse.swt.graphics.Rectangle;
import org.eclipse.swt.layout.FormAttachment;
import org.eclipse.swt.layout.FormData;
import org.eclipse.swt.layout.FormLayout;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Menu;
import org.eclipse.swt.widgets.MenuItem;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Table;
import org.eclipse.swt.widgets.TableColumn;
import org.eclipse.swt.widgets.TableItem;
import org.eclipse.ui.IEditorInput;
import org.eclipse.ui.IEditorSite;
import org.eclipse.ui.PartInitException;

import com.raytheon.edex.meteoLib.WindComp;
import com.raytheon.uf.common.sounding.SoundingLayer;
import com.raytheon.uf.common.sounding.VerticalSounding;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.viz.core.IDisplayPane;
import com.raytheon.uf.viz.core.IRenderableDisplayChangedListener;
import com.raytheon.uf.viz.core.VizApp;
import com.raytheon.uf.viz.core.drawables.IRenderableDisplay;
import com.raytheon.uf.viz.core.drawables.ResourcePair;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.rsc.IInputHandler;
import com.raytheon.uf.viz.core.rsc.IInputHandler.InputPriority;
import com.raytheon.uf.viz.core.rsc.ResourceList;
import com.raytheon.uf.viz.core.rsc.ResourceList.AddListener;
import com.raytheon.uf.viz.core.rsc.ResourceList.RemoveListener;
import com.raytheon.uf.viz.sounding.SoundingParams;
import com.raytheon.uf.viz.xy.map.IInsetMapContainer;
import com.raytheon.uf.viz.xy.map.IInsetMapContainer.InsetMapUtil;
import com.raytheon.uf.viz.xy.map.IInsetMapDisplayPaneContainer;
import com.raytheon.viz.skewt.mouse.SkewtMouseInspectAdapter;
import com.raytheon.viz.skewt.rsc.InteractiveSkewTResource;
import com.raytheon.viz.skewt.rsc.SkewTResource;
import com.raytheon.viz.skewt.rsc.SkewTResource.ISkewTDataChangedListener;
import com.raytheon.viz.skewt.ui.SkewtControlsDialog;
import com.raytheon.viz.ui.editor.AbstractEditor;
import com.raytheon.viz.ui.input.InputManager;
import com.raytheon.viz.ui.panes.PaneManager;
import com.raytheon.viz.ui.panes.VizDisplayPane;
import com.vividsolutions.jts.geom.Coordinate;

/**
 * 
 * This class was merged with AbstractXyEditor so that AbstractXyEditor could go
 * IMultiPaneEditor without affecting SkewT.
 * 
 * SkewTEditor , 3 frames, SkewT, Hodo, and 24 Hour TempChange
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Sep 29, 2008            dhladky     Initial creation
 * Dec 07, 2008            dhladky     Got rid of problematic View window, consolidated.
 * </pre>
 * 
 * @author dhladky
 * @version 1.0
 */
public class SkewTEditor extends AbstractEditor implements AddListener,
        RemoveListener, ISkewTDataChangedListener,
        IRenderableDisplayChangedListener, IInsetMapDisplayPaneContainer {
    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(SkewTEditor.class);

    public static final String EDITOR_ID = "com.raytheon.viz.skewt.SkewTEditor";

    private static UnitConverter metersToFeet = javax.measure.unit.SI.METER
            .getConverterTo(NonSI.FOOT);

    private static UnitConverter metersPerSecondToKnots = SI.METERS_PER_SECOND
            .getConverterTo(NonSI.KNOT);

    private static UnitConverter kelvinToCelsius = SI.KELVIN
            .getConverterTo(SI.CELSIUS);

    private static UnitConverter kelvinToFahrenheit = SI.KELVIN
            .getConverterTo(NonSI.FAHRENHEIT);

    private static UnitConverter celsiusToFahrenheit = SI.CELSIUS
            .getConverterTo(NonSI.FAHRENHEIT);

    private static UnitConverter centimetersToInches = SI.CENTIMETER
            .getConverterTo(NonSI.INCH);

    private static final char degree = '\u00B0';

    private static final char squared = '\u00B2';

    private Table paramTable = null;

    // private Composite paramComp = null;

    private TableColumn paramLabelColumn = null;

    private TableColumn paramValueColumn = null;

    private static List<SkewTResource> rscList = new ArrayList<SkewTResource>();

    private static SkewTResource displayedRsc = null;

    protected SkewtControlsDialog skewtControlsDialog = null;

    /** The plan is to make these read from some config file eventually */
    public static String[] generalParams = new String[] { "Precipitable Water",
            "K-Index", "Total Totals", "SWEAT Index",
            "Dry Microburst Potential", "Freezing Level", "Wet-bulb Zero HGT",
            "0-6 KM AVG Wind", "0-6 KM Storm Relative Motion",
            "0-3 KM Storm Relative Helicity", "Forecast Max Temp",
            "Trigger Temp", "Soaring Index", "MDPI / WINDEX" };

    /** The plan is to make these read from some config file eventually */
    public static String[] parcelParams = new String[] { "Parcel Pressure",
            "Parcel Temp", "Parcel Dewpoint", "Convective Temp",
            "Lifted Index", "CCL", "LCL", "LFC", "Max Hail Size",
            "Max Vertical Velocity", "Equilibrium Level",
            "Approximate Cloud Top", "+ Energy Above LFC",
            "- Energy Below LFC", "Bulk Richardson Number" };

    public String dialogTitle;

    private Composite baseComposite;

    private boolean manSelection = false;

    protected VizDisplayPane activeDisplayPane;

    protected VizDisplayPane graphPane;

    protected VizDisplayPane insetMap;

    /** The input manager */
    protected InputManager inputManager;

    protected boolean hasCornerMap = false;

    protected Composite parent;

    /** The renderable display object */
    protected IRenderableDisplay renderableDisplay;

    public SkewTEditor() {
        addRenderableDisplayChangedListener(this);
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * org.eclipse.ui.part.WorkbenchPart#createPartControl(org.eclipse.swt.widgets
     * .Composite)
     */
    @Override
    public void createPartControl(Composite parent) {
        if (renderableDisplay == null) {
            renderableDisplay = new SkewtDisplay();
        }

        SashForm sashForm = new SashForm(parent, SWT.HORIZONTAL);
        FormData sfd = new FormData();
        sfd.left = new FormAttachment(0, 0);
        sfd.top = new FormAttachment(0, 0);
        sfd.bottom = new FormAttachment(100, 0);
        sfd.right = new FormAttachment(100, 0);
        sashForm.setLayoutData(sfd);
        sashForm.SASH_WIDTH = 3;

        // baseComposite
        baseComposite = new Composite(sashForm, SWT.NONE);
        FormLayout bfl = new FormLayout();
        baseComposite.setLayout(bfl);

        // parameter composite
        final Composite paramsComposite = new Composite(sashForm, SWT.NONE);
        GridLayout gl2 = new GridLayout(1, false);
        gl2.horizontalSpacing = 3;
        gl2.verticalSpacing = 3;
        gl2.marginHeight = 0;
        gl2.marginWidth = 0;
        paramsComposite.setLayoutData(new GridData(SWT.FILL, SWT.FILL, true,
                true));
        paramsComposite.setLayout(gl2);

        paramTable = new Table(paramsComposite, SWT.BORDER | SWT.VIRTUAL);
        GridData data = new GridData(SWT.FILL, SWT.FILL, true, true);
        data.heightHint = 200;
        paramTable.setLayoutData(data);

        paramTable.setLinesVisible(true);
        paramTable.setHeaderVisible(true);

        paramLabelColumn = new TableColumn(paramTable, SWT.NONE);
        paramValueColumn = new TableColumn(paramTable, SWT.NONE);
        paramValueColumn.setToolTipText("Click to select sounding");
        paramValueColumn.addSelectionListener(new SelectionAdapter() {

            /*
             * (non-Javadoc)
             * 
             * @see
             * org.eclipse.swt.events.SelectionAdapter#widgetSelected(org.eclipse
             * .swt.events.SelectionEvent)
             */
            @Override
            public void widgetSelected(SelectionEvent e) {
                Menu menu = new Menu(paramsComposite);
                if (rscList.size() == 0) {
                    MenuItem item = new MenuItem(menu, SWT.PUSH);
                    item.setText("No soundings loaded");
                } else {
                    for (SkewTResource rsc : rscList) {
                        MenuItem item = new MenuItem(menu, SWT.RADIO);
                        item.setText(SkewTEditor.this.getRscTitle(rsc));
                        item.setData(rsc);
                        item.setSelection(rsc == displayedRsc);
                        item.addSelectionListener(new SelectionAdapter() {

                            @Override
                            public void widgetSelected(SelectionEvent e) {
                                manSelection = true;
                                setDisplayedRsc((SkewTResource) e.widget
                                        .getData());
                            }
                        });
                    }
                }
                Rectangle rect = paramsComposite.getBounds();
                Point pt = new Point(rect.x + rect.width
                        - paramValueColumn.getWidth(), rect.y);
                pt = paramsComposite.getParent().toDisplay(pt);
                menu.setLocation(pt);
                menu.setVisible(true);
            }
        });
        inititializeTableValues();
        paramLabelColumn.pack();
        paramValueColumn.pack();
        // set the default splits
        sashForm.setWeights(new int[] { 65, 35 });
        createPartControlParent(baseComposite);
        registerListener(graphPane);

    }

    /*
     * 
     * (non-Javadoc)
     * 
     * @see org.eclipse.ui.part.WorkbenchPart#dispose()
     */
    @Override
    public void dispose() {
        if (skewtControlsDialog != null) {
            skewtControlsDialog.exit();
        }
        super.dispose();
    }

    /**
     * Perform a refresh asynchronously
     * 
     */
    @Override
    public void refresh() {
        graphPane.refresh();
        if (insetMap != null) {
            insetMap.refresh();
        }

        if (graphPane.getRenderableDisplay() instanceof SkewtDisplay) {
            SkewtDisplay gp = (SkewtDisplay) graphPane.getRenderableDisplay();
            if (displayedRsc == null) {
                if (gp.getRsc() instanceof SkewTResource
                        || gp.getRsc() instanceof InteractiveSkewTResource) {
                    displayedRsc = (SkewTResource) gp.getRsc();
                }
            } else if ((gp.getRsc() instanceof SkewTResource || gp.getRsc() instanceof InteractiveSkewTResource)
                    && manSelection == false) {
                setDisplayedRsc((SkewTResource) gp.getRsc());
            }
            updateColumn(displayedRsc);
        }
    }

    /**
     * Sets the General Parameters table
     * 
     * @param paramTable
     */
    private void inititializeTableValues() {
        TableItem item;
        item = new TableItem(paramTable, SWT.BORDER);
        item.setText(0, "General Parameters");
        item.setText(1, "");
        item.setBackground(item.getDisplay()
                .getSystemColor(SWT.COLOR_DARK_GRAY));
        item.setForeground(item.getDisplay().getSystemColor(SWT.COLOR_WHITE));

        for (int i = 0; i < generalParams.length; i++) {
            item = new TableItem(paramTable, SWT.BORDER);

            item.setText(0, "    " + generalParams[i] + " ");
            item.setText(1, "");

        }

        item = new TableItem(paramTable, SWT.BORDER);
        item.setText(0, "Parcel Parameters");
        item.setText(1, "");
        item.setBackground(item.getDisplay()
                .getSystemColor(SWT.COLOR_DARK_GRAY));
        item.setForeground(item.getDisplay().getSystemColor(SWT.COLOR_WHITE));

        for (int i = 0; i < parcelParams.length; i++) {
            item = new TableItem(paramTable, SWT.BORDER);
            item.setText(0, "    " + parcelParams[i] + " ");
            item.setText(1, "");

        }
    }

    /*
     * (non-Javadoc)
     * 
     * @seecom.raytheon.viz.skewt.rsc.SkewTResource.ISkewTDataChangedListener#
     * skewTResourceChanged(com.raytheon.viz.skewt.rsc.SkewTResource)
     */
    @Override
    public void skewTResourceChanged(SkewTResource rsc) {
        if (rsc == displayedRsc) {
            updateColumn(rsc);

        } else {
            setDisplayedRsc(rsc);
        }
        if (insetMap != null) {
            insetMap.refresh();
        }
    }

    protected void setDisplayedRsc(SkewTResource rsc) {
        displayedRsc = rsc;
        updateColumn(rsc);
    }

    protected SkewTResource getDisplayedRsc() {
        return displayedRsc;
    }

    /**
     * @param rsc
     * @return
     */
    private String getRscTitle(final SkewTResource rsc) {
        if (rsc == null) {
            return "";
        }
        String title = "";
        if (rsc.getDataTimes().length == 0) {
            return "NO DATA";
        }

        DataTime dt = null;
        dt = rsc.getDisplayedDate();
        if (dt == null) {
            return title;
        }
        VerticalSounding sounding = rsc.getSoundingByDataTime(
                rsc.getDisplayedDate()).getInterleavedData();

        title = sounding.getName();
        String id = sounding.getStationId();
        if (title == null || title.isEmpty()) {
            title = id;
        }
        if ("Interactive".equals(id)) {
            title = id;
        }
        return title;
    }

    private void updateColumn(final SkewTResource rsc) {
        if (!paramTable.isDisposed()) {

            VizApp.runSync(new Runnable() {
                @Override
                public void run() {
                    paramValueColumn.setText(getRscTitle(rsc));
                    if (rsc == null) {
                        for (TableItem item : paramTable.getItems()) {
                            item.setText(1, "");
                        }

                        return;
                    }
                    SoundingParams sp = rsc.getSoundingParameters();

                    if (sp == null || sp.getInterleavedData().size() == 0) {
                        for (TableItem item : paramTable.getItems()) {
                            int i = 0;
                            if (i != 0 && i != generalParams.length) {
                                item.setText(1, "N/A");
                            }
                            i++;
                        }
                        return;
                    }
                    // general parameters
                    paramTable.getItem(1).setText(1, getPrecipWaterString(sp));
                    paramTable.getItem(2).setText(1, getKIndexString(sp));
                    paramTable.getItem(3).setText(1, getTotalTotalsString(sp));
                    paramTable.getItem(4).setText(1, getSWEATIndexString(sp));
                    paramTable.getItem(5).setText(1,
                            getConvectiveGustPotentialString(sp));
                    paramTable.getItem(6).setText(1, getFreezeLevelString(sp));
                    paramTable.getItem(7).setText(1,
                            getWetBulbZeroHeightString(sp));
                    paramTable.getItem(8).setText(1,
                            getZeroTo6KmAvgWndString(sp));
                    paramTable.getItem(9).setText(1,
                            getStormRelativeMotionString(sp));
                    paramTable.getItem(10).setText(1,
                            getStormRelativeHelicityString(sp));
                    paramTable.getItem(11).setText(1, getFcstMaxTempString(sp));
                    paramTable.getItem(12).setText(1, getTriggerTempString(sp));
                    paramTable.getItem(13)
                            .setText(1, getSoaringIndexString(sp));
                    paramTable.getItem(14).setText(1,
                            getMDPIwindexParmsString(sp));

                    // parcel parameters
                    paramTable.getItem(16).setText(1,
                            getParcelStartPressureString(sp));
                    paramTable.getItem(17).setText(1,
                            getParcelStartTempString(sp));
                    paramTable.getItem(18).setText(1,
                            getParcelStartDewpointString(sp));
                    paramTable.getItem(19).setText(1,
                            getConvectiveTempString(sp));
                    paramTable.getItem(20).setText(1, getLiftedIndexString(sp));
                    paramTable.getItem(21).setText(1, getCCLHeightString(sp));
                    paramTable.getItem(22).setText(1, getLCLHeightString(sp));
                    paramTable.getItem(23).setText(1, getLFCHeightString(sp));
                    paramTable.getItem(24).setText(1, getHailString(sp));
                    paramTable.getItem(25).setText(1, getMaxVertVelString(sp));
                    paramTable.getItem(26).setText(1, getEQHeightString(sp));
                    paramTable.getItem(27).setText(1, getCloudTopString(sp));
                    paramTable.getItem(28).setText(1, getCAPEString(sp));
                    paramTable.getItem(29).setText(1, getCINString(sp));
                    paramTable.getItem(30).setText(1, getRichardsonString(sp));
                    paramValueColumn.pack();
                }
            });
        }

    }

    private void addColumn(SkewTResource rsc) {
        rscList.add(rsc);
        rsc.addListener(SkewTEditor.this);
        setDisplayedRsc(rsc);
    }

    /**
     * Get rid of column
     * 
     * @param index
     */
    private void removeColumn(SkewTResource rsc) {
        rsc.removeListener(SkewTEditor.this);
        rscList.remove(rsc);

        if (displayedRsc == rsc) {
            int n = rscList.size();
            if (n > 0) {
                setDisplayedRsc(rscList.get(n - 1));
            } else {
                setDisplayedRsc(null);
            }
        }
    }

    /**
     * Add the listenerList
     * 
     * @param main
     */
    private void registerListener(VizDisplayPane pane) {
        synchronized (this) {
            if (pane != null) {
                try {
                    ResourceList rscList = pane.getRenderableDisplay()
                            .getDescriptor().getResourceList();
                    // add the Editor as a resource listener
                    rscList.addPostAddListener(this);
                    rscList.addPostRemoveListener(this);
                    for (ResourcePair rp : rscList) {
                        notifyAdd(rp);
                    }
                } catch (Exception e) {
                    statusHandler
                            .handle(Priority.PROBLEM,
                                    "Error recovering Resource, Can't add listenerList",
                                    e);
                }
            }
        }
    }

    protected void addCustomHandlers(InputManager manager) {
        /**
         * TODO: The skewt mouse inspect adapter should be added and live in a
         * resource
         */
        manager.registerMouseHandler(new SkewtMouseInspectAdapter());
    }

    @Override
    public void notifyAdd(ResourcePair rp) throws VizException {
        if (rp.getResource() instanceof SkewTResource) {
            SkewTResource rsc = (SkewTResource) rp.getResource();
            addColumn(rsc);
        }
    }

    @Override
    public void notifyRemove(ResourcePair rp) throws VizException {
        if (rp.getResource() instanceof SkewTResource) {
            SkewTResource rsc = (SkewTResource) rp.getResource();
            removeColumn(rsc);
        }
    }

    /**
     * Add a resource listener on first pane
     * 
     * @param SkewTResource
     */
    public void addResource(SkewTResource str) {
        if (str != null) {
            // This is the way grib resources are added
            graphPane.getRenderableDisplay().getDescriptor().getResourceList()
                    .add(str);
        }
    }

    /**
     * @param rsc
     *            The resource to activate the dialog for.
     * @param activate
     *            If true, create and activate, or if false, remove.
     * @throws VizException
     */
    public void showControlsDialog(SkewTResource rsc) throws VizException {

        if (skewtControlsDialog != null) {
            closeControlDialog();
        }
        final SkewTResource frsc = rsc;
        Display.getDefault().asyncExec(new Runnable() {

            public void run() {
                // skewtControlsDialog = SkewtControlsDialog.getInstance(
                // new Shell(), frsc.getName(),
                // (SkewtDisplay) displayPane[0].getRenderableDisplay());
                // skewtControlsDialog.open();
                skewtControlsDialog = SkewtControlsDialog.getInstance(
                        new Shell(), frsc.getName());
                skewtControlsDialog.open();
            }
        });
    }

    /**
     * Close param dialogs
     */
    public void closeControlDialog() {
        if (skewtControlsDialog.getShell() != null) {
            skewtControlsDialog.close();
            skewtControlsDialog = null;
        }
    }

    private String getConvertedValue(String fmt, float value) {
        if (value < SoundingLayer.NODATA) {
            return String.format(fmt, value);
        }
        return "N/A";
    }

    private String getPrecipWaterString(SoundingParams sp) {
        int n = sp.getTemperatures().length;
        if (n == 0 || sp.getPressures()[0] - sp.getPressures()[n - 1] < 300) {
            return "N/A";
        }
        return getConvertedValue("%.2f in", sp.precipWater());
    }

    private String getKIndexString(SoundingParams sp) {
        int n = sp.getTemperatures().length;
        if (n <= 1 || sp.getPressures()[n - 1] > 500
                || sp.getPressures()[0] < 800
                || sp.getTemperatures()[1] > SoundingLayer.NODATA) {
            return "N/A";
        }
        return getConvertedValue("%.0f", sp.KIndex());
    }

    private String getTotalTotalsString(SoundingParams sp) {
        int n = sp.getTemperatures().length;
        if (n <= 1 || sp.getPressures()[n - 1] > 500
                || sp.getPressures()[0] < 800
                || sp.getTemperatures()[1] > SoundingLayer.NODATA) {
            return "N/A";
        }
        return getConvertedValue("%.0f", sp.Totals());
    }

    private String getSWEATIndexString(SoundingParams sp) {
        float sweat = sp.sweatIdx();
        if (sweat >= SoundingLayer.NODATA) {
            return "N/A";
        }
        return String.format("%d", (int) sweat);
    }

    private String getConvectiveGustPotentialString(SoundingParams sp) {
        switch (sp.gustPotential()) {
        case 1: // (LOW LVL TOO MOIST)
            return "1: GST < 30 kts";
        case 2: // (UPR LVL TOO STABLE)
            return "2: GST < 30 kts";
        case 3:
            return "3: GST 30-40 kts";
        case 4:
            return "4: GST > 40 kts";
        default:
            return "N/A";
        }
    }

    private String getFreezeLevelString(SoundingParams sp) {
        float frzgLvlZ = sp.frzgLvlZ();
        if (frzgLvlZ >= SoundingLayer.NODATA) {
            return "N/A";
        }
        return String.format("%.0f ft ASL", metersToFeet.convert(frzgLvlZ));
    }

    private String getWetBulbZeroHeightString(SoundingParams sp) {
        float hgtWBZ = sp.hgtWBZ();
        if (hgtWBZ >= SoundingLayer.NODATA) {
            return "N/A";
        }
        return String.format("%d ft ASL", (int) metersToFeet.convert(hgtWBZ));
    }

    private String getZeroTo6KmAvgWndString(SoundingParams sp) {
        WindComp avgWind = sp.avgWind();

        if (avgWind == null
                || avgWind.getWindDirection() >= SoundingLayer.NODATA
                || avgWind.getWindSpeed() >= SoundingLayer.NODATA) {
            return "N/A";
        }

        return String.format("%d%c/%d kts", (int) avgWind.getWindDirection(),
                degree,
                (int) metersPerSecondToKnots.convert(avgWind.getWindSpeed()));
    }

    private String getStormRelativeMotionString(SoundingParams sp) {
        WindComp helicity = sp.helicity();

        if (helicity == null
                || helicity.getStormMotionDir() >= SoundingLayer.NODATA
                || helicity.getStormMotionSpd() >= SoundingLayer.NODATA) {
            return "N/A";
        }
        float stormDir = helicity.getStormMotionDir();
        float stormSpd = helicity.getStormMotionSpd();
        return String.format("%d%c/%d kts", (int) stormDir, degree,
                (int) stormSpd);
    }

    private String getStormRelativeHelicityString(SoundingParams sp) {
        WindComp helicity = sp.helicity();

        if (helicity == null
                || helicity.getStormRelativeHelicity() >= SoundingLayer.NODATA) {
            return "N/A";
        }

        float _SRelHelicity = helicity.getStormRelativeHelicity();
        return String.format("%d m%c/s%c", (int) _SRelHelicity, squared,
                squared);
    }

    private String getFcstMaxTempString(SoundingParams sp) {
        float maxTemp = sp.maxtemp();

        if (maxTemp >= SoundingLayer.NODATA) {
            return "N/A";
        }

        return String.format("%d%cC/%d%cF",
                (int) kelvinToCelsius.convert(maxTemp), degree,
                (int) kelvinToFahrenheit.convert(maxTemp), degree);
    }

    private String getTriggerTempString(SoundingParams sp) {
        float trigTemp = sp.triggerTemp_C();

        if (trigTemp >= SoundingLayer.NODATA) {
            return "N/A";
        }

        return String.format("%d%cC/%d%cF", (int) trigTemp, degree,
                (int) celsiusToFahrenheit.convert(trigTemp), degree);
    }

    private String getSoaringIndexString(SoundingParams sp) {
        float soarIndex = sp.soarIndex();

        if (soarIndex >= SoundingLayer.NODATA) {
            return "N/A";
        }

        return String.format("%d ft/min", (int) soarIndex);
    }

    private String getMDPIwindexParmsString(SoundingParams sp) {
        float mdpi = sp.mdpi();
        float windex = sp.windex();

        String s;
        if (mdpi < SoundingLayer.NODATA) {
            s = String.format("%.2f/", mdpi);
        } else {
            s = "NA/";
        }

        if (windex < SoundingLayer.NODATA) {
            s += String.format("%.0f", windex);
        } else {
            s += "NA";
        }

        return s;
    }

    private String getParcelStartPressureString(SoundingParams sp) {
        float initParP = sp.initParP();
        float elev = sp.getHeights()[0];
        if (elev > SoundingLayer.NODATA
                || sp.pressLCL() == SoundingLayer.MISSING
                || sp.hgtLCL() == SoundingLayer.MISSING
                || sp.getTemperatures().length < 2
                || sp.getTemperatures()[0] > SoundingLayer.NODATA) {
            return "N/A";
        }
        if (initParP >= SoundingLayer.NODATA) {
            return "N/A";
        }

        return String.format("%d mb", (int) initParP);
    }

    private String getParcelStartTempString(SoundingParams sp) {
        float initParT_C = sp.initParT_C();
        float elev = sp.getHeights()[0];
        if (elev > SoundingLayer.NODATA
                || sp.pressLCL() == SoundingLayer.MISSING
                || sp.hgtLCL() == SoundingLayer.MISSING
                || sp.getTemperatures().length < 2
                || sp.getTemperatures()[0] > SoundingLayer.NODATA) {
            return "N/A";
        }
        if (initParT_C >= SoundingLayer.NODATA) {
            return "N/A";
        }

        return String.format("%d%cC/%d%cF", (int) initParT_C, degree,
                (int) celsiusToFahrenheit.convert(initParT_C), degree);
    }

    private String getParcelStartDewpointString(SoundingParams sp) {
        float initParTd_C = sp.initParTd_C();
        float elev = sp.getHeights()[0];
        if (elev > SoundingLayer.NODATA
                || sp.pressLCL() == SoundingLayer.MISSING
                || sp.hgtLCL() == SoundingLayer.MISSING
                || sp.getTemperatures().length < 2
                || sp.getTemperatures()[0] > SoundingLayer.NODATA) {
            return "N/A";
        }
        if (initParTd_C >= SoundingLayer.NODATA) {
            return "N/A";
        }

        return String.format("%d%cC/%d%cF", (int) initParTd_C, degree,
                (int) celsiusToFahrenheit.convert(initParTd_C), degree);
    }

    private String getConvectiveTempString(SoundingParams sp) {
        float convTemp = sp.convTemp();
        if (convTemp >= SoundingLayer.NODATA) {
            return "N/A";
        }
        float tempCCL = sp.tempCCL();
        if (tempCCL == SoundingLayer.MISSING) {
            return "N/A";
        }

        return String.format("%d%cC/%d%cF",
                (int) kelvinToCelsius.convert(convTemp), degree,
                (int) kelvinToFahrenheit.convert(convTemp), degree);
    }

    private String getLiftedIndexString(SoundingParams sp) {
        float liftedI = sp.liftedI();
        if (liftedI >= SoundingLayer.NODATA) {
            return "N/A";
        }

        return String.format("%.1f", liftedI);
    }

    private String getCCLHeightString(SoundingParams sp) {
        float elev = sp.getHeights()[0];
        if (elev > SoundingLayer.NODATA
                || sp.initParT_C() > SoundingLayer.NODATA
                || sp.pressLCL() > SoundingLayer.NODATA
                || sp.hgtLCL() > SoundingLayer.NODATA
                || sp.tempLCL() > SoundingLayer.NODATA
                || sp.getTemperatures().length < 2
                || sp.getTemperatures()[0] > SoundingLayer.NODATA) {
            return "N/A";
        }
        return String.format("%d ft ASL/ %d mb",
                (int) metersToFeet.convert(sp.hgtCCL()), (int) sp.pressCCL());
    }

    private String getLCLHeightString(SoundingParams sp) {
        float elev = sp.getHeights()[0];
        if (elev > SoundingLayer.NODATA
                || sp.initParT_C() > SoundingLayer.NODATA
                || sp.pressLCL() > SoundingLayer.NODATA
                || sp.hgtLCL() > SoundingLayer.NODATA
                || sp.tempLCL() > SoundingLayer.NODATA
                || sp.getTemperatures().length < 2
                || sp.getTemperatures()[0] > SoundingLayer.NODATA) {
            return "N/A";
        }
        return String.format("%d ft ASL/ %d mb",
                (int) metersToFeet.convert(sp.hgtLCL()), (int) sp.pressLCL());
    }

    private String getLFCHeightString(SoundingParams sp) {
        float pLFC1 = sp.pLFC1();
        float zLFC1 = sp.zLFC1();
        float elev = sp.getHeights()[0];
        if (elev > SoundingLayer.NODATA
                || sp.initParT_C() > SoundingLayer.NODATA
                || sp.pressLCL() > SoundingLayer.NODATA
                || sp.hgtLCL() > SoundingLayer.NODATA
                || sp.tempLCL() > SoundingLayer.NODATA
                || sp.getTemperatures().length < 2
                || sp.getTemperatures()[0] > SoundingLayer.NODATA) {
            return "N/A";
        }
        if (zLFC1 >= SoundingLayer.NODATA) {
            return "N/A";
        }
        String s = String.format("%d ft ASL/ %d mb",
                (int) metersToFeet.convert(zLFC1), (int) pLFC1);

        float pLFC2 = sp.pLFC2();
        float zLFC2 = sp.zLFC2();
        if (zLFC2 < SoundingLayer.NODATA) {
            s += String.format("\n%d ft ASL/ %d mb",
                    (int) metersToFeet.convert(zLFC2), (int) pLFC2);
        }

        return s;
    }

    private String getHailString(SoundingParams sp) {
        float hailSize = sp.hailSize();
        float elev = sp.getHeights()[0];
        if (elev > SoundingLayer.NODATA
                || sp.initParT_C() > SoundingLayer.NODATA
                || sp.pressLCL() > SoundingLayer.NODATA
                || sp.hgtLCL() > SoundingLayer.NODATA
                || sp.tempLCL() > SoundingLayer.NODATA
                || sp.getTemperatures().length < 2
                || sp.getTemperatures()[0] > SoundingLayer.NODATA) {
            return "N/A";
        }
        if (hailSize >= SoundingLayer.NODATA) {
            return "N/A";
        }
        return String.format("%.1f cm/%.1f in", hailSize,
                centimetersToInches.convert(hailSize));
    }

    private String getMaxVertVelString(SoundingParams sp) {
        float maxVVel = sp.maxVVel();
        float elev = sp.getHeights()[0];
        if (elev > SoundingLayer.NODATA
                || sp.initParT_C() > SoundingLayer.NODATA
                || sp.pressLCL() > SoundingLayer.NODATA
                || sp.hgtLCL() > SoundingLayer.NODATA
                || sp.tempLCL() > SoundingLayer.NODATA
                || sp.getTemperatures().length < 2
                || sp.getTemperatures()[0] > SoundingLayer.NODATA) {
            return "N/A";
        }
        if (maxVVel >= SoundingLayer.NODATA) {
            return "N/A";
        }
        return String.format("%d m/s", (int) maxVVel);
    }

    private String getEQHeightString(SoundingParams sp) {
        float zEqLvl = sp.zEqLvl();
        float pEqLvl = sp.pEqLvl();
        float elev = sp.getHeights()[0];
        if (elev > SoundingLayer.NODATA
                || sp.initParT_C() > SoundingLayer.NODATA
                || sp.pressLCL() > SoundingLayer.NODATA
                || sp.hgtLCL() > SoundingLayer.NODATA
                || sp.tempLCL() > SoundingLayer.NODATA
                || sp.getTemperatures().length < 2
                || sp.getTemperatures()[0] > SoundingLayer.NODATA) {
            return "N/A";
        }
        if (zEqLvl >= SoundingLayer.NODATA || pEqLvl >= SoundingLayer.NODATA) {
            return "N/A";
        }
        return String.format("%d ft ASL/%d mb",
                (int) metersToFeet.convert(zEqLvl), (int) pEqLvl);
    }

    private String getCloudTopString(SoundingParams sp) {
        float hgtCldTop = sp.hgtCldTop();
        float elev = sp.getHeights()[0];
        if (elev > SoundingLayer.NODATA
                || sp.initParT_C() > SoundingLayer.NODATA
                || sp.pressLCL() > SoundingLayer.NODATA
                || sp.hgtLCL() > SoundingLayer.NODATA
                || sp.tempLCL() > SoundingLayer.NODATA
                || sp.getTemperatures().length < 2
                || sp.getTemperatures()[0] > SoundingLayer.NODATA) {
            return "N/A";
        }
        if (hgtCldTop >= SoundingLayer.NODATA) {
            return "N/A";
        }
        return String
                .format("%d ft ASL", (int) metersToFeet.convert(hgtCldTop));
    }

    private String getCAPEString(SoundingParams sp) {
        float posBuoy = sp.posBuoy();
        float elev = sp.getHeights()[0];
        if (elev > SoundingLayer.NODATA
                || sp.initParT_C() > SoundingLayer.NODATA
                || sp.pressLCL() > SoundingLayer.NODATA
                || sp.hgtLCL() > SoundingLayer.NODATA
                || sp.tempLCL() > SoundingLayer.NODATA
                || sp.getTemperatures().length < 2
                || sp.getTemperatures()[0] > SoundingLayer.NODATA) {
            return "N/A";
        }
        if (posBuoy >= SoundingLayer.NODATA) {
            return "N/A";
        }
        if ((int) posBuoy == 0) {
            return "NONE";
        }
        return String.format("%d J/KG", (int) posBuoy);
    }

    private String getCINString(SoundingParams sp) {
        float negBuoy = sp.negBuoy();
        float elev = sp.getHeights()[0];
        if (elev > SoundingLayer.NODATA
                || sp.initParT_C() > SoundingLayer.NODATA
                || sp.pressLCL() > SoundingLayer.NODATA
                || sp.hgtLCL() > SoundingLayer.NODATA
                || sp.tempLCL() > SoundingLayer.NODATA
                || sp.getTemperatures().length < 2
                || sp.getTemperatures()[0] > SoundingLayer.NODATA) {
            return "N/A";
        }
        if (negBuoy >= SoundingLayer.NODATA) {
            return "N/A";
        }
        if ((int) negBuoy == 0) {
            return "NONE";
        }
        return String.format("%d J/KG", (int) negBuoy);
    }

    private String getRichardsonString(SoundingParams sp) {
        if (sp.zLFC1() >= SoundingLayer.NODATA
                || sp.pEqLvl() >= SoundingLayer.NODATA) {
            return "N/A";
        }
        float richNum = sp.richNum();
        float elev = sp.getHeights()[0];
        if (elev > SoundingLayer.NODATA
                || sp.initParT_C() > SoundingLayer.NODATA
                || sp.pressLCL() > SoundingLayer.NODATA
                || sp.hgtLCL() > SoundingLayer.NODATA
                || sp.tempLCL() > SoundingLayer.NODATA
                || sp.getTemperatures().length < 2
                || sp.getTemperatures()[0] > SoundingLayer.NODATA) {
            return "N/A";
        }
        if (richNum == 0 || richNum >= SoundingLayer.NODATA) {
            return "NONE";
        }
        return String.format("%.1f", richNum);
    }

    // Methods copied from AbstractXyEditor Jan 18th, 2010

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.ui.editor.AbstractEditor#init(org.eclipse.ui.IEditorSite
     * , org.eclipse.ui.IEditorInput)
     */
    @Override
    public void init(IEditorSite site, IEditorInput input)
            throws PartInitException {
        inputManager = new InputManager(this);
        super.init(site, input);
        this.renderableDisplay = displaysToLoad[0];
        if (renderableDisplay instanceof IInsetMapContainer
                && ((IInsetMapContainer) renderableDisplay)
                        .getInsetMapLocation() != null) {
            hasCornerMap = true;
        }
    }

    /**
     * Copied from AbstractXyEditor's createPartControl when making SkewTEditor
     * separate
     * 
     * @param parent
     */
    public void createPartControlParent(Composite parent) {
        this.parent = parent;
        parent.setLayout(new FormLayout());
        parent.addFocusListener(new FocusAdapter() {
            @Override
            public void focusLost(FocusEvent e) {
                activeDisplayPane = graphPane;
            }
        });
        parent.addMouseTrackListener(new MouseTrackAdapter() {
            @Override
            public void mouseExit(MouseEvent e) {
                activeDisplayPane = graphPane;
            }
        });
        try {
            if (hasCornerMap) {
                constructInsetMap(renderableDisplay);
            }
            Composite graphComposite = new Composite(parent, SWT.NONE);
            graphComposite.setLayout(new FormLayout());
            graphComposite.setLayoutData(getFullFormData());
            graphPane = new VizDisplayPane(this, graphComposite,
                    renderableDisplay);
            activeDisplayPane = graphPane;
            graphPane.getCanvas().setLayoutData(getFullFormData());
            graphPane.getCanvas().addMouseTrackListener(
                    new MouseTrackAdapter() {
                        @Override
                        public void mouseEnter(MouseEvent e) {
                            activeDisplayPane = graphPane;
                        }
                    });
        } catch (VizException e) {
            statusHandler.handle(Priority.PROBLEM,
                    "Error setting up xy editor", e);
        }
        graphPane.setRenderableDisplay(this.renderableDisplay);
        addCustomHandlers(inputManager);
        registerListeners(graphPane);
        if (insetMap != null) {
            registerListeners(insetMap);
        }

        graphPane.setRenderableDisplay(this.renderableDisplay);

        contributePerspectiveActions();
    }

    private void registerListeners(IDisplayPane pane) {
        pane.addListener(SWT.MouseUp, inputManager);
        pane.addListener(SWT.MouseDown, inputManager);
        pane.addListener(SWT.MouseMove, inputManager);
        pane.addListener(SWT.MouseWheel, inputManager);
        pane.addListener(SWT.MouseHover, inputManager);
        pane.addListener(SWT.MouseDoubleClick, inputManager);
        pane.addListener(SWT.KeyDown, inputManager);
        pane.addListener(SWT.KeyUp, inputManager);
        pane.addListener(SWT.MenuDetect, inputManager);
        pane.addListener(SWT.MouseExit, inputManager);
    }

    /**
     * @param renderableDisplay
     */
    private void constructInsetMap(IRenderableDisplay renderableDisplay)
            throws VizException {
        if (insetMap != null) {
            insetMap.setRenderableDisplay(InsetMapUtil
                    .loadInsetMap(renderableDisplay));
        } else {
            Composite insetParent = new Composite(this.parent, SWT.NONE);
            insetParent.setLayout(new FormLayout());
            insetParent.setLayoutData(((IInsetMapContainer) renderableDisplay)
                    .getInsetMapLocation());
            insetMap = new VizDisplayPane(this, insetParent,
                    InsetMapUtil.loadInsetMap(renderableDisplay), true);
            insetMap.getCanvas().setLayoutData(getFullFormData());
            insetMap.getCanvas().addMouseTrackListener(new MouseTrackAdapter() {
                @Override
                public void mouseEnter(MouseEvent e) {
                    activeDisplayPane = insetMap;
                }

                @Override
                public void mouseExit(MouseEvent e) {
                    activeDisplayPane = graphPane;
                }
            });
        }
    }

    /**
     * @return
     */
    private static FormData getFullFormData() {
        FormData fd = new FormData();
        fd.right = new FormAttachment(100, 0);
        fd.left = new FormAttachment(0, 0);
        fd.top = new FormAttachment(0, 0);
        fd.bottom = new FormAttachment(100, 0);
        return fd;
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.viz.ui.editor.AbstractEditor#getDisplayPane()
     */
    @Override
    public IDisplayPane[] getDisplayPanes() {
        return new IDisplayPane[] { activeDisplayPane };
    }

    @Override
    public void registerMouseHandler(IInputHandler handler,
            InputPriority priority) {
        inputManager.registerMouseHandler(handler, priority);
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.ui.editor.AbstractEditor#registerMouseHandler(com.raytheon
     * .viz.ui.mouse.IMouseHandler)
     */
    @Override
    public void registerMouseHandler(IInputHandler handler) {
        inputManager.registerMouseHandler(handler);
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.ui.editor.AbstractEditor#unregisterMouseHandler(com.
     * raytheon.viz.ui.mouse.IMouseHandler)
     */
    @Override
    public void unregisterMouseHandler(IInputHandler handler) {
        inputManager.unregisterMouseHandler(handler);
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.viz.ui.editor.AbstractEditor#translateClick(double,
     * double)
     */
    @Override
    public Coordinate translateClick(double x, double y) {
        double[] grid = getActiveDisplayPane().screenToGrid(x, y, 0);

        return new Coordinate(grid[0], grid[1], grid[2]);
    }

    /*
     * (non-Javadoc)
     * 
     * @seecom.raytheon.viz.ui.editor.AbstractEditor#translateInverseClick(com.
     * vividsolutions.jts.geom.Coordinate)
     */
    @Override
    public double[] translateInverseClick(Coordinate c) {

        if (Double.isNaN(c.z)) {
            c.z = 0.0;
        }
        return getActiveDisplayPane().gridToScreen(
                new double[] { c.x, c.y, c.z });
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.viz.ui.editor.AbstractEditor#getActiveDisplayPane()
     */
    @Override
    public IDisplayPane getActiveDisplayPane() {
        return activeDisplayPane;
    }

    @Override
    public void setFocus() {
        activeDisplayPane = graphPane;
        activeDisplayPane.setFocus();
    }

    @Override
    public void renderableDisplayChanged(IDisplayPane pane,
            IRenderableDisplay newRenderableDisplay, DisplayChangeType type) {
        if (pane == graphPane && hasCornerMap && type == DisplayChangeType.ADD) {
            // the graph pane was updated, update the inset map
            try {
                constructInsetMap(newRenderableDisplay);
            } catch (VizException e) {
                statusHandler.handle(Priority.PROBLEM,
                        "Error reconstructing inset map", e);
            }
        }
    }

    @Override
    public void doSave(IProgressMonitor monitor) {
        // TODO Auto-generated method stub

    }

    @Override
    public void doSaveAs() {
        // TODO Auto-generated method stub

    }

    @Override
    public InputManager getMouseManager() {
        return inputManager;
    }

    @Override
    public boolean isSaveAsAllowed() {
        // TODO Auto-generated method stub
        return false;
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.viz.ui.editor.AbstractEditor#getNewPaneManager()
     */
    @Override
    protected PaneManager getNewPaneManager() {
        return null;
    }

    @Override
    public IDisplayPane addPane(IRenderableDisplay renderableDisplay) {
        return null;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.xy.map.IInsetMapDisplayPaneContainer#getInsetPanes()
     */
    @Override
    public IDisplayPane[] getInsetPanes() {
        return new IDisplayPane[] { insetMap };
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.xy.map.IInsetMapDisplayPaneContainer#getInsetPanes
     * (com.raytheon.uf.viz.core.IDisplayPane)
     */
    @Override
    public IDisplayPane[] getInsetPanes(IDisplayPane pane) {
        if (pane == graphPane) {
            return getInsetPanes();
        } else {
            return new IDisplayPane[] {};
        }
    }

}
