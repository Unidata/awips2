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
package com.raytheon.viz.mpe.ui.dialogs.hourlyradar;

import java.awt.Rectangle;
import java.io.File;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Layout;
import org.eclipse.swt.widgets.Menu;
import org.eclipse.swt.widgets.MenuItem;
import org.eclipse.swt.widgets.Shell;
import org.locationtech.jts.geom.Coordinate;
import org.opengis.metadata.spatial.PixelOrientation;

import com.raytheon.uf.common.dataplugin.shef.tables.Colorvalue;
import com.raytheon.uf.common.localization.PathManagerFactory;
import com.raytheon.uf.common.mpe.util.DPAFile;
import com.raytheon.uf.common.mpe.util.RadarCoverageFile;
import com.raytheon.uf.common.ohd.AppsDefaults;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.common.xmrg.hrap.HRAP;
import com.raytheon.uf.viz.core.IDisplayPane;
import com.raytheon.uf.viz.core.IExtent;
import com.raytheon.uf.viz.core.IPane;
import com.raytheon.uf.viz.core.IPane.CanvasType;
import com.raytheon.uf.viz.core.IRenderableDisplayChangedListener;
import com.raytheon.uf.viz.core.IRenderableDisplayChangedListener.DisplayChangeType;
import com.raytheon.uf.viz.core.InputManager;
import com.raytheon.uf.viz.core.datastructure.LoopProperties;
import com.raytheon.uf.viz.core.drawables.IRenderableDisplay;
import com.raytheon.uf.viz.core.drawables.ResourcePair;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.procedures.Bundle;
import com.raytheon.uf.viz.core.rsc.AbstractVizResource;
import com.raytheon.uf.viz.core.rsc.IInputHandler;
import com.raytheon.uf.viz.core.rsc.IInputHandler.InputPriority;
import com.raytheon.uf.viz.core.rsc.LoadProperties;
import com.raytheon.uf.viz.core.rsc.ResourceProperties;
import com.raytheon.viz.hydrocommon.whfslib.colorthreshold.GetColorValues;
import com.raytheon.viz.hydrocommon.whfslib.colorthreshold.NamedColorUseSet;
import com.raytheon.viz.mpe.MPEDateFormatter;
import com.raytheon.viz.mpe.core.MPEDataManager;
import com.raytheon.viz.mpe.core.MPEDataManager.MPERadarData;
import com.raytheon.viz.mpe.ui.MPEDisplayManager;
import com.raytheon.viz.mpe.ui.rsc.DPAResource;
import com.raytheon.viz.mpe.ui.rsc.DPAResource.SingleSiteRadarType;
import com.raytheon.viz.mpe.ui.rsc.RadarCoverageResource;
import com.raytheon.viz.mpe.ui.rsc.RadarGageOverlayRsc;
import com.raytheon.viz.mpe.ui.rsc.RadarGageOverlayRscData;
import com.raytheon.viz.ui.BundleLoader;
import com.raytheon.viz.ui.EditorUtil;
import com.raytheon.viz.ui.dialogs.CaveSWTDialog;
import com.raytheon.viz.ui.editor.IMultiPaneEditor;
import com.raytheon.viz.ui.editor.ISelectedPanesChangedListener;
import com.raytheon.viz.ui.input.InputAdapter;
import com.raytheon.viz.ui.panes.PaneManager;

/**
 * Review Hourly Radar Dialog.
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jul 22, 2009 2675       mpduff     Initial creation
 * Aug 13, 2009 2675       mpduff     TIM changes added
 * Nov 08, 2009 3232       mpduff     Implement the precip gage overlay
 * Jun 18, 2013 16053      snaples    Removed reference to setRadarEditFlag
 * Jun 05, 2015 4401       bkowal     Renamed LoadSerializedXml to
 *                                    LoadPerspectiveHandler.
 * Sep 13, 2022 8792       mapeters   Added new methods to support combo editor
 * Oct 12, 2022 8946       mapeters   Added getCanvases(CanvasType)
 * May 11, 2023 2029803    mapeters   Add horizontal layout methods
 * Sep 05, 2024 2037782    jsebahar   Remove the 2 panels that displayed DPA radar
 *                                    and Dpa bias adjusted radar. Also removed menu
 *                                    menu options for Supplemental data and
 *                                    Adaptable params.
 *
 * </pre>
 *
 * @author mpduff
 */
public class ReviewHourlyRadarDlg extends CaveSWTDialog
        implements IMultiPaneEditor {

    private static final IUFStatusHandler statusHandler = UFStatus
            .getHandler(ReviewHourlyRadarDlg.class);

    /** Apps Default DPA Grid Directory token. */
    private static final String DPA_GRID_DIR_TOKEN = "dpa_grid_dir";

    /** Apps Default DAA Grid Directory token. */
    private static final String DAA_GRID_DIR_TOKEN = "daa_grid_dir";

    /** Apps Default Radar Coverage Directory token. */
    private static final String RADAR_COVERAGE_DIR_TOKEN = "rfcwide_misbin_dir";

    /** Newline character */
    private static final String newLine = "\n";

    /** Bundle file location */
    private static final String BUNDLE_LOC = "bundles/MPE/reviewHourlyRadarBundle.xml";

    /** Popup shell. */
    private Shell popupShell;

    private LoopProperties loopProps = new LoopProperties();

    // Menu items
    private MenuItem rfcBoundaryMI;

    private MenuItem statesMI;

    private MenuItem countiesMI;

    private MenuItem citiesMI;

    private MenuItem riversMI;

    private MenuItem gagesMI;

    private MenuItem radarUmbrellaMI;

    private Label biasLbl;

    /** The radar Id */
    private String radId = null;

    /** Radar SP bias value */
    private String biasValue = null;

    /** Raw DP Radar resource */
    private DPAResource rawDPRadarRsc = null;

    /** DAA top-of-hour datetime */
    private Date daaDate = null;

    /**
     * Time used for the query in the DAARadar table
     */
    private Date daaProductDateTime = null;

    /** Popup displayed flag */
    private boolean popupOpen = false;

    private final PaneManager paneManager = new PaneManager();

    /**
     * Constructor.
     *
     * @param parentShell
     *            The parent shell
     * @param radId
     *            The radar id
     */
    public ReviewHourlyRadarDlg(Shell parentShell, String radId) {
        super(parentShell);
        setText("Single Radar Site");

        this.radId = radId;

        biasValue = getBiasValue(radId);

        // this code is generating the TOH date/time (ex: 14:00)
        daaDate = MPEDisplayManager.getCurrent().getCurrentEditDate();

        daaProductDateTime = RadarDataManager.getInstance()
                .getClosestObstimeToTOH(radId, daaDate);

    }

    @Override
    protected Layout constructShellLayout() {
        // Create the main layout for the shell.
        GridLayout mainLayout = new GridLayout(1, true);
        mainLayout.marginHeight = 1;
        mainLayout.marginWidth = 1;
        mainLayout.horizontalSpacing = 1;
        mainLayout.verticalSpacing = 1;
        return mainLayout;
    }

    @Override
    protected void initializeComponents(Shell shell) {
        setReturnValue(false);
        // Initialize all of the controls and layout
        initializeComponents();
    }

    /**
     * Initialize the gui widgets
     */
    private void initializeComponents() {
        Composite comp = new Composite(shell, SWT.NONE);
        GridLayout gl = new GridLayout(1, true);
        gl.horizontalSpacing = 1;
        gl.verticalSpacing = 1;
        gl.marginHeight = 1;
        gl.marginWidth = 1;
        comp.setLayout(gl);
        GridData gd = new GridData(600, SWT.DEFAULT);
        comp.setLayoutData(gd);

        createMenus();
        createTwoPanel(comp);

        createLowerLabels(comp);
    }

    /**
     * Create the menus
     */
    private void createMenus() {
        Menu menuBar = new Menu(shell, SWT.BAR);

        createControlMenu(menuBar);
        createOverlayMenu(menuBar);

        shell.setMenuBar(menuBar);
    }

    /**
     * Create the control menu
     *
     * @param menuBar
     *            The menubar
     */
    private void createControlMenu(Menu menuBar) {
        RadarDataManager radarDataManager = RadarDataManager.getInstance();

        // Create the Control menu
        MenuItem controlMI = new MenuItem(menuBar, SWT.CASCADE);
        controlMI.setText("Control");

        Menu controlMenu = new Menu(menuBar);
        controlMI.setMenu(controlMenu);

        // Close menu item
        MenuItem closeMI = new MenuItem(controlMenu, SWT.NONE);
        closeMI.setText("&Close\tCtrl+C");
        closeMI.setAccelerator(SWT.CTRL + 'C');
        closeMI.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                shell.dispose();
            }
        });

        // Create the Options menu
        MenuItem optionsMI = new MenuItem(menuBar, SWT.CASCADE);
        optionsMI.setText("Options");

        Menu optionsMenu = new Menu(menuBar);
        optionsMI.setMenu(optionsMenu);

        // Edit Bias Value menu item
        MenuItem editBiasValueMI = new MenuItem(optionsMenu, SWT.NONE);
        editBiasValueMI.setText("Edit Bias Value");
        editBiasValueMI.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                EditBiasDlg dlg = new EditBiasDlg(shell, radId, biasValue);
                double bias = (Double) dlg.open();
                biasValue = String.format("%-1.2f", bias);
                biasLbl.setText("  Bias Value: " + biasValue + "  ");
            }
        });

        // Ignore Radar SP menu item
        boolean ignoreRadarSP = false;
        try {
            ignoreRadarSP = radarDataManager.getIgnoreRadarSP(radId, daaDate);
        } catch (VizException e) {
            statusHandler.error("Error getting Ignore SP Radar Flag from IHFS",
                    e);
        }
        MenuItem ignoreRadarSPMI = new MenuItem(optionsMenu, SWT.CHECK);
        ignoreRadarSPMI.setText("Ignore Single Pol Radar Product");
        ignoreRadarSPMI.setSelection(ignoreRadarSP);
        ignoreRadarSPMI.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                MenuItem ir = (MenuItem) event.getSource();
                boolean ignoreRadarSP = ir.getSelection();

                try {
                    int status = RadarDataManager.getInstance()
                            .updateIgnoreRadarSP(radId, daaDate, ignoreRadarSP);
                    if (status == 0) {
                        statusHandler.error(
                                "Update of Ignore SP field not successful");
                    }
                } catch (VizException e) {
                    statusHandler.error(
                            "Error updating ignore SP radar flag in IHFS", e);
                }

                for (IDisplayPane pane : getDisplayPanes()) {
                    List<DPAResource> rscs = pane.getDescriptor()
                            .getResourceList()
                            .getResourcesByTypeAsType(DPAResource.class);
                    for (DPAResource rsc : rscs) {
                        rsc.update();
                    }
                }
            }
        });

        // Ignore Radar DP menu item
        boolean ignoreRadarDP = false;
        try {
            ignoreRadarDP = radarDataManager.getIgnoreRadarDP(radId, daaDate);
        } catch (VizException e) {
            statusHandler.error("Error getting Ignore DP Radar Flag from IHFS",
                    e);
        }
        MenuItem ignoreRadarDPMI = new MenuItem(optionsMenu, SWT.CHECK);
        ignoreRadarDPMI.setText("Ignore Dual Pol Radar Product");
        ignoreRadarDPMI.setSelection(ignoreRadarDP);
        ignoreRadarDPMI.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                MenuItem ir = (MenuItem) event.getSource();
                boolean ignoreRadarDP = ir.getSelection();

                try {
                    int status = RadarDataManager.getInstance()
                            .updateIgnoreRadarDP(radId, daaDate, ignoreRadarDP);
                    if (status == 0) {
                        statusHandler.error(
                                "Update of Ignore DP field not successful");
                    }
                } catch (VizException e) {
                    statusHandler.error(
                            "Error updating ignore DP radar flag in IHFS", e);
                }

                for (IDisplayPane pane : getDisplayPanes()) {
                    List<DPAResource> rscs = pane.getDescriptor()
                            .getResourceList()
                            .getResourcesByTypeAsType(DPAResource.class);
                    for (DPAResource rsc : rscs) {
                        rsc.update();
                    }
                }
            }
        });

    }

    /**
     * Create the overlays menu
     *
     * @param menuBar
     *            The MenuBar
     */
    private void createOverlayMenu(Menu menuBar) {
        // Create the Overlays menu
        MenuItem overlayMI = new MenuItem(menuBar, SWT.CASCADE);
        overlayMI.setText("Overlays");

        Menu overlayMenu = new Menu(menuBar);
        overlayMI.setMenu(overlayMenu);

        // RFC Boundaries menu item
        rfcBoundaryMI = new MenuItem(overlayMenu, SWT.CHECK);
        rfcBoundaryMI.setText("RFC Boundaries");
        rfcBoundaryMI.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                toggleOverlay((MenuItem) event.getSource());
            }
        });

        // States menu item
        statesMI = new MenuItem(overlayMenu, SWT.CHECK);
        statesMI.setText("States");
        statesMI.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                toggleOverlay((MenuItem) event.getSource());
            }
        });

        // Counties menu item
        countiesMI = new MenuItem(overlayMenu, SWT.CHECK);
        countiesMI.setText("Counties");
        countiesMI.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                toggleOverlay((MenuItem) event.getSource());
            }
        });

        // Cities menu item
        citiesMI = new MenuItem(overlayMenu, SWT.CHECK);
        citiesMI.setText("Cities/Towns");
        citiesMI.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                toggleOverlay((MenuItem) event.getSource());
            }
        });

        // Basin Boundaries menu item
        MenuItem basinBoundariesMI = new MenuItem(overlayMenu, SWT.CHECK);
        basinBoundariesMI.setText("Basin Boundaries");
        basinBoundariesMI.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                toggleOverlay((MenuItem) event.getSource());
            }
        });

        // Rivers menu item
        riversMI = new MenuItem(overlayMenu, SWT.CHECK);
        riversMI.setText("Rivers");
        riversMI.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                toggleOverlay((MenuItem) event.getSource());
            }
        });

        // Precip Gages menu item
        gagesMI = new MenuItem(overlayMenu, SWT.CHECK);
        gagesMI.setText("Precip Gages");
        gagesMI.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                toggleOverlay((MenuItem) event.getSource());
            }
        });

        // Radar Umbrella menu item
        radarUmbrellaMI = new MenuItem(overlayMenu, SWT.CHECK);
        radarUmbrellaMI.setText("Radar Umbrella");
        radarUmbrellaMI.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                toggleOverlay((MenuItem) event.getSource());
            }
        });
    }

    /**
     * Create the 2 panel display
     *
     * @param mainComp
     *            The main composite of the dialog
     */
    private void createTwoPanel(Composite mainComp) {
        Composite paneComp = new Composite(mainComp, SWT.NONE);
        GridData gd = new GridData(SWT.FILL, SWT.FILL, true, true);
        gd.widthHint = 600;
        gd.heightHint = 600;
        paneComp.setLayoutData(gd);

        paneManager.initializeComponents(this, paneComp);
        initializeMaps();
    }

    /**
     * Create the labels along the bottom
     *
     * @param mainComp
     *            The main composite
     */
    private void createLowerLabels(Composite mainComp) {
        Composite labelComp = new Composite(mainComp, SWT.NONE);
        GridLayout gl = new GridLayout(4, false);
        GridData gd = new GridData(SWT.LEFT, SWT.CENTER, true, true);
        labelComp.setLayout(gl);
        labelComp.setLayoutData(gd);

        Label radarLbl = new Label(labelComp, SWT.LEAD | SWT.BORDER);
        radarLbl.setText("  Radar: " + radId + "  ");
        radarLbl.setBackground(Display.getDefault()
                .getSystemColor(SWT.COLOR_WIDGET_BACKGROUND));

        Date currentDate = MPEDisplayManager.getCurrent().getCurrentEditDate();
        Label dateLbl = new Label(labelComp, SWT.LEAD | SWT.BORDER);
        dateLbl.setText("  "
                + MPEDateFormatter.format_MMM_dd_yyyy_HH(currentDate) + "z  ");
        dateLbl.setBackground(Display.getDefault()
                .getSystemColor(SWT.COLOR_WIDGET_BACKGROUND));

        biasLbl = new Label(labelComp, SWT.LEAD | SWT.BORDER);
        biasLbl.setText("  Bias Value: " + biasValue + "  ");
        biasLbl.setBackground(Display.getDefault()
                .getSystemColor(SWT.COLOR_WIDGET_BACKGROUND));
    }

    /**
     * Initialize the maps.
     *
     * @param comp
     *            The composite to hold the 2 panels
     */
    private void initializeMaps() {
        AppsDefaults appsDefaults = AppsDefaults.getInstance();
        RadarDataManager radarManager = RadarDataManager.getInstance();
        File defaultBundle = PathManagerFactory.getPathManager()
                .getStaticFile(BUNDLE_LOC);
        try {
            new BundleLoader(this,
                    Bundle.unmarshalBundle(defaultBundle, getSubstitutions()))
                            .run();

            IDisplayPane[] panes = getDisplayPanes();

            String user_id = System.getProperty("user.name");
            String app_name = "hmapmpe";
            List<NamedColorUseSet> pColorSetGroup = MPEDisplayManager
                    .getCurrent().getColorSetGroup();

            // TODO: Replace with resources looking up color maps themselves!!!
            List<Colorvalue> colorSet = GetColorValues.get_colorvalues(user_id,
                    app_name, "PRECIP_ACCUM", 3600, "E", pColorSetGroup);

            List<Colorvalue> radCovColorSet = GetColorValues.get_colorvalues(
                    user_id, app_name, "RADCOV", 3600, "E", pColorSetGroup);

            // Get the DAA Data
            if (daaProductDateTime == null) {
                daaProductDateTime = daaDate;
            }
            String daaFilename = radarManager.getDAAFileName(radId,
                    daaProductDateTime);
            String daaDirname = appsDefaults.getToken(DAA_GRID_DIR_TOKEN);

            DPAFile daaFile = new DPAFile(daaDirname + "/" + daaFilename);

            // probably not needed
            daaFile.setBiasValue(1.00);

            // Get the Radar Coverage Data
            String radCovFilename = "misbin." + radId;
            String radCovDirname = appsDefaults
                    .getToken(RADAR_COVERAGE_DIR_TOKEN);
            RadarCoverageFile radCovFile = new RadarCoverageFile(
                    radCovDirname + "/" + radCovFilename);

            int ngrd = radarManager.getNgrd(radId);

            RadarGageOverlayRscData rscData = new RadarGageOverlayRscData(
                    "Precip Gages", null);
            RadarGageOverlayRsc gageRsc = rscData.construct(
                    new LoadProperties(), EditorUtil.getActiveVizContainer()
                            .getActiveDisplayPane().getDescriptor());

            ResourceProperties gageResourceProperties = new ResourceProperties();
            gageResourceProperties.setMapLayer(true);
            gageResourceProperties.setSystemResource(false);
            gageResourceProperties.setVisible(false);
            gageResourceProperties.setResource(gageRsc);
            panes[0].getDescriptor().getResourceList().add(gageRsc,
                    gageResourceProperties);

            // Pane 0 (DP radar display)

            rawDPRadarRsc = new DPAResource(daaFile, colorSet, radId,
                    SingleSiteRadarType.RAW_DP_RADAR, ngrd);

            panes[0].getDescriptor().getResourceList().add(rawDPRadarRsc);
            panes[0].getDescriptor().getResourceList().add(gageRsc,
                    gageResourceProperties);

            // Pane 1 (radar coverage map display)

            RadarCoverageResource misbinRadarRsc = new RadarCoverageResource(
                    radCovFile, radCovColorSet, radId, ngrd);

            panes[1].getDescriptor().getResourceList().add(misbinRadarRsc);
            panes[1].getDescriptor().getResourceList().add(gageRsc,
                    gageResourceProperties);

            initOverlays();

            registerMouseHandler(new InputAdapter() {

                @Override
                public boolean handleMouseDown(int x, int y, int mouseButton) {
                    if (mouseButton == 3) {
                        createPopup(x, y);
                        return true;
                    }
                    return super.handleMouseDown(x, y, mouseButton);
                }

                @Override
                public boolean handleMouseUp(int x, int y, int mouseButton) {
                    if (popupOpen) {
                        closePopup();
                    }
                    return super.handleMouseUp(x, y, mouseButton);
                }

            });
        } catch (VizException e) {
            statusHandler.handle(Priority.PROBLEM, "Error initializing panes",
                    e);
            shell.dispose();
        }
    }

    /**
     * @return
     */
    private Map<String, String> getSubstitutions() {
        Map<String, String> subs = new HashMap<>();
        RadarDataManager rdm = RadarDataManager.getInstance();
        double[] latlon = rdm.getLatLon(radId);
        subs.put("mapCenter", latlon[0] + " " + latlon[1] + " 0.0");
        return subs;
    }

    /**
     * Toggle the overlays off/on. The mapName in the bundle (resource name)
     * must be equal to the text of the menu.
     *
     * @param mi
     *            The menuItem selected
     */
    private void toggleOverlay(MenuItem mi) {
        boolean visible = mi.getSelection();
        for (IDisplayPane pane : getDisplayPanes()) {
            for (ResourcePair rp : pane.getDescriptor().getResourceList()) {
                if (rp.getProperties().isMapLayer()
                        && rp.getResource().getName().equals(mi.getText())) {
                    rp.getProperties().setVisible(visible);
                }
            }
        }
    }

    /**
     * Initialize the overlays.
     */
    private void initOverlays() {
        boolean first = true;
        for (IDisplayPane pane : getDisplayPanes()) {
            if (first) {
                for (ResourcePair rp : pane.getDescriptor().getResourceList()) {
                    if (rp.getProperties().isVisible()
                            && rp.getProperties().isMapLayer()) {
                        setOverlayMenu(rp.getResource());
                    }
                }
                first = false;
            }
        }
    }

    /**
     * Sets the initial state of the overlay menus.
     *
     * @param rsc
     */
    private void setOverlayMenu(AbstractVizResource<?, ?> rsc) {
        if (rsc.getName().equalsIgnoreCase(citiesMI.getText())) {
            citiesMI.setSelection(true);
        } else if (rsc.getName().equalsIgnoreCase(countiesMI.getText())) {
            countiesMI.setSelection(true);
        } else if (rsc.getName().equalsIgnoreCase(gagesMI.getText())) {
            gagesMI.setSelection(true);
        } else if (rsc.getName().equalsIgnoreCase(radarUmbrellaMI.getText())) {
            radarUmbrellaMI.setSelection(true);
        } else if (rsc.getName().equalsIgnoreCase(rfcBoundaryMI.getText())) {
            rfcBoundaryMI.setSelection(true);
        } else if (rsc.getName().equalsIgnoreCase(riversMI.getText())) {
            riversMI.setSelection(true);
        } else if (rsc.getName().equalsIgnoreCase(statesMI.getText())) {
            statesMI.setSelection(true);
        }
    }

    /**
     * Create the Location Data popup.
     *
     * @param e
     *            The MouseEvent for the click
     */
    private void createPopup(int x, int y) {
        // Lat/Lon of mouse click
        Coordinate coord = translateClick(x, y);

        /* calculate national HRAP coordinates from lat/lon */
        Coordinate hp = new Coordinate(0, 0);
        try {
            hp = HRAP.getInstance().latLonToGridCoordinate(coord,
                    PixelOrientation.LOWER_LEFT);

            /* Get local hrap coordinate */
            Rectangle localHrapExtent = rawDPRadarRsc.getExtent();

            int localX = (int) (hp.x - localHrapExtent.x);
            int localY = (int) (hp.y - localHrapExtent.y);

            /* Get data values */
            StringBuilder buffer = new StringBuilder();
            buffer.append("National HRAP: x=");
            buffer.append((int) hp.x);
            buffer.append(" y=");
            buffer.append((int) hp.y);
            buffer.append(newLine);

            buffer.append("Latitude: " + String.format("%.2f", coord.x));
            buffer.append("  Longitude: " + String.format("%.2f", coord.y));
            buffer.append(newLine);

            buffer.append("Local HRAP: x=" + localX + " y=" + localY);
            buffer.append(newLine);

            /*
             * value divided by 100 and divided by 25.4 to convert to inches in
             * locator popup
             */
            double value;

            value = rawDPRadarRsc.getData()[localX][localY] / 100 / 25.4;
            if (value >= 0) {
                buffer.append("Raw DP Radar: " + String.format("%.3f", value)
                        + " in");
            } else {
                buffer.append("Raw DP Radar: missing");
            }

            buffer.append(newLine);

            // code ported directly from OB9.X
            // create_ss_interface_rfcwide.c line 1326
            /* value = misbinRadarRsc.getData()[localX][localY]; */
            value = 0;
            if (value >= 0) {
                buffer.append("Misbin: " + String.format("%.3f", value));
            } else {
                buffer.append("Misbin: missing");
            }
            buffer.append(newLine);

            // code ported directly from OB9.X
            // create_ss_interface_rfcwide.c line 1326
            /* value = climoRadarRsc.getData()[localX][localY]; */
            value = 0;
            if (value >= 0) {
                buffer.append("Rad Clim: " + String.format("%.3f", value));
            } else {
                buffer.append("Rad Clim: missing");
            }

            showPopup(buffer.toString());
        } catch (Exception ex) {
            statusHandler.handle(Priority.PROBLEM,
                    "Error getting showing popup dialog", ex);
        }

    }

    /**
     * Show the popup dialog.
     *
     * @param text
     *            The text to display
     */
    private void showPopup(String text) {
        Shell parent = getShell();
        popupShell = new Shell(parent, SWT.DIALOG_TRIM);
        popupShell.setText("Single Radar Site");

        // Create the main layout for the shell.
        GridLayout mainLayout = new GridLayout(1, true);
        mainLayout.marginHeight = 1;
        mainLayout.marginWidth = 1;
        mainLayout.horizontalSpacing = 1;
        mainLayout.verticalSpacing = 1;
        popupShell.setLayout(mainLayout);

        Composite comp = new Composite(popupShell, SWT.NONE);
        GridLayout gl = new GridLayout(1, true);
        gl.horizontalSpacing = 1;
        gl.verticalSpacing = 1;
        gl.marginHeight = 1;
        gl.marginWidth = 1;
        comp.setLayout(gl);
        GridData gd = new GridData(225, SWT.DEFAULT);
        comp.setLayoutData(gd);

        Label label = new Label(comp, SWT.NONE);
        label.setText(text);

        popupShell.pack();

        popupShell.open();
        popupOpen = true;
    }

    /**
     * Close the popup.
     */
    private void closePopup() {
        popupOpen = false;
        popupShell.dispose();
    }

    /**
     * Get the bias value used.
     *
     * @param radId
     *            The Radar Id
     */
    private String getBiasValue(String radId) {
        MPERadarData radarData = MPEDataManager.getInstance()
                .readDPRadarData(
                        MPEDisplayManager.getCurrent().getCurrentEditDate())
                .get(radId);
        return String.format("%-1.2f", radarData.getRwBiasValUsed());
    }

    public InputManager getMouseManager() {
        return paneManager.getMouseManager();
    }

    @Override
    public void registerMouseHandler(IInputHandler handler,
            InputPriority priority) {
        paneManager.registerMouseHandler(handler, priority);
    }

    @Override
    public void registerMouseHandler(IInputHandler handler) {
        paneManager.registerMouseHandler(handler);
    }

    @Override
    public void unregisterMouseHandler(IInputHandler handler) {
        paneManager.unregisterMouseHandler(handler);
    }

    @Override
    public LoopProperties getLoopProperties() {
        return loopProps;
    }

    @Override
    public void setLoopProperties(LoopProperties loopProperties) {
        loopProps = loopProperties;
    }

    @Override
    public IDisplayPane addPane(IRenderableDisplay renderableDisplay) {
        return paneManager.addPane(renderableDisplay);
    }

    @Override
    public Coordinate translateClick(double x, double y) {
        IDisplayPane pane = getActiveDisplayPane();
        // Convert the screen coordinates to grid space
        double[] world = pane.screenToGrid(x, y, 0);
        IExtent extent = pane.getRenderableDisplay().getExtent();
        // Verify grid space is within the extent, otherwiser return null
        if (world == null || !extent.contains(world)) {
            return null;
        }
        // use descriptor to convert pixel world to CRS world space
        world = pane.getDescriptor().pixelToWorld(world);
        // Check for null
        if (world == null) {
            return null;
        }
        return new Coordinate(world[0], world[1], world[2]);
    }

    @Override
    public double[] translateInverseClick(Coordinate c) {
        if (c == null) {
            return null;
        }
        IDisplayPane pane = getActiveDisplayPane();
        double[] grid = pane.getDescriptor()
                .worldToPixel(new double[] { c.x, c.y, c.z });
        if (grid == null) {
            return null;
        }
        return pane.gridToScreen(grid);
    }

    @Override
    public IDisplayPane[] getDisplayPanes() {
        return paneManager.getDisplayPanes();
    }

    @Override
    public void refresh() {
        paneManager.refresh();
    }

    @Override
    public IDisplayPane getActiveDisplayPane() {
        return paneManager.getActiveDisplayPane();
    }

    @Override
    public int getNumberofPanes() {
        return paneManager.getNumberofPanes();
    }

    @Override
    public void setSelectedPane(String action, IDisplayPane pane) {
        paneManager.setSelectedPane(action, pane);
    }

    @Override
    public IDisplayPane getSelectedPane(String action) {
        return paneManager.getSelectedPane(action);
    }

    @Override
    public IDisplayPane[] getSelectedPanes(String action) {
        return paneManager.getSelectedPanes(action);
    }

    @Override
    public boolean isSelectedPane(String action, IDisplayPane pane) {
        return paneManager.isSelectedPane(action, pane);
    }

    @Override
    public void addSelectedPaneChangedListener(
            ISelectedPanesChangedListener listener) {
        paneManager.addSelectedPaneChangedListener(listener);
    }

    @Override
    public void removeSelectedPaneChangedListener(
            ISelectedPanesChangedListener listener) {
        paneManager.removeSelectedPaneChangedListener(listener);
    }

    @Override
    public void removePane(IDisplayPane pane) {
        paneManager.removePane(pane);
    }

    @Override
    public void hidePane(IDisplayPane pane) {
        paneManager.hidePane(pane);
    }

    @Override
    public void showPane(IDisplayPane pane) {
        paneManager.showPane(pane);
    }

    @Override
    public int displayedPaneCount() {
        return paneManager.displayedPaneCount();
    }

    @Override
    public void clear() {
        paneManager.clear();
    }

    @Override
    public void addRenderableDisplayChangedListener(
            IRenderableDisplayChangedListener displayChangedListener) {

    }

    @Override
    public void removeRenderableDisplayChangedListener(
            IRenderableDisplayChangedListener displayChangedListener) {

    }

    @Override
    public void notifyRenderableDisplayChangedListeners(IDisplayPane pane,
            IRenderableDisplay display, DisplayChangeType type) {

    }

    @Override
    public List<IPane> getPanes() {
        return paneManager.getPanes();
    }

    @Override
    public IPane getActivePane() {
        return paneManager.getActivePane();
    }

    @Override
    public List<IDisplayPane> getCanvasesCompatibleWithActive() {
        return paneManager.getCanvasesCompatibleWithActive();
    }

    @Override
    public IDisplayPane[] getCanvases(CanvasType type) {
        return paneManager.getCanvases(type);
    }

    @Override
    public boolean isHorizontalLayout() {
        return paneManager.isHorizontalLayout();
    }

    @Override
    public void setHorizontalLayout(boolean horizontalLayout) {
        paneManager.setHorizontalLayout(horizontalLayout);
    }
}
