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

package com.raytheon.viz.aviation;

import java.io.File;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Map;
import java.util.concurrent.atomic.AtomicInteger;

import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.jobs.Job;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.graphics.Font;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Layout;
import org.eclipse.swt.widgets.Shell;

import com.raytheon.uf.common.aviation.avnconfig.ITafSiteConfig;
import com.raytheon.uf.common.aviation.avnconfig.TafSiteConfigFactory;
import com.raytheon.uf.common.localization.IPathManager;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationLevel;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationType;
import com.raytheon.uf.common.localization.PathManagerFactory;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.viz.core.localization.LocalizationManager;
import com.raytheon.uf.viz.datacube.DataCubeContainer;
import com.raytheon.viz.aviation.climatology.ClimateMenuDlg;
import com.raytheon.viz.aviation.observer.TafMonitorDlg;
import com.raytheon.viz.aviation.resource.ResourceConfigMgr;
import com.raytheon.viz.aviation.utility.IBackupRestart;
import com.raytheon.viz.avncommon.AvnMessageMgr.StatusMessageType;
import com.raytheon.viz.ui.dialogs.CaveSWTDialog;
import com.raytheon.viz.ui.dialogs.ICloseCallback;

/**
 * The Aviation Dialog class that displays the start up menu for AvnFPS.
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date          Ticket#  Engineer  Description
 * ------------- -------- --------- --------------------------------------------
 * Jan 21, 2008  817      grichard  Initial creation.
 * Apr 09, 2008  934      grichard  Added modeless dialogs.
 * Aug 11, 2008  1314     grichard  Used PathManager for pathnames.
 * Sep 12, 2008  1444     grichard  Accommodate separate message logs.
 * May 11, 2009  1982     grichard  Added backup/restart monitor feature.
 * Jul 09, 2010  5078     rferrel   Use AvnConfigFileUtil to find configuration
 *                                  files.
 * Aug 01, 2010  4345     rferrel   Monitor shows default product sites.
 * Oct 06, 2010  6009     rferrel   Changes to use product.
 * Oct 27, 2010  7383     rferrel   Restart now reloads setup information from
 *                                  configuration.
 * Dec 06, 2010  5342     rferrel   Change getForecasterConfig to only return
 *                                  login user name configuration.
 * Dec 14, 2010  5782     rferrel   Restart reloads all resource information.
 * Mar 14, 2011  8588     rferrel   Allow monitoring of multiple products.
 * Mar 31, 2011  8774     rferrel   The shell.disposed() handled correctly for
 *                                  both stand alone and from Cave.
 * Aug 31, 2011  10837    rferrel   Added checks to see if the avnImage file
 *                                  exists.
 * Oct 02, 2012  1229     rferrel   Made dialog non-blocking.
 * Oct 09, 2012  1229     rferrel   Changes for non-blocking TafMonitorDlg.
 * Apr 10, 2013  1735     rferrel   Changes for taf monitor speed up.
 * Aug 09, 2013  2033     mschenke  Switched File.separator to
 *                                  IPathManager.SEPARATOR
 * Aug 12, 2013  2256     lvenable  Removed unnecessary font code and other code
 *                                  clean up.
 * May 06, 2014  3091     rferrel   Use OUP authorization to bring up send
 *                                  dialog.
 * Sep 15, 2015  4880     njensen   Removed ForecastModel reference
 * Jan 26, 2016  5054     randerso  Change top level dialog to be parented to
 *                                  the display
 * Jan 24, 2018  6692     tgurney   Do not prompt to close dialog when
 *                                  restarting
 * Jul 25, 2018  6748     randerso  Code cleanup.
 * Nov 07, 2018  6692     tgurney   Preserve the order of stations as written in config
 * May 15, 2019 20693 mgamazaychikov ITafSiteConfig, TafSiteConfigFactory refactor
 *
 * </pre>
 *
 */
public class AviationDialog extends CaveSWTDialog implements IBackupRestart {
    private static final IUFStatusHandler statusHandler = UFStatus
            .getHandler(AviationDialog.class);

    private java.util.List<Font> fontList;

    private Image avnImage;

    private Image eclipseImage;

    private Map<String, java.util.List<String>> stationMap;

    /**
     * Products whose sites will display in the monitor.
     */
    private java.util.List<String> productDisplayList;

    private TafMonitorDlg tafMonitorDlg;

    private ClimateMenuDlg climateMenuDlg;

    /**
     * Number of dialogs currently open,
     */
    private final AtomicInteger dlgCount = new AtomicInteger(0);

    public static String getForecaster() {
        return LocalizationManager.getInstance().getCurrentUser();
    }

    public AviationDialog(Display display) {
        super(display, SWT.DIALOG_TRIM, CAVE.PERSPECTIVE_INDEPENDENT
                | CAVE.INDEPENDENT_SHELL | CAVE.DO_NOT_BLOCK);
        setText("AvnFPS Menu");

        BackupRestart.setBackupRestartUtility(this);
    }

    @Override
    protected Layout constructShellLayout() {
        return new GridLayout(1, false);
    }

    @Override
    protected void disposed() {
        for (Font f : fontList) {
            f.dispose();
        }
        fontList.clear();
        fontList = null;

        if (avnImage != null) {
            avnImage.dispose();
            avnImage = null;
        }

        if (eclipseImage != null) {
            eclipseImage.dispose();
            eclipseImage = null;
        }
    }

    @Override
    protected void initializeComponents(Shell shell) {
        setReturnValue(false);

        fontList = new ArrayList<>();

        String path = loadGifPath("avn");
        if (path != null) {
            avnImage = new Image(getDisplay(), path);
        }

        path = loadGifPath("eclipse");
        if (path != null) {
            eclipseImage = new Image(getDisplay(), path);
        }

        loadTafSiteConfig();

        // Initialize all of the controls and layouts
        initializeComponents();
    }

    private void loadTafSiteConfig() {
        try {
            ITafSiteConfig config = TafSiteConfigFactory.getInstance();
            stationMap = config.getAllProducts();
            if (productDisplayList == null) {
                productDisplayList = new ArrayList<>();
            }
            if (productDisplayList.isEmpty()
                    && (config.getDefaultProduct() != null)) {
                productDisplayList.add(config.getDefaultProduct());
            }
        } catch (Exception e) {
            statusHandler.handle(Priority.PROBLEM, "Error loading site config",
                    e);
        }
    }

    private void initializeComponents() {
        createLabel();
        createForecasterLabel();
        createComposite();
        initAcarsSounding();
    }

    /**
     * To speed up the display of the monitor this method starts up the Data
     * Cube for the acarssounding inventory which can take over a second for the
     * initial retrieval. This may cause blocking issues that slows the display
     * of the monitor.
     */
    private void initAcarsSounding() {
        Job job = new Job("AviationDialog") {

            @Override
            protected IStatus run(IProgressMonitor monitor) {
                DataCubeContainer.getInventory("acarssounding");
                return Status.OK_STATUS;
            }
        };
        job.schedule();
    }

    private void createLabel() {
        createAvnFPSLabel();
    }

    private void createComposite() {
        GridData gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        Composite bottomComposite = new Composite(shell, SWT.NONE);
        GridLayout layoutBC = new GridLayout(2, false);
        bottomComposite.setLayout(layoutBC);
        bottomComposite.setLayoutData(gd);

        Composite leftComposite = new Composite(bottomComposite, SWT.NONE);
        GridLayout layoutLC = new GridLayout(1, false);
        leftComposite.setLayout(layoutLC);

        Composite rightComposite = new Composite(bottomComposite, SWT.NONE);
        GridLayout layoutRC = new GridLayout(1, false);
        rightComposite.setLayout(layoutRC);

        Label eclipseLabel = new Label(leftComposite, SWT.CENTER);
        if (eclipseImage != null) {
            eclipseLabel.setImage(eclipseImage);
        } else {
            Font font = new Font(getDisplay(), "sans-serif", 9, SWT.NORMAL);
            fontList.add(font);
            eclipseLabel.setText("No Image\nfound");
            eclipseLabel.setBackground(getDisplay()
                    .getSystemColor(SWT.COLOR_TITLE_BACKGROUND_GRADIENT));
            eclipseLabel.setFont(font);
        }

        Button tafsBtn = new Button(rightComposite, SWT.PUSH);
        String tafsBtnTitle = "TAFs";
        GridData data = new GridData(SWT.FILL, SWT.FILL, true, true);
        data.widthHint = 170;
        tafsBtn.setLayoutData(data);
        tafsBtn.setText(tafsBtnTitle);
        tafsBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                shell.setVisible(false);
                if ((tafMonitorDlg == null)
                        || tafMonitorDlg.getShell().isDisposed()) {
                    ResourceConfigMgr configMgr = ResourceConfigMgr
                            .getInstance();
                    configMgr.reloadResourceData();
                    dlgCount.incrementAndGet();
                    loadTafSiteConfig();
                    displayTafMonitorDialog();
                } else {
                    tafMonitorDlg.bringToTop();
                }
            }
        });

        Button climBtn = new Button(rightComposite, SWT.PUSH);
        String climBtnTitle = "Climate";
        data = new GridData(SWT.FILL, SWT.FILL, true, true);
        data.widthHint = 100;
        climBtn.setLayoutData(data);
        climBtn.setText(climBtnTitle);
        climBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                shell.setVisible(false);
                if ((climateMenuDlg == null)
                        || (climateMenuDlg.getShell() == null)
                        || climateMenuDlg.isDisposed()) {

                    StatusMessageType[] msgTypes = new StatusMessageType[4];
                    msgTypes[0] = StatusMessageType.Metar;
                    msgTypes[1] = StatusMessageType.WindRose;
                    msgTypes[2] = StatusMessageType.CigVis;
                    msgTypes[3] = StatusMessageType.CigVisTrend;

                    dlgCount.incrementAndGet();
                    climateMenuDlg = new ClimateMenuDlg(shell, msgTypes, null);
                    climateMenuDlg.addCloseCallback(new ICloseCallback() {

                        @Override
                        public void dialogClosed(Object returnValue) {
                            climateMenuDlg = null;
                            if (dlgCount.decrementAndGet() == 0) {
                                shell.dispose();
                            }
                        }
                    });
                    climateMenuDlg.open();
                } else {
                    climateMenuDlg.bringToTop();
                }
            }
        });

        Button cancelBtn = new Button(rightComposite, SWT.PUSH);
        String cancelBtnTitle = "Cancel";
        data = new GridData(SWT.FILL, SWT.FILL, true, true);
        data.widthHint = 100;
        cancelBtn.setLayoutData(data);
        cancelBtn.setText(cancelBtnTitle);
        cancelBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                close();
            }
        });
    }

    /**
     * Get fully qualified path name for image
     *
     * @param imageName
     *            - base image name
     * @return path - fully qualified name or null if file does not exist
     */
    private String loadGifPath(String imageName) {
        IPathManager pm = PathManagerFactory.getPathManager();
        String path = null;
        File file = pm.getFile(
                pm.getContext(LocalizationType.CAVE_STATIC,
                        LocalizationLevel.BASE),
                "aviation" + File.separatorChar + "avnwatch"
                        + File.separatorChar + imageName + ".gif");
        if (file.canRead()) {
            path = file.getAbsolutePath();
        }
        return path;
    }

    private void createAvnFPSLabel() {
        GridData gd = new GridData(275, 250);
        Label avnLabel = new Label(shell, SWT.CENTER);
        if (avnImage != null) {
            avnLabel.setImage(avnImage);
        } else {
            Font font = new Font(getDisplay(), "sans-serif", 14, SWT.NORMAL);
            fontList.add(font);
            avnLabel.setText("No Image found");
            avnLabel.setBackground(getDisplay()
                    .getSystemColor(SWT.COLOR_TITLE_BACKGROUND_GRADIENT));
            avnLabel.setFont(font);
        }
        avnLabel.setLayoutData(gd);
    }

    private void createForecasterLabel() {
        GridData gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        Label forecasterLabel = new Label(shell, SWT.CENTER);
        forecasterLabel.setText(getForecaster());
        forecasterLabel.setLayoutData(gd);
    }

    @Override
    public void restartTafMonitor() {
        // This prevents tafMonitorDlg from closing this shell when closing the
        // TaMonitorDlg prior to the restart.
        dlgCount.incrementAndGet();
        if (!tafMonitorDlg.closeDisplay(false)) {
            dlgCount.decrementAndGet();
            return;
        }

        // Reload the Station Configuration Information.
        ResourceConfigMgr configMgr = ResourceConfigMgr.getInstance();
        TafSiteConfigFactory.clearInstance();
        configMgr.reloadResourceData();
        loadTafSiteConfig();
        displayTafMonitorDialog();
    }

    @Override
    public void backupTafMonitor(java.util.List<String> productDisplayList,
            Map<String, java.util.List<String>> stationMap) {
        this.stationMap = stationMap;
        this.productDisplayList = productDisplayList;
        boolean emptyStationList = true;
        for (String product : productDisplayList) {
            if (!stationMap.get(product).isEmpty()) {
                emptyStationList = false;
                break;
            }
        }

        if (!emptyStationList) {
            // This prevents tafMonitorDlg from closing this shell when closing
            // the TaMonitorDlg prior to the restart.
            dlgCount.incrementAndGet();
            if (!tafMonitorDlg.closeDisplay(false)) {
                dlgCount.decrementAndGet();
                return;
            }

            displayTafMonitorDialog();
        } else {
            restartTafMonitor();
        }
    }

    /**
     * Display the TAF monitor dialog.
     *
     */
    private void displayTafMonitorDialog() {
        ResourceConfigMgr configMgr = ResourceConfigMgr.getInstance();
        if (!configMgr.isResourceLoaded()) {
            configMgr.reloadResourceData();
        }

        LinkedHashSet<String> stations = new LinkedHashSet<>();
        for (String product : productDisplayList) {
            for (String site : stationMap.get(product)) {
                stations.add(site);
            }
        }

        if (stations.isEmpty()) {
            if (productDisplayList.isEmpty()) {
                statusHandler.handle(Priority.PROBLEM,
                        "No stations configured");
            }
            for (String product : productDisplayList) {
                statusHandler.handle(Priority.PROBLEM,
                        "Error no stations configured for " + product);
            }
        } else {
            if ((tafMonitorDlg == null) || (tafMonitorDlg.getShell() == null)
                    || tafMonitorDlg.isDisposed()) {
                List<String> stationList = Arrays
                        .asList(stations.toArray(new String[0]));
                tafMonitorDlg = new TafMonitorDlg(shell, stationList,
                        productDisplayList);
                tafMonitorDlg.addCloseCallback(new ICloseCallback() {

                    @Override
                    public void dialogClosed(Object returnValue) {
                        tafMonitorDlg = null;
                        if ((dlgCount.decrementAndGet() == 0)
                                && !productDisplayList.isEmpty()) {
                            shell.dispose();
                        }
                    }
                });
                tafMonitorDlg.open();
            } else {
                tafMonitorDlg.bringToTop();
            }
        }
    }
}
