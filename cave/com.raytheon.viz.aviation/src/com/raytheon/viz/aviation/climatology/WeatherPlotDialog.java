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
package com.raytheon.viz.aviation.climatology;

import java.io.File;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Calendar;
import java.util.List;
import java.util.TimeZone;

import javax.xml.bind.JAXB;

import org.eclipse.swt.SWT;
import org.eclipse.swt.custom.ScrolledComposite;
import org.eclipse.swt.events.ControlAdapter;
import org.eclipse.swt.events.ControlEvent;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.graphics.Color;
import org.eclipse.swt.graphics.Cursor;
import org.eclipse.swt.graphics.Font;
import org.eclipse.swt.graphics.GC;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.graphics.ImageData;
import org.eclipse.swt.graphics.ImageLoader;
import org.eclipse.swt.graphics.Point;
import org.eclipse.swt.graphics.RGB;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Combo;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.FileDialog;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Layout;
import org.eclipse.swt.widgets.MessageBox;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Spinner;

import com.raytheon.uf.common.localization.IPathManager;
import com.raytheon.uf.common.localization.LocalizationFile;
import com.raytheon.uf.common.localization.PathManagerFactory;
import com.raytheon.uf.common.time.SimulatedTime;
import com.raytheon.uf.viz.core.RGBColors;
import com.raytheon.uf.viz.core.VizApp;
import com.raytheon.viz.aviation.resource.ResourceConfigMgr;
import com.raytheon.viz.aviation.utility.TimeSelectorDialog;
import com.raytheon.viz.aviation.xml.PlotViewerCfg;
import com.raytheon.viz.aviation.xml.WxPlotCfg;
import com.raytheon.viz.avncommon.AvnMessageMgr.StatusMessageType;
import com.raytheon.viz.avnconfig.HelpUsageDlg;
import com.raytheon.viz.avnconfig.MessageStatusComp;
import com.raytheon.viz.ui.dialogs.CaveSWTDialog;
import com.raytheon.viz.ui.dialogs.ICloseCallback;

/**
 * WeatherPlotDialog class displays the Weather Plot dialog for AvnFPS.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 28 FEB 2008  938        lvenable    Initial creation.
 * 4/10/2008    934        grichard    Populated site lists with icaos.
 * 7/29/2008    1342       grichard    Get calendar instance in UTC time zone.
 * 9/12/2008    1444       grichard    Accommodate separate message logs.
 * 11/18/2010   6701       rferrel     Now uses the same wxPlotCfg
 *                                     as WeatherPlotDataManager.
 * 04/28/2011   8065       rferrel     Use cache data.
 * 10/02/2012   1229       rferrel     Made dialog non-blocking.
 * 10/10/2012   1229       rferrel     Changes for non-blocking TimeSelectorDlg.
 * 10/15/2012   1229       rferrel     Changes for non-blocking HelpUsageDlg.
 * 11/26/2012   1298       rferrel     Non-blocking dialog code cleanup.
 * 
 * 
 * </pre>
 * 
 * @author lvenable
 * @version 1.0
 * 
 */
public class WeatherPlotDialog extends CaveSWTDialog {

    /**
     * Label font.
     */
    private Font font;

    /**
     * Site ID combo control.
     */
    private Combo siteIdCbo;

    /**
     * Zoom spinner.
     */
    private Spinner zoomSpinner;

    /**
     * Display button.
     */
    private Button displayBtn;

    /**
     * Times button.
     */
    private Button timesBtn;

    /**
     * Print button.
     */
    private Button printBtn;

    /**
     * Close button.
     */
    private Button closeBtn;

    /**
     * Help button.
     */
    private Button helpBtn;

    /**
     * Array of data sources check boxes.
     */
    private List<Button> dataSourceCheckBoxes;

    /**
     * Ceiling canvas composite.
     */
    private WeatherCigCanvasComp cigCanvasComp;

    /**
     * Visibility canvas composite.
     */
    private WeatherVisCanvasComp visCanvasComp;

    /**
     * Wind canvas composite.
     */
    private WeatherWindCanvasComp windCanvasComp;

    /**
     * Current hour.
     */
    private long currentTime = 0;

    /**
     * Count used to determine busy state for the dialog.
     */
    private int busyCnt = 0;

    /**
     * Formatted UTC.
     */
    SimpleDateFormat sdfUTC = new SimpleDateFormat("dd/MMM/yyyy HH:mm:ss z");

    /**
     * Site time label.
     */
    private Label siteTimeLbl;

    /**
     * Scrolled composite containing plot data.
     */
    private ScrolledComposite scrolledComp;

    /**
     * Graph composite containing the plot graph.
     */
    private Composite graphComp;

    private Cursor waitCursor = null;

    /**
     * Status message type.
     */
    private StatusMessageType msgType;

    /**
     * Main composite that will contain all of the controls.
     */
    private Composite mainComp;

    private WxPlotCfg wxPlotCfg;

    /**
     * Array of image filter names.
     */
    private final String[] FILTER_NAMES = { "Bitmap (*.bmp)", "JPEG (*.jpg)",
            "PNG (*.png)" };

    /**
     * Array of image filters.
     */
    private final String[] FILTER_EXTS = { "*.bmp", "*.jpg", "*.png" };

    /**
     * The ICAOs to use to populate the station list.
     */
    private List<String> icaos;

    private TimeSelectorDialog timeDlg;

    private HelpUsageDlg usageDlg;

    private WeatherPlotDataManager dataMgr;

    /**
     * Constructor.
     * 
     * @param parent
     *            Parent Shell.
     * @param disposeOnExit
     *            Flag to indicate whether to dispose dialog on exit.
     */
    public WeatherPlotDialog(Shell parent, StatusMessageType msgType,
            List<String> stationList) {
        super(parent, SWT.DIALOG_TRIM | SWT.MODELESS | SWT.RESIZE,
                CAVE.PERSPECTIVE_INDEPENDENT | CAVE.MODE_INDEPENDENT
                        | CAVE.DO_NOT_BLOCK);
        setText("AvnFPS Weather Plot");

        this.msgType = msgType;
        this.icaos = stationList;
        this.dataMgr = WeatherPlotDataManager.getInstance();
    }

    @Override
    protected Layout constructShellLayout() {
        GridLayout mainLayout = new GridLayout(1, false);
        mainLayout.marginHeight = 0;
        mainLayout.marginWidth = 0;
        return mainLayout;
    }

    @Override
    protected Object constructShellLayoutData() {
        return new GridData(SWT.FILL, SWT.FILL, true, true);
    }

    @Override
    protected void disposed() {
        busyCnt = 0;
        font.dispose();
        waitCursor.dispose();
    }

    @Override
    protected void initializeComponents(Shell shell) {
        setReturnValue(false);

        GridData gd = new GridData(SWT.FILL, SWT.FILL, true, true);
        GridLayout mainLayout = new GridLayout(1, false);
        mainComp = new Composite(shell, SWT.NONE);
        mainComp.setLayout(mainLayout);
        mainComp.setLayoutData(gd);

        font = new Font(shell.getDisplay(), "Monospace", 10, SWT.NORMAL);
        waitCursor = new Cursor(shell.getDisplay(), SWT.CURSOR_WAIT);

        // Initialize the data and all of the controls and layouts
        initData();
        initializeComponents();

        populateStations(icaos);
    }

    @Override
    protected void preOpened() {
        shell.setMinimumSize(600, 500);
        populateData();
    }

    private void initData() {
        dataMgr.getNewConfig();
        wxPlotCfg = dataMgr.getWxPlotCfg();
    }

    /**
     * Populate the stations.
     */
    private void populateStations(List<String> stationList) {
        siteIdCbo.removeAll();
        for (String s : stationList) {
            siteIdCbo.add(s);
        }
        siteIdCbo.select(0);
    }

    /**
     * Show the dialog.
     */
    public void showDialog() {

        if (shell.isVisible() == false) {
            shell.setVisible(true);
        }

        shell.setActive();
    }

    /**
     * Initialize the components on the display.
     */
    private void initializeComponents() {
        sdfUTC.setTimeZone(TimeZone.getTimeZone("UTC"));

        ResourceConfigMgr configMgr = ResourceConfigMgr.getInstance();

        configMgr.setDefaultColors(mainComp);

        createTopControls(configMgr);
        createDataSourcesControls(configMgr);
        createTimeLabel(configMgr);
        createScrolledComposite(configMgr);
        createBottomMessageControls();
    }

    /**
     * Create the controls at the top of the display.
     */
    private void createTopControls(ResourceConfigMgr configMgr) {
        GridData gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        Composite controlsComp = new Composite(mainComp, SWT.NONE);
        controlsComp.setLayout(new GridLayout(9, false));
        controlsComp.setLayoutData(gd);
        configMgr.setDefaultColors(controlsComp);

        // Site ID
        Label siteIdLbl = new Label(controlsComp, SWT.NONE);
        siteIdLbl.setText("Site ID: ");
        configMgr.setDefaultFontAndColors(siteIdLbl);

        gd = new GridData(SWT.DEFAULT, SWT.DEFAULT, true, false);
        gd.widthHint = 100;
        siteIdCbo = new Combo(controlsComp, SWT.DROP_DOWN | SWT.READ_ONLY);
        configMgr.setDefaultFont(siteIdCbo);
        siteIdCbo.add("KWWW", 0);
        Point p = siteIdCbo.computeSize(SWT.DEFAULT, SWT.DEFAULT);
        if (gd.widthHint < p.x) {
            gd.widthHint = p.x;
        }
        siteIdCbo.setLayoutData(gd);
        siteIdCbo.remove(0);
        siteIdCbo.select(0);
        siteIdCbo.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                refreshSiteData();
            }
        });

        // Zoom
        Label zoomLbl = new Label(controlsComp, SWT.NONE);
        zoomLbl.setText("Zoom:");
        configMgr.setDefaultFontAndColors(zoomLbl);

        gd = new GridData(50, SWT.DEFAULT);
        zoomSpinner = new Spinner(controlsComp, SWT.BORDER);
        zoomSpinner.setDigits(0);
        zoomSpinner.setIncrement(1);
        zoomSpinner.setPageIncrement(1);
        zoomSpinner.setSelection(1);
        zoomSpinner.setMinimum(1);
        zoomSpinner.setMaximum(3);
        p = zoomSpinner.computeSize(SWT.DEFAULT, SWT.DEFAULT);
        if (gd.widthHint < p.x) {
            gd.widthHint = p.x;
        }
        zoomSpinner.setLayoutData(gd);
        zoomSpinner.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                int zoomLevel = zoomSpinner.getSelection();
                cigCanvasComp.updateAndRedraw(zoomLevel);
                visCanvasComp.updateAndRedraw(zoomLevel);
                windCanvasComp.updateAndRedraw(zoomLevel);
                scrolledComp.setMinSize(graphComp.computeSize(SWT.DEFAULT,
                        SWT.DEFAULT));
                graphComp.layout();
            }
        });
        configMgr.setDefaultFont(zoomSpinner);

        int buttonWidth = 70;

        // Display button
        gd = new GridData(buttonWidth, SWT.DEFAULT);
        displayBtn = new Button(controlsComp, SWT.PUSH);
        configMgr.setDefaultFontAndColors(displayBtn, "Display", gd);
        displayBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                refreshSiteData();
            }
        });

        // Times button
        gd = new GridData(buttonWidth, SWT.DEFAULT);
        timesBtn = new Button(controlsComp, SWT.PUSH);
        configMgr.setDefaultFontAndColors(timesBtn, "Times", gd);
        timesBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                if (timeDlg == null) {
                    timeDlg = new TimeSelectorDialog(shell, wxPlotCfg);
                    timeDlg.setCloseCallback(new ICloseCallback() {

                        @Override
                        public void dialogClosed(Object returnValue) {
                            if (returnValue instanceof Boolean) {
                                boolean value = (Boolean) returnValue;
                                if (value) {
                                    displayData();
                                }
                            }
                            timeDlg = null;
                        }
                    });
                }
                timeDlg.open();
            }
        });

        // Print button
        gd = new GridData(buttonWidth, SWT.DEFAULT);
        printBtn = new Button(controlsComp, SWT.PUSH);
        configMgr.setDefaultFontAndColors(printBtn, "Print", gd);
        printBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                saveImage();
            }
        });

        // Close button
        gd = new GridData(buttonWidth, SWT.DEFAULT);
        closeBtn = new Button(controlsComp, SWT.PUSH);
        configMgr.setDefaultFontAndColors(closeBtn, "Close", gd);
        closeBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                shell.dispose();
            }
        });

        // Help button
        gd = new GridData(buttonWidth, SWT.DEFAULT);
        helpBtn = new Button(controlsComp, SWT.PUSH);
        configMgr.setDefaultFontAndColors(helpBtn, "Help", gd);
        helpBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                if (usageDlg == null) {
                    String description = "Help";
                    String helpText = "This dialog is used to display TAFs, METARs and guidance forecasts.\n\nMenus:\n    Site ID     - pulldown menu displaying list of all TAF sites.\n                  Selection of a site from the list redraws the window.\n    zoom        - zoom factor (time scale). \n\nButtons:\n    Display     - Redraws the window.\n    Times       - Displays forecast time selection window\n    Print       - Dumps an image of the window to a command specified in\n                  the configration file etc/wxplot.cfg.\n    Close       - Closes this dialog.\n    Help        - Displays this help.\n\nData Sources    - selection of available data sources. ";
                    usageDlg = new HelpUsageDlg(shell, description, helpText);
                }
                usageDlg.open();
            }
        });
    }

    private void saveImage() {
        FileDialog dlg = new FileDialog(shell, SWT.SAVE);
        dlg.setFilterNames(FILTER_NAMES);
        dlg.setFilterExtensions(FILTER_EXTS);
        String filename = dlg.open();
        if (filename == null) {
            return;
        }

        int style = SWT.IMAGE_PNG;

        if (filename.endsWith(".jpg") == true) {
            style = SWT.IMAGE_JPEG;
        } else if (filename.endsWith(".png") == true) {
            style = SWT.IMAGE_PNG;
        } else if (filename.endsWith(".bmp") == true) {
            style = SWT.IMAGE_BMP;
        } else {
            MessageBox mb = new MessageBox(shell, SWT.ICON_INFORMATION | SWT.OK);
            mb.setText("File Selection");
            mb.setMessage("No extension was provided for the image name.\n"
                    + "Defalting to PNG format.");
            mb.open();

            filename += ".png";
        }

        Image cigImage = cigCanvasComp.getImage();
        Image visImage = visCanvasComp.getImage();
        Image windImage = windCanvasComp.getImage();

        Font font = new Font(getDisplay(), "Monospace", 12, SWT.NORMAL);
        Image masterImage = new Image(getDisplay(), cigCanvasComp.getWidth(),
                cigCanvasComp.getHeight() * 3 + 60);
        GC gc = new GC(masterImage);
        int fontHeight = gc.getFontMetrics().getHeight();
        gc.setFont(font);
        gc.drawText(siteTimeLbl.getText(), 20, (20 - fontHeight) / 2 + 1);
        gc.drawText("Ceiling", 524, (20 - fontHeight) / 2 + 1);
        gc.drawImage(cigImage, 0, 20);
        gc.drawText("Visibility", 513, (20 - fontHeight) / 2 + 1
                + cigCanvasComp.getHeight() + 20);
        gc.drawImage(visImage, 0, cigCanvasComp.getHeight() + 40);
        gc.drawText("Wind", 537,
                (20 - fontHeight) / 2 + 1 + cigCanvasComp.getHeight() * 2 + 40);
        gc.drawImage(windImage, 0, cigCanvasComp.getHeight() * 2 + 60);

        gc.dispose();
        font.dispose();
        ImageLoader loader = new ImageLoader();
        loader.data = new ImageData[] { masterImage.getImageData() };
        loader.save(filename, style);
    }

    /**
     * Create the Data Sources check boxes.
     */
    private void createDataSourcesControls(ResourceConfigMgr configMgr) {
        dataSourceCheckBoxes = new ArrayList<Button>();

        List<PlotViewerCfg> plotCfgArray = wxPlotCfg.getPlotViewers();

        int numPlotViewers = plotCfgArray.size();

        // Calculate the number of columns needed by adding 1 to the number of
        // plot viewers.
        // The 1 is for the Data Sources label in front of the check boxes.
        int numCols = numPlotViewers + 1;

        Composite dataSourceComp = new Composite(mainComp, SWT.BORDER);
        GridLayout gl = new GridLayout(numCols, false);
        GridData gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        dataSourceComp.setLayout(gl);
        dataSourceComp.setLayoutData(gd);
        configMgr.setDefaultColors(dataSourceComp);

        Label dataSourceLbl = new Label(dataSourceComp, SWT.NONE);
        dataSourceLbl.setText("Data Sources: ");
        configMgr.setDefaultFontAndColors(dataSourceLbl);

        /*
         * Create the Plot Viewer check boxes.
         */
        for (PlotViewerCfg pltCfg : plotCfgArray) {
            Button dataChk = createPlotCheckButton(dataSourceComp, pltCfg,
                    configMgr);
            dataSourceCheckBoxes.add(dataChk);
        }
    }

    private void toggleDataSource(String dataSource, boolean status) {
        List<PlotViewerCfg> plotCfgArray = wxPlotCfg.getPlotViewers();

        for (PlotViewerCfg viewer : plotCfgArray) {
            if (viewer.getLabelName().equals(dataSource)) {
                viewer.setSelected(status);
                break;
            }
        }

        try {
            wxPlotCfg.setPlotViewers(plotCfgArray);
            String fname = "aviation/config/gui/WxPlotCfg.xml";
            IPathManager pm = PathManagerFactory.getPathManager();
            LocalizationFile lFile = pm.getStaticLocalizationFile(fname);
            File file = lFile.getFile();
            JAXB.marshal(wxPlotCfg, file);
        } catch (RuntimeException e) {
            e.printStackTrace();
        }
    }

    /**
     * Create a plot viewer check box, set the text, color, and selection.
     * 
     * @param parentComp
     *            Parent composite.
     * @param pltCfg
     *            Plot viewer configuration data.
     * @return The create plot check box.
     */
    private Button createPlotCheckButton(Composite parentComp,
            PlotViewerCfg pltCfg, ResourceConfigMgr configMgr) {
        // Create the Check Box
        GridData gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        Button chkBtn = new Button(parentComp, SWT.CHECK | SWT.BORDER);
        configMgr.setDefaultFontAndColors(chkBtn, pltCfg.getLabelName().trim(),
                gd);
        // chkBtn.setText(pltCfg.getLabelName().trim());

        // Set the background color of the check box
        RGB rgb = RGBColors.getRGBColor(pltCfg.getColorName().trim());
        Color c = new Color(parentComp.getDisplay(), rgb);
        chkBtn.setBackground(c);
        c.dispose();

        // Select/Unselect the check box.
        chkBtn.setSelection(pltCfg.getSelected());

        // Set the layout data.
        chkBtn.setLayoutData(gd);

        return chkBtn;
    }

    /**
     * Create the time label.
     */
    private void createTimeLabel(ResourceConfigMgr configMgr) {
        Composite timeLabelComp = new Composite(mainComp, SWT.NONE);
        GridLayout gl = new GridLayout(1, false);
        GridData gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        timeLabelComp.setLayout(gl);
        timeLabelComp.setLayoutData(gd);
        configMgr.setDefaultColors(timeLabelComp);

        gd = new GridData(SWT.CENTER, SWT.DEFAULT, true, false);
        siteTimeLbl = new Label(timeLabelComp, SWT.CENTER);
        siteTimeLbl.setFont(font);
        siteTimeLbl.setLayoutData(gd);
        configMgr.setDefaultFontAndColors(siteTimeLbl);

        updateSiteTimeLabel();
    }

    /**
     * Create the Scrolled Composite that will contain the graphs.
     */
    private void createScrolledComposite(ResourceConfigMgr configMgr) {
        scrolledComp = new ScrolledComposite(mainComp, SWT.V_SCROLL
                | SWT.H_SCROLL | SWT.BORDER);
        GridLayout gl = new GridLayout(1, false);
        gl.verticalSpacing = 1;
        scrolledComp.setLayout(gl);
        GridData gd = new GridData(SWT.FILL, SWT.FILL, true, true);
        scrolledComp.setLayoutData(gd);
        configMgr.setDefaultColors(scrolledComp);

        gl = new GridLayout(1, false);
        gl.verticalSpacing = 1;
        graphComp = new Composite(scrolledComp, SWT.NONE);
        graphComp.setLayout(gl);

        // TODO : Add the weather plot composites here...
        cigCanvasComp = new WeatherCigCanvasComp(graphComp, currentTime,
                wxPlotCfg);
        visCanvasComp = new WeatherVisCanvasComp(graphComp, currentTime,
                wxPlotCfg);
        windCanvasComp = new WeatherWindCanvasComp(graphComp, currentTime,
                wxPlotCfg);

        graphComp.layout();

        scrolledComp.setContent(graphComp);
        scrolledComp.setExpandHorizontal(true);
        scrolledComp.setExpandVertical(true);

        scrolledComp.addControlListener(new ControlAdapter() {
            @Override
            public void controlResized(ControlEvent e) {
                int zoomLevel = zoomSpinner.getSelection();
                cigCanvasComp.updateAndRedraw(zoomLevel);
                visCanvasComp.updateAndRedraw(zoomLevel);
                windCanvasComp.updateAndRedraw(zoomLevel);
                scrolledComp.setMinSize(graphComp.computeSize(SWT.DEFAULT,
                        SWT.DEFAULT));
            }
        });

        scrolledComp.layout();
    }

    /**
     * Create the message status composite.
     */
    private void createBottomMessageControls() {

        ResourceConfigMgr configMgr = ResourceConfigMgr.getInstance();
        new MessageStatusComp(shell, msgType,
                configMgr.getDefaultBackgroundRGB(),
                configMgr.getMsgBarBackground());
    }

    /**
     * Update the time label.
     */
    private void updateSiteTimeLabel() {
        Calendar c = Calendar.getInstance(TimeZone.getTimeZone("GMT"));
        c.setTimeInMillis(System.currentTimeMillis());
        // currentTime = c.get(Calendar.HOUR_OF_DAY);
        currentTime = c.getTimeInMillis();
        siteTimeLbl.setText(siteIdCbo.getText() + "  "
                + sdfUTC.format(SimulatedTime.getSystemTime().getTime())
                + "              ");
    }

    private void populateData() {
        if (isDisposed()) {
            return;
        }
        setCursorBusy(true);
        final String siteId = siteIdCbo.getItem(siteIdCbo.getSelectionIndex());
        Thread thread = new Thread(new Runnable() {
            @Override
            public void run() {
                dataMgr.loadCacheData(siteId);
                if (isDisposed() == false) {
                    VizApp.runAsync(new Runnable() {
                        @Override
                        public void run() {
                            if (dataMgr.loadData(siteId, currentTime)) {
                                updateSiteTimeLabel();
                                displayData();
                            } else {
                                // Something cleared the cache try again.
                                populateData();
                            }
                            setCursorBusy(false);
                        }
                    });
                }
            }
        });
        thread.start();
    }

    private void displayData() {
        if (dataMgr.havePendingCache()) {
            setCursorBusy(true);
            Thread thread = new Thread(new Runnable() {
                @Override
                public void run() {
                    dataMgr.waitForCacheRequests();
                    if (isDisposed() == false) {
                        VizApp.runAsync(new Runnable() {
                            @Override
                            public void run() {
                                setCursorBusy(false);
                                displayData();
                            }
                        });
                    }
                }
            });
            thread.start();
        }
        cigCanvasComp.updateAndRedraw(zoomSpinner.getSelection());
        visCanvasComp.updateAndRedraw(zoomSpinner.getSelection());
        windCanvasComp.updateAndRedraw(zoomSpinner.getSelection());

        scrolledComp
                .setMinSize(graphComp.computeSize(SWT.DEFAULT, SWT.DEFAULT));
    }

    private void refreshSiteData() {
        for (Button btn : dataSourceCheckBoxes) {
            String dataSource = btn.getText();
            boolean status = btn.getSelection();
            toggleDataSource(dataSource, status);
        }

        populateData();
    }

    private synchronized void setCursorBusy(boolean state) {
        if (shell.isDisposed()) {
            busyCnt = 0;
            return;
        }
        if (state) {
            ++busyCnt;
            shell.setCursor(waitCursor);
        } else {
            --busyCnt;
            if (busyCnt == 0) {
                shell.setCursor(null);
            }
        }
    }

}
