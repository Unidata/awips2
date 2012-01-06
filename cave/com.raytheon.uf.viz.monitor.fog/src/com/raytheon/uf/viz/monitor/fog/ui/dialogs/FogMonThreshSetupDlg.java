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
package com.raytheon.uf.viz.monitor.fog.ui.dialogs;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.Iterator;

import org.eclipse.jface.resource.ImageDescriptor;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.graphics.Font;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Dialog;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Menu;
import org.eclipse.swt.widgets.MenuItem;
import org.eclipse.swt.widgets.Scale;
import org.eclipse.swt.widgets.Shell;

import com.raytheon.uf.common.localization.LocalizationFile;
import com.raytheon.uf.viz.monitor.fog.Activator;
import com.raytheon.uf.viz.monitor.fog.FogMonitor;
import com.raytheon.uf.viz.monitor.fog.listeners.IFogResourceListener;
import com.raytheon.uf.viz.monitor.fog.threshold.FogAlgorithmMgr;
import com.raytheon.uf.viz.monitor.fog.ui.dialogs.ThresholdCanvas.RangeEnum;
import com.raytheon.uf.viz.monitor.fog.ui.dialogs.ThresholdData.Threshold;
import com.raytheon.uf.viz.monitor.ui.dialogs.LoadSaveDeleteSelectDlg;
import com.raytheon.uf.viz.monitor.ui.dialogs.LoadSaveDeleteSelectDlg.DialogType;

public class FogMonThreshSetupDlg extends Dialog {
    /**
     * Dialog shell.
     */
    private Shell shell;

    /**
     * The display control.
     */
    private Display display;

    /**
     * Return value when the shell is disposed.
     */
    private Boolean returnValue = false;

    /**
     * Control font.
     */
    private Font controlFont;

    private Button fogProductRdo;

    private Button visRdo;

    private Button yLoRdo;

    private Button rLoRdo;

    private Button rHiRdo;

    private Button yHiRdo;

    private Button undoBtn;

    private Button redoBtn;

    private ThresholdCanvas threshCanvas;

    private Scale maxCloudTempScale;

    private Scale iceSnowVsFogScale;

    private Button iceSnowVsFogChk;

    private Scale coolFogVsWarmSurfScale;

    private Button coolFogVsWarmSurfChk;

    private Scale daytimeSmoothScale;

    private Button daytimeSmoothChk;

    private Scale adjacencyThreshScale;

    private Button adjacencyThreshChk;

    private Scale twilightAngleScale;

    private Button twilightAngleChk;

    private Scale fractalDimScale;

    private Button fractalDimChk;

    private Image maxCloudTempImg;

    private Image iceSnowVsFogImg;

    private Image coolFogVsWarmSurfImg;

    private Image daytimeSmoothImg;

    private Image adjacencyThreshImg;

    private Image twilightAngleImg;

    private Image fractalDimImg;

    private ThresholdData thresholdData;

    private HashMap<Scale, Label> scaleLblMap;

    /** gateway to fog monitor **/
    private FogMonitor fog = null;

    /** Algorithm Data manager **/
    private FogAlgorithmMgr fam = null;

    /** Array of fogAlg listeners **/
    private ArrayList<IFogResourceListener> fogListeners = new ArrayList<IFogResourceListener>();

    /**
     * Constructor.
     * 
     * @param parent
     *            Parent shell.
     */
    public FogMonThreshSetupDlg(Shell parent) {
        super(parent, 0);
        fam = FogAlgorithmMgr.getInstance();
        fog = FogMonitor.getInstance();
        this.addFogListener(fog);
    }

    /**
     * Open method used to display the dialog.
     * 
     * @return True/False.
     */
    public Object open() {
        Shell parent = getParent();
        display = parent.getDisplay();
        shell = new Shell(parent, SWT.DIALOG_TRIM);
        shell.setText("Fog Monitor Threshold Setup");

        // Create the main layout for the shell.
        GridLayout mainLayout = new GridLayout(1, false);
        mainLayout.marginHeight = 1;
        mainLayout.marginWidth = 1;
        mainLayout.verticalSpacing = 1;
        shell.setLayout(mainLayout);

        // Initialize all of the controls and layouts
        initializeComponents();

        shell.pack();

        shell.open();
        while (!shell.isDisposed()) {
            if (!display.readAndDispatch()) {
                display.sleep();
            }
        }

        controlFont.dispose();
        maxCloudTempImg.dispose();
        iceSnowVsFogImg.dispose();
        coolFogVsWarmSurfImg.dispose();
        daytimeSmoothImg.dispose();
        adjacencyThreshImg.dispose();
        twilightAngleImg.dispose();
        fractalDimImg.dispose();

        this.removeFogAlgListener(fog);

        return returnValue;
    }

    /**
     * Initialize the components on the display.
     */
    private void initializeComponents() {
        scaleLblMap = new HashMap<Scale, Label>();

        controlFont = new Font(shell.getDisplay(), "Monospace", 10, SWT.NORMAL);
        createImages();
        thresholdData = new ThresholdData();

        createMenu();

        createTopRadioControls();
        createThresholdControls();

        addSeparator(shell);
        addSeparator(shell);

        createScaleControls();

        updateDataControls();
    }

    private void createMenu() {
        Menu menuBar = new Menu(shell, SWT.BAR);

        createFileMenu(menuBar);

        shell.setMenuBar(menuBar);
    }

    /**
     * Create the File menu.
     * 
     * @param menuBar
     *            Menu bar.
     */
    private void createFileMenu(Menu menuBar) {
        // -------------------------------------
        // Create the file menu
        // -------------------------------------
        MenuItem fileMenuItem = new MenuItem(menuBar, SWT.CASCADE);
        fileMenuItem.setText("&File");

        // Create the File menu item with a File "dropdown" menu
        Menu fileMenu = new Menu(menuBar);
        fileMenuItem.setMenu(fileMenu);

        // -------------------------------------------------
        // Create all the items in the File dropdown menu
        // -------------------------------------------------

        // Configure menu item
        MenuItem openMI = new MenuItem(fileMenu, SWT.NONE);
        openMI.setText("Open...");
        openMI.addSelectionListener(new SelectionAdapter() {
            public void widgetSelected(SelectionEvent event) {
                openAction();
            }
        });

        new MenuItem(fileMenu, SWT.SEPARATOR);

        MenuItem saveAsMI = new MenuItem(fileMenu, SWT.NONE);
        saveAsMI.setText("Save As...");
        saveAsMI.addSelectionListener(new SelectionAdapter() {
            public void widgetSelected(SelectionEvent event) {
                saveAsAction();
            }
        });

        new MenuItem(fileMenu, SWT.SEPARATOR);

        MenuItem selectAsDefaultMI = new MenuItem(fileMenu, SWT.NONE);
        selectAsDefaultMI.setText("Select As Default...");
        selectAsDefaultMI.addSelectionListener(new SelectionAdapter() {
            public void widgetSelected(SelectionEvent event) {
                selectDefaultAction();
            }
        });

        MenuItem loadDefaultMI = new MenuItem(fileMenu, SWT.NONE);
        loadDefaultMI.setText("Load Default");
        loadDefaultMI.addSelectionListener(new SelectionAdapter() {
            public void widgetSelected(SelectionEvent event) {
                loadDefaultAction();
            }
        });

        new MenuItem(fileMenu, SWT.SEPARATOR);

        MenuItem notifyMI = new MenuItem(fileMenu, SWT.NONE);
        notifyMI.setText("Notify");
        notifyMI.addSelectionListener(new SelectionAdapter() {
            public void widgetSelected(SelectionEvent event) {
                notifyAction();
            }
        });

        new MenuItem(fileMenu, SWT.SEPARATOR);

        MenuItem deleteMI = new MenuItem(fileMenu, SWT.NONE);
        deleteMI.setText("Delete");
        deleteMI.addSelectionListener(new SelectionAdapter() {
            public void widgetSelected(SelectionEvent event) {
                deleteAction();
            }
        });

        new MenuItem(fileMenu, SWT.SEPARATOR);

        // Exit menu item
        MenuItem exitMI = new MenuItem(fileMenu, SWT.NONE);
        exitMI.setText("Exit");
        exitMI.addSelectionListener(new SelectionAdapter() {
            public void widgetSelected(SelectionEvent event) {
                shell.dispose();
            }
        });
    }

    private void createTopRadioControls() {
        Composite radioBtnComp = new Composite(shell, SWT.NONE);
        GridLayout gl = new GridLayout(2, false);
        gl.horizontalSpacing = 15;
        radioBtnComp.setLayout(gl);

        fogProductRdo = new Button(radioBtnComp, SWT.RADIO);
        fogProductRdo.setText("Fog Product [ T(10.7) - T(3.9), C ]");
        fogProductRdo.setSelection(true);
        fogProductRdo.addSelectionListener(new SelectionAdapter() {
            public void widgetSelected(SelectionEvent event) {
                threshCanvas.drawFog(true);
            }
        });

        visRdo = new Button(radioBtnComp, SWT.RADIO);
        visRdo.setText("VIS (normalized count)");
        visRdo.addSelectionListener(new SelectionAdapter() {
            public void widgetSelected(SelectionEvent event) {
                threshCanvas.drawFog(false);
            }
        });
    }

    private void createThresholdControls() {
        threshCanvas = new ThresholdCanvas(shell);

        /*
         * Add the Y_lo/R_lo/R_hi/Y_hi radio buttons.
         */
        Composite loHiComp = new Composite(shell, SWT.NONE);
        loHiComp.setLayout(new GridLayout(4, true));
        loHiComp
                .setLayoutData(new GridData(SWT.FILL, SWT.DEFAULT, true, false));

        yLoRdo = new Button(loHiComp, SWT.RADIO);
        yLoRdo.setText("Y_lo");
        yLoRdo.setSelection(true);
        yLoRdo.setLayoutData(new GridData(SWT.FILL, SWT.DEFAULT, true, false));
        yLoRdo.setBackground(display.getSystemColor(SWT.COLOR_YELLOW));
        yLoRdo.addSelectionListener(new SelectionAdapter() {
            public void widgetSelected(SelectionEvent event) {
                threshCanvas.setThresholdRange(ThresholdCanvas.RangeEnum.YLo);
            }
        });

        rLoRdo = new Button(loHiComp, SWT.RADIO);
        rLoRdo.setText("R_lo");
        rLoRdo.setLayoutData(new GridData(SWT.FILL, SWT.DEFAULT, true, false));
        rLoRdo.setBackground(display.getSystemColor(SWT.COLOR_RED));
        rLoRdo.addSelectionListener(new SelectionAdapter() {
            public void widgetSelected(SelectionEvent event) {
                threshCanvas.setThresholdRange(ThresholdCanvas.RangeEnum.RLo);
            }
        });

        rHiRdo = new Button(loHiComp, SWT.RADIO);
        rHiRdo.setText("R_hi");
        rHiRdo.setLayoutData(new GridData(SWT.FILL, SWT.DEFAULT, true, false));
        rHiRdo.setBackground(display.getSystemColor(SWT.COLOR_RED));
        rHiRdo.addSelectionListener(new SelectionAdapter() {
            public void widgetSelected(SelectionEvent event) {
                threshCanvas.setThresholdRange(ThresholdCanvas.RangeEnum.RHi);
            }
        });

        yHiRdo = new Button(loHiComp, SWT.RADIO);
        yHiRdo.setText("Y_hi");
        yHiRdo.setLayoutData(new GridData(SWT.FILL, SWT.DEFAULT, true, false));
        yHiRdo.setBackground(display.getSystemColor(SWT.COLOR_YELLOW));
        yHiRdo.addSelectionListener(new SelectionAdapter() {
            public void widgetSelected(SelectionEvent event) {
                threshCanvas.setThresholdRange(ThresholdCanvas.RangeEnum.YHi);
            }
        });

        /*
         * Add the increment/decrement/undo/redo buttons.
         */
        Composite incDecComp = new Composite(shell, SWT.NONE);
        incDecComp.setLayout(new GridLayout(6, true));
        incDecComp.setLayoutData(new GridData(SWT.FILL, SWT.DEFAULT, true,
                false));

        Button largeDecBtn = new Button(incDecComp, SWT.PUSH);
        largeDecBtn.setText("<<");
        largeDecBtn.setLayoutData(new GridData(SWT.FILL, SWT.DEFAULT, true,
                false));
        largeDecBtn.addSelectionListener(new SelectionAdapter() {
            public void widgetSelected(SelectionEvent event) {
                threshCanvas.largeDecrement();
            }
        });

        Button smallDecBtn = new Button(incDecComp, SWT.PUSH);
        smallDecBtn.setText("<");
        smallDecBtn.setLayoutData(new GridData(SWT.FILL, SWT.DEFAULT, true,
                false));
        smallDecBtn.addSelectionListener(new SelectionAdapter() {
            public void widgetSelected(SelectionEvent event) {
                threshCanvas.smallDecrement();
            }
        });

        undoBtn = new Button(incDecComp, SWT.PUSH);
        undoBtn.setText("Undo");
        undoBtn.setLayoutData(new GridData(SWT.FILL, SWT.DEFAULT, true, false));
        undoBtn.addSelectionListener(new SelectionAdapter() {
            public void widgetSelected(SelectionEvent event) {

            }
        });

        redoBtn = new Button(incDecComp, SWT.PUSH);
        redoBtn.setText("Redo");
        redoBtn.setLayoutData(new GridData(SWT.FILL, SWT.DEFAULT, true, false));
        redoBtn.addSelectionListener(new SelectionAdapter() {
            public void widgetSelected(SelectionEvent event) {

            }
        });

        Button smallIncBtn = new Button(incDecComp, SWT.PUSH);
        smallIncBtn.setText(">");
        smallIncBtn.setLayoutData(new GridData(SWT.FILL, SWT.DEFAULT, true,
                false));
        smallIncBtn.addSelectionListener(new SelectionAdapter() {
            public void widgetSelected(SelectionEvent event) {
                threshCanvas.smallIncrement();
            }
        });

        Button largeIncBtn = new Button(incDecComp, SWT.PUSH);
        largeIncBtn.setText(">>");
        largeIncBtn.setLayoutData(new GridData(SWT.FILL, SWT.DEFAULT, true,
                false));
        largeIncBtn.addSelectionListener(new SelectionAdapter() {
            public void widgetSelected(SelectionEvent event) {
                threshCanvas.largeIncrement();
            }
        });
    }

    private void createScaleControls() {
        Composite controlComp = new Composite(shell, SWT.NONE);
        controlComp.setLayout(new GridLayout(3, false));
        controlComp.setLayoutData(new GridData(SWT.FILL, SWT.DEFAULT, true,
                false));

        /*
         * Create the Maximum Cloud Temperature controls.
         */
        Label maxCloudTempImgLbl = new Label(controlComp, SWT.NONE);
        maxCloudTempImgLbl.setImage(maxCloudTempImg);
        GridData gd = new GridData();
        gd.horizontalSpan = 2;
        maxCloudTempImgLbl.setLayoutData(gd);

        Composite maxCloudTempComp = createScaleSpinnerComp(controlComp);

        Label maxCloudTempLbl = new Label(maxCloudTempComp, SWT.NONE);
        maxCloudTempLbl.setText("Maximum Cloud Temperature (C)");
        gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        gd.horizontalSpan = 2;
        maxCloudTempLbl.setLayoutData(gd);

        ThresholdData.Threshold thresh = ThresholdData.Threshold.MaxCloudTemp;

        gd = new GridData(SWT.FILL, SWT.CENTER, true, true);
        maxCloudTempScale = new Scale(maxCloudTempComp, SWT.HORIZONTAL);
        maxCloudTempScale.setLayoutData(gd);

        // maxCloudTempSpnr = new Spinner(maxCloudTempComp, SWT.BORDER);
        // maxCloudTempSpnr.setLayoutData(new GridData(50, SWT.DEFAULT));
        //        
        // setupControlsDecimal(maxCloudTempScale, maxCloudTempSpnr, thresh);

        Label tmp = new Label(maxCloudTempComp, SWT.RIGHT);
        tmp.setLayoutData(new GridData(50, SWT.DEFAULT));
        tmp.setFont(controlFont);

        setupControlsDecimalLbl(maxCloudTempScale, tmp, thresh);

        addSeparator(controlComp);

        /*
         * Create the Daytime Ice/Snow vs. Fog Thresholds controls.
         */
        iceSnowVsFogChk = new Button(controlComp, SWT.CHECK);
        iceSnowVsFogChk.setLayoutData(new GridData(18, SWT.DEFAULT));

        Label daytimeIceSnowImgLbl = new Label(controlComp, SWT.NONE);
        daytimeIceSnowImgLbl.setImage(iceSnowVsFogImg);

        Composite daytimeIceSnowComp = createScaleSpinnerComp(controlComp);

        Label daytimeIceSnowLbl = new Label(daytimeIceSnowComp, SWT.NONE);
        daytimeIceSnowLbl.setText("Daytime Ice/Snow vs. Fog Threshold (C)");
        gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        gd.horizontalSpan = 2;
        daytimeIceSnowLbl.setLayoutData(gd);

        thresh = ThresholdData.Threshold.IceSnowVsFog;

        gd = new GridData(SWT.FILL, SWT.CENTER, true, true);
        iceSnowVsFogScale = new Scale(daytimeIceSnowComp, SWT.HORIZONTAL);
        iceSnowVsFogScale.setLayoutData(gd);

        // iceSnowVsFogSpnr = new Spinner(daytimeIceSnowComp, SWT.BORDER);
        // iceSnowVsFogSpnr.setLayoutData(new GridData(50, SWT.DEFAULT));
        //        
        // setupControlsDecimal(iceSnowVsFogScale, iceSnowVsFogSpnr, thresh);

        tmp = new Label(daytimeIceSnowComp, SWT.RIGHT);
        tmp.setLayoutData(new GridData(50, SWT.DEFAULT));
        tmp.setFont(controlFont);

        setupControlsDecimalLbl(iceSnowVsFogScale, tmp, thresh);

        addSeparator(controlComp);

        /*
         * Create the Cool Fog vs. Warm Surface Thresholds controls.
         */
        coolFogVsWarmSurfChk = new Button(controlComp, SWT.CHECK);
        coolFogVsWarmSurfChk.setLayoutData(new GridData(18, SWT.DEFAULT));

        Label coolFogLblImg = new Label(controlComp, SWT.NONE);
        coolFogLblImg.setImage(coolFogVsWarmSurfImg);

        Composite coolFogComp = createScaleSpinnerComp(controlComp);

        Label coolFogLbl = new Label(coolFogComp, SWT.NONE);
        coolFogLbl.setText("Cool Fog vs. Warm Surface Threshold (C)");
        gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        gd.horizontalSpan = 2;
        coolFogLbl.setLayoutData(gd);

        thresh = ThresholdData.Threshold.CoolFogVsWarmSurf;

        gd = new GridData(SWT.FILL, SWT.CENTER, true, true);
        coolFogVsWarmSurfScale = new Scale(coolFogComp, SWT.HORIZONTAL);
        coolFogVsWarmSurfScale.setLayoutData(gd);

        // coolFogVsWarmSurfSpnr = new Spinner(coolFogComp, SWT.BORDER);
        // coolFogVsWarmSurfSpnr.setLayoutData(new GridData(50, SWT.DEFAULT));
        //        
        // setupControlsDecimal(coolFogVsWarmSurfScale, coolFogVsWarmSurfSpnr,
        // thresh);

        tmp = new Label(coolFogComp, SWT.RIGHT);
        tmp.setLayoutData(new GridData(50, SWT.DEFAULT));
        tmp.setFont(controlFont);

        setupControlsDecimalLbl(coolFogVsWarmSurfScale, tmp, thresh);

        addSeparator(controlComp);

        /*
         * Create the Daytime Smoothness Thresholds controls.
         */
        daytimeSmoothChk = new Button(controlComp, SWT.CHECK);
        daytimeSmoothChk.setLayoutData(new GridData(18, SWT.DEFAULT));

        Label daytimeSmoothLblImg = new Label(controlComp, SWT.NONE);
        daytimeSmoothLblImg.setImage(daytimeSmoothImg);

        Composite daytimeSmoothComp = createScaleSpinnerComp(controlComp);

        Label daytimeSmoothLbl = new Label(daytimeSmoothComp, SWT.NONE);
        daytimeSmoothLbl.setText("Daytime Smoothness Threshold (%)");
        gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        gd.horizontalSpan = 2;
        daytimeSmoothLbl.setLayoutData(gd);

        thresh = ThresholdData.Threshold.DaytimeSmooth;

        gd = new GridData(SWT.FILL, SWT.CENTER, true, true);
        daytimeSmoothScale = new Scale(daytimeSmoothComp, SWT.HORIZONTAL);
        daytimeSmoothScale.setLayoutData(gd);

        // daytimeSmoothSpnr = new Spinner(daytimeSmoothComp, SWT.BORDER);
        // daytimeSmoothSpnr.setLayoutData(new GridData(50, SWT.DEFAULT));
        //        
        // setupControlsDecimal(daytimeSmoothScale, daytimeSmoothSpnr, thresh);

        tmp = new Label(daytimeSmoothComp, SWT.RIGHT);
        tmp.setLayoutData(new GridData(50, SWT.DEFAULT));
        tmp.setFont(controlFont);

        setupControlsDecimalLbl(daytimeSmoothScale, tmp, thresh);

        addSeparator(controlComp);

        /*
         * Create the Adjacency Thresholds controls.
         */
        adjacencyThreshChk = new Button(controlComp, SWT.CHECK);
        adjacencyThreshChk.setLayoutData(new GridData(18, SWT.DEFAULT));

        Label adjacencyThreshLblImg = new Label(controlComp, SWT.NONE);
        adjacencyThreshLblImg.setImage(adjacencyThreshImg);

        Composite adjacencyThreshComp = createScaleSpinnerComp(controlComp);

        Label adjacencyThreshLbl = new Label(adjacencyThreshComp, SWT.NONE);
        adjacencyThreshLbl.setText("Adjacency Threshold");
        gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        gd.horizontalSpan = 2;
        adjacencyThreshLbl.setLayoutData(gd);

        thresh = ThresholdData.Threshold.AdjacencyThresh;

        gd = new GridData(SWT.FILL, SWT.CENTER, true, true);
        adjacencyThreshScale = new Scale(adjacencyThreshComp, SWT.HORIZONTAL);
        adjacencyThreshScale.setLayoutData(gd);

        // adjacencyThreshSpnr = new Spinner(adjacencyThreshComp, SWT.BORDER);
        // adjacencyThreshSpnr.setLayoutData(new GridData(50, SWT.DEFAULT));
        //        
        // setupControlsInteger(adjacencyThreshScale, adjacencyThreshSpnr,
        // thresh);

        tmp = new Label(adjacencyThreshComp, SWT.RIGHT);
        tmp.setLayoutData(new GridData(50, SWT.DEFAULT));
        tmp.setFont(controlFont);

        setupControlsIntegerLbl(adjacencyThreshScale, tmp, thresh);

        addSeparator(controlComp);

        /*
         * Create the Twilight Angle Threshold controls.
         */
        twilightAngleChk = new Button(controlComp, SWT.CHECK);
        twilightAngleChk.setLayoutData(new GridData(18, SWT.DEFAULT));

        Label twilightAngleLblImg = new Label(controlComp, SWT.NONE);
        twilightAngleLblImg.setImage(twilightAngleImg);

        Composite twilightAngleComp = createScaleSpinnerComp(controlComp);

        Label twilightAngleLbl = new Label(twilightAngleComp, SWT.NONE);
        twilightAngleLbl.setText("Twilight Angle Threshold (deg)");
        gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        gd.horizontalSpan = 2;
        twilightAngleLbl.setLayoutData(gd);

        thresh = ThresholdData.Threshold.TwilightAngle;

        gd = new GridData(SWT.FILL, SWT.CENTER, true, true);
        twilightAngleScale = new Scale(twilightAngleComp, SWT.HORIZONTAL);
        twilightAngleScale.setLayoutData(gd);

        // twilightAngleSpnr = new Spinner(twilightAngleComp, SWT.BORDER);
        // twilightAngleSpnr.setLayoutData(new GridData(50, SWT.DEFAULT));

        tmp = new Label(twilightAngleComp, SWT.RIGHT);
        tmp.setLayoutData(new GridData(50, SWT.DEFAULT));
        tmp.setFont(controlFont);

        setupControlsDecimalLbl(twilightAngleScale, tmp, thresh);

        // setupControlsDecimal(twilightAngleScale, twilightAngleSpnr, thresh);

        addSeparator(controlComp);

        /*
         * Create the Fractal Dimension Threshold controls.
         */
        fractalDimChk = new Button(controlComp, SWT.CHECK);
        fractalDimChk.setLayoutData(new GridData(18, SWT.DEFAULT));

        Label fractalDimLblImg = new Label(controlComp, SWT.NONE);
        fractalDimLblImg.setImage(fractalDimImg);

        Composite fractalDimComp = createScaleSpinnerComp(controlComp);

        Label fractalDimLbl = new Label(fractalDimComp, SWT.NONE);
        fractalDimLbl.setText("Fractal Dimension Threshold");
        gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        gd.horizontalSpan = 2;
        fractalDimLbl.setLayoutData(gd);

        thresh = ThresholdData.Threshold.FractalDim;

        gd = new GridData(SWT.FILL, SWT.CENTER, true, true);
        fractalDimScale = new Scale(fractalDimComp, SWT.HORIZONTAL);
        fractalDimScale.setLayoutData(gd);

        // fractalDimSpnr = new Spinner(fractalDimComp, SWT.BORDER);
        // fractalDimSpnr.setLayoutData(new GridData(50, SWT.DEFAULT));

        tmp = new Label(fractalDimComp, SWT.RIGHT);
        tmp.setLayoutData(new GridData(50, SWT.DEFAULT));
        tmp.setFont(controlFont);

        setupControlsDecimalLbl(fractalDimScale, tmp, thresh);

        // setupControlsDecimal(fractalDimScale, fractalDimSpnr, thresh);
    }

    private void setupControlsDecimalLbl(final Scale scale, final Label label,
            ThresholdData.Threshold thresh) {
        scale.setMinimum(0);
        scale.setData(thresh);
        scale.setMaximum(thresholdData.getScaleMaxValue(thresh));
        scale.setSelection(thresholdData.getInitialScaleValue(thresh));
        scale.addSelectionListener(new SelectionAdapter() {
            public void widgetSelected(SelectionEvent event) {
                ThresholdData.Threshold threshold = (ThresholdData.Threshold) label
                        .getData();
                label.setText(String.format(" %3.1f",
                        thresholdData.calcSpnrValueFromScale(threshold, scale
                                .getSelection()) / 10.0));
            }
        });

        label.setData(thresh);
        label.setText(String.format(" %3.1f", thresholdData
                .calcSpnrValueFromScale(thresh, scale.getSelection()) / 10.0));

        scaleLblMap.put(scale, label);
    }

    private void setupControlsIntegerLbl(final Scale scale, final Label label,
            ThresholdData.Threshold thresh) {
        scale.setMinimum(0);
        scale.setData(thresh);
        scale.setMaximum(thresholdData.getScaleMaxValue(thresh));
        scale.setSelection(thresholdData.getInitialScaleValue(thresh));
        scale.addSelectionListener(new SelectionAdapter() {
            public void widgetSelected(SelectionEvent event) {
                ThresholdData.Threshold threshold = (ThresholdData.Threshold) label
                        .getData();
                label.setText(String.format("%3d",
                        thresholdData.calcSpnrValueFromScale(threshold, scale
                                .getSelection())));
            }
        });

        label.setData(thresh);
        label.setText(String.format("%3d", thresholdData
                .calcSpnrValueFromScale(thresh, scale.getSelection())));

        scaleLblMap.put(scale, label);
    }

    private Composite createScaleSpinnerComp(Composite parentComp) {
        Composite comp = new Composite(parentComp, SWT.NONE);
        comp.setLayout(new GridLayout(2, false));
        comp.setLayoutData(new GridData(SWT.FILL, SWT.DEFAULT, true, false));

        return comp;
    }

    private void addSeparator(Composite parentComp) {
        GridLayout gl = (GridLayout) parentComp.getLayout();

        GridData gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        gd.horizontalSpan = gl.numColumns;
        Label sepLbl = new Label(parentComp, SWT.SEPARATOR | SWT.HORIZONTAL);
        sepLbl.setLayoutData(gd);
    }

    private void createImages() {
        ImageDescriptor id = Activator.imageDescriptorFromPlugin(
                Activator.PLUGIN_ID, "images/lenticular.png");
        maxCloudTempImg = id.createImage();

        id = Activator.imageDescriptorFromPlugin(Activator.PLUGIN_ID,
                "images/glacier.png");
        iceSnowVsFogImg = id.createImage();

        id = Activator.imageDescriptorFromPlugin(Activator.PLUGIN_ID,
                "images/MtStHelen.png");
        coolFogVsWarmSurfImg = id.createImage();

        id = Activator.imageDescriptorFromPlugin(Activator.PLUGIN_ID,
                "images/man-made.png");
        daytimeSmoothImg = id.createImage();

        id = Activator.imageDescriptorFromPlugin(Activator.PLUGIN_ID,
                "images/penguin.png");
        adjacencyThreshImg = id.createImage();

        id = Activator.imageDescriptorFromPlugin(Activator.PLUGIN_ID,
                "images/twilight-angle.png");
        twilightAngleImg = id.createImage();

        id = Activator.imageDescriptorFromPlugin(Activator.PLUGIN_ID,
                "images/fractal_dim.png");
        fractalDimImg = id.createImage();
    }

    private void formatLabelValue(Label lbl, int value) {
        ThresholdData.Threshold threshold = (ThresholdData.Threshold) lbl
                .getData();

        if (threshold == ThresholdData.Threshold.AdjacencyThresh) {
            lbl.setText(String.format("%3d", value));
        } else {
            lbl.setText(String.format(" %3.1f", (value / 10.0)));
        }
    }

    private void updateDataControls() {
        FogAlgorithmMgr fam = FogAlgorithmMgr.getInstance();

        /*
         * Threshold canvas (Fog Product & VIS)
         */
        threshCanvas.setFogVisThreshholds(fam.getAlgorithmXML()
                .getFogProductYLo(), fam.getAlgorithmXML().getFogProductRLo(),
                fam.getAlgorithmXML().getFogProductRHi(), fam.getAlgorithmXML()
                        .getFogProductYHi(), fam.getAlgorithmXML().getVisYLo(),
                fam.getAlgorithmXML().getVisRLo(), fam.getAlgorithmXML()
                        .getVisRHi(), fam.getAlgorithmXML().getVisYHi());

        int scaleValInt = Integer.MIN_VALUE;
        int spnrValFromScale = Integer.MIN_VALUE;

        Threshold currentThreshold = Threshold.MaxCloudTemp;

        /*
         * Maximum Cloud Temperature
         */
        currentThreshold = Threshold.MaxCloudTemp;
        scaleValInt = thresholdData.calcScaleValueFromSpinner(currentThreshold,
                fam.getAlgorithmXML().getMaxCloudTemp());
        maxCloudTempScale.setSelection(scaleValInt);
        spnrValFromScale = thresholdData.calcSpnrValueFromScale(
                currentThreshold, maxCloudTempScale.getSelection());
        formatLabelValue(scaleLblMap.get(maxCloudTempScale), spnrValFromScale);

        /*
         * Daytime Ice/Snow vs Fog Threshold
         */
        currentThreshold = Threshold.IceSnowVsFog;
        scaleValInt = thresholdData.calcScaleValueFromSpinner(currentThreshold,
                fam.getAlgorithmXML().getIceSnowVsFog());
        iceSnowVsFogScale.setSelection(scaleValInt);
        spnrValFromScale = thresholdData.calcSpnrValueFromScale(
                currentThreshold, iceSnowVsFogScale.getSelection());
        formatLabelValue(scaleLblMap.get(iceSnowVsFogScale), spnrValFromScale);

        iceSnowVsFogChk.setSelection(fam.getAlgorithmXML().isIceSnowVsFogOn());

        /*
         * Cool Fog vs Warm Surface Threshold
         */
        currentThreshold = Threshold.CoolFogVsWarmSurf;
        scaleValInt = thresholdData.calcScaleValueFromSpinner(currentThreshold,
                fam.getAlgorithmXML().getCoolFogVsWarmSurface());
        coolFogVsWarmSurfScale.setSelection(scaleValInt);
        spnrValFromScale = thresholdData.calcSpnrValueFromScale(
                currentThreshold, coolFogVsWarmSurfScale.getSelection());
        formatLabelValue(scaleLblMap.get(coolFogVsWarmSurfScale),
                spnrValFromScale);

        coolFogVsWarmSurfChk.setSelection(fam.getAlgorithmXML()
                .isCoolFogVsWarmSurfaceOn());

        /*
         * Daytime Smoothness Threshold
         */
        currentThreshold = Threshold.DaytimeSmooth;
        scaleValInt = thresholdData.calcScaleValueFromSpinner(
                Threshold.DaytimeSmooth, fam.getAlgorithmXML()
                        .getDaytimeSmoothThresh());
        daytimeSmoothScale.setSelection(scaleValInt);
        spnrValFromScale = thresholdData.calcSpnrValueFromScale(
                currentThreshold, daytimeSmoothScale.getSelection());
        formatLabelValue(scaleLblMap.get(daytimeSmoothScale), spnrValFromScale);

        daytimeSmoothChk.setSelection(fam.getAlgorithmXML()
                .isDaytimeSmoothThreshOn());

        /*
         * Adjacency Threshold
         */
        currentThreshold = Threshold.AdjacencyThresh;
        scaleValInt = thresholdData.calcScaleValueFromSpinner(
                Threshold.AdjacencyThresh, fam.getAlgorithmXML()
                        .getAdjacencyThresh());
        adjacencyThreshScale.setSelection(scaleValInt);
        spnrValFromScale = thresholdData.calcSpnrValueFromScale(
                currentThreshold, adjacencyThreshScale.getSelection());
        formatLabelValue(scaleLblMap.get(adjacencyThreshScale),
                spnrValFromScale);

        adjacencyThreshChk.setSelection(fam.getAlgorithmXML()
                .isAdjacencyThreshOn());

        /*
         * Twilight Angle Threshold
         */
        currentThreshold = Threshold.TwilightAngle;
        scaleValInt = thresholdData.calcScaleValueFromSpinner(
                Threshold.TwilightAngle, fam.getAlgorithmXML()
                        .getTwilightAngle());
        twilightAngleScale.setSelection(scaleValInt);
        spnrValFromScale = thresholdData.calcSpnrValueFromScale(
                currentThreshold, twilightAngleScale.getSelection());
        formatLabelValue(scaleLblMap.get(twilightAngleScale), spnrValFromScale);

        twilightAngleChk
                .setSelection(fam.getAlgorithmXML().isTwilightAngleOn());

        /*
         * Fractal Dimension Threshold
         */
        currentThreshold = Threshold.FractalDim;
        scaleValInt = thresholdData.calcScaleValueFromSpinner(
                Threshold.FractalDim, fam.getAlgorithmXML()
                        .getFractalDimension());
        fractalDimScale.setSelection(scaleValInt);
        spnrValFromScale = thresholdData.calcSpnrValueFromScale(
                currentThreshold, fractalDimScale.getSelection());
        formatLabelValue(scaleLblMap.get(fractalDimScale), spnrValFromScale);

        fractalDimChk
                .setSelection(fam.getAlgorithmXML().isFractalDimensionOn());
    }

    private void updateAlgorithmData() {

        double tmpVal = Double.NaN;

        /*
         * Fog Product
         */
        double[] fogVals = threshCanvas.getFogProductValues();
        fam.getAlgorithmXML()
                .setFogProductYLo(fogVals[RangeEnum.YLo.ordinal()]);
        fam.getAlgorithmXML()
                .setFogProductRLo(fogVals[RangeEnum.RLo.ordinal()]);
        fam.getAlgorithmXML()
                .setFogProductRHi(fogVals[RangeEnum.RHi.ordinal()]);
        fam.getAlgorithmXML()
                .setFogProductYHi(fogVals[RangeEnum.YHi.ordinal()]);

        /*
         * VIS values.
         */
        double[] visVals = threshCanvas.getVisValues();
        fam.getAlgorithmXML().setVisYLo(visVals[RangeEnum.YLo.ordinal()]);
        fam.getAlgorithmXML().setVisRLo(visVals[RangeEnum.RLo.ordinal()]);
        fam.getAlgorithmXML().setVisRHi(visVals[RangeEnum.RHi.ordinal()]);
        fam.getAlgorithmXML().setVisYHi(visVals[RangeEnum.YHi.ordinal()]);

        /*
         * Maximum Cloud Temperature
         */
        System.out.println("+++ Max cloud scale value = "
                + maxCloudTempScale.getSelection());
        tmpVal = thresholdData.calcSpnrValueFromScale(Threshold.MaxCloudTemp,
                maxCloudTempScale.getSelection()) / 10.0;
        System.out.println("+++ tmp value = " + tmpVal);
        fam.getAlgorithmXML().setMaxCloudTemp(tmpVal);

        /*
         * Daytime Ice/Snow vs Fog Threshold
         */
        tmpVal = thresholdData.calcSpnrValueFromScale(Threshold.IceSnowVsFog,
                iceSnowVsFogScale.getSelection()) / 10.0;
        fam.getAlgorithmXML().setIceSnowVsFog(tmpVal);

        fam.getAlgorithmXML().setIceSnowVsFogOn(iceSnowVsFogChk.getSelection());

        /*
         * Cool Fog vs Warm Surface Threshold
         */
        tmpVal = thresholdData.calcSpnrValueFromScale(
                Threshold.CoolFogVsWarmSurf, coolFogVsWarmSurfScale
                        .getSelection()) / 10.0;
        fam.getAlgorithmXML().setCoolFogVsWarmSurface(tmpVal);

        fam.getAlgorithmXML().setCoolFogVsWarmSurfaceOn(
                coolFogVsWarmSurfChk.getSelection());

        /*
         * Daytime Smoothness Threshold
         */
        tmpVal = thresholdData.calcSpnrValueFromScale(Threshold.DaytimeSmooth,
                daytimeSmoothScale.getSelection()) / 10.0;
        fam.getAlgorithmXML().setDaytimeSmoothThresh(tmpVal);

        fam.getAlgorithmXML().setDaytimeSmoothThreshOn(
                daytimeSmoothChk.getSelection());

        /*
         * Adjacency Threshold
         */
        tmpVal = thresholdData.calcSpnrValueFromScale(
                Threshold.AdjacencyThresh, adjacencyThreshScale.getSelection());
        fam.getAlgorithmXML().setAdjacencyThresh(tmpVal);

        fam.getAlgorithmXML().setAdjacencyThreshOn(
                adjacencyThreshChk.getSelection());

        /*
         * Twilight Angle Threshold
         */
        tmpVal = thresholdData.calcSpnrValueFromScale(Threshold.TwilightAngle,
                twilightAngleScale.getSelection()) / 10.0;
        fam.getAlgorithmXML().setTwilightAngle(tmpVal);

        fam.getAlgorithmXML().setTwilightAngleOn(
                twilightAngleChk.getSelection());

        /*
         * Fractal Dimension Threshold
         */
        tmpVal = thresholdData.calcSpnrValueFromScale(Threshold.FractalDim,
                fractalDimScale.getSelection()) / 10.0;
        fam.getAlgorithmXML().setFractalDimension(tmpVal);

        fam.getAlgorithmXML().setFractalDimensionOn(
                fractalDimChk.getSelection());
    }

    private void openAction() {
        FogAlgorithmMgr fam = FogAlgorithmMgr.getInstance();

        LoadSaveDeleteSelectDlg lsDlg = new LoadSaveDeleteSelectDlg(shell,
                DialogType.OPEN, fam.getAlgorithmThresholdPath(), fam
                        .getDefaultAlgorithmFileName());
        LocalizationFile fileName = (LocalizationFile) lsDlg.open();

        if (fileName == null) {
            System.out.println("FileName is null...");
            return;
        }

        System.out.println("Selected file absolute path= "
                + fileName.getFile().getAbsolutePath());
        System.out.println("Selected file name = "
                + fileName.getFile().getName());

        fam.loadAlgorithmThreashold(fileName.getFile().getName());
        updateDataControls();
    }

    private void saveAsAction() {
        updateAlgorithmData();

        LoadSaveDeleteSelectDlg lsDlg = new LoadSaveDeleteSelectDlg(shell,
                DialogType.SAVE_AS, fam.getAlgorithmThresholdPath(), fam
                        .getDefaultAlgorithmFileName());
        LocalizationFile fileName = (LocalizationFile) lsDlg.open();

        if (fileName == null) {
            System.out.println("FileName is null...");
            return;
        }

        System.out.println("Selected file absolute path= "
                + fileName.getFile().getAbsolutePath());
        System.out.println("Selected file name = "
                + fileName.getFile().getName());

        fam.saveAlgorithmXmlAs(fileName.getFile().getName());
    }

    private void selectDefaultAction() {
        FogAlgorithmMgr fam = FogAlgorithmMgr.getInstance();

        LoadSaveDeleteSelectDlg lsDlg = new LoadSaveDeleteSelectDlg(shell,
                DialogType.SELECT_DEFAULT, fam.getAlgorithmThresholdPath(), fam
                        .getDefaultAlgorithmFileName());
        LocalizationFile fileName = (LocalizationFile) lsDlg.open();

        if (fileName == null) {
            System.out.println("FileName is null...");
            return;
        }

        System.out.println("Selected file absolute path= "
                + fileName.getFile().getAbsolutePath());
        System.out.println("Selected file name = "
                + fileName.getFile().getName());

        fam.setDefaultAlgorithmFileName(fileName.getFile().getName());
    }

    private void loadDefaultAction() {
        FogAlgorithmMgr fam = FogAlgorithmMgr.getInstance();
        fam.loadDefaultAlgorithmData();
        updateDataControls();
    }

    private void notifyAction() {

        Display.getDefault().asyncExec(new Runnable() {
            public void run() {
                Iterator<IFogResourceListener> iter = fogListeners
                        .iterator();

                while (iter.hasNext()) {
                    IFogResourceListener listener = (IFogResourceListener) iter
                            .next();
                    listener.algorithmUpdate();
                }
            }
        });
    }

    private void deleteAction() {
        FogAlgorithmMgr fam = FogAlgorithmMgr.getInstance();

        LoadSaveDeleteSelectDlg lsDlg = new LoadSaveDeleteSelectDlg(shell,
                DialogType.DELETE, fam.getDefaultFileNamePath(), fam
                        .getDefaultAlgorithmFileName());
        LocalizationFile fileName = (LocalizationFile) lsDlg.open();

        if (fileName == null) {
            return;
        }

        System.out.println("Selected file absolute path= "
                + fileName.getFile().getAbsolutePath());
        System.out.println("Selected file name = "
                + fileName.getFile().getName());

        try {
            fileName.delete();
        } catch (Exception ex) {
            ex.printStackTrace();
        }
    }

    /**
     * add a listener
     * 
     * @param ifogl
     */
    public void addFogListener(IFogResourceListener ifogl) {
        fogListeners.add(ifogl);
    }

    /**
     * remove a listener
     * 
     * @param ifogl
     */
    public void removeFogAlgListener(IFogResourceListener ifogl) {
        fogListeners.remove(ifogl);
    }

    /**
     * @return the fogListeners
     */
    public ArrayList<IFogResourceListener> getFogListeners() {
        return fogListeners;
    }

    /**
     * @param fogListeners
     *            the fogListeners to set
     */
    public void setFogListeners(ArrayList<IFogResourceListener> fogListeners) {
        this.fogListeners = fogListeners;
    }
}
