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
import java.util.List;
import java.util.Map;

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
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Layout;
import org.eclipse.swt.widgets.Menu;
import org.eclipse.swt.widgets.MenuItem;
import org.eclipse.swt.widgets.Scale;
import org.eclipse.swt.widgets.Shell;

import com.raytheon.uf.common.localization.LocalizationFile;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.viz.monitor.fog.Activator;
import com.raytheon.uf.viz.monitor.fog.FogMonitor;
import com.raytheon.uf.viz.monitor.fog.listeners.IFogResourceListener;
import com.raytheon.uf.viz.monitor.fog.threshold.FogAlgorithmMgr;
import com.raytheon.uf.viz.monitor.fog.ui.dialogs.ThresholdCanvas.RangeEnum;
import com.raytheon.uf.viz.monitor.fog.ui.dialogs.ThresholdData.Threshold;
import com.raytheon.uf.viz.monitor.ui.dialogs.LoadSaveDeleteSelectDlg;
import com.raytheon.uf.viz.monitor.ui.dialogs.LoadSaveDeleteSelectDlg.DialogType;
import com.raytheon.viz.ui.dialogs.CaveSWTDialog;
import com.raytheon.viz.ui.dialogs.ICloseCallback;

/**
 * Fog Monitor threshold algorithm setting dialog.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Nov 29, 2012 1351       skorolev    Changes for non-blocking dialog.
 * 
 * </pre>
 * 
 * @author
 * @version 1.0
 */
public class FogMonThreshSetupDlg extends CaveSWTDialog {

    private final IUFStatusHandler statusHandler = UFStatus
            .getHandler(FogMonThreshSetupDlg.class);

    /** Control font. **/
    private Font controlFont;

    /** Fog Product **/
    private Button fogProductRdo;

    /** VIS (normalized count) **/
    private Button visRdo;

    /** Y_lo boundary **/
    private Button yLoRdo;

    /** R_lo boundary **/
    private Button rLoRdo;

    /** R_hi boundary **/
    private Button rHiRdo;

    /** Y_hi boundary **/
    private Button yHiRdo;

    /** Undo **/
    private Button undoBtn;

    /** Redo **/
    private Button redoBtn;

    /** Threshold Canvas **/
    private ThresholdCanvas threshCanvas;

    /** max Cloud Temperature Threshold Scale **/
    private Scale maxCloudTempScale;

    /** Ice Snow vs Fog Threshold Scale **/
    private Scale iceSnowVsFogScale;

    /** Ice Snow vs Fog Threshold check box **/
    private Button iceSnowVsFogChk;

    /** Cool Fog vs Warm Surface Threshold Scale **/
    private Scale coolFogVsWarmSurfScale;

    /** Cool Fog vs Warm Surface Threshold check box **/
    private Button coolFogVsWarmSurfChk;

    /** daytime Smooth Threshold Scale **/
    private Scale daytimeSmoothScale;

    /** daytime Smooth Threshold check box **/
    private Button daytimeSmoothChk;

    /** adjacency Threshold Scale **/
    private Scale adjacencyThreshScale;

    /** Adjacency Threshold check box **/
    private Button adjacencyThreshChk;

    /** twilight Angle Threshold Scale **/
    private Scale twilightAngleScale;

    /** twilight Angle Threshold check box **/
    private Button twilightAngleChk;

    /** fractal Dimension Scale **/
    private Scale fractalDimScale;

    /** fractal Dimension check box **/
    private Button fractalDimChk;

    /** max Cloud Temperature Threshold image **/
    private Image maxCloudTempImg;

    /** Ice Snow vs Fog Threshold image **/
    private Image iceSnowVsFogImg;

    /** Cool Fog vs Warm Surface Threshold image **/
    private Image coolFogVsWarmSurfImg;

    /** daytime Smooth Threshold image **/
    private Image daytimeSmoothImg;

    /** Adjacency Threshold image **/
    private Image adjacencyThreshImg;

    /** twilight Angle image **/
    private Image twilightAngleImg;

    /** fractal Dimension image **/
    private Image fractalDimImg;

    /** Threshold Data **/
    private ThresholdData thresholdData;

    /** Scale labels map **/
    private Map<Scale, Label> scaleLblMap;

    /** gateway to fog monitor **/
    private FogMonitor fog = null;

    /** Algorithm Data manager **/
    private FogAlgorithmMgr fam = null;

    /** Array of fogAlg listeners **/
    private List<IFogResourceListener> fogListeners = new ArrayList<IFogResourceListener>();

    /** Menu option Open... **/
    private LoadSaveDeleteSelectDlg openDlg;

    /** Menu option Save as ... **/
    private LoadSaveDeleteSelectDlg saveDlg;

    /** Menu option Delete... **/
    private LoadSaveDeleteSelectDlg deleteDlg;

    /** Menu option Select... **/
    private LoadSaveDeleteSelectDlg selectDlg;

    /**
     * Constructor.
     * 
     * @param parent
     *            Parent shell.
     */
    public FogMonThreshSetupDlg(Shell parent) {
        super(parent, SWT.DIALOG_TRIM, CAVE.DO_NOT_BLOCK
                | CAVE.INDEPENDENT_SHELL);
        setText("Fog Monitor Threshold Setup");
        fam = FogAlgorithmMgr.getInstance();
        fog = FogMonitor.getInstance();
        this.addFogListener(fog);
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.viz.ui.dialogs.CaveSWTDialogBase#constructShellLayout()
     */
    @Override
    protected Layout constructShellLayout() {
        // Create the main layout for the shell.
        GridLayout mainLayout = new GridLayout(1, false);
        mainLayout.marginHeight = 1;
        mainLayout.marginWidth = 1;
        mainLayout.verticalSpacing = 1;
        return mainLayout;
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.viz.ui.dialogs.CaveSWTDialogBase#disposed()
     */
    @Override
    protected void disposed() {
        controlFont.dispose();
        maxCloudTempImg.dispose();
        iceSnowVsFogImg.dispose();
        coolFogVsWarmSurfImg.dispose();
        daytimeSmoothImg.dispose();
        adjacencyThreshImg.dispose();
        twilightAngleImg.dispose();
        fractalDimImg.dispose();
        this.removeFogAlgListener(fog);
    }

    /**
     * @param shell
     */
    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.ui.dialogs.CaveSWTDialogBase#initializeComponents(org
     * .eclipse.swt.widgets.Shell)
     */
    @Override
    protected void initializeComponents(Shell shell) {
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

    /**
     * Creates the Menu.
     */
    private void createMenu() {
        Menu menuBar = new Menu(shell, SWT.BAR);
        createFileMenu(menuBar);
        shell.setMenuBar(menuBar);
    }

    /**
     * Creates the File menu.
     * 
     * @param menuBar
     *            Menu bar.
     */
    private void createFileMenu(Menu menuBar) {
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
                close();
            }
        });
    }

    /**
     * Creates Top Radio Controls
     */
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

    /**
     * Creates Threshold Controls
     */
    private void createThresholdControls() {
        threshCanvas = new ThresholdCanvas(shell);
        /*
         * Add the Y_lo/R_lo/R_hi/Y_hi radio buttons.
         */
        Composite loHiComp = new Composite(shell, SWT.NONE);
        loHiComp.setLayout(new GridLayout(4, true));
        loHiComp.setLayoutData(new GridData(SWT.FILL, SWT.DEFAULT, true, false));

        yLoRdo = new Button(loHiComp, SWT.RADIO);
        yLoRdo.setText("Y_lo");
        yLoRdo.setSelection(true);
        yLoRdo.setLayoutData(new GridData(SWT.FILL, SWT.DEFAULT, true, false));
        yLoRdo.setBackground(getDisplay().getSystemColor(SWT.COLOR_YELLOW));
        yLoRdo.addSelectionListener(new SelectionAdapter() {
            public void widgetSelected(SelectionEvent event) {
                threshCanvas.setThresholdRange(ThresholdCanvas.RangeEnum.YLo);
            }
        });

        rLoRdo = new Button(loHiComp, SWT.RADIO);
        rLoRdo.setText("R_lo");
        rLoRdo.setLayoutData(new GridData(SWT.FILL, SWT.DEFAULT, true, false));
        rLoRdo.setBackground(getDisplay().getSystemColor(SWT.COLOR_RED));
        rLoRdo.addSelectionListener(new SelectionAdapter() {
            public void widgetSelected(SelectionEvent event) {
                threshCanvas.setThresholdRange(ThresholdCanvas.RangeEnum.RLo);
            }
        });

        rHiRdo = new Button(loHiComp, SWT.RADIO);
        rHiRdo.setText("R_hi");
        rHiRdo.setLayoutData(new GridData(SWT.FILL, SWT.DEFAULT, true, false));
        rHiRdo.setBackground(getDisplay().getSystemColor(SWT.COLOR_RED));
        rHiRdo.addSelectionListener(new SelectionAdapter() {
            public void widgetSelected(SelectionEvent event) {
                threshCanvas.setThresholdRange(ThresholdCanvas.RangeEnum.RHi);
            }
        });

        yHiRdo = new Button(loHiComp, SWT.RADIO);
        yHiRdo.setText("Y_hi");
        yHiRdo.setLayoutData(new GridData(SWT.FILL, SWT.DEFAULT, true, false));
        yHiRdo.setBackground(getDisplay().getSystemColor(SWT.COLOR_YELLOW));
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

    /**
     * Creates Scale Controls
     */
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

        tmp = new Label(twilightAngleComp, SWT.RIGHT);
        tmp.setLayoutData(new GridData(50, SWT.DEFAULT));
        tmp.setFont(controlFont);

        setupControlsDecimalLbl(twilightAngleScale, tmp, thresh);
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

        tmp = new Label(fractalDimComp, SWT.RIGHT);
        tmp.setLayoutData(new GridData(50, SWT.DEFAULT));
        tmp.setFont(controlFont);

        setupControlsDecimalLbl(fractalDimScale, tmp, thresh);
    }

    /**
     * Sets Controls Decimal Label
     * 
     * @param scale
     * @param label
     * @param thresh
     */
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
                label.setText(String.format(
                        " %3.1f",
                        thresholdData.calcSpnrValueFromScale(threshold,
                                scale.getSelection()) / 10.0));
            }
        });

        label.setData(thresh);
        label.setText(String.format(
                " %3.1f",
                thresholdData.calcSpnrValueFromScale(thresh,
                        scale.getSelection()) / 10.0));

        scaleLblMap.put(scale, label);
    }

    /**
     * Sets Controls Integer Label
     * 
     * @param scale
     * @param label
     * @param thresh
     */
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
                label.setText(String.format(
                        "%3d",
                        thresholdData.calcSpnrValueFromScale(threshold,
                                scale.getSelection())));
            }
        });
        label.setData(thresh);
        label.setText(String.format(
                "%3d",
                thresholdData.calcSpnrValueFromScale(thresh,
                        scale.getSelection())));
        scaleLblMap.put(scale, label);
    }

    /**
     * Creates Scale Spinner Composites
     * 
     * @param parentComp
     * @return
     */
    private Composite createScaleSpinnerComp(Composite parentComp) {
        Composite comp = new Composite(parentComp, SWT.NONE);
        comp.setLayout(new GridLayout(2, false));
        comp.setLayoutData(new GridData(SWT.FILL, SWT.DEFAULT, true, false));
        return comp;
    }

    /**
     * Adds Separator
     * 
     * @param parentComp
     */
    private void addSeparator(Composite parentComp) {
        GridLayout gl = (GridLayout) parentComp.getLayout();
        GridData gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        gd.horizontalSpan = gl.numColumns;
        Label sepLbl = new Label(parentComp, SWT.SEPARATOR | SWT.HORIZONTAL);
        sepLbl.setLayoutData(gd);
    }

    /**
     * Creates Images
     */
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

    /**
     * Formats Label Value
     * 
     * @param lbl
     * @param value
     */
    private void formatLabelValue(Label lbl, int value) {
        ThresholdData.Threshold threshold = (ThresholdData.Threshold) lbl
                .getData();
        if (threshold == ThresholdData.Threshold.AdjacencyThresh) {
            lbl.setText(String.format("%3d", value));
        } else {
            lbl.setText(String.format(" %3.1f", (value / 10.0)));
        }
    }

    /**
     * Updates Data Controls
     */
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

    /**
     * Updates Algorithm Data.
     */
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
        tmpVal = thresholdData.calcSpnrValueFromScale(Threshold.MaxCloudTemp,
                maxCloudTempScale.getSelection()) / 10.0;
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
                Threshold.CoolFogVsWarmSurf,
                coolFogVsWarmSurfScale.getSelection()) / 10.0;
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

    /**
     * Open action.
     */
    private void openAction() {
        if (openDlg == null) {
            FogAlgorithmMgr fam = FogAlgorithmMgr.getInstance();
            openDlg = new LoadSaveDeleteSelectDlg(shell, DialogType.OPEN,
                    fam.getAlgorithmThresholdPath(),
                    fam.getDefaultAlgorithmFileName());
            openDlg.setCloseCallback(new ICloseCallback() {
                @Override
                public void dialogClosed(Object returnValue) {
                    if (returnValue instanceof LocalizationFile) {
                        LocalizationFile fileName = (LocalizationFile) returnValue;
                        doOpenFile(fileName);
                    }
                    openDlg = null;
                }
            });
        }
        openDlg.open();
    }

    /**
     * Opens selected file.
     * 
     * @param fileName
     */
    private void doOpenFile(LocalizationFile fileName) {
        FogAlgorithmMgr fam = FogAlgorithmMgr.getInstance();
        fam.loadAlgorithmThreashold(fileName.getFile().getName());
        updateDataControls();
    }

    /**
     * Save As Action.
     */
    private void saveAsAction() {
        if (saveDlg == null) {
            saveDlg = new LoadSaveDeleteSelectDlg(shell, DialogType.SAVE_AS,
                    fam.getAlgorithmThresholdPath(),
                    fam.getDefaultAlgorithmFileName());
            saveDlg.setCloseCallback(new ICloseCallback() {
                @Override
                public void dialogClosed(Object returnValue) {
                    if (returnValue instanceof LocalizationFile) {
                        LocalizationFile fileName = (LocalizationFile) returnValue;
                        doSaveFileAs(fileName);
                    }
                    saveDlg = null;
                }
            });
        }
        saveDlg.open();
    }

    /**
     * Saves selected file.
     * 
     * @param fileName
     */
    private void doSaveFileAs(LocalizationFile fileName) {
        FogAlgorithmMgr fam = FogAlgorithmMgr.getInstance();
        updateAlgorithmData();
        fam.saveAlgorithmXmlAs(fileName.getFile().getName());
    }

    /**
     * Select Default Algorithm File Action.
     */
    private void selectDefaultAction() {
        if (selectDlg == null) {
            FogAlgorithmMgr fam = FogAlgorithmMgr.getInstance();
            selectDlg = new LoadSaveDeleteSelectDlg(shell,
                    DialogType.SELECT_DEFAULT, fam.getAlgorithmThresholdPath(),
                    fam.getDefaultAlgorithmFileName());
            selectDlg.setCloseCallback(new ICloseCallback() {
                @Override
                public void dialogClosed(Object returnValue) {
                    if (returnValue instanceof LocalizationFile) {
                        LocalizationFile fileName = (LocalizationFile) returnValue;
                        doSelectDefault(fileName);
                    }
                    selectDlg = null;
                }
            });
        }
        selectDlg.open();
    }

    /**
     * Selects default file.
     * 
     * @param fileName
     */
    private void doSelectDefault(LocalizationFile fileName) {
        FogAlgorithmMgr fam = FogAlgorithmMgr.getInstance();
        fam.setDefaultAlgorithmFileName(fileName.getFile().getName());
    }

    /**
     * Load Default Algorithm Action.
     */
    private void loadDefaultAction() {
        FogAlgorithmMgr fam = FogAlgorithmMgr.getInstance();
        fam.loadDefaultAlgorithmData();
        updateDataControls();
    }

    /**
     * Notify Action.
     */
    private void notifyAction() {
        Display.getDefault().asyncExec(new Runnable() {
            public void run() {
                Iterator<IFogResourceListener> iter = fogListeners.iterator();
                while (iter.hasNext()) {
                    IFogResourceListener listener = (IFogResourceListener) iter
                            .next();
                    listener.algorithmUpdate();
                }
            }
        });
    }

    /**
     * Delete Action.
     */
    private void deleteAction() {
        if (deleteDlg == null) {
            FogAlgorithmMgr fam = FogAlgorithmMgr.getInstance();
            deleteDlg = new LoadSaveDeleteSelectDlg(shell, DialogType.DELETE,
                    fam.getDefaultFileNamePath(),
                    fam.getDefaultAlgorithmFileName());
            deleteDlg.setCloseCallback(new ICloseCallback() {
                @Override
                public void dialogClosed(Object returnValue) {
                    if (returnValue instanceof LocalizationFile) {
                        LocalizationFile fileName = (LocalizationFile) returnValue;
                        doDelete(fileName);
                    }
                    deleteDlg = null;
                }
            });
        }
        deleteDlg.open();
    }

    /**
     * Deletes current default file.
     * 
     * @param fileName
     */
    private void doDelete(LocalizationFile fileName) {
        try {
            fileName.delete();
        } catch (Exception ex) {
            statusHandler.handle(Priority.ERROR, ex.getMessage());
        }
    }

    /**
     * Adds a listener
     * 
     * @param ifogl
     */
    public void addFogListener(IFogResourceListener ifogl) {
        fogListeners.add(ifogl);
    }

    /**
     * Removes a listener
     * 
     * @param ifogl
     */
    public void removeFogAlgListener(IFogResourceListener ifogl) {
        fogListeners.remove(ifogl);
    }

    /**
     * Gets Fog listeners.
     * 
     * @return the fogListeners
     */
    public List<IFogResourceListener> getFogListeners() {
        return fogListeners;
    }

    /**
     * Sets Fog listeners.
     * 
     * @param fogListeners
     *            the fogListeners to set
     */
    public void setFogListeners(List<IFogResourceListener> fogListeners) {
        this.fogListeners = fogListeners;
    }
}
