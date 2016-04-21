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
package com.raytheon.uf.viz.monitor.scan.commondialogs;

import java.util.LinkedHashMap;
import java.util.Set;

import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.graphics.Font;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Combo;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Layout;
import org.eclipse.swt.widgets.MessageBox;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Spinner;

import com.raytheon.uf.common.monitor.scan.config.SCANConfigEnums.CELLTable;
import com.raytheon.uf.common.monitor.scan.config.SCANConfigEnums.ScanTables;
import com.raytheon.uf.common.monitor.scan.xml.SCANAttributesXML;
import com.raytheon.uf.common.monitor.scan.xml.SCANConfigCellXML;
import com.raytheon.uf.viz.monitor.scan.config.CellConfigMgr;
import com.raytheon.uf.viz.monitor.scan.config.SCANConfig;
import com.raytheon.viz.ui.dialogs.CaveSWTDialog;

/**
 * Storm Cell ID Display Dialog
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 24 Jul 2013  #2143      skorolev    Changes for non-blocking dialogs.
 * 15 Aug 2013   2143      mpduff      Remove resize.
 * </pre>
 * 
 * @author
 * @version 1.0
 */
public class StormCellIdDisplayDlg extends CaveSWTDialog implements
        ICommonDialogAction {

    /**
     * Large Label Font
     */
    private Font lrgLabelFont;

    /**
     * Small Label Font
     */
    private Font smLabelFont;

    /**
     * High Hexagon Checkbox
     */
    private Button highHexagonChk;

    /**
     * Middle Hexagon Checkbox
     */
    private Button midHexagonChk;

    /**
     * Low Hexagon Checkbox
     */
    private Button lowHexagonChk;

    /**
     * Future Tracks Checkbox
     */
    private Button futureTracksChk;

    /**
     * Past Traks Checkbox
     */
    private Button pastTracksChk;

    /**
     * Low Arrow Checkbox
     */
    private Button lowArrowsChk;

    /**
     * Middle Arrow Checkbox
     */
    private Button midArrowsChk;

    /**
     * High Arrow Checkbox
     */
    private Button highArrowsChk;

    /**
     * High ID Checkbox
     */
    private Button highIdsChk;

    /**
     * Middle ID Checkbox
     */
    private Button midIdsChk;

    /**
     * Low ID Checkbox
     */
    private Button lowIdsChk;

    /**
     * Radius Ring Two Value Slider
     */
    private TwoValueSliderCanvas radiusRngSlider;

    /**
     * Radius Interpolation Two Value Slider
     */
    private TwoValueSliderCanvas radiusInterpolSlider;

    /**
     * Clutter Three Value Slider
     */
    private ThreeValueSliderCanvas clutterSlider;

    /**
     * Radius Interpolation Combo Control
     */
    private Combo radInterpolCbo;

    /**
     * Unit Label Width
     */
    private final int unitLabelWidth = 100;

    /**
     * Attribute Unit Map
     */
    private LinkedHashMap<String, String> attrUnitMap;

    /**
     * Radius Interpolation Unit Label
     */
    private Label raduisInterpolUnitLbl;

    /**
     * SCAN Configuration
     */
    private SCANConfig scanCfg;

    /**
     * Full Shaft Checkbox
     */
    private Button fullShaftChk;

    /**
     * Conversion
     */
    private Spinner conversionSpnr;

    /**
     * Clutter combo control
     */
    private Combo clutterThreshCbo;

    /**
     * Threshold Unit Label
     */
    private Label thresholdUnitLbl;

    private final IStormCellDisplayUpdate stormCellCB;

    /**
     * Constructor
     * 
     * @param parentShell
     * @param cb
     */
    public StormCellIdDisplayDlg(Shell parentShell, IStormCellDisplayUpdate cb) {
        super(parentShell, SWT.DIALOG_TRIM, CAVE.DO_NOT_BLOCK);
        setText("Storm Cell Identification Display Parameters");

        stormCellCB = cb;
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
        mainLayout.marginWidth = 2;
        mainLayout.verticalSpacing = 10;
        return mainLayout;
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.viz.ui.dialogs.CaveSWTDialogBase#disposed()
     */
    @Override
    protected void disposed() {
        lrgLabelFont.dispose();
        smLabelFont.dispose();
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.ui.dialogs.CaveSWTDialogBase#initializeComponents(org
     * .eclipse.swt.widgets.Shell)
     */
    @Override
    protected void initializeComponents(Shell shell) {
        // Initialize all of the controls and layouts
        scanCfg = SCANConfig.getInstance();
        lrgLabelFont = new Font(getDisplay(), "Monospaced", 12, SWT.BOLD);
        smLabelFont = new Font(getDisplay(), "Sans", 10, SWT.BOLD);
        attrUnitMap = scanCfg.getClutterAttributes(ScanTables.CELL);

        createSymbolsControls();
        addSeparator(shell);
        createHexagonControls();
        addSeparator(shell);
        createArrowControls();
        addSeparator(shell);
        createClutterControls();
        addSeparator(shell);
        createBottomButtons();

        radiusInterpolationAction(true);
        clutterAction();
    }

    /**
     * Create Symbols Controls
     */
    private void createSymbolsControls() {
        SCANConfigCellXML cellCfgXML = ((CellConfigMgr) scanCfg
                .getAbsConfigMgr(ScanTables.CELL)).getScanCellCfgXML();

        Composite symbolsComp = new Composite(shell, SWT.NONE);
        GridLayout gl = new GridLayout(3, true);
        gl.verticalSpacing = 10;
        gl.horizontalSpacing = 80;
        symbolsComp.setLayout(gl);
        symbolsComp.setLayoutData(new GridData(SWT.CENTER, SWT.DEFAULT, true,
                false));

        GridData gd = new GridData(SWT.CENTER, SWT.DEFAULT, true, false);
        gd.horizontalSpan = ((GridLayout) symbolsComp.getLayout()).numColumns;
        Label symbolsLbl = new Label(symbolsComp, SWT.NONE);
        symbolsLbl.setText("-- SYMBOLS --");
        symbolsLbl.setFont(lrgLabelFont);
        symbolsLbl.setLayoutData(gd);

        gd = new GridData(SWT.LEFT, SWT.DEFAULT, true, false);
        Label blankLabel = new Label(symbolsComp, SWT.NONE);
        blankLabel.setText("");
        blankLabel.setFont(smLabelFont);
        blankLabel.setLayoutData(gd);

        gd = new GridData(SWT.LEFT, SWT.DEFAULT, true, false);
        futureTracksChk = new Button(symbolsComp, SWT.CHECK);
        futureTracksChk.setText("Future Tracks");
        futureTracksChk.setSelection(cellCfgXML.getFutureTracks());
        futureTracksChk.setLayoutData(gd);

        gd = new GridData(SWT.LEFT, SWT.DEFAULT, true, false);
        pastTracksChk = new Button(symbolsComp, SWT.CHECK);
        pastTracksChk.setText("Past Tracks");
        pastTracksChk.setSelection(cellCfgXML.getPastTracks());
        pastTracksChk.setLayoutData(gd);

        // Ids
        gd = new GridData(SWT.LEFT, SWT.DEFAULT, true, false);
        highIdsChk = new Button(symbolsComp, SWT.CHECK);
        highIdsChk.setText("High IDs");
        highIdsChk.setSelection(cellCfgXML.getSymsIdHigh());
        highIdsChk.setLayoutData(gd);

        gd = new GridData(SWT.LEFT, SWT.DEFAULT, true, false);
        midIdsChk = new Button(symbolsComp, SWT.CHECK);
        midIdsChk.setText("Mid IDs");
        midIdsChk.setSelection(cellCfgXML.getSymsIdMid());
        midIdsChk.setLayoutData(gd);

        gd = new GridData(SWT.LEFT, SWT.DEFAULT, true, false);
        lowIdsChk = new Button(symbolsComp, SWT.CHECK);
        lowIdsChk.setText("Low IDs");
        lowIdsChk.setSelection(cellCfgXML.getSymsIdLow());
        lowIdsChk.setLayoutData(gd);

        // hexagons
        gd = new GridData(SWT.LEFT, SWT.DEFAULT, true, false);
        highHexagonChk = new Button(symbolsComp, SWT.CHECK);
        highHexagonChk.setText("High Hexagons");
        highHexagonChk.setSelection(cellCfgXML.getSymsCircleHigh());
        highHexagonChk.setLayoutData(gd);

        gd = new GridData(SWT.LEFT, SWT.DEFAULT, true, false);
        midHexagonChk = new Button(symbolsComp, SWT.CHECK);
        midHexagonChk.setText("Mid Hexagons");
        midHexagonChk.setSelection(cellCfgXML.getSymsCircleMid());
        midHexagonChk.setLayoutData(gd);

        gd = new GridData(SWT.LEFT, SWT.DEFAULT, true, false);
        lowHexagonChk = new Button(symbolsComp, SWT.CHECK);
        lowHexagonChk.setText("Low Hexagons");
        lowHexagonChk.setSelection(cellCfgXML.getSymsCircleLow());
        lowHexagonChk.setLayoutData(gd);

        // arrow toggles
        gd = new GridData(SWT.LEFT, SWT.DEFAULT, true, false);
        highArrowsChk = new Button(symbolsComp, SWT.CHECK);
        highArrowsChk.setText("High Arrows");
        highArrowsChk.setSelection(cellCfgXML.getSymsArrowHigh());
        highArrowsChk.setLayoutData(gd);

        gd = new GridData(SWT.LEFT, SWT.DEFAULT, true, false);
        midArrowsChk = new Button(symbolsComp, SWT.CHECK);
        midArrowsChk.setText("Mid Arrows");
        midArrowsChk.setSelection(cellCfgXML.getSymsArrowMid());
        midArrowsChk.setLayoutData(gd);

        gd = new GridData(SWT.LEFT, SWT.DEFAULT, true, false);
        lowArrowsChk = new Button(symbolsComp, SWT.CHECK);
        lowArrowsChk.setText("Low Arrows");
        lowArrowsChk.setSelection(cellCfgXML.getSymsArrowLow());
        lowArrowsChk.setLayoutData(gd);

    }

    /**
     * Create Hexagon Controls
     */
    private void createHexagonControls() {
        SCANConfigCellXML cellCfgXML = ((CellConfigMgr) scanCfg
                .getAbsConfigMgr(ScanTables.CELL)).getScanCellCfgXML();

        Composite hexComp = new Composite(shell, SWT.NONE);
        GridLayout gl = new GridLayout(4, false);
        gl.verticalSpacing = 10;
        hexComp.setLayout(gl);
        hexComp.setLayoutData(new GridData(SWT.FILL, SWT.DEFAULT, true, false));

        GridData gd = new GridData(SWT.CENTER, SWT.DEFAULT, true, false);
        gd.horizontalSpan = ((GridLayout) hexComp.getLayout()).numColumns;
        Label hexLbl = new Label(hexComp, SWT.NONE);
        hexLbl.setText("-- HEXAGON --");
        hexLbl.setFont(lrgLabelFont);
        hexLbl.setLayoutData(gd);

        gd = new GridData(SWT.RIGHT, SWT.BOTTOM, true, true);
        gd.horizontalSpan = 2;
        Label radiusRngLbl = new Label(hexComp, SWT.RIGHT);
        radiusRngLbl.setText("Radius Range:");
        radiusRngLbl.setFont(smLabelFont);
        radiusRngLbl.setLayoutData(gd);

        double minRaduis = cellCfgXML.getMinRadius();
        double maxRadius = cellCfgXML.getMaxRadius();

        if (minRaduis < 1) {
            minRaduis = 3;
        }

        if (maxRadius < minRaduis || maxRadius > 12) {
            maxRadius = 7;
            minRaduis = 3;
        }

        radiusRngSlider = new TwoValueSliderCanvas(hexComp, 1.0, 11.0, 1.0,
                maxRadius, minRaduis);

        gd = new GridData(unitLabelWidth, SWT.DEFAULT);
        Label radRngUnitLbl = new Label(hexComp, SWT.NONE);
        radRngUnitLbl.setText("nmi");
        radRngUnitLbl.setFont(smLabelFont);
        radRngUnitLbl.setLayoutData(gd);

        gd = new GridData(SWT.RIGHT, SWT.BOTTOM, true, true);
        Label radiusInterpolLbl = new Label(hexComp, SWT.RIGHT);
        radiusInterpolLbl.setText("Radius Interpolation:");
        radiusInterpolLbl.setFont(smLabelFont);
        radiusInterpolLbl.setLayoutData(gd);

        gd = new GridData(SWT.DEFAULT, SWT.BOTTOM, false, true);
        radInterpolCbo = new Combo(hexComp, SWT.DROP_DOWN | SWT.READ_ONLY);
        populateAttributeCombo(cellCfgXML);
        radInterpolCbo.setLayoutData(gd);
        radInterpolCbo.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                radiusInterpolationAction(false);
            }
        });

        radiusInterpolSlider = new TwoValueSliderCanvas(hexComp, 0.0, 100.0,
                1.0, 90.0, 10.0);

        gd = new GridData(SWT.DEFAULT, SWT.BOTTOM, false, true);
        gd.widthHint = unitLabelWidth;
        raduisInterpolUnitLbl = new Label(hexComp, SWT.NONE);
        raduisInterpolUnitLbl.setFont(smLabelFont);
        raduisInterpolUnitLbl.setLayoutData(gd);
    }

    /**
     * Create Arrow Controls
     */
    private void createArrowControls() {
        SCANConfigCellXML cellCfgXML = ((CellConfigMgr) scanCfg
                .getAbsConfigMgr(ScanTables.CELL)).getScanCellCfgXML();

        Composite arrowComp = new Composite(shell, SWT.NONE);
        GridLayout gl = new GridLayout(4, false);
        gl.verticalSpacing = 10;
        arrowComp.setLayout(gl);
        arrowComp
                .setLayoutData(new GridData(SWT.FILL, SWT.DEFAULT, true, false));

        GridData gd = new GridData(SWT.CENTER, SWT.DEFAULT, true, false);
        gd.horizontalSpan = ((GridLayout) arrowComp.getLayout()).numColumns;
        Label arrowLbl = new Label(arrowComp, SWT.NONE);
        arrowLbl.setText("-- ARROW --");
        arrowLbl.setFont(lrgLabelFont);
        arrowLbl.setLayoutData(gd);

        gd = new GridData(SWT.DEFAULT, SWT.CENTER, false, true);
        gd.horizontalIndent = 30;
        fullShaftChk = new Button(arrowComp, SWT.CHECK);
        fullShaftChk.setText("Full Shaft (On/Off)");
        fullShaftChk.setSelection(cellCfgXML.getArrowMode());
        fullShaftChk.setLayoutData(gd);

        gd = new GridData(SWT.DEFAULT, SWT.CENTER, false, true);
        gd.horizontalIndent = 40;
        Label conversionLbl = new Label(arrowComp, SWT.NONE);
        conversionLbl.setText("Conversion:");
        conversionLbl.setFont(smLabelFont);
        conversionLbl.setLayoutData(gd);

        gd = new GridData(50, SWT.DEFAULT);
        conversionSpnr = new Spinner(arrowComp, SWT.BORDER);
        conversionSpnr.setIncrement(1);
        conversionSpnr.setPageIncrement(5);
        conversionSpnr.setMaximum(40);
        conversionSpnr.setMinimum(1);
        conversionSpnr.setLayoutData(gd);

        conversionSpnr.setSelection(cellCfgXML.getArrowConversion());

        gd = new GridData(SWT.DEFAULT, SWT.CENTER, false, true);
        gd.widthHint = unitLabelWidth;
        Label conversionUnitsLbl = new Label(arrowComp, SWT.NONE);
        conversionUnitsLbl.setText("kts/nmi.");
        conversionUnitsLbl.setFont(smLabelFont);
        conversionUnitsLbl.setLayoutData(gd);
    }

    /**
     * Create Clutter Controls
     */
    private void createClutterControls() {
        SCANConfigCellXML cellCfgXML = ((CellConfigMgr) scanCfg
                .getAbsConfigMgr(ScanTables.CELL)).getScanCellCfgXML();

        Composite clutterComp = new Composite(shell, SWT.NONE);
        GridLayout gl = new GridLayout(4, false);
        gl.verticalSpacing = 10;
        clutterComp.setLayout(gl);
        clutterComp.setLayoutData(new GridData(SWT.FILL, SWT.DEFAULT, true,
                false));

        GridData gd = new GridData(SWT.CENTER, SWT.DEFAULT, true, false);
        gd.horizontalSpan = ((GridLayout) clutterComp.getLayout()).numColumns;
        Label clutterLbl = new Label(clutterComp, SWT.NONE);
        clutterLbl.setText("-- CELL IDENTIFIER CLUTTER CONTROL --");
        clutterLbl.setFont(lrgLabelFont);
        clutterLbl.setLayoutData(gd);

        gd = new GridData(SWT.DEFAULT, SWT.BOTTOM, false, true);
        gd.horizontalIndent = 20;
        clutterThreshCbo = new Combo(clutterComp, SWT.DROP_DOWN | SWT.READ_ONLY);
        populateClutterCombo(cellCfgXML);
        clutterThreshCbo.setLayoutData(gd);
        clutterThreshCbo.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                clutterAction();
            }
        });

        gd = new GridData(SWT.DEFAULT, SWT.BOTTOM, false, true);
        gd.horizontalIndent = 30;
        Label thresholdLbl = new Label(clutterComp, SWT.NONE);
        thresholdLbl.setText("Thresholds:");
        thresholdLbl.setFont(smLabelFont);
        thresholdLbl.setLayoutData(gd);

        clutterSlider = new ThreeValueSliderCanvas(clutterComp, 0.0, 100.0,
                1.0, 90.0, 50.0, 10.0, ScanTables.CELL);

        gd = new GridData(SWT.DEFAULT, SWT.BOTTOM, false, true);
        gd.widthHint = unitLabelWidth;
        thresholdUnitLbl = new Label(clutterComp, SWT.NONE);
        thresholdUnitLbl.setFont(smLabelFont);
        thresholdUnitLbl.setLayoutData(gd);
    }

    /**
     * Create Bottom Buttons
     */
    private void createBottomButtons() {
        Composite buttonComp = new Composite(shell, SWT.NONE);
        buttonComp.setLayout(new GridLayout(3, true));
        buttonComp.setLayoutData(new GridData(SWT.FILL, SWT.DEFAULT, true,
                false));

        GridData gd = new GridData(SWT.CENTER, SWT.DEFAULT, true, false);
        gd.widthHint = 80;
        Button okBtn = new Button(buttonComp, SWT.PUSH);
        okBtn.setText("OK");
        okBtn.setLayoutData(gd);
        okBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                applyAction();
                closeDialog();
            }
        });

        gd = new GridData(SWT.CENTER, SWT.DEFAULT, true, false);
        gd.widthHint = 80;
        Button applyBtn = new Button(buttonComp, SWT.PUSH);
        applyBtn.setText("Apply");
        applyBtn.setLayoutData(gd);
        applyBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                applyAction();
            }
        });

        gd = new GridData(SWT.CENTER, SWT.DEFAULT, true, false);
        gd.widthHint = 80;
        Button cancelBtn = new Button(buttonComp, SWT.PUSH);
        cancelBtn.setText("Cancel");
        cancelBtn.setLayoutData(gd);
        cancelBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                closeDialog();
            }
        });
    }

    /**
     * Populate Attribute Combo Box
     * 
     * @param cellCfgXML
     */
    private void populateAttributeCombo(SCANConfigCellXML cellCfgXML) {
        Set<String> keys = attrUnitMap.keySet();

        for (String key : keys) {
            radInterpolCbo.add(key);
        }

        String radVar = cellCfgXML.getRadVar();

        int index = radInterpolCbo.indexOf(radVar);

        radInterpolCbo.select(index);
    }

    /**
     * Populate Clutter Combo Box
     * 
     * @param cellCfgXML
     */
    private void populateClutterCombo(SCANConfigCellXML cellCfgXML) {
        Set<String> keys = attrUnitMap.keySet();

        for (String key : keys) {
            clutterThreshCbo.add(key);
        }

        String clutterCtrl = cellCfgXML.getClutterControl();

        int index = clutterThreshCbo.indexOf(clutterCtrl);

        clutterThreshCbo.select(index);
    }

    /**
     * Radius Interpolation Action
     * 
     * @param startup
     */
    private void radiusInterpolationAction(boolean startup) {
        String attribute = radInterpolCbo.getItem(radInterpolCbo
                .getSelectionIndex());
        raduisInterpolUnitLbl.setText(attrUnitMap.get(attribute));

        SCANConfigCellXML cellCfgXML = ((CellConfigMgr) scanCfg
                .getAbsConfigMgr(ScanTables.CELL)).getScanCellCfgXML();

        SCANAttributesXML attrData = scanCfg.getAttributeXML(ScanTables.CELL,
                attribute);

        double inc = getIncrement(attribute);

        StringBuilder sb = new StringBuilder();

        double radHigh = attrData.getUpper();
        double radLow = attrData.getLow();

        if (startup == true) {
            radHigh = cellCfgXML.getRadHigh();
            radLow = cellCfgXML.getRadLow();
        }

        if (radLow < attrData.getMin()) {
            sb.append("Radius Interpolation:");
            sb.append("The lower value for ").append(attribute)
                    .append(" is less than\n");
            sb.append("the minimum value of the scale.  Setting the lower value to the\n");
            sb.append("lowest value allowed on the scale");

            radLow = attrData.getMin();
        }

        double upperLimit = (Math.round(attrData.getRange()) + attrData
                .getMin());

        System.out.println("upperLimit = " + upperLimit);

        if (radHigh > upperLimit) {
            if (sb.length() > 0) {
                sb.append("\n\n");
            } else {
                sb.append("Radius Interpolation:");
            }

            sb.append("The upper value for ").append(attribute)
                    .append(" is less than\n");
            sb.append("the highest value of the scale.  Setting the upper value to the\n");
            sb.append("highest value allowed on the scale");

            radHigh = attrData.getLow() + Math.round(attrData.getRange());
        }

        radiusInterpolSlider.setValues(attrData.getMin(), attrData.getRange(),
                inc, radHigh, radLow);

        if (sb.length() > 0) {
            MessageBox mb = new MessageBox(shell, SWT.ICON_ERROR | SWT.OK);
            mb.setText("Range Error");
            mb.setMessage(sb.toString());
            mb.open();

            ((CellConfigMgr) scanCfg.getAbsConfigMgr(ScanTables.CELL))
                    .getScanCellCfgXML().setRadLow(radLow);
            ((CellConfigMgr) scanCfg.getAbsConfigMgr(ScanTables.CELL))
                    .getScanCellCfgXML().setRadHigh(radHigh);
        }
    }

    /**
     * Clutter Action
     */
    private void clutterAction() {
        String attribute = clutterThreshCbo.getItem(clutterThreshCbo
                .getSelectionIndex());
        thresholdUnitLbl.setText(attrUnitMap.get(attribute));

        SCANAttributesXML attrData = scanCfg.getAttributeXML(ScanTables.CELL,
                attribute);

        double inc = getIncrement(attribute);

        StringBuilder sb = new StringBuilder();

        double upperVal = attrData.getUpper();
        double lowerVal = attrData.getLow();

        if (attrData.getLow() < attrData.getMin()) {
            sb.append("Clutter Control:");
            sb.append("The lower value for ").append(attribute)
                    .append(" is less than\n");
            sb.append("the minimum value of the scale.  Setting the lower value to the\n");
            sb.append("lowest value allowed on the scale");

            lowerVal = attrData.getMin();
        }

        if (attrData.getUpper() > (Math.round(attrData.getRange()) + attrData
                .getMin())) {
            if (sb.length() > 0) {
                sb.append("\n\n");
            } else {
                sb.append("Clutter Control:");
            }

            sb.append("The upper value for ").append(attribute)
                    .append(" is less than\n");
            sb.append("the highest value of the scale.  Setting the upper value to the\n");
            sb.append("highest value allowed on the scale");

            upperVal = attrData.getLow() + Math.round(attrData.getRange());
        }

        clutterSlider.setValues(attrData.getMin(), attrData.getRange(), inc,
                upperVal, attrData.getMid(), lowerVal);

        if (sb.length() > 0) {
            MessageBox mb = new MessageBox(shell, SWT.ICON_ERROR | SWT.OK);
            mb.setText("Range Error");
            mb.setMessage(sb.toString());
            mb.open();

            scanCfg.setThresholds(ScanTables.CELL, attribute, upperVal,
                    attrData.getMid(), lowerVal);
            if (attribute.equalsIgnoreCase("mdaSR")) {
                scanCfg.setThresholds(ScanTables.MESO, attribute, upperVal,
                        attrData.getMid(), lowerVal);
            }
        }
    }

    /**
     * Get Increment
     * 
     * @param attribute
     * @return
     */
    private double getIncrement(String attribute) {
        if (CELLTable.HSIZE.getColName().compareTo(attribute) == 0) {
            return 0.25;
        } else if (CELLTable.CGRATE.getColName().compareTo(attribute) == 0) {
            return 0.10;
        }

        return 1.0;
    }

    /**
     * Add Separator
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
     * Apply Action
     */
    private void applyAction() {
        /*
         * TODO : set all of the configuration variables
         */
        CellConfigMgr cellCfgMgr = (CellConfigMgr) scanCfg
                .getAbsConfigMgr(ScanTables.CELL);

        boolean symsCircleHigh = highHexagonChk.getSelection();
        boolean symsCircleMid = midHexagonChk.getSelection();
        boolean symsCircleLow = lowHexagonChk.getSelection();
        boolean pastTracks = pastTracksChk.getSelection();
        boolean futureTracks = futureTracksChk.getSelection();
        boolean symsArrowHigh = highArrowsChk.getSelection();
        boolean symsArrowMid = midArrowsChk.getSelection();
        boolean symsArrowLow = lowArrowsChk.getSelection();
        boolean symsIdsHigh = highIdsChk.getSelection();
        boolean symsIdsMid = midIdsChk.getSelection();
        boolean symsIdsLow = lowIdsChk.getSelection();
        int minRadius = (int) Math.round(radiusRngSlider.getLowerValue());
        int maxRadius = (int) Math.round(radiusRngSlider.getUpperValue());
        String radVar = radInterpolCbo.getItem(radInterpolCbo
                .getSelectionIndex());
        double radLow = radiusInterpolSlider.getLowerValue();
        double radHigh = radiusInterpolSlider.getUpperValue();
        boolean arrowMode = fullShaftChk.getSelection();
        int arrowConversion = conversionSpnr.getSelection();
        String attrName = clutterThreshCbo.getItem(clutterThreshCbo
                .getSelectionIndex());
        double upperVal = clutterSlider.getUpperValue();
        double midVal = clutterSlider.getMidValue();
        double lowerVal = clutterSlider.getLowerValue();

        cellCfgMgr.getScanCellCfgXML().setSymsCircleHigh(symsCircleHigh);
        cellCfgMgr.getScanCellCfgXML().setSymsCircleMid(symsCircleMid);
        cellCfgMgr.getScanCellCfgXML().setSymsCircleLow(symsCircleLow);
        cellCfgMgr.getScanCellCfgXML().setPastTracks(pastTracks);
        cellCfgMgr.getScanCellCfgXML().setFutureTracks(futureTracks);
        cellCfgMgr.getScanCellCfgXML().setSymsArrowHigh(symsArrowHigh);
        cellCfgMgr.getScanCellCfgXML().setSymsArrowMid(symsArrowMid);
        cellCfgMgr.getScanCellCfgXML().setSymsArrowLow(symsArrowLow);
        cellCfgMgr.getScanCellCfgXML().setSymsIdHigh(symsIdsHigh);
        cellCfgMgr.getScanCellCfgXML().setSymsIdMid(symsIdsMid);
        cellCfgMgr.getScanCellCfgXML().setSymsIdLow(symsIdsLow);
        cellCfgMgr.getScanCellCfgXML().setMinRadius(minRadius);
        cellCfgMgr.getScanCellCfgXML().setMaxRadius(maxRadius);
        cellCfgMgr.getScanCellCfgXML().setRadVar(radVar);
        cellCfgMgr.getScanCellCfgXML().setRadLow(radLow);
        cellCfgMgr.getScanCellCfgXML().setRadHigh(radHigh);
        cellCfgMgr.getScanCellCfgXML().setArrowMode(arrowMode);
        cellCfgMgr.getScanCellCfgXML().setArrowConversion(arrowConversion);
        cellCfgMgr.getScanCellCfgXML().setClutterControl(attrName);

        scanCfg.setThresholds(ScanTables.CELL, attrName, upperVal, midVal,
                lowerVal);
        if (attrName.equalsIgnoreCase("mdaSR")) {
            scanCfg.setThresholds(ScanTables.MESO, attrName, upperVal, midVal,
                    lowerVal);
        }

        stormCellCB.stormCellUpdated();
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.viz.monitor.scan.commondialogs.ICommonDialogAction#
     * closeDialog()
     */
    @Override
    public void closeDialog() {
        close();
    }

    /**
     * Method that refreshes storm cell when color threshold is updated.
     */
    public void refresh() {
        clutterAction();
    }
}
