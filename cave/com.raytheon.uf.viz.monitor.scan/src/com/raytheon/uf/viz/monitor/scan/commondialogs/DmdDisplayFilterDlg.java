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

import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.graphics.Font;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Layout;
import org.eclipse.swt.widgets.MessageBox;
import org.eclipse.swt.widgets.Shell;

import com.raytheon.uf.common.monitor.scan.config.SCANConfigEnums.DMDTable;
import com.raytheon.uf.common.monitor.scan.config.SCANConfigEnums.ScanTables;
import com.raytheon.uf.common.monitor.scan.xml.SCANAttributesXML;
import com.raytheon.uf.common.monitor.scan.xml.SCANConfigDmdXML;
import com.raytheon.uf.viz.monitor.scan.config.DmdConfigMgr;
import com.raytheon.uf.viz.monitor.scan.config.SCANConfig;
import com.raytheon.viz.ui.dialogs.CaveSWTDialog;

/**
 * This class displays the DMD Display Filter Control dialog.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Nov 12, 2009 3039       lvenable     Initial creation.
 * 24 Jul 2013 #2143       skorolev     Changes for non-blocking dialog.
 * Aug 15, 2013  2143      mpduff       Remove resize.
 * </pre>
 * 
 * @author lvenable
 * @version 1.0
 */
public class DmdDisplayFilterDlg extends CaveSWTDialog implements
        ICommonDialogAction {

    /**
     * Large label font.
     */
    private Font lrgLabelFont;

    /**
     * Small label font.
     */
    private Font smLabelFont;

    /**
     * SCAN configuration.
     */
    private SCANConfig scanCfg;

    /**
     * Display past and forecast tracks on D2D check box.
     */
    private Button displayTracksChk;

    /**
     * Display icons for overlapping circulation features.
     */
    private Button displayOverlapChk;

    /**
     * Display threshold filter slider control.
     */
    private ThreeValueSliderCanvas threshFilterSlider;

    /**
     * Display filter update callback.
     */
    private final IDisplayFilterUpdate displayFilterUpdateCB;

    /**
     * Constructor.
     * 
     * @param parentShell
     *            Parent shell.
     * @param cb
     *            Callback that is called when changes are made.
     */
    public DmdDisplayFilterDlg(Shell parentShell, IDisplayFilterUpdate cb) {
        super(parentShell, SWT.DIALOG_TRIM, CAVE.DO_NOT_BLOCK);
        setText("DMD Display Filter Control");

        displayFilterUpdateCB = cb;
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
        initData();
        initializeComponents();
    }

    /**
     * Initialize the data.
     */
    private void initData() {
        scanCfg = SCANConfig.getInstance();
        lrgLabelFont = new Font(getDisplay(), "Monospaced", 12, SWT.BOLD);
        smLabelFont = new Font(getDisplay(), "Sans", 10, SWT.BOLD);
    }

    /**
     * Initialize the compoenets on the display.
     */
    private void initializeComponents() {
        createTracksAndOverlapControls();
        addSeparator(shell);
        createDisplayThresholdControls();
        addSeparator(shell);
        createBottomButtons();

        setSliderValues();
    }

    /**
     * Create the tracks and overlap controls.
     */
    private void createTracksAndOverlapControls() {
        SCANConfigDmdXML dmdCfgXML = ((DmdConfigMgr) scanCfg
                .getAbsConfigMgr(ScanTables.DMD)).getScanDmdCfgXML();

        Composite tracksOverlapComp = new Composite(shell, SWT.NONE);
        GridLayout gl = new GridLayout(1, true);
        gl.verticalSpacing = 10;
        tracksOverlapComp.setLayout(gl);
        tracksOverlapComp.setLayoutData(new GridData(SWT.FILL, SWT.DEFAULT,
                true, false));

        GridData gd = new GridData(SWT.CENTER, SWT.DEFAULT, true, false);
        Label symbolsLbl = new Label(tracksOverlapComp, SWT.NONE);
        symbolsLbl.setText("-- TRACKS & OVERLAP --");
        symbolsLbl.setFont(lrgLabelFont);
        symbolsLbl.setLayoutData(gd);

        gd = new GridData(SWT.CENTER, SWT.DEFAULT, true, false);
        gd.widthHint = 350;
        displayTracksChk = new Button(tracksOverlapComp, SWT.CHECK);
        displayTracksChk.setText("Display Past and Forecast Tracks on D-2D");
        displayTracksChk.setSelection(dmdCfgXML.getTrackOpt());
        displayTracksChk.setLayoutData(gd);

        gd = new GridData(SWT.CENTER, SWT.DEFAULT, true, false);
        gd.widthHint = 350;
        displayOverlapChk = new Button(tracksOverlapComp, SWT.CHECK);
        displayOverlapChk
                .setText("Display Icons for Overlapping Circulation Features");
        displayOverlapChk.setSelection(dmdCfgXML.getOverlapOpt());
        displayOverlapChk.setLayoutData(gd);
    }

    /**
     * Create the display threshold controls.
     */
    private void createDisplayThresholdControls() {
        Composite threshFilterComp = new Composite(shell, SWT.NONE);
        GridLayout gl = new GridLayout(2, false);
        gl.verticalSpacing = 10;
        threshFilterComp.setLayout(gl);
        threshFilterComp.setLayoutData(new GridData(SWT.FILL, SWT.DEFAULT,
                true, false));

        GridData gd = new GridData(SWT.CENTER, SWT.DEFAULT, true, false);
        gd.horizontalSpan = ((GridLayout) threshFilterComp.getLayout()).numColumns;
        Label threshFilterLbl = new Label(threshFilterComp, SWT.NONE);
        threshFilterLbl.setText("-- DISPLAY THRESHOLD FILTER --");
        threshFilterLbl.setFont(lrgLabelFont);
        threshFilterLbl.setLayoutData(gd);

        Label srRankLbl = new Label(threshFilterComp, SWT.NONE);
        srRankLbl.setText("Strength Rank Threshold\nfor D-2D Display");
        srRankLbl.setFont(smLabelFont);

        threshFilterSlider = new ThreeValueSliderCanvas(threshFilterComp, 0.0,
                100.0, 1.0, 90.0, 50.0, 10.0, ScanTables.DMD);

        gd = new GridData(SWT.CENTER, SWT.DEFAULT, true, false);
        gd.verticalIndent = 15;
        gd.horizontalSpan = ((GridLayout) threshFilterComp.getLayout()).numColumns;
        Label notesLbl = new Label(threshFilterComp, SWT.NONE);
        notesLbl.setLayoutData(gd);
        notesLbl.setText("Notes: These settings will filter icons from the D-2D display, but the circulation\n"
                + "features will remain in the DMD table.");
        notesLbl.setFont(smLabelFont);
    }

    /**
     * Create the buttons at the bottom of the display.
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
                close();
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
     * Set the threshold/filter slider control values.
     */
    private void setSliderValues() {
        SCANAttributesXML attrData = scanCfg.getAttributeXML(ScanTables.DMD,
                DMDTable.STRANK.getColName());

        StringBuilder sb = new StringBuilder();

        double upperVal = attrData.getUpper();
        double lowerVal = attrData.getLow();

        if (attrData.getLow() < attrData.getMin()) {
            sb.append("DMD Threshold Filter:");
            sb.append("The lower value for ")
                    .append(DMDTable.STRANK.getColName())
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
                sb.append("DMD Threshold Filter:");
            }

            sb.append("The upper value for ")
                    .append(DMDTable.STRANK.getColName())
                    .append(" is less than\n");
            sb.append("the highest value of the scale.  Setting the upper value to the\n");
            sb.append("highest value allowed on the scale");

            upperVal = attrData.getLow() + Math.round(attrData.getRange());
        }

        threshFilterSlider.setValues(attrData.getMin(), attrData.getRange(),
                1.0, upperVal, attrData.getMid(), lowerVal);

        if (sb.length() > 0) {
            MessageBox mb = new MessageBox(shell, SWT.ICON_ERROR | SWT.OK);
            mb.setText("Range Error");
            mb.setMessage(sb.toString());
            mb.open();

            scanCfg.setThresholds(ScanTables.DMD, DMDTable.STRANK.getColName(),
                    upperVal, attrData.getMid(), lowerVal);
        }
    }

    /**
     * Add a separator to the display.
     * 
     * @param parentComp
     *            Parent composite.
     */
    private void addSeparator(Composite parentComp) {
        GridLayout gl = (GridLayout) parentComp.getLayout();

        GridData gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        gd.horizontalSpan = gl.numColumns;
        Label sepLbl = new Label(parentComp, SWT.SEPARATOR | SWT.HORIZONTAL);
        sepLbl.setLayoutData(gd);
    }

    /**
     * Method called when the Apply button is pressed.
     */
    private void applyAction() {
        DmdConfigMgr dmdCfgMgr = (DmdConfigMgr) scanCfg
                .getAbsConfigMgr(ScanTables.DMD);

        boolean track = displayTracksChk.getSelection();
        boolean overlap = displayOverlapChk.getSelection();

        double upperVal = threshFilterSlider.getUpperValue();
        double midVal = threshFilterSlider.getMidValue();
        double lowerVal = threshFilterSlider.getLowerValue();

        dmdCfgMgr.getScanDmdCfgXML().setTrackOpt(track);
        dmdCfgMgr.getScanDmdCfgXML().setOverlapOpt(overlap);

        scanCfg.setThresholds(ScanTables.DMD, DMDTable.STRANK.getColName(),
                upperVal, midVal, lowerVal);

        scanCfg.setupDmdClassificationMap(upperVal);

        displayFilterUpdateCB.displayFilterUpdated();
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
     * Method that refreshes threshold filter when color threshold is updated.
     */
    public void refresh() {
        setSliderValues();
    }
}
