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
import org.eclipse.swt.widgets.Spinner;

import com.raytheon.uf.common.monitor.scan.config.SCANConfig;
import com.raytheon.uf.common.monitor.scan.config.SCANConfigEnums.ScanColors;
import com.raytheon.uf.common.monitor.scan.config.UnwarnedConfig;
import com.raytheon.viz.ui.dialogs.CaveSWTDialog;

/**
 * 
 * Displays the Unwarned Alarm control Dialog
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Oct 25, 2009            lvenable    Initial creation
 * 24 Jul 2013  #2143      skorolev    Changes non-blocking dialogs.
 * Aug 15, 2013  2143      mpduff      Remove resize.
 * 04 Dec 2013  #2592      lvenable    Update how the checkboxes are handled
 *                                     (background/foreground colors) since the Redhat
 *                                     6 upgrade causes the check in the checkbox to be
 *                                     colored the same as the background.
 * </pre>
 * 
 * @author lvenable
 * @version 1.0
 */
public class SCANUnwarnedDlg extends CaveSWTDialog implements
        ICommonDialogAction {

    /**
     * Font for the top label.
     */
    private Font topLabelFont;

    /**
     * TOR check boxes
     */
    private Button unwarnedTorChk;

    private Button torTvsChk;

    private Button torMaxDbzChk;

    private Button torMesoStRankChk;

    private Button torMaxVilChk;

    /**
     * TOR spinner controls.
     */
    private Spinner torMaxDbzSpnr;

    private Spinner torMesoStRankSpnr;

    private Spinner torMaxVilSpnr;

    /**
     * Unwarned SVR check box.
     */
    private Button unwarnedSvrChk;

    /**
     * SVR TVS check box.
     */
    private Button svrTvsChk;

    /**
     * SVR Max DBZ check box.
     */
    private Button svrMaxDbzChk;

    /**
     * SVR MESO stRank check box.
     */
    private Button svrMesoStRankChk;

    /**
     * SVR Max VIL check box.
     */
    private Button svrMaxVilChk;

    /**
     * SVR Hail Size check box.
     */
    private Button svrHailSizeChk;

    /**
     * SVR Max DBZ spinner control.
     */
    private Spinner svrMaxDbzSpnr;

    /**
     * SVR MESO stRank spinner control.
     */
    private Spinner svrMesoStRankSpnr;

    /**
     * SVR Max VIL spinner control.
     */
    private Spinner svrMaxVilSpnr;

    /**
     * SVR Hail Size spinner control.
     */
    private Spinner svrHailSizeSpnr;

    /**
     * Spinner width.
     */
    private final int spinnerWidth = 60;

    private StringBuilder infoText;

    /**
     * Constructor.
     * 
     * @param parentShell
     *            Parent shell.
     */
    public SCANUnwarnedDlg(Shell parentShell) {
        super(parentShell, SWT.DIALOG_TRIM, CAVE.DO_NOT_BLOCK);
        setText("Unwarned Alarm Control");
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
        topLabelFont.dispose();
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
        setReturnValue(false);

        // Initialize all of the controls and layouts
        topLabelFont = new Font(getDisplay(), "Sans", 12, SWT.BOLD);
        createControls();
        addSeparator(shell);
        createBottomButtons();

        enableTorControls();
        enableSvrControls();
    }

    /**
     * Create the check box and spinner controls.
     */
    private void createControls() {
        UnwarnedConfig cfgData = SCANConfig.getInstance().getUnwarnedConfig();

        Composite controlComp = new Composite(shell, SWT.NONE);
        GridLayout gl = new GridLayout(4, false);
        gl.verticalSpacing = 10;
        controlComp.setLayout(gl);
        controlComp.setLayoutData(new GridData(SWT.FILL, SWT.DEFAULT, true,
                false));

        GridData gd = new GridData(SWT.CENTER, SWT.DEFAULT, true, false);
        gd.horizontalSpan = 4;
        Label topLbl = new Label(controlComp, SWT.NONE);
        topLbl.setText("SCAN Unwarned County Alarm functionality:");
        topLbl.setFont(topLabelFont);
        topLbl.setLayoutData(gd);

        gd = new GridData(SWT.CENTER, SWT.DEFAULT, true, false);
        gd.horizontalSpan = 4;

        createInfoString();
        Label msgLbl = new Label(controlComp, SWT.NONE);
        msgLbl.setText(infoText.toString());
        msgLbl.setLayoutData(gd);

        addSeparator(controlComp);

        /*
         * TOR Controls
         */
        gd = new GridData(SWT.CENTER, SWT.DEFAULT, true, false);
        gd.horizontalSpan = 4;
        gd.verticalIndent = 20;
        unwarnedTorChk = createCheckLabelColor(controlComp, gd, "Unwarned TOR");
        unwarnedTorChk.setSelection(cfgData.getUnwarnedTor());
        unwarnedTorChk.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                enableTorControls();
            }
        });

        gd = new GridData(SWT.LEFT, SWT.CENTER, false, true);
        gd.horizontalSpan = 2;
        torTvsChk = new Button(controlComp, SWT.CHECK);
        torTvsChk.setText("TVS");
        torTvsChk.setSelection(cfgData.getTorTvs());
        torTvsChk.setLayoutData(gd);

        gd = new GridData();
        torMaxDbzChk = new Button(controlComp, SWT.CHECK);
        torMaxDbzChk.setText("Max Dbz:");
        torMaxDbzChk.setSelection(cfgData.getTorMaxDbz());

        gd = new GridData(spinnerWidth, SWT.DEFAULT);
        torMaxDbzSpnr = new Spinner(controlComp, SWT.BORDER);
        torMaxDbzSpnr.setMinimum(0);
        torMaxDbzSpnr.setMaximum(999999);
        torMaxDbzSpnr.setLayoutData(gd);
        torMaxDbzSpnr.setSelection(cfgData.getTorMaxDbzVal());

        torMesoStRankChk = new Button(controlComp, SWT.CHECK);
        torMesoStRankChk.setText("Meso stRank:");
        torMesoStRankChk.setSelection(cfgData.getTorMesoStRank());

        gd = new GridData(spinnerWidth, SWT.DEFAULT);
        torMesoStRankSpnr = new Spinner(controlComp, SWT.BORDER);
        torMesoStRankSpnr.setMinimum(0);
        torMesoStRankSpnr.setMaximum(999999);
        torMesoStRankSpnr.setLayoutData(gd);
        torMesoStRankSpnr.setSelection(cfgData.getTorMesoStRankVal());

        torMaxVilChk = new Button(controlComp, SWT.CHECK);
        torMaxVilChk.setText("Max VIL:");
        torMaxVilChk.setSelection(cfgData.getTorMaxVil());

        gd = new GridData(spinnerWidth, SWT.DEFAULT);
        torMaxVilSpnr = new Spinner(controlComp, SWT.BORDER);
        torMaxVilSpnr.setMinimum(0);
        torMaxVilSpnr.setMaximum(999999);
        torMaxVilSpnr.setLayoutData(gd);
        torMaxVilSpnr.setSelection(cfgData.getTorMaxVilVal());

        /*
         * SVR Controls
         */
        gd = new GridData(SWT.CENTER, SWT.DEFAULT, true, false);
        gd.horizontalSpan = 4;
        gd.verticalIndent = 20;
        unwarnedSvrChk = createCheckLabelColor(controlComp, gd, "Unwarned SVR");
        unwarnedSvrChk.setSelection(cfgData.getUnwarnedSvr());
        unwarnedSvrChk.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                enableSvrControls();
            }
        });

        gd = new GridData(SWT.LEFT, SWT.CENTER, false, true);
        gd.horizontalSpan = 2;
        svrTvsChk = new Button(controlComp, SWT.CHECK);
        svrTvsChk.setText("TVS");
        svrTvsChk.setSelection(cfgData.getSvrTvs());
        svrTvsChk.setLayoutData(gd);

        svrMaxDbzChk = new Button(controlComp, SWT.CHECK);
        svrMaxDbzChk.setText("Max Dbz:");
        svrMaxDbzChk.setSelection(cfgData.getSvrMaxDbz());

        gd = new GridData(spinnerWidth, SWT.DEFAULT);
        svrMaxDbzSpnr = new Spinner(controlComp, SWT.BORDER);
        svrMaxDbzSpnr.setMinimum(0);
        svrMaxDbzSpnr.setMaximum(999999);
        svrMaxDbzSpnr.setLayoutData(gd);
        svrMaxDbzSpnr.setSelection(cfgData.getSvrMaxDbzVal());

        svrMesoStRankChk = new Button(controlComp, SWT.CHECK);
        svrMesoStRankChk.setText("Meso stRank:");
        svrMesoStRankChk.setSelection(cfgData.getSvrMesoStRank());

        gd = new GridData(spinnerWidth, SWT.DEFAULT);
        svrMesoStRankSpnr = new Spinner(controlComp, SWT.BORDER);
        svrMesoStRankSpnr.setMinimum(0);
        svrMesoStRankSpnr.setMaximum(999999);
        svrMesoStRankSpnr.setLayoutData(gd);
        svrMesoStRankSpnr.setSelection(cfgData.getSvrMesoStRankVal());

        svrMaxVilChk = new Button(controlComp, SWT.CHECK);
        svrMaxVilChk.setText("Max VIL:");
        svrMaxVilChk.setSelection(cfgData.getSvrMaxVil());

        gd = new GridData(spinnerWidth, SWT.DEFAULT);
        svrMaxVilSpnr = new Spinner(controlComp, SWT.BORDER);
        svrMaxVilSpnr.setMinimum(0);
        svrMaxVilSpnr.setMaximum(999999);
        svrMaxVilSpnr.setLayoutData(gd);
        svrMaxVilSpnr.setSelection(cfgData.getSvrMaxVilVal());

        gd = new GridData(SWT.RIGHT, SWT.CENTER, false, true);
        gd.horizontalSpan = 2;
        svrHailSizeChk = new Button(controlComp, SWT.CHECK);
        svrHailSizeChk.setText("Hail Size:");
        svrHailSizeChk.setSelection(cfgData.getHailSize());
        svrHailSizeChk.setLayoutData(gd);

        gd = new GridData(spinnerWidth, SWT.DEFAULT);
        svrHailSizeSpnr = new Spinner(controlComp, SWT.BORDER);
        svrHailSizeSpnr.setDigits(2);
        svrHailSizeSpnr.setMinimum(0);
        svrHailSizeSpnr.setMaximum(1000);
        svrHailSizeSpnr.setLayoutData(gd);
        svrHailSizeSpnr.setSelection((int) (cfgData.getHailSizeVal() * 100));
    }

    /**
     * Create the buttons at the bottom of the display.
     */
    private void createBottomButtons() {
        Composite buttonComp = new Composite(shell, SWT.NONE);
        buttonComp.setLayout(new GridLayout(2, true));
        buttonComp.setLayoutData(new GridData(SWT.FILL, SWT.DEFAULT, true,
                false));

        GridData gd = new GridData(SWT.RIGHT, SWT.DEFAULT, true, false);
        gd.widthHint = 80;
        Button okBtn = new Button(buttonComp, SWT.PUSH);
        okBtn.setText("OK");
        okBtn.setLayoutData(gd);
        okBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                if (verifyEntries() == false) {
                    return;
                }
                setReturnValue(true);

                closeDialog();
            }
        });

        gd = new GridData(SWT.LEFT, SWT.DEFAULT, true, false);
        gd.widthHint = 80;
        Button cancelBtn = new Button(buttonComp, SWT.PUSH);
        cancelBtn.setText("Cancel");
        cancelBtn.setLayoutData(gd);
        cancelBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                setReturnValue(false);
                closeDialog();
            }
        });
    }

    /**
     * Add a horizontal separator line to the display.
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
     * Enable/Disable the TOR controls based on the selection of the Unwarned
     * TOR check box.
     */
    private void enableTorControls() {
        torTvsChk.setEnabled(unwarnedTorChk.getSelection());
        torMaxDbzChk.setEnabled(unwarnedTorChk.getSelection());
        torMesoStRankChk.setEnabled(unwarnedTorChk.getSelection());
        torMaxVilChk.setEnabled(unwarnedTorChk.getSelection());

        torMaxDbzSpnr.setEnabled(unwarnedTorChk.getSelection());
        torMesoStRankSpnr.setEnabled(unwarnedTorChk.getSelection());
        torMaxVilSpnr.setEnabled(unwarnedTorChk.getSelection());

        if (unwarnedTorChk.getSelection() == false) {
            torTvsChk.setSelection(false);
            torMaxDbzChk.setSelection(false);
            torMesoStRankChk.setSelection(false);
            torMaxVilChk.setSelection(false);
        }
    }

    /**
     * Enable/Disable the SVR controls based on the selection of the Unwarned
     * SVR check box.
     */
    private void enableSvrControls() {
        svrTvsChk.setEnabled(unwarnedSvrChk.getSelection());
        svrMaxDbzChk.setEnabled(unwarnedSvrChk.getSelection());
        svrMesoStRankChk.setEnabled(unwarnedSvrChk.getSelection());
        svrMaxVilChk.setEnabled(unwarnedSvrChk.getSelection());
        svrHailSizeChk.setEnabled(unwarnedSvrChk.getSelection());

        svrMaxDbzSpnr.setEnabled(unwarnedSvrChk.getSelection());
        svrMesoStRankSpnr.setEnabled(unwarnedSvrChk.getSelection());
        svrMaxVilSpnr.setEnabled(unwarnedSvrChk.getSelection());
        svrHailSizeSpnr.setEnabled(unwarnedSvrChk.getSelection());

        if (unwarnedSvrChk.getSelection() == false) {
            svrTvsChk.setSelection(false);
            svrMaxDbzChk.setSelection(false);
            svrMesoStRankChk.setSelection(false);
            svrMaxVilChk.setSelection(false);
            svrHailSizeChk.setSelection(false);
        }
    }

    /**
     * Verify there are valid selections on the display.
     * 
     * @return True if there are valid selections.
     */
    private boolean verifyEntries() {
        /*
         * Verify the TOR entries.
         */
        if (unwarnedTorChk.getSelection() == true) {
            if (torTvsChk.getSelection() == false
                    && torMaxDbzChk.getSelection() == false
                    && torMaxVilChk.getSelection() == false
                    && torMesoStRankChk.getSelection() == false) {
                displayMessage("Select at least one attribute for Unwarned TOR");
                return false;
            }
        }

        /*
         * Verify the SVR entries.
         */
        if (unwarnedSvrChk.getSelection() == true) {
            if (svrTvsChk.getSelection() == false
                    && svrMaxDbzChk.getSelection() == false
                    && svrMaxVilChk.getSelection() == false
                    && svrMesoStRankChk.getSelection() == false
                    && svrHailSizeChk.getSelection() == false) {
                displayMessage("Select at least one attribute for Unwarned SVR");
                return false;
            }
        }

        /*
         * Update the unwarned configuration data.
         */
        SCANConfig scanCfg = SCANConfig.getInstance();

        scanCfg.getUnwarnedConfig().setUnwarnedTor(
                unwarnedTorChk.getSelection());
        scanCfg.getUnwarnedConfig().setTorTvs(torTvsChk.getSelection());
        scanCfg.getUnwarnedConfig().setTorMaxDbz(torMaxDbzChk.getSelection());
        scanCfg.getUnwarnedConfig().setTorMaxDbzVal(
                torMaxDbzSpnr.getSelection());
        scanCfg.getUnwarnedConfig().setTorMesoStRank(
                torMesoStRankChk.getSelection());
        scanCfg.getUnwarnedConfig().setTorMesoStRankVal(
                torMesoStRankSpnr.getSelection());
        scanCfg.getUnwarnedConfig().setTorMaxVil(torMaxVilChk.getSelection());
        scanCfg.getUnwarnedConfig().setTorMaxVilVal(
                torMaxVilSpnr.getSelection());

        scanCfg.getUnwarnedConfig().setUnwarnedSvr(
                unwarnedSvrChk.getSelection());
        scanCfg.getUnwarnedConfig().setSvrTvs(svrTvsChk.getSelection());
        scanCfg.getUnwarnedConfig().setSvrMaxDbz(svrMaxDbzChk.getSelection());
        scanCfg.getUnwarnedConfig().setSvrMaxDbzVal(
                svrMaxDbzSpnr.getSelection());
        scanCfg.getUnwarnedConfig().setSvrMaxVil(svrMaxVilChk.getSelection());
        scanCfg.getUnwarnedConfig().setSvrMaxVilVal(
                svrMaxVilSpnr.getSelection());
        scanCfg.getUnwarnedConfig().setSvrMesoStRank(
                svrMesoStRankChk.getSelection());
        scanCfg.getUnwarnedConfig().setSvrMesoStRankVal(
                svrMesoStRankSpnr.getSelection());

        scanCfg.getUnwarnedConfig().setHailSize(svrHailSizeChk.getSelection());
        scanCfg.getUnwarnedConfig().setHailSizeVal(
                svrHailSizeSpnr.getSelection() / 100.0);

        return true;
    }

    /**
     * Display a pop-up message box.
     * 
     * @param message
     *            Message to be displayed.
     */
    private void displayMessage(String message) {
        MessageBox mb = new MessageBox(shell, SWT.ICON_ERROR | SWT.OK);
        mb.setText("Invalid Selection");
        mb.setMessage(message);
        mb.open();
    }

    /**
     * Create Info String.
     */
    private void createInfoString() {
        infoText = new StringBuilder();

        infoText.append("SCAN identifies those storm cells that contain a\n");
        infoText.append("Tornado Vortex Signature (TVS) and to some extent\n");
        infoText.append("severe weather (based on various storm cell parameters).\n");
        infoText.append("Now SCAN can determine which storm cells currently have\n");
        infoText.append("an active TOR or SVR warning and which do not. For\n");
        infoText.append("those that do not, the SCAN user can set various storm\n");
        infoText.append("cell parameter thresholds (see below). If these thresholds\n");
        infoText.append("are met or exceeded and no TOR and/or SVR is in effect\n");
        infoText.append("in the polygon  where the cell is located, an Unwarned Storm\n");
        infoText.append("Alarm will be issued.\n\n");
        infoText.append("To turn this functionality on for TOR and/or SVR warnings,\n");
        infoText.append("simply click the toggle below on and then check and specify\n");
        infoText.append("the thresholds you would like be used in order to issue a TOR\n");
        infoText.append("and/or SVR Unwarned Storm Cell Alarm. You will know that\n");
        infoText.append("an Unwarned Storm Alarm has been issued when the storm\n");
        infoText.append("cell identifier in the Storm Cell Table changes color to\n");
        infoText.append("magenta for TOR warnings and yellow for SVR warnings.");
    }

    /**
     * Create a composite that contains a checkbox with no text and a colored
     * label.
     * 
     * @param parentComp
     *            Parent composite.
     * @param gd
     *            GridData used for the composite.
     * @param labelText
     *            Text for the label.
     * @return The checkbox that is created.
     */
    private Button createCheckLabelColor(Composite parentComp, GridData gd,
            String labelText) {

        GridLayout gl = new GridLayout(2, false);
        gl.marginHeight = 2;
        gl.marginWidth = 2;
        gl.horizontalSpacing = 0;

        Composite chkLblComp = new Composite(parentComp, SWT.NONE);
        chkLblComp.setLayout(gl);
        chkLblComp.setLayoutData(gd);

        gd = new GridData(18, SWT.DEFAULT);
        Button chkBox = new Button(chkLblComp, SWT.CHECK);
        chkBox.setLayoutData(gd);

        Label lbl = new Label(chkLblComp, SWT.NONE);
        lbl.setBackground(SCANConfig.getInstance().getScanColor(
                ScanColors.Unwarned));
        lbl.setForeground(getDisplay().getSystemColor(SWT.COLOR_WHITE));
        lbl.setText(" " + labelText);

        return chkBox;
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
}
