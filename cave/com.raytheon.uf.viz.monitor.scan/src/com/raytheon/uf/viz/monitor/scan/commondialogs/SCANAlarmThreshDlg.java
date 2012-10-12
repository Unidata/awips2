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

import java.util.Set;

import org.eclipse.swt.SWT;
import org.eclipse.swt.events.FocusAdapter;
import org.eclipse.swt.events.FocusEvent;
import org.eclipse.swt.events.KeyAdapter;
import org.eclipse.swt.events.KeyEvent;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Combo;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Layout;
import org.eclipse.swt.widgets.MessageBox;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Text;

import com.raytheon.uf.common.monitor.scan.config.AbsConfigMgr;
import com.raytheon.uf.common.monitor.scan.config.SCANConfig;
import com.raytheon.uf.common.monitor.scan.config.SCANConfigEnums.ScanTables;
import com.raytheon.uf.viz.monitor.scan.ScanMonitor;
import com.raytheon.uf.viz.monitor.scan.tables.AbstractTableDlg;
import com.raytheon.uf.viz.monitor.scan.tables.SCANAlarmAlertManager;
import com.raytheon.uf.viz.monitor.scan.tables.SCANCellTableDlg;
import com.raytheon.uf.viz.monitor.scan.tables.SCANAlarmAlertManager.AlarmType;
import com.raytheon.uf.viz.monitor.scan.tables.SCANAlarmAlertManager.AlertedAlarms;
import com.raytheon.viz.ui.dialogs.CaveSWTDialog;

public class SCANAlarmThreshDlg extends CaveSWTDialog implements
        ICommonDialogAction {

    private ScanTables scanTable;

    private SCANConfig scanCfg;

    private Combo attributeCbo;

    private Button disableAllAlarmChk;

    private Button absValueRdo;

    private Button rateOfChangeRdo;

    private Label selectedChoiceLbl;

    private Text valueTF;

    private Label unitsLbl;

    private final String absValStr = "Absolute Value";

    private final String rocStr = "Rate of Change";

    private Button bellChk;

    private SCANAlarmAlertManager mgr;

    private AlarmType type;

    private String prevAttr;

    private String site = null;

    public SCANAlarmThreshDlg(String site, Shell parentShell,
            ScanTables scanTable) {
        super(parentShell);
        if (scanTable == ScanTables.CELL) {
            setText("CELL Alarm Thresh");
        } else {
            setText("DMD Alarm Thresh");
        }

        mgr = SCANAlarmAlertManager.getInstance(site);
        this.scanTable = scanTable;
        this.site = site;
    }

    @Override
    protected Layout constructShellLayout() {
        // Create the main layout for the shell.
        GridLayout mainLayout = new GridLayout(1, false);
        mainLayout.marginWidth = 2;
        mainLayout.verticalSpacing = 10;
        return mainLayout;
    }

    @Override
    protected void initializeComponents(Shell shell) {
        // Initialize all of the controls and layouts
        scanCfg = SCANConfig.getInstance();
        createControls();
        addSeparator(shell);
        createBottomButtons();

        attributeChanged();
    }

    private void createControls() {
        Composite controlComp = new Composite(shell, SWT.NONE);
        controlComp.setLayout(new GridLayout(3, false));
        controlComp.setLayoutData(new GridData(SWT.FILL, SWT.DEFAULT, true,
                false));

        GridData gd = new GridData(SWT.FILL, SWT.CENTER, false, true);
        Label attrLbl = new Label(controlComp, SWT.RIGHT);
        attrLbl.setText("Attribute:");
        attrLbl.setLayoutData(gd);

        gd = new GridData();
        attributeCbo = new Combo(controlComp, SWT.DROP_DOWN | SWT.READ_ONLY);
        populateAttributeCombo();
        attributeCbo.setLayoutData(gd);
        attributeCbo.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                attributeChanged();
            }
        });

        gd = new GridData();
        gd.horizontalIndent = 15;
        disableAllAlarmChk = new Button(controlComp, SWT.CHECK);
        disableAllAlarmChk.setText("Disable All Alarms");
        disableAllAlarmChk.setLayoutData(gd);
        disableAllAlarmChk.setSelection(scanCfg.getAlarmsDisabled(scanTable));
        disableAllAlarmChk.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                boolean alarmDisabled = disableAllAlarmChk.getSelection();

                scanCfg.setAlarmsDisabled(scanTable, alarmDisabled);
                disableSelectedAttributeAlarm(alarmDisabled);
            }
        });

        gd = new GridData(SWT.FILL, SWT.CENTER, false, true);
        gd.verticalIndent = 10;
        Label alarmChoiceLbl = new Label(controlComp, SWT.RIGHT);
        alarmChoiceLbl.setText("Alarm Choice:");
        alarmChoiceLbl.setLayoutData(gd);

        gd = new GridData();
        gd.verticalIndent = 10;
        gd.horizontalSpan = 2;
        absValueRdo = new Button(controlComp, SWT.RADIO);
        absValueRdo.setText(absValStr);
        absValueRdo.setSelection(true);
        absValueRdo.setLayoutData(gd);
        absValueRdo.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                if (absValueRdo.getSelection() == true) {
                    alarmChoiceChanged();
                }
            }
        });

        // Filler label
        new Label(controlComp, SWT.NONE);

        gd = new GridData();
        gd.horizontalSpan = 2;
        rateOfChangeRdo = new Button(controlComp, SWT.RADIO);
        rateOfChangeRdo.setText(rocStr);
        rateOfChangeRdo.setLayoutData(gd);
        if (scanTable == ScanTables.DMD) {
            rateOfChangeRdo.setEnabled(false);
        }
        rateOfChangeRdo.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {

            }
        });

        gd = new GridData(SWT.FILL, SWT.CENTER, false, true);
        gd.verticalIndent = 10;
        gd.widthHint = 120;
        selectedChoiceLbl = new Label(controlComp, SWT.RIGHT);
        selectedChoiceLbl.setText(absValStr + ":");
        selectedChoiceLbl.setLayoutData(gd);

        gd = new GridData(50, SWT.DEFAULT);
        gd.verticalIndent = 10;
        valueTF = new Text(controlComp, SWT.BORDER);
        valueTF.setLayoutData(gd);
        valueTF.addFocusListener(new FocusAdapter() {
            @Override
            public void focusLost(FocusEvent e) {
                validateAndSetValue();
            }
        });

        valueTF.addKeyListener(new KeyAdapter() {
            @Override
            public void keyPressed(KeyEvent ke) {
                if (ke.keyCode == SWT.KEYPAD_CR || ke.keyCode == SWT.CR) {
                    validateAndSetValue();
                }
            }
        });

        gd = new GridData(80, SWT.DEFAULT);
        gd.verticalIndent = 10;
        unitsLbl = new Label(controlComp, SWT.NONE);
        unitsLbl.setLayoutData(gd);
    }

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
                alarmChoiceChanged();
                String columnName = attributeCbo.getText();
                mgr.clearAlertedAlarms(site, scanTable);
				AbstractTableDlg tableDlg = ScanMonitor.getInstance()
						.getDialog(scanTable, site);
				tableDlg.updateThresh(columnName);
                if (mgr.getAlertedAlarmCount(site, scanTable) == 0) {
                	tableDlg.turnOffAlarm();
                } else {
                	tableDlg.turnOnAlarm();
                }
                shell.dispose();
            }
        });

        gd = new GridData(SWT.LEFT, SWT.CENTER, true, true);
        bellChk = new Button(buttonComp, SWT.CHECK);
        bellChk.setText("Bell");
        bellChk.setLayoutData(gd);
        bellChk.setSelection(scanCfg.getAlarmBell(scanTable));
        bellChk.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                scanCfg.setAlarmBell(scanTable, bellChk.getSelection());
            }
        });
    }

    private void addSeparator(Composite parentComp) {
        GridLayout gl = (GridLayout) parentComp.getLayout();

        GridData gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        gd.horizontalSpan = gl.numColumns;
        Label sepLbl = new Label(parentComp, SWT.SEPARATOR | SWT.HORIZONTAL);
        sepLbl.setLayoutData(gd);
    }

    private void populateAttributeCombo() {
        AbsConfigMgr absCfgMgr = scanCfg.getAbsConfigMgr(scanTable);

        String[] attributes = absCfgMgr.getAlarmAttributes();

        for (String str : attributes) {
            attributeCbo.add(str);
        }

        attributeCbo.select(0);
    }

    private void attributeChanged() {
        String attrName = attributeCbo
                .getItem(attributeCbo.getSelectionIndex());
        /*
         * Check if the alarm is disabled and enable/disable the controls.
         */
        // boolean alarmDisabled = scanCfg.getAlarmsDisabled(scanTable);
        // System.out.println("alarmDisabled = " + alarmDisabled);
        // disableAllAlarmChk.setSelection(alarmDisabled);
        //
        // disableSelectedAttributeAlarm(alarmDisabled);

        /*
         * Set the value text control with the proper abs/roc value.
         */
        alarmChoiceChanged();

        /*
         * Change the units label.
         */
        unitsLbl.setText(scanCfg.getAbsConfigMgr(scanTable).getUnit(attrName));
    }

    private void disableSelectedAttributeAlarm(boolean alarmDisabled) {
        // String attrName =
        // attributeCbo.getItem(attributeCbo.getSelectionIndex());
        // boolean alarmEnabled = !disableAlarmChk.getSelection();
        //
        // scanCfg.getAbsConfigMgr(scanTable).setAlarmDisabled(attrName,
        // alarmEnabled);

        System.out.println("alarmDisabled = " + alarmDisabled);

        absValueRdo.setEnabled(!alarmDisabled);

        /*
         * If the Alarm Threshold is for the DMD table then we need to
         * permanently disable the rate of change option.
         */
        if (scanTable == ScanTables.DMD) {
            rateOfChangeRdo.setEnabled(false);
        } else {
            rateOfChangeRdo.setEnabled(!alarmDisabled);
        }

        valueTF.setEnabled(!alarmDisabled);
    }

    private void alarmChoiceChanged() {
        if (!valueTF.getText().isEmpty()) {
            mgr.updateScheduledAlarm(site, scanTable, prevAttr, type,
                    Integer.parseInt(valueTF.getText()));
        }
        String attrName = attributeCbo
                .getItem(attributeCbo.getSelectionIndex());
        if (absValueRdo.getSelection() == true) {
            type = AlarmType.AbsVal;
            int val = scanCfg.getAbsConfigMgr(scanTable).getAbsoluteValue(
                    attrName);
            valueTF.setText(String.valueOf(val));
            selectedChoiceLbl.setText(absValStr + ":");
        } else {
            type = AlarmType.RateOfChange;
            int val = scanCfg.getAbsConfigMgr(scanTable).getRateOfChange(
                    attrName);
            valueTF.setText(String.valueOf(val));
            selectedChoiceLbl.setText(rocStr + ":");
        }
        mgr.updateScheduledAlarm(site, scanTable, attributeCbo.getText(), type,
                Integer.parseInt(valueTF.getText()));
        prevAttr = attrName;
    }

    private void validateAndSetValue() {
        int newValue = 0;
        String valueStr = valueTF.getText().trim();

        if (valueStr.matches("[-]*[0-9]+") == true) {
            newValue = Integer.valueOf(valueStr);
        } else {
            MessageBox mb = new MessageBox(shell, SWT.ICON_ERROR | SWT.OK);
            mb.setText("Entry Error");
            mb.setMessage("You must enter a whole number - no letters, spaces, or decimals");
            mb.open();

            valueTF.setFocus();
            valueTF.selectAll();
            return;
        }

        if (newValue == -999) {
            MessageBox mb = new MessageBox(shell, SWT.ICON_ERROR | SWT.OK);
            mb.setText("Entry Error");
            mb.setMessage("-999 is not an allowable number.");
            mb.open();

            valueTF.setFocus();
            valueTF.selectAll();
            return;
        }

        String attrName = attributeCbo
                .getItem(attributeCbo.getSelectionIndex());

        if (absValueRdo.getSelection() == true) {
            scanCfg.getAbsConfigMgr(scanTable).setAbsoluteValue(attrName,
                    newValue);
        } else {
            scanCfg.getAbsConfigMgr(scanTable).setRateOfChangeAlarm(attrName,
                    newValue);
        }
    }

    @Override
    public void closeDialog() {
        shell.dispose();
    }
}
