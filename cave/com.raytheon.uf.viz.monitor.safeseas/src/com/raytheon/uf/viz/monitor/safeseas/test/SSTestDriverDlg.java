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
package com.raytheon.uf.viz.monitor.safeseas.test;

import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.graphics.Font;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Dialog;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Shell;

import com.raytheon.uf.common.localization.LocalizationFile;
import com.raytheon.uf.common.monitor.data.CommonTableConfig.CellType;
import com.raytheon.uf.common.monitor.data.ObConst.DataUsageKey;
import com.raytheon.uf.viz.monitor.safeseas.threshold.SSThresholdMgr;
import com.raytheon.uf.viz.monitor.thresholds.AbstractThresholdMgr.ThresholdKey;
import com.raytheon.uf.viz.monitor.ui.dialogs.LoadSaveDeleteSelectDlg;
import com.raytheon.uf.viz.monitor.ui.dialogs.LoadSaveDeleteSelectDlg.DialogType;
import com.raytheon.uf.viz.monitor.util.MonitorConfigConstants;

public class SSTestDriverDlg extends Dialog {
    private Display display;

    private Shell shell;

    private Font lblFont;

    public SSTestDriverDlg(Shell parentShell) {
        super(parentShell, 0);
    }

    public Object open() {
        Shell parent = getParent();
        display = parent.getDisplay();
        shell = new Shell(parent, SWT.DIALOG_TRIM);
        shell.setText("SAFESEAS Driver");

        GridLayout gl = new GridLayout(1, false);
        gl.horizontalSpacing = 0;
        shell.setLayout(gl);

        shell.setSize(600, 600);

        lblFont = new Font(shell.getDisplay(), "Monospace", 14, SWT.NORMAL);

        initComponents();

        shell.open();

        while (!shell.isDisposed()) {
            if (!display.readAndDispatch())
                display.sleep();
        }

        lblFont.dispose();

        return null;
    }

    private void initComponents() {
        Label lbl = new Label(shell, SWT.NONE);
        lbl.setText("SAFESEAS");
        lbl.setFont(lblFont);

        Button printDisplayThresh = new Button(shell, SWT.PUSH);
        printDisplayThresh.setText("Print Display Threshold Data");
        printDisplayThresh.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                printDisplayThresholdData();
            }
        });

        Button changeThresh = new Button(shell, SWT.PUSH);
        changeThresh.setText("Change Threshold Data");
        changeThresh.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                changeThresholdData();
            }
        });

        Button loadThreshDlg = new Button(shell, SWT.PUSH);
        loadThreshDlg.setText("Load Dialog");
        loadThreshDlg.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                displayLoadDialog();
            }
        });

        Button saveAsThreshDlg = new Button(shell, SWT.PUSH);
        saveAsThreshDlg.setText("Save As Dialog");
        saveAsThreshDlg.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                displaySaveDialog();
            }
        });

        Button printDefaultFileNameBtn = new Button(shell, SWT.PUSH);
        printDefaultFileNameBtn.setText("Print Default Threshold File Name");
        printDefaultFileNameBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                displayDefaultThreshFileName();
            }
        });

        Button selectDefaultFileNameBtn = new Button(shell, SWT.PUSH);
        selectDefaultFileNameBtn.setText("Select Default Threshold File Name");
        selectDefaultFileNameBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                selectDefaultThreshFileName();
            }
        });

        Button loadDefaultFileNameBtn = new Button(shell, SWT.PUSH);
        loadDefaultFileNameBtn.setText("Load Default Threshold");
        loadDefaultFileNameBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                loadDefaultThresholds();
            }
        });

        Button cellTypeBtn = new Button(shell, SWT.PUSH);
        cellTypeBtn.setText("Print CellType");
        cellTypeBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                printCellType();
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

    private void printDisplayThresholdData() {
        SSThresholdMgr sstm = SSThresholdMgr.getInstance();
        sstm.printDisplayThresholds();
    }

    private void changeThresholdData() {
        System.out.println("NOT WORKING...");
        SSThresholdMgr sstm = SSThresholdMgr.getInstance();
        sstm.setThresholdValue(DataUsageKey.DISPLAY, ThresholdKey.RED, "NE093",
                MonitorConfigConstants.SafeSeasDisplay.SS_DISP_METEO_TEMP
                        .getXmlKey(), 111);
    }

    private void displayLoadDialog() {
        SSThresholdMgr sstm = SSThresholdMgr.getInstance();

        LoadSaveDeleteSelectDlg lsDlg = new LoadSaveDeleteSelectDlg(shell,
                DialogType.OPEN, sstm.getDisplayThresholdPath(), sstm
                        .getDefaultFileName(DataUsageKey.DISPLAY));
        LocalizationFile fileName = (LocalizationFile) lsDlg.open();

        if (fileName == null) {
            System.out.println("FileName is null...");
            return;
        }

        System.out.println("Selected file absolute path= "
                + fileName.getFile().getAbsolutePath());
        System.out.println("Selected file name = "
                + fileName.getFile().getName());

        sstm.loadDisplayThreashold(fileName.getFile().getName());
    }

    private void displaySaveDialog() {
        SSThresholdMgr sstm = SSThresholdMgr.getInstance();

        LoadSaveDeleteSelectDlg lsDlg = new LoadSaveDeleteSelectDlg(shell,
                DialogType.SAVE_AS, sstm.getDisplayThresholdPath(), sstm
                        .getDefaultFileName(DataUsageKey.DISPLAY));
        LocalizationFile fileName = (LocalizationFile) lsDlg.open();

        if (fileName == null) {
            System.out.println("FileName is null...");
            return;
        }

        System.out.println("Selected file absolute path= "
                + fileName.getFile().getAbsolutePath());
        System.out.println("Selected file name = "
                + fileName.getFile().getName());

        sstm.saveAsDisplayThresholds(fileName.getFile().getName());
    }

    private void displayDefaultThreshFileName() {
        SSThresholdMgr sstm = SSThresholdMgr.getInstance();

        System.out.println(">" + sstm.getDefDisplayThreshFileName() + "<");
    }

    private void selectDefaultThreshFileName() {
        SSThresholdMgr sstm = SSThresholdMgr.getInstance();

        LoadSaveDeleteSelectDlg lsDlg = new LoadSaveDeleteSelectDlg(shell,
                DialogType.SELECT_DEFAULT, sstm.getDisplayThresholdPath(), sstm
                        .getDefaultFileName(DataUsageKey.DISPLAY));
        LocalizationFile fileName = (LocalizationFile) lsDlg.open();

        if (fileName == null) {
            System.out.println("FileName is null...");
            return;
        }

        System.out.println("Selected file absolute path= "
                + fileName.getFile().getAbsolutePath());
        System.out.println("Selected file name = "
                + fileName.getFile().getName());

        sstm.setDefaultDisplayFileName(fileName.getFile().getName());
    }

    private void loadDefaultThresholds() {
        SSThresholdMgr sstm = SSThresholdMgr.getInstance();

        sstm.loadDefaultDisplayThreshold();
    }

    private void printCellType() {
        CellType ct;
        String key;
        double value = Double.NaN;

        SSThresholdMgr sstm = SSThresholdMgr.getInstance();

    }
}
