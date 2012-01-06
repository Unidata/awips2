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
package com.raytheon.uf.viz.monitor.fog.test;

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
import com.raytheon.uf.viz.monitor.fog.threshold.FogAlgorithmMgr;
import com.raytheon.uf.viz.monitor.fog.threshold.FogThresholdMgr;
import com.raytheon.uf.viz.monitor.fog.ui.dialogs.FogMonThreshSetupDlg;
import com.raytheon.uf.viz.monitor.fog.xml.FogMonitorAlgorithmXML;
import com.raytheon.uf.viz.monitor.thresholds.AbstractThresholdMgr.ThresholdKey;
import com.raytheon.uf.viz.monitor.ui.dialogs.LoadSaveDeleteSelectDlg;
import com.raytheon.uf.viz.monitor.ui.dialogs.LoadSaveDeleteSelectDlg.DialogType;
import com.raytheon.uf.viz.monitor.util.MonitorConfigConstants;
import com.raytheon.uf.viz.monitor.util.MonitorConfigConstants.FogDisplay;

public class FogTestDriverDlg extends Dialog {
    private Display display;

    private Shell shell;

    private Font lblFont;

    public FogTestDriverDlg(Shell parentShell) {
        super(parentShell, 0);
    }

    public Object open() {
        Shell parent = getParent();
        display = parent.getDisplay();
        shell = new Shell(parent, SWT.DIALOG_TRIM);
        shell.setText("FOG Driver");

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
        lbl.setText("FOG");
        lbl.setFont(lblFont);

        Button printFogDisplayThresh = new Button(shell, SWT.PUSH);
        printFogDisplayThresh.setText("Print FOG Display Threshold Data");
        printFogDisplayThresh.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                printFogDisplayThresholdData();
            }
        });

        Button printCopyFogDisplayThresh = new Button(shell, SWT.PUSH);
        printCopyFogDisplayThresh
                .setText("**** Print FOG Display Threshold Data COPY");
        printCopyFogDisplayThresh.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                printCopyFogDisplayThresholdData();
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

        /*
         * ************************************************************************
         */
        addSeparator(shell);

        lbl = new Label(shell, SWT.NONE);
        lbl.setText("FOG ALGORITHM");
        lbl.setFont(lblFont);

        Button fogAlgorithmBtn = new Button(shell, SWT.PUSH);
        fogAlgorithmBtn.setText("Print FOG Algorithm Data");
        fogAlgorithmBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                printFogAlgorithmData();
            }
        });

        Button fogAlgorithmChangeBtn = new Button(shell, SWT.PUSH);
        fogAlgorithmChangeBtn.setText("Change FOG Algorithm Data");
        fogAlgorithmChangeBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                changeFogAlgorithmData();
            }
        });

        Button saveAsAlgorithmBtn = new Button(shell, SWT.PUSH);
        saveAsAlgorithmBtn.setText("Algorithm Save As Dialog");
        saveAsAlgorithmBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                algorithmSaveDialog();
            }
        });

        Button loadAlgorithmBtn = new Button(shell, SWT.PUSH);
        loadAlgorithmBtn.setText("Algorithm Load Dialog");
        loadAlgorithmBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                algorithmLoadDialog();
            }
        });

        Button selectAlgorithmFileNameBtn = new Button(shell, SWT.PUSH);
        selectAlgorithmFileNameBtn
                .setText("Select Default Algorithm File Name");
        selectAlgorithmFileNameBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                selectDefaultAlgorithmFileName();
            }
        });

        Button displayFogAlgDailogBtn = new Button(shell, SWT.PUSH);
        displayFogAlgDailogBtn.setText("Display Fog Algorithm Dialog");
        displayFogAlgDailogBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                displayFogAlgDialog();
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

    private void printFogDisplayThresholdData() {
        FogThresholdMgr ftm = FogThresholdMgr.getInstance();
        ftm.printDisplayThresholds();
    }

    private void printCopyFogDisplayThresholdData() {
        FogThresholdMgr ftm = FogThresholdMgr.getInstance();
        ftm.printDisplayThresholdsXMLCopy();
    }

    private void changeThresholdData() {
        FogThresholdMgr ftm = FogThresholdMgr.getInstance();
        ftm.setThresholdValue(DataUsageKey.DISPLAY, ThresholdKey.RED, "NE093",
                MonitorConfigConstants.FogDisplay.FOG_DISP_METEO_TEMP
                        .getXmlKey(), 111);
    }

    private void displayLoadDialog() {
        FogThresholdMgr ftm = FogThresholdMgr.getInstance();

        LoadSaveDeleteSelectDlg lsDlg = new LoadSaveDeleteSelectDlg(shell,
                DialogType.OPEN, ftm.getDisplayThresholdPath(), ftm
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

        ftm.loadDisplayThreashold(fileName.getFile().getName());
    }

    private void displaySaveDialog() {
        FogThresholdMgr ftm = FogThresholdMgr.getInstance();

        LoadSaveDeleteSelectDlg lsDlg = new LoadSaveDeleteSelectDlg(shell,
                DialogType.SAVE_AS, ftm.getDisplayThresholdPath(), ftm
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

        ftm.saveAsDisplayThresholds(fileName.getFile().getName());
    }

    private void displayDefaultThreshFileName() {
        FogThresholdMgr ftm = FogThresholdMgr.getInstance();

        System.out.println(">" + ftm.getDefDisplayThreshFileName() + "<");
    }

    private void selectDefaultThreshFileName() {
        FogThresholdMgr ftm = FogThresholdMgr.getInstance();

        LoadSaveDeleteSelectDlg lsDlg = new LoadSaveDeleteSelectDlg(shell,
                DialogType.SELECT_DEFAULT, ftm.getDisplayThresholdPath(), ftm
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

        ftm.setDefaultDisplayFileName(fileName.getFile().getName());
    }

    private void loadDefaultThresholds() {
        FogThresholdMgr ftm = FogThresholdMgr.getInstance();

        ftm.loadDefaultDisplayThreshold();
    }

    private void printCellType() {
        CellType ct;
        String key;
        double value = Double.NaN;

        FogThresholdMgr ftm = FogThresholdMgr.getInstance();

        /*
         * Ceiling
         */

        // -----------------------------
        key = FogDisplay.FOG_DISP_METEO_CEILING.getXmlKey();
        value = 3.0;
        ct = ftm.getThresholdValueCellType(DataUsageKey.DISPLAY, "NE093", key,
                value);
        System.out.println("key = " + key + "\t value = " + value
                + "\tCellType = " + ct.name());

        // -----------------------------
        value = 7.0;
        ct = ftm.getThresholdValueCellType(DataUsageKey.DISPLAY, "NE093", key,
                value);
        System.out.println("key = " + key + "\t value = " + value
                + "\tCellType = " + ct.name());

        // -----------------------------
        value = 12.0;
        ct = ftm.getThresholdValueCellType(DataUsageKey.DISPLAY, "NE093", key,
                value);
        System.out.println("key = " + key + "\t value = " + value
                + "\tCellType = " + ct.name());

        /*
         * Temp
         */

        // -----------------------------
        key = FogDisplay.FOG_DISP_METEO_TEMP.getXmlKey();
        value = 100.0;
        ct = ftm.getThresholdValueCellType(DataUsageKey.DISPLAY, "NE093", key,
                value);
        System.out.println("key = " + key + "\t value = " + value
                + "\tCellType = " + ct.name());

        // -----------------------------
        key = FogDisplay.FOG_DISP_METEO_TEMP.getXmlKey();
        value = 85.0;
        ct = ftm.getThresholdValueCellType(DataUsageKey.DISPLAY, "NE093", key,
                value);
        System.out.println("key = " + key + "\t value = " + value
                + "\tCellType = " + ct.name());

        // -----------------------------
        key = FogDisplay.FOG_DISP_METEO_TEMP.getXmlKey();
        value = 75.0;
        ct = ftm.getThresholdValueCellType(DataUsageKey.DISPLAY, "NE093", key,
                value);
        System.out.println("key = " + key + "\t value = " + value
                + "\tCellType = " + ct.name());
    }

    private void printFogAlgorithmData() {
        FogAlgorithmMgr fam = FogAlgorithmMgr.getInstance();

        FogMonitorAlgorithmXML algXML = fam.getAlgorithmXML();

        System.out.println(algXML.getFogProductYLo());
        System.out.println(algXML.getFogProductRLo());
        System.out.println(algXML.getFogProductRHi());
        System.out.println(algXML.getFogProductYHi());
        System.out.println(algXML.getVisYLo());
        System.out.println(algXML.getVisRLo());
        System.out.println(algXML.getVisRHi());
        System.out.println(algXML.getVisYHi());
        System.out.println(algXML.getMaxCloudTemp());
        System.out.println(algXML.getIceSnowVsFog());
        System.out.println(algXML.getCoolFogVsWarmSurface());
        System.out.println(algXML.getDaytimeSmoothThresh());
        System.out.println(algXML.getAdjacencyThresh());
        System.out.println(algXML.getTwilightAngle());
        System.out.println(algXML.getFractalDimension());
    }

    private void changeFogAlgorithmData() {
        FogAlgorithmMgr fam = FogAlgorithmMgr.getInstance();
        fam.getAlgorithmXML().setFractalDimension(1.7);
    }

    private void algorithmSaveDialog() {
        FogAlgorithmMgr fam = FogAlgorithmMgr.getInstance();

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

    private void algorithmLoadDialog() {
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
    }

    private void selectDefaultAlgorithmFileName() {
        FogAlgorithmMgr fam = FogAlgorithmMgr.getInstance();

        LoadSaveDeleteSelectDlg lsDlg = new LoadSaveDeleteSelectDlg(shell,
                DialogType.SELECT_DEFAULT, fam.getDefaultFileNamePath(), fam
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

    private void displayFogAlgDialog() {
        FogMonThreshSetupDlg fogThreshSetup = new FogMonThreshSetupDlg(shell);
        fogThreshSetup.open();
    }
}
