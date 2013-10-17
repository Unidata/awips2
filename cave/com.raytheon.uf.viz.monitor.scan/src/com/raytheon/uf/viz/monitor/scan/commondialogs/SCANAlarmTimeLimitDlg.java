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

import java.io.File;

import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Layout;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Spinner;

import com.raytheon.uf.common.localization.IPathManager;
import com.raytheon.uf.common.localization.LocalizationContext;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationLevel;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationType;
import com.raytheon.uf.common.localization.LocalizationFile;
import com.raytheon.uf.common.localization.PathManagerFactory;
import com.raytheon.uf.common.monitor.scan.config.SCANConfigEnums.ScanTables;
import com.raytheon.uf.common.monitor.scan.xml.ScanAlarmXML;
import com.raytheon.uf.common.serialization.SingleTypeJAXBManager;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.viz.ui.dialogs.CaveSWTDialog;

/**
 * 
 * Dialog to change the time limits for CELL, DMD, MESO, and/or TVS.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Nov 22, 2010            lvenable     Initial creation
 * 24 Jul 2013 #2143       skorolev     Changes for non-blocking dialogs.
 * Aug 15, 2013 2143       mpduff       Remove resize.
 * Oct 17, 2013 2361       njensen      Use JAXBManager for XML
 * 
 * </pre>
 * 
 * @author lvenable
 * @version 1.0
 */
public class SCANAlarmTimeLimitDlg extends CaveSWTDialog implements
        ICommonDialogAction {

    private static final SingleTypeJAXBManager<ScanAlarmXML> jaxb = SingleTypeJAXBManager
            .createWithoutException(ScanAlarmXML.class);

    private final IUFStatusHandler statusHandler = UFStatus
            .getHandler(SCANAlarmTimeLimitDlg.class);

    /**
     * Scan Tables
     */
    private final ScanTables scanTable;

    /**
     * Cell Spinner
     */
    private Spinner cellSpnr;

    /**
     * Meso Spinner
     */
    private Spinner mesoSpnr;

    /**
     * TVS Spinner
     */
    private Spinner tvsSpnr;

    /**
     * DMD Spinner
     */
    private Spinner dmdSpnr;

    /**
     * Label Width
     */
    private final int labelWidth = 80;

    /**
     * Width Spinner
     */
    private final int spinnerWidth = 70;

    /**
     * SCAN Alarm XML
     */
    private ScanAlarmXML dataXML;

    /**
     * Constructor
     * 
     * @param parentShell
     * @param scanTable
     * @param site
     */
    public SCANAlarmTimeLimitDlg(Shell parentShell, ScanTables scanTable,
            String site) {
        super(parentShell, SWT.DIALOG_TRIM, CAVE.DO_NOT_BLOCK);
        setText("Alarm Time Limit for: " + site);

        this.scanTable = scanTable;
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
     * @see
     * com.raytheon.viz.ui.dialogs.CaveSWTDialogBase#initializeComponents(org
     * .eclipse.swt.widgets.Shell)
     */
    @Override
    protected void initializeComponents(Shell shell) {

        /*
         * Read in the alarm data from XML
         */
        readAlarmData();

        // Initialize all of the controls and layouts
        if (scanTable == ScanTables.CELL || scanTable == ScanTables.MESO
                || scanTable == ScanTables.TVS) {
            createCellMesoTvsControls();
        } else {
            createDmdControls();
        }

        createBottomButtons();
    }

    /**
     * Create CELL, MESO, TVS Controls.
     */
    private void createCellMesoTvsControls() {
        Composite controlComp = new Composite(shell, SWT.NONE);
        GridLayout gl = new GridLayout(3, false);
        controlComp.setLayout(gl);
        controlComp.setLayoutData(new GridData(SWT.FILL, SWT.DEFAULT, true,
                false));

        /*
         * CELL
         */
        GridData gd = new GridData(SWT.DEFAULT, SWT.CENTER, false, true);
        gd.widthHint = labelWidth;
        Label cellLbl = new Label(controlComp, SWT.RIGHT);
        cellLbl.setText("CELL:");
        cellLbl.setLayoutData(gd);

        gd = new GridData(spinnerWidth, SWT.DEFAULT);
        cellSpnr = new Spinner(controlComp, SWT.BORDER);
        cellSpnr.setMinimum(0);
        cellSpnr.setMaximum(999999);
        cellSpnr.setLayoutData(gd);

        gd = new GridData(SWT.DEFAULT, SWT.CENTER, false, true);
        gd.widthHint = labelWidth;
        gd.horizontalIndent = 10;
        Label cellMinLbl = new Label(controlComp, SWT.NONE);
        cellMinLbl.setText("min");
        cellMinLbl.setLayoutData(gd);

        /*
         * MESO
         */
        gd = new GridData(SWT.DEFAULT, SWT.CENTER, false, true);
        gd.widthHint = labelWidth;
        Label mesoLbl = new Label(controlComp, SWT.RIGHT);
        mesoLbl.setText("MESO:");
        mesoLbl.setLayoutData(gd);

        gd = new GridData(spinnerWidth, SWT.DEFAULT);
        mesoSpnr = new Spinner(controlComp, SWT.BORDER);
        mesoSpnr.setMinimum(0);
        mesoSpnr.setMaximum(999999);
        mesoSpnr.setLayoutData(gd);

        gd = new GridData(SWT.DEFAULT, SWT.CENTER, false, true);
        gd.widthHint = labelWidth;
        gd.horizontalIndent = 10;
        Label mesoMinLbl = new Label(controlComp, SWT.NONE);
        mesoMinLbl.setText("min");
        mesoMinLbl.setLayoutData(gd);

        /*
         * TVS
         */
        gd = new GridData(SWT.DEFAULT, SWT.CENTER, false, true);
        gd.widthHint = labelWidth;
        Label tvsLbl = new Label(controlComp, SWT.RIGHT);
        tvsLbl.setText("TVS:");
        tvsLbl.setLayoutData(gd);

        gd = new GridData(spinnerWidth, SWT.DEFAULT);
        tvsSpnr = new Spinner(controlComp, SWT.BORDER);
        tvsSpnr.setMinimum(0);
        tvsSpnr.setMaximum(999999);
        tvsSpnr.setLayoutData(gd);

        gd = new GridData(SWT.DEFAULT, SWT.CENTER, false, true);
        gd.widthHint = labelWidth;
        gd.horizontalIndent = 10;
        Label tvsMinLbl = new Label(controlComp, SWT.NONE);
        tvsMinLbl.setText("min");
        tvsMinLbl.setLayoutData(gd);

        if (dataXML != null) {
            cellSpnr.setSelection(dataXML.getCellAlarmTime());
            mesoSpnr.setSelection(dataXML.getMesoAlarmTime());
            tvsSpnr.setSelection(dataXML.getTvsAlarmTime());
        }
    }

    /**
     * Create DMD Controls
     */
    private void createDmdControls() {
        Composite controlComp = new Composite(shell, SWT.NONE);
        GridLayout gl = new GridLayout(3, false);
        gl.marginHeight = 20;
        controlComp.setLayout(gl);
        controlComp.setLayoutData(new GridData(SWT.FILL, SWT.DEFAULT, true,
                false));

        /*
         * DMD
         */
        GridData gd = new GridData(SWT.DEFAULT, SWT.CENTER, false, true);
        gd.widthHint = labelWidth;
        Label dmdLbl = new Label(controlComp, SWT.RIGHT);
        dmdLbl.setText("DMD:");
        dmdLbl.setLayoutData(gd);

        gd = new GridData(spinnerWidth, SWT.DEFAULT);
        dmdSpnr = new Spinner(controlComp, SWT.BORDER);
        dmdSpnr.setMinimum(0);
        dmdSpnr.setMaximum(999999);
        dmdSpnr.setLayoutData(gd);

        gd = new GridData(SWT.DEFAULT, SWT.CENTER, false, true);
        gd.widthHint = labelWidth;
        gd.horizontalIndent = 10;
        Label dmdMinLbl = new Label(controlComp, SWT.NONE);
        dmdMinLbl.setText("min");
        dmdMinLbl.setLayoutData(gd);

        if (dataXML != null) {
            dmdSpnr.setSelection(dataXML.getDmdAlarmTime());
        }
    }

    /**
     * Create Bottom Buttons.
     */
    private void createBottomButtons() {
        Composite buttonComp = new Composite(shell, SWT.NONE);
        buttonComp.setLayout(new GridLayout(2, true));
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

                saveAlarmData();
                close();
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
                close();
            }
        });
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
     * Read Alarm Data.
     */
    private void readAlarmData() {
        try {
            dataXML = null;
            IPathManager pm = PathManagerFactory.getPathManager();
            String path = pm.getStaticFile(getFullPathAndFileName())
                    .getAbsolutePath();

            dataXML = jaxb.unmarshalFromXmlFile(path);
        } catch (Exception e) {
            statusHandler.handle(Priority.PROBLEM,
                    "Scan Alarms not available (ScanAlarms.xml).", e);
            dataXML = null;
        }
    }

    /**
     * Save Alarm Data.
     */
    private void saveAlarmData() {
        if (dataXML == null) {
            return;
        }

        if (scanTable == ScanTables.CELL || scanTable == ScanTables.MESO
                || scanTable == ScanTables.TVS) {
            dataXML.setCellAlarmTime(cellSpnr.getSelection());
            dataXML.setMesoAlarmTime(mesoSpnr.getSelection());
            dataXML.setTvsAlarmTime(tvsSpnr.getSelection());
        } else {
            dataXML.setDmdAlarmTime(dmdSpnr.getSelection());
        }

        IPathManager pm = PathManagerFactory.getPathManager();
        LocalizationContext context = pm.getContext(
                LocalizationType.COMMON_STATIC, LocalizationLevel.SITE);
        LocalizationFile locFile = pm.getLocalizationFile(context,
                getFullPathAndFileName());

        if (!locFile.getFile().getParentFile().exists()) {
            locFile.getFile().getParentFile().mkdirs();
        }

        try {
            jaxb.marshalToXmlFile(dataXML, locFile.getFile().getAbsolutePath());
            locFile.save();
        } catch (Exception e) {
            statusHandler.handle(Priority.ERROR,
                    "Error saving configuration file", e);
        }
    }

    /**
     * Get Full Path And File Name.
     * 
     * @return file name
     */
    public String getFullPathAndFileName() {
        String fs = String.valueOf(File.separatorChar);
        StringBuilder sb = new StringBuilder();

        sb.append("scan").append(fs).append("ScanAlarms.xml");

        return sb.toString();
    }
}
