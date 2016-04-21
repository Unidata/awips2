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
package com.raytheon.uf.viz.monitor.ui.dialogs;

import java.io.File;
import java.util.Arrays;

import javax.xml.bind.JAXB;

import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Shell;

import com.raytheon.uf.common.localization.IPathManager;
import com.raytheon.uf.common.localization.LocalizationContext;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationLevel;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationType;
import com.raytheon.uf.common.localization.LocalizationFile;
import com.raytheon.uf.common.localization.PathManagerFactory;
import com.raytheon.uf.common.monitor.data.CommonConfig;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.viz.monitor.config.CommonTableConfig;
import com.raytheon.uf.viz.monitor.config.CommonTableConfig.ObsHistType;
import com.raytheon.uf.viz.monitor.xml.HistConfigXML;
import com.raytheon.viz.ui.dialogs.CaveSWTDialog;

/**
 * Observation History configuration dialog.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Apr  6, 2009            lvenable     Initial creation
 * May 23, 2014 3068       skorolev     Corrected ObsHistType. Cleaned code.
 * 
 * </pre>
 * 
 * @author lvenable
 * @version 1.0
 */
public class ObsHistConfigDlg extends CaveSWTDialog {

    private final IUFStatusHandler statusHandler = UFStatus
            .getHandler(ObsHistConfigDlg.class);

    /**
     * Application name.
     */
    private final CommonConfig.AppName appName;

    /**
     * Common table configuration class.
     */
    private CommonTableConfig tableConfig;

    /**
     * Array of visible Maritime table columns changed by the user.
     */
    private boolean[] newVisColsMaritime;

    /**
     * Array of visible METAR table columns changed by the user.
     */
    private boolean[] newVisColsMetar;

    /**
     * Array on Maritime check boxes.
     */
    private Button[] maritimeChkArray;

    /**
     * Array of METAR check boxes.
     */
    private Button[] metarChkArray;

    /**
     * Main Maritime check box. Checking/Unchecking will check/uncheck all of
     * the Maritime check boxes.
     */
    private Button maritimeChk;

    /**
     * Main METER check box. Checking/Unchecking will check/uncheck all of the
     * METAR check boxes.
     */
    private Button metarChk;

    /**
     * Skip Maritime flag. Indicates if the Maritime check boxes need to be
     * displayed.
     */
    public boolean skipMaritime;

    /**
     * Starting index of the columns that can be hidden.
     */
    private final int startingColIndex = 1;

    /**
     * XML configuration file for visible columns.
     */
    private HistConfigXML cfgXML = null;

    /**
     * File in the localization system.
     */
    private LocalizationFile locFile;

    /**
     * Composite for History Table.
     */
    private final ObsHistTableComp obsHistTable;

    /**
     * History table observation type.
     */
    private final ObsHistType obsType;

    /**
     * Keys for columns.
     */
    private String[] colKeys;

    /**
     * Initiate Columns for Metar data.
     */
    private boolean[] initColsMetar;

    /**
     * Initiate Columns for Maritime data.
     */
    private boolean[] initColsMaritime;

    /**
     * Constructor.
     * 
     * @param parent
     *            Parent shell.
     * @param appName
     *            Application name.
     * @param obsHistTable
     * @param maritimeCols
     *            Visible/Hidden Maritime columns (can be null).
     * @param obsType
     *            Visible/Hidden METAR columns.
     */
    public ObsHistConfigDlg(Shell parent, CommonConfig.AppName appName,
            ObsHistTableComp obsHistTable, ObsHistType obsType) {
        super(parent, SWT.DIALOG_TRIM | SWT.RESIZE);

        this.appName = appName;
        this.obsHistTable = obsHistTable;
        this.obsType = obsType;
        this.cfgXML = getHistConfigFile();

        /*
         * NOTE : Element 0 of the boolean array of visible columns passed in
         * for Maritime and METAR is for the Time (UTC) column. This column is
         * always visible in the Obs History table so when the button (checkbox)
         * arrays are indexed against the boolean arrays a -1 must be used. For
         * example: if there are 10 METAR columns passed in we have to ignore
         * the first column. When the METAR check boxes are created there will
         * only be 9 check boxes because Time (UTC) is not allowed to change.
         * Element 1 of the metarCols is element 0 of the METAR checkbox array.
         */
        tableConfig = CommonTableConfig.getInstance();
        initiateColumns();
    }

    /**
     * Initiates Columns.
     */
    private void initiateColumns() {

        this.setInitColsMetar(new boolean[tableConfig.getMetarConfigureNames().length + 1]);
        newVisColsMetar = new boolean[tableConfig.getMetarConfigureNames().length + 1];
        Arrays.fill(getInitColsMetar(), true);
        setSkipMaritime(true);
        if (appName.equals(CommonConfig.AppName.FOG)) {
            this.setInitColsMaritime(new boolean[tableConfig
                    .getMaritimeFogConfigureNames().length + 1]);
            newVisColsMaritime = new boolean[tableConfig
                    .getMaritimeFogConfigureNames().length + 1];
            Arrays.fill(getInitColsMaritime(), true);

            this.setInitColsMetar(new boolean[tableConfig
                    .getMetarFogConfigureNames().length + 1]);
            newVisColsMetar = new boolean[tableConfig
                    .getMetarFogConfigureNames().length + 1];
            Arrays.fill(getInitColsMetar(), true);
            setSkipMaritime(false);
        }
        if (appName.equals(CommonConfig.AppName.SAFESEAS)) {
            this.setInitColsMaritime(new boolean[tableConfig
                    .getMaritimeSSConfigureNames().length + 1]);
            newVisColsMaritime = new boolean[tableConfig
                    .getMaritimeSSConfigureNames().length + 1];
            Arrays.fill(getInitColsMaritime(), true);
            setSkipMaritime(false);
        }
        if (this.cfgXML != null) {
            // read saved configurations
            this.setInitColsMetar(cfgXML.getMetar());
            this.setInitColsMaritime(cfgXML.getMaritime());
        }
        if (!isSkipMaritime()) {
            System.arraycopy(getInitColsMaritime(), 0, newVisColsMaritime, 0,
                    getInitColsMaritime().length);
        }
        System.arraycopy(getInitColsMetar(), 0, newVisColsMetar, 0,
                getInitColsMetar().length);

    }

    /**
     * Initialize the components on the display.
     */
    @Override
    protected void initializeComponents(Shell shell) {
        tableConfig = CommonTableConfig.getInstance();

        createColumnControls();

        createBottomButtons();

        /*
         * Check the columns that are visible on the table.
         */
        checkVisibleColumnButtons();

        if (!this.isSkipMaritime()) {
            determineMainCheckBox(maritimeChk, maritimeChkArray);
        }

        determineMainCheckBox(metarChk, metarChkArray);
    }

    /**
     * Create the column controls.
     */
    private void createColumnControls() {
        int gridColumns = 3;

        GridData gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        Composite mainControlComp = new Composite(shell, SWT.NONE);
        mainControlComp.setLayout(new GridLayout(gridColumns, false));
        mainControlComp.setLayoutData(gd);

        if (!this.isSkipMaritime()) {
            /*
             * Create the Maritime controls
             */
            createMaritimeControls(mainControlComp);

            /*
             * Create a vertical separator bar
             */
            gd = new GridData(SWT.CENTER, SWT.FILL, true, true);
            Label sepLbl = new Label(mainControlComp, SWT.SEPARATOR
                    | SWT.VERTICAL);
            sepLbl.setLayoutData(gd);
        }

        /*
         * Create the METAR column controls
         */
        createMetarControls(mainControlComp);
        // lock unused check boxes
        if (obsType == ObsHistType.METAR && !this.isSkipMaritime()) {
            for (int i = startingColIndex; i < newVisColsMaritime.length; i++) {
                maritimeChkArray[i - 1].setEnabled(false);
            }
        }
        if (obsType == ObsHistType.MARITIME) {
            for (int i = startingColIndex; i < newVisColsMetar.length; i++) {
                metarChkArray[i - 1].setEnabled(false);
            }
        }
    }

    /**
     * Create the Maritime check box controls.
     * 
     * @param parent
     */
    private void createMaritimeControls(Composite parent) {
        colKeys = new String[0];
        if (appName.equals(CommonConfig.AppName.SAFESEAS)) {
            colKeys = tableConfig.getMaritimeSSConfigureNames();
        }
        if (appName.equals(CommonConfig.AppName.FOG)) {
            colKeys = tableConfig.getMaritimeFogConfigureNames();
        }

        /*
         * NOTE : The list of visible columns
         */
        maritimeChkArray = new Button[colKeys.length];

        GridData gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        Composite controlComp = new Composite(parent, SWT.NONE);
        controlComp.setLayout(new GridLayout(1, false));
        controlComp.setLayoutData(gd);

        gd = new GridData(SWT.CENTER, SWT.DEFAULT, true, false);
        maritimeChk = new Button(controlComp, SWT.CHECK);
        maritimeChk.setText("Maritime");
        maritimeChk.setLayoutData(gd);
        maritimeChk.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                setCheckBoxes(maritimeChk.getSelection(), maritimeChkArray);
                applyVisCols();
            }
        });

        for (int i = 0; i < colKeys.length; i++) {
            Button b = new Button(controlComp, SWT.CHECK);
            // b.setText(tableConfig.getObsHistColumnAttr(colKeys[i]).getName());
            b.setText(colKeys[i]);
            b.addSelectionListener(new SelectionAdapter() {
                @Override
                public void widgetSelected(SelectionEvent event) {
                    determineMainCheckBox(maritimeChk, maritimeChkArray);
                    applyVisCols();
                }

            });

            maritimeChkArray[i] = b;
        }

    }

    /**
     * Create the METAR check box controls.
     * 
     * @param parent
     */
    private void createMetarControls(Composite parent) {
        colKeys = new String[0];
        if (appName.equals(CommonConfig.AppName.FOG)) {
            colKeys = tableConfig.getMetarFogConfigureNames();
        } else {
            colKeys = tableConfig.getMetarConfigureNames();
        }
        metarChkArray = new Button[colKeys.length];

        GridData gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        Composite controlComp = new Composite(parent, SWT.NONE);
        controlComp.setLayout(new GridLayout(1, false));
        controlComp.setLayoutData(gd);

        gd = new GridData(SWT.CENTER, SWT.DEFAULT, true, false);
        metarChk = new Button(controlComp, SWT.CHECK);
        metarChk.setText("METAR");
        metarChk.setLayoutData(gd);
        metarChk.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                setCheckBoxes(metarChk.getSelection(), metarChkArray);
                applyVisCols();
            }
        });

        for (int i = 0; i < colKeys.length; i++) {
            Button b = new Button(controlComp, SWT.CHECK);
            // b.setText(tableConfig.getObsHistColumnAttr(colKeys[i]).getName());
            b.setText(colKeys[i]);
            if (newVisColsMetar != null) {
                b.setSelection(newVisColsMetar[i]);
            }
            b.addSelectionListener(new SelectionAdapter() {
                @Override
                public void widgetSelected(SelectionEvent event) {
                    determineMainCheckBox(metarChk, metarChkArray);
                    applyVisCols();
                }
            });
            metarChkArray[i] = b;
        }
    }

    /**
     * Create the bottom buttons on the display.
     */
    private void createBottomButtons() {
        addSeparator(shell);

        GridData gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        Composite mainButtonComp = new Composite(shell, SWT.NONE);
        mainButtonComp.setLayout(new GridLayout(1, false));
        mainButtonComp.setLayoutData(gd);

        gd = new GridData(SWT.CENTER, SWT.DEFAULT, false, false);
        Composite buttonComp = new Composite(shell, SWT.NONE);
        buttonComp.setLayout(new GridLayout(2, false));
        buttonComp.setLayoutData(gd);

        gd = new GridData(100, SWT.DEFAULT);
        Button saveApplyBtn = new Button(buttonComp, SWT.PUSH);
        saveApplyBtn.setText("Save");
        saveApplyBtn.setLayoutData(gd);
        saveApplyBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                saveConfig();
                initiateColumns();
                shell.dispose();
            }
        });

        gd = new GridData(100, SWT.DEFAULT);
        Button closeBtn = new Button(buttonComp, SWT.PUSH);
        closeBtn.setText("Cancel");
        closeBtn.setLayoutData(gd);
        closeBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                restoreVisCols();
                shell.dispose();
            }
        });
    }

    /**
     * Add a separator bar to the display.
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
     * Check the check box to reflect the current visible columns.
     */
    private void checkVisibleColumnButtons() {
        if (!isSkipMaritime()) {
            for (int i = startingColIndex; i < newVisColsMaritime.length; i++) {
                maritimeChkArray[i - 1].setSelection(newVisColsMaritime[i]);
            }
        }

        for (int i = startingColIndex; i < newVisColsMetar.length; i++) {
            metarChkArray[i - 1].setSelection(newVisColsMetar[i]);
        }
    }

    /**
     * Set the check boxes to either checked or unchecked.
     * 
     * @param checked
     *            Checked flag.
     * @param buttons
     *            Array of check boxes to check/uncheck.
     */
    private void setCheckBoxes(boolean checked, Button[] buttons) {
        for (int i = 0; i < buttons.length; i++) {
            buttons[i].setSelection(checked);
        }
    }

    /**
     * Determine if a main check box needs to be checked or unchecked.
     * 
     * @param mainCheckBox
     *            Main check box.
     * @param buttons
     *            Array of check boxes.
     */
    private void determineMainCheckBox(Button mainCheckBox, Button[] buttons) {
        boolean allChecked = true;

        for (int i = 0; i < buttons.length; i++) {
            if (buttons[i].getSelection() == false) {
                allChecked = false;
            }
        }

        mainCheckBox.setSelection(allChecked);

        if ((obsType == ObsHistType.METAR && mainCheckBox == maritimeChk)
                || (obsType == ObsHistType.MARITIME && mainCheckBox == metarChk)) {
            mainCheckBox.setEnabled(false);
        }
    }

    /**
     * Get the path where the history config XML file is contained.
     */
    private LocalizationFile getXmlFile() {
        IPathManager pm = PathManagerFactory.getPathManager();
        LocalizationContext context = pm.getContext(
                LocalizationType.COMMON_STATIC, LocalizationLevel.USER);
        String visFileName = getHistPath() + "visFieldsFile.xml";
        LocalizationFile locFile = pm.getLocalizationFile(context, visFileName);
        return locFile;
    }

    /**
     * Saves Configuration file.
     */
    private void saveConfig() {
        updateCheckBoxesStatus();
        this.cfgXML = new HistConfigXML();
        cfgXML.setMaritime(newVisColsMaritime);
        cfgXML.setMetar(newVisColsMetar);
        locFile = getXmlFile();
        try {
            statusHandler.info("Saving -- "
                    + locFile.getFile().getAbsolutePath());
            JAXB.marshal(cfgXML, locFile.getFile());
            locFile.save();
        } catch (Exception e) {
            e.printStackTrace();
        }
    }

    /**
     * Gets History Configuration XML.
     * 
     * @return
     */
    private HistConfigXML getHistConfigFile() {
        // Open user file if exist.
        locFile = getXmlFile();
        if (locFile.exists()) {
            try {
                cfgXML = JAXB.unmarshal(locFile.getFile(), HistConfigXML.class);
            } catch (RuntimeException e) {
                e.printStackTrace();
            }
        }
        return cfgXML;
    }

    /**
     * Gets History Table Configuration Path.
     * 
     * @return
     */
    private String getHistPath() {
        String fs = String.valueOf(File.separatorChar);
        StringBuilder sb = new StringBuilder();
        sb.append(appName).append(fs);
        sb.append("history").append(fs);
        return sb.toString();
    }

    /**
     * Updates Check Boxes Status.
     */
    private void updateCheckBoxesStatus() {
        {
            if (!isSkipMaritime()) {
                for (int i = startingColIndex; i < newVisColsMaritime.length; i++) {
                    newVisColsMaritime[i] = maritimeChkArray[i - 1]
                            .getSelection();
                }
            }

            for (int i = startingColIndex; i < newVisColsMetar.length; i++) {
                newVisColsMetar[i] = metarChkArray[i - 1].getSelection();
            }
        }

    }

    /**
     * Apply Visible Columns.
     */
    private void applyVisCols() {
        updateCheckBoxesStatus();
        if (obsType.equals(ObsHistType.METAR)
                || obsType.equals(ObsHistType.MESONET)) {
            obsHistTable.showHideTableColumns(newVisColsMetar);
        } else {
            obsHistTable.showHideTableColumns(newVisColsMaritime);
        }
        obsHistTable.getParent().pack();
    }

    /**
     * Restore Visible Columns.
     */
    private void restoreVisCols() {
        if (obsType.equals(ObsHistType.METAR)
                || obsType.equals(ObsHistType.MESONET)) {
            obsHistTable.showHideTableColumns(getInitColsMetar());
        } else {
            obsHistTable.showHideTableColumns(getInitColsMaritime());
        }
        obsHistTable.getParent().pack();
    }

    /**
     * Get Skip Maritime flag.
     * 
     * @return the skipMaritime
     */
    protected boolean isSkipMaritime() {
        return skipMaritime;
    }

    /**
     * Sets Skip Maritime flag.
     * 
     * @param skipMaritime
     *            the skipMaritime to set
     */
    protected void setSkipMaritime(boolean skipMaritime) {
        this.skipMaritime = skipMaritime;
    }

    /**
     * Gets InitColsMetar flag.
     * 
     * @return the initColsMetar
     */
    protected boolean[] getInitColsMetar() {
        return initColsMetar;
    }

    /**
     * Sets InitColsMetar flag.
     * 
     * @param initColsMetar
     *            the initColsMetar to set
     */
    protected void setInitColsMetar(boolean[] initColsMetar) {
        this.initColsMetar = initColsMetar;
    }

    /**
     * Gets InitColsMaritime flag.
     * 
     * @return the initColsMaritime
     */
    protected boolean[] getInitColsMaritime() {
        return initColsMaritime;
    }

    /**
     * Sets InitColsMaritime flag.
     * 
     * @param initColsMaritime
     *            the initColsMaritime to set
     */
    protected void setInitColsMaritime(boolean[] initColsMaritime) {
        this.initColsMaritime = initColsMaritime;
    }

}
