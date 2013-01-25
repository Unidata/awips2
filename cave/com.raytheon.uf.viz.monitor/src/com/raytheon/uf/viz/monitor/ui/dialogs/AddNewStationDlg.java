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

import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Layout;
import org.eclipse.swt.widgets.MessageBox;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Text;

import com.raytheon.uf.common.geospatial.ISpatialQuery;
import com.raytheon.uf.common.geospatial.SpatialQueryFactory;
import com.raytheon.uf.common.monitor.config.FogMonitorConfigurationManager;
import com.raytheon.uf.common.monitor.config.MonitorConfigurationManager;
import com.raytheon.uf.common.monitor.config.SSMonitorConfigurationManager;
import com.raytheon.uf.common.monitor.config.SnowMonitorConfigurationManager;
import com.raytheon.uf.common.monitor.data.CommonConfig;
import com.raytheon.uf.common.monitor.data.CommonConfig.AppName;
import com.raytheon.uf.common.monitor.xml.StationIdXML;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.viz.ui.dialogs.CaveSWTDialog;

/**
 * 
 * Add New Station dialog.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Apr 2, 2009            lvenable     Initial creation
 * Nov 20, 2012 1297      skorolev     Changes for non-blocking dialog.
 * 
 * </pre>
 * 
 * @author lvenable
 * @version 1.0
 */
public class AddNewStationDlg extends CaveSWTDialog {
    private final IUFStatusHandler statusHandler = UFStatus
            .getHandler(AddNewStationDlg.class);

    /**
     * Application name.
     */
    private AppName appName;

    /**
     * METAR radio button.
     */
    private Button metarRdo;

    /**
     * Maritime button.
     */
    private Button maritimeRdo;

    /**
     * Mesonet button;
     */
    private Button mesonetRdo;

    /**
     * Station label.
     */
    private Label stationLbl;

    /**
     * Station text control.
     */
    private Text stationTF;

    /**
     * Zone
     */
    private String area;

    /**
     * Call back interface
     */
    private INewZoneStnAction macDlg;

    /**
     * Area configuration manager.
     */
    private MonitorConfigurationManager configManager;

    /**
     * Constructor.
     * 
     * @param parent
     *            Parent shell.
     * @param appName
     * @param area
     * @param macDlg
     */
    public AddNewStationDlg(Shell parent, CommonConfig.AppName appName,
            String area, INewZoneStnAction macDlg) {
        super(parent, SWT.DIALOG_TRIM, CAVE.DO_NOT_BLOCK);
        setText(appName.toString() + ": Add a New Stn to Monitor Area");
        this.appName = appName;
        this.area = area;
        this.macDlg = macDlg;
        configManager = getConfigManager(appName);
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.viz.ui.dialogs.CaveSWTDialogBase#constructShellLayout()
     */
    @Override
    protected Layout constructShellLayout() {
        GridLayout mainLayout = new GridLayout(1, false);
        mainLayout.marginHeight = 2;
        mainLayout.marginWidth = 2;
        mainLayout.verticalSpacing = 2;
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
        setReturnValue(false);
        createTopLabelRadioControls();
        createTextControls();
        createBottomButtons();
        setStationLabel();
    }

    /**
     * Creates the top radio controls.
     */
    private void createTopLabelRadioControls() {
        Composite topComp = new Composite(shell, SWT.NONE);
        topComp.setLayout(new GridLayout(2, true));
        topComp.setLayoutData(new GridData(SWT.FILL, SWT.DEFAULT, true, false));
        /*
         * Add the label.
         */
        Label topLbl = new Label(topComp, SWT.RIGHT);
        topLbl.setText("Please type in a new: ");
        topLbl.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true, true));
        /*
         * Add the radio controls.
         */
        Composite radioComp = new Composite(topComp, SWT.NONE);
        radioComp.setLayout(new GridLayout(1, false));
        metarRdo = new Button(radioComp, SWT.RADIO);
        metarRdo.setText("Metar");
        metarRdo.setSelection(true);
        metarRdo.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                setStationLabel();
            }
        });
        // TODO: Disable if area has no marine zones.
        maritimeRdo = new Button(radioComp, SWT.RADIO);
        maritimeRdo.setText("Maritime");
        maritimeRdo.setEnabled(appName != CommonConfig.AppName.SNOW);
        maritimeRdo.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                setStationLabel();
            }
        });
        mesonetRdo = new Button(radioComp, SWT.RADIO);
        mesonetRdo.setText("Mesonet");
        mesonetRdo.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                setStationLabel();
            }
        });
    }

    /**
     * Create the text controls.
     */
    private void createTextControls() {
        Composite textComp = new Composite(shell, SWT.NONE);
        textComp.setLayout(new GridLayout(1, true));
        textComp.setLayoutData(new GridData(SWT.FILL, SWT.DEFAULT, true, false));
        stationLbl = new Label(textComp, SWT.NONE);
        stationLbl.setLayoutData(new GridData(SWT.FILL, SWT.DEFAULT, true,
                false));
        stationTF = new Text(textComp, SWT.BORDER);
        stationTF.setLayoutData(new GridData(250, SWT.DEFAULT));
    }

    /**
     * Creates the bottom buttons.
     */
    private void createBottomButtons() {
        GridData gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        Composite mainButtonComp = new Composite(shell, SWT.NONE);
        mainButtonComp.setLayout(new GridLayout(1, false));
        mainButtonComp.setLayoutData(gd);
        // Add a separator label.
        Label sepLbl = new Label(mainButtonComp, SWT.SEPARATOR | SWT.HORIZONTAL);
        sepLbl.setLayoutData(gd);

        gd = new GridData(SWT.CENTER, SWT.DEFAULT, false, false);
        Composite buttonComp = new Composite(shell, SWT.NONE);
        buttonComp.setLayout(new GridLayout(2, false));
        buttonComp.setLayoutData(gd);

        gd = new GridData(100, SWT.DEFAULT);
        Button addBtn = new Button(buttonComp, SWT.PUSH);
        addBtn.setText("Add");
        addBtn.setLayoutData(gd);
        addBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                String stationType = StationIdXML.METAR;
                if (metarRdo.getSelection()) {
                    stationType = StationIdXML.METAR;
                } else if (mesonetRdo.getSelection()) {
                    String s = stationTF.getText();
                    s = s.substring(s.indexOf("#") + 1, s.length());
                    stationType = s.toUpperCase();
                    // TODO need to verify the stationType exists.
                    // was in SSmesonetStationInfo.txt in AWIPS1.
                } else {
                    stationType = StationIdXML.MARITIME;
                }
                if (!stationTF.getText().isEmpty()) {
                    configManager.addStation(area, stationTF.getText(),
                            stationType, false);
                    /**
                     * for DR #7854: add new station to Monitor Area Config GUI
                     */
                    handleAddNewStation();
                } else {
                    displayInputErrorMsg("Invalid Station ID entered: Please enter a valid Station ID for the selected Station Type");
                }
            }
        });

        gd = new GridData(100, SWT.DEFAULT);
        Button closeBtn = new Button(buttonComp, SWT.PUSH);
        closeBtn.setText("Close");
        closeBtn.setLayoutData(gd);
        closeBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                setReturnValue(true);
                close();
            }
        });
    }

    /**
     * Set the station label.
     */
    private void setStationLabel() {
        if (mesonetRdo.getSelection() == true) {
            stationLbl.setText("StationID#Provider:");
        } else {
            stationLbl.setText("StationID:");
        }
    }

    /**
     * Adds a new station.
     */
    private void handleAddNewStation() {
        if (!isValidStation()) {
            displayInputErrorMsg("Invalid Station ID entered: Please enter a valid Station ID for the selected Station Type");
            return;
        }
        String stn = stationTF.getText();
        if (metarRdo.getSelection()) {
            stn = stn + "#METAR";
        } else if (maritimeRdo.getSelection()) {
            stn = stn + "#MARITIME";
        } else {
            // TODO: Mesonet
        }
        if (macDlg.isExistingStation(stn)) {
            displayInputErrorMsg("The Station, "
                    + stn
                    + ", is already in your Monitoring Area or among your Additional Stations");
            return;
        }
        macDlg.addNewStationAction(stn);

    }

    /**
     * Checks if station is valid.
     * 
     * @return boolean value
     */
    private boolean isValidStation() {
        String stnId = stationTF.getText();
        if (stnId.contains("#") && !mesonetRdo.getSelection()) {
            return false;
        }
        String catalogtypePhrase = "";
        if (metarRdo.getSelection()) {
            catalogtypePhrase = "catalogtype = 1"; // METAR
        } else if (maritimeRdo.isEnabled() && maritimeRdo.getSelection()) {
            catalogtypePhrase = "catalogtype = 33 or catalogtype = 32"; // MARITIME
        } else {
            catalogtypePhrase = "catalogtype = 1000"; // MESONET
            stnId = stnId.substring(0, stnId.indexOf("#"));
        }
        try {
            String sql = "select stationid, catalogtype from common_obs_spatial where ( "
                    + catalogtypePhrase + " ) and stationid = '" + stnId + "'";
            ISpatialQuery sq = SpatialQueryFactory.create();
            Object[] results = sq.dbRequest(sql, "metadata");
            if (results == null) {
                return false;
            }
            if (results.length != 2) {
                return false;
            }
            return true;
            /**
             * TODO: need to add code for handling Mesonet station type
             */
        } catch (Exception e) {
            statusHandler.handle(Priority.ERROR, e.getMessage());
        }
        return false;
    }

    /**
     * Displays input error.
     * 
     * @param msg
     */
    private void displayInputErrorMsg(String msg) {
        MessageBox messageBox = new MessageBox(shell, SWT.ICON_INFORMATION
                | SWT.OK);
        messageBox.setText("Invalid input");
        messageBox.setMessage(msg);
        messageBox.open();
    }

    private MonitorConfigurationManager getConfigManager(AppName app) {
        MonitorConfigurationManager mngr = null;
        if (app == AppName.FOG) {
            mngr = FogMonitorConfigurationManager.getInstance();
        } else if (app == AppName.SAFESEAS) {
            mngr = SSMonitorConfigurationManager.getInstance();
        } else if (app == AppName.SNOW) {
            mngr = SnowMonitorConfigurationManager.getInstance();
        }
        return mngr;
    }
}
