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
 * Apr 23, 2014 3054      skorolev     Added MESONET handling.
 * Apr 28, 2014 3086      skorolev     Removed local getAreaConfigMgr method.
 * Dec 02, 2015 3873      dhladky      Pulled 3841 to 16.1.1.
 * Jun 13, 2018 7023      tgurney      Specify newly added stations as added
 *
 * </pre>
 *
 * @author lvenable
 */
public class AddNewStationDlg extends CaveSWTDialog {
    private final IUFStatusHandler statusHandler = UFStatus
            .getHandler(AddNewStationDlg.class);

    /** Application name. */
    private AppName appName;

    /** METAR radio button. */
    private Button metarRdo;

    /** Maritime button. */
    private Button maritimeRdo;

    /** Station label. */
    private Label stationLbl;

    /** Station text control. */
    private Text stationTF;

    /** Zone */
    private String area;

    /** Call back interface */
    private MonitoringAreaConfigDlg macDlg;

    public AddNewStationDlg(Shell parent, CommonConfig.AppName appName,
            String area, MonitoringAreaConfigDlg macDlg) {
        super(parent, SWT.DIALOG_TRIM, CAVE.DO_NOT_BLOCK);
        setText(appName.toString() + ": Add a New Stn to Monitor Area");
        this.appName = appName;
        this.area = area;
        this.macDlg = macDlg;
    }

    @Override
    protected Layout constructShellLayout() {
        GridLayout mainLayout = new GridLayout(1, false);
        mainLayout.marginHeight = 2;
        mainLayout.marginWidth = 2;
        mainLayout.verticalSpacing = 2;
        return mainLayout;
    }

    @Override
    protected void initializeComponents(Shell shell) {
        setReturnValue(false);
        createTopLabelRadioControls();
        createTextControls();
        createBottomButtons();
        stationLbl.setText("StationID:");
    }

    private void createTopLabelRadioControls() {
        Composite topComp = new Composite(shell, SWT.NONE);
        topComp.setLayout(new GridLayout(2, true));
        topComp.setLayoutData(new GridData(SWT.FILL, SWT.DEFAULT, true, false));

        // Add the label.
        Label topLbl = new Label(topComp, SWT.RIGHT);
        topLbl.setText("Please type in a new: ");
        topLbl.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true, true));

        // Add the radio controls.
        Composite radioComp = new Composite(topComp, SWT.NONE);
        radioComp.setLayout(new GridLayout(1, false));
        metarRdo = new Button(radioComp, SWT.RADIO);
        metarRdo.setText("Metar");
        metarRdo.setSelection(true);

        maritimeRdo = new Button(radioComp, SWT.RADIO);
        maritimeRdo.setText("Maritime");
        maritimeRdo.setEnabled(appName != CommonConfig.AppName.SNOW);

        Button mesonetRdo = new Button(radioComp, SWT.RADIO);
        mesonetRdo.setText("Mesonet");
    }

    private void createTextControls() {
        Composite textComp = new Composite(shell, SWT.NONE);
        textComp.setLayout(new GridLayout(1, true));
        textComp.setLayoutData(
                new GridData(SWT.FILL, SWT.DEFAULT, true, false));
        stationLbl = new Label(textComp, SWT.NONE);
        stationLbl.setLayoutData(
                new GridData(SWT.FILL, SWT.DEFAULT, true, false));
        stationTF = new Text(textComp, SWT.BORDER);
        stationTF.setLayoutData(new GridData(250, SWT.DEFAULT));
    }

    private void createBottomButtons() {
        GridData gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        Composite mainButtonComp = new Composite(shell, SWT.NONE);
        mainButtonComp.setLayout(new GridLayout(1, false));
        mainButtonComp.setLayoutData(gd);
        // Add a separator label.
        Label sepLbl = new Label(mainButtonComp,
                SWT.SEPARATOR | SWT.HORIZONTAL);
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
                /**
                 * for DR #7854: add new station to Monitor Area Config GUI
                 */
                String type;
                if (metarRdo.getSelection()) {
                    type = StationIdXML.METAR;
                } else if (maritimeRdo.getSelection()) {
                    type = StationIdXML.MARITIME;
                } else {
                    type = StationIdXML.MESONET;
                }
                handleAddNewStation(type);
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

    private void handleAddNewStation(String type) {

        if (stationTF.getText().isEmpty()) {
            displayInputErrorMsg("No Station ID entered."
                    + "\nPlease enter a valid Station ID for the selected Station Type.");
            return;
        }
        String stn = stationTF.getText().toUpperCase();
        if (!isValidStation(stn)) {
            displayInputErrorMsg("Invalid Station ID entered: " + stn
                    + " \nPlease enter a valid Station ID for the selected Station Type.");
            return;
        }
        stn = stn + "#" + type;
        if (macDlg.isExistingStation(stn)) {
            displayInputErrorMsg("The Station '" + stn
                    + "' is already in your Monitoring Area or among your Additional Stations.");
            return;
        }
        macDlg.addNewStationAction(stn);
        macDlg.getInstance().addNewStation(area, stn, type, true);
        macDlg.getInstance().getStations().add(stn);
    }

    private boolean isValidStation(String stnId) {
        String catalogtypePhrase = "";
        if (metarRdo.getSelection()) {
            catalogtypePhrase = "catalogtype = 1"; // METAR
        } else if (maritimeRdo.isEnabled() && maritimeRdo.getSelection()) {
            catalogtypePhrase = "catalogtype = 33 or catalogtype = 32"; // MARITIME
        } else {
            catalogtypePhrase = "catalogtype = 1000"; // MESONET
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
        } catch (Exception e) {
            statusHandler.handle(Priority.ERROR,
                    "Can not get " + stnId + " station in the database. ", e);
        }
        return false;
    }

    private void displayInputErrorMsg(String msg) {
        MessageBox messageBox = new MessageBox(shell,
                SWT.ICON_INFORMATION | SWT.OK);
        messageBox.setText("Invalid input");
        messageBox.setMessage(msg);
        messageBox.open();
    }
}
