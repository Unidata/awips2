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

import com.raytheon.uf.common.monitor.config.FogMonitorConfigurationManager;
import com.raytheon.uf.common.monitor.config.MonitorConfigurationManager;
import com.raytheon.uf.common.monitor.config.SSMonitorConfigurationManager;
import com.raytheon.uf.common.monitor.config.SnowMonitorConfigurationManager;
import com.raytheon.uf.common.monitor.data.CommonConfig.AppName;
import com.raytheon.uf.common.monitor.xml.AreaIdXML.ZoneType;
import com.raytheon.viz.ui.dialogs.CaveSWTDialog;

/**
 * Dialog to display the control for adding a new zone.
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
public class AddNewZoneDlg extends CaveSWTDialog {

    /**
     * Application name.
     */
    private AppName appName;

    /**
     * Marine zone radio button.
     */
    private Button marineZoneRdo;

    /**
     * County radio button.
     */
    private Button countyRdo;

    /**
     * ID text control.
     */
    private Text idTF;

    /**
     * Centroid latitude text control.
     */
    private Text centroidLatTF;

    /**
     * Centroid longitude text control.
     */
    private Text centroidLonTF;

    /**
     * Call back interface.
     */
    private INewZoneStnAction macDlg;

    /**
     * Area configuration manager.
     */
    private MonitorConfigurationManager configMan;

    /**
     * Constructor.
     * 
     * @param parent
     *            Parent shell.
     * @param appName
     *            Application name.
     */
    public AddNewZoneDlg(Shell parent, AppName appName, INewZoneStnAction macDlg) {
        super(parent, SWT.DIALOG_TRIM, CAVE.DO_NOT_BLOCK);
        setText(appName.toString() + ": Add a New Zone to Monitor Area.");
        this.appName = appName;
        this.macDlg = macDlg;
        configMan = getConfigManager(appName);
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
        if (appName != AppName.SNOW) {
            createTopZoneCountyControls();
        } else {
            createCountyZoneLabels();
        }
        createTextControls();
        createBottomButtons();
    }

    /**
     * Creates the top zone county controls.
     */
    private void createTopZoneCountyControls() {
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
        marineZoneRdo = new Button(radioComp, SWT.RADIO);
        marineZoneRdo.setText("Marine Zone");
        marineZoneRdo.setSelection(true);
        countyRdo = new Button(radioComp, SWT.RADIO);
        countyRdo.setText("County");
    }

    /**
     * Creates the county zone labels.
     */
    private void createCountyZoneLabels() {
        Composite labelComp = new Composite(shell, SWT.NONE);
        labelComp.setLayout(new GridLayout(1, false));
        labelComp
                .setLayoutData(new GridData(SWT.FILL, SWT.DEFAULT, true, false));
        Label topLbl = new Label(labelComp, SWT.CENTER);
        topLbl.setText("Please type in a new county/zone");
        topLbl.setLayoutData(new GridData(SWT.FILL, SWT.DEFAULT, true, false));
    }

    /**
     * Creates the text controls.
     */
    private void createTextControls() {
        Composite textComp = new Composite(shell, SWT.NONE);
        GridLayout gl = new GridLayout(2, false);
        gl.verticalSpacing = 10;
        textComp.setLayout(gl);
        textComp.setLayoutData(new GridData(SWT.FILL, SWT.DEFAULT, true, false));

        GridData gd = new GridData(SWT.FILL, SWT.CENTER, true, true);
        Label idLbl = new Label(textComp, SWT.RIGHT);
        idLbl.setText("Id (e.g. AMZ080):");
        idLbl.setLayoutData(gd);

        idTF = new Text(textComp, SWT.BORDER);
        idTF.setLayoutData(new GridData(120, SWT.DEFAULT));

        gd = new GridData(SWT.FILL, SWT.CENTER, true, true);
        Label centroidLatLbl = new Label(textComp, SWT.RIGHT);
        centroidLatLbl.setText("Centroid Lat (e.g. 29.198):");
        centroidLatLbl.setLayoutData(gd);

        centroidLatTF = new Text(textComp, SWT.BORDER);
        centroidLatTF.setLayoutData(new GridData(120, SWT.DEFAULT));

        gd = new GridData(SWT.FILL, SWT.CENTER, true, true);
        Label centroidLonLbl = new Label(textComp, SWT.RIGHT);
        centroidLonLbl.setText("Centroid Lon (e.g. -71.75):");
        centroidLonLbl.setLayoutData(gd);

        centroidLonTF = new Text(textComp, SWT.BORDER);
        centroidLonTF.setLayoutData(new GridData(120, SWT.DEFAULT));
        /*
         * Create the Use Decimal label.
         */
        gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        gd.horizontalSpan = 2;
        Label useDecimalLbl = new Label(textComp, SWT.CENTER);
        useDecimalLbl.setText("Use Decimal Degrees, West Longitude negative");
        useDecimalLbl.setLayoutData(gd);
    }

    /**
     * Creates the bottom Add and Close buttons.
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
                String latString = centroidLatTF.getText();
                String lonString = centroidLonTF.getText();
                handleAddNewAction(latString, lonString);
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
     * Adds a new zone.
     * 
     * @param latString
     * @param lonString
     */
    private void handleAddNewAction(String latString, String lonString) {
        String areaId = idTF.getText();
        if (areaId.equals("") || areaId.length() != 6
                || (areaId.charAt(2) != 'C' && areaId.charAt(2) != 'Z')) {
            displayInputErrorMsg("Invalid Area ID entered. Please enter a correctly formatted Area ID");
            return;
        }
        if (macDlg.isExistingZone(areaId)) {
            displayInputErrorMsg("The Area ID, "
                    + areaId
                    + ", is already in your Monitoring Area or among your Additional Zones");
            return;
        }
        if (latString == null || latString.isEmpty() || lonString == null
                || lonString.isEmpty()) {
            MessageBox messageBox = new MessageBox(shell, SWT.ICON_INFORMATION
                    | SWT.OK);
            messageBox.setText("Invalid Lat/Lon");
            messageBox
                    .setMessage("Invalid Lat/Lon entered.  Please enter correctly formatted Lat and Lon values");
            messageBox.open();
            return;
        } else {
            try {
                double lat = Double.parseDouble(latString.trim());
                double lon = Double.parseDouble(lonString.trim());
                ZoneType type = ZoneType.REGULAR;
                if (appName != AppName.SNOW) {
                    if (marineZoneRdo.getSelection()) {
                        type = ZoneType.MARITIME;
                    }
                }
                configMan.addArea(areaId, lat, lon, type, false);
                macDlg.addNewZoneAction(areaId, centroidLatTF.getText(),
                        centroidLonTF.getText());
            } catch (NumberFormatException e) {
                MessageBox messageBox = new MessageBox(shell,
                        SWT.ICON_INFORMATION | SWT.OK);
                messageBox.setText("Invalid Lat/Lon");
                messageBox
                        .setMessage("Invalid Lat/Lon entered.  Please enter correctly formatted Lat and Lon values");
                messageBox.open();
                return;
            }
        }
    }

    /**
     * Displays Input Error Message
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

    /**
     * Gets Configuration Manager.
     * 
     * @param app
     * @return manager
     */
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
