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
import org.eclipse.swt.graphics.Font;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Layout;
import org.eclipse.swt.widgets.List;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Text;

import com.raytheon.uf.common.monitor.config.FogMonitorConfigurationManager;
import com.raytheon.uf.common.monitor.config.MonitorConfigurationManager;
import com.raytheon.uf.common.monitor.config.SSMonitorConfigurationManager;
import com.raytheon.uf.common.monitor.config.SnowMonitorConfigurationManager;
import com.raytheon.uf.common.monitor.data.CommonConfig.AppName;
import com.raytheon.uf.common.monitor.xml.AreaIdXML;
import com.raytheon.uf.common.monitor.xml.AreaIdXML.ZoneType;
import com.raytheon.viz.ui.dialogs.CaveSWTDialog;

/**
 * Dialog for editing new zones.
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
public class EditNewZoneDlg extends CaveSWTDialog {
    /**
     * Zone list control.
     */
    private List zoneList;

    /**
     * ID text control.
     */
    private Text idTF;

    /**
     * Latitude text control.
     */
    private Text latTF;

    /**
     * Longitude text control.
     */
    private Text lonTF;

    /**
     * Save button.
     */
    private Button saveBtn;

    /**
     * Delete button.
     */
    private Button deleteBtn;

    /**
     * Control font.
     */
    private Font controlFont;

    /**
     * Marine station radio button.
     */
    private Button marineRdo;

    /**
     * None Marine station radio button.
     */
    private Button nonMarineRdo;

    /**
     * Area configuration manager.
     */
    private MonitorConfigurationManager configMan;

    /**
     * Bottom label
     */
    private Label bottomLbl;

    /**
     * Constructor.
     * 
     * @param parent
     *            Parent shell.
     * @param appName
     *            Application name.
     */
    public EditNewZoneDlg(Shell parent, AppName appName) {
        super(parent, SWT.DIALOG_TRIM, CAVE.DO_NOT_BLOCK);
        setText(appName.toString() + ": Edit a Newly Added Zone");
        configMan = this.getConfigManager(appName);
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
        controlFont = new Font(shell.getDisplay(), "Monospace", 10, SWT.NORMAL);
        createTopLabel();
        createListAndTextControls();
        createBottomLabel();
        createCloseButton();
        populate();
    }

    /**
     * Create the top label.
     */
    private void createTopLabel() {
        Composite labelComp = new Composite(shell, SWT.NONE);
        labelComp.setLayout(new GridLayout(1, false));
        labelComp
                .setLayoutData(new GridData(SWT.FILL, SWT.DEFAULT, true, false));
        Label topLbl = new Label(labelComp, SWT.CENTER);
        topLbl.setText("Select Zone from the list box to edit");
        topLbl.setLayoutData(new GridData(SWT.FILL, SWT.DEFAULT, true, false));
    }

    /**
     * Create the list and text controls.
     */
    private void createListAndTextControls() {
        Composite mainComp = new Composite(shell, SWT.NONE);
        GridLayout gl = new GridLayout(2, false);
        gl.horizontalSpacing = 10;
        mainComp.setLayout(gl);
        // Add the Zone list control
        GridData gd = new GridData(SWT.FILL, SWT.FILL, true, true);
        gd.widthHint = 185;
        gd.heightHint = 220;
        zoneList = new List(mainComp, SWT.BORDER | SWT.SINGLE | SWT.V_SCROLL);
        zoneList.setFont(controlFont);
        zoneList.setLayoutData(gd);
        zoneList.addSelectionListener(new SelectionAdapter() {
            /*
             * (non-Javadoc)
             * 
             * @see
             * org.eclipse.swt.events.SelectionAdapter#widgetSelected(org.eclipse
             * .swt.events.SelectionEvent)
             */
            @Override
            public void widgetSelected(SelectionEvent e) {
                handleZoneSelection();
            }
        });
        // Add the text controls and the Save/Delete buttons
        Composite textButtonComp = new Composite(mainComp, SWT.NONE);
        textButtonComp.setLayout(new GridLayout(2, true));

        int textWidth = 200;

        gd = new GridData();
        gd.horizontalSpan = 2;
        Label idLbl = new Label(textButtonComp, SWT.NONE);
        idLbl.setText("Id:");
        idLbl.setLayoutData(gd);

        gd = new GridData(textWidth, SWT.DEFAULT);
        gd.horizontalSpan = 2;
        idTF = new Text(textButtonComp, SWT.BORDER);
        idTF.setLayoutData(gd);

        gd = new GridData();
        gd.horizontalSpan = 2;
        gd.verticalIndent = 5;
        Label latLbl = new Label(textButtonComp, SWT.NONE);
        latLbl.setText("Lat:");
        latLbl.setLayoutData(gd);

        gd = new GridData(textWidth, SWT.DEFAULT);
        gd.horizontalSpan = 2;
        latTF = new Text(textButtonComp, SWT.BORDER);
        latTF.setLayoutData(gd);

        gd = new GridData();
        gd.horizontalSpan = 2;
        gd.verticalIndent = 5;
        Label lonLbl = new Label(textButtonComp, SWT.NONE);
        lonLbl.setText("Lon:");
        lonLbl.setLayoutData(gd);

        gd = new GridData(textWidth, SWT.DEFAULT);
        gd.horizontalSpan = 2;
        lonTF = new Text(textButtonComp, SWT.BORDER);
        lonTF.setLayoutData(gd);

        gd = new GridData();
        gd.horizontalSpan = 2;
        gd.verticalIndent = 15;
        marineRdo = new Button(textButtonComp, SWT.RADIO);
        marineRdo.setLayoutData(gd);
        marineRdo.setSelection(false);
        marineRdo.setText("Marine Station");

        gd = new GridData();
        gd.horizontalSpan = 2;
        nonMarineRdo = new Button(textButtonComp, SWT.RADIO);
        nonMarineRdo.setLayoutData(gd);
        nonMarineRdo.setSelection(true);
        nonMarineRdo.setText("Non-Marine Station");

        gd = new GridData(SWT.CENTER, SWT.DEFAULT, false, true);
        gd.widthHint = 80;
        gd.verticalIndent = 5;
        saveBtn = new Button(textButtonComp, SWT.PUSH);
        saveBtn.setText("Save");
        saveBtn.setLayoutData(gd);
        saveBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                saveSelected();
            }
        });
        gd = new GridData(SWT.CENTER, SWT.DEFAULT, false, true);
        gd.widthHint = 80;
        gd.verticalIndent = 5;
        deleteBtn = new Button(textButtonComp, SWT.PUSH);
        deleteBtn.setText("Delete");
        deleteBtn.setLayoutData(gd);
        deleteBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                deleteSelected();
            }
        });
    }

    /**
     * Create the bottom label centroid label.
     */
    private void createBottomLabel() {
        Composite labelComp = new Composite(shell, SWT.NONE);
        labelComp.setLayout(new GridLayout(1, false));
        labelComp
                .setLayoutData(new GridData(SWT.FILL, SWT.DEFAULT, true, false));
        bottomLbl = new Label(labelComp, SWT.NONE);
        bottomLbl
                .setText("Centriod Lat/Lon use Decimal Degrees, West Longitude negative");
    }

    /**
     * Create the Close button.
     */
    private void createCloseButton() {
        GridData gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        Composite buttonComp = new Composite(shell, SWT.NONE);
        buttonComp.setLayout(new GridLayout(1, false));
        buttonComp.setLayoutData(gd);
        // Add a separator label.
        Label sepLbl = new Label(buttonComp, SWT.SEPARATOR | SWT.HORIZONTAL);
        sepLbl.setLayoutData(gd);
        gd = new GridData(SWT.CENTER, SWT.DEFAULT, false, false);
        gd.widthHint = 100;
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
     * Populate list of added zones.
     */
    private void populate() {
        java.util.List<String> newList = configMan.getAddedZones();
        zoneList.setItems(newList.toArray(new String[newList.size()]));
    }

    /**
     * Zone selection
     */
    private void handleZoneSelection() {
        String zone = zoneList.getItem(zoneList.getSelectionIndex());
        AreaIdXML areaXml = configMan.getAreaXml(zone);
        // DR #7343: a null areaXml causes an "Unhandled event loop exception"
        if (areaXml != null) {
            idTF.setText(areaXml.getAreaId());
            latTF.setText(String.valueOf(areaXml.getCLat()));
            lonTF.setText(String.valueOf(areaXml.getCLon()));
            if (areaXml.getType() == ZoneType.REGULAR) {
                nonMarineRdo.setSelection(true);
                marineRdo.setSelection(false);
            } else {
                nonMarineRdo.setSelection(false);
                marineRdo.setSelection(true);
            }
        }
    }

    /**
     * Delete selected zones.
     */
    // TODO: Delete zone from left list in the Area Configuration dialog.
    private void deleteSelected() {
        if (zoneList.getItemCount() != 0) {
            String area = zoneList.getItem(zoneList.getSelectionIndex());
            zoneList.remove(zoneList.getSelectionIndex());
            configMan.removeArea(area);
            idTF.setText("");
            latTF.setText("");
            lonTF.setText("");
        } else {
            bottomLbl.setText("No zones have been added.");
        }
    }

    /**
     * Save selected zones.
     */
    private void saveSelected() {

        if (zoneList.getItemCount() != 0) {
            String area = zoneList.getItem(zoneList.getSelectionIndex());
            double lat;
            if (!latTF.getText().isEmpty()) {
                lat = Double.parseDouble(latTF.getText());
            } else {
                // wrong value will be filtered when save
                lat = 9999.0;
            }
            double lon;
            if (!lonTF.getText().isEmpty()) {
                lon = Double.parseDouble(lonTF.getText());
            } else {
                // wrong value will be filtered when save
                lon = 9999.0;
            }
            ZoneType type = ZoneType.REGULAR;
            if (marineRdo.getSelection()) {
                type = ZoneType.MARITIME;
            }
            configMan.removeArea(area);
            configMan.removeAddedArea(area);
            configMan.addArea(idTF.getText(), lat, lon, type, false);
            populate();
        } else {
            bottomLbl.setText("No zones have been added.");
        }
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

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.viz.ui.dialogs.CaveSWTDialogBase#disposed()
     */
    @Override
    protected void disposed() {
        controlFont.dispose();
    }

}
