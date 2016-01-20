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

import java.util.ArrayList;

import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.events.VerifyEvent;
import org.eclipse.swt.events.VerifyListener;
import org.eclipse.swt.graphics.Font;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Layout;
import org.eclipse.swt.widgets.List;
import org.eclipse.swt.widgets.MessageBox;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Text;

import com.raytheon.uf.common.monitor.config.FSSObsMonitorConfigurationManager;
import com.raytheon.uf.common.monitor.data.CommonConfig.AppName;
import com.raytheon.uf.common.monitor.xml.AreaIdXML;
import com.raytheon.uf.common.monitor.xml.AreaIdXML.ZoneType;
import com.raytheon.uf.common.monitor.xml.StationIdXML;
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
 * Apr 23, 2014 3054      skorolev     Fixed issues with removing a new zone from list.
 * Apr 28, 2014 3086      skorolev     Removed local getAreaConfigMgr method.
 * Nov 10, 2014 3741      skorolev     Fixed configXML issue.
 * Aug 17, 2015 3841      skorolev     Made editable a content of ID field.
 * Nov 12, 2015 3841      dhladky      Augmented Slav's update fixes.
 * Dec 02, 2015 3873      dhladky      Pulled 3841 to 16.1.1.
 * 
 * </pre>
 * 
 * @author lvenable
 * @version 1.0
 */
public class EditNewZoneDlg extends CaveSWTDialog {

    /** Call back interface. */
    private final MonitoringAreaConfigDlg macDlg;

    /** Zone list control. */
    private List zoneList;

    /** ID text control. */
    private Text idTF;

    /** Latitude text control. */
    private Text latTF;

    /** Longitude text control. */
    private Text lonTF;

    /** Save button. */
    private Button saveBtn;

    /** Delete button. */
    private Button deleteBtn;

    /** Control font. */
    private Font controlFont;

    /** Bottom label */
    private Label bottomLbl;

    /** Deleted zone */
    private boolean delZone = false;

    /**
     * Constructor.
     * 
     * @param parent
     *            Parent shell.
     * @param appName
     *            Application name.
     * @param monitoringAreaConfigDlg
     */
    public EditNewZoneDlg(Shell parent, AppName appName,
            MonitoringAreaConfigDlg macDlg) {
        super(parent, SWT.DIALOG_TRIM, CAVE.DO_NOT_BLOCK);
        setText(appName.toString() + ": Edit a Newly Added Zone");
        this.macDlg = macDlg;
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
        populate("");
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
        idTF.addVerifyListener(new VerifyListener() {
            @Override
            public void verifyText(VerifyEvent e) {
                e.text = e.text.toUpperCase();
            }
        });

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

        gd = new GridData(SWT.CENTER, SWT.DEFAULT, false, true);
        gd.widthHint = 80;
        gd.verticalIndent = 5;
        saveBtn = new Button(textButtonComp, SWT.PUSH);
        saveBtn.setText("Save");
        saveBtn.setLayoutData(gd);
        saveBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                if (zoneList.getItemCount() != 0) {
                    String area = zoneList.getItem(zoneList.getSelectionIndex());
                    String areaStr = idTF.getText();
                    String latStr = latTF.getText();
                    String lonStr = lonTF.getText();
                    if (macDlg.formIsValid(areaStr, latStr, lonStr)) {
                        saveSelected(area, areaStr, latStr, lonStr);
                    }
                } else {
                    bottomLbl.setText("No zones have been edited.");
                }
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
                delZone = deleteSelected();
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
                .setText("Centroid Lat/Lon use Decimal Degrees, West Longitude negative");
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
                setReturnValue(delZone);
                close();
            }
        });
    }

    /**
     * Populate list of added zones.
     */
    private void populate(String selected) {
        java.util.List<String> newList = new ArrayList<String>();
        java.util.List<AreaIdXML> maList = macDlg.getInstance().getConfigXml()
                .getAreaIds();
        for (AreaIdXML maZone : maList) {
            if (maZone.getCLat() != null) {
                newList.add(maZone.getAreaId());
            }
        }
        java.util.List<AreaIdXML> adtnlList = macDlg.getInstance()
                .getAdjAreaConfigXml().getAreaIds();
        for (AreaIdXML aZone : adtnlList) {
            if (aZone.getCLat() != null) {
                newList.add(aZone.getAreaId());
            }
        }
        zoneList.removeAll();
        zoneList.setItems(newList.toArray(new String[newList.size()]));
        macDlg.populateLeftLists(selected);
    }

    /**
     * Zone selection
     */
    private void handleZoneSelection() {
        String zone = zoneList.getItem(zoneList.getSelectionIndex());
        FSSObsMonitorConfigurationManager cfgMgr = macDlg.getInstance();
        AreaIdXML areaXml = cfgMgr.getAreaXml(zone);
        AreaIdXML adjAreaXml = cfgMgr.getAdjAreaXML(zone);
        if (areaXml != null) {
            idTF.setText(areaXml.getAreaId());
            latTF.setText(String.valueOf(areaXml.getCLat()));
            lonTF.setText(String.valueOf(areaXml.getCLon()));
        } else if (adjAreaXml != null) {
            idTF.setText(adjAreaXml.getAreaId());
            latTF.setText(String.valueOf(adjAreaXml.getCLat()));
            lonTF.setText(String.valueOf(adjAreaXml.getCLon()));
        }
    }

    /**
     * Delete selected zones.
     */
    private boolean deleteSelected() {
        if (zoneList.getItemCount() != 0) {
            if (zoneList.getSelectionIndex() == -1) {
                MessageBox messageBox = new MessageBox(shell,
                        SWT.ICON_INFORMATION | SWT.OK);
                messageBox.setText("Error.");
                messageBox.setMessage("Please select zone to be deleted.");
                messageBox.open();
                zoneList.select(0);
                return false;
            }
            String area = zoneList.getItem(zoneList.getSelectionIndex());
            AreaIdXML zoneXML = null;
            FSSObsMonitorConfigurationManager cfgMgr = macDlg.getInstance();
            if (cfgMgr.getConfigXml().containsArea(area)) {
                zoneXML = cfgMgr.getAreaXml(area);
            } else if (cfgMgr.getAdjAreaConfigXml().containsArea(area)) {
                zoneXML = cfgMgr.getAdjAreaXML(area);
            }
            zoneList.remove(zoneList.getSelectionIndex());
            zoneList.select(0);
            if (zoneList.getItemCount() != 0) {
                handleZoneSelection();
            } else {
                idTF.setText("");
                latTF.setText("");
                lonTF.setText("");
            }
            if (zoneXML != null && macDlg.getMaZones().contains(area)) {
                macDlg.getMaZones().remove(area);
                cfgMgr.removeArea(zoneXML);
            }
            if (zoneXML != null && macDlg.getAdditionalZones().contains(area)) {
                macDlg.getAdditionalZones().remove(area);
                cfgMgr.removeAdjArea(zoneXML);
            }
            macDlg.populateLeftLists("");
            macDlg.maZonesChanged = true;
            return true;
        } else {
            bottomLbl.setText("No zones have been deleted.");
        }
        return false;
    }

    /**
     * Save selected zones.
     * 
     * @param area
     *            Original zone ID
     * @param areaStr
     *            New zone ID
     * @param latStr
     *            Latitude
     * @param lonStr
     *            Longitude
     * @throws NumberFormatException
     */
    private void saveSelected(String area, String areaStr, String latStr,
            String lonStr) throws NumberFormatException {
        FSSObsMonitorConfigurationManager cfgMgr = macDlg.getInstance();
        java.util.List<StationIdXML> stationIds;
        if (macDlg.getMaZones().contains(area)) {
            stationIds = cfgMgr.getAreaXml(area).getStationIds();
        } else if (macDlg.getAdditionalZones().contains(area)) {
            stationIds = cfgMgr.getAdjAreaXML(area).getStationIds();
        } else {
            return;
        }
        double lat = Double.parseDouble(latStr);
        double lon = Double.parseDouble(lonStr);
        if (lat > AddNewZoneDlg.upLatBound || lat < AddNewZoneDlg.lowLatBound || lon > AddNewZoneDlg.upLonBound || lon < AddNewZoneDlg.lowLonBound) {
            macDlg.latLonErrorMsg(latStr, lonStr);
            return;
        }
        ZoneType type = ZoneType.REGULAR;
        if (areaStr.charAt(2) != 'C') {
            type = (ZoneType.MARITIME);
        }
        AreaIdXML areaXML = new AreaIdXML();
        areaXML.setAreaId(areaStr);
        areaXML.setCLat(lat);
        areaXML.setCLon(lon);
        areaXML.setType(type);
        areaXML.setStationIds(stationIds);
        // Replace previously added zone
        if (cfgMgr.getAreaList().contains(area)) {
            if (macDlg.getMaZones().contains(area)) {
                int idx = macDlg.getMaZones().indexOf(area);
                macDlg.getMaZones().set(idx, areaXML.getAreaId());
            }
            cfgMgr.replaceArea(cfgMgr.getAreaXml(area), areaXML);
        } else if (macDlg.getAdditionalZones().contains(area)) {
            if (macDlg.getAdditionalZones().contains(area)) {
                int idx = macDlg.getAdditionalZones().indexOf(area);
                macDlg.getAdditionalZones().set(idx, areaXML.getAreaId());
            }
            cfgMgr.replaceAdjArea(cfgMgr.getAdjAreaXML(area), areaXML);
        }
        populate(areaStr);
        // Return cursor to the list.
        zoneList.select(zoneList.indexOf(areaStr));
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
