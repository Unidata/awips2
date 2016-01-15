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
import org.eclipse.swt.events.VerifyEvent;
import org.eclipse.swt.events.VerifyListener;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Layout;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Text;

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
 * Apr 23, 2014 3054      skorolev     Deleted unnecessary parameter in addArea method.
 * Apr 28, 2014 3086      skorolev     Removed local getAreaConfigMgr method.
 * Feb 10, 2015 3886      skorolev     Added fixed width for dialog.
 * Aug 17, 2015 3841      skorolev     Corrected handleAddNewAction method.
 * Nov 12, 2015 3841      dhladky      Augmented Slav's fix for moving platforms.
 * 
 * </pre>
 * 
 * @author lvenable
 * @version 1.0
 */
public class AddNewZoneDlg extends CaveSWTDialog {

    /** Application name. */
    private final AppName appName;

    /** Marine zone radio button. */
    private Button marineZoneRdo;

    /** County radio button. */
    private Button countyRdo;

    /** ID text control. */
    private Text idTF;

    /** Centroid latitude text control. */
    private Text centroidLatTF;

    /** Centroid longitude text control. */
    private Text centroidLonTF;

    /** Monitoring Area Configuration Dialog. */
    private final MonitoringAreaConfigDlg macDlg;
    
    /** County constant char */
    private static final char C = 'C';
    
    /** Zone constant char */
    private static final char Z = 'Z';
    
    /** Upper Latitude Boundary **/
    private static double upLatBound = 90.0;
    
    /** Lower Latitude Boundary **/
    private static double lowLatBound = -90.0;
    
    /** Upper Longitude Boundary **/
    private static double upLonBound = 180.0;
    
    /** Lower Longitude Boundary **/
    private static double lowLonBound = -180.0;

    /**
     * Constructor.
     * 
     * @param parent
     *            Parent shell.
     * @param appName
     *            Application name.
     */
    public AddNewZoneDlg(Shell parent, AppName appName,
            MonitoringAreaConfigDlg macDlg) {
        super(parent, SWT.DIALOG_TRIM, CAVE.DO_NOT_BLOCK);
        setText(appName.toString() + ": Add a New Zone to Monitor Area.");
        this.appName = appName;
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
        if (!appName.equals(AppName.SNOW)) {
            Composite radioComp = new Composite(topComp, SWT.NONE);
            radioComp.setLayout(new GridLayout(1, false));
            marineZoneRdo = new Button(radioComp, SWT.RADIO);
            marineZoneRdo.setText("Marine Zone");
            marineZoneRdo.setSelection(true);
            countyRdo = new Button(radioComp, SWT.RADIO);
            countyRdo.setText("County");
        }
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
        gd.widthHint = 200;
        Label idLbl = new Label(textComp, SWT.RIGHT);
        if (appName.equals(AppName.SNOW)) {
            idLbl.setText("Id (e.g. AMC080):");
        } else {
            idLbl.setText("Id (e.g. AMZ080):");
        }
        idLbl.setLayoutData(gd);

        idTF = new Text(textComp, SWT.BORDER);
        idTF.setLayoutData(new GridData(120, SWT.DEFAULT));
        idTF.addVerifyListener(new VerifyListener() {
            @Override
            public void verifyText(VerifyEvent e) {
                e.text = e.text.toUpperCase();
            }
        });

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
        useDecimalLbl
                .setText("Use Decimal Degrees. West Longitude is negative.");
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
                String areaId = idTF.getText();
                String latString = centroidLatTF.getText();
                String lonString = centroidLonTF.getText();
                if (macDlg.formIsValid(areaId, latString, lonString)) {
                    handleAddNewAction(areaId, latString, lonString);
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
     * Adds a new zone.
     * 
     * @param latString
     * @param lonString
     */
    private void handleAddNewAction(String areaId, String latString,
            String lonString) throws NumberFormatException {

        ZoneType type = ZoneType.REGULAR;
        char charAt = idTF.getText().charAt(2);
        if (!appName.equals(AppName.SNOW)) {
            if (marineZoneRdo.getSelection()) {
                type = ZoneType.MARITIME;
            }
            // correct zone type
            if (marineZoneRdo.getSelection() && charAt != Z) {
                String z = idTF.getText().substring(2).replace(charAt, Z);
                idTF.setText(idTF.getText().substring(0, 2) + z);
            }
            if (countyRdo.getSelection() && charAt != C) {
                String c = idTF.getText().substring(2).replace(charAt, C);
                idTF.setText(idTF.getText().substring(0, 2) + c);
            }
        } else if (appName.equals(AppName.SNOW) && charAt != C) {
            String c = idTF.getText().substring(2).replace(charAt, C);
            idTF.setText(idTF.getText().substring(0, 2) + c);
        }
        double lat = Double.parseDouble(latString.trim());
        double lon = Double.parseDouble(lonString.trim());
        if (lat > upLatBound || lat < lowLatBound || lon > upLonBound || lon < lowLonBound) {
            macDlg.latLonErrorMsg(latString, lonString);
            return;
        }
        areaId = idTF.getText();
        if (macDlg.isExistingZone(areaId)) {
            macDlg.displayInputErrorMsg("The Area ID, "
                    + areaId
                    + ", is already in your Monitoring Area or among your Additional Zones.");
            return;
        } else {
            macDlg.configMgr.addNewArea(areaId, lat, lon, type);
            macDlg.addZoneToMA(areaId);
        }
    }
}