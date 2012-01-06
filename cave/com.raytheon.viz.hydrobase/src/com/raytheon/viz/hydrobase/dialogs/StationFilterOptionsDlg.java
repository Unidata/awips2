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
package com.raytheon.viz.hydrobase.dialogs;

import java.util.ArrayList;

import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.graphics.Font;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Layout;
import org.eclipse.swt.widgets.List;
import org.eclipse.swt.widgets.MessageBox;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Text;

import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.viz.hydrobase.HydroStationDataManager;
import com.raytheon.viz.hydrobase.listeners.IStationFilterListener;
import com.raytheon.viz.hydrocommon.HydroConstants;
import com.raytheon.viz.hydrocommon.datamanager.AddModifyLocationDataManager;
import com.raytheon.viz.hydrocommon.util.HydroDataUtils;
import com.raytheon.viz.hydrocommon.whfslib.GeoUtil;
import com.raytheon.viz.ui.dialogs.CaveSWTDialog;

/**
 * This class displays the Station Filter Options dialog.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date			Ticket#		Engineer	Description
 * ------------	----------	-----------	--------------------------
 * Sep 9, 2008				lvenable	Initial creation
 * Jun 17,2010	 #5414 	    lbousaidi   Apply button closing         
 * 
 * </pre>
 * 
 * @author lvenable
 * @version 1.0
 */
public class StationFilterOptionsDlg extends CaveSWTDialog {

    /**
     * Control font.
     */
    private Font controlFont;

    /**
     * HSA list control.
     */
    private List hsaList;

    /**
     * SHEF post check box.
     */
    private Button shefPostChk;

    /**
     * SHEF no post check box.
     */
    private Button shefNoPostChk;

    /**
     * Latitude center text control.
     */
    private Text latCenterTF;

    /**
     * Longitude center text control.
     */
    private Text lonCenterTF;

    /**
     * Latitude offset text control.
     */
    private Text latOffsetTF;

    /**
     * Longitude offset text control.
     */
    private Text lonOffsetTF;

    /**
     * Enable check box.
     */
    private Button enableChk;

    /**
     * List of listeners to filter changes
     */
    private ArrayList<IStationFilterListener> filterListeners;

    /**
     * Constructor.
     * 
     * @param parent
     *            Parent shell.
     */
    public StationFilterOptionsDlg(Shell parent) {
        super(parent);
        setText("Station List Filter Options");
    }

    @Override
    protected Layout constructShellLayout() {
        // Create the main layout for the shell.
        GridLayout mainLayout = new GridLayout(1, false);
        mainLayout.marginHeight = 2;
        mainLayout.marginWidth = 2;
        mainLayout.verticalSpacing = 2;
        return mainLayout;
    }

    @Override
    protected void disposed() {
        controlFont.dispose();
    }

    @Override
    protected void initializeComponents(Shell shell) {
        setReturnValue(false);

        controlFont = new Font(shell.getDisplay(), "Monospace", 10, SWT.NORMAL);

        createTopFilterControls();
        createFilterLatLonControls();
        createBottomButtons();

        loadStaticData();

        updateDisplay();
    }

    /**
     * Create the filter controls at the top of the dialog.
     */
    private void createTopFilterControls() {
        // ----------------------------------------------------
        // Create the main composite for the top controls.
        // ----------------------------------------------------
        GridData gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        Composite topComp = new Composite(shell, SWT.NONE);
        topComp.setLayout(new GridLayout(2, false));
        topComp.setLayoutData(gd);

        // ---------------------------------------
        // Create the Filter by HSA group
        // ---------------------------------------
        Group hsaFilterGroup = new Group(topComp, SWT.NONE);
        hsaFilterGroup.setLayout(new GridLayout(1, false));
        gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        hsaFilterGroup.setLayoutData(gd);
        hsaFilterGroup.setText(" Filter by HSA ");

        gd = new GridData(SWT.FILL, SWT.FILL, true, true);
        gd.widthHint = 125;
        gd.heightHint = 150;
        hsaList = new List(hsaFilterGroup, SWT.BORDER | SWT.MULTI
                | SWT.V_SCROLL);
        hsaList.setLayoutData(gd);
        hsaList.setFont(controlFont);

        // ---------------------------------------
        // Create the Filter by SHEF Post switch
        // ---------------------------------------
        Group shefFilterGroup = new Group(topComp, SWT.NONE);
        shefFilterGroup.setLayout(new GridLayout(1, false));
        gd = new GridData(SWT.FILL, SWT.FILL, true, true);
        shefFilterGroup.setLayoutData(gd);
        shefFilterGroup.setText(" Filter by SHEF Post Switch ");

        // TODO : NOTE: I'm not sure if the code is using check boxes
        // as radio buttons. I coded the GUI off of a screen
        // shot. I don't know if it makes sense to have
        // SHEF post and no post checked.

        gd = new GridData(SWT.FILL, SWT.BOTTOM, true, true);
        shefPostChk = new Button(shefFilterGroup, SWT.CHECK);
        shefPostChk.setText("Show SHEF Post");
        // shefPostChk.setSelection(true);
        shefPostChk.setLayoutData(gd);

        gd = new GridData(SWT.FILL, SWT.TOP, true, true);
        shefNoPostChk = new Button(shefFilterGroup, SWT.CHECK);
        shefNoPostChk.setText("Show SHEF No Post");
        shefNoPostChk.setLayoutData(gd);
    }

    /**
     * Create the Filter Latitude and Longitude controls.
     */
    private void createFilterLatLonControls() {
        Group latLonFilterGroup = new Group(shell, SWT.NONE);
        latLonFilterGroup.setLayout(new GridLayout(3, false));
        GridData gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        latLonFilterGroup.setLayoutData(gd);
        latLonFilterGroup.setText(" Filter by Lat/Lon ");

        gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        Label centerLbl = new Label(latLonFilterGroup, SWT.CENTER);
        centerLbl.setText("Center");
        centerLbl.setLayoutData(gd);

        // Filler label
        new Label(latLonFilterGroup, SWT.NONE);

        gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        Label offsetLbl = new Label(latLonFilterGroup, SWT.CENTER);
        offsetLbl.setText("Offset");
        offsetLbl.setLayoutData(gd);

        gd = new GridData(150, SWT.DEFAULT);
        latCenterTF = new Text(latLonFilterGroup, SWT.BORDER);
        latCenterTF.setLayoutData(gd);

        gd = new GridData(120, SWT.DEFAULT);
        Label latitudeLbl = new Label(latLonFilterGroup, SWT.LEFT);
        latitudeLbl.setText("Latitude");
        latitudeLbl.setLayoutData(gd);

        gd = new GridData(150, SWT.DEFAULT);
        latOffsetTF = new Text(latLonFilterGroup, SWT.BORDER);
        latOffsetTF.setLayoutData(gd);

        gd = new GridData(150, SWT.DEFAULT);
        lonCenterTF = new Text(latLonFilterGroup, SWT.BORDER);
        lonCenterTF.setLayoutData(gd);

        gd = new GridData(120, SWT.DEFAULT);
        Label longitudeLbl = new Label(latLonFilterGroup, SWT.LEFT);
        longitudeLbl.setText("Longitude");
        longitudeLbl.setLayoutData(gd);

        gd = new GridData(150, SWT.DEFAULT);
        lonOffsetTF = new Text(latLonFilterGroup, SWT.BORDER);
        lonOffsetTF.setLayoutData(gd);

        // Filler label
        new Label(latLonFilterGroup, SWT.NONE);

        enableChk = new Button(latLonFilterGroup, SWT.CHECK);
        enableChk.setText("Enable");
    }

    /**
     * Create the buttons at the bottom of the dialog.
     */
    private void createBottomButtons() {
        GridData gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        Composite mainButtonComp = new Composite(shell, SWT.NONE);
        mainButtonComp.setLayout(new GridLayout(1, false));
        mainButtonComp.setLayoutData(gd);

        gd = new GridData(SWT.CENTER, SWT.DEFAULT, false, false);
        Composite buttonComp = new Composite(shell, SWT.NONE);
        buttonComp.setLayout(new GridLayout(2, false));
        buttonComp.setLayoutData(gd);

        gd = new GridData(100, SWT.DEFAULT);
        Button applyBtn = new Button(buttonComp, SWT.PUSH);
        applyBtn.setText("Apply");
        applyBtn.setLayoutData(gd);
        applyBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                if (saveSettings()) {
                    fireUpdateEvent();                    
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
                shell.dispose();
            }
        });
    }

    private void loadStaticData() {
        // Load HSAs
        hsaList.removeAll();
        try {
            for (String currHSA : AddModifyLocationDataManager.getInstance()
                    .getHSAsForFilter()) {
                hsaList.add(currHSA);
            }
        } catch (VizException e) {
            e.printStackTrace();
        }
    }

    /**
     * Displays the currently selected options
     */
    private void updateDisplay() {
        HydroStationDataManager man = HydroStationDataManager.getInstance();

        // Set selected HSAs
        ArrayList<String> filter = man.getHsaFilter();
        for (int i = 0; i < hsaList.getItemCount(); i++) {
            if (filter.contains(hsaList.getItem(i))) {
                hsaList.select(i);
                hsaList.showSelection();
            }
        }
        hsaList.showSelection();

        // Set Post/No Post info
        shefPostChk.setSelection(man.isShowPost());
        shefNoPostChk.setSelection(man.isShowNoPost());

        // Get Lat/Lon info
        latOffsetTF
                .setText(HydroDataUtils.getDisplayString(man.getLatOffset()));
        lonOffsetTF
                .setText(HydroDataUtils.getDisplayString(man.getLonOffset()));

        latCenterTF
                .setText((man.getLatCenter() != HydroConstants.MISSING_VALUE) ? GeoUtil
                        .getInstance().cvt_latlon_from_double(
                                man.getLatCenter())
                        : "");
        lonCenterTF
                .setText((man.getLonCenter() != HydroConstants.MISSING_VALUE) ? GeoUtil
                        .getInstance().cvt_latlon_from_double(
                                man.getLonCenter())
                        : "");

        enableChk.setSelection(man.isFilterByLatLon());
    }

    /**
     * Saves the settings to the Station List Data Manager
     */
    private boolean saveSettings() {
        HydroStationDataManager man = HydroStationDataManager.getInstance();

        // Set selected HSAs
        ArrayList<String> filter = new ArrayList<String>();
        for (String currHSA : hsaList.getSelection()) {
            filter.add(currHSA);
        }
        man.setHsaFilter(filter);

        // Set Post/No Post info
        man.setShowPost(shefPostChk.getSelection());
        man.setShowNoPost(shefNoPostChk.getSelection());

        // Get Lat/Lon info
        Double temp;

        temp = HydroDataUtils.getDoubleFromTF(shell, latOffsetTF,
                "Latitude Offset");
        if (temp == null) {
            return false;
        }
        man.setLatOffset(temp);

        temp = HydroDataUtils.getDoubleFromTF(shell, lonOffsetTF,
                "Longitude Offset");
        if (temp == null) {
            return false;
        }
        man.setLonOffset(temp);

        // Latitude
        String latTxt = latCenterTF.getText();
        double lat = HydroConstants.MISSING_VALUE;
        if (!latTxt.equals("")) {
            boolean invalidLat = false;

            try {
                lat = GeoUtil.getInstance().cvt_spaced_format(latTxt, 0);
            } catch (Exception e) {
                invalidLat = true;
            }

            if ((lat < -90) || (lat > 90) || invalidLat) {
                MessageBox mb = new MessageBox(shell, SWT.ICON_ERROR | SWT.OK);
                mb.setText("Invalid Value");
                mb
                        .setMessage("Please enter a VALID (-90 to 90) Latitude\nin the form: DD MM SS");
                mb.open();

                return false;
            }
        }
        man.setLatCenter(lat);

        // Longitude
        String lonTxt = lonCenterTF.getText();
        double lon = HydroConstants.MISSING_VALUE;
        if (!lonTxt.equals("")) {
            boolean invalidLon = false;

            try {
                lon = GeoUtil.getInstance().cvt_spaced_format(lonTxt, 0);
            } catch (Exception e) {
                invalidLon = true;
                e.printStackTrace();
            }

            if ((lon > 180) || (lon < -180) || invalidLon) {
                MessageBox mb = new MessageBox(shell, SWT.ICON_ERROR | SWT.OK);
                mb.setText("Invalid Value");
                mb
                        .setMessage("Please enter a VALID (-180 to 180) Longitude\nin the form: DD MM SS");
                mb.open();

                return false;
            }
        }
        man.setLonCenter(lon);

        man.setFilterByLatLon(enableChk.getSelection());

        return true;
    }

    /**
     * Notifies the listeners of the fcst group change
     */
    private void fireUpdateEvent() {
        for (IStationFilterListener currListener : filterListeners) {
            currListener.notifyFilterChange();
        }
    }

    /**
     * Adds a listener for fcst group changes
     * 
     * @param listener
     */
    public void addListener(IStationFilterListener listener) {
        if (filterListeners == null) {
            filterListeners = new ArrayList<IStationFilterListener>();
        }

        filterListeners.add(listener);
    }

    /**
     * Removes a listener for fcst group changes
     * 
     * @param listener
     */
    public void removeListener(IStationFilterListener listener) {
        filterListeners.remove(listener);
    }
}
