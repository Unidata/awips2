/**
 * This code has unlimited rights, and is provided "as is" by the National Centers 
 * for Environmental Prediction, without warranty of any kind, either expressed or implied, 
 * including but not limited to the implied warranties of merchantability and/or fitness 
 * for a particular purpose.
 * 
 * This code has been developed by the NCEP-SIB for use in the AWIPS2 system.
 * 
 **/
package gov.noaa.nws.ncep.viz.rtkp.controls;

import gov.noaa.nws.ncep.viz.rtkp.palette.GeoMagRTKpDataBlockWindow;
import gov.noaa.nws.ncep.viz.rtkp.util.RTKpUtil;
import gov.noaa.nws.ncep.viz.rtkp.util.RTKpUtil.GeoMagStationType;

import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.eclipse.jface.dialogs.IDialogConstants;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Event;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.Listener;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Table;
import org.eclipse.swt.widgets.TableItem;

import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.viz.ui.dialogs.CaveJFACEDialog;

/**
 * A dialog to Retrieve PGEN activities from EDEX.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date       	 Ticket#		Engineer	Description
 * ------------	 ----------	-----------	-----------------------------------
 * April 4, 2014 1122		sgurung  	Initial creation
 * 
 * </pre>
 * 
 * @author sgurung
 * @version 1.0
 */
public class EditNetworkDialog extends CaveJFACEDialog {
    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(EditNetworkDialog.class);

    private String title = null;

    private Shell shell;

    private Button saveBtn = null;

    private Button cancelBtn = null;

    private Button begCurSynPeriodBtn;

    private Button begPrevSynPeriodBtn;

    private Button prevSynPeriodOnlyBtn;

    private List<String> kpStations = null;

    private HashMap<String, Map<String, Object>> curKpStationsStates = null;

    private GeoMagRTKpDataBlockWindow dbWindow = null;

    private int kpState = 0;

    private static final int SAVE_ID = IDialogConstants.CLIENT_ID + 7687;

    private static final String SAVE_LABEL = "Save";

    private static final int CANCEL_ID = IDialogConstants.CLIENT_ID + 7688;

    private static final String CANCEL_LABEL = "Exit";

    /** Table control. */
    private Table table;

    /*
     * Constructor
     */
    public EditNetworkDialog(Shell parShell, String btnName,
            GeoMagRTKpDataBlockWindow dbWindow) throws VizException {

        super(parShell);

        setTitle(btnName);

        this.kpStations = RTKpUtil.getGeoMagStationCodes(GeoMagStationType.KP);
        this.curKpStationsStates = RTKpUtil.getStationsStatesMap(
                GeoMagStationType.KP, 0);
        this.dbWindow = dbWindow;

    }

    /*
     * Set up the file mode.
     */
    private void setTitle(String btnName) {
        title = "Edit Network";

    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * org.eclipse.jface.window.Window#configureShell(org.eclipse.swt.widgets
     * .Shell)
     */
    @Override
    protected void configureShell(Shell shell) {
        super.configureShell(shell);
        this.setShellStyle(SWT.RESIZE | SWT.PRIMARY_MODAL);

        this.shell = shell;
        if (title != null) {
            shell.setText(title);
        }
    }

    /**
     * (non-Javadoc) Create all of the widgets on the Dialog
     * 
     * @see org.eclipse.jface.dialogs.Dialog#createDialogArea(org.eclipse.swt.widgets.Composite)
     */
    @Override
    public Control createDialogArea(Composite parent) {

        Composite dlgAreaForm = (Composite) super.createDialogArea(parent);

        // Create the main layout for the dialog.
        GridLayout mainLayout = new GridLayout(2, false);
        mainLayout.marginHeight = 3;
        mainLayout.marginWidth = 3;
        dlgAreaForm.setLayout(mainLayout);

        Group stnsGroup = new Group(dlgAreaForm, SWT.NONE);
        stnsGroup.setLayoutData(new GridData(GridData.FILL_HORIZONTAL));
        stnsGroup.setText(" Select Stations to use for Kp Est  ");
        GridData gd = new GridData();
        gd.grabExcessHorizontalSpace = true;
        gd.grabExcessVerticalSpace = true;
        gd.horizontalAlignment = SWT.FILL;
        gd.verticalAlignment = SWT.FILL;
        stnsGroup.setLayoutData(gd);
        stnsGroup.setLayout(new GridLayout(1, true));

        /*
         * stations
         */
        table = new Table(stnsGroup, SWT.BORDER | SWT.V_SCROLL | SWT.MULTI
                | SWT.CHECK | SWT.FULL_SELECTION);
        table.setHeaderVisible(false);
        table.setLinesVisible(false);

        int kpStationsSize = (kpStations != null) ? kpStations.size() : 0;
        for (int i = 0; i < kpStationsSize; i++) {
            String stnCode = kpStations.get(i);

            TableItem item = new TableItem(table, SWT.NONE);
            item.setText(stnCode);
            // item.setData(stnCode);

            if (curKpStationsStates.containsKey(stnCode)) {
                Map<String, Object> stnMap = curKpStationsStates.get(stnCode);
                item.setChecked((Integer) stnMap.get("kp_active") == 1 ? true
                        : false);
            } else {
                item.setChecked(false);
            }

        }

        /**
         * Draw settings buttons
         */
        Group applySettingsButtonGroup = new Group(dlgAreaForm, SWT.NONE);
        applySettingsButtonGroup.setText("Apply these settings");
        gd = new GridData();
        gd.grabExcessHorizontalSpace = true;
        gd.grabExcessVerticalSpace = true;
        gd.horizontalAlignment = SWT.FILL;
        gd.verticalAlignment = SWT.FILL;
        applySettingsButtonGroup.setLayoutData(gd);
        applySettingsButtonGroup.setLayout(new GridLayout(1, false));

        begCurSynPeriodBtn = new Button(applySettingsButtonGroup, SWT.RADIO);
        begCurSynPeriodBtn
                .setText("beginning with the current synoptic period");
        begCurSynPeriodBtn.setSelection(true);

        begPrevSynPeriodBtn = new Button(applySettingsButtonGroup, SWT.RADIO);
        begPrevSynPeriodBtn
                .setText("beginning with the previous synoptic period");

        prevSynPeriodOnlyBtn = new Button(applySettingsButtonGroup, SWT.RADIO);
        prevSynPeriodOnlyBtn.setText("for the previous synoptic period only");

        begCurSynPeriodBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                if (begCurSynPeriodBtn.getSelection()) {
                    kpState = 0;
                }
            }
        });

        begPrevSynPeriodBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                if (begPrevSynPeriodBtn.getSelection()) {
                    kpState = 1;
                }
            }
        });

        prevSynPeriodOnlyBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                if (prevSynPeriodOnlyBtn.getSelection()) {
                    kpState = 2;
                }
            }
        });

        return dlgAreaForm;
    }

    /**
     * Create Replace/Append/Cancel button for "Open" a product file or
     * Save/Cancel button for "Save" a product file.
     */
    @Override
    protected void createButtonsForButtonBar(Composite parent) {

        saveBtn = createButton(parent, SAVE_ID, SAVE_LABEL, true);
        cancelBtn = createButton(parent, CANCEL_ID, CANCEL_LABEL, true);

        saveBtn.addListener(SWT.MouseUp, new Listener() {
            public void handleEvent(Event event) {

                MessageDialog confirmDlg = new MessageDialog(shell, "Confirm",
                        null, "Do you want to save the changes?",
                        MessageDialog.QUESTION, new String[] { "Yes", "No" }, 0);
                confirmDlg.open();

                if (confirmDlg.getReturnCode() == MessageDialog.CANCEL) {
                    return;
                }
                save();
            }
        });

        cancelBtn.addListener(SWT.MouseUp, new Listener() {
            public void handleEvent(Event event) {
                close();
            }
        });

    }

    public void save() {

        try {

            int save = 0;
            HashMap<String, Map<String, Object>> prevKpStationsStates = RTKpUtil
                    .getStationsStatesMap(GeoMagStationType.KP, 1);

            TableItem[] items = table.getItems();

            if (items != null) {
                for (int i = 0; i < items.length; i += 1) {
                    TableItem item = items[i];

                    String stnCode = item.getText();

                    if (curKpStationsStates.containsKey(stnCode)) {
                        Map<String, Object> stnMap = curKpStationsStates
                                .get(stnCode);

                        if (stnMap != null) {
                            // Apply changes beginning with current synoptic
                            // period
                            boolean curStateActive = ((Integer) stnMap
                                    .get("kp_active") == 1) ? true : false;
                            if (kpState != 2
                                    && curStateActive != item.getChecked()) {
                                stnMap.put("kp_active", (item.getChecked() ? 1
                                        : 0));
                                boolean ret = RTKpUtil.changeStationState(
                                        stnCode, 0);
                                if (!ret) {
                                    MessageDialog confirmDlg = new MessageDialog(
                                            shell,
                                            "Error",
                                            null,
                                            "Encountered error in ChangeStationState...",
                                            MessageDialog.ERROR,
                                            new String[] { "OK" }, 0);
                                    confirmDlg.open();
                                }
                            }
                        }
                    }

                    if (prevKpStationsStates.containsKey(stnCode)) {
                        Map<String, Object> stnMap = curKpStationsStates
                                .get(stnCode);

                        if (stnMap != null) {
                            // Apply changes to previous synoptic period
                            boolean prevStateActive = ((Integer) stnMap
                                    .get("kp_active") == 1) ? true : false;
                            if (kpState != 0) {
                                if (prevStateActive != item.getChecked()) {

                                    boolean ret = RTKpUtil.changeStationState(
                                            stnCode, 1);
                                    if (!ret) {
                                        MessageDialog confirmDlg = new MessageDialog(
                                                shell,
                                                "Error",
                                                null,
                                                "Encountered error in ChangeStationState...",
                                                MessageDialog.ERROR,
                                                new String[] { "OK" }, 0);
                                        confirmDlg.open();
                                    }
                                    save = 2;
                                }
                            }
                        }
                    }

                }
                if (kpState == 2) {
                    int maxDiff = 0;
                    for (int i = 0; i < items.length; i += 1) {
                        TableItem item = items[i];
                        int butnVal = (item.getChecked() == true ? 1 : 0);

                        String stnCode = item.getText();
                        int curStnStateVal = 0;

                        if (curKpStationsStates.containsKey(stnCode)) {
                            Map<String, Object> stnMap = curKpStationsStates
                                    .get(stnCode);
                            curStnStateVal = (Integer) stnMap.get("kp_active");
                        }

                        int diff = Math.abs(curStnStateVal - butnVal);

                        if (diff > maxDiff)
                            maxDiff = diff;

                    }
                    if (save == 2 && maxDiff != 0) {
                        MessageDialog confirmDlg = new MessageDialog(
                                shell,
                                "Information",
                                null,
                                "Done. Reverting check boxes to show active stations for current synoptic period.",
                                MessageDialog.INFORMATION,
                                new String[] { "OK" }, 0);
                        confirmDlg.open();
                    }
                }

            }

            dbWindow.setDataBlockContent();
            dbWindow.displayDataBlock();

            System.out.println("Done. ");
            super.close();
        } catch (Exception e) {
            statusHandler.handle(Priority.WARN,
                    "Encountered error in ChangeStationState.", e);
        }
    }
}
