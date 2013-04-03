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

package com.raytheon.viz.hydro.alertalarm;

import static com.raytheon.viz.hydro.alertalarm.AlertAlarmConstants.ALARM_CATEGSTR;
import static com.raytheon.viz.hydro.alertalarm.AlertAlarmConstants.ALERT_CATEGSTR;
import static com.raytheon.viz.hydro.alertalarm.AlertAlarmConstants.DIFF_CHECKSTR;
import static com.raytheon.viz.hydro.alertalarm.AlertAlarmConstants.LOWER_CHECKSTR;
import static com.raytheon.viz.hydro.alertalarm.AlertAlarmConstants.ROC_CHECKSTR;
import static com.raytheon.viz.hydro.alertalarm.AlertAlarmConstants.UPPER_CHECKSTR;
import static com.raytheon.viz.hydro.alertalarm.AlertAlarmConstants.alertAlarmTimeFormat;
import static com.raytheon.viz.hydro.alertalarm.AlertAlarmConstants.db2DatabaseFormat;
import static com.raytheon.viz.hydro.alertalarm.AlertAlarmConstants.dbDatabaseFormat;
import static com.raytheon.viz.hydro.alertalarm.AlertAlarmConstants.smallerTime;

import java.util.ArrayList;
import java.util.Date;
import java.util.TimeZone;

import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.events.SelectionListener;
import org.eclipse.swt.graphics.Font;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Combo;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Layout;
import org.eclipse.swt.widgets.List;
import org.eclipse.swt.widgets.MessageBox;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Text;

import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.viz.core.catalog.DirectDbQuery;
import com.raytheon.uf.viz.core.catalog.DirectDbQuery.QueryLanguage;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.viz.hydro.alertalarm.AlertAlarmConstants.AaOption;
import com.raytheon.viz.hydro.alertalarm.AlertAlarmConstants.CheckOption;
import com.raytheon.viz.hydro.alertalarm.AlertAlarmConstants.TypeOption;
import com.raytheon.viz.hydro.timeseries.TimeSeriesDlg;
import com.raytheon.viz.hydro.timeseries.util.TimeSeriesUtil;
import com.raytheon.viz.hydrocommon.HydroConstants;
import com.raytheon.viz.hydrocommon.datamanager.IhfsAlertalarmvalData;
import com.raytheon.viz.hydrocommon.datamanager.IhfsLocdatalimitsData;
import com.raytheon.viz.ui.dialogs.CaveSWTDialog;

/**
 * This class displays the Alert & Alarm Data Values dialog for HydroView.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 29 NOV 2007  373        lvenable    Initial creation. 
 * 10/21/2008   1617       grichard    Support alert alarm data.
 * 04 NOV 2010  5517       Judy Wang   Fixed Delete button.
 * May, 2011    9359       Jingtao Deng Updated queryLocdatalimits(), check locdatalimits table
 *                                      first, is not existing, check for datalimits table as defaults.
 *                                      Both not existing, set as MISSING
 * Dec 07, 2012 1353       rferrel      Make dialog non-blocking.
 * 
 * </pre>
 * 
 * @author lvenable
 * @version 1.0
 * 
 */
public class AlertAlarmValuesDlg extends CaveSWTDialog implements
        IhfsAlertalarmvalData, IhfsLocdatalimitsData {
    private final IUFStatusHandler statusHandler = UFStatus
            .getHandler(AlertAlarmValuesDlg.class);

    /**
     * Font used for the data list.
     */
    private Font font;

    /**
     * Show combo box.
     */
    private Combo showCbo;

    /**
     * Alert/Alarm combo box.
     */
    private Combo alertAlarmCbo;

    /**
     * Exceeding limits combo box.
     */
    private Combo exceedingCbo;

    /**
     * Sort by time radio button.
     */
    private Button timeRdo;

    /**
     * Sort by location radio button.
     */
    private Button locationRdo;

    /**
     * List control displaying the data.
     */
    private List dataList;

    /**
     * List containing valid times in database format.
     */
    private java.util.List<String> dataValidTimes = new ArrayList<String>();

    private java.util.List<String> dataBasisTimes = new ArrayList<String>();

    /**
     * Product text control.
     */
    private Text productTF;

    /**
     * Product time text control.
     */
    private Text productTimeTF;

    /**
     * Posting time text control.
     */
    private Text postingTimeTF;

    /**
     * Last reported time text control.
     */
    private Text lastReportedTimeTF;

    /**
     * Upper alert label.
     */
    private Label alertUpperLbl;

    /**
     * Lower alert label.
     */
    private Label alertLowerLbl;

    /**
     * ROC alert label.
     */
    private Label alertRocLbl;

    /**
     * Diff alert label.
     */
    private Label alertDiffLbl;

    /**
     * Upper alarm label.
     */
    private Label alarmUpperLbl;

    /**
     * Lower alarm label.
     */
    private Label alarmLowerLbl;

    /**
     * ROC alarm label.
     */
    private Label alarmRocLbl;

    /**
     * Diff alarm label.
     */
    private Label alarmDiffLbl;

    /**
     * Currently selected location ID.
     */
    private String selectedLid;

    private String selectedPe;

    private String selectedDur;

    /**
     * Constructor.
     * 
     * @param parent
     *            Parent shell.
     */

    public AlertAlarmValuesDlg(Shell parent) {
        super(parent, SWT.DIALOG_TRIM, CAVE.DO_NOT_BLOCK);
        setText("Alert and Alarm Data Values");
    }

    @Override
    protected Layout constructShellLayout() {
        // Create the main layout for the shell.
        GridLayout mainLayout = new GridLayout(1, true);
        mainLayout.marginHeight = 1;
        mainLayout.marginWidth = 1;
        return mainLayout;
    }

    @Override
    protected void disposed() {
        font.dispose();
    }

    @Override
    protected void initializeComponents(Shell shell) {
        setReturnValue(false);

        font = new Font(shell.getDisplay(), "Monospace", 11, SWT.NORMAL);

        // Initialize all of the controls and layouts
        initializeComponents();
    }

    /**
     * Initialize the controls and display them on the dialog.
     */
    private void initializeComponents() {
        createTopControls();
        createSortByControls();
        createListLabels();
        createDataListControl();
        createSelectedItemGroup();
        createBottomButtons();

        queryAlertalarmval();

    }

    /**
     * Create the control located at the top of the screen.
     */
    private void createTopControls() {
        Composite topControlComp = new Composite(shell, SWT.NONE);
        GridLayout topControlGl = new GridLayout(6, false);
        topControlComp.setLayout(topControlGl);

        Label showLbl = new Label(topControlComp, SWT.NONE);
        showLbl.setText("Show");

        showCbo = new Combo(topControlComp, SWT.DROP_DOWN | SWT.READ_ONLY);
        showCbo.add("All");
        showCbo.add("Observed");
        showCbo.add("Forecast");
        showCbo.select(0);
        showCbo.addSelectionListener(new SelectionAdapter() {

            @Override
            public void widgetSelected(SelectionEvent e) {

                queryAlertalarmval();

                super.widgetSelected(e);
            }

        });

        alertAlarmCbo = new Combo(topControlComp, SWT.DROP_DOWN | SWT.READ_ONLY);
        alertAlarmCbo.add("Alert/Alarms");
        alertAlarmCbo.add("Alarms");
        alertAlarmCbo.add("Alerts");
        alertAlarmCbo.select(0);
        alertAlarmCbo.addSelectionListener(new SelectionAdapter() {

            @Override
            public void widgetSelected(SelectionEvent e) {

                queryAlertalarmval();

                super.widgetSelected(e);
            }

        });

        GridData gd = new GridData(100, SWT.DEFAULT);
        Label exceedLbl = new Label(topControlComp, SWT.RIGHT);
        exceedLbl.setText("Exceeding");
        exceedLbl.setLayoutData(gd);

        exceedingCbo = new Combo(topControlComp, SWT.DROP_DOWN | SWT.READ_ONLY);
        exceedingCbo.add("Any Limit");
        exceedingCbo.add("Rate-of-Change (roc)");
        exceedingCbo.add("Upper Limit");
        exceedingCbo.add("Lower Limit");
        exceedingCbo.add("Diff Limit");
        exceedingCbo.select(0);
        exceedingCbo.addSelectionListener(new SelectionAdapter() {

            @Override
            public void widgetSelected(SelectionEvent e) {

                queryAlertalarmval();

                super.widgetSelected(e);
            }

        });
        gd = new GridData(370, SWT.DEFAULT);
        Label noteLbl = new Label(topControlComp, SWT.CENTER);
        noteLbl.setText("Note: SupVal is ObsValue for forecast diff threats\n"
                + "and actual ROC for ROC threats");
        noteLbl.setLayoutData(gd);
    }

    /**
     * Create the controls for sorting the data.
     */
    private void createSortByControls() {
        Composite sortByComp = new Composite(shell, SWT.NONE);
        GridLayout sortByGl = new GridLayout(3, false);
        sortByGl.horizontalSpacing = 10;
        sortByComp.setLayout(sortByGl);

        Label sortByLbl = new Label(sortByComp, SWT.NONE);
        sortByLbl.setText("Sort by: ");

        timeRdo = new Button(sortByComp, SWT.RADIO);
        timeRdo.setText("Time");
        timeRdo.setSelection(true);
        timeRdo.addSelectionListener(new SelectionAdapter() {

            @Override
            public void widgetSelected(SelectionEvent e) {

                timeRdo.setSelection(true);
                locationRdo.setSelection(false);

                queryAlertalarmval();

                super.widgetSelected(e);
            }

        });

        locationRdo = new Button(sortByComp, SWT.RADIO);
        locationRdo.setText("Location");
        locationRdo.addSelectionListener(new SelectionAdapter() {

            @Override
            public void widgetSelected(SelectionEvent e) {

                timeRdo.setSelection(false);
                locationRdo.setSelection(true);

                queryAlertalarmval();

                super.widgetSelected(e);
            }

        });
    }

    /**
     * Create the labels located above the data list control.
     */
    private void createListLabels() {
        Composite labelComp = new Composite(shell, SWT.NONE);
        GridLayout labelGl = new GridLayout(12, false);
        labelComp.setLayout(labelGl);

        new GridData(500, SWT.DEFAULT);

        String header = "Location       Name                    PE         Dur TS  Ext      Value             SupVal      QC   ThreatDesc           ValidTime             BasisTime";

        new GridData(SWT.DEFAULT, SWT.DEFAULT);
        Label nameLbl = new Label(labelComp, SWT.NONE);
        nameLbl.setText(header);
    }

    /**
     * Get the selected type option on the dialog.
     */
    private TypeOption getType() {

        switch (showCbo.getSelectionIndex()) {

        case 1:
            return TypeOption.OBS;
        case 2:
            return TypeOption.FCST;
        default:
            return TypeOption.ALL;

        }
    }

    /**
     * Get the selected alert/alarm option on the dialog.
     */
    private AaOption getAlertAlarmType() {
        switch (alertAlarmCbo.getSelectionIndex()) {
        case 1:
            return AaOption.ALARM;
        case 2:
            return AaOption.ALERT;
        default:
            return AaOption.BOTH;
        }
    }

    /**
     * Get the selected exceeding option on the dialog.
     */
    private CheckOption getCheckType() {
        switch (exceedingCbo.getSelectionIndex()) {
        case 1:
            return CheckOption.ROC;
        case 2:
            return CheckOption.UPPER;
        case 3:
            return CheckOption.LOWER;
        case 4:
            return CheckOption.DIFF;
        default:
            return CheckOption.ANY;
        }
    }

    // -------------------------------------------------------------
    // Query the Alertalarmval Table in the IHFS database using SQL.
    // -------------------------------------------------------------
    @Override
    public void queryAlertalarmval() {
        // -------------------------------------
        // Populate data list or Fill main list.
        // -------------------------------------

        String fmtStr;
        String fmtStr1 = "%-8s %-12.12s %2s %5d %-2s %-3s %-8.2f %-8.2f %1s  %-6s%-6s %11s %11s";
        String fmtStr2 = "%-8s %-12.12s %2s %5d %-2s %-3s %-8.2f %-8s %1s  %-6s%-6s %11s %11s";
        Object supplValue;
        String basisTimeValue;
        String tmpStr;
        StringBuilder myQuery = new StringBuilder(
                "select aav.lid, aav.pe, aav.dur, aav.ts, aav.extremum, aav.value, aav.suppl_value, aav.quality_code, aav.aa_check, aav.aa_categ, aav.validtime, aav.basistime, location.name from location, alertalarmval aav where location.lid = aav.lid");
        ArrayList<Object[]> alertData;

        // Build 'where' clause by using the three options in the combo boxes on
        // the dialog.

        if (getType() == TypeOption.ALL) {
            myQuery.append(" and aav.ts like '%'");
        } else if (getType() == TypeOption.OBS) {
            myQuery.append(" and (aav.ts like 'R%' or aav.ts like 'P%')");
        } else if (getType() == TypeOption.FCST) {
            myQuery.append(" and (aav.ts like 'F%' or aav.ts like 'C%')");
        }

        if (getAlertAlarmType() == AaOption.ALARM) {
            myQuery.append(" and aav.aa_categ = ");
            myQuery.append(ALARM_CATEGSTR);
        } else if (getAlertAlarmType() == AaOption.ALERT) {
            myQuery.append(" and aav.aa_categ = ");
            myQuery.append(ALERT_CATEGSTR);
        }

        if (getCheckType() == CheckOption.ROC) {
            myQuery.append(" and aav.aa_check = ");
            myQuery.append(ROC_CHECKSTR);
        } else if (getCheckType() == CheckOption.UPPER) {
            myQuery.append(" and aav.aa_check = ");
            myQuery.append(UPPER_CHECKSTR);
        } else if (getCheckType() == CheckOption.LOWER) {
            myQuery.append(" and aav.aa_check = ");
            myQuery.append(LOWER_CHECKSTR);
        } else if (getCheckType() == CheckOption.DIFF) {
            myQuery.append(" and aav.aa_check = ");
            myQuery.append(DIFF_CHECKSTR);
        }

        // Build 'sort' options based on toggle buttons on the dialog.

        if (locationRdo.getSelection()) {
            if (getType() != TypeOption.FCST) {
                myQuery.append(" order by aav.lid, aav.validtime desc, aav.ts asc, aav.basistime desc");
            } else {
                myQuery.append(" order by aav.lid, aav.validtime asc, aav.ts asc, aav.basistime desc");
            }
        } else {
            if (getType() != TypeOption.FCST) {
                myQuery.append(" order by aav.validtime desc, aav.lid, aav.basistime desc");
            } else {
                myQuery.append(" order by aav.validtime asc, aav.lid, aav.basistime desc");
            }
        }

        // use UTC time zone
        smallerTime.setTimeZone(TimeZone.getTimeZone("UTC"));
        dbDatabaseFormat.setTimeZone(TimeZone.getTimeZone("UTC"));
        db2DatabaseFormat.setTimeZone(TimeZone.getTimeZone("UTC"));

        try {
            dataList.removeAll();
            dataValidTimes.clear();
            dataBasisTimes.clear();
            alertData = (ArrayList<Object[]>) DirectDbQuery.executeQuery(
                    myQuery.toString(), HydroConstants.IHFS, QueryLanguage.SQL);
            for (Object[] rowData : alertData) {
                /* reformat some of the fields for improved display. */

                /*
                 * don't bother showing the following fields: probability value,
                 * shef revision code, action time. If aa_check is not 'roc' or
                 * 'diff', then assign the value of * 'roc' or 'diff' as "--".
                 */
                if ((rowData[8].toString().trim().equals("roc"))
                        || (rowData[8].toString().trim().equals("diff"))) {
                    fmtStr = fmtStr1;
                    supplValue = rowData[6];
                } else {
                    fmtStr = fmtStr2;
                    supplValue = "   --  ";
                }

                /*
                 * for time vars, need full year info and two separate fields
                 * for possible parsing later. only show the basis time if
                 * processing observed type data.
                 */
                if ((rowData[3].toString().trim().toUpperCase().startsWith("R"))
                        || (rowData[3].toString().trim().toUpperCase()
                                .startsWith("P"))) {

                    basisTimeValue = "----- -----";
                } else {
                    basisTimeValue = smallerTime.format((Date) rowData[11]);
                }

                String QcSymbol = TimeSeriesUtil.buildQcSymbol(Long
                        .parseLong(rowData[7].toString()));

                tmpStr = String.format(fmtStr, rowData[0].toString(),
                        rowData[12].toString(), rowData[1].toString(),
                        rowData[2], rowData[3].toString(),
                        rowData[4].toString(), rowData[5], supplValue,
                        QcSymbol, rowData[8].toString(), rowData[9].toString(),
                        smallerTime.format((Date) rowData[10]), basisTimeValue,
                        rowData[11].toString());

                /* load the list strings. */
                dataList.add(tmpStr);
                dataList.setSelection(0);
                dataValidTimes.add(dbDatabaseFormat.format((Date) rowData[10]));
                dataBasisTimes
                        .add(db2DatabaseFormat.format((Date) rowData[11]));
            }
        } catch (VizException e) {
            statusHandler.handle(Priority.PROBLEM, e.getMessage(), e);
        }
    }

    // -------------------------------------------------------------
    // Query the Alertalarmval Table in the IHFS database using SQL.
    // -------------------------------------------------------------
    @Override
    public void queryAlertalarmvalTimes(int alertAlarmIndex) {
        // Query 'alertalarmval' table in ihfs database for selected fields
        // using SQL.
        StringBuilder myQuery = new StringBuilder(
                "select aav.product_id, aav.producttime, aav.postingtime, aav.action_time from alertalarmval aav");
        ArrayList<Object[]> alertData;
        /* get the index to the selected item and get its info */
        String selectedItem = dataList.getItem(alertAlarmIndex).trim();
        String[] selectedItems = selectedItem.split("(\\s)+");
        selectedLid = selectedItems[0];
        selectedPe = selectedItems[2];
        selectedDur = selectedItems[3];
        myQuery.append(" where aav.lid = '");
        myQuery.append(selectedLid);
        myQuery.append("'");

        // use UTC time zone
        alertAlarmTimeFormat.setTimeZone(TimeZone.getTimeZone("UTC"));
        // -------------------------------------------------------------
        // Query the Alertalarmval Table in the IHFS database using SQL.
        // -------------------------------------------------------------
        try {
            alertData = (ArrayList<Object[]>) DirectDbQuery.executeQuery(
                    myQuery.toString(), HydroConstants.IHFS, QueryLanguage.SQL);
            for (Object[] rowData : alertData) {
                /* load the product id and time */
                // set productid.
                productTF.setText((String) rowData[0]);
                // set producttime.
                if (rowData[1] != null) {
                    productTimeTF.setText(alertAlarmTimeFormat
                            .format((Date) rowData[1]));
                } else {
                    productTimeTF.setText(" Unreported ");
                }
                /* load the posting time */
                // set postingtime.
                if (rowData[2] != null) {
                    postingTimeTF.setText(alertAlarmTimeFormat
                            .format((Date) rowData[2]));
                } else {
                    postingTimeTF.setText(" Unreported ");
                }
                /* load the reporting time */
                // set actiontime.
                if (rowData[3] != null) {
                    lastReportedTimeTF.setText(alertAlarmTimeFormat
                            .format((Date) rowData[3]));
                } else {
                    lastReportedTimeTF.setText(" Unreported ");
                }
            }
        } catch (VizException e) {
            statusHandler.handle(Priority.PROBLEM, e.getMessage(), e);
        }

    }

    /**
     * Prompts the user to delete the current record
     */
    private void deleteRecord() {
        if (dataList.getSelectionIndex() != -1) {
            MessageBox mb = new MessageBox(shell, SWT.ICON_QUESTION | SWT.OK
                    | SWT.CANCEL);
            mb.setText("Delete Confirmation");
            mb.setMessage("Do you wish to delete this entry?");

            int result = mb.open();

            if (result == SWT.OK) {
                try {
                    deleteAlertalarmvalSelected(dataList.getSelectionIndex());
                    queryAlertalarmval();
                } catch (VizException e) {
                    MessageBox mbFail = new MessageBox(shell, SWT.ICON_ERROR
                            | SWT.OK);
                    mbFail.setText("Unable to Delete");
                    mbFail.setMessage("Unable to Delete Record");
                    mbFail.open();
                    statusHandler.handle(Priority.PROBLEM, e.getMessage(), e);
                }
            }
        } else {
            MessageBox mb = new MessageBox(shell, SWT.ICON_WARNING | SWT.OK);
            mb.setText("Invalid Selection");
            mb.setMessage("Please select an alert/alarm item first.");
            mb.open();
        }
    }

    // ------------------------------------------------------------
    // Delete the selected item in the data list from alertalarmval.
    // ------------------------------------------------------------
    @Override
    public void deleteAlertalarmvalSelected(int alertAlarmIndex)
            throws VizException {
        // Query 'alertalarmval' table in ihfs database and delete selected item
        // using SQL.
        StringBuilder myQuery = new StringBuilder(
                "delete from alertalarmval aav where ");
        /* get the index to the selected item and get its info */
        String selectedItem = dataList.getItem(alertAlarmIndex).trim();
        String currentValidTime = dataValidTimes.get(alertAlarmIndex);
        String basisTimeStr = dataBasisTimes.get(alertAlarmIndex);
        String[] selectedItems = selectedItem.split("(\\s)+");
        selectedLid = selectedItems[0];
        int index = 2;
        for (int i = 0; i < 10; ++i) {
            if (!selectedItems[index].equals("HG")) {
                index++;
            }
        }
        myQuery.append("(aav.lid = '");
        myQuery.append(selectedLid);
        myQuery.append("')");
        myQuery.append(" AND ");
        // String selectedPe = selectedItems[index++];
        selectedPe = selectedItems[index++];
        myQuery.append("(aav.pe = '");
        myQuery.append(selectedPe);
        myQuery.append("')");
        myQuery.append(" AND ");
        // String selectedDur = selectedItems[index++];
        selectedDur = selectedItems[index++];
        myQuery.append("(aav.dur = '");
        myQuery.append(selectedDur);
        myQuery.append("')");
        myQuery.append(" AND ");
        String selectedTs = selectedItems[index++];
        myQuery.append("(aav.ts = '");
        myQuery.append(selectedTs);
        myQuery.append("')");
        myQuery.append(" AND ");
        String selectedExtremum = selectedItems[index++];
        myQuery.append("(aav.extremum = '");
        myQuery.append(selectedExtremum);
        myQuery.append("')");
        myQuery.append(" AND ");
        index++;
        index++;
        index++;
        String selectedAaCheck = selectedItems[index++];
        myQuery.append("(aav.aa_check = '");
        myQuery.append(selectedAaCheck);
        myQuery.append("')");
        myQuery.append(" AND ");
        String selectedAaCateg = selectedItems[index++];
        myQuery.append("(aav.aa_categ = '");
        myQuery.append(selectedAaCateg);
        myQuery.append("')");
        myQuery.append(" AND ");
        myQuery.append("(aav.validtime = '");
        myQuery.append(currentValidTime);
        myQuery.append("')");
        myQuery.append(" AND ");
        myQuery.append("(aav.basistime = '");
        myQuery.append(basisTimeStr);
        myQuery.append("')");

        // -------------------------------------------------------------
        // Query the Alertalarmval Table in the IHFS database using SQL.
        // -------------------------------------------------------------

        try {
            DirectDbQuery.executeStatement(myQuery.toString(),
                    HydroConstants.IHFS, QueryLanguage.SQL);
        } catch (VizException e) {
            statusHandler.handle(Priority.PROBLEM, e.getMessage(), e);
        }

    }

    // -----------------------------------------------------------------
    // Query 'locdatalimits' table in ihfs database for selected fields
    // using SQL.
    // -----------------------------------------------------------------
    @Override
    public void queryLocdatalimits() {

        // retrieve from location based locdatalimits table
        StringBuilder myQuery = new StringBuilder(
                "select ldl.alert_upper_limit, ldl.alert_lower_limit, ldl.alert_roc_limit, ldl.alert_diff_limit, ldl.alarm_upper_limit, ldl.alarm_lower_limit, ldl.alarm_roc_limit, ldl.alarm_diff_limit from locdatalimits ldl");

        ArrayList<Object[]> loclimitData;
        myQuery.append(" where ldl.lid = '");
        myQuery.append(selectedLid);
        myQuery.append("'");

        Boolean loclimitsFound = true;
        Boolean deflimitsFound = true;

        // retrieve from general datalimits table
        StringBuilder dlQuery = new StringBuilder(
                "select dl.alert_upper_limit, dl.alert_lower_limit, dl.alert_roc_limit, dl.alert_diff_limit, dl.alarm_upper_limit, dl.alarm_lower_limit, dl.alarm_roc_limit, dl.alarm_diff_limit  from datalimits dl");
        ArrayList<Object[]> dataLimits;
        dlQuery.append(" where dl.pe = '");
        dlQuery.append(selectedPe);
        dlQuery.append("'");
        dlQuery.append(" and dl.dur = '");
        dlQuery.append(selectedDur);
        dlQuery.append("'");

        // -------------------------------------------------------------
        // Query the Locdatalimits Table in the IHFS database using SQL.
        // -------------------------------------------------------------
        try {
            loclimitData = (ArrayList<Object[]>) DirectDbQuery.executeQuery(
                    myQuery.toString(), HydroConstants.IHFS, QueryLanguage.SQL);

            // if location specific range is not found, check the default
            // range*/
            if (loclimitData.size() == 0) {
                loclimitsFound = false;

                try {
                    dataLimits = (ArrayList<Object[]>) DirectDbQuery
                            .executeQuery(dlQuery.toString(),
                                    HydroConstants.IHFS, QueryLanguage.SQL);
                    if (dataLimits.size() == 0)
                        deflimitsFound = false;
                    else {
                        deflimitsFound = true;
                        for (Object[] rowData : dataLimits) {
                            setField(alertUpperLbl, (Double) rowData[0]);
                            setField(alertLowerLbl, (Double) rowData[1]);
                            setField(alertRocLbl, (Double) rowData[2]);
                            setField(alertDiffLbl, (Double) rowData[3]);

                            setField(alarmUpperLbl, (Double) rowData[4]);
                            setField(alarmLowerLbl, (Double) rowData[5]);
                            setField(alarmRocLbl, (Double) rowData[6]);
                            setField(alarmDiffLbl, (Double) rowData[7]);

                            return;
                        }
                    }

                } catch (VizException e) {
                    statusHandler.handle(Priority.PROBLEM, e.getMessage(), e);
                }

            }
            // location specific range is found
            if (loclimitsFound) {
                for (Object[] rowData : loclimitData) {
                    setField(alertUpperLbl, (Double) rowData[0]);
                    setField(alertLowerLbl, (Double) rowData[1]);
                    setField(alertRocLbl, (Double) rowData[2]);
                    setField(alertDiffLbl, (Double) rowData[3]);

                    setField(alarmUpperLbl, (Double) rowData[4]);
                    setField(alarmLowerLbl, (Double) rowData[5]);
                    setField(alarmRocLbl, (Double) rowData[6]);
                    setField(alarmDiffLbl, (Double) rowData[7]);
                }
            }
            // both location specific range and default range are not found, set
            // to MISSING
            else if (!deflimitsFound) {
                alertUpperLbl.setText(" MISSING ");
                alertLowerLbl.setText(" MISSING ");
                alertRocLbl.setText(" MISSING ");
                alertDiffLbl.setText(" MISSING ");

                alarmUpperLbl.setText(" MISSING ");
                alarmLowerLbl.setText(" MISSING ");
                alarmRocLbl.setText(" MISSING ");
                alarmDiffLbl.setText(" MISSING ");
            }
        } catch (VizException e) {
            statusHandler.handle(Priority.PROBLEM, e.getMessage(), e);
        }
    }

    /**
     * Create the data list control containing the alert & alarm data.
     */
    private void createDataListControl() {
        GridData gd = new GridData(SWT.CENTER, SWT.DEFAULT, false, false);
        gd.widthHint = 950;
        gd.heightHint = 500;
        dataList = new List(shell, SWT.BORDER | SWT.SINGLE | SWT.V_SCROLL);
        dataList.setLayoutData(gd);

        dataList.setFont(font);
        dataList.addSelectionListener(new SelectionListener() {

            @Override
            public void widgetDefaultSelected(SelectionEvent e) {
                // TODO Auto-generated method stub

            }

            @Override
            public void widgetSelected(SelectionEvent e) {
                if (dataList.getSelectionIndex() != -1) {
                    updateSelectedItemFields(dataList.getSelectionIndex());
                }
            }

        });
    }

    // --------------------------------------------------------------
    // Update the fields based on the selected item in the data list.
    // --------------------------------------------------------------
    private void updateSelectedItemFields(int alertAlarmIndex) {

        queryAlertalarmvalTimes(alertAlarmIndex);

        queryLocdatalimits();

    }

    // -----------------------------------------------------------
    // Set the field according to the presence of data or no data.
    // -----------------------------------------------------------
    private void setField(Label label, Double d) {
        if (d != null) {
            label.setText(String.format("%7.1f", d));
        } else {
            label.setText(" UNDEF ");
        }
    }

    /**
     * Create selected item controls located below the data list control.
     */
    private void createSelectedItemGroup() {
        GridData mainGridData = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        Group selectedItemGroup = new Group(shell, SWT.NONE);
        selectedItemGroup.setText(" Details for Selected Item ");
        GridLayout gl = new GridLayout(8, false);
        selectedItemGroup.setLayout(gl);
        selectedItemGroup.setLayoutData(mainGridData);

        GridData gd = new GridData(60, SWT.DEFAULT);
        Label limitsLbl = new Label(selectedItemGroup, SWT.NONE);
        limitsLbl.setText("Limits:");
        limitsLbl.setLayoutData(gd);

        gd = new GridData(105, SWT.DEFAULT);
        Label upperLbl = new Label(selectedItemGroup, SWT.CENTER);
        upperLbl.setText("Upper");
        upperLbl.setLayoutData(gd);

        gd = new GridData(105, SWT.DEFAULT);
        Label lowerLbl = new Label(selectedItemGroup, SWT.CENTER);
        lowerLbl.setText("Lower");
        lowerLbl.setLayoutData(gd);

        gd = new GridData(105, SWT.DEFAULT);
        Label rocLbl = new Label(selectedItemGroup, SWT.CENTER);
        rocLbl.setText("ROC");
        rocLbl.setLayoutData(gd);

        gd = new GridData(105, SWT.DEFAULT);
        Label diffLbl = new Label(selectedItemGroup, SWT.CENTER);
        diffLbl.setText("Diff");
        diffLbl.setLayoutData(gd);

        gd = new GridData(80, SWT.DEFAULT);
        Label productLbl = new Label(selectedItemGroup, SWT.RIGHT);
        productLbl.setText("Product:");
        productLbl.setLayoutData(gd);

        gd = new GridData(135, SWT.DEFAULT);
        productTF = new Text(selectedItemGroup, SWT.BORDER);
        productTF.setLayoutData(gd);

        gd = new GridData(135, SWT.DEFAULT);
        productTimeTF = new Text(selectedItemGroup, SWT.BORDER);
        productTimeTF.setLayoutData(gd);

        gd = new GridData(60, SWT.DEFAULT);
        Label alertLbl = new Label(selectedItemGroup, SWT.NONE);
        alertLbl.setText("Alert:");
        alertLbl.setLayoutData(gd);

        gd = new GridData(105, SWT.DEFAULT);
        alertUpperLbl = new Label(selectedItemGroup, SWT.BORDER | SWT.CENTER);
        alertUpperLbl.setLayoutData(gd);

        gd = new GridData(105, SWT.DEFAULT);
        alertLowerLbl = new Label(selectedItemGroup, SWT.BORDER | SWT.CENTER);
        alertLowerLbl.setLayoutData(gd);

        gd = new GridData(105, SWT.DEFAULT);
        alertRocLbl = new Label(selectedItemGroup, SWT.BORDER | SWT.CENTER);
        alertRocLbl.setLayoutData(gd);

        gd = new GridData(105, SWT.DEFAULT);
        alertDiffLbl = new Label(selectedItemGroup, SWT.BORDER | SWT.CENTER);
        alertDiffLbl.setLayoutData(gd);

        gd = new GridData(80, SWT.DEFAULT);
        Label filler1Lbl = new Label(selectedItemGroup, SWT.NONE);
        filler1Lbl.setLayoutData(gd);

        gd = new GridData(135, SWT.DEFAULT);
        Label postingTimeLbl = new Label(selectedItemGroup, SWT.RIGHT);
        postingTimeLbl.setText("Posting Time:");
        postingTimeLbl.setLayoutData(gd);

        gd = new GridData(156, SWT.DEFAULT);
        postingTimeTF = new Text(selectedItemGroup, SWT.BORDER);
        postingTimeTF.setLayoutData(gd);

        gd = new GridData(60, SWT.DEFAULT);
        Label alarmLbl = new Label(selectedItemGroup, SWT.NONE);
        alarmLbl.setText("Alarm:");
        alarmLbl.setLayoutData(gd);

        gd = new GridData(105, SWT.DEFAULT);
        alarmUpperLbl = new Label(selectedItemGroup, SWT.BORDER | SWT.CENTER);
        alarmUpperLbl.setLayoutData(gd);

        gd = new GridData(105, SWT.DEFAULT);
        alarmLowerLbl = new Label(selectedItemGroup, SWT.BORDER | SWT.CENTER);
        alarmLowerLbl.setLayoutData(gd);

        gd = new GridData(105, SWT.DEFAULT);
        alarmRocLbl = new Label(selectedItemGroup, SWT.BORDER | SWT.CENTER);
        alarmRocLbl.setLayoutData(gd);

        gd = new GridData(105, SWT.DEFAULT);
        alarmDiffLbl = new Label(selectedItemGroup, SWT.BORDER | SWT.CENTER);
        alarmDiffLbl.setLayoutData(gd);

        gd = new GridData(80, SWT.DEFAULT);
        Label filler2Lbl = new Label(selectedItemGroup, SWT.NONE);
        filler2Lbl.setLayoutData(gd);

        gd = new GridData(156, SWT.DEFAULT);
        Label lartReportedLbl = new Label(selectedItemGroup, SWT.RIGHT);
        lartReportedLbl.setText("Time Last Reported:");
        lartReportedLbl.setLayoutData(gd);

        gd = new GridData(156, SWT.DEFAULT);
        lastReportedTimeTF = new Text(selectedItemGroup, SWT.BORDER);
        lastReportedTimeTF.setLayoutData(gd);
    }

    /**
     * Create the buttons located at the bottom of the dialog.
     */
    private void createBottomButtons() {
        GridData mainGD = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        Composite buttonComp = new Composite(shell, SWT.NONE);
        GridLayout buttonGl = new GridLayout(4, false);
        buttonGl.horizontalSpacing = 10;
        buttonComp.setLayoutData(mainGD);
        buttonComp.setLayout(buttonGl);

        GridData gd = new GridData(160, SWT.DEFAULT);
        Button tabularBtn = new Button(buttonComp, SWT.PUSH);
        tabularBtn.setText("Tabular Time Series");
        tabularBtn.setLayoutData(gd);
        tabularBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                TimeSeriesDlg tsd = new TimeSeriesDlg(shell, selectedLid);
                tsd.open();
            }
        });

        gd = new GridData(160, SWT.DEFAULT);
        Button graphBtn = new Button(buttonComp, SWT.PUSH);
        graphBtn.setText("Graphical Time Series");
        graphBtn.setLayoutData(gd);
        graphBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                TimeSeriesDlg tsd = new TimeSeriesDlg(shell, selectedLid);
                tsd.open();
            }
        });

        gd = new GridData(90, SWT.DEFAULT);
        Button deleteBtn = new Button(buttonComp, SWT.PUSH);
        deleteBtn.setText("Delete");
        deleteBtn.setLayoutData(gd);
        deleteBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                deleteRecord();
            }
        });

        gd = new GridData(SWT.END, SWT.DEFAULT, true, false);
        gd.widthHint = 90;
        Button closeBtn = new Button(buttonComp, SWT.PUSH);
        closeBtn.setText("Close");
        closeBtn.setLayoutData(gd);
        closeBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                close();
            }
        });
    }
}
