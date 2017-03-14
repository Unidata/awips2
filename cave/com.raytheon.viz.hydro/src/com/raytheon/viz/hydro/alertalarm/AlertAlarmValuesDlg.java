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
import java.util.Arrays;
import java.util.Date;
import java.util.List;
import java.util.TimeZone;

import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.graphics.Color;
import org.eclipse.swt.graphics.Font;
import org.eclipse.swt.graphics.GC;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Combo;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Layout;
import org.eclipse.swt.widgets.MessageBox;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Table;
import org.eclipse.swt.widgets.TableColumn;
import org.eclipse.swt.widgets.TableItem;
import org.eclipse.swt.widgets.Text;

import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.common.util.Pair;
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
 * Dec 07, 2012 1353       rferrel     Make dialog non-blocking.
 * Feb 05, 2013 1578       rferrel     Changes for non-blocking singleton TimeSeriesDlg.
 * Jun 15, 2015 16579      wkwock      Add HSA filter.
 * Apr 27, 2016 5438       randerso    Fixed GUI sizing issues
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
     * Max number of characters to display
     */
    private static final int NAME_LIMIT = 20;

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
     * HSA combo box.
     */
    private Combo hsaCbo;

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
    private List<Pair<String, Integer>> dataTableColumnDefs = Arrays.asList(
            new Pair<String, Integer>("Location", SWT.LEFT),
            new Pair<String, Integer>("Name", SWT.LEFT),
            new Pair<String, Integer>("PE", SWT.LEFT),
            new Pair<String, Integer>("Dur", SWT.RIGHT),
            new Pair<String, Integer>("TS", SWT.LEFT),
            new Pair<String, Integer>("Ext", SWT.LEFT),
            new Pair<String, Integer>("Value", SWT.RIGHT),
            new Pair<String, Integer>("SupVal", SWT.LEFT),
            new Pair<String, Integer>("QC", SWT.LEFT),
            new Pair<String, Integer>("ThreatDesc", SWT.LEFT),
            new Pair<String, Integer>("ValidTime", SWT.LEFT),
            new Pair<String, Integer>("BasisTime", SWT.LEFT));

    private Table dataTable;

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
     * Upper alert text field.
     */
    private Text alertUpperTF;

    /**
     * Lower alert text field.
     */
    private Text alertLowerTF;

    /**
     * ROC alert test field.
     */
    private Text alertRocTF;

    /**
     * Diff alert test field.
     */
    private Text alertDiffTF;

    /**
     * Upper alarm test field.
     */
    private Text alarmUpperTF;

    /**
     * Lower alarm test field.
     */
    private Text alarmLowerTF;

    /**
     * ROC alarm test field.
     */
    private Text alarmRocTF;

    /**
     * Diff alarm test field.
     */
    private Text alarmDiffTF;

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
        createDataTableControl();
        createSelectedItemGroup();
        createBottomButtons();

        queryAlertalarmval();

    }

    /**
     * Create the control located at the top of the screen.
     */
    private void createTopControls() {
        Composite topControlComp = new Composite(shell, SWT.NONE);
        GridLayout gl = new GridLayout(4, false);
        gl.horizontalSpacing = 10;
        topControlComp.setLayout(gl);

        Composite showComp = new Composite(topControlComp, SWT.NONE);
        gl = new GridLayout(3, false);
        gl.marginWidth = 0;
        gl.marginHeight = 0;
        showComp.setLayout(gl);
        GridData gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        showComp.setLayoutData(gd);

        Label showLbl = new Label(showComp, SWT.NONE);
        showLbl.setText("Show");

        showCbo = new Combo(showComp, SWT.DROP_DOWN | SWT.READ_ONLY);
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

        alertAlarmCbo = new Combo(showComp, SWT.DROP_DOWN | SWT.READ_ONLY);
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

        Composite exceedingComp = new Composite(topControlComp, SWT.NONE);
        gl = new GridLayout(2, false);
        gl.marginWidth = 0;
        gl.marginHeight = 0;
        exceedingComp.setLayout(gl);
        gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        exceedingComp.setLayoutData(gd);

        Label exceedLbl = new Label(exceedingComp, SWT.RIGHT);
        exceedLbl.setText("Exceeding");

        exceedingCbo = new Combo(exceedingComp, SWT.DROP_DOWN | SWT.READ_ONLY);
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

        Composite hsaComp = new Composite(topControlComp, SWT.NONE);
        gl = new GridLayout(2, false);
        gl.marginWidth = 0;
        gl.marginHeight = 0;
        hsaComp.setLayout(gl);
        gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        hsaComp.setLayoutData(gd);

        Label hsaLbl = new Label(hsaComp, SWT.RIGHT);
        hsaLbl.setText("  HSA");

        hsaCbo = new Combo(hsaComp, SWT.DROP_DOWN | SWT.READ_ONLY);
        populateHsa();
        hsaCbo.select(0);
        hsaCbo.addSelectionListener(new SelectionAdapter() {

            @Override
            public void widgetSelected(SelectionEvent e) {
                queryAlertalarmval();
                super.widgetSelected(e);
            }

        });
    }

    /**
     * Populate the HsaCbo
     */
    private void populateHsa() {
        hsaCbo.add("All HSAs");
        String hsaSql = "select distinct(hsa) from location order by hsa";

        java.util.List<Object[]> rs;
        try {
            rs = DirectDbQuery.executeQuery(hsaSql, HydroConstants.IHFS,
                    QueryLanguage.SQL);
            if (rs.size() > 0) {
                for (Object[] oa : rs) {
                    hsaCbo.add((String) oa[0]);
                }
            }
        } catch (VizException e) {
            statusHandler.handle(Priority.PROBLEM, e.getMessage(), e);
        }

    }

    /**
     * Create the controls for sorting the data.
     */
    private void createSortByControls() {
        Composite sortByComp = new Composite(shell, SWT.NONE);
        GridLayout gl = new GridLayout(3, false);
        gl.marginHeight = 0;
        gl.horizontalSpacing = 10;
        sortByComp.setLayout(gl);

        Label sortByLbl = new Label(sortByComp, SWT.NONE);
        sortByLbl.setText("Sort by:");

        timeRdo = new Button(sortByComp, SWT.RADIO);
        timeRdo.setText("Time");
        timeRdo.setSelection(true);
        timeRdo.addSelectionListener(new SelectionAdapter() {

            @Override
            public void widgetSelected(SelectionEvent e) {
                queryAlertalarmval();
            }

        });

        locationRdo = new Button(sortByComp, SWT.RADIO);
        locationRdo.setText("Location");
        locationRdo.addSelectionListener(new SelectionAdapter() {

            @Override
            public void widgetSelected(SelectionEvent e) {
                queryAlertalarmval();
            }

        });
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

        StringBuilder myQuery = new StringBuilder(
                "select aav.lid, location.name, aav.pe, aav.dur, aav.ts, aav.extremum, aav.value, aav.suppl_value, aav.quality_code, aav.aa_check, aav.aa_categ, aav.validtime, aav.basistime from location, alertalarmval aav where location.lid = aav.lid");
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

        // HSA filter
        if (!hsaCbo.getItem(hsaCbo.getSelectionIndex()).equalsIgnoreCase(
                "All HSAs")) {
            myQuery.append(" and hsa='"
                    + hsaCbo.getItem(hsaCbo.getSelectionIndex()) + "'");
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
            dataTable.removeAll();
            alertData = (ArrayList<Object[]>) DirectDbQuery.executeQuery(
                    myQuery.toString(), HydroConstants.IHFS, QueryLanguage.SQL);
            for (Object[] rowData : alertData) {
                TableItem item = new TableItem(dataTable, SWT.NONE);

                // Location
                item.setText(0, rowData[0].toString());

                // Name
                String name = rowData[1].toString();
                item.setText(1,
                        name.substring(0, Math.min(NAME_LIMIT, name.length())));

                // PE
                item.setText(2, rowData[2].toString());

                // Dur
                item.setText(3, rowData[3].toString());

                // TS
                item.setText(4, rowData[4].toString());

                // Ext
                item.setText(5, rowData[5].toString());

                // Value
                item.setText(6, String.format("%8.2f", rowData[6]));

                // SupVal
                /*
                 * don't bother showing the following fields: probability value,
                 * shef revision code, action time. If aa_check is not 'roc' or
                 * 'diff', then assign the value of * 'roc' or 'diff' as "--".
                 */
                if ((rowData[9].toString().trim().equals("roc"))
                        || (rowData[9].toString().trim().equals("diff"))) {
                    item.setText(7, String.format("%8.2f", rowData[7]));
                } else {
                    item.setText(7, "   --  ");
                }

                // QC
                String QcSymbol = TimeSeriesUtil
                        .buildQcSymbol(((Number) rowData[8]).longValue());
                item.setText(8, QcSymbol);

                // ThreatDesc
                item.setText(9, rowData[9] + " " + rowData[10]);

                item.setText(10, smallerTime.format((Date) rowData[11]));
                item.setData("validTime", rowData[11]);

                /*
                 * for time vars, need full year info and two separate fields
                 * for possible parsing later. only show the basis time if
                 * processing observed type data.
                 */
                if ((rowData[4].toString().trim().toUpperCase().startsWith("R"))
                        || (rowData[4].toString().trim().toUpperCase()
                                .startsWith("P"))) {

                    item.setText(11, "----- -----");
                } else {
                    item.setText(11, smallerTime.format((Date) rowData[12]));
                }
                item.setData("basisTime", rowData[12]);

                dataTable.deselectAll();
            }

            GC gc = new GC(dataTable);
            int pad = gc.textExtent(" ").x;
            gc.dispose();
            for (TableColumn column : dataTable.getColumns()) {
                column.pack();
                column.setWidth(column.getWidth() + pad);
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
        TableItem selectedItem = dataTable.getItem(alertAlarmIndex);
        selectedLid = selectedItem.getText(0);
        selectedPe = selectedItem.getText(2);
        selectedDur = selectedItem.getText(3);
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
                    productTimeTF.setText("Unreported");
                }
                /* load the posting time */
                // set postingtime.
                if (rowData[2] != null) {
                    postingTimeTF.setText(alertAlarmTimeFormat
                            .format((Date) rowData[2]));
                } else {
                    postingTimeTF.setText("Unreported");
                }
                /* load the reporting time */
                // set actiontime.
                if (rowData[3] != null) {
                    lastReportedTimeTF.setText(alertAlarmTimeFormat
                            .format((Date) rowData[3]));
                } else {
                    lastReportedTimeTF.setText("Unreported");
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
        int selectionIndex = dataTable.getSelectionIndex();
        if (selectionIndex != -1) {
            MessageBox mb = new MessageBox(shell, SWT.ICON_QUESTION | SWT.OK
                    | SWT.CANCEL);
            mb.setText("Delete Confirmation");
            mb.setMessage("Do you wish to delete this entry?");

            int result = mb.open();

            if (result == SWT.OK) {
                try {
                    deleteAlertalarmvalSelected(selectionIndex);
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
        TableItem selectedItem = dataTable.getItem(alertAlarmIndex);
        String currentValidTime = dbDatabaseFormat.format((Date) selectedItem
                .getData("validTime"));
        String basisTimeStr = db2DatabaseFormat.format((Date) selectedItem
                .getData("basisTime"));
        selectedLid = selectedItem.getText(0);

        myQuery.append("(aav.lid = '");
        myQuery.append(selectedLid);
        myQuery.append("')");
        myQuery.append(" AND ");

        selectedPe = selectedItem.getText(2);
        myQuery.append("(aav.pe = '");
        myQuery.append(selectedPe);
        myQuery.append("')");
        myQuery.append(" AND ");

        selectedDur = selectedItem.getText(3);
        myQuery.append("(aav.dur = '");
        myQuery.append(selectedDur);
        myQuery.append("')");
        myQuery.append(" AND ");

        String selectedTs = selectedItem.getText(4);
        myQuery.append("(aav.ts = '");
        myQuery.append(selectedTs);
        myQuery.append("')");
        myQuery.append(" AND ");

        String selectedExtremum = selectedItem.getText(5);
        myQuery.append("(aav.extremum = '");
        myQuery.append(selectedExtremum);
        myQuery.append("')");
        myQuery.append(" AND ");

        String selectedThreatDesc = selectedItem.getText(9).trim();
        String[] splitThreat = selectedThreatDesc.split("\\s+");
        myQuery.append("(aav.aa_check = '");
        myQuery.append(splitThreat[0]);
        myQuery.append("')");
        myQuery.append(" AND ");
        myQuery.append("(aav.aa_categ = '");
        myQuery.append(splitThreat[1]);
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
                    if (dataLimits.size() == 0) {
                        deflimitsFound = false;
                    } else {
                        deflimitsFound = true;
                        for (Object[] rowData : dataLimits) {
                            setField(alertUpperTF, (Double) rowData[0]);
                            setField(alertLowerTF, (Double) rowData[1]);
                            setField(alertRocTF, (Double) rowData[2]);
                            setField(alertDiffTF, (Double) rowData[3]);

                            setField(alarmUpperTF, (Double) rowData[4]);
                            setField(alarmLowerTF, (Double) rowData[5]);
                            setField(alarmRocTF, (Double) rowData[6]);
                            setField(alarmDiffTF, (Double) rowData[7]);

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
                    setField(alertUpperTF, (Double) rowData[0]);
                    setField(alertLowerTF, (Double) rowData[1]);
                    setField(alertRocTF, (Double) rowData[2]);
                    setField(alertDiffTF, (Double) rowData[3]);

                    setField(alarmUpperTF, (Double) rowData[4]);
                    setField(alarmLowerTF, (Double) rowData[5]);
                    setField(alarmRocTF, (Double) rowData[6]);
                    setField(alarmDiffTF, (Double) rowData[7]);
                }
            }
            // both location specific range and default range are not found, set
            // to MISSING
            else if (!deflimitsFound) {
                alertUpperTF.setText(" MISSING ");
                alertLowerTF.setText(" MISSING ");
                alertRocTF.setText(" MISSING ");
                alertDiffTF.setText(" MISSING ");

                alarmUpperTF.setText(" MISSING ");
                alarmLowerTF.setText(" MISSING ");
                alarmRocTF.setText(" MISSING ");
                alarmDiffTF.setText(" MISSING ");
            }
        } catch (VizException e) {
            statusHandler.handle(Priority.PROBLEM, e.getMessage(), e);
        }
    }

    /**
     * Create the data list control containing the alert & alarm data.
     */
    private void createDataTableControl() {

        Composite dataComp = new Composite(shell, SWT.NONE);
        GridLayout gl = new GridLayout(1, false);
        dataComp.setLayout(gl);
        GridData gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        dataComp.setLayoutData(gd);

        Label noteLbl = new Label(dataComp, SWT.CENTER);
        noteLbl.setText("Note: SupVal is ObsValue for forecast diff threats "
                + "and actual ROC for ROC threats");
        gd = new GridData(SWT.CENTER, SWT.DEFAULT, true, false);
        noteLbl.setLayoutData(gd);

        dataTable = new Table(dataComp, SWT.BORDER | SWT.SINGLE | SWT.V_SCROLL);
        gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        gd.heightHint = dataTable.getItemHeight() * 22;
        dataTable.setLayoutData(gd);

        dataTable.setLinesVisible(true);
        dataTable.setHeaderVisible(true);

        for (Pair<String, Integer> columnDef : dataTableColumnDefs) {
            TableColumn column = new TableColumn(dataTable,
                    columnDef.getSecond());
            column.setText(columnDef.getFirst());
            column.pack();
        }

        dataTable.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                int index = dataTable.getSelectionIndex();
                if (index != -1) {
                    updateSelectedItemFields(index);
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
    private void setField(Text control, Double d) {
        if (d != null) {
            control.setText(String.format("%7.1f", d));
        } else {
            control.setText(" UNDEF ");
        }
    }

    /**
     * Create selected item controls located below the data list control.
     */
    private void createSelectedItemGroup() {
        GridData gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        Group selectedItemGroup = new Group(shell, SWT.NONE);
        selectedItemGroup.setText("Details for Selected Item");
        GridLayout gl = new GridLayout(2, false);
        gl.horizontalSpacing = 10;
        selectedItemGroup.setLayout(gl);
        selectedItemGroup.setLayoutData(gd);

        Composite leftComp = new Composite(selectedItemGroup, SWT.NONE);
        gl = new GridLayout(5, false);
        gl.marginWidth = 0;
        gl.marginHeight = 0;
        leftComp.setLayout(gl);
        gd = new GridData(SWT.FILL, SWT.CENTER, true, true);
        leftComp.setLayoutData(gd);
        /*
         * Fill in the left composite
         */
        gd = new GridData(SWT.RIGHT, SWT.CENTER, false, false);
        Label limitsLbl = new Label(leftComp, SWT.RIGHT);
        limitsLbl.setText("Limits:");
        limitsLbl.setLayoutData(gd);

        gd = new GridData(SWT.CENTER, SWT.CENTER, false, false);
        Label upperLbl = new Label(leftComp, SWT.CENTER);
        upperLbl.setText("Upper");
        upperLbl.setLayoutData(gd);

        gd = new GridData(SWT.CENTER, SWT.CENTER, false, false);
        Label lowerLbl = new Label(leftComp, SWT.CENTER);
        lowerLbl.setText("Lower");
        lowerLbl.setLayoutData(gd);

        gd = new GridData(SWT.CENTER, SWT.CENTER, false, false);
        Label rocLbl = new Label(leftComp, SWT.CENTER);
        rocLbl.setText("ROC");
        rocLbl.setLayoutData(gd);

        gd = new GridData(SWT.CENTER, SWT.CENTER, false, false);
        Label diffLbl = new Label(leftComp, SWT.CENTER);
        diffLbl.setText("Diff");
        diffLbl.setLayoutData(gd);

        gd = new GridData(SWT.RIGHT, SWT.CENTER, false, false);
        Label alertLbl = new Label(leftComp, SWT.RIGHT);
        alertLbl.setText("Alert:");
        alertLbl.setLayoutData(gd);

        Color bkgColor = getDisplay().getSystemColor(
                SWT.COLOR_WIDGET_BACKGROUND);

        alertUpperTF = new Text(leftComp, SWT.BORDER | SWT.CENTER
                | SWT.READ_ONLY);
        GC gc = new GC(alertUpperTF);
        int labelWidth = gc.getFontMetrics().getAverageCharWidth() * 15; // ????
        gc.dispose();
        gd = new GridData(SWT.DEFAULT, SWT.DEFAULT);
        gd.widthHint = labelWidth;
        alertUpperTF.setLayoutData(gd);
        alertUpperTF.setBackground(bkgColor);

        gd = new GridData(SWT.DEFAULT, SWT.DEFAULT);
        gd.widthHint = labelWidth;
        alertLowerTF = new Text(leftComp, SWT.BORDER | SWT.CENTER
                | SWT.READ_ONLY);
        alertLowerTF.setLayoutData(gd);
        alertLowerTF.setBackground(bkgColor);

        gd = new GridData(SWT.DEFAULT, SWT.DEFAULT);
        gd.widthHint = labelWidth;
        alertRocTF = new Text(leftComp, SWT.BORDER | SWT.CENTER | SWT.READ_ONLY);
        alertRocTF.setLayoutData(gd);
        alertRocTF.setBackground(bkgColor);

        gd = new GridData(SWT.DEFAULT, SWT.DEFAULT);
        gd.widthHint = labelWidth;
        alertDiffTF = new Text(leftComp, SWT.BORDER | SWT.CENTER
                | SWT.READ_ONLY);
        alertDiffTF.setLayoutData(gd);
        alertDiffTF.setBackground(bkgColor);

        gd = new GridData(SWT.RIGHT, SWT.CENTER, false, false);
        Label alarmLbl = new Label(leftComp, SWT.RIGHT);
        alarmLbl.setText("Alarm:");
        alarmLbl.setLayoutData(gd);

        gd = new GridData(SWT.DEFAULT, SWT.DEFAULT);
        gd.widthHint = labelWidth;
        alarmUpperTF = new Text(leftComp, SWT.BORDER | SWT.CENTER
                | SWT.READ_ONLY);
        alarmUpperTF.setLayoutData(gd);
        alarmUpperTF.setBackground(bkgColor);

        gd = new GridData(SWT.DEFAULT, SWT.DEFAULT);
        gd.widthHint = labelWidth;
        alarmLowerTF = new Text(leftComp, SWT.BORDER | SWT.CENTER
                | SWT.READ_ONLY);
        alarmLowerTF.setLayoutData(gd);
        alarmLowerTF.setBackground(bkgColor);

        gd = new GridData(SWT.DEFAULT, SWT.DEFAULT);
        gd.widthHint = labelWidth;
        alarmRocTF = new Text(leftComp, SWT.BORDER | SWT.CENTER | SWT.READ_ONLY);
        alarmRocTF.setLayoutData(gd);
        alarmRocTF.setBackground(bkgColor);

        gd = new GridData(SWT.DEFAULT, SWT.DEFAULT);
        gd.widthHint = labelWidth;
        alarmDiffTF = new Text(leftComp, SWT.BORDER | SWT.CENTER
                | SWT.READ_ONLY);
        alarmDiffTF.setLayoutData(gd);
        alarmDiffTF.setBackground(bkgColor);

        Composite rightComp = new Composite(selectedItemGroup, SWT.NONE);
        gl = new GridLayout(3, false);
        gl.marginWidth = 0;
        gl.marginHeight = 0;
        rightComp.setLayout(gl);
        gd = new GridData(SWT.DEFAULT, SWT.DEFAULT, false, false);
        rightComp.setLayoutData(gd);

        /*
         * Fill in the right composite
         */
        gd = new GridData(SWT.RIGHT, SWT.CENTER, true, false);
        Label productLbl = new Label(rightComp, SWT.RIGHT);
        productLbl.setText("Product:");
        productLbl.setLayoutData(gd);

        productTF = new Text(rightComp, SWT.BORDER);
        gc = new GC(productTF);
        int textWidth = gc.textExtent("MMMMMMMMMM").x;
        gc.dispose();
        gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        gd.widthHint = textWidth;
        productTF.setLayoutData(gd);
        productTF.setBackground(bkgColor);

        productTimeTF = new Text(rightComp, SWT.BORDER);
        gc = new GC(productTimeTF);
        textWidth = gc.textExtent("Wed 00/00 00:00").x;
        gc.dispose();
        gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        gd.widthHint = textWidth;
        productTimeTF.setLayoutData(gd);
        productTimeTF.setBackground(bkgColor);

        gd = new GridData(SWT.RIGHT, SWT.CENTER, true, false);
        gd.horizontalSpan = 2;
        Label postingTimeLbl = new Label(rightComp, SWT.RIGHT);
        postingTimeLbl.setText("Posting Time:");
        postingTimeLbl.setLayoutData(gd);

        postingTimeTF = new Text(rightComp, SWT.BORDER);
        gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        gd.widthHint = textWidth;
        postingTimeTF.setLayoutData(gd);
        postingTimeTF.setBackground(bkgColor);

        gd = new GridData(SWT.RIGHT, SWT.CENTER, true, false);
        gd.horizontalSpan = 2;
        Label lartReportedLbl = new Label(rightComp, SWT.RIGHT);
        lartReportedLbl.setText("Time Last Reported:");
        lartReportedLbl.setLayoutData(gd);

        lastReportedTimeTF = new Text(rightComp, SWT.BORDER);
        gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        gd.widthHint = textWidth;
        lastReportedTimeTF.setLayoutData(gd);
        lastReportedTimeTF.setBackground(bkgColor);
    }

    /**
     * Create the buttons located at the bottom of the dialog.
     */
    private void createBottomButtons() {
        Composite buttonComp = new Composite(shell, SWT.NONE);
        GridLayout gl = new GridLayout(4, false);
        gl.horizontalSpacing = 10;
        buttonComp.setLayout(gl);
        GridData gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        buttonComp.setLayoutData(gd);

        int dpi = getDisplay().getDPI().x;

        gd = new GridData(SWT.LEFT, SWT.DEFAULT, false, false);
        gd.widthHint = dpi * 2;
        Button tabularBtn = new Button(buttonComp, SWT.PUSH);
        tabularBtn.setText("Tabular Time Series");
        tabularBtn.setLayoutData(gd);
        tabularBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                shell.setCursor(shell.getDisplay().getSystemCursor(
                        SWT.CURSOR_WAIT));
                TimeSeriesDlg.getInstance().updateAndOpen(selectedLid, false);
                shell.setCursor(null);
            }
        });

        gd = new GridData(SWT.LEFT, SWT.DEFAULT, false, false);
        gd.widthHint = dpi * 2;
        Button graphBtn = new Button(buttonComp, SWT.PUSH);
        graphBtn.setText("Graphical Time Series");
        graphBtn.setLayoutData(gd);
        graphBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                shell.setCursor(shell.getDisplay().getSystemCursor(
                        SWT.CURSOR_WAIT));
                TimeSeriesDlg.getInstance().updateAndOpen(selectedLid, true);
                shell.setCursor(null);
            }
        });

        gd = new GridData(SWT.LEFT, SWT.DEFAULT, false, false);
        gd.widthHint = dpi;
        Button deleteBtn = new Button(buttonComp, SWT.PUSH);
        deleteBtn.setText("Delete");
        deleteBtn.setLayoutData(gd);
        deleteBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                deleteRecord();
            }
        });

        gd = new GridData(SWT.RIGHT, SWT.DEFAULT, true, false);
        gd.widthHint = dpi;
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
