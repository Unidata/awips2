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
package com.raytheon.viz.hydrobase;

import java.io.BufferedWriter;
import java.io.FileWriter;
import java.io.IOException;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Calendar;
import java.util.Date;
import java.util.Iterator;
import java.util.Map;
import java.util.Set;
import java.util.TimeZone;
import java.util.TreeMap;

import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.events.ShellAdapter;
import org.eclipse.swt.events.ShellEvent;
import org.eclipse.swt.graphics.Cursor;
import org.eclipse.swt.graphics.Font;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Combo;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.FileDialog;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Layout;
import org.eclipse.swt.widgets.List;
import org.eclipse.swt.widgets.MessageBox;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Text;

import com.raytheon.uf.common.hydro.service.WhfsServiceRequest;
import com.raytheon.uf.common.ohd.AppsDefaults;
import com.raytheon.uf.common.serialization.comm.response.ServerErrorResponse;
import com.raytheon.uf.common.time.SimulatedTime;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.requests.ThriftClient;
import com.raytheon.viz.hydrocommon.HydroConstants;
import com.raytheon.viz.hydrocommon.util.RatingUtils;
import com.raytheon.viz.ui.dialogs.CaveSWTDialog;

/**
 * This class displayed the Flood Report dialog.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date			Ticket#		Engineer	Description
 * ------------	----------	-----------	--------------------------
 * Sep 4, 2008	2259    	lvenable	Initial creation
 * Dec 3, 2010  4952        lbousaidi   
 * Jan 13, 2011 5415		lbousaidi	added a dialog when 
 * 							"Compute Latest data" button runs
 * May 14, 2012 14965       wkwock      fix crash in query for data
 * 
 * </pre>
 * 
 * @author lvenable
 * @version 1.0
 */
public class FloodReportDlg extends CaveSWTDialog {
    private static final String format = "%-15S %-10S %6.2f       %16S       %4.1f";

    /**
     * Control font.
     */
    private Font controlFont;

    /**
     * Report period combo box.
     */
    private Combo reportPeriodCbo;

    /**
     * Location label.
     */
    private Label locationLbl;

    /**
     * Stage label.
     */
    private Label stageLbl;

    /**
     * HSA combo box.
     */
    private Combo hsaCbo;

    /**
     * Location list control.
     */
    private List locationList;

    /**
     * Save events button.
     */
    private Button saveEventsBtn;

    /**
     * Delete button.
     */
    private Button deleteBtn;

    /**
     * Above flood stage text control.
     */
    private Text aboveFsTF;

    /**
     * Crest text control.
     */
    private Text crestTF;

    /**
     * Below flood stage text control.
     */
    private Text belowFsTF;

    /**
     * Event list control.
     */
    private List eventList;

    /**
     * Insert into table button.
     */
    private Button insertIntoTableBtn;

    /** The selected lid */
    private String selectedLid = null;

    /** The selected key */
    private String selectedKey = null;

    /** List of location IDs */
    private final ArrayList<String> locationLidList = new ArrayList<String>();

    /**
     * The start date.
     */
    private Date startDate;

    /**
     * The end date.
     */
    private Date endDate;

    /** The standard arrow cursor */
    protected Cursor arrowCursor;

    /** The Hand cursor */
    protected Cursor waitCursor;

    /**
     * Flood canvas.
     */
    private FloodReportCanvasComp floodCanvas;

    private final String[] reportPeriodArray = new String[] { "Month to Date",
            "Year to Date", "Last 12 Months", "January", "February", "March",
            "April", "May", "June", "July", "August", "September", "October",
            "November", "December" };

    /**
     * Offset to make January be 1 in the reportPeriodArray.
     */
    private static final int MONTH_OFFSET = 3;

    private SimpleDateFormat sdf = null;

    private SimpleDateFormat dateFormat = null;

    private SimpleDateFormat hourFormat = null;

    private SimpleDateFormat dbFormat = null;

    private SimpleDateFormat fr = null;

    /**
     * Constructor.
     * 
     * @param parent
     *            Parent shell.
     */
    public FloodReportDlg(Shell parent) {
        super(parent);
        setText("Flood Report");

        dbFormat = new SimpleDateFormat("yyyy-MM-dd HH:mm:ss");
        dbFormat.setTimeZone(TimeZone.getTimeZone("GMT"));
        sdf = new SimpleDateFormat("dd MMM HH:mm");
        sdf.setTimeZone(TimeZone.getTimeZone("GMT"));
        dateFormat = new SimpleDateFormat("yyyy-MM-dd");
        dateFormat.setTimeZone(TimeZone.getTimeZone("GMT"));
        hourFormat = new SimpleDateFormat("HH:mm");
        hourFormat.setTimeZone(TimeZone.getTimeZone("GMT"));
        fr = new SimpleDateFormat("MM/dd/yyyy HH:mm");
        fr.setTimeZone(TimeZone.getTimeZone("GMT"));
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

        arrowCursor = new Cursor(getDisplay(), SWT.CURSOR_ARROW);
        waitCursor = new Cursor(getDisplay(), SWT.CURSOR_WAIT);

        // Initialize all of the controls and layouts
        initializeComponents();

        // Add the data to the list
        updateFloodList();

        shell.addShellListener(new ShellAdapter() {
            @Override
            public void shellClosed(ShellEvent event) {
                FloodReportDataManager.getInstance().setDrawGraph(false);
            }
        });
    }

    /**
     * Initialize the components on the display.
     */
    private void initializeComponents() {
        controlFont = new Font(shell.getDisplay(), "Monospace", 10, SWT.NORMAL);

        createTopComboControl();

        createMiddleControls();

        createSelectedEventGroup();

        createBottomButtons();
    }

    /**
     * Create the top location and stage label and reporting controls.
     */
    private void createTopComboControl() {
        GridData gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        Composite topComp = new Composite(shell, SWT.NONE);
        topComp.setLayout(new GridLayout(4, false));
        topComp.setLayoutData(gd);

        Label reportLbl = new Label(topComp, SWT.NONE);
        reportLbl.setText("Reporting Period ");

        reportPeriodCbo = new Combo(topComp, SWT.DROP_DOWN | SWT.READ_ONLY);
        reportPeriodCbo.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                clearCanvas();
                updateFloodList();
            }
        });
        fillReportPeriodCombo();
        reportPeriodCbo.select(0);

        gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        locationLbl = new Label(topComp, SWT.RIGHT);
        locationLbl.setText("Location: ");
        locationLbl.setLayoutData(gd);

        gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        gd.horizontalIndent = 40;
        stageLbl = new Label(topComp, SWT.LEFT);
        stageLbl.setText("Stage: ");
        stageLbl.setLayoutData(gd);
    }

    /**
     * Create the middle list and canvas controls.
     */
    private void createMiddleControls() {
        GridData gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        Composite mainComp = new Composite(shell, SWT.NONE);
        mainComp.setLayout(new GridLayout(2, false));
        mainComp.setLayoutData(gd);

        // ----------------------------------------------------
        // Create the HSA, Location list and button controls
        // ----------------------------------------------------
        gd = new GridData(SWT.DEFAULT, SWT.TOP, false, true);
        Composite leftComp = new Composite(mainComp, SWT.NONE);
        leftComp.setLayout(new GridLayout(2, false));
        leftComp.setLayoutData(gd);

        Label hsaLbl = new Label(leftComp, SWT.NONE);
        hsaLbl.setText("HSA ");

        // Get the HSA Data
        ArrayList<String> hsaList = FloodReportDataManager.getInstance()
                .getHsaList();
        hsaCbo = new Combo(leftComp, SWT.DROP_DOWN | SWT.READ_ONLY);
        hsaCbo.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                clearCanvas();
                updateFloodList();
            }
        });

        hsaCbo.add("All HSAs");

        for (String s : hsaList) {
            hsaCbo.add(s);
        }

        hsaCbo.select(0);

        // Add a separator bar
        gd = new GridData(GridData.FILL_HORIZONTAL);
        gd.horizontalSpan = 2;
        Label sepLbl = new Label(leftComp, SWT.SEPARATOR | SWT.HORIZONTAL);
        sepLbl.setLayoutData(gd);

        gd = new GridData();
        gd.horizontalIndent = 4;
        gd.horizontalSpan = 2;
        Label listLbl = new Label(leftComp, SWT.NONE);
        listLbl.setText(getListLabel());
        listLbl.setFont(controlFont);
        listLbl.setLayoutData(gd);

        gd = new GridData(550, 250);
        gd.horizontalSpan = 2;
        locationList = new List(leftComp, SWT.BORDER | SWT.SINGLE
                | SWT.V_SCROLL);
        locationList.setLayoutData(gd);
        locationList.setFont(controlFont);
        locationList.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                handleSelection();
            }
        });

        gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        gd.horizontalSpan = 2;
        Composite buttonComp = new Composite(leftComp, SWT.NONE);
        buttonComp.setLayout(new GridLayout(2, false));
        buttonComp.setLayoutData(gd);

        gd = new GridData(SWT.CENTER, SWT.DEFAULT, true, false);
        gd.widthHint = 165;
        saveEventsBtn = new Button(buttonComp, SWT.PUSH);
        saveEventsBtn.setText("Save Events to File...");
        saveEventsBtn.setLayoutData(gd);
        saveEventsBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                exportFloodTsFile();
            }
        });

        gd = new GridData(SWT.CENTER, SWT.DEFAULT, true, false);
        gd.widthHint = 165;
        deleteBtn = new Button(buttonComp, SWT.PUSH);
        deleteBtn.setText("Delete Event");
        deleteBtn.setLayoutData(gd);
        deleteBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                if (locationList.getSelectionIndex() == -1) {
                    MessageBox mb = new MessageBox(shell, SWT.OK);
                    mb.setText("Make a Selection");
                    mb.setMessage("You must select a river observation !");
                    mb.open();
                } else {
                    selectedLid = locationLidList.get(locationList
                            .getSelectionIndex());
                    MessageBox messageBox = new MessageBox(shell, SWT.OK
                            | SWT.CANCEL);
                    messageBox.setText("Delete Confirmation");
                    messageBox.setMessage("Do you wish to delete this entry?");
                    int answer = messageBox.open();

                    if (answer == SWT.OK) {
                        deleteRecord();
                    }
                }
            }
        });

        // ----------------------------------------------------
        // Create the Stage canvas composite
        // ----------------------------------------------------
        floodCanvas = new FloodReportCanvasComp(mainComp, this);
    }

    /**
     * Create the Selected Event group and controls.
     */
    private void createSelectedEventGroup() {
        GridData gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        Group selectedEventGroup = new Group(shell, SWT.NONE);
        selectedEventGroup.setLayout(new GridLayout(2, true));
        selectedEventGroup.setLayoutData(gd);
        selectedEventGroup.setText(" Details for Selected Event ");

        // ------------------------------------
        // Create left side controls
        // ------------------------------------

        gd = new GridData(SWT.FILL, SWT.FILL, true, true);
        Composite leftComp = new Composite(selectedEventGroup, SWT.NONE);
        leftComp.setLayout(new GridLayout(2, false));
        leftComp.setLayoutData(gd);

        // Above FS
        gd = new GridData(SWT.FILL, SWT.CENTER, false, true);
        gd.horizontalIndent = 40;
        Label aboveFsLbl = new Label(leftComp, SWT.RIGHT);
        aboveFsLbl.setText("AboveFS: ");
        aboveFsLbl.setLayoutData(gd);

        gd = new GridData(200, SWT.DEFAULT);
        aboveFsTF = new Text(leftComp, SWT.BORDER);
        aboveFsTF.setLayoutData(gd);

        // Crest
        gd = new GridData(SWT.FILL, SWT.CENTER, false, true);
        gd.horizontalIndent = 40;
        Label crestLbl = new Label(leftComp, SWT.RIGHT);
        crestLbl.setText("Crest: ");
        crestLbl.setLayoutData(gd);

        gd = new GridData(200, SWT.DEFAULT);
        crestTF = new Text(leftComp, SWT.BORDER);
        crestTF.setLayoutData(gd);

        // Below FS
        gd = new GridData(SWT.FILL, SWT.CENTER, false, true);
        gd.horizontalIndent = 40;
        Label belowFsLbl = new Label(leftComp, SWT.RIGHT);
        belowFsLbl.setText("BelowFS: ");
        belowFsLbl.setLayoutData(gd);

        gd = new GridData(200, SWT.DEFAULT);
        belowFsTF = new Text(leftComp, SWT.BORDER);
        belowFsTF.setLayoutData(gd);

        // ------------------------------------
        // Create right side controls
        // ------------------------------------
        gd = new GridData(SWT.FILL, SWT.FILL, true, true);
        Composite rightComp = new Composite(selectedEventGroup, SWT.NONE);
        rightComp.setLayout(new GridLayout(2, false));
        rightComp.setLayoutData(gd);

        gd = new GridData(300, 150);
        eventList = new List(rightComp, SWT.BORDER | SWT.SINGLE | SWT.V_SCROLL);
        eventList.setLayoutData(gd);
        eventList.setFont(controlFont);

        insertIntoTableBtn = new Button(rightComp, SWT.PUSH);
        insertIntoTableBtn.setText("Insert into\nCrest Table");
        insertIntoTableBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                if (eventList.getSelectionIndex() == -1) {
                    MessageBox mb = new MessageBox(shell, SWT.OK);
                    mb.setText("Make a Selection");
                    mb.setMessage("You must select a flood event!");
                    mb.open();
                } else {
                    selectedLid = locationLidList.get(locationList
                            .getSelectionIndex());
                    MessageBox messageBox = new MessageBox(shell, SWT.OK
                            | SWT.CANCEL);
                    messageBox.setText("Insert Confirmation");
                    messageBox
                            .setMessage("Do you wish to insert this entry into the Crest table?");
                    int answer = messageBox.open();

                    if (answer == SWT.OK) {
                        insertRecord();
                    }
                }
            }
        });
    }

    /**
     * Create the buttons at the bottom of the dialog.
     */
    private void createBottomButtons() {
        GridData gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        Composite buttonComp = new Composite(shell, SWT.NONE);
        buttonComp.setLayout(new GridLayout(2, true));
        buttonComp.setLayoutData(gd);

        int buttonWidth = 120;

        gd = new GridData(SWT.LEFT, SWT.DEFAULT, true, false);
        gd.widthHint = buttonWidth;
        Button closeBtn = new Button(buttonComp, SWT.PUSH);
        closeBtn.setText("Close");
        closeBtn.setLayoutData(gd);
        closeBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                FloodReportDataManager.getInstance().setDrawGraph(false);
                shell.dispose();
            }
        });

        gd = new GridData(SWT.RIGHT, SWT.DEFAULT, true, false);
        Button computeLatestDataBtn = new Button(buttonComp, SWT.PUSH);
        computeLatestDataBtn.setText("Compute Latest Data");
        computeLatestDataBtn.setLayoutData(gd);
        computeLatestDataBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                shell.setCursor(waitCursor);
                MessageBox messageBox = new MessageBox(shell, SWT.OK
                        | SWT.CANCEL);
                messageBox.setText("WHFS");                    
                messageBox.setMessage("Update flood sequences?\n" +
                		"This may take a few minutes. \n" +
                		"Press 'OK' to proceed, 'Cancel' to abort.");
                int answer = messageBox.open();                
                try {                	                	                   
                    if (answer == SWT.OK) {
                    	WhfsServiceRequest request = new WhfsServiceRequest();
                        request.setServicesToExecute("run_floodseq");
                        Object obj= ThriftClient.sendRequest(request);
                        shell.setCursor(arrowCursor);
                        if (!(obj instanceof ServerErrorResponse)) {
                        	 MessageBox messageBox2 = new MessageBox(shell, SWT.OK);
                        	 messageBox2.setText("WHFS");
                        	 messageBox2.setMessage("Update of flood " +
                        	 			"sequences complete.");
                        	 messageBox2.open();
                    	}
                    }
                    
                } catch (VizException e1) {
                    // TODO Auto-generated catch block
                    e1.printStackTrace();
                }
                shell.setCursor(arrowCursor);
            }
        });
    }

    /**
     * Get the label text for the location list control.
     * 
     * @return Label text.
     */
    private String getListLabel() {
        String format = "%S                %S      %S       %S";

        String labelStr = String.format(format, "Location", "Crest Stage",
                "Crest Time", "Flood Stage");

        return labelStr;
    }

    /**
     * Fill the report period combo control.
     */
    private void fillReportPeriodCombo() {
        for (int i = 0; i < reportPeriodArray.length; i++) {
            reportPeriodCbo.add(reportPeriodArray[i]);
        }
    }

    private void updateFloodList() {
        FloodReportDataManager dman = FloodReportDataManager.getInstance();
        locationList.removeAll();
        stageLbl.setText("Stage: ");
        locationLbl.setText("Location:  ");
        int month = 0;
        String hsaWhere = null;
        String where = null;
        Map<String, FloodReportData> dataMap = new TreeMap<String, FloodReportData>();

        // Determine the selected time period
        String period = reportPeriodArray[reportPeriodCbo.getSelectionIndex()];

        // Set Calendar objects to current date/time
        Date date = SimulatedTime.getSystemTime().getTime();
        Calendar endCal = Calendar.getInstance(TimeZone.getTimeZone("GMT"));
        Calendar startCal = Calendar.getInstance(TimeZone.getTimeZone("GMT"));
        endCal.setTime(date);
        startCal.setTime(date);
        if (period.equals(reportPeriodArray[0])) {
            // Month to date
            startCal.set(Calendar.DAY_OF_MONTH, 1);
            startCal.set(Calendar.HOUR_OF_DAY, 0);
            startCal.set(Calendar.MINUTE, 0);
            startCal.set(Calendar.SECOND, 0);
        } else if (period.equals(reportPeriodArray[1])) {
            // Year to date
            startCal.set(Calendar.DAY_OF_MONTH, 1);
            startCal.set(Calendar.MONTH, 0);
            startCal.set(Calendar.HOUR_OF_DAY, 0);
            startCal.set(Calendar.MINUTE, 0);
            startCal.set(Calendar.SECOND, 0);
        } else if (period.equals(reportPeriodArray[2])) {
            // Last 12 Months
            startCal.roll(Calendar.YEAR, -1);
        } else {
            // Specific month selected
            month = reportPeriodCbo.getSelectionIndex() - MONTH_OFFSET;
            startCal.set(Calendar.MONTH, month);
            startCal.set(Calendar.DAY_OF_MONTH, 1);
            startCal.set(Calendar.HOUR_OF_DAY, 0);
            startCal.set(Calendar.MINUTE, 0);
            startCal.set(Calendar.SECOND, 0);

            if (month > endCal.get(Calendar.MONTH)) {
                startCal.set(Calendar.YEAR, startCal.get(Calendar.YEAR) - 1);
            }

            // Set endCal to end of first day of next month, then
            // roll off a day to get the end of the last day of the
            // wanted month. This allows java to take care of leap
            // years, different number of days per month, etc.
            endCal.set(Calendar.YEAR, startCal.get(Calendar.YEAR));
            endCal.set(Calendar.MONTH, startCal.get(Calendar.MONTH) + 1);
            endCal.set(Calendar.DAY_OF_MONTH, 1);
            endCal.set(Calendar.HOUR_OF_DAY, 23);
            endCal.set(Calendar.MINUTE, 59);
            endCal.set(Calendar.SECOND, 59);

            endCal.add(Calendar.DAY_OF_MONTH, -1);
        }

        startDate = startCal.getTime();
        endDate = endCal.getTime();
        dman.setStartDate(startDate);
        dman.setEndDate(endDate);

        /* HSA filtering section */
        if (hsaCbo.getSelectionIndex() > 0) {
            /* a specific HSA has been chosen */
            hsaWhere = " lid in (SELECT lid from location where hsa = '"
                    + hsaCbo.getItem(hsaCbo.getSelectionIndex()) + "') and";
        } else {
            /* no specific HSA has been chosen, so don't filter on this */
            hsaWhere = "";
        }

        /* create the where clause */
        String start = dbFormat.format(startCal.getTime());
        String end = dbFormat.format(endCal.getTime());
        where = "WHERE" + hsaWhere + " obstime >= '"
                + start + "' and obstime <= '"
                + end  + "' and flood_event_id > 0 order by lid";

        locationLidList.clear();
        ArrayList<String> lidList = dman.getLidList(where);
        int i = 0;
        for (String lid : lidList) {
            ArrayList<FloodReportData> dataList = dman.getFloodRptData(lid, 
                    start, end);
            for (FloodReportData data : dataList) {
            	if ((data.getFloodEventId() > 0) && (data.getCrest() >= data.getFloodStage())) {
            		dataMap.put(lid + data.getFloodEventId(), data);
            	}
                i++;
            }
        }

        // Save the data map for later use
        dman.setReportData(dataMap);

        // Get sorted list of keys to the dataMap
        Set<String> keySet = dataMap.keySet();

        Iterator<String> iter = keySet.iterator();
        locationLidList.clear();
        String prevSite = null;
        String line = null;

        while (iter.hasNext()) {
            FloodReportData data = dataMap.get(iter.next());

            if (!data.getLid().equals(prevSite)) {
                if (data.getLongName().length() > 15) {
                    data.setLongName(data.getLongName().substring(0, 15));
                }
                line = String.format(format, data.getLongName(), data.getLid(),
                        data.getCrest(), fr.format(data.getCrestDate()), data
                                .getFloodStage());
            } else {
                line = String.format(format, " ", " ", data.getCrest(), fr
                        .format(data.getCrestDate()), data.getFloodStage());
            }
            locationList.add(line);
            locationLidList.add(data.getLid() + data.getFloodEventId());           
            locationList.setSelection(0);
            if (locationList.getSelectionIndex() !=-1 ) {            	
            	loadSignificantTimes();
                FloodReportDataManager.getInstance().setDrawGraph(true);
                floodCanvas.setSelectedKey(selectedKey);
                floodCanvas.redraw();
                locationLbl.setText("Location:  " + selectedLid);
            }
            prevSite = data.getLid();
        }
    }
    
    private void handleSelection() {
        loadSignificantTimes();
        FloodReportDataManager.getInstance().setDrawGraph(true);
        floodCanvas.setSelectedKey(selectedKey);
        floodCanvas.resetGraphCanvas();
        floodCanvas.redraw();

        locationLbl.setText("Location:  " + selectedLid);

    }

    /**
     * Display the Details for Selected Event dates
     */
    private void loadSignificantTimes() {
        FloodReportDataManager dataManager = FloodReportDataManager
                .getInstance();
        Map<String, FloodReportData> dataMap = dataManager.getReportData();
        double crestStage = HydroConstants.FLOOD_REPORT_MSG;
        String crestTxt = null;
        String crestTxt2 = null;
        selectedKey = locationLidList.get(locationList.getSelectionIndex());
        dataManager.setSelectedKey(selectedKey);
        FloodReportData floodData = dataMap.get(selectedKey);

        /* initialize */
        aboveFsTF.setText("");
        crestTF.setText("");
        belowFsTF.setText("");

        /* if flood event info exists */
        if (floodData != null) {
            /* get the crest info */
            crestStage = floodData.getCrest();
            selectedLid = floodData.getLid();
            dataManager.setSelectedLid(selectedLid);

            /* store crest time info; check for a sustained crest. */
            if (crestStage != HydroConstants.FLOOD_REPORT_MSG) {
                crestTxt = fr.format(floodData.getCrestDate()) + " Z";
                if (floodData.getLastCrest() != HydroConstants.FLOOD_REPORT_MSG) {
                    crestTxt2 = fr.format(floodData.getLastCrestDate()) + " Z";
                    crestTxt.concat("\n" + crestTxt2);
                }
            } else {
                crestTxt = "";
            }

            /* get the passthru flood stage times */
            Date[] times = dataManager.getPassthruTimes(floodData);

            if (times[0] != null) {
                aboveFsTF.setText(fr.format(times[0]) + " Z");
            }
            crestTF.setText(crestTxt);
            if (times[1] != null) {
                belowFsTF.setText(fr.format(times[1]) + " Z");
            }
        }

        /* ALSO load the data for the flood time series into the scrolled list */
        listEventVals(floodData);
    }

    /**
     * Update the event list.
     * 
     * @param data
     *            The FloodReportData object
     */
    private void listEventVals(FloodReportData data) {
        eventList.removeAll();

        if (data != null) {
            ArrayList<Object[]> eventData = FloodReportDataManager
                    .getInstance().getFloodEventData(data.getLid(),
                            data.getFloodEventId());

            for (Object[] oa : eventData) {
                double val = (Double) oa[0];
                String date = fr.format((Date) oa[1]);
                eventList.add(String.format(" %12.2f    %s", val, date));
            }
        }
    }

    /**
     * Save the data to file.
     */
    private void exportFloodTsFile() {
        final String dirToken = "whfs_report_dir";
        String reportDir = AppsDefaults.getInstance().getToken(dirToken);

        FileDialog dialog = new FileDialog(shell, SWT.SAVE);

        dialog.setFilterPath(reportDir);
        String filename = dialog.open();
        if (filename == null) {
            return;
        }

        String output = getFileText();

        try {
            BufferedWriter out = new BufferedWriter(new FileWriter(filename));
            out.write(output);
            out.close();
        } catch (IOException e) {
            e.printStackTrace();
        }
    }

    /**
     * Get the text to output to the file.
     * 
     * @return The formated text for the output file
     */
    private String getFileText() {
        FloodReportDataManager dman = FloodReportDataManager.getInstance();
        final String dataFormat = "    %7.1f    %-12s - %-12s%s  %8.2f%s  %-12s\n";
        double roc = HydroConstants.FLOOD_REPORT_MSG;
        String prevBasin = null;
        String prevStream = null;
        String prevLid = null;

        StringBuilder buffer = new StringBuilder();
        buffer
                .append("Flood report information listing generated by WHFS HydroBase.\n");
        Calendar now = Calendar.getInstance(TimeZone.getTimeZone("GMT"));
        buffer.append("File created on:  "
                + HydroConstants.DATE_FORMAT.format(now.getTime()) + " Z\n");

        /* get and write the time window */
        buffer.append("\nFor period beginning: "
                + HydroConstants.DATE_FORMAT.format(startDate));
        buffer.append("\n          and ending: "
                + HydroConstants.DATE_FORMAT.format(endDate));
        buffer.append("\nAll times given in GMT.\n");
        buffer.append("N/A= Not Available   q= questionable\n\n");
        buffer
                .append("Flood events are grouped by location, river, and basin.\n");
        buffer
                .append("For each event, the following information is given:\n\n");
        buffer
                .append("    FLD STG     ABOVE FLOOD  - BELOW FLOOD    CREST    TIME\n");
        buffer
                .append("\n------------------------------------------------------------------\n");

        /* No Data, return the text */
        if (locationList.getItemCount() == 0) {
            buffer.append("No data for reported period.\n");
            return buffer.toString();
        }

        Map<String, FloodReportData> dataMap = dman.getReportData();
        Set<String> keySet = dataMap.keySet();
        Iterator<String> iter = keySet.iterator();
        while (iter.hasNext()) {
            // for (int i = 0; i < locationLidList.size(); i++) {
            // String lid = locationLidList.get(i);
            String lid = iter.next();

            FloodReportData data = dataMap.get(lid);

            if (data != null) {

                /*
                 * get the crest info. Any missing crest info is indicated by a
                 * missing indicator for the value.
                 */

                /* get the passthru flood stage times */
                Date[] times = FloodReportDataManager.getInstance()
                        .getPassthruTimes(data);
                Date aboveTime = null;
                Date belowTime = null;
                String aboveTxt = "-999";
                String belowTxt = "-999";
                String crestTxt = "-999";
                if (times[0] != null) {
                    aboveTime = times[0];
                    aboveTxt = sdf.format(aboveTime);
                }

                crestTxt = sdf.format(data.getCrestDate());

                if (times[1] != null) {
                    belowTime = times[1];
                    belowTxt = sdf.format(belowTime);
                }

                /*
                 * check for some obvious error conditions, i.e. if fall below
                 * fld before crest, or if crest much higher than fld stage, or
                 * if rate-of-change too high
                 */
                String belowFlagStr = " ";
                String crestFlagStr = " ";

                if ((belowTime != null)
                        && (belowTime.getTime() < data.getCrestDate().getTime())
                        && (data.getCrest() != HydroConstants.FLOOD_REPORT_MSG)) {
                    belowFlagStr = "q";
                }

                if ((data.getCrest() == HydroConstants.FLOOD_REPORT_MSG)
                        || (Math.abs(data.getCrest() - data.getFloodStage()) > 50)) {
                    crestFlagStr = "q";
                }

                if (data.getLid().equals("EAKO2")) {
                    roc = 0;
                }

                if ((data.getCrest() != HydroConstants.FLOOD_REPORT_MSG)
                        && (data.getCrestDate() != null)
                        && (aboveTime != null)
                        && (data.getCrestDate().getTime() != aboveTime
                                .getTime())) {
                    roc = 3600 * ((data.getCrest() - data.getFloodStage())
                            / data.getCrestDate().getTime() - aboveTime
                            .getTime());
                    if (Math.abs(roc) > 10) {
                        crestFlagStr = "q";
                    }
                }

                if ((data.getCrest() != HydroConstants.FLOOD_REPORT_MSG)
                        && (data.getCrestDate() != null)
                        && (belowTime != null)
                        && (data.getCrestDate().getTime() != belowTime
                                .getTime())) {
                    roc = 3600 * ((data.getCrest() - data.getFloodStage())
                            / data.getCrestDate().getTime() - belowTime
                            .getTime());
                    if (Math.abs(roc) > 10) {
                        crestFlagStr = "q";
                    }
                }

                /* now output the info for the event */
                String stream = dman.getRiver(data.getLid());
                String basin = dman.getRiverBasin(data.getLid());

                if (basin!=null && prevBasin!=null && !basin.equals(prevBasin)) {
                    buffer.append("\n\nBASIN:  " + basin + "\n");
                }

                if (stream!=null && prevStream!=null && !stream.equals(prevStream)) {
                    buffer.append("\n  RIVER:  " + stream + "\n");
                }

                if (!data.getLid().equals(prevLid)) {
                    buffer
                            .append("\n    " + data.getLongName() + ", "
                                    + dman.getState(data.getLid()) + " (" + data.getLid()
                                    + ")\n");
                }

                buffer.append(String.format(dataFormat, data.getFloodStage(),
                        aboveTxt, belowTxt, belowFlagStr, data.getCrest(),
                        crestFlagStr, crestTxt));

                /* preserve these for the next pass */
                prevLid = data.getLid();
                prevStream = stream;
                prevBasin = basin;
            } else {
                buffer.append("Did NOT find data for:" + lid);
            }
        }

        return buffer.toString();
    }

    /**
     * Delete record from the floodts table.
     */
    private void deleteRecord() {
        // get the data object
        FloodReportDataManager dman = FloodReportDataManager.getInstance();
        Map<String, FloodReportData> dataMap = dman.getReportData();
        FloodReportData data = dataMap.get(selectedLid);
        String where = " where lid ='" + data.getLid()
                + "' and flood_event_id = " + data.getFloodEventId();
        int status = dman.deleteFloodEvent(where);
        if (status == -1) {
            MessageBox mbe = new MessageBox(shell, SWT.OK | SWT.ERROR);
            mbe.setText("An Error Occurred");
            mbe.setMessage("ERROR while attempting to delete flood event...");
            mbe.open();
        } else {
            updateFloodList();
        }
    }

    /**
     * Insert the crest into the crest table.
     */
    private int insertRecord() {
        FloodReportDataManager dman = FloodReportDataManager.getInstance();
        Map<String, FloodReportData> dataMap = dman.getReportData();
        FloodReportData data = dataMap.get(selectedLid);
        String cremark = "Inserted from the FloodTS table via Hydrobase";
        long discharge = Math.round(RatingUtils.stage2discharge(data.getLid(),
                data.getCrest()));

        // Build the insert statement
        StringBuilder sql = new StringBuilder();
        sql.append("insert into crest (lid, datcrst, timcrst, cremark, stage");

        if (discharge != HydroConstants.RATING_CONVERT_FAILED) {
            sql.append(", q");
        }

        sql.append(") values('" + data.getLid() + "', ");
        sql.append("'" + dateFormat.format(data.getCrestDate()) + "', ");
        sql.append("'" + hourFormat.format(data.getCrestDate()) + "', ");
        sql.append("'" + cremark + "', ");
        sql.append("" + data.getCrest());
        if (discharge != HydroConstants.RATING_CONVERT_FAILED) {
            sql.append(", " + discharge);
        }

        sql.append(")");

        int status = dman.insertCrest(sql.toString());
        if (status < 0) {
            MessageBox mbe = new MessageBox(shell, SWT.OK | SWT.ERROR);
            if (status == -2) {
                mbe.setMessage("Record already exists in the Crest table!");
            } else {
                mbe.setMessage("ERROR while attempting to insert crest...");
            }
            mbe.open();
        }

        return status;
    }

    /**
     * Clear the canvas.
     */
    private void clearCanvas() {
        FloodReportDataManager.getInstance().setDrawGraph(false);
        floodCanvas.redraw();
    }

    /**
     * Set the stage label text.
     * 
     * @param text
     */
    public void setStageLbl(String text) {
        stageLbl.setText(text);
    }
}
