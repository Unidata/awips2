package com.raytheon.viz.hydrobase.addEditFloodTS;

import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.Calendar;
import java.util.Collections;
import java.util.Date;
import java.util.LinkedList;
import java.util.ListIterator;
import java.util.Map;
import java.util.TimeZone;

import org.eclipse.swt.SWT;
import org.eclipse.swt.custom.ScrolledComposite;
import org.eclipse.swt.events.MouseAdapter;
import org.eclipse.swt.events.MouseEvent;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.graphics.Cursor;
import org.eclipse.swt.layout.FormAttachment;
import org.eclipse.swt.layout.FormData;
import org.eclipse.swt.layout.FormLayout;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Layout;
import org.eclipse.swt.widgets.List;
import org.eclipse.swt.widgets.MessageBox;
import org.eclipse.swt.widgets.Shell;

import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.time.util.TimeUtil;
import com.raytheon.uf.viz.core.catalog.DirectDbQuery;
import com.raytheon.uf.viz.core.catalog.DirectDbQuery.QueryLanguage;
import com.raytheon.viz.hydrobase.FloodReportData;
import com.raytheon.viz.hydrobase.FloodReportDataManager;
import com.raytheon.viz.hydrobase.FloodReportDlg;
import com.raytheon.viz.hydrocommon.HydroConstants;
import com.raytheon.viz.ui.dialogs.CaveSWTDialog;

/**
 * This class display the Edit Flood Event dialog.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#     Engineer    Description
 * ----------   ----------  ----------- --------------------------
 * 6/10/2015    DCS15095    wkwock      Initial creation
 * 
 * </pre>
 * 
 * @author wkwock
 * @version 1.0
 */
public class AddEditFloodEventDlg extends CaveSWTDialog implements ITSCompositeAction {
    private final IUFStatusHandler statusHandler = UFStatus
            .getHandler(AddEditFloodEventDlg.class);

    /**
     * Location list control.
     */
    private List lidsLst;

    private Label lidLbl;

    private Label stageLbl;

    private SimpleDateFormat dateFormat = null;

    private String lid = null;

    private int floodEventID = -1;

    private double floodStage = 0;

    private String selectedKey;

    private Composite obsComp;

    private ScrolledComposite obsSComp;

    private FloodReportDlg owner;

    private LinkedList<TSComposite> tsLinkedList = null;

    /**
     * Constructor
     * 
     * @param parent
     * @param key
     * @param owner
     */
    public AddEditFloodEventDlg(Shell parent, String key, FloodReportDlg owner) {
        super(parent, SWT.DIALOG_TRIM, CAVE.DO_NOT_BLOCK);
        this.owner = owner;
        this.selectedKey = key;
        if (selectedKey == null) {
            setText("Adding New Flood Event");
        } else {
            FloodReportDataManager dman = FloodReportDataManager.getInstance();
            lid = dman.getSelectedLid();
            floodStage = dman.getFloodStage(lid);
            floodEventID = dman.getReportData().get(selectedKey)
                    .getFloodEventId();

            setText("Editing Flood Event for " + lid);
        }

        dateFormat = new SimpleDateFormat("MM/dd/yyyy HH:mm");
        dateFormat.setTimeZone(TimeZone.getTimeZone("GMT"));
        tsLinkedList = new LinkedList<TSComposite>();
    };

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.viz.ui.dialogs.CaveSWTDialogBase#constructShellLayout()
     */
    @Override
    protected Layout constructShellLayout() {
        // Create the main layout for the shell.
        GridLayout mainLayout = new GridLayout(1, false);
        mainLayout.marginHeight = 2;
        mainLayout.marginWidth = 2;
        mainLayout.verticalSpacing = 2;
        return mainLayout;
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.viz.ui.dialogs.CaveSWTDialogBase#disposed()
     */
    @Override
    protected void disposed() {

    }

    /*
     * initialize components
     * 
     * @see
     * com.raytheon.viz.ui.dialogs.CaveSWTDialogBase#initializeComponents(org
     * .eclipse.swt.widgets.Shell)
     */
    @Override
    protected void initializeComponents(Shell shell) {
        setReturnValue(false);

        // Initialize all of the controls and layouts
        initializeComponents();
    }

    /**
     * Initialize the components on the display.
     */
    private void initializeComponents() {
        createControls();

        createBottomButtons();

        populateLIDLst();

        populateObsComp();
    }

    /**
     * Create the middle list and canvas controls.
     */
    private void createControls() {
        GridData gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        Composite mainComp = new Composite(shell, SWT.NONE);
        mainComp.setLayout(new GridLayout(2, false));
        mainComp.setLayoutData(gd);

        gd = new GridData(SWT.DEFAULT, SWT.TOP, false, true);
        Composite leftComp = new Composite(mainComp, SWT.NONE);
        leftComp.setLayout(new GridLayout(1, false));
        leftComp.setLayoutData(gd);

        Label listLbl = new Label(leftComp, SWT.NONE);
        listLbl.setText("Select a LID");

        if (selectedKey == null) {
            gd = new GridData(50, 400);
            gd.horizontalSpan = 2;
            lidsLst = new List(leftComp, SWT.BORDER | SWT.SINGLE | SWT.V_SCROLL);
            lidsLst.setLayoutData(gd);
        } else {
            listLbl.setText("");// not use it
            lidLbl = new Label(leftComp, SWT.None);
            lidLbl.setText("LID: " + selectedKey);
            stageLbl = new Label(leftComp, SWT.None);
            stageLbl.setText("flood stage: " + floodStage);
        }

        gd = new GridData(SWT.FILL, SWT.FILL, true, true);
        Composite rightComp = new Composite(mainComp, SWT.NONE);
        rightComp.setLayout(new GridLayout(1, false));
        rightComp.setLayoutData(gd);

        gd = new GridData();
        gd.horizontalIndent = 4;
        gd.horizontalSpan = 2;
        Label obsListLbl = new Label(rightComp, SWT.NONE);
        obsListLbl.setText("Observation");
        obsListLbl.setLayoutData(gd);

        obsSComp = new ScrolledComposite(rightComp, SWT.BORDER | SWT.V_SCROLL);
        obsSComp.setLayoutData(new GridData(350, 400));
        obsComp = new Composite(obsSComp, SWT.NONE);
        obsComp.setLayout(new FormLayout());

        obsSComp.setAlwaysShowScrollBars(true);
        obsSComp.setContent(obsComp);
        obsSComp.setExpandHorizontal(true);
        obsSComp.setExpandVertical(true);
        obsSComp.setMinSize(obsComp.computeSize(SWT.DEFAULT, SWT.DEFAULT));

        Label topInsertLbl = new Label(obsComp, SWT.NONE);
        topInsertLbl.setForeground(obsSComp.getDisplay().getSystemColor(
                SWT.COLOR_GREEN));
        topInsertLbl.setText("-->");
        Cursor handCursor = this.getDisplay().getSystemCursor(SWT.CURSOR_HAND);
        topInsertLbl.setCursor(handCursor);
        FormData insertFd = new FormData();
        insertFd.top = new FormAttachment(0, 4);
        insertFd.left = new FormAttachment(0, 4);
        topInsertLbl.setLayoutData(insertFd);
        final AddEditFloodEventDlg floodEventDlg = this;
        topInsertLbl.addMouseListener(new MouseAdapter() {
            @Override
            public void mouseUp(MouseEvent event) {
                TSComposite tsComp= new TSComposite(obsComp, 0.0,TimeUtil.newGmtCalendar(),floodEventDlg );
                tsLinkedList.add(0,tsComp);
                reorganizeGui();
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
        Button saveBtn = new Button(buttonComp, SWT.PUSH|SWT.LEFT);
        saveBtn.setText("Save Event");
        saveBtn.setLayoutData(gd);
        saveBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                int status = saveNewEditTS();
                if (status >= 0) {
                    close();
                    owner.refreshReport();
                }
            }
        });

        gd = new GridData(SWT.RIGHT, SWT.DEFAULT, true, false);
        gd.widthHint = buttonWidth;
        Button cancelBtn = new Button(buttonComp, SWT.PUSH|SWT.RIGHT);
        cancelBtn.setText("Cancel");
        cancelBtn.setLayoutData(gd);
        cancelBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                close();
            }
        });
    }

    /**
     * populate LIDLst. LIDs are from riverstat table
     */
    private void populateLIDLst() {
        FloodReportDataManager dman = FloodReportDataManager.getInstance();
        if (selectedKey == null) {
            java.util.List<String> lidList = dman.getLidListFromRiverStat("");
            Collections.sort(lidList);
            lidsLst.removeAll();
            for (String lid : lidList) {
                lidsLst.add(lid);
            }
        } else {
            dman.setSelectedKey(selectedKey);
            lidLbl.setText("LID: " + lid);
        }
    }

    /**
     * Populate the observation composite
     */
    private void populateObsComp() {
        if (selectedKey == null) {
           TSComposite tsComp= new TSComposite(obsComp, 0.0,TimeUtil.newGmtCalendar(), this);
           tsLinkedList.add(0,tsComp);
           reorganizeGui();
        } else {
            FloodReportDataManager dataManager = FloodReportDataManager
                    .getInstance();
            Map<String, FloodReportData> dataMap = dataManager.getReportData();
            dataManager.setSelectedKey(selectedKey);
            FloodReportData floodData = dataMap.get(selectedKey);

            java.util.List<Object[]> eventData = dataManager.getFloodEventData(
                    floodData.getLid(), floodData.getFloodEventId());

            SimpleDateFormat sdf = new SimpleDateFormat("MM/dd/yyyy HH:mm");
            try {
                for (Object[] oa : eventData) {
                    double val = (Double) oa[0];
                    String date = dateFormat.format((Date) oa[1]);
                    Calendar cal = Calendar.getInstance();
                    cal.setTime(sdf.parse(date));
                    TSComposite tsComp= new TSComposite(obsComp, val,cal,this);
                    tsLinkedList.add(tsComp);
                    reorganizeGui();
                }
            } catch (ParseException e) {
                statusHandler.error("Failed to parse time", e);
            }

        }
    }

    /**
     * Save this new time series to table floodts
     * 
     * @return status:0 succeed to save data, -1 failed to save data.
     */
    private int saveNewEditTS() {
        FloodReportDataManager dman = FloodReportDataManager.getInstance();
        String tsLID = this.lid;
        int eventID = this.floodEventID;
        if (selectedKey == null) {
            tsLID = lidsLst.getSelection()[0];
            eventID = dman.getMaxFloodeventID(tsLID) + 1;
        }

        SimpleDateFormat sdf = new SimpleDateFormat("yyyy-MM-dd HH:mm:00");
        StringBuilder sqlStr = new StringBuilder(
                "insert into floodts (lid,obstime,flood_event_id,value) ");
        boolean firstOneFlg = true;
        try {
           ListIterator<TSComposite> listIterator = tsLinkedList.listIterator();
           while (listIterator.hasNext()) {
                if (firstOneFlg) {
                    sqlStr.append("values ");
                } else {
                    sqlStr.append(",");
                }
                TSComposite tsComp = listIterator.next();
                sqlStr.append("('").append(tsLID).append("','")
                        .append(sdf.format(tsComp.getDateTime().getTime()))
                        .append("',").append(eventID).append(",")
                        .append(tsComp.getValue()).append(")");
                firstOneFlg = false;
            }
        } catch (NumberFormatException nfe) {
            MessageBox mbe = new MessageBox(shell, SWT.OK | SWT.ERROR);
            mbe.setMessage("Error: an observe value is invalid. Please fix and try again.");
            mbe.open();
            return -1;
        }

        int status = 0;
        try {
            if (selectedKey != null) {
                StringBuilder deleteSql = new StringBuilder();
                deleteSql.append("delete from floodts where lid='")
                        .append(tsLID).append("' and flood_event_id=")
                        .append(eventID);
                status = DirectDbQuery.executeStatement(deleteSql.toString(),
                        HydroConstants.IHFS, QueryLanguage.SQL);
            }

            status = DirectDbQuery.executeStatement(sqlStr.toString(),
                    HydroConstants.IHFS, QueryLanguage.SQL);
        } catch (Exception e) {
            MessageBox mbe = new MessageBox(shell, SWT.OK | SWT.ERROR);
            mbe.setMessage("Failed to save data. Please check for observe time confliction.");
            mbe.open();
            return -1;
        }

        return status;
    }
    
    /**
     * add a new TS composite to the linked list and the GUI
     */
    @Override
    public void addTSComp (TSComposite tsComp) {
        int index = tsLinkedList.indexOf(tsComp);
        TSComposite newTSComp= new TSComposite(obsComp, 0.0,TimeUtil.newGmtCalendar(),this);
        tsLinkedList.add(index+1, newTSComp);
        reorganizeGui();
    }
    
    /**
     * Remove a TS composite from the linked list and the GUI
     */
    @Override
    public void removeTSComp (TSComposite tsComp) {
        if (tsLinkedList.size()>1){
            tsLinkedList.remove(tsComp);
            tsComp.dispose();
            reorganizeGui();
        } else {
            tsComp.setValue(0.0);
        }
    }
    
    /**
     * Reflect the tsLinkedList on the AddEditFloodEvent GUI
     */
    private void reorganizeGui () {
        ListIterator<TSComposite> listIterator = tsLinkedList.listIterator();
        TSComposite lastTSComp = null;
        TSComposite tsComp= null;
        while (listIterator.hasNext()) {
            tsComp=listIterator.next();
            FormData tfd = new FormData();
            if (lastTSComp==null) {
                tfd.top = new FormAttachment(0, 20);
            } else {
                tfd.top = new FormAttachment(lastTSComp, 1);
            }
            tsComp.setLayoutData(tfd);
            lastTSComp = tsComp;
         }

        obsComp.setSize(obsComp.computeSize(SWT.DEFAULT, SWT.DEFAULT));
        obsSComp.setMinSize(obsComp.computeSize(SWT.DEFAULT, SWT.DEFAULT));
    }
}
