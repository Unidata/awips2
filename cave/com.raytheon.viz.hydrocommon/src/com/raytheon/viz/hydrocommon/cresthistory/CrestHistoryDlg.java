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
package com.raytheon.viz.hydrocommon.cresthistory;

import java.text.DecimalFormat;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Calendar;
import java.util.Date;
import java.util.TimeZone;

import org.eclipse.swt.SWT;
import org.eclipse.swt.events.ModifyEvent;
import org.eclipse.swt.events.ModifyListener;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
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

import com.raytheon.uf.common.dataquery.db.QueryResult;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.viz.hydrocommon.HydroConstants;
import com.raytheon.viz.hydrocommon.datamanager.HydroDBDataManager;
import com.raytheon.viz.ui.dialogs.CaveSWTDialog;

/**
 * This class displays the Crest History dialog.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date			Ticket#		Engineer	Description
 * ------------	----------	-----------	--------------------------
 * Sep 4, 2008				lvenable	Initial creation
 * Nov 24, 2008   1628      dhladky     Interactiveized.
 * Jan 13, 2009   1802      askripsk    Added check for DB FK constraints.
 * Feb 22, 2010   1985      mpduff      Apply button now reloads data list.
 * Apr 08, 2010   4277      mpduff      Fixed day offset problem.
 * Nov 11, 2010   5518      lbousaidi	fixed filter options to reflect 
 * 										changes in crest data list display
 * Nov 18, 2010   6981      lbousaidi   fixed Ok button and prelim problem
 * Mar 29,2012  14463       wkwock      Fix max # of char for remark text box to 255
 *                                      Also see https://bugs.eclipse.org/bugs/show_bug.cgi?id=43004
 * 
 * 
 * </pre>
 * 
 * @author lvenable
 * @version 1.0
 */
public class CrestHistoryDlg extends CaveSWTDialog implements
        ISelectedCrestData {

    /**
     * Control font.
     */
    private Font font;

    /**
     * Dialog title information.
     */
    private String lid;

    /**
     * Canvas displaying the crest history data.
     */
    private CrestHistoryCanvas crestHistCanvas;

    /**
     * Filter crests combo box.
     */
    private Combo filterCrestsCbo;

    /**
     * Sort crests combo box.
     */
    private Combo sortCrestsCbo;

    /**
     * Crest history list control.
     */
    private List crestHistoryList;

    /**
     * Stage text control.
     */
    private Text stageTF;

    /**
     * Date text control.
     */
    private Text dateTF;

    /**
     * Flow text control.
     */
    private Text flowTF;

    /**
     * Time text control.
     */
    private Text timeTF;

    /**
     * Status combo box.
     */
    private Combo statusCbo;

    /**
     * Old datum check box.
     */
    private Button oldDatumChk;

    /**
     * Ice jam check box.
     */
    private Button iceJamChk;

    /**
     * High water mark check box.
     */
    private Button highWaterMarkChk;

    /**
     * Suppress check box.
     */
    private Button suppressChk;

    /**
     * Remarks text control.
     */
    private Text remarksTF;

    /**
     * OK button.
     */
    private Button okBtn;

    /**
     * Apply button.
     */
    private Button applyBtn;

    /**
     * Cancel button.
     */
    private Button cancelBtn;

    /**
     * New button.
     */
    private Button newBtn;

    /**
     * Delete button.
     */
    private Button deleteBtn;

    /**
     * Flag indicating if all of the controls should be displayed.
     */
    private boolean fullControl = false;

    /**
     * Flag indicating if all, below or above Action Stage is selected.
     */
    private int allFlag= 0;
    /**
     * Crest history data.
     */
    private CrestHistoryData crestHistoryData;

    private CrestData selectedCrest = null;

    private DecimalFormat df = new DecimalFormat();

    private SimpleDateFormat sdf = new SimpleDateFormat("MM/dd/yyyy");

    private SimpleDateFormat stf = new SimpleDateFormat("HH:mm");

    private boolean enabled = false;
    
    /**
     * int that's keeps track of DB mode 0 = none 1 = new 2 = delete
     */
    private int function = 0;

    /**
     * text from the remark text box
     */
    private String currentRemarkText=null;
    
    /**
     * maximum number of character allowed in the remark text box
     */
    private final int MAX_REMARK_CHAR=80;
    /**

    /**
     * Constructor.
     * 
     * @param parent
     *            Parent shell.
     * @param lid
     *            from displayManager
     * @param fullControl
     *            Flag indicating if all of the controls should be displayed.
     */
    public CrestHistoryDlg(Shell parent, String lid, String title,
            boolean fullControl) {
        super(parent);
        setText("Crest History" + title);

        this.fullControl = fullControl;
        this.lid = lid;

        // setup your decimal formatter
        df.setMaximumFractionDigits(2);
        df.setMinimumIntegerDigits(1);
        df.setGroupingUsed(false);
        sdf.setTimeZone(TimeZone.getTimeZone("GMT"));
        stf.setTimeZone(TimeZone.getTimeZone("GMT"));
    }

    @Override
    protected Layout constructShellLayout() {
        // Create the main layout for the shell.
        GridLayout mainLayout = new GridLayout(2, false);
        mainLayout.marginHeight = 3;
        mainLayout.marginWidth = 3;
        return mainLayout;
    }

    @Override
    protected void disposed() {
        font.dispose();
    }

    @Override
    protected void initializeComponents(Shell shell) {
        setReturnValue(false);

        font = new Font(shell.getDisplay(), "Monospace", 10, SWT.NORMAL);

        // Initialize all of the controls and layouts
        getCrestData(allFlag);
        
        createLeftSideControls();
        createRightSideControls();
        addSeparator();
        createBottomButtons();

        if (crestHistoryData != null) {
            sortCrestHistoryList();
            updateCrestHistoryList();            
            crestHistoryList.setSelection(0);
            if ((crestHistoryData != null) && 
            		(crestHistoryData.getCrestDataArray().size() >0) ) {
            	setSelectedCrest(crestHistoryData.getCrestDataArray().get(0));
            }
        }

        executeEnable();
    }

    /**
     * Create the controls on the left side of the display.
     */
    private void createLeftSideControls() {
        Composite leftComp = new Composite(shell, SWT.NONE);
        GridLayout gl = new GridLayout(1, false);
        leftComp.setLayout(gl);

        crestHistCanvas = new CrestHistoryCanvas(leftComp, crestHistoryData,
                this);

        Composite leftControlsComp = new Composite(leftComp, SWT.NONE);
        gl = new GridLayout(4, false);
        leftControlsComp.setLayout(gl);

        Label filterCrestLbl = new Label(leftControlsComp, SWT.NONE);
        filterCrestLbl.setText("Filter Crests By:");

        filterCrestsCbo = new Combo(leftControlsComp, SWT.DROP_DOWN
                | SWT.READ_ONLY);
        filterCrestsCbo.add("All");
        filterCrestsCbo.add("Above Action Stage");
        filterCrestsCbo.add("Below Action Stage");
        filterCrestsCbo.select(0);

        filterCrestsCbo.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                if (filterCrestsCbo.getSelectionIndex() == 0) {
                    crestHistCanvas.drawAllCrestData();
                    getCrestData(0);
                } else if (filterCrestsCbo.getSelectionIndex() == 1) {
                    crestHistCanvas.drawDataAboveActionStage();
                    getCrestData(1);                    
                } else if (filterCrestsCbo.getSelectionIndex() == 2) {
                    crestHistCanvas.drawDataBelowActionStage();
                    getCrestData(2);                    
                }
                
                sortCrestHistoryList();
                updateCrestHistoryList();
                crestHistoryList.setSelection(0);
                if ((crestHistoryData != null) && 
                		(crestHistoryData.getCrestDataArray().size() >0)) {
                	setSelectedCrest(crestHistoryData.getCrestDataArray().get(0));
                } else {
                	clearData();
                }
            }
        });

        GridData gd = new GridData(190, SWT.DEFAULT);
        Label sortCrestLbl = new Label(leftControlsComp, SWT.RIGHT);
        sortCrestLbl.setText("Sort Crests By:");
        sortCrestLbl.setLayoutData(gd);

        sortCrestsCbo = new Combo(leftControlsComp, SWT.DROP_DOWN
                | SWT.READ_ONLY);
        sortCrestsCbo.add("Date");
        sortCrestsCbo.add("Flow");
        sortCrestsCbo.add("Stage");
        sortCrestsCbo.select(2);

        sortCrestsCbo.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                sortCrestHistoryList();
                updateCrestHistoryList();
            }
        });
    }

    /**
     * Create the controls on the right side of the display.
     */
    private void createRightSideControls() {
        Composite rightComp = new Composite(shell, SWT.NONE);
        GridLayout gl = new GridLayout(1, false);
        rightComp.setLayout(gl);

        createCrestHistoryList(rightComp);
        createSelectedCrestGroup(rightComp);
    }

    /**
     * Create the history list control.
     * 
     * @param parentComp
     *            Parent composite.
     */
    private void createCrestHistoryList(Composite parentComp) {
        // -----------------------------------------
        // Create Crest History List labels
        // -----------------------------------------
        Composite labelComp = new Composite(parentComp, SWT.NONE);
        GridLayout gl = new GridLayout(1, false);
        labelComp.setLayout(gl);

        GridData gd = new GridData();
        gd.horizontalIndent = 4;
        Label listLbl = new Label(labelComp, SWT.NONE);
        listLbl.setText(getCrestHistoryListLabel());
        listLbl.setFont(font);
        listLbl.setLayoutData(gd);

        // -----------------------------------------
        // Create Crest History list control
        // -----------------------------------------
        gd = new GridData(SWT.FILL, SWT.DEFAULT, false, false);
        gd.widthHint = 450;
        gd.heightHint = 150;
        crestHistoryList = new List(labelComp, SWT.BORDER | SWT.SINGLE
                | SWT.V_SCROLL);
        crestHistoryList.setLayoutData(gd);
        crestHistoryList.setFont(font);

        crestHistoryList.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                double stage = crestHistoryData.getCrestDataArray().get(
                        crestHistoryList.getSelectionIndex()).getStage();

                int year = crestHistoryData.getCrestDataArray().get(
                        crestHistoryList.getSelectionIndex()).getYear();

                crestHistCanvas.selectCrestData(stage, year);
                // sets the selected crest
                setSelectedCrest(crestHistoryData.getCrestDataArray().get(
                        crestHistoryList.getSelectionIndex()));
            }
        });

        if (crestHistoryData != null) {
            updateCrestHistoryList();
        }
    }

    /**
     * Create the Selected Crest group and controls.
     * 
     * @param parentComp
     *            Parent composite.
     */
    private void createSelectedCrestGroup(Composite parentComp) {
        GridData gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        Group selectedGroup = new Group(parentComp, SWT.NONE);
        selectedGroup.setText("Info for Selected Crest");
        GridLayout gl = new GridLayout(4, false);
        selectedGroup.setLayout(gl);
        selectedGroup.setLayoutData(gd);

        gd = new GridData(55, SWT.DEFAULT);
        Label stageLbl = new Label(selectedGroup, SWT.RIGHT);
        stageLbl.setText("Stage:");
        stageLbl.setLayoutData(gd);

        gd = new GridData(170, SWT.DEFAULT);
        stageTF = new Text(selectedGroup, SWT.BORDER);
        stageTF.setLayoutData(gd);

        gd = new GridData(90, SWT.DEFAULT);
        Label dateLbl = new Label(selectedGroup, SWT.RIGHT);
        dateLbl.setText("Date:");
        dateLbl.setLayoutData(gd);

        gd = new GridData(120, SWT.DEFAULT);
        dateTF = new Text(selectedGroup, SWT.BORDER);
        dateTF.setLayoutData(gd);

        gd = new GridData(55, SWT.DEFAULT);
        Label flowLbl = new Label(selectedGroup, SWT.RIGHT);
        flowLbl.setText("Flow:");
        flowLbl.setLayoutData(gd);

        gd = new GridData(170, SWT.DEFAULT);
        flowTF = new Text(selectedGroup, SWT.BORDER);
        flowTF.setLayoutData(gd);

        gd = new GridData(90, SWT.DEFAULT);
        Label timeLbl = new Label(selectedGroup, SWT.RIGHT);
        timeLbl.setText("Time:");
        timeLbl.setLayoutData(gd);

        gd = new GridData(120, SWT.DEFAULT);
        timeTF = new Text(selectedGroup, SWT.BORDER);
        timeTF.setLayoutData(gd);

        gd = new GridData();
        gd.horizontalSpan = 2;
        oldDatumChk = new Button(selectedGroup, SWT.CHECK);
        oldDatumChk.setText("Based on Old Datum");
        oldDatumChk.setLayoutData(gd);

        gd = new GridData(SWT.RIGHT, SWT.DEFAULT, true, false);
        gd.widthHint = 220;
        gd.horizontalSpan = 2;
        statusCbo = new Combo(selectedGroup, SWT.DROP_DOWN | SWT.READ_ONLY);
        statusCbo.add("Official Crest");
        statusCbo.add("Record Crest");
        statusCbo.add("Preliminary Status");
        statusCbo.select(0);
        statusCbo.setLayoutData(gd);

        gd = new GridData();
        gd.horizontalSpan = 2;
        highWaterMarkChk = new Button(selectedGroup, SWT.CHECK);
        highWaterMarkChk.setText("Observed by High Water Mark");
        highWaterMarkChk.setLayoutData(gd);

        gd = new GridData(SWT.RIGHT, SWT.DEFAULT, true, false);
        gd.widthHint = 220;
        gd.horizontalSpan = 2;
        iceJamChk = new Button(selectedGroup, SWT.CHECK);
        iceJamChk.setText("Affected by Ice Jam");
        iceJamChk.setLayoutData(gd);

        gd = new GridData();
        gd.horizontalSpan = 4;
        suppressChk = new Button(selectedGroup, SWT.CHECK);
        suppressChk.setText("Suppress Printed Display");
        suppressChk.setLayoutData(gd);

        gd = new GridData();
        gd.horizontalSpan = 4;
        Label remarksLbl = new Label(selectedGroup, SWT.NONE);
        remarksLbl.setText("Remarks");
        remarksLbl.setLayoutData(gd);

        gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        gd.heightHint = 100;
        gd.horizontalSpan = 4;
        remarksTF = new Text(selectedGroup, SWT.BORDER | SWT.WRAP);
        remarksTF.setLayoutData(gd);
        remarksTF.setTextLimit(MAX_REMARK_CHAR);

        /*Note: use this method to control number of character in remarkTF
         * because a bug in the Text class. 
         * See https://bugs.eclipse.org/bugs/show_bug.cgi?id=43004*/
        currentRemarkText=remarksTF.getText();
        ModifyListener listener = new ModifyListener() {
        	public void modifyText(ModifyEvent e) {
        		if (remarksTF.getText().length()>MAX_REMARK_CHAR){
        			remarksTF.setText(currentRemarkText);
        			shell.getDisplay().beep();
        		}
        		else
        			currentRemarkText=remarksTF.getText();
        	}
        };

        remarksTF.addModifyListener(listener);
    }

    /**
     * Add a horizontal separator to the main display.
     */
    private void addSeparator() {
        GridData gd = new GridData(GridData.FILL_HORIZONTAL);
        gd.horizontalSpan = 2;
        Label sepLbl = new Label(shell, SWT.SEPARATOR | SWT.HORIZONTAL);
        sepLbl.setLayoutData(gd);
    }

    /**
     * Create the buttons at the bottom of the display.
     */
    private void createBottomButtons() {
        GridData gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        gd.horizontalSpan = 2;
        Composite buttonComp = new Composite(shell, SWT.NONE);
        buttonComp.setLayout(new GridLayout(5, fullControl));
        buttonComp.setLayoutData(gd);

        boolean takeAvailableSpace = true;
        int actionButtonWidth = 85;

        if (fullControl == false) {
            actionButtonWidth = 0;
            takeAvailableSpace = false;
        }
        int cancelBtnWidth = 85;

        gd = new GridData(SWT.CENTER, SWT.DEFAULT, takeAvailableSpace, false);
        gd.widthHint = actionButtonWidth;
        okBtn = new Button(buttonComp, SWT.PUSH);
        okBtn.setText("OK");
        okBtn.setLayoutData(gd);
        okBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                function = 3;
                apply();
                shell.dispose();
            }
        });

        gd = new GridData(SWT.CENTER, SWT.DEFAULT, takeAvailableSpace, false);
        gd.widthHint = actionButtonWidth;
        applyBtn = new Button(buttonComp, SWT.PUSH);
        applyBtn.setText("Apply");
        applyBtn.setLayoutData(gd);
        applyBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                function = 3;
                apply();
                if (function == 1) {
                    crestHistoryList
                            .setSelection(findIndex(getSelectedCrest()));
                }
            }
        });

        gd = new GridData(SWT.CENTER, SWT.DEFAULT, true, false);
        gd.widthHint = cancelBtnWidth;
        cancelBtn = new Button(buttonComp, SWT.PUSH);
        cancelBtn.setText("Close");
        cancelBtn.setLayoutData(gd);
        cancelBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                clearData();
                function = 0;
                selectedCrest = null;
                shell.dispose();
            }
        });

        gd = new GridData(SWT.CENTER, SWT.DEFAULT, takeAvailableSpace, false);
        gd.widthHint = actionButtonWidth;
        newBtn = new Button(buttonComp, SWT.PUSH);
        newBtn.setText("New");
        newBtn.setLayoutData(gd);
        newBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                function = 1;
                create();
            }
        });

        gd = new GridData(SWT.CENTER, SWT.DEFAULT, takeAvailableSpace, false);
        gd.widthHint = actionButtonWidth;
        deleteBtn = new Button(buttonComp, SWT.PUSH);
        deleteBtn.setText("Delete");
        deleteBtn.setLayoutData(gd);
        deleteBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                function = 2;
                try {
                    delete();
                } catch (VizException e) {
                    MessageBox mb = new MessageBox(shell, SWT.ICON_ERROR
                            | SWT.OK);
                    mb.setText("Unable to Delete");
                    StringBuilder sb = new StringBuilder(
                            "An error occurred during the delete.\n");
                    sb.append(e.getCause() + "\n" + e.getMessage());
                    mb.setMessage(sb.toString());
                    mb.open();

                    e.printStackTrace();
                }
                selectedCrest = null;
            }
        });

        if (fullControl == false) {
            okBtn.setVisible(false);
            applyBtn.setVisible(false);
            newBtn.setVisible(false);
            deleteBtn.setVisible(false);
        }
    }

    /**
     * Sort the data in the crest history list control.
     */
    private void sortCrestHistoryList() {

        if (sortCrestsCbo.getText().compareTo("Flow") == 0) {
            crestHistoryData.sortByFlow();
        } else if (sortCrestsCbo.getText().compareTo("Date") == 0) {
            crestHistoryData.sortByDate();
        } else {
            crestHistoryData.sortByStage();
        }
    }

    /**
     * Update the data in the crest history list control.
     */
    private void updateCrestHistoryList() {
        crestHistoryList.removeAll();

        ArrayList<CrestData> crestDataArray = crestHistoryData
                .getCrestDataArray();

        for (CrestData crestData : crestDataArray) {
            crestHistoryList.add(crestData.getFormattedData());
        }
    }

    /**
     * Update the crest data indicating it has been selected.
     */
    public void crestDataSelected(double stage, int year) {
        ArrayList<CrestData> crestDataArray = crestHistoryData
                .getCrestDataArray();

        for (int i = 0; i < crestDataArray.size(); i++) {
            if ((crestDataArray.get(i).getStage() == stage)
                    && (crestDataArray.get(i).getYear() == year)) {
                crestHistoryList.select(i);
                setSelectedCrest(crestHistoryData.getCrestDataArray().get(i));
            }
        }
    }

    /**
     * Get the label text for the crest history list control.
     * 
     * @return Label text.
     */
    private String getCrestHistoryListLabel() {
        String format = "     %S       %S       %S          %S";

        String labelStr = String
                .format(format, "Stage", "Flow", "Date", "Time");

        return labelStr;
    }

    /**
     * getting the crest data with All/Above/Bellow Action
	 * Stage flag
     */
    private void getCrestData(int allFlag) {   
        CrestHistoryDataManager crestManager = CrestHistoryDataManager
                .getInstance();
        crestHistoryData = crestManager.getRiverCrestData(lid, fullControl, allFlag);       
        
        if (crestHistoryData != null) {
            setEnabled(true);
        } else {
            setEnabled(false);
        }
    }

    /**
     * Gets the selected crest
     * 
     * @return
     */
    private CrestData getSelectedCrest() {
        return selectedCrest;
    }

    /**
     * Sets the selection boxes and text fields to chosen values
     * 
     * @param selectedCrest
     */
    private void setSelectedCrest(CrestData selectedCrest) {

        this.selectedCrest = selectedCrest;
        
        if (selectedCrest.getStage() == HydroConstants.MISSING_VALUE) {
            stageTF.setText(HydroConstants.MISSING_STRING);
        } else {
            stageTF.setText(df.format(selectedCrest.getStage()));
        }
        if (selectedCrest.getFlow() == HydroConstants.MISSING_VALUE) {
            flowTF.setText(HydroConstants.MISSING_STRING);
        } else {
            flowTF.setText(df.format(selectedCrest.getFlow()));
        }

        if (selectedCrest.getCrestDate() != null) {
            dateTF.setText(sdf.format(selectedCrest.getCrestDate().getTime()));
            if (selectedCrest.isTime()) {
                timeTF.setText(stf.format(selectedCrest.getCrestDate()
                        .getTime()));
            } else {
                timeTF.setText("UNDEF");
            }
        } else {
            dateTF.setText("UNDEF");
            timeTF.setText("UNDEF");
        }

        oldDatumChk.setSelection(selectedCrest.isOldDatum());
        highWaterMarkChk.setSelection(selectedCrest.isHighWater());
        suppressChk.setSelection(selectedCrest.isSuppress());
        iceJamChk.setSelection(selectedCrest.isIce());

        if (selectedCrest.getRemarks() != null) {
            remarksTF.setText(selectedCrest.getRemarks());
        } else {
            remarksTF.setText("");
        }

        if (selectedCrest.getPrelim() != null) {
            if (selectedCrest.getPrelim().equals("O")) {
                statusCbo.select(0);
            } else if (selectedCrest.getPrelim().equals("R")) {
                statusCbo.select(1);
            } else {
                statusCbo.select(2);
            }
        } else {
            statusCbo.select(0);
        }
    }

    /**
     * Turns on and off data components based on whether it has or hasn't data
     * to display
     * 
     * @param enable
     * @return
     */
    private void setEnabled(boolean enabled) {
        this.enabled = enabled;
    }

    /**
     * execute on and off of components
     */
    private void executeEnable() {
        crestHistCanvas.setEnabled(enabled);
        crestHistoryList.setEnabled(enabled);
        sortCrestsCbo.setEnabled(enabled);
        filterCrestsCbo.setEnabled(enabled);
    }

    /**
     * apply changed crest values to graph
     */
    private void apply() {
        if (checkFKConstraintsMet()) {
            CrestData cd = new CrestData();
            // verify inputs

            if (!verifyDate(dateTF)) {
                return;
            }

            /***********************************************************
             * Check for a valid time (hh:mm) entered. If blank or "UNDEF", then
             * set to "UNDEF". If string is less than 5 characters, then reject.
             * If the third character is not a colon or the first, second,
             * fourth or fifth character is a colon, then reject. If 00 > hh >
             * 23 or 00 > mm > 59, then reject.
             ***********************************************************/
            if (!timeTF.getText().equals("") && !verifyTime(timeTF)) {
                return;
            }

            String tmpStage = stageTF.getText();
            String tmpQ = flowTF.getText();

            // Checks to see if at least one or the other string has some value
            // in it
            if (((tmpStage != null) && !isBlank(tmpStage))
                    || ((tmpQ != null) && !isBlank(tmpQ))) {
                if (!isBlank(tmpStage)) {
                    if (!stageTF.getText().equals(HydroConstants.MISSING_STRING)) {
                        cd.setStage(new Double(stageTF.getText()));
                    }
                }
                if (!isBlank(tmpQ)) {
                    if (!flowTF.getText().equals(HydroConstants.MISSING_STRING)) {
                        cd.setFlow(new Integer(flowTF.getText()));
                    }
                }
            } else {
                error(
                        "You must enter either a stage value or a flow value...\n",
                        null);
            }

            // adding a new crestdata point
            Calendar cal = Calendar.getInstance(TimeZone.getTimeZone("GMT"));

            // a little fanagling with the dates here...
            if (!timeTF.getText().equals("UNDEF") 
            		&& 	(!timeTF.getText().equals(""))) {
                try {
                    Calendar calDate = Calendar.getInstance(TimeZone
                            .getTimeZone("GMT"));
                    Calendar calTime = Calendar.getInstance(TimeZone
                            .getTimeZone("GMT"));
                    Date time = stf.parse(timeTF.getText());
                    calTime.setTime(time);
                    calDate.setTimeInMillis(sdf.parse(dateTF.getText())
                            .getTime());

                    cal.set(calDate.get(Calendar.YEAR), calDate
                            .get(Calendar.MONTH), calDate
                            .get(Calendar.DAY_OF_MONTH), calTime
                            .get(Calendar.HOUR_OF_DAY), calTime
                            .get(Calendar.MINUTE));
                    cd.setIsTime(true);
                } catch (ParseException pe) {
                    pe.printStackTrace();
                }
            } else {
                try {
                    cal.setTimeInMillis(sdf.parse(dateTF.getText()).getTime());
                    cd.setIsTime(false);
                } catch (ParseException pe) {
                    pe.printStackTrace();
                }
            }
            cd.setCrestDate(cal);
            cd.setOldDatum(oldDatumChk.getSelection());
            cd.setHighWater(highWaterMarkChk.getSelection());
            cd.setSuppress(suppressChk.getSelection());
            cd.setIceJam(iceJamChk.getSelection());

            if (statusCbo.getItem(statusCbo.getSelectionIndex()).equals(
                    "Official Crest"))
                cd.setPrelim("O");
            else if (statusCbo.getItem(statusCbo.getSelectionIndex()).equals(
                    "Record Crest")) {
                cd.setPrelim("R");
            } else {
                cd.setPrelim("P");
            }
            cd.setRemarks(remarksTF.getText());

            try {
                updateCrestDB(cd);
            } catch (VizException e) {
                MessageBox mb = new MessageBox(shell, SWT.ICON_ERROR | SWT.OK);
                mb.setText("Unable to Save");
                StringBuilder sb = new StringBuilder(
                        "An error occurred during the save.\n");
                sb.append(e.getCause() + "\n" + e.getMessage());
                mb.setMessage(sb.toString());
                mb.open();

                e.printStackTrace();
            }
            getCrestData(allFlag);            
            sortCrestHistoryList();
            crestHistCanvas.updateCrestHistotryData(crestHistoryData);
            updateCrestHistoryList();
            
        }
    }

    /**
     * delete a crest point
     * 
     * @throws VizException
     */
    private void delete() throws VizException {
        if (getSelectedCrest() != null) {
            crestHistCanvas.updateCrestHistotryData(crestHistoryData);
            crestHistoryData.getCrestDataArray().remove(getSelectedCrest());
            sortCrestHistoryList();
            updateCrestDB(getSelectedCrest());
            clearData();
        }
    }

    /**
     * make a new crest for display, called it create because new is
     * well....used.
     */
    private void create() {
        // clear the crest window 
        
    	stageTF.setText("");
        flowTF.setText("");
        timeTF.setText("");
        dateTF.setText("");
        oldDatumChk.setSelection(false);
        highWaterMarkChk.setSelection(false);
        suppressChk.setSelection(false);
        iceJamChk.setSelection(false);

        statusCbo.select(3);
        remarksTF.setText("");
    }

    /**
     * add and delete crests from DB
     * 
     * @throws VizException
     */
    private void updateCrestDB(CrestData cd) throws VizException {
        CrestHistoryDataManager chdm = CrestHistoryDataManager.getInstance();
        String msg = null;

        if (function == 2) {
            chdm.deleteCrest(cd, lid);
            function = 0;
        } else {
            msg = chdm.insertCrest(cd, lid, selectedCrest, function);
            function = 0;
        }

        if (msg != null) {
            error(msg, null);
        }

        updateCrestHistoryList();
    }

    /* (non-Javadoc)
     * @see com.raytheon.viz.ui.dialogs.CaveSWTDialogBase#opened()
     */
    @Override
    protected void opened() {
        if ((crestHistoryData.getCrestDataArray() == null) || 
                (crestHistoryData.getCrestDataArray().size() == 0)) {
            return;
        }
        
        crestHistoryList.setSelection(0);
        double stage = crestHistoryData.getCrestDataArray().get(
                crestHistoryList.getSelectionIndex()).getStage();

        int year = crestHistoryData.getCrestDataArray().get(
                crestHistoryList.getSelectionIndex()).getYear();

        crestHistCanvas.selectCrestData(stage, year);
        
        // sets the selected crest
        setSelectedCrest(crestHistoryData.getCrestDataArray().get(
                crestHistoryList.getSelectionIndex()));
    }

    /**
     * Checks the foreign key constraints for the record.
     * 
     * @return True if the foreign key constraints are met.
     */
    private boolean checkFKConstraintsMet() {
        boolean rval = false;

        // Lid must exist in riverStat table
        String query = "Select lid FROM riverstat WHERE lid='" + lid + "'";

        try {
            QueryResult data = HydroDBDataManager.getInstance().runMappedQuery(
                    query);

            if (data.getResultCount() > 0) {
                rval = true;
            } else {
                MessageBox mb = new MessageBox(shell, SWT.ICON_ERROR | SWT.OK);
                mb.setText("Unable to Save");
                mb
                        .setMessage("Data for the location must be added via the River Gauge dialog first.");
                mb.open();
            }
        } catch (VizException e) {
            // don't care, just return false
            e.printStackTrace();
        }

        return rval;
    }

    /**
     * Verify validity of input
     * 
     * @param field
     */
    private boolean verifyDouble(Text field) {
        // verify input parameters in text fields.
        try {
            df.parse(field.getText()).doubleValue();
        } catch (ParseException pe) {
            // fire a dialog here
            error("Invalid Stage Format, numbers or decimals required", field);
            return false;
        }
        return true;
    }

    /**
     * Verify validity of input
     * 
     * @param field
     */
    private boolean verifyInt(Text field) {
        // verify input parameters in text fields.
        try {
            df.parse(field.getText()).intValue();
        } catch (ParseException pe) {
            // fire a dialog here
            error("Invalid Flow Format, whole numbers required", field);
            return false;
        }
        return true;
    }

    /**
     * Verify validity of input
     * 
     * @param field
     */
    private boolean verifyTime(Text field) {
        // verify input parameters in time field.
        try {
            if (field.getText().equalsIgnoreCase("MSG")
                    || field.getText().equalsIgnoreCase("UNDEF")) {
                return true;
            }

            stf.parse(field.getText());
        } catch (ParseException pe) {
            // fire a dialog here
            error("Invalid Time Format, required: hh:mm", field);
            return false;
        }
        return true;
    }

    /**
     * Verify validity of input
     * 
     * @param field
     */
    private boolean verifyDate(Text field) {
        // verify input parameters in date field.
        try {
            sdf.parse(field.getText());
        } catch (ParseException pe) {
            // fire a dialog here
            error("Invalid Date Format, required: mm/dd/yyyy", field);
            return false;
        }
        return true;
    }

    /**
     * clears data fields
     */
    private void clearData() {

        stageTF.setText("");
        flowTF.setText("");
        timeTF.setText("");
        dateTF.setText("");

        oldDatumChk.setSelection(false);
        highWaterMarkChk.setSelection(false);
        suppressChk.setSelection(false);
        iceJamChk.setSelection(false);

        statusCbo.select(3);
        remarksTF.setText("");
        // nothing selected
        if (function == 2) {
            crestHistoryList.setSelection(-1);
        }
    }

    /**
     * Get the internal index for this Crest
     * 
     * @param cd
     * @return
     */
    private int findIndex(CrestData cd) {
        int i = 0;
        if (crestHistoryData.getCrestDataArray().contains(cd)) {
            for (CrestData cd2 : crestHistoryData.getCrestDataArray()) {
                if (cd.equals(cd2)) {
                    break;
                } else {
                    i++;
                }
            }
        }
        return i;
    }

    /**
     * Checks the string to see if it is blank.
     * 
     * @param value
     *            The String to check
     * @return True if value is blank
     */
    private boolean isBlank(String value) {
        boolean isBlank = false;
        if (value.trim().length() == 0) {
            isBlank = true;
        }

        return isBlank;
    }

    /**
     * Error message box
     * 
     * @param message
     */
    private void error(String message, Text field) {

        MessageBox mb = new MessageBox(shell, SWT.ICON_QUESTION | SWT.OK
                | SWT.CANCEL);
        mb.setText("Failed Validation");
        mb.setMessage(message);

        mb.open();
        if (field != null) {
            field.setFocus();
        }
    }
}
