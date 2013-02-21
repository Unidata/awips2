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

package com.raytheon.viz.hydro.questionabledata;

import java.util.ArrayList;

import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.FocusEvent;
import org.eclipse.swt.events.FocusListener;
import org.eclipse.swt.events.KeyEvent;
import org.eclipse.swt.events.KeyListener;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.events.SelectionListener;
import org.eclipse.swt.graphics.Font;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Combo;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Layout;
import org.eclipse.swt.widgets.List;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Spinner;
import org.eclipse.swt.widgets.Text;

import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.viz.hydro.timeseries.TimeSeriesDlg;
import com.raytheon.viz.ui.dialogs.CaveSWTDialog;

/**
 * This class displays the Questionable and Bad Data dialog for Hydroview.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 29 NOV 2007  373        lvenable    Initial creation
 * 09 sep 2010  5399	   lbousaidi   changed constructor for both
 * 							 		   openTabularTimeSeries and openGraphTimeSeries
 * 05 Feb 2013  1578       rferrel     Changes for non-blocking singleton TimeSeriesDlg.
 *                                     Changes for non-blocking dialog.
 * 
 * </pre>
 * 
 * @author lvenable
 * @version 1.0
 * 
 */
public class QuestionableBadDataDlg extends CaveSWTDialog {

    /**
     * Font used for list controls.
     */
    private Font font;

    /**
     * Location check box.
     */
    private Button locationChk;

    /**
     * Location text control.
     */
    private Text locationTF;

    /**
     * Physical element combo box.
     */
    private Combo phsicalElemCbo;

    /**
     * Spinner indicating how many days back to display data.
     */
    private Spinner daysBackSpnr;

    /**
     * Sort by location radio button.
     */
    private Button locationRdo;

    /**
     * Sort by time radio button.
     */
    private Button timeRdo;

    /**
     * Sort by SHEF quality radio button.
     */
    private Button shefQualityRdo;

    /**
     * Sort by quality code radio button.
     */
    private Button qualityCodeRdo;

    /**
     * List control displaying the bad data.
     */
    private List dataList;

    /**
     * QC description text control.
     */
    private Text qcDescriptionTF;

    /**
     * Collection of questionable data.
     */
    private java.util.List<QuestionableData> questionableData;

    /**
     * Collection of filtered questionable data.
     */
    private java.util.List<QuestionableData> filteredQuestionableData;

    /**
     * Listener for sort options
     */
    private SelectionListener sortByListener;

    /**
     * Constructor.
     * 
     * @param parent
     *            Parent shell.
     */
    public QuestionableBadDataDlg(Shell parent) {
        super(parent, SWT.DIALOG_TRIM, CAVE.DO_NOT_BLOCK);
        setText("Questionable and Bad Data");
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.viz.ui.dialogs.CaveSWTDialogBase#constructShellLayout()
     */
    @Override
    protected Layout constructShellLayout() {
        // Create the main layout for the shell.
        GridLayout mainLayout = new GridLayout(1, true);
        mainLayout.marginHeight = 1;
        mainLayout.marginWidth = 1;
        return mainLayout;
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.viz.ui.dialogs.CaveSWTDialogBase#disposed()
     */
    @Override
    protected void disposed() {
        font.dispose();
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

        font = new Font(shell.getDisplay(), "Monospace", 11, SWT.NORMAL);

        // Initialize all of the controls and layouts
        sortByListener = new SelectionListener() {
            @Override
            public void widgetDefaultSelected(SelectionEvent e) {
            }

            @Override
            public void widgetSelected(SelectionEvent e) {
                getData();
            }
        };

        createTopControls();
        createListLabels();
        createDataListControl();
        createDescriptionControl();
        createBottomButtons();
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.viz.ui.dialogs.CaveSWTDialog#preOpened()
     */
    @Override
    protected void preOpened() {
        super.preOpened();
        getData();
    }

    /**
     * Create the Filter and Sort By controls located at the top of the dialog.
     */
    private void createTopControls() {
        Composite topComp = new Composite(shell, SWT.NONE);
        GridLayout topGl = new GridLayout(12, false);
        topComp.setLayout(topGl);

        Label locationLbl = new Label(topComp, SWT.NONE);
        locationLbl.setText("Filter By:");

        locationChk = new Button(topComp, SWT.CHECK);
        locationChk.setText("Location");
        locationChk.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                locationTF.setEnabled(locationChk.getSelection());
                updateDisplayList();
            }
        });

        GridData gd = new GridData(80, SWT.DEFAULT);
        locationTF = new Text(topComp, SWT.BORDER);
        locationTF.setLayoutData(gd);
        locationTF.setEnabled(false);
        locationTF.addKeyListener(new KeyListener() {
            @Override
            public void keyPressed(KeyEvent e) {
            }

            @Override
            public void keyReleased(KeyEvent e) {
                filterDisplay();
            }
        });

        phsicalElemCbo = new Combo(topComp, SWT.DROP_DOWN | SWT.READ_ONLY);
        phsicalElemCbo.addSelectionListener(new SelectionListener() {
            @Override
            public void widgetDefaultSelected(SelectionEvent e) {
            }

            @Override
            public void widgetSelected(SelectionEvent e) {
                getData();
            }
        });
        fillPhysicalElementCbo();
        phsicalElemCbo.select(6);

        gd = new GridData(60, SWT.DEFAULT);
        Label lastLbl = new Label(topComp, SWT.RIGHT);
        lastLbl.setText("Last");
        lastLbl.setLayoutData(gd);

        gd = new GridData(35, SWT.DEFAULT);
        daysBackSpnr = new Spinner(topComp, SWT.BORDER);
        daysBackSpnr.setDigits(0);
        daysBackSpnr.setIncrement(1);
        daysBackSpnr.setPageIncrement(5);
        daysBackSpnr.setSelection(1);
        daysBackSpnr.setMinimum(0);
        daysBackSpnr.setLayoutData(gd);
        daysBackSpnr.addFocusListener(new FocusListener() {
            @Override
            public void focusGained(FocusEvent e) {
            }

            @Override
            public void focusLost(FocusEvent e) {
                getData();
            }
        });

        gd = new GridData(60, SWT.DEFAULT);
        Label daysLbl = new Label(topComp, SWT.NONE);
        daysLbl.setText("days");
        daysLbl.setLayoutData(gd);

        Label sortByLbl = new Label(topComp, SWT.RIGHT);
        sortByLbl.setText("Sort By:");

        gd = new GridData(90, SWT.DEFAULT);
        locationRdo = new Button(topComp, SWT.RADIO);
        locationRdo.setText("Location");
        locationRdo.setLayoutData(gd);
        locationRdo.addSelectionListener(sortByListener);

        gd = new GridData(70, SWT.DEFAULT);
        timeRdo = new Button(topComp, SWT.RADIO);
        timeRdo.setText("Time");
        timeRdo.setLayoutData(gd);
        timeRdo.setSelection(true);
        timeRdo.addSelectionListener(sortByListener);

        gd = new GridData(120, SWT.DEFAULT);
        shefQualityRdo = new Button(topComp, SWT.RADIO);
        shefQualityRdo.setText("Shef Quality");
        shefQualityRdo.setLayoutData(gd);
        shefQualityRdo.addSelectionListener(sortByListener);

        gd = new GridData(120, SWT.DEFAULT);
        qualityCodeRdo = new Button(topComp, SWT.RADIO);
        qualityCodeRdo.setText("Quality Code");
        qualityCodeRdo.setLayoutData(gd);
        qualityCodeRdo.addSelectionListener(sortByListener);
    }

    /**
     * Create the label displayed above the "questionable and bad" data list.
     */
    private void createListLabels() {
        Composite labelComp = new Composite(shell, SWT.NONE);
        GridLayout labelGl = new GridLayout(1, false);
        labelComp.setLayout(labelGl);

        String fmt = "%s %s                 %s   %s %s %s    %s "
                + "%s   %s  %s  %s   %s       %s        %s";

        String text = String.format(fmt, "Location", "Name", "PE", "Dur", "TS",
                "Ext", "Value", "Observation Time", "RV", "SQ", "QC",
                "Product", "Time", "Posted");

        GridData gd = new GridData();
        gd.horizontalIndent = 0;
        Label listLbl = new Label(labelComp, SWT.NONE);
        listLbl.setText(text);
        listLbl.setFont(font);
        listLbl.setLayoutData(gd);
    }

    /**
     * Create the "questionable and bad" data list control.
     */
    private void createDataListControl() {
        GridData gd = new GridData(SWT.CENTER, SWT.DEFAULT, false, false);
        gd.widthHint = 1150;
        gd.heightHint = 390;
        dataList = new List(shell, SWT.BORDER | SWT.MULTI | SWT.V_SCROLL);
        dataList.setLayoutData(gd);
        dataList.addSelectionListener(new SelectionListener() {
            @Override
            public void widgetDefaultSelected(SelectionEvent e) {
            }

            @Override
            public void widgetSelected(SelectionEvent e) {
                updateDescription();
            }
        });

        dataList.setFont(font);
    }

    /**
     * Create the QC description label and text control.
     */
    private void createDescriptionControl() {
        GridData mainGD = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        Composite qcComp = new Composite(shell, SWT.NONE);
        GridLayout gl = new GridLayout(2, false);
        qcComp.setLayout(gl);
        qcComp.setLayoutData(mainGD);

        Label qcLbl = new Label(qcComp, SWT.NONE);
        qcLbl.setText("QC Description");

        GridData gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        qcDescriptionTF = new Text(qcComp, SWT.BORDER);
        qcDescriptionTF.setEditable(false);
        qcDescriptionTF.setLayoutData(gd);
    }

    /**
     * Create the button at the bottom of the dialog.
     */
    private void createBottomButtons() {
        GridData mainGD = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        Composite buttonComp = new Composite(shell, SWT.NONE);
        GridLayout buttonGl = new GridLayout(5, false);
        buttonGl.horizontalSpacing = 10;
        buttonComp.setLayout(buttonGl);
        buttonComp.setLayoutData(mainGD);

        GridData gd = new GridData(160, SWT.DEFAULT);
        Button tabularBtn = new Button(buttonComp, SWT.PUSH);
        tabularBtn.setText("Tabular Time Series");
        tabularBtn.setLayoutData(gd);
        tabularBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                openTabularTimeSeries();
            }
        });

        gd = new GridData(160, SWT.DEFAULT);
        Button graphTimeBtn = new Button(buttonComp, SWT.PUSH);
        graphTimeBtn.setText("Graph Time Series");
        graphTimeBtn.setLayoutData(gd);
        graphTimeBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                openGraphTimeSeries();
            }
        });

        gd = new GridData(160, SWT.DEFAULT);
        Button setMissingBtn = new Button(buttonComp, SWT.PUSH);
        setMissingBtn.setText("Set Missing");
        setMissingBtn.setLayoutData(gd);
        setMissingBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                setMissing();
            }
        });

        gd = new GridData(160, SWT.DEFAULT);
        Button deleteSelectedBtn = new Button(buttonComp, SWT.PUSH);
        deleteSelectedBtn.setText("Delete Selected");
        deleteSelectedBtn.setLayoutData(gd);
        deleteSelectedBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                deleteRecords();
            }
        });

        gd = new GridData(SWT.END, SWT.DEFAULT, true, false);
        gd.widthHint = 100;
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

    /**
     * Fill the physical element combo box.
     */
    private void fillPhysicalElementCbo() {
        phsicalElemCbo.add("Agriculture");
        phsicalElemCbo.add("Discharge");
        phsicalElemCbo.add("Evaporation");
        phsicalElemCbo.add("FishCount");
        phsicalElemCbo.add("Gate Dam");
        phsicalElemCbo.add("Ground");
        phsicalElemCbo.add("Height (Stage)");
        phsicalElemCbo.add("Ice");
        phsicalElemCbo.add("Lake");
        phsicalElemCbo.add("Moisture");
        phsicalElemCbo.add("Power");
        phsicalElemCbo.add("Precipitation (PC)");
        phsicalElemCbo.add("Precipitation (PP)");
        phsicalElemCbo.add("Precipitation (Other)");
        phsicalElemCbo.add("Pressure");
        phsicalElemCbo.add("Radiation");
        phsicalElemCbo.add("Snow");
        phsicalElemCbo.add("Temperature");
        phsicalElemCbo.add("WaterQuality");
        phsicalElemCbo.add("Weather");
        phsicalElemCbo.add("Wind");
        phsicalElemCbo.add("YUnique");
    }

    /**
     * Retrieves the sorted data from the database
     */
    private void getData() {
        if (questionableData == null) {
            questionableData = new ArrayList<QuestionableData>();
        } else {
            questionableData.clear();
        }

        String sortCriteria = "";
        if (timeRdo.getSelection()) {
            sortCriteria = timeRdo.getText();
        } else if (locationRdo.getSelection()) {
            sortCriteria = locationRdo.getText();
        } else if (shefQualityRdo.getSelection()) {
            sortCriteria = shefQualityRdo.getText();
        } else if (qualityCodeRdo.getSelection()) {
            sortCriteria = qualityCodeRdo.getText();
        }

        // Pass table, days, sort order
        questionableData = QuestionableDataManager.getInstance()
                .getQuestionableData(
                        phsicalElemCbo.getItem(phsicalElemCbo
                                .getSelectionIndex()),
                        daysBackSpnr.getSelection(), sortCriteria);

        filterDisplay();
    }

    /**
     * Handles the Station Search functionality
     */
    private void filterDisplay() {
        String stationSearch = locationTF.getText().toUpperCase();

        if (filteredQuestionableData == null)
            filteredQuestionableData = new ArrayList<QuestionableData>();
        else
            filteredQuestionableData.clear();

        for (QuestionableData unfilteredLID : questionableData) {
            if (unfilteredLID.getLid().contains(stationSearch)) {
                filteredQuestionableData.add(unfilteredLID);
            }
        }

        updateDisplayList();
    }

    /**
     * Refreshes the records displayed
     */
    private void updateDisplayList() {
        dataList.removeAll();

        java.util.List<QuestionableData> dataToDisplay = (locationChk
                .getSelection() && (filteredQuestionableData != null)) ? filteredQuestionableData
                : questionableData;

        if (dataToDisplay.size() == 0) {
            StringBuffer showErrorMsg = new StringBuffer();
            showErrorMsg
                    .append("No Questionable or Bad data found within the past ");
            showErrorMsg.append(daysBackSpnr.getSelection());
            showErrorMsg.append(" days in table ");
            showErrorMsg.append(phsicalElemCbo.getItem(phsicalElemCbo
                    .getSelectionIndex()));

            if (locationChk.getSelection()) {
                showErrorMsg.append(" for location ");
                showErrorMsg.append(locationTF.getText());
            }

            dataList.add(showErrorMsg.toString());

            qcDescriptionTF.setText("");
        } else {
            for (QuestionableData currData : dataToDisplay) {
                dataList.add(currData.toString());
            }
        }
    }

    /**
     * Updated the QC description for the selected data.
     */
    private void updateDescription() {
        QuestionableData selectedData = getCurrentlySelectedData();

        if (selectedData != null) {
            qcDescriptionTF.setText(QuestionableDataManager.getInstance()
                    .getDescription(selectedData));
        }
    }

    /**
     * Remove the selected data records.
     */
    private void deleteRecords() {
        java.util.List<QuestionableData> recordsToDelete = getCurrentlySelectedRange();

        try {
            QuestionableDataManager.getInstance().deleteRecords(
                    recordsToDelete,
                    phsicalElemCbo.getItem(phsicalElemCbo.getSelectionIndex()));
        } catch (VizException e) {
            MessageDialog.openConfirm(null, "Unable to delete records.",
                    "Unable to delete records.");
        }

        getData();
    }

    /**
     * Display the time series graph for the selected record.
     */
    private void openGraphTimeSeries() {
        QuestionableData currData = getCurrentlySelectedData();

        if (currData != null) {
            shell.setCursor(shell.getDisplay().getSystemCursor(SWT.CURSOR_WAIT));
            TimeSeriesDlg.getInstance().updateAndOpen(currData.getLid(),
                    currData.getPe(), currData.getTs(), true);
            shell.setCursor(null);
        }
    }

    /**
     * Open time series tabular information for the selected record.
     */
    private void openTabularTimeSeries() {
        QuestionableData currData = getCurrentlySelectedData();

        if (currData != null) {
            shell.setCursor(shell.getDisplay().getSystemCursor(SWT.CURSOR_WAIT));
            TimeSeriesDlg.getInstance().updateAndOpen(currData.getLid(),
                    currData.getPe(), currData.getTs(), false);
            shell.setCursor(null);
        }
    }

    /**
     * The the data record for the selected line.
     * 
     * @return questionableData
     */
    private QuestionableData getCurrentlySelectedData() {
        QuestionableData rval = null;

        int index = dataList.getSelectionIndex();

        if (index >= 0) {
            if (locationChk.getSelection()
                    && (filteredQuestionableData.size() > 0)) {
                rval = filteredQuestionableData.get(index);
            } else if (questionableData.size() > 0) {
                rval = questionableData.get(index);
            }
        }

        return rval;
    }

    /**
     * Get a list of records for the selected line(s).
     * 
     * @return list
     */
    private java.util.List<QuestionableData> getCurrentlySelectedRange() {
        ArrayList<QuestionableData> rval = new ArrayList<QuestionableData>();

        for (int i : dataList.getSelectionIndices()) {
            rval.add(locationChk.getSelection() ? filteredQuestionableData
                    .get(i) : questionableData.get(i));
        }

        return rval;
    }

    private void setMissing() {
        java.util.List<QuestionableData> recordsToSetMissing = getCurrentlySelectedRange();

        try {
            QuestionableDataManager.getInstance().setMissing(
                    recordsToSetMissing);
        } catch (VizException e) {
            // TODO Auto-generated catch block
            e.printStackTrace();
        }

        getData();
    }
}
