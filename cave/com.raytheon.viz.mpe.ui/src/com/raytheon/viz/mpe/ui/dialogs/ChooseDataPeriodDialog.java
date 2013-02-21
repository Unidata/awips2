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
package com.raytheon.viz.mpe.ui.dialogs;

import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Calendar;
import java.util.Date;
import java.util.Map;
import java.util.TimeZone;

import org.eclipse.jface.dialogs.IDialogConstants;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Combo;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Spinner;
import org.eclipse.swt.widgets.Text;

import com.raytheon.uf.common.ohd.AppsDefaults;
import com.raytheon.uf.viz.core.IDisplayPane;
import com.raytheon.uf.viz.core.IDisplayPaneContainer;
import com.raytheon.viz.mpe.core.MPEDataManager;
import com.raytheon.viz.mpe.core.MPEDataManager.MPEDateInfo;
import com.raytheon.viz.mpe.ui.MPEDisplayManager;
import com.raytheon.viz.ui.EditorUtil;
import com.raytheon.viz.ui.dialogs.CaveJFACEDialog;
import com.raytheon.viz.ui.editor.IMultiPaneEditor;

/**
 * MPE Dialog for choosing time data should be displaying at
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Sep 23, 2008            randerso     Initial creation
 * </pre>
 * 
 * @author randerso
 * @version 1.0
 */

public class ChooseDataPeriodDialog extends CaveJFACEDialog {

    private static final SimpleDateFormat sdf;
    static {
        sdf = new SimpleDateFormat("yyyy-MM-dd HH:mm:ss");
        sdf.setTimeZone(TimeZone.getTimeZone("GMT"));
    }

    private Calendar cal;

    public static Date prevDate;

    public static String prevArea;

    private Text yearText;

    private Text monthText;

    private Spinner daySpinner;

    private Spinner hourSpinner;

    private Spinner daysSpinner;

    private Map<Date, MPEDateInfo> dateMap;

    private Label lastExec;

    private Label manuallySaved;

    private Label lastSave;

    private MPEDataManager dataMgr;

    private MPEDisplayManager displayMgr;

    private Shell shell;

    private boolean qcEnable = false;

    private String site_ids = null;

    private String area_names = null;

    private AppsDefaults appsDefaults = AppsDefaults.getInstance();

    private Combo areaCombo;

    public ChooseDataPeriodDialog(Shell parentShell) {
        super(parentShell);
        setBlockOnOpen(false);
        shell = parentShell;

        IDisplayPaneContainer container = EditorUtil.getActiveVizContainer();
        IDisplayPane pane = null;
        if (container != null) {
            if (container instanceof IMultiPaneEditor) {
                IMultiPaneEditor multiPane = (IMultiPaneEditor) container;
                if (multiPane.getNumberofPanes() > 1
                        && multiPane.displayedPaneCount() > 1) {
                    pane = multiPane
                            .getSelectedPane(IMultiPaneEditor.LOAD_ACTION);
                }
            }
            if (pane == null) {
                pane = container.getDisplayPanes()[0];
            }
        }

        displayMgr = MPEDisplayManager.getInstance(pane);
        dataMgr = MPEDataManager.getInstance();
        dateMap = dataMgr.getDateMap(false);
        qcEnable = MPEDisplayManager.isMpeQcOptionEnabled();
        cal = Calendar.getInstance(TimeZone.getTimeZone("GMT"));
        prevDate = displayMgr.getCurrentEditDate();
        cal.setTime(prevDate);
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.ui.dialogs.CaveJFACEDialog#createDialogArea(org.eclipse
     * .swt.widgets.Composite)
     */
    @Override
    protected Control createDialogArea(Composite parent) {
        getShell().setText("Choose Data Period");

        Composite composite = (Composite) super.createDialogArea(parent);
        GridLayout layout = (GridLayout) composite.getLayout();
        layout.numColumns = 1;

        // create date area
        Group dateComp = new Group(composite, SWT.SHADOW_ETCHED_IN);
        GridData data = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        dateComp.setLayoutData(data);
        layout = new GridLayout(3, false);
        dateComp.setLayout(layout);
        dateComp.setText("Date");

        Label yearLabel = new Label(dateComp, SWT.NONE);
        yearLabel.setText("Ending Year");

        Label monthLabel = new Label(dateComp, SWT.NONE);
        monthLabel.setText("Month");

        Label dayLabel = new Label(dateComp, SWT.NONE);
        dayLabel.setText("Day");

        yearText = new Text(dateComp, SWT.BORDER | SWT.READ_ONLY);
        data = new GridData(SWT.DEFAULT, SWT.DEFAULT, false, false);
        data.widthHint = 100;
        yearText.setLayoutData(data);

        monthText = new Text(dateComp, SWT.BORDER | SWT.READ_ONLY);
        data = new GridData(SWT.DEFAULT, SWT.DEFAULT, false, false);
        data.widthHint = 50;
        monthText.setLayoutData(data);

        daySpinner = new Spinner(dateComp, SWT.BORDER | SWT.READ_ONLY);
        data = new GridData(SWT.DEFAULT, SWT.DEFAULT, false, false);
        data.widthHint = 50;
        daySpinner.setLayoutData(data);
        daySpinner.setMinimum(0);
        daySpinner.setMaximum(32);
        daySpinner.addSelectionListener(new SelectionAdapter() {

            @Override
            public void widgetSelected(SelectionEvent e) {
                int day = daySpinner.getSelection();

                cal.set(Calendar.DAY_OF_MONTH, day);

                updateTimeControls();
            }

        });

        // create MPE Options area
        Group mpeOptionsGroup = new Group(composite, SWT.SHADOW_ETCHED_IN);
        data = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        mpeOptionsGroup.setLayoutData(data);
        layout = new GridLayout(2, false);
        layout.horizontalSpacing *= 2;
        mpeOptionsGroup.setLayout(layout);
        mpeOptionsGroup.setText("MPE Options");

        Composite hourComp = new Composite(mpeOptionsGroup, SWT.NONE);
        data = new GridData(SWT.DEFAULT, SWT.CENTER, false, true);
        hourComp.setLayoutData(data);
        layout = new GridLayout(2, false);
        layout.marginWidth = 0;
        layout.marginHeight = 0;
        hourComp.setLayout(layout);

        Label hourLabel = new Label(hourComp, SWT.NONE);
        hourLabel.setText("Hour");

        hourSpinner = new Spinner(hourComp, SWT.BORDER | SWT.READ_ONLY);
        data = new GridData(SWT.DEFAULT, SWT.DEFAULT, false, false);
        data.widthHint = 50;
        hourSpinner.setLayoutData(data);
        hourSpinner.setMinimum(-1); // this works in eclipse 3.4+
        hourSpinner.setMaximum(24);
        hourSpinner.addSelectionListener(new SelectionAdapter() {

            @Override
            public void widgetSelected(SelectionEvent e) {
                int hour = hourSpinner.getSelection();

                cal.set(Calendar.HOUR_OF_DAY, hour);

                updateTimeControls();
            }

        });

        Composite statusComp = new Composite(mpeOptionsGroup, SWT.NONE);
        data = new GridData(SWT.DEFAULT, SWT.DEFAULT, false, false);
        statusComp.setLayoutData(data);
        layout = new GridLayout(2, false);
        layout.marginWidth = 0;
        layout.marginHeight = 0;
        statusComp.setLayout(layout);

        Label lab1 = new Label(statusComp, SWT.NONE);
        lab1.setText("Last Save:");

        lastSave = new Label(statusComp, SWT.NONE);
        lastSave.setText("NA");
        data = new GridData(SWT.DEFAULT, SWT.DEFAULT, false, false);
        data.widthHint = 120;
        lastSave.setLayoutData(data);

        Label lab2 = new Label(statusComp, SWT.NONE);
        lab2.setText("Last Exec:");

        lastExec = new Label(statusComp, SWT.NONE);
        lastExec.setText("NA");
        data = new GridData(SWT.DEFAULT, SWT.DEFAULT, false, false);
        data.widthHint = 120;
        lastExec.setLayoutData(data);

        Label lab3 = new Label(statusComp, SWT.NONE);
        lab3.setText("Manually Saved:");

        manuallySaved = new Label(statusComp, SWT.NONE);
        manuallySaved.setText("NA");
        data = new GridData(SWT.DEFAULT, SWT.DEFAULT, false, false);
        data.widthHint = 120;
        manuallySaved.setLayoutData(data);

        Button displayMPEData = new Button(mpeOptionsGroup, SWT.PUSH);
        displayMPEData.setText("Display MPE Data");
        data = new GridData(SWT.CENTER, SWT.DEFAULT, false, false);
        data.horizontalSpan = 2;
        displayMPEData.setLayoutData(data);
        displayMPEData.addSelectionListener(new SelectionAdapter() {

            @Override
            public void widgetSelected(SelectionEvent e) {
                // Hide so if setCurrentEditDate returns false, we can continue
                // to display
                hide();
                if (displayMgr.setCurrentEditDate(getTime())) {
                    close();
                } else {
                    restore();
                }
            }
        });

        // create gage edit options area
        Group gageOptionsGroup = new Group(composite, SWT.SHADOW_ETCHED_IN);
        data = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        gageOptionsGroup.setLayoutData(data);
        layout = new GridLayout(2, false);
        layout.horizontalSpacing *= 2;
        gageOptionsGroup.setLayout(layout);
        gageOptionsGroup.setText("6/24 hr gage edit options");

        new Label(gageOptionsGroup, SWT.NONE);

        Label selectAreaLabel = new Label(gageOptionsGroup, SWT.NONE);
        selectAreaLabel.setText("Select Area");
        data = new GridData(SWT.CENTER, SWT.DEFAULT, false, false);
        selectAreaLabel.setLayoutData(data);

        Composite daysComp = new Composite(gageOptionsGroup, SWT.NONE);
        data = new GridData(SWT.DEFAULT, SWT.CENTER, false, true);
        daysComp.setLayoutData(data);
        layout = new GridLayout(2, false);
        layout.marginWidth = 0;
        layout.marginHeight = 0;
        daysComp.setLayout(layout);

        Label daysLabel = new Label(daysComp, SWT.NONE);
        daysLabel.setText("Num of Days");

        daysSpinner = new Spinner(daysComp, SWT.BORDER | SWT.READ_ONLY);
        data = new GridData(SWT.DEFAULT, SWT.DEFAULT, false, false);
        data.widthHint = 50;
        daysSpinner.setLayoutData(data);
        daysSpinner.setMinimum(1);
        daysSpinner.setMaximum(10);
        daysSpinner.setSelection(displayMgr.getDqcDays());
        daysSpinner.setEnabled(qcEnable);

        areaCombo = new Combo(gageOptionsGroup, SWT.DROP_DOWN | SWT.READ_ONLY);
        if (qcEnable) {
            buildAreaList();
            areaCombo.setText(areaCombo.getItem(0).toString());
        } else {
            areaCombo.setText("");
        }
        data = new GridData(SWT.DEFAULT, SWT.DEFAULT, false, false);
        data.widthHint = 150;
        areaCombo.setLayoutData(data);
        areaCombo.setEnabled(qcEnable);
        areaCombo.addSelectionListener(new SelectionAdapter() {

        });

        Composite buttonComp = new Composite(gageOptionsGroup, SWT.NONE);
        data = new GridData(SWT.CENTER, SWT.DEFAULT, true, false);
        data.horizontalSpan = 2;
        buttonComp.setLayoutData(data);
        layout = new GridLayout(3, false);
        layout.marginWidth = 0;
        layout.marginHeight = 0;
        buttonComp.setLayout(layout);

        Button precipBtn = new Button(buttonComp, SWT.PUSH);
        precipBtn.setText("Precipitation");
        precipBtn.setEnabled(qcEnable);
        precipBtn.addSelectionListener(new SelectionAdapter() {

            @Override
            public void widgetSelected(SelectionEvent e) {
                displayMgr.setDqcDays(daysSpinner.getSelection());
                prevArea = areaCombo.getItem(areaCombo.getSelectionIndex());
                if (QcPrecipOptionsDialog.isFinished() == false) {
                    QcPrecipOptionsDialog.destroy(false);
                }

                Display display = shell.getDisplay();
                final QcPrecipOptionsDialog dialog = new QcPrecipOptionsDialog(
                        getParentShell().getShell());
                display.asyncExec(new Runnable() {
                    public void run() {
                        dialog.open();
                    }
                });
                close();
            }

        });

        Button tempBtn = new Button(buttonComp, SWT.PUSH);
        tempBtn.setText("Temperature");
        tempBtn.setEnabled(qcEnable);
        tempBtn.addSelectionListener(new SelectionAdapter() {

            @Override
            public void widgetSelected(SelectionEvent e) {
                displayMgr.setDqcDays(daysSpinner.getSelection());
                prevArea = areaCombo.getItem(areaCombo.getSelectionIndex());
                if (QcTempOptionsDialog.isFinished() == false) {
                    QcTempOptionsDialog.destroy(false);
                }

                Display display = shell.getDisplay();
                final QcTempOptionsDialog dialog = new QcTempOptionsDialog(
                        getParentShell().getShell());
                display.asyncExec(new Runnable() {
                    public void run() {
                        dialog.open();
                    }
                });
                close();
            }

        });

        Button freezeBtn = new Button(buttonComp, SWT.PUSH);
        freezeBtn.setText("Freezing");
        freezeBtn.setEnabled(qcEnable);
        freezeBtn.addSelectionListener(new SelectionAdapter() {

            @Override
            public void widgetSelected(SelectionEvent e) {
                displayMgr.setDqcDays(daysSpinner.getSelection());
                prevArea = areaCombo.getItem(areaCombo.getSelectionIndex());
                if (QcFreezeOptionsDialog.isFinished() == false) {
                    QcFreezeOptionsDialog.destroy(false);
                }

                Display display = shell.getDisplay();
                final QcFreezeOptionsDialog dialog = new QcFreezeOptionsDialog(
                        getParentShell().getShell());
                display.asyncExec(new Runnable() {
                    public void run() {
                        dialog.open();
                    }
                });
                close();
            }

        });

        updateTimeControls();

        return composite;
    }

    private void updateTimeControls() {
        if (cal.getTime().before(dataMgr.getEarliestDate())
                || cal.getTime().after(dataMgr.getLatestDate())) {
            cal.setTime(prevDate);
        }
        prevDate = cal.getTime();

        yearText.setText(Integer.toString(cal.get(Calendar.YEAR)));
        monthText.setText(Integer.toString(cal.get(Calendar.MONTH) + 1));
        daySpinner.setSelection(cal.get(Calendar.DAY_OF_MONTH));

        hourSpinner.setSelection(cal.get(Calendar.HOUR_OF_DAY));

        if (dateMap.containsKey(cal.getTime()) == false) {
            dateMap = dataMgr.getDateMap(true);
        }
        MPEDateInfo dateInfo = dateMap.get(cal.getTime());
        if (dateInfo != null) {
            lastSave.setText(sdf.format(dateInfo.getLastSaveTime()));
            lastExec.setText(sdf.format(dateInfo.getLastExecTime()));
            if (dateInfo.isAutoSave()) {
                manuallySaved.setText("NO");
            } else {
                manuallySaved.setText("YES");
            }
        } else {
            lastSave.setText("NA");
            lastExec.setText("NA");
            manuallySaved.setText("NA");
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * org.eclipse.jface.dialogs.Dialog#createButtonsForButtonBar(org.eclipse
     * .swt.widgets.Composite)
     */
    @Override
    protected void createButtonsForButtonBar(Composite parent) {
        createButton(parent, IDialogConstants.OK_ID, "Close", true);
    }

    /**
     * Get the selected date/time as a java date.
     * 
     * @return
     */
    public Date getTime() {
        return cal.getTime();
    }

    /**
     * Get the selected year;
     * 
     * @return
     */
    public int getYear() {
        return cal.get(Calendar.YEAR);
    }

    /**
     * Get the selected month;
     * 
     * @return
     */
    public int getMonth() {
        return cal.get(Calendar.MONTH);
    }

    /**
     * Get the selected day of the month;
     * 
     * @return
     */
    public int getDay() {
        return cal.get(Calendar.DAY_OF_MONTH);
    }

    /**
     * Get the selected hour of the day;
     * 
     * @return
     */
    public int getHour() {
        return cal.get(Calendar.HOUR_OF_DAY);
    }

    /**
     * Get number of days
     * 
     * @return
     */
    public int getNumdays() {
        return daysSpinner.getSelection();
    }

    private void buildAreaList() {
        site_ids = appsDefaults.getToken("mpe_site_id");
        area_names = appsDefaults.getToken("mpe_area_names");
        ArrayList<String> aList = new ArrayList<String>();
        site_ids = site_ids.trim();

        if (area_names != null) {
            if (area_names.lastIndexOf(',') != -1) {
                for (int i = 0; i < area_names.length();) {
                    int j = area_names.indexOf(',', i);
                    if (j == -1) {
                        j = area_names.length();
                    }
                    aList.add(area_names.substring(i, j));
                    i += j + 1;
                }
            } else {
                aList.add(0, area_names.trim());
            }
        } else {
            aList.add(0, site_ids);
        }
        areaCombo.setItems(aList.toArray(new String[aList.size()]));
    }
}
