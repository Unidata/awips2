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
import java.util.List;
import java.util.Map;
import java.util.TimeZone;

import org.eclipse.jface.dialogs.IDialogConstants;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.graphics.GC;
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
import com.raytheon.viz.mpe.ui.TransmitBestEstimateQPEProvider;
import com.raytheon.viz.mpe.ui.TransmitRFCBiasProvider;
import com.raytheon.viz.ui.EditorUtil;
import com.raytheon.viz.ui.dialogs.CaveJFACEDialog;
import com.raytheon.viz.ui.editor.IMultiPaneEditor;

/**
 * MPE Dialog for choosing time data should be displaying at
 *
 * <pre>
 * SOFTWARE HISTORY
 *
 * Date          Ticket#  Engineer   Description
 * ------------- -------- ---------- -------------------------------------------
 * Sep 23, 2008           randerso   Initial creation
 * Apr 30, 2013           lbousaidi  made seconds in the date/Time fields
 *                                   visible.
 * Aug 26, 2014  14578    snaples    Added Ending Hydrologic Date selection.
 * Nov 24, 2014  16911    xwei       The day of Hydrologic Date is set to the
 *                                   next day if hour is greater than 18Z.
 * Jan 05, 2015  14246    lbousaidi  enable Transmit Best Estimate QPE.
 * Jul 08, 2015  16790    snaples    Updated call to setCurrentEditDate to pass
 *                                   force variable.
 * Sep 29, 2015  17975    snaples    Fixed issue with Hydro date not following
 *                                   the CAVE time when changed.
 * Apr 11, 2016  5512     bkowal     Cleanup.
 * Dec 15, 2017  6547     bkowal     Initialize {@link #prevHydDate}.
 * Mar 09, 2018  7135     mduff      Apply actions to all applicable
 *                                   MPEDisplayManger instances.
 * May 10, 2018  7131     mduff      DQC Dialogs now in MPEDisplayManager.
 * Sep 25, 2018  7489     dgilling   Excise fixed pixel layouts.
 * Jul 10, 2019  7489     randerso   Set minimum width for lastSave and lastExec
 * Mar  9, 2020  8049     tgurney    Remove read-only option from spinners
 * Jan 06, 2021  8315     randerso   Fix initial selection in areaCombo
 *
 * </pre>
 *
 * @author randerso
 */

public class ChooseDataPeriodDialog extends CaveJFACEDialog {

    private static final SimpleDateFormat sdf;
    static {
        sdf = new SimpleDateFormat("yyyy-MM-dd HH:mm:ss");
        sdf.setTimeZone(TimeZone.getTimeZone("GMT"));
    }

    private Calendar cal;

    private Calendar hydroCal;

    protected static Date prevDate;

    protected static Date prevHydDate;

    public static String prevArea;

    private Text yearText;

    private Text monthText;

    private Spinner daySpinner;

    private Spinner hourSpinner;

    private Spinner daysSpinner;

    private Text hydyearText;

    private Text hydmonthText;

    private Spinner hyddaySpinner;

    private Map<Date, MPEDateInfo> dateMap;

    private Label lastExec;

    private Label manuallySaved;

    private Label lastSave;

    private MPEDataManager dataMgr;

    private List<MPEDisplayManager> displayMgrs = new ArrayList<>();

    private Shell shell;

    private boolean qcEnable = false;

    private AppsDefaults appsDefaults = AppsDefaults.getInstance();

    private Combo areaCombo;

    private static Date currentHydroEndingDate;

    public ChooseDataPeriodDialog(Shell parentShell) {
        super(parentShell);
        setBlockOnOpen(false);
        shell = parentShell;

        IDisplayPaneContainer container = EditorUtil.getActiveVizContainer();
        if (container != null) {
            if (container instanceof IMultiPaneEditor) {
                IMultiPaneEditor multiPane = (IMultiPaneEditor) container;
                IDisplayPane[] displayPanes = multiPane.getDisplayPanes();
                if (displayPanes != null) {
                    for (IDisplayPane idp : displayPanes) {
                        displayMgrs.add(MPEDisplayManager.getInstance(idp));
                    }
                }
            }
        } else {
            displayMgrs.add(MPEDisplayManager.getCurrent());
        }

        dataMgr = MPEDataManager.getInstance();
        dateMap = dataMgr.getDateMap(false);

        if (displayMgrs.get(0) != null) {
            MPEDisplayManager displayMgr = displayMgrs.get(0);
            if (MPEDisplayManager.isMpeQcOptionEnabled()
                    && (displayMgr.getQcFreezeDialog() == null
                            || !displayMgr.getQcFreezeDialog().isOpen())
                    && (displayMgr.getQcPrecipDialog() == null
                            || !displayMgr.getQcPrecipDialog().isOpen())
                    && (displayMgr.getQcTempDialog() == null
                            || !displayMgr.getQcTempDialog().isOpen())) {
                qcEnable = true;
            } else {
                qcEnable = false;
            }

        }

        cal = Calendar.getInstance(TimeZone.getTimeZone("GMT"));
        hydroCal = Calendar.getInstance(TimeZone.getTimeZone("GMT"));
        prevDate = displayMgrs.get(0).getCurrentEditDate();
        cal.setTime(prevDate);

        hydroCal.setTime(prevDate);

        if (hydroCal.get(Calendar.HOUR_OF_DAY) >= 18) {
            hydroCal.add(Calendar.DATE, 1);
        }
    }

    @Override
    protected Control createDialogArea(Composite parent) {
        Composite composite = (Composite) super.createDialogArea(parent);
        composite.setLayout(new GridLayout());

        // create date area
        Group dateComp = new Group(composite, SWT.SHADOW_ETCHED_IN);
        dateComp.setText("Date");
        dateComp.setLayout(new GridLayout(3, false));
        dateComp.setLayoutData(
                new GridData(SWT.FILL, SWT.DEFAULT, true, false));

        Label yearLabel = new Label(dateComp, SWT.NONE);
        yearLabel.setText("Ending Year");

        Label monthLabel = new Label(dateComp, SWT.NONE);
        monthLabel.setText("Month");

        Label dayLabel = new Label(dateComp, SWT.NONE);
        dayLabel.setText("Day");

        yearText = new Text(dateComp, SWT.BORDER | SWT.READ_ONLY);
        GC gc = new GC(yearText);
        gc.setFont(yearText.getFont());
        int longTextWidth = gc.textExtent("99999").x;
        gc.dispose();
        GridData data = new GridData(SWT.FILL, SWT.DEFAULT, false, false);
        data.widthHint = longTextWidth;
        yearText.setLayoutData(data);

        monthText = new Text(dateComp, SWT.BORDER | SWT.READ_ONLY);
        gc = new GC(monthText);
        gc.setFont(monthText.getFont());
        int shortTextWidth = gc.textExtent("999").x;
        gc.dispose();
        data = new GridData(SWT.FILL, SWT.DEFAULT, false, false);
        data.widthHint = shortTextWidth;
        monthText.setLayoutData(data);

        daySpinner = new Spinner(dateComp, SWT.BORDER);
        data = new GridData(SWT.DEFAULT, SWT.DEFAULT, false, false);
        data.widthHint = shortTextWidth;
        daySpinner.setLayoutData(data);
        daySpinner.setMinimum(0);
        daySpinner.setMaximum(32);
        daySpinner.addModifyListener((e) -> {
            int day = daySpinner.getSelection();
            cal.set(Calendar.DAY_OF_MONTH, day);
            updateTimeControls();
        });

        // create MPE Options area
        Group mpeOptionsGroup = new Group(composite, SWT.SHADOW_ETCHED_IN);
        mpeOptionsGroup.setText("MPE Options");
        GridLayout layout = new GridLayout(2, false);
        layout.horizontalSpacing *= 2;
        mpeOptionsGroup.setLayout(layout);
        mpeOptionsGroup.setLayoutData(
                new GridData(SWT.FILL, SWT.DEFAULT, true, false));

        Composite hourComp = new Composite(mpeOptionsGroup, SWT.NONE);
        layout = new GridLayout(2, false);
        layout.marginWidth = 0;
        layout.marginHeight = 0;
        hourComp.setLayout(layout);
        hourComp.setLayoutData(
                new GridData(SWT.DEFAULT, SWT.CENTER, false, true));

        Label hourLabel = new Label(hourComp, SWT.NONE);
        hourLabel.setText("Hour");

        hourSpinner = new Spinner(hourComp, SWT.BORDER);
        data = new GridData(SWT.DEFAULT, SWT.DEFAULT, false, false);
        data.widthHint = shortTextWidth;
        hourSpinner.setLayoutData(data);
        hourSpinner.setMinimum(-1);
        hourSpinner.setMaximum(24);
        hourSpinner.addModifyListener((e) -> {
            int hour = hourSpinner.getSelection();
            cal.set(Calendar.HOUR_OF_DAY, hour);
            updateTimeControls();
        });

        Composite statusComp = new Composite(mpeOptionsGroup, SWT.NONE);
        layout = new GridLayout(2, false);
        layout.marginWidth = 0;
        layout.marginHeight = 0;
        statusComp.setLayout(layout);
        statusComp.setLayoutData(new GridData(SWT.FILL, SWT.FILL, true, true));

        Label lab1 = new Label(statusComp, SWT.NONE);
        lab1.setText("Last Save:");

        lastSave = new Label(statusComp, SWT.NONE);
        gc = new GC(lastSave);
        int width = gc.textExtent("9999-99-99 99:99:99").x;
        gc.dispose();
        lastSave.setText("NA");
        GridData gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        gd.minimumWidth = width;
        lastSave.setLayoutData(gd);

        Label lab2 = new Label(statusComp, SWT.NONE);
        lab2.setText("Last Exec:");

        lastExec = new Label(statusComp, SWT.NONE);
        lastExec.setText("NA");
        gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        gd.minimumWidth = width;
        lastExec.setLayoutData(gd);

        Label lab3 = new Label(statusComp, SWT.NONE);
        lab3.setText("Manually Saved:");

        manuallySaved = new Label(statusComp, SWT.NONE);
        manuallySaved.setText("NA");
        manuallySaved.setLayoutData(
                new GridData(SWT.FILL, SWT.DEFAULT, true, false));

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
                boolean close = false;
                Date time = getTime();
                for (MPEDisplayManager dmgr : displayMgrs) {
                    close = dmgr.setCurrentEditDate(time, true);
                    if (!close) {
                        break;
                    }
                }
                if (close) {
                    close();
                } else {
                    restore();
                }
                TransmitRFCBiasProvider.setEnabled(true);
                TransmitBestEstimateQPEProvider.setEnabled(true);
            }
        });

        // create gage edit options area
        Group gageOptionsGroup = new Group(composite, SWT.SHADOW_ETCHED_IN);
        gageOptionsGroup.setText("6/24 hr gage edit options");
        layout = new GridLayout(2, true);
        layout.horizontalSpacing *= 2;
        gageOptionsGroup.setLayout(layout);
        gageOptionsGroup.setLayoutData(
                new GridData(SWT.FILL, SWT.DEFAULT, true, false));

        // create ending hydro date area
        Label hydrodateLabel = new Label(gageOptionsGroup, SWT.LEFT);
        hydrodateLabel.setText("Ending Hydrologic Date: ");
        data = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        data.horizontalSpan = 2;
        hydrodateLabel.setLayoutData(data);

        Composite hydrodateComp = new Composite(gageOptionsGroup, SWT.NONE);
        hydrodateComp.setLayout(new GridLayout(3, false));
        data = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        data.horizontalSpan = 2;
        hydrodateComp.setLayoutData(data);

        Label hydyearLabel = new Label(hydrodateComp, SWT.NONE);
        hydyearLabel.setText("Ending Year");

        Label hydmonthLabel = new Label(hydrodateComp, SWT.NONE);
        hydmonthLabel.setText("Month");

        Label hyddayLabel = new Label(hydrodateComp, SWT.NONE);
        hyddayLabel.setText("Day");

        hydyearText = new Text(hydrodateComp, SWT.BORDER | SWT.READ_ONLY);
        data = new GridData(SWT.FILL, SWT.DEFAULT, false, false);
        data.widthHint = longTextWidth;
        hydyearText.setLayoutData(data);

        hydmonthText = new Text(hydrodateComp, SWT.BORDER | SWT.READ_ONLY);
        data = new GridData(SWT.FILL, SWT.DEFAULT, false, false);
        data.widthHint = shortTextWidth;
        hydmonthText.setLayoutData(data);

        hyddaySpinner = new Spinner(hydrodateComp, SWT.BORDER);
        data = new GridData(SWT.DEFAULT, SWT.DEFAULT, false, false);
        data.widthHint = shortTextWidth;
        hyddaySpinner.setLayoutData(data);
        hyddaySpinner.setMinimum(0);
        hyddaySpinner.setMaximum(32);
        hyddaySpinner.addModifyListener((e) -> {
            int day = hyddaySpinner.getSelection();
            hydroCal.set(Calendar.DAY_OF_MONTH, day);
            updateTimeControls();
        });

        new Label(gageOptionsGroup, SWT.NONE);
        Label selectAreaLabel = new Label(gageOptionsGroup, SWT.NONE);
        selectAreaLabel.setText("Select Area");
        selectAreaLabel.setLayoutData(
                new GridData(SWT.CENTER, SWT.DEFAULT, false, false));

        Composite daysComp = new Composite(gageOptionsGroup, SWT.NONE);
        daysComp.setLayoutData(
                new GridData(SWT.DEFAULT, SWT.CENTER, false, true));
        layout = new GridLayout(2, false);
        layout.marginWidth = 0;
        layout.marginHeight = 0;
        daysComp.setLayout(layout);

        Label daysLabel = new Label(daysComp, SWT.NONE);
        daysLabel.setText("Num of Days");

        daysSpinner = new Spinner(daysComp, SWT.BORDER);
        data = new GridData(SWT.DEFAULT, SWT.DEFAULT, false, false);
        data.widthHint = shortTextWidth;
        daysSpinner.setLayoutData(data);
        daysSpinner.setMinimum(1);
        daysSpinner.setMaximum(10);
        daysSpinner.setSelection(displayMgrs.get(0).getDqcDays());
        daysSpinner.setEnabled(qcEnable);

        areaCombo = new Combo(gageOptionsGroup, SWT.DROP_DOWN);
        if (qcEnable) {
            buildAreaList();
            areaCombo.select(0);
        } else {
            areaCombo.clearSelection();
        }
        areaCombo.setLayoutData(
                new GridData(SWT.FILL, SWT.DEFAULT, false, false));
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
                for (MPEDisplayManager dmgr : displayMgrs) {
                    dmgr.setDqcDays(daysSpinner.getSelection());
                }
                prevArea = areaCombo.getItem(areaCombo.getSelectionIndex());
                setCurrentHydroEditDate(getHydroTime());

                MPEDisplayManager displayMgr = MPEDisplayManager.getCurrent();
                Display display = shell.getDisplay();
                QcPrecipOptionsDialog dialog = displayMgr.getQcPrecipDialog();
                if (dialog != null && dialog.isOpen()) {
                    displayMgr.getQcPrecipDialog().restore();
                } else {
                    dialog = new QcPrecipOptionsDialog(shell);
                    displayMgr.setQcPrecipDialog(dialog);
                }
                final QcPrecipOptionsDialog dlg = dialog;
                display.asyncExec(() -> dlg.open());
                close();
            }

        });

        Button tempBtn = new Button(buttonComp, SWT.PUSH);
        tempBtn.setText("Temperature");
        tempBtn.setEnabled(qcEnable);
        tempBtn.addSelectionListener(new SelectionAdapter() {

            @Override
            public void widgetSelected(SelectionEvent e) {
                for (MPEDisplayManager dmgr : displayMgrs) {
                    dmgr.setDqcDays(daysSpinner.getSelection());
                }
                prevArea = areaCombo.getItem(areaCombo.getSelectionIndex());
                setCurrentHydroEditDate(getHydroTime());

                MPEDisplayManager displayMgr = MPEDisplayManager.getCurrent();
                Display display = shell.getDisplay();
                QcTempOptionsDialog dialog = displayMgr.getQcTempDialog();
                if (dialog != null && dialog.isOpen()) {
                    displayMgr.getQcTempDialog().restore();
                } else {
                    dialog = new QcTempOptionsDialog(shell);
                    displayMgr.setQcTempDialog(dialog);
                }
                final QcTempOptionsDialog dlg = dialog;
                display.asyncExec(() -> dlg.open());
                close();
            }

        });

        Button freezeBtn = new Button(buttonComp, SWT.PUSH);
        freezeBtn.setText("Freezing");
        freezeBtn.setEnabled(qcEnable);
        freezeBtn.addSelectionListener(new SelectionAdapter() {

            @Override
            public void widgetSelected(SelectionEvent e) {
                for (MPEDisplayManager dmgr : displayMgrs) {
                    dmgr.setDqcDays(daysSpinner.getSelection());
                }
                prevArea = areaCombo.getItem(areaCombo.getSelectionIndex());
                setCurrentHydroEditDate(getHydroTime());

                MPEDisplayManager displayMgr = MPEDisplayManager.getCurrent();
                Display display = shell.getDisplay();
                QcFreezeOptionsDialog dialog = displayMgr.getQcFreezeDialog();
                if (dialog != null && dialog.isOpen()) {
                    displayMgr.getQcFreezeDialog().restore();
                } else {
                    dialog = new QcFreezeOptionsDialog(shell);
                    displayMgr.setQcFreezeDialog(dialog);
                }
                final QcFreezeOptionsDialog dlg = dialog;
                display.asyncExec(() -> dlg.open());
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

        Calendar aCal = Calendar.getInstance(TimeZone.getTimeZone("GMT"));
        aCal.setTime(displayMgrs.get(0).getCurrentEditDate());

        if (aCal.get(Calendar.HOUR_OF_DAY) >= 18) {
            aCal.add(Calendar.DATE, 1);
        }

        prevDate = cal.getTime();
        yearText.setText(Integer.toString(cal.get(Calendar.YEAR)));
        monthText.setText(Integer.toString(cal.get(Calendar.MONTH) + 1));
        daySpinner.setSelection(cal.get(Calendar.DAY_OF_MONTH));

        hourSpinner.setSelection(cal.get(Calendar.HOUR_OF_DAY));

        prevHydDate = hydroCal.getTime();
        hydyearText.setText(Integer.toString(hydroCal.get(Calendar.YEAR)));
        hydmonthText
                .setText(Integer.toString(hydroCal.get(Calendar.MONTH) + 1));

        hyddaySpinner.setSelection(hydroCal.get(Calendar.DAY_OF_MONTH));

        if (!dateMap.containsKey(cal.getTime())) {
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

    @Override
    protected void createButtonsForButtonBar(Composite parent) {
        createButton(parent, IDialogConstants.OK_ID,
                IDialogConstants.CLOSE_LABEL, true);
    }

    /**
     * Get the selected date/time as a java date.
     *
     * @return
     */
    public Date getTime() {
        return cal.getTime();
    }

    public Date getHydroTime() {
        return hydroCal.getTime();
    }

    private void setCurrentHydroEditDate(Date hydroTime) {
        currentHydroEndingDate = hydroTime;
    }

    public static Date getCurrentHydroEditDate() {
        return currentHydroEndingDate;
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
        String site_ids = appsDefaults.getToken("mpe_site_id");
        String area_names = appsDefaults.getToken("mpe_area_names");
        List<String> aList = new ArrayList<>();
        site_ids = site_ids.trim();

        if (area_names != null) {
            if (area_names.lastIndexOf(',') != -1) {
                for (int i = 0; i < area_names.length();) {
                    int j = area_names.indexOf(',', i);
                    if (j == -1) {
                        j = area_names.length();
                    }
                    aList.add(area_names.substring(i, j));
                    i = j + 1;
                }
            } else {
                aList.add(0, area_names.trim());
            }
        } else {
            aList.add(0, site_ids);
        }
        areaCombo.setItems(aList.toArray(new String[aList.size()]));
    }

    @Override
    protected void configureShell(Shell newShell) {
        super.configureShell(newShell);
        newShell.setText("Choose Data Period");
    }
}
