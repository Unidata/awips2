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
package com.raytheon.uf.viz.d2d.ui.time.dialogs;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.Date;
import java.util.HashSet;
import java.util.Iterator;
import java.util.Set;

import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.events.SelectionListener;
import org.eclipse.swt.graphics.GC;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.List;
import org.eclipse.swt.widgets.Shell;

import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.common.time.DataTime.FLAG;
import com.raytheon.uf.common.time.SimulatedTime;
import com.raytheon.uf.viz.core.comm.PerspectiveSpecificLoadProperties;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.d2d.core.D2DLoadProperties;
import com.raytheon.uf.viz.d2d.core.time.LoadMode;
import com.raytheon.uf.viz.d2d.core.time.TimeMatcher;
import com.raytheon.uf.viz.d2d.ui.time.formatter.TimeFormatter;

/**
 * Dialog for selecting the user desired time matches based on the selected
 * latest time and time resolution
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date          Ticket#  Engineer  Description
 * ------------- -------- --------- --------------------------------------------
 * Apr 15, 2009           bgonzale  Initial creation
 * Apr 20, 2018  6624     bsteffen  Fixes for forecast products.
 * Jul 30, 2018  7259     bsteffen  Configure clockDelta instead of specific
 *                                  times.
 * Jul 02, 2019  65608    tjensen   Add Refresh and Reverse Order buttons
 * Jul 10, 2020  80700    tjensen   Fix issues with Reverse Order
 *
 * </pre>
 *
 * @author bgonzale
 */
public class ValidTimeAndTimeResolutionDialog
        extends AbstractTimeMatchingDialog {

    private static final String DEFAULT = "Default";

    /*
     * Formatter for displaying data time.
     */
    private TimeFormatter timeFormatter;

    private List vTimeList;

    private List tResolutionList;

    /*
     * Time resolutions in seconds calculated from the intrinsic period for the
     * given resource's available times.
     */
    private java.util.List<Long> resolutions;

    private DataTime[] cachedAvailableTimes;

    private boolean reversed = false;

    private LoadMode loadMode;

    /**
     * @param parentShell
     * @throws VizException
     */
    public ValidTimeAndTimeResolutionDialog(Shell parentShell) {
        super(parentShell);
        setText("Time Options");

    }

    @Override
    public void init() throws VizException {
        PerspectiveSpecificLoadProperties perspProps = loadProperties
                .getPerspectiveProperty();
        D2DLoadProperties d2dProps = null;
        if (perspProps instanceof D2DLoadProperties) {
            d2dProps = (D2DLoadProperties) perspProps;
        } else {
            d2dProps = new D2DLoadProperties();
            loadProperties.setPerspectiveProperty(d2dProps);
        }

        this.config = d2dProps.getTimeConfig();
        this.loadMode = timeMatcher.getLoadMode();
        this.resolutions = new ArrayList<>();
        Arrays.sort(availableTimes);
        Set<Date> uniqueValidTimes = new HashSet<>();
        java.util.List<DataTime> timeList = new ArrayList<>(
                availableTimes.length);
        for (DataTime dataTime : availableTimes) {
            if (uniqueValidTimes.add(dataTime.getValidTimeAsDate())) {
                timeList.add(dataTime);
            }
        }
        this.cachedAvailableTimes = timeList.toArray(new DataTime[0]);
        this.timeFormatter = new TimeFormatter();
    }

    @Override
    protected void initializeComponents(Shell shell) {
        Composite mainComp = new Composite(shell, SWT.NONE);
        mainComp.setLayout(new GridLayout(2, false));

        Label vTimeLabel = new Label(mainComp, SWT.NONE);
        vTimeLabel.setText("Valid Time:");
        Label tResolutionLabel = new Label(mainComp, SWT.NONE);
        tResolutionLabel.setText("Time Resolution:");

        createValidTimeWidget(mainComp);
        createTimeResolutionWidget(mainComp);
        createRefreshBtns(shell, mainComp);
        updateLists();

        super.initializeComponents(shell);
    }

    private void createRefreshBtns(Shell shell, Composite mainComp) {
        GridData gd = new GridData(SWT.FILL, SWT.FILL, true, true);
        Button btn = new Button(mainComp, SWT.PUSH);
        btn.setText("Refresh");
        gd = new GridData(SWT.CENTER, SWT.CENTER, true, true);
        GC gc = new GC(shell.getDisplay());
        int width = gc.getFontMetrics().getAverageCharWidth() * 18;
        gd.widthHint = width;
        btn.setLayoutData(gd);
        btn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                updateLists();
            }
        });

        Button reverseBtn = new Button(mainComp, SWT.PUSH);
        reverseBtn.setText("Reverse Order");
        gd = new GridData(SWT.CENTER, SWT.CENTER, true, true);
        gd.widthHint = width;
        reverseBtn.setLayoutData(gd);
        reverseBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                toggleReversed();
                updateLists();
            }
        });
    }

    private void toggleReversed() {
        if (reversed) {
            reversed = false;
        } else {
            reversed = true;
        }
    }

    private void createValidTimeWidget(Composite parent) {
        GridData gd = new GridData(SWT.FILL, SWT.FILL, true, true);
        gd.heightHint = 500;
        vTimeList = new List(parent, SWT.BORDER | SWT.SINGLE | SWT.V_SCROLL);
        vTimeList.setLayoutData(gd);
        vTimeList.add(DEFAULT);
        vTimeList.select(0);
        vTimeList.addSelectionListener(new SelectionListener() {
            @Override
            public void widgetDefaultSelected(SelectionEvent e) {
                // no operation
            }

            @Override
            public void widgetSelected(SelectionEvent e) {
                updateLists();
                refreshDisplay();
            }
        });
    }

    private void createTimeResolutionWidget(Composite parent) {
        GridData gd = new GridData(SWT.FILL, SWT.FILL, true, true);
        gd.heightHint = 500;
        tResolutionList = new List(parent,
                SWT.BORDER | SWT.SINGLE | SWT.V_SCROLL);
        tResolutionList.setLayoutData(gd);
        updateTimeResolutionList();
        tResolutionList.select(0);
        tResolutionList.addSelectionListener(new SelectionListener() {
            @Override
            public void widgetDefaultSelected(SelectionEvent e) {
                // no operation
            }

            @Override
            public void widgetSelected(SelectionEvent e) {
                updateLists();
                refreshDisplay();
            }
        });
    }

    private void updateTimeResolutionList() {

        if (cachedAvailableTimes != null && cachedAvailableTimes.length > 0) {

            int currentSelection = tResolutionList.getSelectionIndex();
            tResolutionList.removeAll();
            resolutions.clear();

            Long forecast = config.getForecast();
            if (forecast == null) {
                forecast = timeMatcher.getForecastFilter();
            }
            resolutions.addAll(TimeMatcher.computeTimeRes(forecast.intValue(),
                    cachedAvailableTimes, false));
            Iterator<Long> resIter = resolutions.iterator();
            int frameCount = descriptor.getNumberOfFrames();
            if (resIter.hasNext()) {
                Long resHours = resIter.next();
                StringBuilder sb = new StringBuilder(DEFAULT);
                sb.append(" ");
                sb.append(timeFormatter.getFormattedTimePeriodString(resHours,
                        frameCount));
                tResolutionList.add(sb.toString());
            }
            for (; resIter.hasNext();) {
                Long resHours = resIter.next();
                tResolutionList.add(timeFormatter
                        .getFormattedTimePeriodString(resHours, frameCount));
            }
            tResolutionList.select(currentSelection);

        }

    }

    private Long getSelectedMillisecondsResolution() {
        int selectedIndex = tResolutionList.getSelectionIndex();
        return resolutions.get(selectedIndex) * 1000;
    }

    private DataTime getSelectedLatestTime() {
        DataTime selectedLatest = null;
        // 'Default' occupies the first element in the vTimeList and has the
        // same value as the first time value.
        int selectedIndex = (vTimeList.getSelectionIndex() < 2 ? 0
                : vTimeList.getSelectionIndex() - 1);

        if (cachedAvailableTimes.length > 0) {
            // vTimeList is ordered inverted compared to cachedAvailableTimes
            int timesIndexes = cachedAvailableTimes.length - 1;
            int aTimeIndex = timesIndexes - selectedIndex;

            selectedLatest = cachedAvailableTimes[aTimeIndex];
        }
        return selectedLatest;
    }

    private void updateValidTimeList() {
        if (vTimeList.getSelectionIndex() == 0) {
            config.setClock(
                    cachedAvailableTimes[cachedAvailableTimes.length - 1]
                            .getValidTimeAsDate());
            config.setForecast(timeMatcher.getForecastFilter());
            config.setClockDelta(null);
        } else {
            DataTime dt = getSelectedLatestTime();
            Date validTime = dt.getValidTimeAsDate();
            config.setClock(validTime);
            config.setForecast(validTime.getTime());
            config.setClockDelta(SimulatedTime.getSystemTime().getMillis()
                    - validTime.getTime());
        }

        config.setDelta(getSelectedMillisecondsResolution());
        if (cachedAvailableTimes != null && cachedAvailableTimes.length > 0) {
            DataTime[] dataTimesArray = TimeMatcher.makeEmptyLoadList(
                    cachedAvailableTimes, config.getClock(),
                    descriptor.getNumberOfFrames(), loadMode,
                    config.getForecast(), config.getDelta());
            java.util.List<DataTime> dataTimesList = new ArrayList<>(
                    dataTimesArray.length);

            // ignore any nulls
            for (DataTime dataTime : dataTimesArray) {
                if (dataTime != null) {
                    dataTimesList.add(dataTime);
                }
            }

            int currentSelection = vTimeList.getSelectionIndex();
            vTimeList.removeAll();
            vTimeList.add("Default");

            DataTime[] myTimes = cachedAvailableTimes.clone();
            if (reversed) {
                Arrays.sort(myTimes, Collections.reverseOrder());
            }

            for (int i = myTimes.length - 1; i >= 0; --i) {
                DataTime dataTime = myTimes[i];
                StringBuilder sb = new StringBuilder();

                if (Collections.binarySearch(dataTimesList, dataTime) < 0) {
                    sb.append("...");
                }
                if (dataTime.getUtilityFlags().contains(FLAG.FCST_USED)) {
                    sb.append(
                            timeFormatter.getDayHourForecastHRString(dataTime));
                } else {
                    sb.append(
                            timeFormatter.getDayTimeForecastHRString(dataTime));
                }
                vTimeList.add(sb.toString());
            }
            vTimeList.select(currentSelection);
        }
    }

    private void updateLists() {
        updateTimeResolutionList();
        updateValidTimeList();
    }
}
