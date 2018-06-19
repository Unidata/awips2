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

import java.util.AbstractList;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Date;
import java.util.Iterator;

import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.events.SelectionListener;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.List;
import org.eclipse.swt.widgets.Shell;

import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.common.time.DataTime.FLAG;
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
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Apr 15, 2009            bgonzale     Initial creation
 * 
 * </pre>
 * 
 * @author bgonzale
 * @version 1.0
 */

public class ValidTimeAndTimeResolutionDialog extends
        AbstractTimeMatchingDialog {

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
    private AbstractList<Long> resolutions;

    private DataTime[] cachedAvailableTimes;

    private int frameCount;

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
        this.frameCount = descriptor.getNumberOfFrames();
        this.resolutions = new ArrayList<Long>();
        this.cachedAvailableTimes = availableTimes;
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
        updateValidTimeList();

        super.initializeComponents(shell);
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
                updateValidTimeList();
                refreshDisplay();
            }
        });
    }

    private void createTimeResolutionWidget(Composite parent) {
        GridData gd = new GridData(SWT.FILL, SWT.FILL, true, true);
        gd.heightHint = 500;
        tResolutionList = new List(parent, SWT.BORDER | SWT.SINGLE
                | SWT.V_SCROLL);
        tResolutionList.setLayoutData(gd);
        if (cachedAvailableTimes != null && cachedAvailableTimes.length > 0) {
            Long forecast = config.getForecast();
            if (forecast == null) {
                forecast = timeMatcher.getForecastFilter();
            }
            resolutions.addAll(TimeMatcher.computeTimeRes(forecast.intValue(),
                    cachedAvailableTimes, false));
            Iterator<Long> resIter = resolutions.iterator();
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
                tResolutionList.add(timeFormatter.getFormattedTimePeriodString(
                        resHours, frameCount));
            }
        }
        tResolutionList.select(0);
        tResolutionList.addSelectionListener(new SelectionListener() {
            @Override
            public void widgetDefaultSelected(SelectionEvent e) {
                // no operation
            }

            @Override
            public void widgetSelected(SelectionEvent e) {
                updateValidTimeList();
                refreshDisplay();
            }
        });
    }

    private Long getSelectedMillisecondsResolution() {
        int selectedIndex = tResolutionList.getSelectionIndex();
        return resolutions.get(selectedIndex) * 1000;
    }

    private DataTime getSelectedLatestTime() {
        DataTime selectedLatest = null;
        // 'Default' occupies the first element in the vTimeList and has the
        // same value as the first time value.
        int selectedIndex = (vTimeList.getSelectionIndex() < 2 ? 0 : vTimeList
                .getSelectionIndex() - 1);

        if (cachedAvailableTimes.length > 0) {
            // vTimeList is ordered inverted compared to cachedAvailableTimes
            int timesIndexes = cachedAvailableTimes.length - 1;
            int aTimeIndex = timesIndexes - selectedIndex;

            selectedLatest = cachedAvailableTimes[aTimeIndex];
        }
        return selectedLatest;
    }

    private void updateValidTimeList() {
        DataTime dt = getSelectedLatestTime();
        config.setClock(dt.getValidTime().getTime());
        config.setForecast(new Long(dt.getFcstTime()));
        config.setDelta(getSelectedMillisecondsResolution());

        if (cachedAvailableTimes != null && cachedAvailableTimes.length > 0) {
            DataTime[] dataTimesArray = TimeMatcher.makeEmptyLoadList(
                    cachedAvailableTimes, calculatePreferredLatestTime(),
                    frameCount, loadMode, config.getForecast(),
                    config.getDelta());
            java.util.List<DataTime> dataTimesList = new ArrayList<DataTime>(
                    dataTimesArray.length);

            // ignore any nulls
            for (DataTime dataTime : dataTimesArray) {
                if (dataTime != null) {
                    dataTimesList.add(dataTime);
                }
            }
            config.setDataTimes(dataTimesList.toArray(new DataTime[0]));

            int currentSelection = vTimeList.getSelectionIndex();
            vTimeList.removeAll();
            vTimeList.add("Default");

            for (int i = cachedAvailableTimes.length - 1; i >= 0; --i) {
                DataTime dataTime = cachedAvailableTimes[i];
                StringBuilder sb = new StringBuilder();

                if (Collections.binarySearch(dataTimesList, dataTime) < 0) {
                    sb.append("...");
                }
                if (dataTime.getUtilityFlags().contains(FLAG.FCST_USED)) {
                    sb.append(timeFormatter
                            .getDayHourForecastHRString(dataTime));
                } else {
                    sb.append(timeFormatter
                            .getDayTimeForecastHRString(dataTime));
                }
                vTimeList.add(sb.toString());
            }
            vTimeList.select(currentSelection);
        }
    }

    private Date calculatePreferredLatestTime() {
        Date latestDate = null;
        long clockMillis = config.getClock().getTime();
        long frameDelta = config.getDelta() * frameCount;
        long earliestTime = clockMillis - frameDelta;
        DataTime earliestCached = cachedAvailableTimes[0];
        long minDiff = earliestTime - earliestCached.getRefTime().getTime();

        if (minDiff >= 0) {
            // there are enough possible frame times before the earliest cached
            // time.
            latestDate = config.getClock();
        } else {
            // how many possible frames are available in the time span before
            // the earliestCached time?
            long framesBeforeEarliest = Math
                    .round((double) (clockMillis - earliestCached.getRefTime()
                            .getTime()) / (double) config.getDelta());
            long framesNeeded = frameCount - framesBeforeEarliest;
            long latestMillis = clockMillis + config.getDelta() * framesNeeded;
            latestDate = new Date(latestMillis);
        }
        return latestDate;
    }

}
