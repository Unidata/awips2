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
import java.util.Calendar;
import java.util.Collections;
import java.util.Comparator;

import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.List;
import org.eclipse.swt.widgets.Shell;

import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.viz.core.comm.PerspectiveSpecificLoadProperties;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.d2d.core.D2DLoadProperties;
import com.raytheon.uf.viz.d2d.ui.time.formatter.TimeFormatter;

/**
 * Dialog for selecting the time offset and time matching tolerance.
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

public class ForecastAndInventoryTimeDialog extends AbstractTimeMatchingDialog {

    private TimeFormatter timeFormatter;

    private List inventoryList;

    private List forecastList;

    private java.util.List<DataTime> inventoryTimes;

    private java.util.List<DataTime> forecastTimes;

    /**
     * @param parentShell
     * @throws VizException
     */
    public ForecastAndInventoryTimeDialog(Shell parentShell) {
        super(parentShell);
        setText("Select Forecast And Inventory Time");
    }

    @Override
    public void init() throws VizException {
        this.forecastTimes = new ArrayList<DataTime>();
        this.inventoryTimes = new ArrayList<DataTime>();

        java.util.List<DataTime> availableTimes = Arrays
                .asList(this.availableTimes);
        Calendar now = Calendar.getInstance();
        if (availableTimes != null && !availableTimes.isEmpty()) {
            for (DataTime dataTime : availableTimes) {
                // is this data time a forecast
                DataTime fcstTime = new DataTime(now, dataTime.getFcstTime());
                if (!forecastTimes.contains(fcstTime)) {
                    this.forecastTimes.add(fcstTime);
                }

                DataTime invTime = new DataTime(dataTime.getRefTimeAsCalendar());
                if (!inventoryTimes.contains(invTime)) {
                    this.inventoryTimes.add(invTime);
                }
            }
        }

        Collections.sort(this.forecastTimes);
        Collections.sort(this.inventoryTimes, new Comparator<DataTime>() {
            @Override
            public int compare(DataTime o1, DataTime o2) {
                // sort inventory in descending order
                return o2.compareTo(o1);
            }
        });

        this.timeFormatter = new TimeFormatter();
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
    }

    @Override
    protected void initializeComponents(Shell shell) {
        shell.setMinimumSize(360, 300);
        Composite composite = new Composite(shell, SWT.FILL);
        GridLayout gl = new GridLayout(2, false);
        composite.setLayout(gl);
        composite.setLayoutData(new GridData(SWT.FILL, SWT.FILL, true, true));

        Label forecastLabel = new Label(composite, SWT.NONE);
        forecastLabel.setText("Forecast Time:");
        Label inventoryLabel = new Label(composite, SWT.NONE);
        inventoryLabel.setText("Inventory Time:");

        GridData gd = new GridData(SWT.DEFAULT, SWT.FILL, false, true);
        gd.heightHint = 350;
        gd.widthHint = 125;
        forecastList = new List(composite, SWT.BORDER | SWT.SINGLE
                | SWT.V_SCROLL);
        forecastList.setLayoutData(gd);
        for (DataTime dataTime : forecastTimes) {
            forecastList.add(this.timeFormatter
                    .getDayHourForecastHRString(dataTime));
        }

        if (forecastList.getItemCount() > 0) {
            forecastList.select(0);
        }

        gd = new GridData(SWT.FILL, SWT.FILL, true, true);
        gd.heightHint = 350;
        gd.widthHint = 200;
        inventoryList = new List(composite, SWT.BORDER | SWT.SINGLE
                | SWT.V_SCROLL);
        inventoryList.setLayoutData(gd);
        for (DataTime dataTime : inventoryTimes) {
            inventoryList.add(this.timeFormatter
                    .getTimeMonthDateString(dataTime));
        }
        if (inventoryList.getItemCount() == 0) {
            // no inventory times, use the latest available time if available
            if (inventoryTimes != null && !inventoryTimes.isEmpty()) {
                DataTime first = inventoryTimes.iterator().next();
                forecastList.add(this.timeFormatter
                        .getDayTimeForecastHRString(first));
            }
        }

        if (inventoryList.getItemCount() > 0) {
            inventoryList.select(0);
        }
        super.initializeComponents(shell);
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.viz.d2d.ui.time.dialogs.AbstractTimeMatchingDialog#
     * setSelectedDataTimes()
     */
    @Override
    protected void setSelectedDataTimes() {
        int inventoryIdx = inventoryList.getSelectionIndex();
        int forecastIdx = forecastList.getSelectionIndex();

        DataTime forecastTime = forecastTimes.get(forecastIdx);
        DataTime inventoryTime = inventoryTimes.get(inventoryIdx);

        java.util.List<DataTime> times = new ArrayList<DataTime>();
        for (DataTime avail : availableTimes) {
            if (avail.getFcstTime() == forecastTime.getFcstTime()
                    && avail.getMatchRef() == inventoryTime.getMatchRef()) {
                times.add(avail);
            }
        }
        config.setForecast(forecastTime.getMatchFcst());
        config.setDataTimes(times.toArray(new DataTime[times.size()]));
    }

}
