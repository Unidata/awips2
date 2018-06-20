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
import java.util.Iterator;

import org.eclipse.swt.SWT;
import org.eclipse.swt.events.MouseEvent;
import org.eclipse.swt.events.MouseListener;
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
import com.raytheon.uf.viz.d2d.core.time.TimeMatcher;
import com.raytheon.uf.viz.d2d.ui.actions.TimeOptionsAction;
import com.raytheon.uf.viz.d2d.ui.time.formatter.TimeFormatter;

/**
 * Dialog for selecting the forecast and time resolution.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date          Ticket#  Engineer  Description
 * ------------- -------- --------- --------------------------------------
 * Jul 24, 2009  2698     jelkins   Initial creation
 * Jul 27, 2009  2698     bgonzale  Cleanup/comments
 * Aug 20, 2014  3506     mapeters  Corrected misspelling.
 * Apr 01, 2016  5531     bsteffen  Handle time options setting globally.
 * 
 * </pre>
 * 
 * @author bgonzale
 * @version 1.0
 */

public class ForecastAndResolutionDialog extends AbstractTimeMatchingDialog {

    private TimeFormatter timeFormatter;

    private List forecastList;

    private List tResolutionList;

    /*
     * Time resolutions in seconds calculated from the intrinsic period for the
     * given resource's available times.
     */
    AbstractList<Long> resolutions;

    private int frameCount;

    private boolean useResolutionDialog;

    private DataTime[] cachedResourceTimes;

    private DataTime[] displayedTimes;

    /**
     * @param parentShell
     * @throws VizException
     */
    public ForecastAndResolutionDialog(Shell parentShell) {
        super(parentShell);
        setText("Select Forecast Time And Time Resolution");
    }

    @Override
    public void init() throws VizException {
        this.timeFormatter = new TimeFormatter();
        this.frameCount = descriptor.getNumberOfFrames();
        this.cachedResourceTimes = this.availableTimes;
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
        this.resolutions = new ArrayList<Long>();
        this.useResolutionDialog = TimeOptionsAction.isTimeOptionsSelected();
    }

    @Override
    protected void initializeComponents(Shell shell) {
        Composite composite = new Composite(shell, SWT.NONE);
        int columns = useResolutionDialog ? 2 : 1;
        GridLayout gl = new GridLayout(columns, false);

        composite.setLayout(gl);
        Label offsetLabel = new Label(composite, SWT.NONE);
        offsetLabel.setText("Forecast Time:");

        if (useResolutionDialog) {
            Label tResolutionLabel = new Label(composite, SWT.NONE);
            tResolutionLabel.setText("Time Resolution:");
        } else {
            super.getShell().setText("Select Forecast Time");
        }

        createForecastWidget(composite);
        if (useResolutionDialog) {
            createTimeResolutionWidget(composite);
        }

        super.initializeComponents(shell);

        getConfigSelections();
    }

    private void createForecastWidget(Composite parent) {
        GridData gd = new GridData(SWT.FILL, SWT.FILL, true, true);
        gd.heightHint = 350;
        gd.widthHint = 125;
        forecastList = new List(parent, SWT.BORDER | SWT.SINGLE | SWT.V_SCROLL);
        forecastList.setLayoutData(gd);
        java.util.List<DataTime> displayTimes = new ArrayList<DataTime>();
        if (cachedResourceTimes != null && cachedResourceTimes.length > 0) {
            java.util.List<String> times = new ArrayList<String>();
            for (DataTime dt : this.cachedResourceTimes) {
                if (dt.getUtilityFlags().contains(FLAG.FCST_USED)) {
                    String time = this.timeFormatter
                            .getDayHourForecastHRString(dt);
                    if (times.contains(time) == false) {
                        forecastList.add(time);
                        times.add(time);
                        displayTimes.add(dt);
                    }
                }
            }
        }
        if (forecastList.getItemCount() > 0) {
            forecastList.select(0);
        } else {
            DataTime dataTime = cachedResourceTimes[cachedResourceTimes.length - 1];
            forecastList
                    .add(timeFormatter.getDayTimeForecastHRString(dataTime));
            displayTimes.add(dataTime);
        }
        displayedTimes = displayTimes.toArray(new DataTime[] {});
        forecastList.addMouseListener(new MouseListener() {
            @Override
            public void mouseUp(MouseEvent e) {
            }

            @Override
            public void mouseDown(MouseEvent e) {
            }

            @Override
            public void mouseDoubleClick(MouseEvent e) {
                getConfigSelections();
                getShell().dispose();
            }
        });
        forecastList.addSelectionListener(new SelectionListener() {
            @Override
            public void widgetDefaultSelected(SelectionEvent e) {
                // no operation
            }

            @Override
            public void widgetSelected(SelectionEvent e) {
                getConfigSelections();
                refreshDisplay();
            }
        });
    }

    private void createTimeResolutionWidget(Composite parent) {
        GridData gd = new GridData(SWT.FILL, SWT.FILL, true, true);
        gd.heightHint = 350;
        gd.widthHint = 125;
        tResolutionList = new List(parent, SWT.BORDER | SWT.SINGLE
                | SWT.V_SCROLL);
        tResolutionList.setLayoutData(gd);
        if (cachedResourceTimes != null && cachedResourceTimes.length > 0) {
            Long forecast = getSelectedForecast();
            resolutions.addAll(TimeMatcher.computeTimeRes(forecast.intValue(),
                    cachedResourceTimes, false));
            Iterator<Long> resIter = resolutions.iterator();
            if (resIter.hasNext()) {
                Long resHours = resIter.next();
                StringBuilder sb = new StringBuilder("Default");
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
                // no operation
            }
        });
    }

    private void getConfigSelections() {
        config.setForecast(getSelectedForecast());
        if (useResolutionDialog) {
            config.setDelta(getSelectedMillisecondsResolution());
        }
    }

    private long getSelectedForecast() {
        int selectionIndex = forecastList.getSelectionIndex();

        if (displayedTimes != null && displayedTimes.length > 0) {
            int fcstIndex = 0;

            for (DataTime dt : this.displayedTimes) {
                if (dt.getUtilityFlags().contains(FLAG.FCST_USED)) {
                    if (selectionIndex == fcstIndex) {
                        return dt.getMatchFcst();
                    }
                    fcstIndex++;
                }
            }
        }
        return 0; // no forecast selected
    }

    private Long getSelectedMillisecondsResolution() {
        int selectedIndex = tResolutionList.getSelectionIndex();
        return resolutions.get(selectedIndex) * 1000;
    }

}
