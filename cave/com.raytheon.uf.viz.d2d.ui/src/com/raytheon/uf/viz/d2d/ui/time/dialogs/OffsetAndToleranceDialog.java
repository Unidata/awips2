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
import java.util.HashMap;
import java.util.Map;

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

public class OffsetAndToleranceDialog extends AbstractTimeMatchingDialog {

    private static final Map<String, Float> timeSlopMap = createTimeSlopMap();

    private TimeFormatter timeFormatter;

    private List offsetList;

    private List toleranceList;

    private boolean useForecastDialog;

    private List forecastList;

    /*
     * Offsets in seconds calculated from the given resource's available times.
     */
    AbstractList<Long> offsets;

    private int forecastFilter;

    private DataTime[] cachedResourceTimes;

    /**
     * @param parentShell
     * @throws VizException
     */
    public OffsetAndToleranceDialog(Shell parentShell) {
        super(parentShell);
    }

    public void init() throws VizException {
        this.forecastFilter = (int) timeMatcher.getForecastFilter();
        this.offsets = new ArrayList<Long>();
        this.timeFormatter = new TimeFormatter();
        this.useForecastDialog = timeMatcher.hasTimeMatchBasis()
                && timeMatcher.getLoadMode().equals(LoadMode.PROG_LOOP);
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
    }

    @Override
    protected void initializeComponents(Shell shell) {
        Composite composite = new Composite(shell, SWT.NONE);

        int columns = useForecastDialog ? 3 : 2;
        GridLayout gl = new GridLayout(columns, false);

        composite.setLayout(gl);
        if (useForecastDialog) {
            Label offsetLabel = new Label(composite, SWT.NONE);
            offsetLabel.setText("Forecast:");
        }
        Label offsetLabel = new Label(composite, SWT.NONE);
        offsetLabel.setText("Offset:");
        Label toleranceLabel = new Label(composite, SWT.NONE);
        toleranceLabel.setText("Tolerance:");

        if (useForecastDialog) {
            createForecastWidget(composite);
        }
        createOffsetWidget(composite);
        createToleranceWidget(composite);

        super.initializeComponents(shell);
    }

    private void createForecastWidget(Composite parent) {
        GridData gd = new GridData(SWT.FILL, SWT.FILL, true, true);
        gd.heightHint = 350;
        gd.widthHint = 125;

        forecastList = new List(parent, SWT.BORDER | SWT.SINGLE | SWT.V_SCROLL);
        forecastList.setLayoutData(gd);

        if (cachedResourceTimes != null && cachedResourceTimes.length > 0) {
            for (DataTime dt : this.cachedResourceTimes) {
                if (dt.getUtilityFlags().contains(FLAG.FCST_USED)) {
                    forecastList.add(this.timeFormatter
                            .getDayHourForecastHRString(dt));
                }
            }
        }
        if (forecastList.getItemCount() > 0) {
            forecastList.select(0);
        } else {
            DataTime dataTime = cachedResourceTimes[cachedResourceTimes.length - 1];
            forecastList
                    .add(timeFormatter.getDayTimeForecastHRString(dataTime));
        }
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

    private void createOffsetWidget(Composite parent) {
        GridData gd = new GridData(SWT.FILL, SWT.FILL, true, true);
        gd.heightHint = 350;
        gd.widthHint = 125;
        offsetList = new List(parent, SWT.BORDER | SWT.SINGLE | SWT.V_SCROLL);
        offsetList.setLayoutData(gd);
        if (cachedResourceTimes != null && cachedResourceTimes.length > 0) {
            offsets.addAll(TimeMatcher.computeTimeRes(forecastFilter,
                    cachedResourceTimes, true));

            for (Long offset : offsets) {
                if (offset == 0) {
                    offsetList.add("none");
                    offsetList.select(offsetList.getItemCount() - 1);
                } else {
                    offsetList.add(timeFormatter
                            .getFormattedOffsetString(offset));
                }
            }
        }
        offsetList.addSelectionListener(new SelectionListener() {
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

    private void createToleranceWidget(Composite parent) {
        GridData gd = new GridData(SWT.FILL, SWT.FILL, true, true);
        gd.heightHint = 350;
        gd.widthHint = 125;
        toleranceList = new List(parent, SWT.BORDER | SWT.SINGLE | SWT.V_SCROLL);
        toleranceList.setLayoutData(gd);
        toleranceList.add("none");
        toleranceList.add("strict");
        toleranceList.add("default");
        toleranceList.add("loose");
        toleranceList.add("infinite");
        toleranceList.select(2);
        toleranceList.addSelectionListener(new SelectionListener() {
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

    private void getConfigSelections() {
        config.setDelta(getSelectedMillisecondOffset());
        config.setTolerance(getSelectedTolerance());
        if (useForecastDialog) {
            config.setForecast(getSelectedForecast());
        }
    }

    private Long getSelectedMillisecondOffset() {
        int selectedIndex = offsetList.getSelectionIndex();
        return offsets.get(selectedIndex) * 1000;
    }

    private Float getSelectedTolerance() {
        String selection = toleranceList.getSelection()[0];
        return timeSlopMap.get(selection);
    }

    private long getSelectedForecast() {
        int selectionIndex = forecastList.getSelectionIndex();

        if (cachedResourceTimes != null && cachedResourceTimes.length > 0) {
            int fcstIndex = 0;

            for (DataTime dt : this.cachedResourceTimes) {
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

    private static final Map<String, Float> createTimeSlopMap() {
        Map<String, Float> slopMap = new HashMap<String, Float>();
        slopMap.put("none", -1f);
        slopMap.put("strict", 0f);
        slopMap.put("default", 30f);
        slopMap.put("loose", 100f);
        slopMap.put("infinite", 999999f);
        return Collections.unmodifiableMap(slopMap);
    }

}
