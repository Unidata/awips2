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
package com.raytheon.viz.hydro.timeseries;

import java.text.SimpleDateFormat;
import java.util.List;
import java.util.TimeZone;

import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.graphics.Point;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Shell;

import com.raytheon.viz.hydro.timeseries.util.TraceData;
import com.raytheon.viz.hydrocommon.ui.CaveHydroSWTDialog;

/**
 * This class is the popup menu when right/middle click on the Time Series
 * Display. There should be only one toggle time series dialog at a time.
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date          Ticket#     Engineer     Description
 * ------------- ----------- ------------ --------------------------
 * Jan 16, 2013  15695       wkwock       Initial creation
 * May 06, 2016  5483        dgilling     Code-cleanup.
 * Jun 27, 2018  6748        randerso     Reworked to better match A1 format.
 *
 * </pre>
 *
 * @author wkwock
 */
public class ToggleTimeSeriesDlg extends CaveHydroSWTDialog {
    /**
     * Interface for call back when a trace is selected
     */
    public static interface ITraceSelectionCB {

        /**
         * Called when a trace selection has changed
         */
        public void traceSelected();
    }

    private static final String DLG_TITLE = "Toggle Time Series";

    private final List<TraceData> traceList;

    private Point location;

    private final ITraceSelectionCB callback;

    /**
     * @param parentShell
     *            the parent shell
     * @param traceLst
     *            the list of trace data
     * @param location
     *            location where graph is to be displayed
     * @param callback
     *            callback to notify when traces are selected
     */
    public ToggleTimeSeriesDlg(Shell parentShell, List<TraceData> traceLst,
            Point location, ITraceSelectionCB callback) {
        super(parentShell, SWT.DIALOG_TRIM, CAVE.NONE);
        setText(DLG_TITLE);

        this.traceList = traceLst;
        this.location = location;
        this.callback = callback;
    }

    @Override
    protected void initializeComponents(Shell shell) {
        shell.setLayout(new GridLayout(2, true));

        Composite leftComp = new Composite(shell, SWT.NONE);
        leftComp.setLayout(new GridLayout(1, false));
        leftComp.setLayoutData(new GridData(SWT.FILL, SWT.FILL, true, true));

        Composite rightComp = new Composite(shell, SWT.NONE);
        rightComp.setLayout(new GridLayout(1, false));
        rightComp.setLayoutData(new GridData(SWT.FILL, SWT.FILL, true, true));

        int numLeft = (traceList.size() + 1) / 2;
        int count = 0;
        Composite comp = leftComp;
        for (TraceData traceData : traceList) {
            if (count >= numLeft) {
                comp = rightComp;
            }
            count++;

            Button checkBox = new Button(comp, SWT.CHECK);
            checkBox.setData(traceData);

            SimpleDateFormat dateFormat = new SimpleDateFormat("MM/dd HH:mm");
            dateFormat.setTimeZone(TimeZone.getTimeZone("GMT"));

            StringBuilder text = new StringBuilder();
            text.append(traceData.getPe().toUpperCase()).append(' ')
                    .append(traceData.getDur()).append(' ')
                    .append(traceData.getTs().toUpperCase()).append(' ')
                    .append(traceData.getExtremum().toUpperCase());

            if (traceData.isForecast()) {
                if (traceData.getBasistime() != null) {
                    text.append(' ')
                            .append(dateFormat.format(traceData.getBasistime()))
                            .append('z');
                }
            }

            if (traceData.getNpts() > 0) {
                checkBox.setSelection(traceData.isTraceOn());
            } else {
                checkBox.setSelection(false);
                text.append(" (NO DATA)");
            }
            checkBox.setText(text.toString());

            checkBox.addSelectionListener(new SelectionAdapter() {

                @Override
                public void widgetSelected(SelectionEvent e) {
                    Button checkBox = (Button) e.widget;
                    ((TraceData) checkBox.getData())
                            .setTraceOn(checkBox.getSelection());
                    callback.traceSelected();
                }
            });
        }
    }

    @Override
    protected void preOpened() {
        getShell().setLocation(location);
    }
}
