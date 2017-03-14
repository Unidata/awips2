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

import java.util.Collection;

import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Shell;

import com.raytheon.viz.hydro.CaveHydroSWTDialog;
import com.raytheon.viz.hydro.timeseries.util.TraceData;
import com.raytheon.viz.ui.dialogs.ICloseCallback;

/**
 * This class is the popup menu when right/middle click on the Time Series
 * Display. There should be only one toggle time series dialog at a time.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jan 16, 2013  15695     wkwock      Initial creation
 * May 06, 2016  #5483     dgilling    Code-cleanup.
 * 
 * </pre>
 * 
 * @author wkwock
 * @version 1.0
 */
public class ToggleTimeSeriesDlg extends CaveHydroSWTDialog {

    private static final String DLG_TITLE = "Toggle Time Series";

    private static ToggleTimeSeriesDlg dialogSingleton = null;

    private final Collection<TraceData> traceList;

    private final TimeSeriesDisplayCanvas tsDisCanvas;

    public static ToggleTimeSeriesDlg getInstance(Shell parentShell,
            Collection<TraceData> traceLst, TimeSeriesDisplayCanvas tsdc) {
        if (dialogSingleton != null) {
            dialogSingleton.close();
            dialogSingleton.setCloseCallback(new ICloseCallback() {

                @Override
                public void dialogClosed(Object returnValue) {
                    dialogSingleton = null;
                }
            });
        }
        dialogSingleton = new ToggleTimeSeriesDlg(parentShell, traceLst, tsdc);
        return dialogSingleton;
    }

    /**
     * @param parentShell
     * @param traceLst
     * @param tsdc
     */
    private ToggleTimeSeriesDlg(Shell parentShell,
            Collection<TraceData> traceLst, TimeSeriesDisplayCanvas tsdc) {
        super(parentShell, SWT.DIALOG_TRIM | SWT.RESIZE, CAVE.NONE);
        setText(DLG_TITLE);

        this.traceList = traceLst;
        this.tsDisCanvas = tsdc;
    }

    @Override
    protected void initializeComponents(Shell shell) {
        Composite beginningTimeComp = new Composite(shell, SWT.NONE);
        beginningTimeComp.setLayout(new GridLayout(1, false));
        GridData gd = new GridData(SWT.FILL, SWT.FILL, true, true);
        gd.minimumWidth = getDisplay().getDPI().x * 3;
        beginningTimeComp.setLayoutData(gd);

        for (TraceData td : traceList) {
            Button checkBox = new Button(beginningTimeComp, SWT.CHECK);
            checkBox.setData(td);

            StringBuilder sb = new StringBuilder();
            if (td.isForecast()) {
                sb.append(tsDisCanvas.getFcstPEDTSE(td));
            } else {
                sb.append(tsDisCanvas.getPEDTSE(td));
            }
            if ((td.getLineData() != null) && (td.getLineData().length > 0)) {
                checkBox.setSelection(td.isTraceOn());
            } else {
                checkBox.setSelection(false);
                sb.append("NO DATA");
            }
            checkBox.setText(sb.toString());

            checkBox.addSelectionListener(new SelectionAdapter() {

                @Override
                public void widgetSelected(SelectionEvent e) {
                    handleSelection(e);
                }
            });
        }
    }

    private void handleSelection(SelectionEvent e) {
        this.tsDisCanvas.handleSelection(e);
    }
}
