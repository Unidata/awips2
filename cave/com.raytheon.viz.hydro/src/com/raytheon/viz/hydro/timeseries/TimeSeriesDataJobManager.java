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

import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.QualifiedName;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.jobs.IJobChangeListener;
import org.eclipse.core.runtime.jobs.Job;

/**
 * The TimeSeriesDataJob Manager asynchronously retrieves Time Series Graph and
 * Time Series Tabular Data via the Eclipse Job capability.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jan 28, 2011            bkowal      Initial creation
 * May 06, 2013   1976     mpduff      Moved c.getDataForGraph() inside .runasync block
 * 
 * </pre>
 * 
 * @author bkowal
 * @version 1.0
 */

public class TimeSeriesDataJobManager extends Job {
    public static enum REQUEST_TYPE {
        REQUEST_TYPE_GRAPH, REQUEST_TYPE_TABULAR
    }

    public TimeSeriesDataJobManager() {
        super("");
    }

    /*
     * (non-Javadoc)
     * 
     * @see org.eclipse.core.runtime.jobs.Job#run(org.eclipse.core.runtime.
     * IProgressMonitor)
     */
    @Override
    protected IStatus run(IProgressMonitor monitor) {
        REQUEST_TYPE requestType = (REQUEST_TYPE) this
                .getProperty(new QualifiedName(null, "REQUEST_TYPE"));
        if (requestType == TimeSeriesDataJobManager.REQUEST_TYPE.REQUEST_TYPE_GRAPH) {
            TimeSeriesDisplayCanvas tsDisplayCanvas = (TimeSeriesDisplayCanvas) this
                    .getProperty(new QualifiedName(null,
                            "TimeSeriesDisplayCanvas"));
            final TimeSeriesDisplayCanvas c = tsDisplayCanvas;
            tsDisplayCanvas.getDisplay().asyncExec(new Runnable() {
                @Override
                public void run() {
                    if (!c.isDisposed()) {
                        c.getDataForGraph();
                        c.redraw();
                    }
                }
            });
        } else if (requestType == TimeSeriesDataJobManager.REQUEST_TYPE.REQUEST_TYPE_TABULAR) {
            TabularTimeSeriesDlg tabularTimeSeriesDlg = (TabularTimeSeriesDlg) this
                    .getProperty(new QualifiedName(null, "TabularTimeSeriesDlg"));
            String selection = (String) this.getProperty(new QualifiedName(
                    null, "selection"));
            tabularTimeSeriesDlg.getDataForTable(selection);
        }

        return Status.OK_STATUS;
    }

    public void scheduleGetGraphData(IJobChangeListener jobChangeListener,
            TimeSeriesDisplayCanvas tsDisplayCanvas) {

        this.setName("Retrieving Graph Data ...");
        this.setProperty(new QualifiedName(null, "REQUEST_TYPE"),
                TimeSeriesDataJobManager.REQUEST_TYPE.REQUEST_TYPE_GRAPH);
        this.setProperty(new QualifiedName(null, "TimeSeriesDisplayCanvas"),
                tsDisplayCanvas);
        this.addJobChangeListener(jobChangeListener);
        this.schedule();
    }

    public void scheduleGetTableData(IJobChangeListener jobChangeListener,
            TabularTimeSeriesDlg tabularTimeSeriesDlg, String selection) {
        this.setName("Retrieving Tabular Data ...");
        this.setProperty(new QualifiedName(null, "REQUEST_TYPE"),
                TimeSeriesDataJobManager.REQUEST_TYPE.REQUEST_TYPE_TABULAR);
        this.setProperty(new QualifiedName(null, "TabularTimeSeriesDlg"),
                tabularTimeSeriesDlg);
        this.setProperty(new QualifiedName(null, "selection"), selection);
        this.addJobChangeListener(jobChangeListener);
        this.schedule();
    }
}