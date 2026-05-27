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
 * Date          Ticket#     Engineer     Description
 * ------------- ----------- ------------ --------------------------
 * Jan 28, 2011              bkowal       Initial creation
 * May 06, 2013  1976        mpduff       Moved c.getDataForGraph() inside
 *                                        .runasync block
 * Mar 17, 2016  5483        randerso     Updated for use in reworked
 *                                        TabularTimeSeriesDlg
 * Jun 27, 2018  6748        randerso     No longer used for time series graphs
 *
 * </pre>
 *
 * @author bkowal
 */

public class TimeSeriesDataJobManager extends Job {
    /**
     * Constructor
     */
    public TimeSeriesDataJobManager() {
        super("");
    }

    @Override
    protected IStatus run(IProgressMonitor monitor) {
        TabularTimeSeriesDlg tabularTimeSeriesDlg = (TabularTimeSeriesDlg) this
                .getProperty(new QualifiedName(null, "TabularTimeSeriesDlg"));
        tabularTimeSeriesDlg.getDataForTable();
        return Status.OK_STATUS;
    }

    /**
     * @param jobChangeListener
     * @param tabularTimeSeriesDlg
     */
    public void scheduleGetTableData(IJobChangeListener jobChangeListener,
            TabularTimeSeriesDlg tabularTimeSeriesDlg) {
        this.setName("Retrieving Tabular Data ...");
        this.setProperty(new QualifiedName(null, "TabularTimeSeriesDlg"),
                tabularTimeSeriesDlg);
        this.addJobChangeListener(jobChangeListener);
        this.schedule();
    }
}