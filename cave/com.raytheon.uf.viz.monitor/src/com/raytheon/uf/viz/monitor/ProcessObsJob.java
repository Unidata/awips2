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
package com.raytheon.uf.viz.monitor;

import java.util.Date;
import java.util.HashMap;
import java.util.Map;

import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.SubMonitor;
import org.eclipse.core.runtime.jobs.Job;

import com.raytheon.uf.common.dataplugin.fssobs.FSSObsRecord;
import com.raytheon.uf.common.dataquery.requests.RequestConstraint;
import com.raytheon.uf.common.dataquery.requests.RequestConstraint.ConstraintType;
import com.raytheon.uf.common.inventory.exception.DataCubeException;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.common.time.util.TimeUtil;
import com.raytheon.uf.viz.monitor.data.ObReport;

/**
 * 
 * Load Obs off the UI thread.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Oct 21, 2015  3873      dhladky     Initial creation.
 * 
 * </pre>
 * 
 * @author dhladky
 * @version 1.0
 * 
 */

public class ProcessObsJob extends Job {
    
    private static final IUFStatusHandler statusHandler = UFStatus
            .getHandler(ProcessObsJob.class);
    
    protected static final int PROGRESS_FACTOR = 1;
    
    /** how many hours do FSSObs go back we wish to load here **/
    public static final int HOUR_BACK = 12;
    
    private ObsMonitor obsMonitor;

    public ProcessObsJob(ObsMonitor obsMonitor) {
        super("Obs Load Process");
        this.setSystem(false);
        this.setPriority(INTERACTIVE);
        this.obsMonitor = obsMonitor;
    }

    public IStatus run(IProgressMonitor monitor) {

        try {

            long backTime = TimeUtil.newCalendar().getTimeInMillis();
            Date time = new Date(backTime
                    - (HOUR_BACK * TimeUtil.MILLIS_PER_HOUR));

            Map<String, RequestConstraint> vals = new HashMap<String, RequestConstraint>();
            vals.put("dataTime.refTime",
                    new RequestConstraint(TimeUtil.formatToSqlTimestamp(time),
                            ConstraintType.GREATER_THAN_EQUALS));

            long startPoint = System.currentTimeMillis();
            FSSObsRecord[] recs = obsMonitor.requestFSSObs(vals);
            long endPoint = System.currentTimeMillis();
            SubMonitor smonitor = SubMonitor.convert(monitor, "Loading "
                    + recs.length + " observations...", recs.length);
            smonitor.beginTask(null, recs.length);
            statusHandler.info("Point Data Request, took: "
                    + (endPoint - startPoint) + " ms");

            for (FSSObsRecord rec : recs) {
                if (rec != null) {
                    if (!this.shouldRun()) {
                        return Status.CANCEL_STATUS;
                    }
                    long start = System.currentTimeMillis();
                    doOb(rec, smonitor.newChild(PROGRESS_FACTOR));
                    long end = System.currentTimeMillis();
                    statusHandler.info("Processed " + rec.getIdentifier()
                            + " in " + (end - start) + " ms");
                }
            }

            statusHandler.info("Processed " + recs.length + " FSSObs records.");
            // fire event to trigger re-paint
            obsMonitor.fireMonitorEvent();

        } catch (DataCubeException e) {
            statusHandler.handle(Priority.PROBLEM,
                    "No data in database at startup.");
            return Status.CANCEL_STATUS;
        }

        return Status.OK_STATUS;
    }

    /**
     * Processes the Ob
     * 
     * @param objectToSend
     **/
    protected void doOb(FSSObsRecord objectToSend, SubMonitor smonitor) {

        smonitor.beginTask(null, PROGRESS_FACTOR);
        ObReport result = GenerateFSSObReport
                .generateObReport(objectToSend);
        obsMonitor.processAtStartup(result);
        
    }

}
