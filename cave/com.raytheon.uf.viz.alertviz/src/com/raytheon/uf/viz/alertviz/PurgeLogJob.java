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
package com.raytheon.uf.viz.alertviz;

import java.sql.Timestamp;
import java.util.Collection;
import java.util.concurrent.CopyOnWriteArraySet;

import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.jobs.Job;
import org.eclipse.jface.dialogs.ErrorDialog;
import org.eclipse.swt.widgets.Display;

import com.raytheon.uf.common.time.util.TimeUtil;
import com.raytheon.uf.viz.alertviz.internal.LogMessageDAO;

/**
 * Purges old database entries.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Sep 22, 2008 1433       chammack    Initial creation
 * Aug 04, 2014 3356       njensen     Added e.printStacktrace() to trace errors
 * Feb 23, 2016 5314       dgilling    Rewrite as singleton, add listeners.
 * 
 * </pre>
 * 
 * @author chammack
 * @version 1.0
 */
public class PurgeLogJob extends Job {

    private static final int DEFAULT_AGE_OF_LOG_IN_HRS = 12;

    private static final long JOB_CYCLE_TIME = 1 * TimeUtil.MILLIS_PER_HOUR;

    private static final PurgeLogJob INSTANCE = new PurgeLogJob();

    private final Collection<IAlertVizLogPurgedNotifier> purgeListeners;

    private final int ageOfLogInHours;

    public static PurgeLogJob getInstance() {
        return INSTANCE;
    }

    private PurgeLogJob() {
        super("Archive Log Purge");
        setSystem(true);

        this.purgeListeners = new CopyOnWriteArraySet<>();

        int configuredLogAge = Activator.getDefault().getPreferenceStore()
                .getInt(Constants.P_MAX_AGE_OF_LOGS);
        this.ageOfLogInHours = (configuredLogAge != 0) ? configuredLogAge
                : DEFAULT_AGE_OF_LOG_IN_HRS;

        this.schedule();

    }

    @Override
    protected IStatus run(IProgressMonitor monitor) {
        try {
            long now = System.currentTimeMillis();

            Timestamp ts = new Timestamp(now
                    - (ageOfLogInHours * TimeUtil.MILLIS_PER_HOUR));
            Collection<Integer> recordsDeleted = LogMessageDAO.getInstance()
                    .purge(ts);
            firePurgeListeners(recordsDeleted);
        } catch (AlertvizException e) {
            final Status s = new Status(Status.ERROR, Activator.PLUGIN_ID,
                    "Error occurred during purge and rotate", e);
            e.printStackTrace();
            Display.getDefault().syncExec(new Runnable() {

                @Override
                public void run() {
                    ErrorDialog.openError(
                            Display.getDefault().getActiveShell(), "Error",
                            "Error purging logs.", s);
                }

            });

        }

        this.schedule(JOB_CYCLE_TIME);

        return Status.OK_STATUS;
    }

    public void addLogPurgeListener(IAlertVizLogPurgedNotifier listener) {
        purgeListeners.add(listener);
    }

    public void removeLogPurgeListener(IAlertVizLogPurgedNotifier listener) {
        purgeListeners.remove(listener);
    }

    private void firePurgeListeners(Collection<Integer> recordsDeleted) {
        for (IAlertVizLogPurgedNotifier listener : purgeListeners) {
            listener.recordsPurged(recordsDeleted);
        }
    }
}
