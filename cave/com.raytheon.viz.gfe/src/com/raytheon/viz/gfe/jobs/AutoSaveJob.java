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
package com.raytheon.viz.gfe.jobs;

import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.jobs.Job;

import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.time.util.TimeUtil;
import com.raytheon.viz.gfe.GFEPreference;
import com.raytheon.viz.gfe.PreferenceConstants;
import com.raytheon.viz.gfe.core.DataManager;
import com.raytheon.viz.gfe.core.parm.Parm;
import com.raytheon.viz.gfe.ui.HazardUIUtils;

/**
 * Job implementing the auto save functionality. Uses user preferences to
 * initially check if auto save is on.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jun 25, 2008            Eric Babin  Initial Creation
 * Aug 27, 2013     #2302  randerso    Fixed to behave more like A1, code cleanup
 * 
 * </pre>
 * 
 * @author ebabin
 * @version 1.0
 */

public class AutoSaveJob extends Job {
    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(AutoSaveJob.class);

    /**
     * Max auto save interval in minutes
     */
    public static final int MAX_INTERVAL = 60;

    /**
     * Min auto save interval in minutes
     */
    public static final int MIN_INTERVAL = 1;

    /**
     * auto save interval in milliseconds, 0 = disabled
     */
    private long interval;

    /**
     * Set the auto save interval
     * 
     * @param interval
     *            desired interval in minutes
     */
    public void setInterval(int interval) {
        this.interval = interval * TimeUtil.MILLIS_PER_MINUTE;
        this.cancel();
        this.reSchedule(this.interval);
    }

    /**
     * Get the auto save interval
     * 
     * @return interval in minutes
     */
    public int getInterval() {
        return (int) (interval / TimeUtil.MILLIS_PER_MINUTE);
    }

    private DataManager dataManager;

    private boolean disposed;

    /**
     * Constructor taking the associated dataManager
     * 
     * @param dataManager
     */
    public AutoSaveJob(DataManager dataManager) {
        super("AutoSaveJob");
        this.dataManager = dataManager;
        this.disposed = false;
        this.setSystem(true);
        int pref = GFEPreference
                .getIntPreference(PreferenceConstants.GFE_AUTO_SAVE_INTERVAL);
        if (pref > MAX_INTERVAL) {
            pref = MAX_INTERVAL;
        } else if (pref < 0) {
            pref = 0;
        }

        this.interval = pref * TimeUtil.MILLIS_PER_MINUTE;
        reSchedule(this.interval);
    }

    /**
     * Schedule this job to run after the desired
     * 
     * @param delay
     *            interval in milliseconds
     */
    public void reSchedule(long delay) {
        if (!disposed) {
            if (delay > 0) {
                this.schedule(delay);
            }
        } else {
            dataManager = null;
        }
    }

    /**
     * Cancel this job and release its reference to the DataManager
     */
    public void dispose() {
        this.disposed = true;
        this.cancel();

        if (this.getState() != Job.RUNNING) {
            this.dataManager = null;
        }
    }

    /**
     * Perform auto save
     */
    @Override
    protected IStatus run(IProgressMonitor monitor) {
        long delay = this.interval;
        if (delay > 0) {
            String msg = "GFE Auto Save called.  ";
            boolean success = false;
            try {
                Parm modifiedParm = getModifiedParm();
                if (modifiedParm != null) {
                    msg += "Save of Parm "
                            + modifiedParm.getParmID().getParmName() + " = ";

                    // save the data
                    success = modifiedParm.saveParameter(false);
                    msg += (success ? "success" : "failure");
                    delay = 1500;
                } else {
                    msg += "Nothing to save.";
                }
                statusHandler.info(msg);
            } catch (Exception e) {
                msg += "failure";
                statusHandler.error(msg, e);
            }
            reSchedule(delay);
        }
        return Status.OK_STATUS;
    }

    /**
     * Find the first available modified parm. Skip Hazards if temp hazards
     * exist
     * 
     * @return the first available modified parm or null if none
     */
    private Parm getModifiedParm() {
        boolean tempHazDisplayed = HazardUIUtils
                .tempHazardsExist(this.dataManager);
        for (Parm p : this.dataManager.getParmManager().getModifiedParms()) {
            if (p.getParmID().getParmName().contains("Hazards")
                    && tempHazDisplayed) {
                continue;
            } else {
                return p;
            }
        }
        return null;
    }

    /**
     * Saves the specified Parm
     * 
     * @param parm
     * @return String of result of save ParmName=success, or failure.
     */
    private String saveParm(Parm parm) {
        boolean saveResult = this.dataManager.getParmManager().saveParm(parm);

        String s = parm.getParmID().getParmName() + "= "
                + (saveResult ? " success " : " failure ");
        return s;
    }
}
