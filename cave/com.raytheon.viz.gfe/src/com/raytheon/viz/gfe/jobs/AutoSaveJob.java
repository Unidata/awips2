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

import com.raytheon.viz.gfe.Activator;
import com.raytheon.viz.gfe.GFEPreference;
import com.raytheon.viz.gfe.PreferenceConstants;
import com.raytheon.viz.gfe.core.DataManager;
import com.raytheon.viz.gfe.core.parm.Parm;

/**
 * Job implementing the auto save functionality. Uses user preferences to
 * initially check if auto save is on.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 	Jun 25, 2008					Eric Babin Initial Creation
 * 
 * </pre>
 * 
 * @author ebabin
 * @version 1.0
 */

public class AutoSaveJob extends Job {

    private static final int MILLISECONDS_PER_MINUTE = 60 * 1000;

    public static final int MAX_INTERVAL = 60;

    public static final int MIN_INTERVAL = 1;

    /**
     * auto save interval in minutes, 0 = disabled
     */
    private static int interval;
    static {
        int i = 0;
        if (GFEPreference.storeAvailable()) {
            i = GFEPreference
                    .getIntPreference(PreferenceConstants.GFE_AUTO_SAVE_INTERVAL);
        }

        interval = i;
    }

    /**
     * Set the auto save interval
     * 
     * @param i
     *            desired interval in minutes
     */
    public static void setInterval(int i) {
        interval = i;

        GFEPreference.setPreference(PreferenceConstants.GFE_AUTO_SAVE_INTERVAL,
                Integer.toString(i));
    }

    /**
     * Get the auto save interval
     * 
     * @return interval in minutes
     */
    public static int getInterval() {
        return interval;
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

        reSchedule();
    }

    /**
     * Schedule this job to run after the desired interval
     */
    public void reSchedule() {
        if (!disposed) {
            if (interval > 0) {
                this.schedule(interval * MILLISECONDS_PER_MINUTE);
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
        IStatus status = Status.OK_STATUS;
        try {
            Parm[] parms = this.dataManager.getParmManager().getModifiedParms();
            if (parms != null && parms.length != 0) {
                String autoSaveResult = "GFE Auto Save called.  Save of Parms[";
                for (int i = 0; i < parms.length; i++) {
                    String name = parms[i].getParmID().getParmName();
                    if (name.indexOf("Hazards") == -1) {
                        autoSaveResult += saveParm(parms[i]);
                    } else {
                        if (!tempHazardsExist()) {
                            saveParm(parms[i]);
                            autoSaveResult += saveParm(parms[i]);
                        }
                    }
                }
                autoSaveResult += "]";

                Activator.getDefault().getLog().log(
                        new Status(IStatus.INFO, Activator.PLUGIN_ID,
                                autoSaveResult));
            }
        } catch (Exception e) {
            status = new Status(IStatus.ERROR, Activator.PLUGIN_ID,
                    "GFE auto save failed.", e);
        }
        reSchedule();
        return status;
    }

    /**
     * If tempHazards exist, then autosave kicked off in the middle of merge
     * hazard. (which creates a "temp" hazard with "haz" in the name.
     * 
     * So we will ignore the "Hazard" WE parm save...
     * 
     * @return if temp haz exists.
     */
    private boolean tempHazardsExist() {
        Parm[] parms = this.dataManager.getParmManager().getDisplayedParms();
        for (int i = 0; i < parms.length; i++) {
            if (parms[i].getParmID().getParmName().indexOf("haz") != -1) {
                return true;
            }
        }

        return false;
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
