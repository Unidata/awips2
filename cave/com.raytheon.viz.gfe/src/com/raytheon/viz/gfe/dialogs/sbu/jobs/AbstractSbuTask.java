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
package com.raytheon.viz.gfe.dialogs.sbu.jobs;

import java.util.Date;
import java.util.HashSet;
import java.util.Set;

import com.raytheon.uf.common.dataplugin.gfe.svcbu.JobProgress;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.viz.core.exception.VizException;

/**
 * Abstract base class for Service Backup tasks
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Mar 9, 2015            randerso     Initial creation
 * 
 * </pre>
 * 
 * @author randerso
 * @version 1.0
 */

public abstract class AbstractSbuTask {
    protected final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(AbstractSbuTask.class);

    private String statusFileName;

    private String guiDescription;

    private JobProgress status;

    private Date statusTime;

    private Set<ITaskCompleteCallBack> callBacks;

    public AbstractSbuTask(String statusFileName, String guiDescription) {
        this.statusFileName = statusFileName;
        this.guiDescription = guiDescription;

        this.callBacks = new HashSet<>();

        setStatus(JobProgress.UNKNOWN);
    }

    /**
     * @return the statusFileName
     */
    public String getStatusFileName() {
        return statusFileName;
    }

    /**
     * @return the guiDescription
     */
    public String getGuiDescription() {
        return guiDescription;
    }

    /**
     * @return the status
     */
    public JobProgress getStatus() {
        return status;
    }

    private void setStatus(JobProgress status) {
        this.status = status;
        if (status.equals(JobProgress.UNKNOWN)) {
            this.statusTime = new Date(0);
        } else {
            this.statusTime = new Date();
        }
        for (ITaskCompleteCallBack callBack : callBacks) {
            callBack.taskComplete(this);
        }
    }

    public Date getStatusTime() {
        return this.statusTime;
    }

    public void addCallBack(ITaskCompleteCallBack callBack) {
        this.callBacks.add(callBack);
    }

    public void removeCallBack(ITaskCompleteCallBack callBack) {
        this.callBacks.remove(callBack);
    }

    public final void run() {
        setStatus(JobProgress.IN_PROGRESS);

        try {
            setStatus(runTask());
        } catch (VizException e) {
            statusHandler.error(e.getLocalizedMessage(), e);
            setStatus(JobProgress.FAILED);
        }
    }

    protected abstract JobProgress runTask() throws VizException;

}
