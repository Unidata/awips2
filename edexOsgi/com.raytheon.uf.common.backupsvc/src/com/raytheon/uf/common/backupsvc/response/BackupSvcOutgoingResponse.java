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
package com.raytheon.uf.common.backupsvc.response;

import java.util.HashMap;
import java.util.List;
import java.util.Map;

import com.raytheon.uf.common.dataplugin.backupsvc.database.BackupSvcOutgoing;
import com.raytheon.uf.common.serialization.ISerializableObject;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;

/**
 * Outgoing Tab on the Backup Services Dialog. Allows users with the correct
 * Backup Services Roles to Send Outgoing Localization files to other sites.
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * May 19, 2021 91044      Robert.Blum Initial creation
 * Jun 08, 2021 92508      Robert.Blum Added helper method to check for successful
 *                                     response and added failedJobs field.
 * Jul 23, 2023 2035783    Lisa.Singh  Added support for individual job failures.
 * </pre>
 *
 * @author Robert.Blum
 */

@DynamicSerialize
public class BackupSvcOutgoingResponse implements ISerializableObject {

    @DynamicSerializeElement
    private List<BackupSvcOutgoing> outgoingJobs;

    @DynamicSerializeElement
    private Map<BackupSvcOutgoing, Exception> failedOutgoingJobs;

    /**
     * General response errors.
     */
    @DynamicSerializeElement
    private Exception exceptions;

    public BackupSvcOutgoingResponse() {

    }

    public List<BackupSvcOutgoing> getOutgoingJobs() {
        return outgoingJobs;
    }

    public void setOutgoingJobs(List<BackupSvcOutgoing> outgoingjobs) {
        this.outgoingJobs = outgoingjobs;
    }

    public Map<BackupSvcOutgoing, Exception> getFailedOutgoingJobs() {
        return failedOutgoingJobs;
    }

    public void setFailedOutgoingJobs(
            Map<BackupSvcOutgoing, Exception> failedOutgoingJobs) {
        this.failedOutgoingJobs = failedOutgoingJobs;
    }

    /**
     * Add a job with an error message to a map of failed jobs.
     * 
     * @param job
     * @param message
     */
    public void addFailedOutgoingJob(BackupSvcOutgoing job, Exception e) {
        if (failedOutgoingJobs == null) {
            failedOutgoingJobs = new HashMap<>();
        }

        failedOutgoingJobs.put(job, e);
    }

    /**
     * Add multiple jobs with the same error message to the map of failed jobs.
     * 
     * @param jobs
     * @param message
     */
    public void addFailedOutgoingJob(List<BackupSvcOutgoing> jobs,
            Exception e) {
        if (failedOutgoingJobs == null) {
            failedOutgoingJobs = new HashMap<>();
        }

        for (BackupSvcOutgoing job : jobs) {
            failedOutgoingJobs.put(job, e);
        }
    }

    public Exception getExceptions() {
        return exceptions;
    }

    public void setExceptions(Exception exceptions) {
        this.exceptions = exceptions;
    }

    public boolean isSuccessful() {
        return exceptions == null
                && (failedOutgoingJobs == null || failedOutgoingJobs.isEmpty());
    }

}
