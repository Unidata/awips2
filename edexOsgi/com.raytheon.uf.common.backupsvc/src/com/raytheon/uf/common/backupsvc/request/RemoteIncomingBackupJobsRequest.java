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
package com.raytheon.uf.common.backupsvc.request;

import java.util.List;

import com.raytheon.uf.common.dataplugin.backupsvc.database.BackupSvcIncoming;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;

/**
 * Request to Remote hosts to transfer Backup Service Incoming jobs.
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jun 3, 2021  92508      Amanuel.Challa Initial creation
 *
 * </pre>
 *
 * @author Amanuel.Challa
 */
@DynamicSerialize
public class RemoteIncomingBackupJobsRequest
        extends IncomingOutgoingJobsRequest {
    /**
     * An array of Incoming jobs
     */
    @DynamicSerializeElement
    private List<BackupSvcIncoming> jobs;

    public RemoteIncomingBackupJobsRequest() {
        super();
    }

    public RemoteIncomingBackupJobsRequest(RequestType type, String site,
            List<BackupSvcIncoming> jobs, List<Long> jobIds) {
        super(type, site, jobIds);

    }

    public RemoteIncomingBackupJobsRequest(RequestType type, String site,
            List<BackupSvcIncoming> jobs) {
        super(type, site);
        this.jobs = jobs;

    }

    public List<BackupSvcIncoming> getJobs() {
        return jobs;
    }

    public void setJobs(List<BackupSvcIncoming> jobs) {
        this.jobs = jobs;
    }
}
