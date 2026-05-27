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

import com.raytheon.uf.common.dataplugin.backupsvc.database.BackupSvcOutgoing;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;

/**
 * Request to Remote hosts to transfer Backup Service Outgoing jobs.
 * 
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jun 3, 2021  92508      Robert.Blum Initial creation
 *
 * </pre>
 *
 * @author Robert.Blun
 */
@DynamicSerialize
public class RemoteOutgoingBackupJobsRequest
        extends IncomingOutgoingJobsRequest {

    /**
     * A list of Outgoing jobs
     */
    @DynamicSerializeElement
    private List<BackupSvcOutgoing> jobs;

    public RemoteOutgoingBackupJobsRequest() {
        super();
    }

    public RemoteOutgoingBackupJobsRequest(RequestType type, String site,
            List<Long> jobsIds, List<BackupSvcOutgoing> jobs) {
        super(type, site, jobsIds);
        this.jobs = jobs;
    }

    public List<BackupSvcOutgoing> getJobs() {
        return jobs;
    }

    public void setJobs(List<BackupSvcOutgoing> jobs) {
        this.jobs = jobs;
    }

}