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

import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;

/**
 * Request to update or retrieve incoming Backup Jobs
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * May 6, 2021  91325      lsingh           Initial creation
 * May 24, 2021  84473     Amanuel Challa   Added Outgoing jobs Id
 * Jul 01, 2021 93517      Amanuel Challa   Added a new Constructor with filterQueryStr
 * </pre>
 *
 * @author lsingh
 */
@DynamicSerialize
public class IncomingBackupJobsRequest extends IncomingOutgoingJobsRequest {
    /**
     * A List of Outgoing job IDs
     */
    @DynamicSerializeElement
    private List<Long> outJobIds;

    public IncomingBackupJobsRequest() {
        super();
    }

    public IncomingBackupJobsRequest(RequestType type, String site,
            List<Long> jobIds) {
        super(type, site, jobIds);
    }

    public IncomingBackupJobsRequest(RequestType type, String site,
            List<Long> jobIds, List<Long> outJobIds) {
        super(type, site, jobIds);
        this.outJobIds = outJobIds;

    }

    public IncomingBackupJobsRequest(RequestType type, String site) {
        super(type, site);
    }

    public IncomingBackupJobsRequest(RequestType type, String site,
            String filterQueryStr) {
        super(type, site, filterQueryStr);
    }

    public List<Long> getOutJobIds() {
        return outJobIds;
    }

    public void setOutJobIds(List<Long> outJobIds) {
        this.outJobIds = outJobIds;
    }

}
